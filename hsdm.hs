{-# LANGUAGE DeriveGeneric #-}

module Main where

import           Control.Concurrent
import           Control.Concurrent.MVar
import           Control.Exception
import           Control.Exception.Lens
import           Control.Lens
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as LBSC
import           GHC.Generics
import           Graphics.X11.Xlib
import           Foreign.C
import           System.Environment
import           System.Exit
import           System.Posix.Directory
--import           System.Posix.PAM
import           System.Posix.Process
import           System.Posix.Signals
import           System.Posix.Types
import           System.Posix.User
import           System.Process
--import           Xwrap
--import           PAM

data ConfigFile = ConfigFile { default_xserver   :: FilePath
                             , xserver_arguments :: [String]
                             , sessiondir        :: FilePath
                             , login_cmd         :: String
                             , display           :: String }
                deriving (Generic, Show)
instance FromJSON ConfigFile

data GuiReturn = GuiStop | GuiRestart;

oldMain :: IO ()
oldMain = do
  display <- openDisplay ""
  print display
  let root = defaultRootWindow display
  let default_screen = defaultScreen display
  w <- createSimpleWindow display root 0 0 480 480 0 0 0;
  gc <- createGC display w
  freeGC display gc
  mapWindow display w
  flush display
  print root
  getLine >>= putStrLn
  closeDisplay display
  return ()

initGroups :: String -> GroupID -> IO ()
initGroups username group = do
  allGroups <- getAllGroupEntries
  let memberGroups = (group :)
                     $ map groupID
                     $ filter (elem username . groupMembers) allGroups
  setGroups memberGroups
  return ()

dumpIDs = do
  uid <- getRealUserID
  euid <- getEffectiveUserID
  gid <- getRealGroupID
  egid <- getEffectiveGroupID
  putStrLn $ "uid: " ++ (show uid) ++ ", " ++ (show euid)
  putStrLn $ "gid: " ++ (show gid) ++ ", " ++ (show egid)
  return ()

switchUser :: String -> IO UserEntry
switchUser username = do
  dumpIDs
  entry <- getUserEntryForName username
  let (uid, gid) = (userID entry, userGroupID entry)
  initGroups username gid
  setGroupID gid
  setUserID  uid
  dumpIDs
  ( _, _, _, ph) <- createProcess $ shell "id"
  code <- waitForProcess ph
  return entry

restoreRoot :: IO ()
restoreRoot = do
  dumpIDs
  setRealEffectiveGroupID 0 0
  setRealEffectiveUserID  0 0
  setGroups []
  dumpIDs
  return ()

setRealEffectiveUserID :: UserID -> UserID -> IO ()
setRealEffectiveUserID ruid euid = throwErrnoIfMinus1_ "setRealEffectiveUserID" (c_setreuid ruid euid)

setRealEffectiveGroupID :: GroupID -> GroupID -> IO ()
setRealEffectiveGroupID rgid egid = throwErrnoIfMinus1_ "setRealEffectiveGroupID" (c_setregid rgid egid)

foreign import ccall unsafe "setreuid" c_setreuid :: CUid -> CUid -> IO CInt
foreign import ccall unsafe "setregid" c_setregid :: CGid -> CGid -> IO CInt

{-pamtest :: String -> String -> IO ()
pamtest username password = do
  foo <- authenticate "hsdm" username password
  case foo of
    Left code -> print $ pamCodeDetails code
    Right ()  -> do
      _ <- switchUser username
      return ()
  return ()-}

runner :: ConfigFile -> MVar Bool -> IO ()
runner c mutex = do
  let args = xserver_arguments c
  threadDelay 500000 -- FIXME
  installHandler sigUSR1 Ignore Nothing
  executeFile (default_xserver c) False args Nothing
  return ()

serverup :: MVar Bool -> IO ()
serverup mutex = void $ do
  putStrLn "got user1"
  putMVar mutex True

childDead :: ProcessID -> MVar Bool -> SignalInfo -> IO ()
childDead xorgPid mutex info = void $ do
  putStrLn "child process dead"
  case info of
    SignalInfo sig err info -> do -- lens time?
      if xorgPid == (siginfoPid info) then
          putMVar mutex False
        else
          putStrLn "session quit"

startX :: ConfigFile -> IO GuiReturn -> IO ()
startX c f = do
  mutex <- newEmptyMVar
  installHandler sigUSR1 (Catch $ serverup mutex) Nothing
  pid <- forkProcess $ runner c mutex
  installHandler sigCHLD (CatchInfo $ childDead pid mutex) Nothing
  result <- takeMVar mutex
  if result
    then do
      looper f pid mutex
      signalProcess sigTERM pid
    else error "fixme1"

looper :: IO GuiReturn -> ProcessID -> MVar Bool -> IO ()
looper f pid mutex = do
  status <- f
  case status of
    GuiStop -> return ()
    GuiRestart -> do
      signalProcess sigHUP pid
      result <- takeMVar mutex
      if result then
          looper f pid mutex
        else
          error "fixme2"

sessionRunner :: ConfigFile -> String -> IO ()
sessionRunner c username = do
  -- pam_setcred(handle, PAM_ESTABLISH_CRED);
  -- pam_open_session(handle, 0);
  pid <- forkProcess (childProc username (login_cmd c) (display c) )
  status <- getProcessStatus True False pid
  dumpIDs
  -- pam_close_session(handle, 0);
  -- pam_setcred(handle, PAM_DELETE_CRED);
  return ()

childProc :: String -> String -> String -> IO ()
childProc username command dsp = do
  entry <- switchUser username
  let cmd = ShellCommand $ command
  let env = [ ("DISPLAY", dsp)
            , ("HOME", homeDirectory entry)
            , ("USER", username)
            , ("LOGNAME", username)
            , ("SHELL", userShell entry)
            , ("XDG_RUNTIME_DIR", "/run/user/1100")
            , ("PATH","/run/current-system/sw/bin")
            , ("XDG_DATA_DIRS", "/run/current-system/sw/share") ]
  let proc1 = CreateProcess cmd Nothing (Just env) Inherit Inherit Inherit True False False
  changeWorkingDirectory $ homeDirectory entry
  executeFile "sh" True ["-c", command] (Just env)
  --(_, _, _, ph) <- createProcess proc1
  --status <- waitForProcess ph
  --print status
  --return ()



-- FIXME: Write code for login prompt!
-- `login' is a continuation that, when run, starts the login process
-- Before you run `login', the following conditions must hold:
--     * You must verify that the password given was correct
--     * You must destroy any X11 'Display's you have created
--
-- loginPrompt should check the resulting 'ProcessStatus' and, if an error
-- occurred, pop up an error message of some kind.
loginPrompt :: (String -> IO ()) -> IO ()
loginPrompt login = void $ login "test"


doGui :: ConfigFile -> IO GuiReturn
doGui c = do
  putStrLn "gui starting"
  loginPrompt $ sessionRunner c
  putStrLn "gui stopping"
  return GuiRestart

parse :: [String] -> IO (Maybe ConfigFile)
parse ["-c", file] = decode <$> LBSC.readFile file
parse _            = usage >> exit

usage = do
  foo <- getProgName
  putStrLn $ "Usage: " ++ foo ++ " -c config_file"
exit = exitWith ExitSuccess

main :: IO ()
main = do
  changeWorkingDirectory "/"
  result <- getArgs >>= parse
  case result
    of Just cfg -> startX cfg $ doGui cfg
       Nothing  -> usage
  return ()

hack :: String -> IO (Maybe ConfigFile)
hack file = decode <$> LBSC.readFile file

uiTest :: String -> IO ()
uiTest cfg = do
  config <- hack cfg
  case config of
    Just cfg -> do
      print config
      startX cfg $ uiTest2 cfg
  return ()

uiTest2 :: ConfigFile -> IO GuiReturn
uiTest2 c = do
  --showWindow $ display c
  return GuiStop

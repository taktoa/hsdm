{-# LANGUAGE DeriveGeneric #-}

module Main where

import           Control.Concurrent.MVar
import           Control.Exception
import           Control.Exception.Lens
import           Control.Lens
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as LBSC
import           GHC.Generics
import           Graphics.X11.Xlib
import           System.Environment
import           System.Exit
import           System.Posix.Directory
import           System.Posix.PAM
import           System.Posix.Process
import           System.Posix.Signals
import           System.Posix.Types
import           System.Posix.User
import           System.Process

data ConfigFile = ConfigFile { default_xserver   :: FilePath
                             , xserver_arguments :: [String]
                             , sessiondir        :: FilePath
                             , login_cmd         :: String }
                deriving (Generic, Show)
instance FromJSON ConfigFile

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

switchUser :: String -> IO UserEntry
switchUser username = do
  entry <- getUserEntryForName username
  let (uid, gid) = (userID entry, userGroupID entry)
  initGroups username gid
  setGroupID gid
  setUserID uid
  ( _, _, _, ph) <- createProcess $ shell "id"
  code <- waitForProcess ph
  return entry

pamtest :: String -> String -> IO ()
pamtest username password = do
  foo <- authenticate "hsdm" username password
  case foo of
    Left code -> print $ pamCodeDetails code
    Right ()  -> do
      _ <- switchUser username
      return ()
  return ()

runner :: ConfigFile -> MVar Bool -> IO ()
runner c mutex = do
  let args = xserver_arguments c
  installHandler sigUSR1 Ignore Nothing
  executeFile (default_xserver c) False args Nothing
  return ()

serverup :: MVar Bool -> IO ()
serverup mutex = void $ putMVar mutex True

childDead :: MVar Bool -> IO ()
childDead mutex = void $ putMVar mutex False

startX :: ConfigFile -> IO () -> IO ()
startX c f = do
  mutex <- newEmptyMVar
  installHandler sigUSR1 (CatchOnce $ serverup mutex) Nothing
  installHandler sigCHLD (Catch $ childDead mutex) Nothing
  pid <- forkProcess $ runner c mutex
  result <- takeMVar mutex
  if result
    then f >> signalProcess sigTERM pid
    else error "fixme"

sessionRunner :: ConfigFile -> String -> IO ()
sessionRunner c username = do
  entry <- switchUser "test"
  -- pam_setcred(handle, PAM_ESTABLISH_CRED);
  -- pam_open_session(handle, 0);
  --let cmd = RawCommand "/nix/store/idm1067y9i6m87crjqrbamdsq2ma5r7p-bash-4.3-p42/bin/bash" [ "/nix/store/sbmm3fpgh5sgwhsaaq9k9v66xf8019nh-xsession", "xfce" ]
  --let cmd = RawCommand "/run/current-system/sw/bin/xterm" [ ]
  --let cmd = RawCommand "/run/current-system/sw/bin/strace" [ "-ff", "-o", "/home/test/logfiles3", "-s", "90000", "/run/current-system/sw/bin/xterm"]
  let cmd = ShellCommand $ login_cmd c
  let env = [ ("DISPLAY", ":0")
            , ("HOME", homeDirectory entry)
            , ("USER", username)
            , ("LOGNAME", username)
            , ("SHELL", userShell entry)
            , ("XDG_RUNTIME_DIR", "/run/user/1100")
            , ("PATH","/run/current-system/sw/bin")
            , ("XDG_DATA_DIRS", "/run/current-system/sw/share") ]
  let proc1 = CreateProcess cmd Nothing (Just env) Inherit Inherit Inherit True False False
  changeWorkingDirectory $ homeDirectory entry
  (_, _, _, ph) <- createProcess proc1
  waitForProcess ph
  -- pam_close_session(handle, 0);
  return ()



-- FIXME: Write code for login prompt!
-- `login' is a continuation that, when run, starts the login process
-- Before you run `login', the following conditions must hold:
--     * You must verify that the password given was correct
--     * You must destroy any X11 'Display's you have created
--
-- loginPrompt should check the resulting 'ProcessStatus' and, if an error
-- occurred, pop up an error message of some kind.
loginPrompt :: (String -> IO (Maybe ProcessStatus)) -> IO ()
loginPrompt login = void $ login "test"


doGui :: ConfigFile -> IO ()
doGui c = void $ do
  putStrLn "gui starting"
  let login username = forkProcess (sessionRunner c username)
                       >>= getProcessStatus True False
  loginPrompt login
  putStrLn "gui stopping"
  return ()

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

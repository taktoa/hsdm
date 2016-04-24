module Main where

import           Control.Concurrent.MVar
import           Control.Exception
import           Control.Exception.Lens
import           Control.Lens
import           Control.Monad
import           Graphics.X11.Xlib
import           System.Posix.Directory
import           System.Posix.PAM
import           System.Posix.Process
import           System.Posix.Signals
import           System.Posix.Types
import           System.Posix.User
import           System.Process

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
  let (uid, gid) = (userID entry, userGroupId entry)
  initGroups username gid
  setGroupID gid
  setUserID uid
  ( _, _, _, ph) <- createProcess $ shell "id"
  code <- waitForProcess ph
  return entry

pamtest :: String -> String -> IO ()
pamtest username password = do
  foo <- authenticate "slim" username password
  case foo of
    Left code -> print $ pamCodeDetails code
    Right () -> do
      _ <- switchUser username
      return ()
  return ()

runner :: MVar Bool -> IO ()
runner mutex = do
  let args = [ "-nolisten", "tcp"
             ,":1"
             , "-fp", "/nix/store/cidl7bc0npkpjq1rv0mvbc6bf1yhsk00-font-misc-misc-1.1.2/lib/X11/fonts/misc/,/nix/store/9jx0cdw81f3alg6lbqzgxkjxxlci1709-font-cursor-misc-1.0.3/lib/X11/fonts/misc/"
             , "-depth", "24" ]
  installHandler sigUSR1 Ignore Nothing
  executeFile "/nix/store/4b12d4sypix8b2h8p1js5lxz290gs736-xorg-server-1.17.4/bin/Xorg" False args Nothing
  return ()

serverup :: MVar Bool -> IO ()
serverup mutex = void $ putMVar mutex True

childDead :: MVar Bool -> IO ()
childDead mutex = void $ putMVar mutex False

startX :: IO () -> IO ()
startX = do
  mutex <- newEmptyMVar
  installHandler sigUSR1 (CatchOnce $ serverup mutex) Nothing
  installHandler sigCHLD (Catch $ childDead mutex) Nothing
  pid <- forkProcess $ runner mutex
  result <- takeMVar mutex
  if result
    then f >> signalProcess sigTERM pid
    else error "fixme"

sessionRunner :: String -> IO ()
sessionRunner username = do
  entry <- switchUser "test"
  --let cmd = RawCommand "/nix/store/idm1067y9i6m87crjqrbamdsq2ma5r7p-bash-4.3-p42/bin/bash" [ "/nix/store/sbmm3fpgh5sgwhsaaq9k9v66xf8019nh-xsession", "xfce" ]
  let cmd = RawCommand "/run/current-system/sw/bin/xterm" [ ]
  --let cmd = RawCommand "/run/current-system/sw/bin/strace" [ "-ff", "-o", "/home/test/logfiles3", "-s", "90000", "/run/current-system/sw/bin/xterm"]
  let env = [ ("DISPLAY", ":1")
            , ("HOME", homeDirectory entry)
            , ("USER", username)
            , ("LOGNAME", username)
            , ("SHELL", userShell entry)
            , ("XDG_RUNTIME_DIR", "/run/user/1100") ]
  let proc1 = CreateProcess cmd Nothing (Just env) Inherit Inherit Inherit True False False
  changeWorkingDirectory $ homeDirectory entry
  (_, _, _, ph) <- createProcess proc1
  waitForProcess ph

doGui :: IO ()
doGui = void $ do
  pid <- forkProcess (sessionRunner "test")
  status <- getProcessStatus True False pid
  -- FIXME: use status for something
  return ()

main :: IO ()
main = changeWorkingDirectory "/" >> startX doGui

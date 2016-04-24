module Main where

import Graphics.X11.Xlib
import System.Posix.PAM
import System.Posix.User
import System.Posix.Process
import System.Process
import System.Posix.Directory
import System.Posix.Types
import System.Posix.Signals
import Control.Concurrent.MVar
import Control.Lens
import Control.Exception
import Control.Exception.Lens
import Control.Monad

oldmain :: IO ()
oldmain = do
  display <- openDisplay ""
  print display
  let
    root = defaultRootWindow display
    default_screen = defaultScreen display
    --d :: Ptr CChar
    --image = createImage display (defaultVisual display default_screen) (defaultDepth display default_screen) zPixmap 0 d 640 480 8 0
  w <- createSimpleWindow display root 0 0 480 480 0 0 0;
  gc <- createGC display w
  freeGC display gc
  mapWindow display w
  flush display
  print root
  getLine >>= putStrLn
  closeDisplay display
  return ()

initgroups :: String -> GroupID -> IO ()
initgroups username group = do
  allGroups <- getAllGroupEntries
  let memberGroups = (group :) . map groupID $ filter (elem username . groupMembers) allGroups
  setGroups memberGroups
  return ()

switchUser :: String -> IO UserEntry
switchUser username = do
  entry <- getUserEntryForName username
  initgroups username $ userGroupID entry
  setGroupID $ userGroupID entry
  setUserID $ userID entry
  ( _, _, _, proc) <- createProcess $ shell "id"
  code <- waitForProcess proc
  return entry

pamtest :: IO ()
pamtest = do
  let
    username = "test"
    password = "test"
  foo <- authenticate "slim" username password
  case foo of
    Left code -> print $ pamCodeDetails code
    Right () -> do
      _ <- switchUser username
      return ()
  return ()

runner :: MVar Bool -> IO ()
runner mutex = do
  let
    args :: [String]
    args = [
      "-nolisten", "tcp"
      ,":1"
      , "-fp", "/nix/store/cidl7bc0npkpjq1rv0mvbc6bf1yhsk00-font-misc-misc-1.1.2/lib/X11/fonts/misc/,/nix/store/9jx0cdw81f3alg6lbqzgxkjxxlci1709-font-cursor-misc-1.0.3/lib/X11/fonts/misc/"
      , "-depth", "24"
      ]
  installHandler sigUSR1 Ignore Nothing
  --executeFile "/nix/store/4379v653klzrrkhk0wfjb89ksg1bg422-tightvnc-1.3.10/bin/Xvnc" False args Nothing
  executeFile "/nix/store/4b12d4sypix8b2h8p1js5lxz290gs736-xorg-server-1.17.4/bin/Xorg" False args Nothing
  return ()

serverup :: MVar Bool -> IO ()
serverup mutex = do
  putMVar mutex True
  return ()

childDead :: MVar Bool -> IO ()
childDead mutex = do
  putMVar mutex False
  return ()

startX :: (Bool -> IO ()) -> IO ()
startX f = do
  mutex <- newEmptyMVar
  installHandler sigUSR1 (CatchOnce $ serverup mutex) Nothing
  installHandler sigCHLD (Catch $ childDead mutex) Nothing
  pid <- forkProcess $ runner mutex
  result <- takeMVar mutex
  f result
  signalProcess sigTERM pid
  return ()

block :: IO ()
block = void $ do
  getLine >>= putStrLn

session_runner :: String -> IO ()
session_runner username = do
  entry <- switchUser "test"
  let
    --cmd = RawCommand "/nix/store/idm1067y9i6m87crjqrbamdsq2ma5r7p-bash-4.3-p42/bin/bash" [ "/nix/store/sbmm3fpgh5sgwhsaaq9k9v66xf8019nh-xsession", "xfce" ]
    cmd = RawCommand "/run/current-system/sw/bin/xterm" [ ]
    --cmd = RawCommand "/run/current-system/sw/bin/strace" [ "-ff", "-o", "/home/test/logfiles3", "-s", "90000", "/run/current-system/sw/bin/xterm"]
    env = [
      ("DISPLAY",":1")
      ,("HOME", homeDirectory entry)
      ,("USER", username)
      ,("LOGNAME", username)
      ,("SHELL", userShell entry)
      ,("XDG_RUNTIME_DIR", "/run/user/1100")
      ]
    proc1 = CreateProcess cmd Nothing (Just env) Inherit Inherit Inherit True False False
  changeWorkingDirectory $ homeDirectory entry
  (_, _, _, ph) <- createProcess proc1
  waitForProcess ph
  return ()

doGui :: Bool -> IO ()
doGui _ = void $ do
  pid <- forkProcess $ session_runner "test"
  status <- getProcessStatus True False pid
  return ()

main :: IO ()
main = do
  changeWorkingDirectory "/"
  startX doGui
  return ()

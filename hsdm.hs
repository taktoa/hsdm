module Main where

import Graphics.X11.Xlib
import Foreign.C.Types
import Foreign
import System.Posix.PAM
import System.Posix.User
import System.Process
import System.Posix.Types

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

switchUser :: String -> IO ()
switchUser username = do
  entry <- getUserEntryForName username
  initgroups username $ userGroupID entry
  setGroupID $ userGroupID entry
  setUserID $ userID entry
  ( _, _, _, proc) <- createProcess $ shell "id"
  code <- waitForProcess proc
  return ()

main :: IO ()
main = do
  let
    username = "test"
    password = "test"
  foo <- authenticate "slim" username password
  case foo of
    Left code -> print $ pamCodeDetails code
    Right () -> switchUser username
  return ()

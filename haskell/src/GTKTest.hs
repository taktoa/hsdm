module Main where

import           Data.Monoid
import           Data.Text
import           System.HSDM.GTKRedo
import           System.HSDM.PAM

usernameEntered :: LoginCB
usernameEntered user callback = do
  putStrLn $ "got user " <> show user
  callback [PamMessage "Password:" PamPromptEchoOff]

main :: IO ()
main = runGUI usernameEntered gtkGreeter
--System.HSDM.GTKRedo.main

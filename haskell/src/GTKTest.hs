module Main where

import           Data.Monoid
import           Data.Text
import qualified System.HSDM.GTKRedo as GTKRedo
import           System.HSDM.PAM

usernameEntered :: GTKRedo.LoginCB
usernameEntered user callback = do
  putStrLn $ "got user " <> show user
  callback [PamMessage "Password:" PamPromptEchoOff]

main :: IO ()
main = GTKRedo.main
--runGUI usernameEntered gtkGreeter

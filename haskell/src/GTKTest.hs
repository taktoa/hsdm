module Main where

import           Data.Monoid
import           System.HSDM.GTKRedo
import           System.HSDM.UI
import Data.Text
import System.HSDM.PAM

data Callbacks = Callbacks
instance HSDMCallbacks Callbacks where
  usernameEntered g c user = do
    putStrLn $ "got user " <> show user
    reply <- tokenRequest g [PamMessage "Password:" PamPromptEchoOff]
    return ()

main :: IO ()
main = do
  foo <- mkState
  let cb = Callbacks
  runGui foo cb
--System.HSDM.GTKRedo.main

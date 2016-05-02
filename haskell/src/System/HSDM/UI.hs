module System.HSDM.UI where

import System.HSDM.PAM
import Data.Text

class HSDMCallbacks c where
  usernameEntered :: (HSDMGui g) => g -> c -> Text -> IO ()

class HSDMGui g where
  runGui :: (HSDMCallbacks a) => g -> a -> IO ()
  tokenRequest :: g -> [PamMessage] -> IO [PamResponse]

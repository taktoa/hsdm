{-# LANGUAGE PackageImports #-}

module System.HSDM.GTKRedo where

-- FIXME: remove PackageImport when done
import           "gtk3" Graphics.UI.Gtk

import           Data.Text       (Text)
import qualified Data.Text       as T

data Login = Login { _username :: Text
                   , _password :: Text }
             deriving (Eq, Show, Read)

main :: IO ()
main = loginScreen (\(Login u p) ->
                      putStrLn $ mconcat [ "button pressed: "
                                         , "username = ", show u, ", "
                                         , "password = ", show p ])

loginScreen :: (Login -> IO ()) -> IO ()
loginScreen callback = do
  initGUI

  builder <- builderNew
  builderAddFromFile builder "res/hsdm.ui"

  window  <- builderGetObject builder castToWindow      "window"
  userBuf <- builderGetObject builder castToEntryBuffer "usernameBuf"
  passBuf <- builderGetObject builder castToEntryBuffer "passwordBuf"
  button  <- builderGetObject builder castToButton      "submit"

  let textBufferGetAll b = get b entryBufferText

  on button buttonActivated $ do
    username <- textBufferGetAll userBuf
    password <- textBufferGetAll passBuf
    callback (Login username password)

  on window objectDestroy mainQuit

  widgetShowAll window
  mainGUI

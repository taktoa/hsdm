{-# LANGUAGE PackageImports #-}

module System.HSDM.GTKRedo where

-- FIXME: remove PackageImport when done
import           "gtk3" Graphics.UI.Gtk

main = do
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
    putStrLn $ mconcat [ "button pressed: "
                       , "username = ", username, ", "
                       , "password = ", password ]

  on window objectDestroy mainQuit

  widgetShowAll window
  mainGUI

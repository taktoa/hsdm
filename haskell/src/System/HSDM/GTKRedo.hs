{-# LANGUAGE PackageImports #-}

module System.HSDM.GTKRedo where

-- FIXME: remove PackageImport when done
import           "gtk3" Graphics.UI.Gtk

import           Data.Text       (Text)
import qualified Data.Text       as T
import           System.HSDM.UI
import System.HSDM.PAM


data GtkState = GtkState {  _window :: Window
                          , _userBuf :: EntryBuffer
                          , _passBuf :: EntryBuffer
                          , _button :: Button
                          , _label :: Label }
instance HSDMGui GtkState where
  runGui g c = loginScreen g c (\(Login u p) ->
                            putStrLn $ mconcat [ "button pressed: "
                                               , "username = ", show u, ", "
                                               , "password = ", show p ])
  tokenRequest g msgs = go msgs
    where
      go [PamMessage msg style] = do
        labelSetText (_label g) msg
        return [ PamResponse "foo" ]
--    putStrLn "token req"
    -- open pw prompt
    --mainIteration

data Login = Login { _username :: Text
                   , _password :: Text }
             deriving (Eq, Show, Read)

mkState :: IO GtkState
mkState = do
  initGUI

  builder <- builderNew
  builderAddFromFile builder "res/hsdm.ui"

  window  <- builderGetObject builder castToWindow      "window"
  userBuf <- builderGetObject builder castToEntryBuffer "usernameBuf"
  passBuf <- builderGetObject builder castToEntryBuffer "passwordBuf"
  button  <- builderGetObject builder castToButton      "submit"
  label   <- builderGetObject builder castToLabel       "passwordLabel"

  return $ GtkState window userBuf passBuf button label

loginScreen :: (HSDMCallbacks c) => GtkState -> c -> (Login -> IO ()) -> IO ()
loginScreen g c callback = do

  let textBufferGetAll b = get b entryBufferText

  on (_button g) buttonActivated $ do
    username <- textBufferGetAll $ _userBuf g
    password <- textBufferGetAll $ _passBuf g
    callback (Login username password)
    usernameEntered g c username

  on (_window g) objectDestroy mainQuit

  widgetShowAll $ _window g
  mainGUI

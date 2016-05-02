{-# LANGUAGE PackageImports #-}

module System.HSDM.GTKRedo where

-- FIXME: remove PackageImport when done
import           "gtk3" Graphics.UI.Gtk

import           Data.Text       (Text)
import qualified Data.Text       as T
import           System.HSDM.PAM

type AuthRequest = PamMessage

type LoginCB = Text -> ([AuthRequest] -> IO ()) -> IO ()

newtype Greeter = Greeter { _greeter :: LoginCB -> IO () }

runGUI :: LoginCB -> Greeter -> IO ()
runGUI cb (Greeter g) = g cb

gtkGreeter :: Greeter
gtkGreeter = Greeter $ \callback -> do
  initGUI

  builder <- builderNew
  builderAddFromFile builder "res/hsdm.ui"

  window <- builderGetObject builder castToWindow      "window"
  buffer <- builderGetObject builder castToEntryBuffer "buffer"
  button <- builderGetObject builder castToButton      "submit"
  label  <- builderGetObject builder castToLabel       "label"

  let textBufferGetAll b = get b entryBufferText

  let handleAuth (PamMessage msg style) = do labelSetText label msg
                                             return ()
                                             -- putStrLn "token req"
                                             -- open pw prompt
                                             -- mainIteration

  on button buttonActivated $ do
    username <- textBufferGetAll buffer
    callback username $ mapM_ handleAuth

  on window objectDestroy mainQuit

  widgetShowAll window
  mainGUI


{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}

module System.HSDM.X11Redo where

import           Control.Concurrent
import           Control.Lens
import           Control.Monad
import           Data.Array.Storable
import           Data.Bits
import           Data.Bits
import           Data.ByteString             (ByteString, useAsCString)
import qualified Data.ByteString             as BS
import qualified Data.Colour.Names           as C
import qualified Data.Colour.Names           as C
import           Data.Default
import           Data.Foldable               (toList)
import           Data.Int
import           Data.Maybe
import           Data.Semigroup
import           Data.Set                    (Set)
import qualified Data.Set                    as Set
import           Data.Typeable               (Typeable)
import           Data.Word
import           Diagrams
import           Diagrams.Backend.Rasterific
import           Graphics.Rasterific.Svg     (loadCreateFontCache,
                                              renderSvgDocument)
import           Graphics.Svg                (Document, loadSvgFile)
import           Graphics.Text.TrueType      (FontCache)
import           Graphics.X11.Xlib
import           Graphics.X11.Xlib.Extras
import           Linear
import           System.Exit
import qualified System.HSDM.X11             as Old
import           System.Posix.Process

data State = State { _doc   :: Document
                   , _cache :: FontCache }
makeLenses ''State

makeState :: IO State
makeState = do
  f <- loadSvgFile "../circle.svg"
  case f of
    Nothing -> error "unable to load svg"
    Just d  -> State d <$> loadCreateFontCache "/tmp/fonty-texture-cache"

mainRewrite :: IO ()
mainRewrite = do
  state <- makeState
  dpy <- openDisplay ""
  let scr = defaultScreen dpy
  let (w, h) = ( fromIntegral $ displayWidth  dpy scr
               , fromIntegral $ displayHeight dpy scr )
  win <- mkFramelessWindow dpy (V2 0 0) (V2 600 600)
  Old.initializeX11Events dpy win
  typeA        <- intern dpy "ATOM"
  cardinalA    <- intern dpy "CARDINAL"
  window_typeA <- intern dpy "_NET_WM_WINDOW_TYPE"
  dialogA      <- intern dpy "_NET_WM_WINDOW_TYPE_DIALOG"
  pidA         <- intern dpy "_NET_WM_PID"
  allowedA     <- intern dpy "_NET_WM_ALLOWED_ACTIONS"
  closeA       <- intern dpy "_NET_WM_ACTION_CLOSE"
  let changeProp = changeProperty32 dpy win
  let changePropR k t = changeProp k t propModeReplace
  let changePropA k t = changeProp k t propModeAppend
  pid <- fromIntegral <$> getProcessID
  changePropR window_typeA typeA     [ dialogA ]
  changePropR pidA         cardinalA [ pid     ]
  changePropA allowedA     cardinalA [ closeA  ]
  mapWindow dpy win
  img <- renderScene state (V2 w h)
  Old.drawJPImage dpy win img
  eventLoop state dpy win
  closeDisplay dpy
  exitSuccess
  where
    intern :: (Integral i) => Display -> String -> IO i
    intern dpy x = fromIntegral <$> internAtom dpy x False

mkFramelessWindow :: Display -> V2 Position -> V2 Dimension -> IO Window
mkFramelessWindow dpy (V2 x y) (V2 w h) = do
  rw <- rootWindow dpy $ defaultScreen dpy
  backgroundColor <- Old.initColor dpy "purple"
  borderColor     <- Old.initColor dpy "green"
  createSimpleWindow dpy rw x y w h 0 borderColor backgroundColor

renderScene :: State -> V2 Int -> IO Old.JImage
renderScene (State d c) (V2 w h) = do
  (finalImage, _) <- renderSvgDocument c (Just (w, h)) 96 d
  return finalImage

eventLoop :: State -> Display -> Window -> IO ()
eventLoop state dpy win = allocaXEvent go
  where
    go xev = do nextEvent dpy xev
                shouldQuit <- Old.makeX11Event dpy xev >>= handle . fromJust
                unless shouldQuit $ go xev
    handle e@Old.ExposeNotify  {} = do let Just pos = e ^? Old.xev_pos
                                       dprint pos
                                       img <- renderScene state (V2 600 600)
                                       Old.drawJPImage dpy win img
                                       loopA
    handle e@Old.ButtonPress   {} = dprint e >> loopA
    handle e@Old.ButtonRelease {} = dprint e >> loopA
    handle e@Old.KeyPress      {} = if e ^? Old.xev_key == Just xK_q
                                    then quitA
                                    else dprint e >> loopA
    handle e@Old.KeyRelease    {} = dprint e >> loopA
    handle _                      = loopA
    loopA = return False
    quitA = return True

debugEnabled :: Bool
debugEnabled = False

dprint :: (Show s) => s -> IO ()
dprint = when debugEnabled . print

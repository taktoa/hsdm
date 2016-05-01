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
  img <- renderScene state (V2 w h) (V2 0 0)
  Old.drawJPImage dpy win img
  eventLoop state dpy win
  closeDisplay dpy
  exitSuccess
  where
    intern :: (Integral i) => Display -> String -> IO i
    intern dpy x = fromIntegral <$> internAtom dpy x False

mkFramelessWindow :: Display -> V2 Position -> V2 Dimension -> IO Window
mkFramelessWindow dpy (V2 x y) (V2 w h) = do
  let scr = defaultScreenOfDisplay dpy
  rw <- rootWindow dpy (defaultScreen dpy)
  let visual = defaultVisualOfScreen scr
  let depth  = defaultDepthOfScreen scr
  let attrmask = cWOverrideRedirect .|. cWBackPixel
  backgroundColor <- Old.initColor dpy "purple"
  borderColor     <- Old.initColor dpy "green"
  createSimpleWindow dpy rw x y w h 0 borderColor backgroundColor

renderScene :: State -> V2 Int -> V2 Int -> IO Old.JImage
renderScene (State d c) (V2 x y) (V2 w h) = do
  (finalImage, _) <- renderSvgDocument c (Just (w, h)) 96 d
  return finalImage

eventLoop :: State -> Display -> Window -> IO ()
eventLoop state dpy win = allocaXEvent $ go 1000
  where
    go 0 _   = return ()
    go n xev = do nextEvent dpy xev
                  Just parsed <- Old.makeX11Event dpy xev
                  shouldQuit <- handle parsed
                  unless shouldQuit $ go (n - 1) xev
    handle e@Old.MotionNotify  {} = do let Just pos = e ^? Old.xev_pos
                                       dprint pos
                                       img <- renderScene state pos (V2 600 600)
                                       Old.drawJPImage dpy win img
                                       return False
    handle e@Old.ButtonPress   {} = dprint e >> return False
    handle e@Old.ButtonRelease {} = dprint e >> return False
    handle e@Old.KeyPress      {} = dprint e >> return True
    handle e@Old.KeyRelease    {} = dprint e >> return False
    handle _                      = return False


debugEnabled :: Bool
debugEnabled = False

dprint :: (Show s) => s -> IO ()
dprint = when debugEnabled . print

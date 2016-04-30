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
import           Linear
import           System.Exit
import qualified System.HSDM.X11             as Old

data State = State { _doc   :: Document
                   , _cache :: FontCache }
makeLenses ''State

makeState :: IO State
makeState = do
  f <- loadSvgFile "../circle.svg"
  case f of
    Nothing -> error "unable to load svg"
    Just d  -> State d <$> loadCreateFontCache "fonty-texture-cache"

mainRewrite :: IO ()
mainRewrite = do
  state <- makeState
  dpy <- openDisplay ""
  let scr = defaultScreen dpy
  let (width, height) = ( fromIntegral $ displayWidth  dpy scr
                        , fromIntegral $ displayHeight dpy scr )
  win <- mkFramelessWindow dpy (0,0) (width,height)
  Old.initializeX11Events dpy win
  mapWindow dpy win
  img <- renderScene state (fromIntegral width, fromIntegral height) (0,0)
  Old.drawJPImage dpy win img
  eventLoop state dpy win
  closeDisplay dpy
  exitSuccess

mkFramelessWindow :: Display -> (Old.Pos,Old.Pos) -> (Old.Dim,Old.Dim) -> IO Window
mkFramelessWindow dpy (x,y) (w,h) = do
  let scr = defaultScreenOfDisplay dpy
  rw <- rootWindow dpy (defaultScreen dpy)
  let visual = defaultVisualOfScreen scr
  let depth  = defaultDepthOfScreen scr
  let attrmask = cWOverrideRedirect .|. cWBackPixel
  backgroundColor <- Old.initColor dpy "purple"
  borderColor     <- Old.initColor dpy "green"
  --allocaSetWindowAttributes $ \attrs -> do
  --  set_override_redirect attrs True
  --  set_background_pixel attrs backgroundColor
  --  createWindow dpy rw x y w h 0 depth inputOutput visual attrmask attrs
  createSimpleWindow dpy rw x y w h 0 borderColor backgroundColor

renderScene :: State -> (Int, Int) -> (Int, Int) -> IO Old.JImage
renderScene state (w,h) (x,y) = do
  --let cursor = translate (V2 (fromIntegral x) (fromIntegral y)) $ fc C.blue $ circle 10
  --let topleft = translate (V2 0 0) $ fc C.green $ circle 5
  --let bottomright = translate (V2 100 100) $ fc C.red $ circle 5
  --let box = rect 200 100 # lc C.red
  --let scene = box
  --let border = rect w h # lc C.green
  (finalImage, _) <- renderSvgDocument (state ^. cache) (Just (w, h)) 96 (state ^. doc)
  return finalImage
  --Old.renderRast $ reflectY $ cursor `atop` (scene `atop` border)

eventLoop :: State -> Display -> Window -> IO ()
eventLoop state dpy win = allocaXEvent $ go 1000
  where
    go 0 _ = return ()
    go limit evtPtr = do
      nextEvent dpy evtPtr
      parsed <- Old.makeX11Event dpy evtPtr
      res <- handle parsed
      when res $ go (limit -1) evtPtr
    handle (Just evt) = do
      level2 evt
    level2 (Old.MotionNotify ev) = do
      let V2 x y = Old._X11Event_cpos ev
      print $ [ x, y ]
      --let step1 = circle 5
      --let step2 = fc C.blue step1
      --let cursor = translate (V2 (fromIntegral x) (fromIntegral y))
      --             $ fc C.blue $ circle 5
      --let topleft = translate (V2 0 0) $ fc C.green $ circle 10
      --let scene = cursor `atop` topleft
      --let image = Old.renderRast $ cursor `atop` rect 500 500
      --Old.drawJPImage dpy win image
      img <- renderScene state (800, 800) (x,y)
      Old.drawJPImage dpy win img
      return True
    level2 (Old.ButtonPress ev) = do
      print ev
      return True
    level2 (Old.ButtonRelease ev) = do
      print ev
      return True
    level2 (Old.KeyPress ev) = do
      print ev
      return False
    level2 (Old.KeyRelease ev) = do
      print ev
      return True

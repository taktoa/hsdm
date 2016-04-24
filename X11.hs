module Main where

import           Control.Concurrent
import           Control.Monad
import           Data.Bits
import           Data.Int
import           Data.Word
import           Graphics.X11.Xlib
import           System.Exit
import           System.Random

initColor :: Display -> String -> IO Pixel
initColor dpy color = do
  let colormap = defaultColormap dpy (defaultScreen dpy)
  (approx, _) <- allocNamedColor dpy colormap color
  return $ color_pixel approx

mkUnmanagedWindow :: Display
                  -> Screen
                  -> Window
                  -> Position
                  -> Position
                  -> Dimension
                  -> Dimension
                  -> IO Window
mkUnmanagedWindow dpy scr rw x y w h = do
  let visual = defaultVisualOfScreen scr
  let depth  = defaultDepthOfScreen scr
  let attrmask = cWOverrideRedirect .|. cWBorderPixel .|. cWBackPixel
  backgroundColor <- initColor dpy "black"
  borderColor     <- initColor dpy "red"
  allocaSetWindowAttributes $ \attributes -> do
    set_override_redirect attributes True
    set_background_pixel attributes backgroundColor
    set_border_pixel attributes borderColor
    createWindow dpy rw x y w h 1 depth inputOutput visual attrmask attributes

type Point32 = (Int32, Int32)
type Vector32 = (Word32, Word32)

drawRect :: Display -> Window -> Point32 -> Vector32 -> String -> IO ()
drawRect dpy win (x, y) (w, h) colorName = do
  gc <- createGC dpy win
  color <- initColor dpy colorName
  setForeground dpy gc color
  fillRectangle dpy win gc x y w h
  freeGC dpy gc

drawShit :: Display -> Window -> Vector32 -> Int -> IO ()
drawShit dpy win (width, height) num = getStdGen >>= flip go num
  where
    colors = ["red", "green", "blue"]
    rectangle = drawRect dpy win
    coerceInt = fromIntegral
    go :: RandomGen g => g -> Int -> IO ()
    go _ 0 = return ()
    go g n = do let (x, gen0) = randomR (0, width)  g
                let (y, gen1) = randomR (0, height) gen0
                let (w, gen2) = randomR (0, width)  gen1
                let (h, gen3) = randomR (0, height) gen2
                let (c, gen4) = randomR (0, length colors - 1) gen3
                rectangle (coerceInt x, coerceInt y) (w, h) (colors !! c)
                sync dpy False
                threadDelay 10000
                go gen4 (n - 1)

main :: IO ()
main = do dpy <- openDisplay ""
          let dflt = defaultScreen dpy
          let scr = defaultScreenOfDisplay dpy
          let (width, height) = (400, 400)
          rootw <- rootWindow dpy dflt
          win <- mkUnmanagedWindow dpy scr rootw 0 0 width height
          mapWindow dpy win
          drawShit dpy win (width, height) 500
          sync dpy False
          void getLine
          exitSuccess

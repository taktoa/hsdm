{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import qualified Codec.Picture.Png        as JP
import qualified Codec.Picture.Types      as JP
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Reader
import           Data.Array.Storable
import           Data.Bits
import           Data.ByteString          (ByteString, useAsCString)
import qualified Data.ByteString          as BS
import           Data.Default
import           Data.Int
import           Data.Maybe
import           Data.Word
import           Foreign                  hiding (newArray)
import           Foreign.C.String         (CString)
import qualified Graphics.X11.Xlib        as X
import qualified Graphics.X11.Xlib.Extras as X
import           System.Exit
import           System.IO
import           System.Random

type Pos = X.Position
type Dim = X.Dimension
type Image = X.Image
type Screen = X.Screen
type Window = X.Window
type Display = X.Display

class MonadX11 m where
  flush       :: m ()
  sync        :: m ()
  syncDiscard :: m ()
  getGeometry :: m (Dim, Dim)

class MonadGC m where
  putImage :: Image -> Pos -> Pos -> Dim -> Dim -> IO ()

newtype XM a = XM (ReaderT (Display, Window) IO a)
             deriving (Functor, Applicative, Monad, MonadIO)

runXM :: XM a -> IO a
runXM (XM r) = do
  dpy <- X.openDisplay ""
  rootw <- X.rootWindow dpy (X.defaultScreen dpy)
  attr <- X.getWindowAttributes dpy rootw
  let winW = fromIntegral $ X.wa_width attr
  let winH = fromIntegral $ X.wa_height attr
  win <- X.createSimpleWindow dpy rootw 0 0 winW winH 1 0 0
  X.selectInput dpy win X.exposureMask
  X.mapWindow dpy win
  result <- runReaderT r (dpy, win)
  X.closeDisplay dpy
  return result

wrapXM :: _
wrapXM = XM . ReaderT

instance MonadX11 XM where
  flush = XM $ ReaderT $ \(dpy, _) -> X.flush dpy
  sync  = XM $ ReaderT $ \(dpy, _) -> X.sync dpy False
  syncDiscard = XM $ ReaderT $ \(dpy, _) -> X.sync dpy True
  getGeometry = undefined

newtype GCM a = GCM (ReaderT X.GC XM a)
              deriving (Functor, Applicative, Monad, MonadIO)

runGCM :: GCM a -> XM a
runGCM (GCM r) = do
  gc <- XM $ ReaderT $ uncurry X.createGC
  result <- runReaderT r gc
  XM $ ReaderT $ \(dpy, _) -> X.freeGC dpy gc
  return result

instance MonadX11 GCM where
  flush = GCM $ lift $ flush

instance MonadGC GCM where


data XImg = XImg { xImage     :: !X.Image
                 , xImageData :: !(StorableArray (Int, Int, Int) Word8)
                 , xImageH    :: !Int
                 , xImageW    :: !Int }

testDrawImage :: FilePath -> IO ()
testDrawImage file = do
  edimg <- JP.decodePng <$> BS.readFile file
  case edimg of
    Left  err  -> putStrLn err
    Right dimg -> runXM $ do
      fromMaybe
        (putStrLn "FAILURE")
        (drawJPImage <$> convertDynImage dimg)
      flush
      sync
      flush
      liftIO getLine
      flush

drawImg :: XImg -> XM ()
drawImg ximg = runGCM $ do
  (w, h) <- getGeometry
  putImage (xImage ximg) 0 0 w h
  freeGC dpy gc
  syncDiscard

initColor :: String -> XM Pixel
initColor color = do
  colormap <- defaultColormap
  -- let colormap = defaultColormap dpy (defaultScreen dpy)
  (apros, _) <- allocNamedColor dpy colormap color
  return $ color_pixel apros

type ILArray = StorableArray (Int, Int, Int) Word8

fromJPData :: JP.Image JP.PixelRGBA8 -> XM ILArray
fromJPData img = liftIO $ newArray ((0, 0, 0), (h - 1, w - 1, 3)) 0 >>= setValues
  where
    w = JP.imageWidth  img
    h = JP.imageHeight img
    getC :: Int -> JP.PixelRGBA8 -> Word8
    getC 0 (JP.PixelRGBA8 r _ _ _) = r
    getC 1 (JP.PixelRGBA8 _ g _ _) = g
    getC 2 (JP.PixelRGBA8 _ _ b _) = b
    getC 3 (JP.PixelRGBA8 _ _ _ a) = a
    getC _ _                       = error "Invalid color channel"
    getPix = JP.pixelAt img
    setValues :: ILArray -> IO ILArray
    setValues arr = getBounds arr >>= go . range
      where
        go :: [(Int, Int, Int)] -> IO ILArray
        go []             = return arr
        go ((y, x, c):xs) = do writeArray arr (y, x, c) $ getC c $ getPix x y
                               go xs

makeXImage :: JP.Image JP.PixelRGBA8 -> IO XImg
makeXImage image = do

  d <- fromJPData image
  xid <- mapIndices bs mapIdx d
  withStorableArray xid (ci xid . castPtr)
  where
    w, h :: Integral i => i
    w = fromIntegral $ JP.imageWidth  image
    h = fromIntegral $ JP.imageHeight image
    scr = defaultScreen
    dep = defaultDepth  dpy scr
    vis = defaultVisual dpy scr
    ci :: StorableArray (Int, Int, Int) Word8 -> CString -> IO XImg
    ci bytes p = let makeXI x = XImg x bytes w h
                 in makeXI <$> createImage dpy vis dep zPixmap 0 p w h 32 0
    bs = ((0, 0, 0), (h - 1, w - 1, 3))
    mapIdx (y, x, c) = let helper 0 = 2
                           helper 1 = 1
                           helper 2 = 0
                           helper 3 = 0
                       in (y, x, helper c)

drawJPImage :: JP.Image JP.PixelRGBA8 -> XM ()
drawJPImage image = makeXImage image >>= drawImg

convertDynImage :: JP.DynamicImage -> Maybe (JP.Image JP.PixelRGBA8)
convertDynImage (JP.ImageY8     img) = Just $ JP.promoteImage img
convertDynImage (JP.ImageYA8    img) = Just $ JP.promoteImage img
convertDynImage (JP.ImageRGB8   img) = Just $ JP.promoteImage img
convertDynImage (JP.ImageRGBA8  img) = Just $ JP.promoteImage img
convertDynImage (JP.ImageY16      _) = Nothing
convertDynImage (JP.ImageYF       _) = Nothing
convertDynImage (JP.ImageYA16     _) = Nothing
convertDynImage (JP.ImageRGB16    _) = Nothing
convertDynImage (JP.ImageRGBF     _) = Nothing
convertDynImage (JP.ImageRGBA16   _) = Nothing
convertDynImage (JP.ImageYCbCr8   _) = Nothing
convertDynImage (JP.ImageCMYK8    _) = Nothing
convertDynImage (JP.ImageCMYK16   _) = Nothing














{-

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

createImageFromBS :: ( Integral depth
                     , Integral offset
                     , Integral dimension
                     , Integral padding
                     , Integral bytesPerLine )
                     => Display
                     -> Visual
                     -> depth
                     -> ImageFormat
                     -> offset
                     -> ByteString
                     -> dimension
                     -> dimension
                     -> padding
                     -> bytesPerLine
                     -> IO Image
createImageFromBS dpy vis depth fmt offset bs w h pad bpl = useAsCString bs go
  where
    c :: (Integral a, Num b) => a -> b
    c = fromIntegral
    (cdep, coff, cw, ch, cp, cb) = (c depth, c offset, c w, c h, c pad, c bpl)
    go cs = createImage dpy vis cdep fmt coff cs cw ch cp cb

createImageEasy :: (Integral dimension)
                   => Display
                   -> ByteString
                   -> dimension
                   -> dimension
                   -> IO Image
createImageEasy dpy bs w h
  = let scr = defaultScreen dpy
        vis = defaultVisual dpy scr
    in createImageFromBS dpy vis 0 xyBitmap 0 bs w h 0 8


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
          hdl <- openFile "output.xbm" ReadMode
          !xbm <- BS.hGetContents hdl
          hClose hdl
          img <- createImageEasy dpy xbm 16 7
          gc <- createGC dpy win
          putImage dpy win gc img 0 0 0 0 16 7
          freeGC dpy gc
          destroyImage img
          sync dpy False
          threadDelay (10 * 1000 * 1000)
          --void getLine
          exitSuccess


-}

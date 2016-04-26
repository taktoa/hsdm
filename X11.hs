{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module X11 where

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
import           Foreign                  (castPtr)
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

data ImageFmt = XYBitmap | XYPixmap | ZPixmap

fromImageFmt :: ImageFmt -> X.ImageFormat
fromImageFmt XYBitmap = X.xyBitmap
fromImageFmt XYPixmap = X.xyPixmap
fromImageFmt  ZPixmap =  X.zPixmap

class MonadX11 m where
  flush       :: m ()
  sync        :: m ()
  syncDiscard :: m ()
  getGeometry :: m (Dim, Dim)
  createImage :: ImageFmt -> Int -> CString -> Dim -> Dim -> Int -> Int -> m Image

class MonadGC m where
  putImage :: Image -> Pos -> Pos -> Dim -> Dim -> m ()

newtype XM a = XM (ReaderT (Display, Window) IO a)
             deriving (Functor, Applicative, Monad, MonadIO)

runXM :: XM a -> IO a
runXM (XM r) = do
  dpy <- X.openDisplay ""
  -- attr <- X.getWindowAttributes dpy rootw
  -- let winW = fromIntegral $ X.wa_width attr
  -- let winH = fromIntegral $ X.wa_height attr
  -- col <- initColor' dpy "#000000"
  -- win <- X.createSimpleWindow dpy rootw 0 0 winW winH 1 col col
  win <- mkUnmanagedWindow dpy 50 50 480 480
  X.selectInput dpy win X.exposureMask
  X.mapWindow dpy win
  result <- runReaderT r (dpy, win)
  X.closeDisplay dpy
  return result

wrapXM :: ((Display, Window) -> IO a) -> XM a
wrapXM = XM . ReaderT

getXM :: XM (Display, Window)
getXM = wrapXM return

instance MonadX11 XM where
  flush       = wrapXM $ \(dpy, _) -> X.flush dpy
  sync        = wrapXM $ \(dpy, _) -> X.sync dpy False
  syncDiscard = wrapXM $ \(dpy, _) -> X.sync dpy True
  getGeometry = wrapXM $ \(dpy, win) -> do
    (_, _, _, _, w, h, _) <- X.getGeometry dpy win
    return (w, h)
  createImage f o s w h p b = wrapXM $ \(dpy, _) -> do
    let ci = X.createImage dpy
    let scr = X.defaultScreen dpy
    let vis = X.defaultVisual dpy scr
    let dep = X.defaultDepth dpy scr
    let fmt = fromImageFmt f
    let off = fromIntegral o
    let pad = fromIntegral p
    let bpl = fromIntegral b
    liftIO $ ci vis dep fmt off s w h pad bpl


newtype GCM a = GCM (ReaderT X.GC XM a)
              deriving (Functor, Applicative, Monad, MonadIO)

runGCM :: GCM a -> XM a
runGCM (GCM r) = do
  gc <- XM $ ReaderT $ uncurry X.createGC
  result <- runReaderT r gc
  XM $ ReaderT $ \(dpy, _) -> X.freeGC dpy gc
  return result

instance MonadX11 GCM where
  flush                     = GCM $ lift flush
  sync                      = GCM $ lift sync
  syncDiscard               = GCM $ lift syncDiscard
  getGeometry               = GCM $ lift getGeometry
  createImage f o s w h p b = GCM $ lift $ createImage f o s w h p b

instance MonadGC GCM where
  putImage i x y w h = GCM $ ReaderT $ \gc -> do
    (dpy, win) <- getXM
    liftIO $ X.putImage dpy win gc i x y x y w h

type XImageArray = StorableArray (Int, Int, Int) Word8

data XImg = XImg { xImage     :: !X.Image
                 , xImageData :: !XImageArray
                 , xImageH    :: !Int
                 , xImageW    :: !Int }

testDrawImage :: FilePath -> IO ()
testDrawImage file = do
  edimg <- JP.decodePng <$> BS.readFile file
  case edimg of
    Left  err  -> putStrLn err
    Right dimg -> runXM $ do
      fromMaybe
        (liftIO $ putStrLn "FAILURE")
        (drawImage <$> convertDynImage dimg)
      void $ liftIO getLine
      return ()

getGeometryJP :: JP.Image p -> (Int, Int)
getGeometryJP img = (JP.imageWidth img, JP.imageHeight img)

coerceInt :: (Integral a, Num b) => a -> b
coerceInt = fromIntegral

drawImage :: JP.Image JP.PixelRGBA8 -> XM ()
drawImage image = do arr <- xImage <$> converted
                     runGCM $ do
                       (w, h) <- getGeometry
                       putImage arr 0 0 w h
  where
    converted = do xid <- liftIO $ fromJPData image >>= mapIndices bs mapIdx
                   wrapXM $ \(dpy, _) -> withStorableArray xid (ci dpy xid . castPtr)
    (w, h) = getGeometryJP image
    ci :: Display -> XImageArray -> CString -> IO XImg
    ci dpy bytes p = do
      let ci =  dpy
      let scr = X.defaultScreen dpy
      let vis = X.defaultVisual dpy scr
      let dep = X.defaultDepth dpy scr
      let fmt = X.zPixmap
      arr <- X.createImage dpy vis dep fmt 0 p (coerceInt w) (coerceInt h) 32 0
      return $ XImg arr bytes w h
    bs :: ((Int, Int, Int), (Int, Int, Int))
    bs = ((0, 0, 0), (coerceInt (h - 1), coerceInt (w - 1), 3))
    mapIdx (y, x, c) = let helper 0 = 2
                           helper 1 = 1
                           helper 2 = 0
                           helper 3 = 0
                       in (y, x, helper c)

fromJPData :: JP.Image JP.PixelRGBA8 -> IO XImageArray
fromJPData img = newArray indexRange 0 >>= setValues img
  where
    indexRange = ((0, 0, 0), (h - 1, w - 1, 3))
    (w, h) = getGeometryJP img

setValues :: JP.Image JP.PixelRGBA8 -> XImageArray -> IO (XImageArray)
setValues img arr = getBounds arr >>= go . range
  where
    go :: [(Int, Int, Int)] -> IO (StorableArray (Int, Int, Int) Word8)
    go []             = return arr
    go ((y, x, c):xs) = writeArray arr (y, x, c) (getC c (getPix x y)) >> go xs
    getC :: Int -> JP.PixelRGBA8 -> Word8
    getC 0 (JP.PixelRGBA8 r _ _ _) = r
    getC 1 (JP.PixelRGBA8 _ g _ _) = g
    getC 2 (JP.PixelRGBA8 _ _ b _) = b
    getC 3 (JP.PixelRGBA8 _ _ _ a) = a
    getC _ _                       = error "Invalid color channel"
    getPix = JP.pixelAt img

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















mkUnmanagedWindow :: Display
                  -> Pos -> Pos
                  -> Dim -> Dim
                  -> IO Window
mkUnmanagedWindow dpy x y w h = do
  let scr = X.defaultScreenOfDisplay dpy
  rw <- X.rootWindow dpy (X.defaultScreen dpy)
  let visual = X.defaultVisualOfScreen scr
  let depth  = X.defaultDepthOfScreen scr
  let attrmask = X.cWOverrideRedirect .|. X.cWBorderPixel .|. X.cWBackPixel
  backgroundColor <- initColor' dpy "black"
  borderColor     <- initColor' dpy "red"
  X.allocaSetWindowAttributes $ \attrs -> do
    X.set_override_redirect attrs True
    X.set_background_pixel attrs backgroundColor
    X.set_border_pixel attrs borderColor
    X.createWindow dpy rw x y w h 1 depth X.inputOutput visual attrmask attrs

{-

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
























testDrawImage' :: FilePath -> IO ()
testDrawImage' file = do
  edimg <- JP.decodePng <$> BS.readFile file
  case edimg of
    Left  err  -> putStrLn err
    Right dimg -> do
      dpy <- X.openDisplay ""
      win <- mkUnmanagedWindow dpy 500 100 500 500
      X.selectInput dpy win X.exposureMask
      X.mapWindow dpy win
      fromMaybe
        (putStrLn "FAILURE")
        (drawJPImage' dpy win <$> convertDynImage dimg)
      _ <- getLine
      X.closeDisplay dpy

drawImg' :: X.Display -> X.Window -> XImg -> IO ()
drawImg' dpy win ximg = do
  gc <- X.createGC dpy win
  (_, _, _, w, h, _, _) <- X.getGeometry dpy win
  X.putImage dpy win gc (xImage ximg) 0 0 0 0 w h
  X.freeGC dpy gc

initColor' :: X.Display -> String -> IO X.Pixel
initColor' dpy color = do
  let colormap = X.defaultColormap dpy (X.defaultScreen dpy)
  (apros, _) <- X.allocNamedColor dpy colormap color
  return $ X.color_pixel apros

type ILArray = StorableArray (Int, Int, Int) Word8

fromJPData' :: JP.Image JP.PixelRGBA8 -> IO XImageArray
fromJPData' img = newArray ((0, 0, 0), (h - 1, w - 1, 3)) 0 >>= setValues
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
    setValues :: XImageArray -> IO XImageArray
    setValues arr = getBounds arr >>= go . range
      where
        go :: [(Int, Int, Int)] -> IO ILArray
        go []             = return arr
        go ((y, x, c):xs) = do writeArray arr (y, x, c) $ getC c $ getPix x y
                               go xs

makeXImage' :: X.Display -> JP.Image JP.PixelRGBA8 -> IO XImg
makeXImage' dpy image = do d <- fromJPData' image
                           xid <- mapIndices bs mapIdx d
                           withStorableArray xid (ci xid . castPtr)
  where
    w, h :: Integral i => i
    w = fromIntegral $ JP.imageWidth  image
    h = fromIntegral $ JP.imageHeight image
    scr = X.defaultScreen dpy
    dep = X.defaultDepth  dpy scr
    vis = X.defaultVisual dpy scr
    ci :: XImageArray -> CString -> IO XImg
    ci bytes p = let makeXI x = XImg x bytes w h
                 in makeXI <$> X.createImage dpy vis dep X.zPixmap 0 p w h 32 0
    bs = ((0, 0, 0), (h - 1, w - 1, 3))
    mapIdx (y, x, c) = let helper 0 = 2
                           helper 1 = 1
                           helper 2 = 0
                           helper 3 = 0
                       in (y, x, helper c)

drawJPImage' :: X.Display -> X.Window -> JP.Image JP.PixelRGBA8 -> IO ()
drawJPImage' dpy win image = makeXImage' dpy image >>= drawImg' dpy win

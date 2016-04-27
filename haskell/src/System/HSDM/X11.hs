{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module System.HSDM.X11 where

import qualified Codec.Picture.Png           as JP
import qualified Codec.Picture.Types         as JP
import           Control.Concurrent
import           Control.Lens
import           Control.Monad
import           Data.Array.Storable
import           Data.Bits
import           Data.ByteString             (ByteString, useAsCString)
import qualified Data.ByteString             as BS
import qualified Data.Colour.Names           as C
import           Data.Default
import           Data.Int
import           Data.Maybe
import           Data.Semigroup
import           Data.Set                    (Set)
import qualified Data.Set                    as Set
import           Data.Typeable               (Typeable)
import           Data.Word
import           Diagrams
import           Diagrams.Backend.Rasterific
import           Foreign                     (castPtr)
import           Foreign.C                   (CInt)
import           Foreign.C.String            (CString)
import           Graphics.Rasterific.Svg
import qualified Graphics.X11.Xlib           as X
import qualified Graphics.X11.Xlib.Extras    as X
import           System.Exit
import           System.IO
import           System.Random

main :: IO ()
main = testDrawImage

type Pos = X.Position
type Dim = X.Dimension
type Image = X.Image
type Screen = X.Screen
type Window = X.Window
type Display = X.Display

type XImageArray = StorableArray (Int, Int, Int) Word8

data XImg = XImg { xImage     :: X.Image
                 , xImageData :: XImageArray
                 , xImageH    :: Int
                 , xImageW    :: Int
                 }

type XDiagram = QDiagram Rasterific V2 Double Any

drawDia :: Display -> Window -> XDiagram -> IO ()
drawDia dpy win = drawJPImage dpy win
                  . renderDia Rasterific options
  where
    options = RasterificOptions (mkWidth 500)

example1, example2 :: XDiagram
example1 = fc C.blue (circle 3) ||| fc C.red  (rect 5 8)
example2 = fc C.red  (circle 3) ||| fc C.blue (rect 5 8)

type X11Position = V2 Int

type X11Button  = X.Button
type X11KeyCode = X.KeyCode

data X11Mask = MShift
             | MLock
             | MControl
             | MMod1
             | MMod2
             | MMod3
             | MMod4
             | MMod5
             | MButton1
             | MButton2
             | MButton3
             | MButton4
             | MButton5
             deriving (Eq, Ord, Read, Show)

newtype X11Modifier = X11Modifier { _masks :: Set X11Mask }
                    deriving (Eq, Read, Show, Monoid)

data X11EventData d = X11EventData { _X11Event_root     :: !X.Window
                                   , _X11Event_child    :: !X.Window
                                   , _X11Event_time     :: !X.Time
                                   , _X11Event_cpos     :: !X11Position
                                   , _X11Event_rpos     :: !X11Position
                                   , _X11Event_modifier :: !X11Modifier
                                   , _X11Event_detail   :: !d
                                   }
                    deriving (Eq, Show)

data X11Event = KeyPress      !(X11EventData X11KeyCode)
              | KeyRelease    !(X11EventData X11KeyCode)
              | ButtonPress   !(X11EventData X11Button)
              | ButtonRelease !(X11EventData X11Button)
              | MotionNotify  !(X11EventData ())
              deriving (Eq, Show)

type XEventTuple d = ( X.Window, X.Window, X.Time
                     , CInt, CInt, CInt, CInt
                     , X.Modifier, d, Bool )

makeX11Event :: X.XEventPtr -> IO (Maybe X11Event)
makeX11Event xev = X.get_EventType xev >>= go
  where
    go e | e == X.keyPress      = pure . KeyPress      <$> makeKeyEvent
    go e | e == X.keyRelease    = pure . KeyRelease    <$> makeKeyEvent
    go e | e == X.buttonPress   = pure . ButtonPress   <$> makeMouseEvent
    go e | e == X.buttonRelease = pure . ButtonRelease <$> makeMouseEvent
    go e | e == X.motionNotify  = pure . MotionNotify  <$> makeMotionEvent
    go _                        = return Nothing

    makeKeyEvent    = makeEvent X.get_KeyEvent id
    makeMouseEvent  = makeEvent X.get_ButtonEvent id
    makeMotionEvent = makeEvent X.get_MotionEvent (const ())

    makeEvent :: (X.XEventPtr -> IO (XEventTuple t))
              -> (t -> d) -> IO (X11EventData d)
    makeEvent getter f = do
      (rw, cw, time, cx, cy, rx, ry, m, d, _) <- getter xev
      let cpos = V2 (fromIntegral cx) (fromIntegral cy)
      let rpos = V2 (fromIntegral rx) (fromIntegral ry)
      return $ X11EventData rw cw time cpos rpos (makeModifier m) (f d)

makeModifier :: X.Modifier -> X11Modifier
makeModifier mod = X11Modifier
                   $ helper X.shiftMask   MShift
                   $ helper X.lockMask    MLock
                   $ helper X.controlMask MControl
                   $ helper X.mod1Mask    MMod1
                   $ helper X.mod2Mask    MMod2
                   $ helper X.mod3Mask    MMod3
                   $ helper X.mod4Mask    MMod4
                   $ helper X.mod5Mask    MMod5
                   $ helper X.button1Mask MButton1
                   $ helper X.button2Mask MButton2
                   $ helper X.button3Mask MButton3
                   $ helper X.button4Mask MButton4
                   $ helper X.button5Mask MButton5
                   $ mempty
  where
    helper :: X.Modifier -> X11Mask -> Set X11Mask -> Set X11Mask
    helper m e s | m ∈ mod = Set.insert e s
    helper _ _ s           = s
    a ∈ b = (a .&. complement b) == 0

testDrawImage :: IO ()
testDrawImage = do
  --edimg <- JP.decodePng <$> BS.readFile file
  --case edimg of
  --  Left  err  -> putStrLn err
  --  Right dimg -> do
  dpy <- X.openDisplay ""
  win <- mkUnmanagedWindow dpy 1000 400 500 500
  let foldMask = foldr (.|.) 0
  X.selectInput dpy win $ foldMask $ [ X.exposureMask
                                     , X.keyPressMask
                                     , X.keyReleaseMask
                                     , X.buttonPressMask
                                     , X.buttonReleaseMask
                                     , X.pointerMotionMask ]

  X.mapWindow dpy win
  let while m n = m >>= (`when` (n >> while m n))
  forever $ do drawDia dpy win example1
               drawDia dpy win example2
               --threadDelay 1000000
               while ((/= 0) <$> X.pending dpy)
                 $ X.allocaXEvent
                 $ \xev -> do X.nextEvent dpy xev
                              makeX11Event xev >>= print
                              --X.get_EventType xev >>= print . convEventType
               X.pending dpy >>= print
  --drawDia dpy win $ circle 5 ||| rect 10 8
  _ <- getLine
  X.closeDisplay dpy

drawImg :: X.Display -> X.Window -> XImg -> IO ()
drawImg dpy win ximg = do
  gc <- X.createGC dpy win
  (_, _, _, w, h, _, _) <- X.getGeometry dpy win
  X.putImage dpy win gc (xImage ximg) 0 0 0 0 w h
  X.freeGC dpy gc

initColor :: X.Display -> String -> IO X.Pixel
initColor dpy color = do
  let colormap = X.defaultColormap dpy (X.defaultScreen dpy)
  (apros, _) <- X.allocNamedColor dpy colormap color
  return $ X.color_pixel apros

fromJPData :: JP.Image JP.PixelRGBA8 -> IO XImageArray
fromJPData img = newArray ((0, 0, 0), (h - 1, w - 1, 3)) 0 >>= setValues
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
        go :: [(Int, Int, Int)] -> IO XImageArray
        go []             = return arr
        go ((y, x, c):xs) = do writeArray arr (y, x, c) $! getC c $! getPix x y
                               go xs

makeXImage :: X.Display -> JP.Image JP.PixelRGBA8 -> IO XImg
makeXImage dpy image = do d <- fromJPData image
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

drawJPImage :: X.Display -> X.Window -> JP.Image JP.PixelRGBA8 -> IO ()
drawJPImage dpy win image = makeXImage dpy image >>= drawImg dpy win

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
  --let attrmask = X.cWOverrideRedirect .|. X.cWBorderPixel .|. X.cWBackPixel
  backgroundColor <- initColor dpy "black"
  borderColor     <- initColor dpy "green"
  --X.allocaSetWindowAttributes $ \attrs -> do
  --  X.set_override_redirect attrs True
  --  X.set_background_pixel attrs backgroundColor
  --  X.set_border_pixel attrs borderColor
  --  X.createWindow dpy rw x y w h 1 depth X.inputOutput visual attrmask attrs
  X.createSimpleWindow dpy rw x y w h 1 borderColor backgroundColor

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

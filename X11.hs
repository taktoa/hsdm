{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import qualified Codec.Picture.Png           as JP
import qualified Codec.Picture.Types         as JP
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Reader
import           Data.Array.Storable
import           Data.Bits
import           Data.ByteString             (ByteString, useAsCString)
import qualified Data.ByteString             as BS
import qualified Data.Colour.Names           as C
import           Data.Default
import           Data.Int
import           Data.Maybe
import           Data.Semigroup
import           Data.Typeable               (Typeable)
import           Data.Word
import           Diagrams
import           Diagrams.Backend.Rasterific
import           Foreign                     (castPtr)
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
data X11EventType = EKeyPress
                  | EKeyRelease
                  | EButtonPress
                  | EButtonRelease
                  | EMotionNotify
                  | EEnterNotify
                  | ELeaveNotify
                  | EFocusIn
                  | EFocusOut
                  | EKeymapNotify
                  | EExpose
                  | EGraphicsExpose
                  | ENoExpose
                  | EVisibilityNotify
                  | ECreateNotify
                  | EDestroyNotify
                  | EUnmapNotify
                  | EMapNotify
                  | EMapRequest
                  | EReparentNotify
                  | EConfigureNotify
                  | EConfigureRequest
                  | EGravityNotify
                  | EResizeRequest
                  | ECirculateNotify
                  | ECirculateRequest
                  | EPropertyNotify
                  | ESelectionClear
                  | ESelectionRequest
                  | ESelectionNotify
                  | EColormapNotify
                  | EClientMessage
                  | EMappingNotify
                  | ERRScreenChangeNotify
                  | ERRNotify
                  | ERRNotifyCrtcChange
                  | ERRNotifyOutputChange
                  | ERRNotifyOutputProperty
                  deriving (Eq, Read, Show)

convEventType :: X.EventType -> Maybe X11EventType
convEventType e | e == X.keyPress               = Just EKeyPress
convEventType e | e == X.keyRelease             = Just EKeyRelease
convEventType e | e == X.buttonPress            = Just EButtonPress
convEventType e | e == X.buttonRelease          = Just EButtonRelease
convEventType e | e == X.motionNotify           = Just EMotionNotify
convEventType e | e == X.enterNotify            = Just EEnterNotify
convEventType e | e == X.leaveNotify            = Just ELeaveNotify
convEventType e | e == X.focusIn                = Just EFocusIn
convEventType e | e == X.focusOut               = Just EFocusOut
convEventType e | e == X.keymapNotify           = Just EKeymapNotify
convEventType e | e == X.expose                 = Just EExpose
convEventType e | e == X.graphicsExpose         = Just EGraphicsExpose
convEventType e | e == X.noExpose               = Just ENoExpose
convEventType e | e == X.visibilityNotify       = Just EVisibilityNotify
convEventType e | e == X.createNotify           = Just ECreateNotify
convEventType e | e == X.destroyNotify          = Just EDestroyNotify
convEventType e | e == X.unmapNotify            = Just EUnmapNotify
convEventType e | e == X.mapNotify              = Just EMapNotify
convEventType e | e == X.mapRequest             = Just EMapRequest
convEventType e | e == X.reparentNotify         = Just EReparentNotify
convEventType e | e == X.configureNotify        = Just EConfigureNotify
convEventType e | e == X.configureRequest       = Just EConfigureRequest
convEventType e | e == X.gravityNotify          = Just EGravityNotify
convEventType e | e == X.resizeRequest          = Just EResizeRequest
convEventType e | e == X.circulateNotify        = Just ECirculateNotify
convEventType e | e == X.circulateRequest       = Just ECirculateRequest
convEventType e | e == X.propertyNotify         = Just EPropertyNotify
convEventType e | e == X.selectionClear         = Just ESelectionClear
convEventType e | e == X.selectionRequest       = Just ESelectionRequest
convEventType e | e == X.selectionNotify        = Just ESelectionNotify
convEventType e | e == X.colormapNotify         = Just EColormapNotify
convEventType e | e == X.clientMessage          = Just EClientMessage
convEventType e | e == X.mappingNotify          = Just EMappingNotify
convEventType e | e == X.rrScreenChangeNotify   = Just ERRScreenChangeNotify
convEventType e | e == X.rrNotify               = Just ERRNotify
convEventType e | e == X.rrNotifyCrtcChange     = Just ERRNotifyCrtcChange
convEventType e | e == X.rrNotifyOutputChange   = Just ERRNotifyOutputChange
convEventType e | e == X.rrNotifyOutputProperty = Just ERRNotifyOutputProperty
convEventType _                                 = Nothing


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
               threadDelay 1000000
               while ((/= 0) <$> X.pending dpy)
                 $ X.allocaXEvent
                 $ \xev -> do X.nextEvent dpy xev
                              X.get_EventType xev >>= print . convEventType
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
























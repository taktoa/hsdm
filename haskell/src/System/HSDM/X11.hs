{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module System.HSDM.X11 where

import qualified Codec.Picture.Png           as JP
import qualified Codec.Picture.Types         as JP
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Exception
import           Control.Lens
import           Control.Monad
import           Data.Array.Storable
import           Data.Bits
import           Data.ByteString             (ByteString, useAsCString)
import qualified Data.ByteString             as BS
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
import           Foreign                     (castPtr)
import           Foreign.C                   (CInt)
import           Foreign.C.String            (CString)
import           Graphics.Rasterific.Svg
import qualified Graphics.X11.Xlib           as X
import qualified Graphics.X11.Xlib.Extras    as X
import           Pipes                       (Pipe, (>->))
import qualified Pipes                       as P
import           Pipes.Concurrent
import qualified Pipes.Prelude               as P
import           System.Exit
import           System.IO
import           System.Random

type X11Position  = V2 Int
type X11Dimension = V2 Int

type X11Button = X.Button
type X11KeySym = X.KeySym

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

data X11Event
  = KeyPress
    { _xev_window    :: !X.Window
    , _xev_subwindow :: !X.Window
    , _xev_time      :: !X.Time
    , _xev_pos       :: !X11Position
    , _xev_modifier  :: !X11Modifier
    , _xev_key       :: !X11KeySym }
  | KeyRelease
    { _xev_window    :: !X.Window
    , _xev_subwindow :: !X.Window
    , _xev_time      :: !X.Time
    , _xev_pos       :: !X11Position
    , _xev_modifier  :: !X11Modifier
    , _xev_key       :: !X11KeySym }
  | ButtonPress
    { _xev_window    :: !X.Window
    , _xev_subwindow :: !X.Window
    , _xev_time      :: !X.Time
    , _xev_pos       :: !X11Position
    , _xev_modifier  :: !X11Modifier
    , _xev_button    :: !X11Button }
  | ButtonRelease
    { _xev_window    :: !X.Window
    , _xev_subwindow :: !X.Window
    , _xev_time      :: !X.Time
    , _xev_pos       :: !X11Position
    , _xev_modifier  :: !X11Modifier
    , _xev_button    :: !X11Button }
  | MotionNotify
    { _xev_window :: !X.Window
    , _xev_pos    :: !X11Position }
  | ExposeNotify
    { _xev_window :: !X.Window
    , _xev_pos    :: !X11Position
    , _xev_size   :: !X11Dimension
    , _xev_count  :: !Int }
  | MapNotify
    { _xev_window :: !X.Window
    , _xev_event  :: !X.Window }
  deriving (Eq, Show)

makeLenses ''X11Event

type JImage = JP.Image JP.PixelRGBA8
type XDiagram = QDiagram Rasterific V2 Double Any

main :: IO ()
main = startX11Pipe def testPipe `finally` putStrLn "clean exit"

testPipe :: (P.MonadIO m) => Pipe X11Event X11Action m ()
testPipe = go2
  where
    go2 = do
      P.liftIO $ print "bb"
      P.liftIO $ hFlush stdout
      go
    handleMotion m = do
      let Just (V2 x y) = m ^? xev_pos
      let cursor = translate (V2 (fromIntegral x) (fromIntegral (negate y)))
                   $ translate (V2 (-250) (250))
                   $ fc C.blue $ circle 5
      P.yield $ ADraw $ renderRast $ cursor `atop` rect 500 500
    go = do
      P.liftIO $ putStrLn "reblock"
      event <- P.await
      P.liftIO $ print event
      case event of m@(MotionNotify {}) -> handleMotion m >> go2
                    k@(KeyPress {})     -> if k ^? xev_key == Just X.xK_Q
                                           then P.yield AQuit
                                           else P.liftIO (print event) >> go
                    _                   -> go

renderRast :: XDiagram -> JImage
renderRast = renderDia Rasterific options
  where
    options = RasterificOptions (mkWidth 500)

data X11Action = ADraw !JImage
               | APoll
               | AQuit
               | ANop

data X11Config = X11Config { _queueSize :: Int }
               deriving (Eq, Read, Show)

instance Default X11Config where
  def = X11Config { _queueSize = 256 }

startX11Pipe :: X11Config -> Pipe X11Event X11Action IO () -> IO ()
startX11Pipe cfg pipe = do
  (environmentThread, outputX, inputX) <- startX11 cfg
  let producer = fromInput inputX
  let consumer = toOutput outputX
  P.runEffect $ producer >-> pipe >-> consumer
  wait environmentThread

startX11 :: X11Config -> IO (Async (), Output X11Action, Input X11Event)
startX11 cfg = do
  (outputAction, inputAction) <- spawn $ bounded 2048 -- latest ANop
  (outputEvent,  inputEvent)  <- spawn $ bounded 1024
  thread <- async $ do
    dpy <- X.openDisplay ""
    -- FIXME: should cover whole screen
    win <- mkUnmanagedWindow dpy 1000 400 500 500
    initializeX11Events dpy win
    X.mapWindow dpy win
    let waitX11 = do
          count <- X.pending dpy
          if count == 0 then
            X.waitForEvent dpy 1000000000
          else
            return False
    let pollX11 = getPendingX11Events dpy >>= sendAll outputEvent
    let loop = do
                  count <- X.pending dpy
                  putStrLn $ "pending: " <> (show count)
                  waitX11
                  pollX11
                  action <- atomically $ recv inputAction
                  case action
                    of Just (ADraw i) -> drawJPImage dpy win i >> loop
                       Just APoll     -> pollX11               >> loop
                       Just AQuit     -> X.closeDisplay dpy
                       Nothing        -> loop
    pollX11
    loop
    performGC
  return (thread, outputAction, inputEvent)

sendAll :: Output a -> [a] -> IO ()
sendAll out = go
  where
    go []     = return ()
    go (x:xs) = atomically (send out x) >> go xs

type XEventTuple d = ( X.Window, X.Window, X.Time
                     , CInt, CInt, CInt, CInt
                     , X.Modifier, d, Bool )



makeX11Event :: Display -> X.XEventPtr -> IO (Maybe X11Event)
makeX11Event dpy xev = do e <- X.getEvent xev
                          t <- X.get_EventType xev
                          go (e, t)
  where
    getType e = (e, X.ev_event_type e)

    go (e, t) | t == X.keyPress      = pure <$> makeKeyPressEvent e
    go (e, t) | t == X.keyRelease    = pure <$> makeKeyReleaseEvent e
    go (e, t) | t == X.buttonPress   = pure <$> makeButtonPressEvent e
    go (e, t) | t == X.buttonRelease = pure <$> makeButtonReleaseEvent e
    go (e, t) | t == X.motionNotify  = pure <$> makeMotionNotifyEvent e
    go (e, t) | t == X.expose        = pure <$> makeExposeNotifyEvent e
    go (e, t) | t == X.mapNotify     = pure <$> makeMapNotifyEvent e
    go _                             = return Nothing

    -- go e | e == X.keyPress      = pure <$> makeKeyPressEvent
    -- go e | e == X.keyRelease    = pure <$> makeKeyReleaseEvent
    -- go e | e == X.buttonPress   = pure <$> makeButtonPressEvent
    -- go e | e == X.buttonRelease = pure <$> makeButtonReleaseEvent
    -- go e | e == X.motionNotify  = pure <$> makeMotionNotifyEvent
    -- go e | e == X.expose        = pure <$> makeExposeNotifyEvent
    -- go e | e == X.mapNotify     = pure <$> makeMapNotifyEvent
    -- go e                        = return Nothing

    makeV :: (Integral i, Num n) => i -> i -> V2 n
    makeV x y = V2 (fromIntegral x) (fromIntegral y)

    makeKeyPressEvent e = do
      let (w, sw, t, x, y, m, k) = ( X.ev_root e, X.ev_subwindow e
                                   , X.ev_time e, X.ev_x e, X.ev_y e
                                   , X.ev_state e, X.ev_keycode e )
      key <- convertKey k
      return $ KeyPress w sw t (makeV x y) (makeModifier m) key
    makeKeyReleaseEvent e = do
      let (w, sw, t, x, y, m, k) = ( X.ev_root e, X.ev_subwindow e
                                   , X.ev_time e, X.ev_x e, X.ev_y e
                                   , X.ev_state e, X.ev_keycode e )
      key <- convertKey k
      return $ KeyRelease w sw t (makeV x y) (makeModifier m) key
    makeButtonPressEvent e = do
      let (w, sw, t, x, y, m, b) = ( X.ev_root e, X.ev_subwindow e
                                   , X.ev_time e, X.ev_x e, X.ev_y e
                                   , X.ev_state e, X.ev_button e )
      return $ ButtonPress w sw t (makeV x y) (makeModifier m) b
    makeButtonReleaseEvent e = do
      let (w, sw, t, x, y, m, b) = ( X.ev_root e, X.ev_subwindow e
                                   , X.ev_time e, X.ev_x e, X.ev_y e
                                   , X.ev_state e, X.ev_button e )
      return $ ButtonRelease w sw t (makeV x y) (makeModifier m) b
    makeMotionNotifyEvent e = do
      let (w, x, y) = (X.ev_window e, X.ev_x e, X.ev_y e)
      return $ MotionNotify w (makeV x y)
    makeExposeNotifyEvent e = do
      let (win, x, y, w, h, c) = ( X.ev_window e, X.ev_x e, X.ev_y e
                                 , X.ev_width e, X.ev_height e, X.ev_count e )
      return $ ExposeNotify win (makeV x y) (makeV w h) (fromIntegral c)
    makeMapNotifyEvent e = do
      let (win, ev) = (X.ev_window e, X.ev_event e)
      return $ MapNotify win ev

    convertKey k = X.keycodeToKeysym dpy k 0

-- get_ExposeEvent (Position, Position, Dimension, Dimension, CInt)

--  | MapNotify
--    { _window    :: !X.Window
--    , _event     :: !X.Window }

makeModifier :: X.Modifier -> X11Modifier
makeModifier xmod = X11Modifier
                    $ check X.shiftMask   MShift
                    $ check X.lockMask    MLock
                    $ check X.controlMask MControl
                    $ check X.mod1Mask    MMod1
                    $ check X.mod2Mask    MMod2
                    $ check X.mod3Mask    MMod3
                    $ check X.mod4Mask    MMod4
                    $ check X.mod5Mask    MMod5
                    $ check X.button1Mask MButton1
                    $ check X.button2Mask MButton2
                    $ check X.button3Mask MButton3
                    $ check X.button4Mask MButton4
                    $ check X.button5Mask MButton5 mempty
  where
    check :: X.Modifier -> X11Mask -> Set X11Mask -> Set X11Mask
    check m e s | m ∈ xmod = Set.insert e s
    check _ _ s            = s
    a ∈ b = (a .&. complement b) == 0

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

initializeX11Events :: Display -> Window -> IO ()
initializeX11Events dpy win = X.selectInput dpy win $ foldr (.|.) 0 masks
  where
    masks = [ X.keyPressMask
            , X.keyReleaseMask
            , X.buttonPressMask
            , X.buttonReleaseMask
            , X.pointerMotionMask
            , X.exposureMask ]

getPendingX11Events :: Display -> IO [X11Event]
getPendingX11Events dpy = X.allocaXEvent $ flip go []
  where
    go xev es = do p <- X.pending dpy
                   case p
                     of 0 -> return es
                        _ -> getEvent xev >>= go xev . (<> es) . toList
    getEvent :: X.XEventPtr -> IO (Maybe X11Event)
    getEvent xev = X.nextEvent dpy xev >> makeX11Event dpy xev

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

fromJPData :: JImage -> IO XImageArray
fromJPData img = newArray ((0, 0, 0), (h - 1, w - 1, 3)) 0 >>= setValues
  where
    w = JP.imageWidth  img
    h = JP.imageHeight img
    getC :: Int -> JP.PixelRGBA8 -> Word8
    getC 0 (JP.PixelRGBA8 _ _ b _) = b
    getC 1 (JP.PixelRGBA8 _ g _ _) = g
    getC 2 (JP.PixelRGBA8 r _ _ _) = r
    getC 3 (JP.PixelRGBA8 _ _ _ a) = a
    getC _ _                       = error "Invalid color channel"
    getChannel !c !x !y = getC c $ JP.pixelAt img x y
    setValues :: XImageArray -> IO XImageArray
    setValues arr = getBounds arr >>= go . range
      where
        go :: [(Int, Int, Int)] -> IO XImageArray
        go ps = mapM_ copyPixel ps >> return arr
        copyPixel (y, x, c) = writeArray arr (y, x, c) $! getChannel c x y

makeXImage :: X.Display -> JImage -> IO XImg
makeXImage dpy img = do xid <- fromJPData img
                        withStorableArray xid (ci xid . castPtr)
  where
    w, h :: Integral i => i
    w = fromIntegral $ JP.imageWidth  img
    h = fromIntegral $ JP.imageHeight img
    scr = X.defaultScreen dpy
    dep = X.defaultDepth  dpy scr
    vis = X.defaultVisual dpy scr
    ci bytes p = let makeXI x = XImg x bytes w h
                 in makeXI <$> X.createImage dpy vis dep X.zPixmap 0 p w h 32 0

drawJPImage :: X.Display -> X.Window -> JImage -> IO ()
drawJPImage dpy win img = makeXImage dpy img >>= drawImg dpy win

convertDynImage :: JP.DynamicImage -> Maybe JImage
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
  backgroundColor <- initColor dpy "black"
  borderColor     <- initColor dpy "green"
  X.createSimpleWindow dpy rw x y w h 1 borderColor backgroundColor

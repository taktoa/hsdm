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
import           Graphics.Svg                (Document, loadSvgFile,
                                              resolveUses)
import           Graphics.Text.TrueType      (FontCache)
import           Graphics.X11.Xlib
import           Graphics.X11.Xlib.Extras
import           Linear
import           System.Exit
import qualified System.HSDM.X11             as Old
import           System.Posix.Process

data State = State { _doc     :: Document
                   , _cache   :: FontCache
                   , _winSize :: (Int, Int) }
makeLenses ''State

makeState :: IO State
makeState = do
  f <- loadSvgFile "../circle.svg"
  case f of
    Nothing -> error "unable to load svg"
    Just d  -> do fc <- loadCreateFontCache "/tmp/fonty-texture-cache"
                  return $ State (resolveUses d) fc (600, 600)

mainRewrite :: IO ()
mainRewrite = do
  state <- makeState
  dpy <- openDisplay ""
  let scr = defaultScreen dpy
  -- let (w, h) = ( fromIntegral $ displayWidth  dpy scr
  --              , fromIntegral $ displayHeight dpy scr )
  let Just (w, h) = state ^? winSize
  win <- mkFramelessWindow dpy (V2 0 0) (V2 (fromIntegral w) (fromIntegral h))
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
  img <- renderScene state
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

renderScene :: State -> IO Old.JImage
renderScene (State d c (w, h)) = do
  (finalImage, _) <- renderSvgDocument c (Just (w, h)) 96 d
  return finalImage

eventLoop :: State -> Display -> Window -> IO ()
eventLoop state dpy win = allocaXEvent $ go state
  where
    go st xev = do nextEvent dpy xev
                   evt <- Old.makeX11Event dpy xev
                   res <- handle st $ fromJust evt
                   case res of Just st' -> go st' xev
                               Nothing  -> return ()
    handle st e@Old.ExposeNotify  {} = dput (e ^? Old.xev_pos) >> reloadA st
    handle st e@Old.ButtonPress   {} = dput e >> loopA st
    handle st e@Old.ButtonRelease {} = dput e >> loopA st
    handle st e@Old.KeyPress      {} = case fromJust (e ^? Old.xev_key)
                                       of k | k == xK_q -> quitA st
                                          k | k == xK_r -> rereadA st
                                          _             -> dput e >> loopA st
    handle st e@Old.KeyRelease    {} = dput e >> loopA st
    handle st _                      = loopA st
    rereadA _  = putStrLn "Rereading SVG file..." >> makeState >>= reloadA
    reloadA st = renderScene st >>= Old.drawJPImage dpy win >> loopA st
    loopA   st = return $ Just st
    quitA   _  = putStrLn "Quitting..." >> return Nothing

debugEnabled :: Bool
debugEnabled = False

dput :: (Show s) => s -> IO ()
dput = when debugEnabled . print

{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.HSDM.GTKRedo where

import           Control.Arrow
import           Control.Monad
import           Control.Monad.Trans

import           Control.Lens

-- FIXME: remove when done
import           "gtk3" Graphics.UI.Gtk       (AttrOp ((:=)))
import qualified "gtk3" Graphics.UI.Gtk       as G

import           Control.Applicative
import           Control.Concurrent

import           Control.Applicative
import           Control.Varying
import           Data.Functor.Identity
import           Text.Printf

import           Linear

-- TEMPORARY vvvv
import           Control.Applicative
import           Data.IORef
import           Data.Maybe
import           Debug.Trace
import           System.Glib.GDateTime
import           System.Mem.Weak
-- TEMPORARY ∧∧∧∧

-- # import           Control.FRPNow
-- # import           Control.FRPNow.GTK

sync :: (Monad m) => m r -> VarT m a r
sync m = varM (const m)

-- An exponential tween back and forth from 0 to 100 over 2 seconds that
-- loops forever. This spline takes float values of delta time as input,
-- outputs the current x value at every step and would result in () if it
-- terminated.
tweenx :: (Applicative m, Monad m) => SplineT Float Float m ()
tweenx = forever $ do
    -- Tween from 0 to 100 over 1 second
    x <- tween easeOutExpo 0 100 1
    -- Chain another tween back to the starting position
    tween easeOutExpo x 0 1

-- A quadratic tween from 0 to 100 and back over 2 seconds that never ends.
tweeny :: (Applicative m, Monad m) => SplineT Float Float m ()
tweeny = forever $ do y <- tween easeOutQuad 0 100 1
                      tween easeOutQuad y 0 1

-- Our time signal that provides delta time samples.
time :: VarT IO a Float
time = deltaUTC

-- | Our Point value that varies over time continuously in x and y.
backAndForth :: VarT IO a (V2 Float)
backAndForth = let x = outputStream 0 tweenx
                   y = outputStream 0 tweeny
               in time ~> (V2 <$> x <*> y)

main :: IO ()
main = do
    putStrLn "An example of value streams using the varying library."
    putStrLn "Enter a newline to continue, quit with ctrl+c"
    getLine

    loop backAndForth
      where
        loop :: VarT IO () (V2 Float) -> IO ()
        loop v = do (V2 x y, vNext) <- runVarT v ()
                    printf "Point %03.1f %03.1f\n" x y
                    loop vNext



-- # main :: IO ()
-- # main = runNowGTK now
-- #
-- # now :: Now ()
-- # now = mdo
-- #   window <- sync windowNew
-- #   sync $ windowSetTypeHint window WindowTypeHintDialog
-- #   sync $ set window [ containerBorderWidth := 10 ]
-- #
-- #   image <- sync $ imageNewFromFile "../circle.png"
-- #
-- #   sync $ set window [ containerChild := image ]
-- #   sync $ window `on` deleteEvent $ liftIO mainQuit >> return False
-- #
-- #   -- logic with recursive do
-- #   --d <- sample $ fromChanges 0 (e1 `merge` fmap (1 - ) e2)
-- #   --(slider1, e1) <- createSlider 0 1 0.1 d
-- #   --(slider2, e2) <- createSlider 0 1 0.1 ((1 -) <$> d)
-- #
-- #   -- layout and more initialization
-- #   sync $  widgetShowAll window
-- #
-- # --  getClock 50000000 >>= void . createButton . fmap show

-- MonadIO m =>  VarT IO a r

-- data Event a
-- data Behavior a
-- data Now a
-- never        :: Event a
-- switch       :: Behavior a -> Event (Behavior a) -> Behavior a
-- whenJust     :: Behavior (Maybe a) -> Behavior (Event a)
-- futuristic   :: Behavior (Event a) -> Behavior (Event a)
-- async        :: IO a -> Now (Event a)
-- asyncOS      :: IO a -> Now (Event a)
-- callback     :: Now (Event a, a -> IO ())
-- sampleNow    :: Behavior a -> Now a
-- planNow      :: Event (Now a) -> Now (Event a)
-- sync         :: IO a -> Now a
-- runNowMaster :: Now (Event a) -> IO a
-- initNow      :: (IO (Maybe a) -> IO ()) -> Now (Event a) -> IO ()
-- sample :: (Swap b e, Sample b) => Behavior a -> (b :. e) a
-- sample :: Behavior a -> Behavior a
-- sample :: Behavior a -> Now a

data Stream m a b = Stream (VarT m a (Event [b]))

callIOStream :: (MonadIO m) => (b -> m ()) -> Stream m a b -> VarT m a ()
callIOStream = undefined

toChanges :: (Eq a) => VarT m a b -> Stream m a b
toChanges = undefined

fromChanges :: c -> Stream m a c -> VarT m a (VarT m b c)
fromChanges = undefined

callback :: (MonadIO m) => VarT m a (Event b, b -> m ())
callback = undefined

callbackStream :: (MonadIO m) => VarT m a (Stream m a b, b -> m ())
callbackStream = undefined

beforeEs :: Stream m a b -> Event () -> Stream m a b
beforeEs = undefined

initVar :: (MonadIO m) => (m (Maybe a) -> m ()) -> VarT m a (Event a) -> m ()
initVar = undefined

-- | Run a Now computation which can interact with GTK.
-- Also starts the GTK system.
-- Call only once, or GTK will freak out.
runNowGTK :: (MonadIO m) => VarT m a b -> m b
runNowGTK n = do liftIO $ G.initGUI
                 doneRef <- liftIO $ newIORef Nothing
                 initVar (schedule doneRef) (n >> never)
                 liftIO $ G.mainGUI

schedule :: (MonadIO m) => IORef (Maybe a) -> m (Maybe a) -> m ()
schedule ref m = liftIO $ G.postGUIAsync $
                   m >>= \x ->
                     case x of
                      Just _  -> liftIO $ writeIORef ref x
                      Nothing -> return ()

-- | Set a GTK attribute to a behavior. Each time the behavior changes the
-- attribute is updated.
setAttr :: (G.WidgetClass w, Eq a, MonadIO m)
           => G.Attr w a -> w -> VarT m a b -> VarT m a ()
setAttr a w b = do i <- b
                   sync $ set w [a := i]
                   (e,cb) <- callback
                   sync $ G.on w G.unrealize (cb ())
                   let updates = toChanges b `beforeEs` e
                   callIOStream setEm updates
  where
    setEm i = set w [a := i] >> G.widgetQueueDraw w


-- | Obtain an event stream from a unit GTK signal, i.e. a signal with handler type:
--
-- > IO ()
getUnitSignal :: (G.GObjectClass widget, MonadIO m)
                 => G.Signal widget (m ()) -> widget -> VarT m a ()
getUnitSignal s w = getSignal s w (\f -> f ())


-- | Obtain an event stream from a GTK signal giving a single value.
getSimpleSignal :: (G.GObjectClass widget, MonadIO m)
                   => G.Signal widget (b -> m ())
                   -> widget -> VarT m a (Stream m a b)
getSimpleSignal s w = getSignal s w id


-- | General interface to convert an GTK signal to an event stream.
--
-- The signal has type @callback@, for example @(ScrollType -> Double -> IO Bool)@
-- and the eventstream gives elements of type @value@, for instance @(ScrollType,Double)@
-- The conversion function (3rd argument) takes a function to call for producing the value
-- in our example, a function of type @(ScollType,Double) -> IO ()@ and produces
-- a function of the form @callback@, in our example @(ScrollType -> Double -> IO Bool)@.
--
-- In this example we can covert a signal with hander @(ScrollType -> Double -> IO Bool)@
-- to an eventstream giving elements of type @(ScrollType,Double)@ by letting the handler return @False@
-- as follows:
--
-- > scrollToEvStream :: Signal widget (ScrollType -> Double -> IO Bool) -> widget -> Now (EvStream (ScrollType,Double))
-- > scrollToEvStream s w = getSignal s w convert where
-- >   convert call scrolltype double = do call (scrolltype, double)
-- >                                       return False
--
-- The signal is automatically disconnected, when the event stream is garbage collected.
getSignal :: (G.GObjectClass widget, MonadIO m)
             => G.Signal widget callback
             -> widget
             -> ((b -> m ()) -> callback)
             -> Stream m a b
getSignal s w conv =
   do (res, f) <- callbackStream
      conn <- sync $ G.on w s (conv f)
      --sync $ addFinalizer res (putStrLn "Run final" >> signalDisconnect conn)
      return res


-- | Get a clock that gives the time since the creation of the clock in seconds, and updates maximally even given numer of seconds.
--
-- The clock is automatically destroyed and all resources associated with the clock are freed
-- when the behavior is garbage collected.
getTimer :: (Fractional f, MonadIO m) => Double -> VarT m a f
getTimer precision =
  do start <- sync $ gGetCurrentTime
     (res,cb) <- callbackStream
     wres<- sync $ mkWeakPtr res Nothing
     let getDiff = do now <- gGetCurrentTime
                      let seconds = gTimeValSec now - gTimeValSec start
                      let microsec = gTimeValUSec now - gTimeValUSec start
                      return $ (fromIntegral seconds) + (fromIntegral microsec) * 0.000001
     let onTimeOut =
              deRefWeak wres >>= \x ->
                 case x of
                   Just _ -> getDiff >>= cb >> return True
                   Nothing -> return False
     sync $ G.timeoutAdd onTimeOut (round (precision * 1000))
     fromChanges 0 res



createLabel :: (MonadIO m) => VarT m a String -> VarT m a G.Label
createLabel s = do l <- sync $ G.labelNew (Nothing :: Maybe String)
                   setAttr G.labelLabel l s
                   return l


createButton :: (MonadIO m) => VarT m a String -> VarT m a G.Button
createButton s = do button <- sync $ G.buttonNew
                    setAttr G.buttonLabel button s
                    stream <- getUnitSignal G.buttonActivated button
                    return (button,stream)


createProgressBar :: (MonadIO m, Fractional f)
                     => VarT m a (G.ProgressBar, f -> m ())
createProgressBar = do (evs, cb) <- callbackStream
                       progress <- fromChanges 0 evs
                       bar <- sync $ G.progressBarNew
                       setAttr G.progressBarFraction bar progress
                       return (bar,cb)

createSlider :: (MonadIO m, Fractional f)
                => f -> f -> f -> VarT m a f -> VarT m a (G.HScale, Stream m a f)
createSlider min max step b =
  do i <- b
     slider <- sync $ G.hScaleNewWithRange min max step
     setAttr G.rangeValue slider b
     stream <- getSignal G.changeValue slider (\f _ d -> f d >> return True)
     return (slider, stream)

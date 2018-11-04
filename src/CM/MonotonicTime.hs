{-# LANGUAGE CPP #-}

module CM.MonotonicTime
  ( getMonotonicTime
  )
where

#ifdef GHCJS
import           Control.Monad.IO.Class
foreign import javascript "if ( typeof performance !== 'undefined' ) { $r = performance.now(); } else { $r = Date.now(); }" get_performance_now :: IO Double

getMonotonicTime :: MonadIO m => m Double
getMonotonicTime = liftIO $ do
  msecs <- get_performance_now
  return $ msecs / 1000.0
#else
import           Control.Monad.IO.Class
import           System.Clock

{-# INLINABLE getMonotonicTime #-}
getMonotonicTime :: MonadIO m => m Double
getMonotonicTime = liftIO $ do
  ts <- getTime Monotonic
  let nsecs = toNanoSecs ts
      secs  = nsecs `div` 1000000000
      frac  = nsecs `mod` 1000000000
  return $ fromIntegral secs + (fromIntegral frac / 1000000000.0)
#endif

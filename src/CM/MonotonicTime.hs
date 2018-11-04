module CM.MonotonicTime
  ( getMonotonicTime
  )
where

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

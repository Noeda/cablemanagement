{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

module CM.AnimatedTextIO
  ( AnimatedTextIO()
  , AnimatedTile()
  , dontFlushWith
  , withDOffset
  , static
  , animated
  , runAnimatedTextIO
  )
where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.Data
import qualified Data.IntMap.Strict            as IM
import           GHC.Generics

import           CM.KeyInput
import           CM.Coords
import           CM.MonotonicTime
import           CM.LevelRender
import           CM.TextIO

data DoFlush = DoFlush | DontFlush
  deriving ( Eq, Ord, Show, Read )

data AnimateState m tile = AnimateState
  { animatedTiles :: !(TVar (IM.IntMap (AnimatedTile_ m tile)))
  , staticTiles :: !(TVar (IM.IntMap tile))
  , clearBeforeNextFlush :: !(TVar Bool)
  , dontFlushCounter :: !(TVar Int)
  , doffsetVar :: !(TVar Coords2D)
  , mule :: !(TChan (m DoFlush)) }

newtype AnimatedTextIO m tile a = AnimatedTextIO (ReaderT (AnimateState m tile) IO a)
  deriving ( Functor, Applicative, Monad, MonadIO )

data AnimatedTile m tile = StaticTile !tile | AnimatedTile !(Coords2D -> Double -> m tile)
  deriving ( Functor, Generic, Typeable )

newtype AnimatedTile_ m tile = AnimatedTile_ (Coords2D -> Double -> m tile)
  deriving ( Functor, Generic, Typeable )

{-# INLINABLE dontFlushWith #-}
dontFlushWith :: AnimatedTextIO m tile a -> AnimatedTextIO m tile a
dontFlushWith (AnimatedTextIO action) = AnimatedTextIO $ mask $ \restore -> do
  env <- ask
  liftIO $ atomically $ modifyTVar (dontFlushCounter env) (+ 1)
  finally (restore action) $ liftIO $ atomically $ modifyTVar
    (dontFlushCounter env)
    (\old -> old - 1)

{-# INLINABLE withDOffset #-}
withDOffset :: Coords2D -> AnimatedTextIO m tile a -> AnimatedTextIO m tile a
withDOffset offset (AnimatedTextIO action) =
  AnimatedTextIO $ mask $ \restore -> do
    env <- ask
    old <- liftIO $ atomically $ do
      r <- readTVar (doffsetVar env)
      writeTVar (doffsetVar env) offset
      return r
    finally (restore action) $ liftIO $ atomically $ writeTVar
      (doffsetVar env)
      old

{-# INLINE static #-}
static :: tile -> AnimatedTile m tile
static = StaticTile

{-# INLINE animated #-}
animated :: (Coords2D -> Double -> m tile) -> AnimatedTile m tile
animated = AnimatedTile

instance MonadIO m => KeyInteractiveIO (AnimatedTextIO m tile) where
  waitForKey = liftIO waitForKey

-- | Note: `setChar` is quite inefficient for `AnimatedTextIO` because
-- internally those are communicated between Haskell threads on STM tchannels.
--
-- `flush` can force a refresh; `AnimatedTextIO` refreshes by itself too.
instance (MonadIO m, TextIO m, TiledRenderer m tile) => TextIO (AnimatedTextIO m tile) where
  terminalSize = getDisplaySize

  {-# INLINE setChar #-}
  setChar atts ch coords = AnimatedTextIO $ do
    env <- ask
    liftIO $ atomically $ writeTChan (mule env) $ setChar atts ch coords >> pure DontFlush

  {-# INLINE flush #-}
  flush = AnimatedTextIO $ do
    env <- ask
    liftIO $ atomically $ writeTChan (mule env) $ return DoFlush

  {-# INLINE clear #-}
  clear = clearTiles


getDisplaySize
  :: (MonadIO m, TiledRenderer m tile) => AnimatedTextIO m tile Coords2D
getDisplaySize = AnimatedTextIO $ do
  env <- ask
  liftIO $ do
    sz_mvar <- newEmptyMVar
    atomically $ writeTChan (mule env) $ do
      sz <- displaySize
      liftIO $ putMVar sz_mvar sz
      return DontFlush
    takeMVar sz_mvar

instance (MonadIO m, TiledRenderer m tile) => TiledRenderer (AnimatedTextIO m tile) (AnimatedTile m tile) where
  {-# INLINEABLE displaySize #-}
  displaySize = getDisplaySize

  {-# INLINE drawTile #-}
  drawTile coords (StaticTile tile) = AnimatedTextIO $ do
    let !offset = coords2DToInt coords
    env <- ask
    liftIO $ atomically $ do
      modifyTVar (animatedTiles env) $ \old ->
        IM.delete offset old
      modifyTVar (staticTiles env) $ \old ->
        IM.insert offset tile old
  drawTile coords (AnimatedTile func) = AnimatedTextIO $ do
    let !offset = coords2DToInt coords
    env <- ask
    liftIO $ atomically $ do
      modifyTVar (animatedTiles env) $ \old ->
        IM.insert offset (AnimatedTile_ func) old
      modifyTVar (staticTiles env) $ \old ->
        IM.delete offset old

  flushTiles = AnimatedTextIO $ do
    env <- ask
    liftIO $ atomically $ writeTChan (mule env) $ return DoFlush

  clearTiles = AnimatedTextIO $ do
    env <- ask
    liftIO $ atomically $ do
      writeTVar (animatedTiles env) IM.empty
      writeTVar (staticTiles env) IM.empty
      writeTVar (clearBeforeNextFlush env) True

data StopFun = StopFun
  deriving ( Eq, Ord, Show, Read, Typeable, Generic )

instance Exception StopFun

{-# INLINE runAnimatedTextIO #-}
runAnimatedTextIO
  :: forall m tile a
   . (MonadIO m, MonadMask m, TiledRenderer m tile)
  => AnimatedTextIO m tile a
  -> m a
runAnimatedTextIO (AnimatedTextIO reader) = mask $ \restore -> do
  tiles_tvar              <- liftIO $ newTVarIO IM.empty
  static_tvar             <- liftIO $ newTVarIO IM.empty
  clear_before_next_flush <- liftIO $ newTVarIO True
  mule_tchan              <- liftIO $ newTChanIO :: m (TChan (m DoFlush))
  dont_flush_counter      <- liftIO $ newTVarIO 0
  parent                  <- liftIO myThreadId
  start                   <- liftIO newEmptyMVar :: m (MVar ())
  doffset_var             <- liftIO $ newTVarIO (Coords2D 0 0)
  result_mvar             <- liftIO $ newEmptyMVar
  void $ liftIO $ forkIOWithUnmask $ \unmask ->
    try
        (unmask $ do
          () <- takeMVar start :: IO ()
          runReaderT
            reader
            (AnimateState
              { mule                 = mule_tchan
              , animatedTiles        = tiles_tvar
              , staticTiles          = static_tvar
              , clearBeforeNextFlush = clear_before_next_flush
              , dontFlushCounter     = dont_flush_counter
              , doffsetVar           = doffset_var
              }
            )
        )
      >>= (\case
            Left  (exc :: SomeException) -> throwTo parent exc
            Right ok                     -> do
              putMVar result_mvar ok
              throwTo parent StopFun
          )
  result <- try $ liftIO (putMVar start ()) >> restore
    (animator mule_tchan
              tiles_tvar
              static_tvar
              clear_before_next_flush
              dont_flush_counter
              doffset_var
    )
  case result of
    Left StopFun -> liftIO $ takeMVar result_mvar
    _            -> error "impossible"

{-# INLINE animator #-}
animator
  :: (MonadIO m, TiledRenderer m tile)
  => TChan (m DoFlush)
  -> TVar (IM.IntMap (AnimatedTile_ m tile))
  -> TVar (IM.IntMap tile)
  -> TVar Bool
  -> TVar Int
  -> TVar Coords2D
  -> m ()
animator mule_tvar animated_tiles static_tiles clear_before_next_flush dont_flush_counter doffset_var
  = do
    now <- getMonotonicTime
    go now
 where
  go previous_tick = do
    let !next_tick = previous_tick + 0.5
    result <- liftIO $ do
      now <- getMonotonicTime
      let !mseconds = max 0 $ floor $ (next_tick - now) * 500000
      tid <- forkIO $ threadDelay mseconds >> atomically
        (do
          flush_counter <- readTVar dont_flush_counter
          when (flush_counter > 0) retry
          writeTChan mule_tvar (return DontFlush)
        )
      r <- atomically $ readTChan mule_tvar
      killThread tid
      return r
    flush              <- result
    now                <- getMonotonicTime
    next_previous_tick <- if now >= next_tick || flush == DoFlush
      then refresh >> pure (if flush == DoFlush then previous_tick else now)
      else pure previous_tick
    go next_previous_tick

  refresh = do
    (static, tiles, do_clear, doffset) <-
      liftIO
      $   atomically
      $   (,,,)
      <$> readTVar static_tiles
      <*> readTVar animated_tiles
      <*> readTVar clear_before_next_flush
      <*> readTVar doffset_var
    when do_clear clearTiles
    now <- getMonotonicTime
    void $ flip IM.traverseWithKey tiles $ \offset (AnimatedTile_ func) -> do
      let !coords = intToCoords2D offset
      !tile <- func (coords .+ doffset) now
      drawTile coords tile
    void $ flip IM.traverseWithKey static $ \offset tile -> do
      let !coords = intToCoords2D offset
      drawTile coords tile
    liftIO $ atomically $ do
      writeTVar static_tiles            IM.empty
      writeTVar clear_before_next_flush False
    flushTiles

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}

module FovBenchmarks
  ( fovBenchmarks
  )
where

import           Control.Monad
import           Control.Monad.Trans.State.Strict
import           CriterionShim
import           Data.Default.Class

import           CM.LevelRender
import           CM.ImapLevel

newtype RenderCounter a = RenderCounter (State Int a)
  deriving ( Monad, Applicative, Functor )

instance TiledRenderer RenderCounter () where
  displaySize = return $ Coords2D 100 100
  setTile !_ !_ = RenderCounter $ modify (+1)

data Tile = Floor | Wall

instance Obstacle Tile where
  isObstacle Wall = 10
  isObstacle Floor = 0

instance Default Tile where
  def = Floor

instance TileToRenderedTile Tile () where
  toRenderedTile _ _ = ()

instance TileMemorizer (IMapLevel Tile) Coords2D Tile where
  memorizedTileAt _ _ = Just Floor
  memorizeTile l _ _ = l

runRenderCounter :: RenderCounter () -> Int -> Int
runRenderCounter (RenderCounter st) !x = execState st x

emptyImapLevel :: IMapLevel Tile
emptyImapLevel = empty

fovBenchmarks :: [Benchmark]
fovBenchmarks =
  [ bench "compute field of view at 100x100 radius empty imap level raycasting"
    $ whnf
        (runRenderCounter $ void $ renderRaycastingView
          emptyImapLevel
          (relativeRenderView (Coords2D 50 50) (Coords2D 0 0) (Coords2D 100 100)
          )
          ()
        )
        11
  , bench "compute field of view at 80x24 radius empty imap level raycasting"
    $ whnf
        (runRenderCounter $ void $ renderRaycastingView
          emptyImapLevel
          (relativeRenderView (Coords2D 40 12) (Coords2D 0 0) (Coords2D 80 24))
          ()
        )
        11
  , bench "compute field of view at 100x100 radius empty imap level, 32 slopes"
    $ whnf
        (runRenderCounter $ renderBeamcastFOV
          emptyImapLevel
          (relativeRenderView (Coords2D 50 50) (Coords2D 0 0) (Coords2D 100 100)
          )
          ()
          32
        )
        11
  , bench "compute field of view at 80x24 radius empty imap level, 32 slopes"
    $ whnf
        (runRenderCounter $ renderBeamcastFOV
          emptyImapLevel
          (relativeRenderView (Coords2D 40 12) (Coords2D 0 0) (Coords2D 80 24))
          ()
          32
        )
        11
  , bench "compute simple field of view at 100x100 empty imap level" $ whnf
    (runRenderCounter $ renderLevel
      emptyImapLevel
      (coords2DRenderView (Coords2D 0 0) (Coords2D 0 0) (Coords2D 100 100))
      ()
    )
    11
  , bench "compute simple field of view at 80x24 empty imap level" $ whnf
    (runRenderCounter $ renderLevel
      emptyImapLevel
      (coords2DRenderView (Coords2D 0 0) (Coords2D 0 0) (Coords2D 80 24))
      ()
    )
    11
  ]

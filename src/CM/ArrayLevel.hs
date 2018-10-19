-- | This module implements levels based on arrays.
--
-- Reads are very fast but modifications are very slow (any mutation copies the
-- whole level). Moreover, array levels require memory for a rectangle around
-- the extremes of the level.
--
-- Array levels are good for immutable levels or levels that mutate rarely.
--
-- Array levels require Unbox on the tile type.
--

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}

module CM.ArrayLevel
  ( ArrayLevel()
  , module CM.Coords
  , module CM.LevelLike
  )
where

import qualified Data.Array.Unboxed            as UA
import           Data.Data
import           Data.Default.Class
import           Data.Foldable
import           GHC.Generics

import           CM.Coords
import           CM.LevelLike

newtype ArrayLevel tile = ArrayLevel (UA.UArray Coords2D tile)

deriving instance (Eq tile, UA.IArray UA.UArray tile) => Eq (ArrayLevel tile)
deriving instance (Ord tile, UA.IArray UA.UArray tile) => Ord (ArrayLevel tile)
deriving instance (Show tile, UA.IArray UA.UArray tile) => Show (ArrayLevel tile)
deriving instance Typeable (ArrayLevel tile)
deriving instance Generic (ArrayLevel tile)

instance (Eq tile, Default tile, UA.IArray UA.UArray tile) => LevelLike (ArrayLevel tile) Coords2D tile where
  {-# INLINE tileAt #-}
  tileAt (ArrayLevel !arr) coords@(Coords2D !x !y) =
    if x < minx || y < miny || x > maxx || y > maxy
      then def
      else arr UA.! coords
   where
    (Coords2D !minx !miny, Coords2D !maxx !maxy) = UA.bounds arr

  {-# INLINE empty #-}
  empty = ArrayLevel $ UA.array (Coords2D 0 0, Coords2D (-1) (-1)) []

  fromPairList pairs' =
    if null pairs'
      then empty
      else ArrayLevel $ UA.accumArray (flip const) def (min_bound, max_bound) pairs'
   where
    -- No need to set anything to pairs that have the default element;
    -- filter down our argument to only include pairs that are non-default
    -- tile.
    pairs = filter (\(_, tile) -> tile /= def) pairs'

    -- Compute minimum and maximum bounds
    min_bound = foldl' (\(Coords2D x1 y1) (Coords2D x2 y2) ->
                            Coords2D (min x1 x2) (min y1 y2))
                       (fst $ head pairs)
                       (fmap fst pairs)

    max_bound = foldl' (\(Coords2D x1 y1) (Coords2D x2 y2) ->
                            Coords2D (max x1 x2) (max y1 y2))
                       (fst $ head pairs)
                       (fmap fst pairs)

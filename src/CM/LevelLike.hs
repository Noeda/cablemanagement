{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}

module CM.LevelLike
  ( LevelLike(..)
  , RelativeCoordinable(..)
  , CharToTile(..)
  , TileMemorizer(..)
  , levelFromText
  )
where

import           CM.Coords

import           Data.Text                      ( Text )
import qualified Data.Text                     as T

-- | Class of level-like things.
class LevelLike l coords block | l -> coords block where
  tileAt       :: l -> coords -> block
  setTile      :: l -> coords -> block -> l
  fromPairList :: [(coords, block)] -> l
  empty        :: l

-- | Class of levels (or well, it doesn't really have to be levels) that can
-- memorize previously seen tiles in some way.
class TileMemorizer l coords block | l -> coords block where
  memorizedTileAt :: l -> coords -> Maybe block
  memorizeTile :: l -> coords -> block -> l

-- | Class of things where you can take coords and add `Coords2D` to get
-- another block.
--
-- This is used for rendering "remembered" tiles that are not currently visible
-- from player's perspective. This shouldn't go through any portals if the
-- world has them.
class RelativeCoordinable l coords where
  atRelativeCoords :: l -> coords -> Coords2D -> coords

-- | This type class is used with `levelFromText`. It defines a typeclass on
-- how to turn characters into tiles.
class CharToTile t where
  charToTile :: Char -> t

{-# INLINABLE levelFromText #-}
-- | This function makes a level out of a text representation.
--
-- This is designed to make it easy to define levels in-line in code.
--
-- This is not about how tiles are actually rendered. It's just for specifying
-- the levels themselves.
--
-- @
-- levelFromText
--   ["######"
--   ,"#..#.#"
--   ,"#....#"
--   ,"######"]
-- @
levelFromText :: (LevelLike l Coords2D t, CharToTile t) => [Text] -> l
levelFromText []    = empty
levelFromText lines = fromPairList $ concat $ go lines 0
 where
  go [] _ = []
  go (line : rest) y =
    fmap
        (\(x, ch) ->
          let !coords = Coords2D x y
              !tile   = charToTile ch
          in  (coords, tile)
        )
        (zip [0 ..] (T.unpack line))
      : go rest (y + 1)

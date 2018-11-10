{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}

module CM.LevelLike
  ( LevelLike(..)
  , LevelLikeIterable(..)
  , RelativeCoordinable(..)
  , CharToTile(..)
  , TileMemorizer(..)
  , levelFromText
  , lookForTile
  )
where

import           CM.Coords

import           Data.Int
import           Data.Text                      ( Text )
import qualified Data.Text                     as T

-- | Class of level-like things.
class LevelLike l coords block | l -> coords block where
  tileAt       :: l -> coords -> block
  setTile      :: l -> coords -> block -> l
  fromPairList :: [(coords, block)] -> l
  empty        :: l

-- | Class of level-like things that can iterate their blocks.
class LevelLike l coords block => LevelLikeIterable l coords block where
  -- | This should return the list of blocks in a level.
  --
  -- Some levels are infinite, where some "default" block is not stored.
  -- `listBlocks` is not guaranteed to return such blocks.
  listBlocks :: l -> [(coords, block)]

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

-- | Looks for a tile in a circle around some origin.
--
-- This relies on `RelativeCoordinable`.
--
-- This expands a circle from the original position.
{-# INLINABLE lookForTile #-}
lookForTile
  :: forall l coords block
   . (RelativeCoordinable l coords, LevelLike l coords block)
  => coords
  -> l
  -> Int  -- ^ Maximum number of blocks to check.
  -> (coords -> block -> Bool)
  -> Maybe (coords, block)
lookForTile origin level max_iterations tester = go 0 max_iterations
 where
  go !circle_size !counter =
    let candidates = concat
          [ [ (-circle_size, z)
            , (circle_size , z)
            , (z           , -circle_size)
            , (z           , circle_size)
            ]
          | z <- [-circle_size .. circle_size]
          ]
    in  go2 candidates counter
   where
    go2 :: [(Int16, Int16)] -> Int -> Maybe (coords, block)
    go2 _ !counter | counter <= 0 = Nothing
    go2 [] !counter               = go (circle_size + 1) counter
    go2 ((x, y) : rest) !counter =
      let !c           = Coords2D x y
          !test_coords = atRelativeCoords level origin c
          !block       = tileAt level test_coords
      in  if tester test_coords block
            then Just (test_coords, block)
            else go2 rest (counter - 1)


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

{-# LANGUAGE FlexibleContexts #-}

module LevelBenchmarks
  ( levelBenchmarks
  )
where

import           CriterionShim
import           Data.Foldable
import           Data.Int

import           CM.Coords
import           CM.ImapLevel
import           CM.ArrayLevel

levelBenchmarks :: [Benchmark]
levelBenchmarks =
  [ bgroup "imapLevelBenchmarks"  imapLevelBenchmarks
  , bgroup "arrayLevelBenchmarks" arrayLevelBenchmarks
  ]

imapLevelBenchmarks :: [Benchmark]
imapLevelBenchmarks =
  [ bench "sum 10000 tiles from an empty level"
    $ whnf (tileAtBench (empty :: IMapLevel Int)) [0 .. 10000]
  , bench "sum 10000 tiles at a populated (1 million tiles) level"
    $ whnf (uncurry tileAtBench) (populatedLevel :: IMapLevel Int, [0 .. 10000])
  ]

arrayLevelBenchmarks :: [Benchmark]
arrayLevelBenchmarks =
  [ bench "sum 10000 tiles from an empty level"
    $ whnf (tileAtBench (empty :: ArrayLevel Int)) [0 .. 10000]
  , bench "sum 10000 tiles at a populated (1 million tiles) level" $ whnf
    (uncurry tileAtBench)
    (populatedLevel :: ArrayLevel Int, [0 .. 10000])
  ]

populatedLevel :: LevelLike level Coords2D Int => level
populatedLevel = fromPairList
  [ (Coords2D x y, fromIntegral $ x + y)
  | x <- [-500 .. 500]
  , y <- [-500 .. 500]
  ]

{-# INLINE tileAtBench #-}
tileAtBench :: LevelLike level Coords2D Int => level -> [Int16] -> Int
tileAtBench level coords = foldl' (\accum item -> accum + tileAt level item)
                                  0
                                  [ Coords2D x 123 | x <- coords ]

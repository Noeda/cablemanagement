module Main
  ( main
  )
where

import           Criterion.Main

import           FovBenchmarks
import           LevelBenchmarks

main :: IO ()
main = defaultMain
  [ bgroup "Field of view benchmarks" fovBenchmarks
  , bgroup "Level benchmarks"         levelBenchmarks
  ]

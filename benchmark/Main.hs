module Main
  ( main
  )
where

import           Criterion.Main

import           LevelBenchmarks

main :: IO ()
main = defaultMain [bgroup "Level benchmarks" levelBenchmarks]

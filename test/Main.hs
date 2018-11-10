module Main
  ( main
  )
where

import           Test.Framework

import           CM.TextIO.Tests
import           CM.Chain.Tests
import           CM.Coords.Tests

main :: IO ()
main = defaultMain
  [ testGroup "CM.TextIO tests" textIOTests
  , testGroup "CM.Chain tests"  chainTests
  , testGroup "CM.Coords tests" coordTests
  ]

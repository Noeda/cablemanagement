module Main
  ( main
  )
where

import           Test.Framework

import           CM.TextIO.Tests

main :: IO ()
main = defaultMain [testGroup "CM.TextIO tests" textIOTests]

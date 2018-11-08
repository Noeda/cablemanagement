module CM.Chain.Tests
  ( chainTests
  )
where

import           Data.Int
import           Data.Maybe
import           Test.Framework
import           Test.Framework.Providers.QuickCheck2
import           Test.QuickCheck

import           CM.Chain
import           CM.Coords

instance Arbitrary Coords2D where
  arbitrary = Coords2D <$> arbitrary <*> arbitrary

chainTests :: [Test]
chainTests =
  [ testProperty "pulling single-part chain fully retracts it"
                 pullingSinglePartChainsRetractsIt
  , testProperty "pulling two-part chain results in a single-part chain"
                 pullTwoPartChain
  , testProperty "pulling 100-part straight chain results in 99-part chain"
                 pullHundredPartChain
  ]

pullingSinglePartChainsRetractsIt :: Coords2D -> Bool
pullingSinglePartChainsRetractsIt pos =
  pullChain (singlePartChain pos) () == CableFullyRetracted

pullTwoPartChain :: Coords2D -> Int16 -> Int16 -> Bool
pullTwoPartChain (Coords2D x y) dx' dy'
  = let
      dx            = (abs dx' `mod` 3) - 1
      dy            = (abs dy' `mod` 3) - 1
      Just twochain = layNewChain (singlePartChain (Coords2D x y))
                                  ()
                                  (Coords2D (x + dx) (y + dy))
    in
      case pullChain twochain () of
        Pulled chain ->
          chainLength chain == 1 && chainToList chain == [Coords2D x y]
        _ -> False

pullHundredPartChain :: Coords2D -> Int16 -> Int16 -> Bool
pullHundredPartChain (Coords2D x y) dx' dy'
  = let
      dx = (abs dx' `mod` 3) - 1
      dy = (abs dy' `mod` 3) - 1

      chain =
        go (singlePartChain (Coords2D x y)) (Coords2D x y) dx dy (99 :: Int)
    in
      case pullChain chain () of
        Pulled chain -> chainLength chain == 99 && case pullChain chain () of
          Pulled chain -> chainLength chain == 98
          _            -> False
        _ -> False
 where
  go chain _ _ _ 0 = chain
  go chain (Coords2D x y) dx dy n =
    let next_coords = Coords2D (x + dx) (y + dy)
    in  go (fromJust $ layNewChain chain () next_coords)
           next_coords
           dx
           dy
           (n - 1)

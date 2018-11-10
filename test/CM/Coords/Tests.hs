module CM.Coords.Tests
  ( coordTests
  )
where

import           Data.Word
import           Test.Framework
import           Test.Framework.Providers.QuickCheck2
import           Test.QuickCheck

import           CM.Coords

instance Arbitrary Coords2D where
  arbitrary = Coords2D <$> arbitrary <*> arbitrary

coordTests :: [Test]
coordTests =
  [ testProperty "int-to-coords2d and coords2d-to-int is identity"
                 intToCoordsIdentity
  , testProperty "coords2d-to-int and int-to-coords2d is identity"
                 coordsToIntIdentity
  ]

intToCoordsIdentity :: Coords2D -> Bool
intToCoordsIdentity coords = intToCoords2D (coords2DToInt coords) == coords

coordsToIntIdentity :: Word32 -> Bool
coordsToIntIdentity coords =
  coords2DToInt (intToCoords2D $ fromIntegral coords) == fromIntegral coords

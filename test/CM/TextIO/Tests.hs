module CM.TextIO.Tests
  ( textIOTests
  )
where

import           Data.Bits
import           Test.Framework
import           Test.Framework.Providers.QuickCheck2
import           Test.QuickCheck         hiding ( (.&.) )

import           CM.TextIO

instance Arbitrary Color3 where
  arbitrary = Color3 <$> bit6arb <*> bit6arb <*> bit6arb
   where
    bit6arb = (0x3f .&.) <$> arbitrary

textIOTests :: [Test]
textIOTests =
  [ testProperty "attributesToColors . colorsToAttributes is identity"
                 attributesToColorsIdentity
  ]

attributesToColorsIdentity :: Color3 -> Color3 -> Bool
attributesToColorsIdentity col1 col2 =
  let (col1', col2') = attributesToColors $ colorsToAttributes col1 col2
  in  col1' == col1 && col2' == col2

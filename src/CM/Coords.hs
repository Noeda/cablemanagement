{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module CM.Coords
  ( Coords2D(..)
  , zero
  , (.+)
  , (.-)
  )
where

import           Data.Data
import           Data.Int
import           Data.Ix
import           GHC.Generics

data Coords2D = Coords2D {-# UNPACK #-} !Int16 {-# UNPACK #-} !Int16
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Ix )

{-# INLINE zero #-}
zero :: Coords2D
zero = Coords2D 0 0

{-# INLINE (.+) #-}
infixl 6 .+
(.+) :: Coords2D -> Coords2D -> Coords2D
(.+) (Coords2D x1 x2) (Coords2D y1 y2) = Coords2D (x1 + x2) (y1 + y2)

{-# INLINE (.-) #-}
infixl 6 .-
(.-) :: Coords2D -> Coords2D -> Coords2D
(.-) (Coords2D x1 x2) (Coords2D y1 y2) = Coords2D (x1 - x2) (y1 - y2)

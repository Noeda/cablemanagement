{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiWayIf #-}

module CM.Coords
  ( Coords2D(..)
  , coords2DToInt
  , intToCoords2D
  , TileRotation(..)
  , PortalSwizzle(..)
  , ToCoords2D(..)
  , SwizzCoords2D(..)
  , Rotatable(..)
  , zero
  , (.+)
  , (.-)
  , minC
  , maxC
  , TiledCoordMoving(..)
  , movingByLocations
  )
where

import           Data.Bits
import           Data.Data
import           Data.Int
import           Data.Ix
import           Data.Word
import           GHC.Generics

import           CM.Portal

-- | Coordinates in 2D-space.
data Coords2D = Coords2D {-# UNPACK #-} !Int16 {-# UNPACK #-} !Int16
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Ix )

-- | Packs a `Coords2D` into an integer.
{-# INLINE coords2DToInt #-}
coords2DToInt :: Coords2D -> Int
coords2DToInt (Coords2D x y) =
  fromIntegral
    $   ((fromIntegral x :: Word32) `shiftL` 16)
    .|. (fromIntegral y :: Word32)

{-# INLINE intToCoords2D #-}
intToCoords2D :: Int -> Coords2D
intToCoords2D val = Coords2D (fromIntegral (val .&. 0xffff0000 `shiftR` 16))
                             (fromIntegral (val .&. 0x0000ffff))

-- | Same as `Coords2D` but may have different idea of how to move in each
-- cardinal direction. (This is influenced by portals).
--
-- `OnLeft` tile orientation is considered identity.
data SwizzCoords2D = SwizzCoords2D
  {-# UNPACK #-} !Coords2D
  !TileRotation    -- ^ How does coordinate system change after
                                  --   going through the portal.
  !PortalSwizzle   -- ^ Horizontal (left-right) swizzling
  !PortalSwizzle   -- ^ Vertical (up-down) swizzling
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )

class ToCoords2D a where
  -- | Extracts the `Coords2D` from swizzled coordinates.
  --
  -- This throws away the swizzling information but it's only relevant if you are
  -- performing operations to move coordinates.
  toCoords2D :: a -> Coords2D

instance ToCoords2D SwizzCoords2D where
  {-# INLINE toCoords2D #-}
  toCoords2D (SwizzCoords2D coords _ _ _) = coords

instance ToCoords2D Coords2D where
  {-# INLINE toCoords2D #-}
  toCoords2D = id

{-# INLINE zero #-}
zero :: Coords2D
zero = Coords2D 0 0

{-# INLINE (.+) #-}
infixl 6 .+
(.+) :: Coords2D -> Coords2D -> Coords2D
(.+) (Coords2D x1 y1) (Coords2D x2 y2) = Coords2D (x1 + x2) (y1 + y2)

{-# INLINE (.-) #-}
infixl 6 .-
(.-) :: Coords2D -> Coords2D -> Coords2D
(.-) (Coords2D x1 y1) (Coords2D x2 y2) = Coords2D (x1 - x2) (y1 - y2)

{-# INLINE maxC #-}
maxC :: Coords2D -> Coords2D -> Coords2D
maxC (Coords2D x1 y1) (Coords2D x2 y2) = Coords2D (max x1 x2) (max y1 y2)

{-# INLINE minC #-}
minC :: Coords2D -> Coords2D -> Coords2D
minC (Coords2D x1 y1) (Coords2D x2 y2) = Coords2D (min x1 x2) (min y1 y2)

-- | Moving around.
--
-- Coordinates may go through portals or weird topologies, so moving from point
-- in coordinates to another is not as simple as summing coordinates together.
--
-- Instead, you use these functions.
class TiledCoordMoving coords where
  type Context coords

  toRight :: Context coords -> coords -> coords
  toLeft :: Context coords -> coords -> coords
  toUp :: Context coords -> coords -> coords
  toDown :: Context coords -> coords -> coords
  toRightUp :: Context coords -> coords -> coords
  toRightDown :: Context coords -> coords -> coords
  toLeftUp :: Context coords -> coords -> coords
  toLeftDown :: Context coords -> coords -> coords

-- | Given two `Coords2D` returns one of the functions for `TiledCoordMoving`
{-# INLINE movingByLocations #-}
movingByLocations
  :: TiledCoordMoving coords
  => Coords2D
  -> Coords2D
  -> Maybe (Context coords -> coords -> coords)
movingByLocations (Coords2D x1 y1) (Coords2D x2 y2) = if
  | x2 == x1
  -> (if
       | y2 == y1 + 1 -> Just toDown
       | y2 == y1 - 1 -> Just toUp
       | otherwise    -> Nothing
     )
  | y2 == y1
  -> (if
       | x2 == x1 + 1 -> Just toRight
       | x2 == x1 - 1 -> Just toLeft
       | otherwise    -> Nothing
     )
  | x2 == x1 + 1
  -> (if
       | y2 == y1 + 1 -> Just toRightDown
       | y2 == y1 - 1 -> Just toRightUp
       | otherwise    -> Nothing
     )
  | x2 == x1 - 1
  -> (if
       | y2 == y1 + 1 -> Just toLeftDown
       | y2 == y1 - 1 -> Just toLeftUp
       | otherwise    -> Nothing
     )
  | otherwise
  -> Nothing


instance TiledCoordMoving Coords2D where
  type Context Coords2D = ()

  {-# INLINE toRight #-}
  toRight _ c = c .+ Coords2D 1 0
  {-# INLINE toLeft #-}
  toLeft _ c = c .- Coords2D (-1) 0
  {-# INLINE toUp #-}
  toUp _ c = c .- Coords2D 0 (-1)
  {-# INLINE toDown #-}
  toDown _ c = c .+ Coords2D 0 1
  {-# INLINE toRightUp #-}
  toRightUp _ c = c .+ Coords2D 1 (-1)
  {-# INLINE toRightDown #-}
  toRightDown _ c = c .+ Coords2D 1 1
  {-# INLINE toLeftUp #-}
  toLeftUp _ c = c .+ Coords2D (-1) (-1)
  {-# INLINE toLeftDown #-}
  toLeftDown _ c = c .+ Coords2D (-1) 1

-- | This instance uses swizzle rules to alter the rules how coordinates move
-- around.
instance TiledCoordMoving SwizzCoords2D where
  type Context SwizzCoords2D = ()

  {-# INLINE toRight #-}
  toRight _ = (`applyTransformation` Coords2D 1 0)
  {-# INLINE toLeft #-}
  toLeft _ = (`applyTransformation` Coords2D (-1) 0)
  {-# INLINE toUp #-}
  toUp _ = (`applyTransformation` Coords2D 0 (-1))
  {-# INLINE toDown #-}
  toDown _ = (`applyTransformation` Coords2D 0 1)
  {-# INLINE toRightUp #-}
  toRightUp _ = (`applyTransformation` Coords2D 1 (-1))
  {-# INLINE toRightDown #-}
  toRightDown _ = (`applyTransformation` Coords2D 1 1)
  {-# INLINE toLeftUp #-}
  toLeftUp _ = (`applyTransformation` Coords2D (-1) (-1))
  {-# INLINE toLeftDown #-}
  toLeftDown _ = (`applyTransformation` Coords2D (-1) 1)

{-# INLINE applyTransformation #-}
applyTransformation :: SwizzCoords2D -> Coords2D -> SwizzCoords2D
applyTransformation (SwizzCoords2D c rot hmirroring vmirroring) coords =
  let c' = c .+ applyRotation rot (applyMirror hmirroring vmirroring coords)
  in  SwizzCoords2D c' rot hmirroring vmirroring

instance Rotatable Coords2D where
  {-# INLINE applyRotation #-}
  applyRotation Rot0   coords         = coords
  applyRotation Rot90  (Coords2D x y) = Coords2D (-y) x
  applyRotation Rot180 (Coords2D x y) = Coords2D (-x) (-y)
  applyRotation Rot270 (Coords2D x y) = Coords2D y (-x)

{-# INLINE applyMirror #-}
applyMirror :: PortalSwizzle -> PortalSwizzle -> Coords2D -> Coords2D
applyMirror NoSwizzle NoSwizzle coords         = coords
applyMirror Mirror    NoSwizzle (Coords2D x y) = Coords2D (negate x) y
applyMirror NoSwizzle Mirror    (Coords2D x y) = Coords2D x (negate y)
applyMirror Mirror    Mirror    (Coords2D x y) = Coords2D (negate x) (negate y)

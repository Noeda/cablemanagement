{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}

module CM.Portal
  ( TilePortal(..)
  , TileRotation(..)
  , TileOrientation(..)
  , PortalSwizzle(..)
  , Reversible(..)
  , Rotatable(..)
  , Swizzable(..)
  , portalOrientation
  )
where

import           Data.Data
import           GHC.Generics

-- | Class of things that have a concept of being reversed.
class Reversible a where
  dreverse :: a -> a

data TileOrientation
  = OnLeft
  | OnTop
  | OnRight
  | OnBottom
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Enum )

instance Swizzable TileOrientation where
  applyVSwizz Mirror OnTop = OnBottom
  applyVSwizz Mirror OnBottom = OnTop
  applyVSwizz _ OnTop = OnTop
  applyVSwizz _ OnBottom = OnBottom
  applyVSwizz _ OnLeft = OnLeft
  applyVSwizz _ OnRight = OnRight
  applyHSwizz Mirror OnLeft = OnRight
  applyHSwizz Mirror OnRight = OnLeft
  applyHSwizz _ OnLeft = OnLeft
  applyHSwizz _ OnRight = OnRight
  applyHSwizz _ OnTop = OnTop
  applyHSwizz _ OnBottom = OnBottom

instance Reversible TileOrientation where
  dreverse OnLeft = OnRight
  dreverse OnTop = OnBottom
  dreverse OnRight = OnLeft
  dreverse OnBottom = OnTop

data TileRotation = Rot0 | Rot90 | Rot180 | Rot270
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Enum )

instance Reversible TileRotation where
  dreverse Rot0 = Rot0
  dreverse Rot90 = Rot270
  dreverse Rot180 = Rot180
  dreverse Rot270 = Rot90

instance Semigroup TileRotation where
  {-# INLINE (<>) #-}
  x <> y = x `mappend` y

instance Monoid TileRotation where
  {-# INLINE mempty #-}
  mempty = Rot0

  {-# INLINE mappend #-}
  -- Zero rotations
  Rot0 `mappend` x = x
  x `mappend` Rot0 = x

  -- 90 rotations
  Rot90 `mappend` Rot90 = Rot180
  Rot90 `mappend` Rot180 = Rot270
  Rot90 `mappend` Rot270 = Rot0
  Rot180 `mappend` Rot90 = Rot270
  Rot270 `mappend` Rot90 = Rot0

  -- 180 rotations
  Rot180 `mappend` Rot180 = Rot0
  Rot180 `mappend` Rot270 = Rot90
  Rot270 `mappend` Rot180 = Rot90

  -- 270 rotations
  Rot270 `mappend` Rot270 = Rot180

class Rotatable a where
  applyRotation :: TileRotation -> a -> a

instance Rotatable TileOrientation where
  {-# INLINE applyRotation #-}
  applyRotation Rot0 x = x
  applyRotation Rot90 OnTop = OnRight
  applyRotation Rot90 OnRight = OnBottom
  applyRotation Rot90 OnBottom = OnLeft
  applyRotation Rot90 OnLeft = OnTop
  applyRotation Rot180 OnTop = OnBottom
  applyRotation Rot180 OnRight = OnLeft
  applyRotation Rot180 OnBottom = OnTop
  applyRotation Rot180 OnLeft = OnRight
  applyRotation Rot270 OnTop = OnLeft
  applyRotation Rot270 OnRight = OnTop
  applyRotation Rot270 OnBottom = OnRight
  applyRotation Rot270 OnLeft = OnBottom

data PortalSwizzle
  = NoSwizzle
  | Mirror
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Enum )

class Swizzable a where
  applyHSwizz :: PortalSwizzle -> a -> a
  applyVSwizz :: PortalSwizzle -> a -> a

instance Reversible PortalSwizzle where
  dreverse Mirror = Mirror
  dreverse NoSwizzle = NoSwizzle

instance Semigroup PortalSwizzle where
  {-# INLINE (<>) #-}
  x <> y = x `mappend` y

instance Monoid PortalSwizzle where
  {-# INLINE mempty #-}
  mempty = NoSwizzle

  {-# INLINE mappend #-}
  NoSwizzle `mappend` x = x
  x `mappend` NoSwizzle = x
  _ `mappend` _ = NoSwizzle

data TilePortal target = TilePortal !TileOrientation !TileRotation !PortalSwizzle !PortalSwizzle !target
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Functor, Foldable, Traversable )

{-# INLINE portalOrientation #-}
portalOrientation :: TilePortal a -> TileOrientation
portalOrientation (TilePortal orientation _ _ _ _) = orientation

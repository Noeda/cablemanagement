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
  , portalOrientation
  )
where

import           Data.Data
import           GHC.Generics

data TileOrientation
  = OnLeft
  | OnTop
  | OnRight
  | OnBottom
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Enum )

data TileRotation = Rot0 | Rot90 | Rot180 | Rot270
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Enum )

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

data PortalSwizzle
  = NoSwizzle
  | Mirror
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Enum )

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

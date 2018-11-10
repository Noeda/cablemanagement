-- | This module implements levels based on IntMap.
--
-- It is efficient for both reading and mutation.
--

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE BangPatterns #-}

module CM.ImapLevel
  ( IMapLevel()
  , module CM.Coords
  , module CM.LevelLike
  )
where

import           Data.Bits
import           Data.Default.Class
import           Data.Data
import           Data.Foldable                  ( foldl' )
import qualified Data.IntMap.Strict            as IM
import           Data.Maybe                     ( fromMaybe )
import           Data.Traversable               ( )
import           Data.Word
import           GHC.Generics

import           CM.Coords
import           CM.LevelLike

newtype IMapLevel tile = IMapLevel (IM.IntMap tile)
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Functor, Foldable, Traversable )

instance (Default tile, Eq tile) => LevelLikeIterable (IMapLevel tile) Coords2D tile where
  {-# INLINE listBlocks #-}
  listBlocks (IMapLevel tilemap) = (\(key, value) -> (intToCoords2D key, value)) <$> IM.assocs tilemap

instance (Default tile, Eq tile) => LevelLike (IMapLevel tile) Coords2D tile where
  {-# INLINE tileAt #-}
  tileAt (IMapLevel tilemap) coords =
    fromMaybe def $ IM.lookup (coords2DToInt coords) tilemap

  {-# INLINE setTile #-}
  setTile (IMapLevel tilemap) coords block = IMapLevel $
    if block == def
      then IM.delete (coords2DToInt coords) tilemap
      else IM.insert (coords2DToInt coords) block tilemap

  {-# INLINE empty #-}
  empty = IMapLevel IM.empty

  fromPairList pairs = IMapLevel $ foldl' folder IM.empty pairs
   where
    folder imap (coords, block) =
      if block /= def
        then IM.insert (coords2DToInt coords) block imap
        else imap

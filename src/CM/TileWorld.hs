{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}

module CM.TileWorld
  ( TileWorld()
  , WorldCoords2D()
  , WorldLevel(..)
  , ToWorldLevel(..)
  , AnyTileWorld(..)
  )
where

import qualified Data.Array.Unboxed            as UA
import           Data.Data
import           Data.Default.Class
import           Data.Foldable
import qualified Data.IntMap.Strict            as IM
import qualified Data.Map.Strict               as M
import           Data.Maybe
import           GHC.Generics

import           CM.ImapLevel
import           CM.ArrayLevel
import           CM.Portal
import           CM.WorldLike

data WorldLevel tile
  = IMapLevel !(IMapLevel tile)
  | ArrayLevel !(ArrayLevel tile)

deriving instance (Eq tile, UA.IArray UA.UArray tile) => Eq (WorldLevel tile)
deriving instance (Ord tile, UA.IArray UA.UArray tile) => Ord (WorldLevel tile)
deriving instance (Show tile, UA.IArray UA.UArray tile) => Show (WorldLevel tile)
deriving instance Typeable (WorldLevel tile)
deriving instance Generic (WorldLevel tile)

class ToWorldLevel a tile where
  toWorldLevel :: a tile -> WorldLevel tile

instance ToWorldLevel IMapLevel tile where
  toWorldLevel = IMapLevel

instance ToWorldLevel ArrayLevel tile where
  toWorldLevel = ArrayLevel

data LevelNode tile = LevelNode
  { level   :: !(WorldLevel tile)
  , portals :: !(IM.IntMap (M.Map TileOrientation (TilePortal WorldSimpleCoords2D))) }

emptyNode :: Default tile => LevelNode tile
emptyNode = LevelNode {level = IMapLevel empty, portals = IM.empty}

deriving instance (Eq tile, UA.IArray UA.UArray tile) => Eq (LevelNode tile)
deriving instance (Ord tile, UA.IArray UA.UArray tile) => Ord (LevelNode tile)
deriving instance (Show tile, UA.IArray UA.UArray tile) => Show (LevelNode tile)

data TileWorld tile = TileWorld
  { levels       :: !(IM.IntMap (LevelNode tile))
  , runningIndex :: !Int }

deriving instance (Eq tile, UA.IArray UA.UArray tile) => Eq (TileWorld tile)
deriving instance (Ord tile, UA.IArray UA.UArray tile) => Ord (TileWorld tile)
deriving instance (Show tile, UA.IArray UA.UArray tile) => Show (TileWorld tile)

newtype TileWorldLevelKey = TileWorldLevelKey Int
  deriving ( Eq, Ord, Show, Typeable, Data, Generic )

data WorldCoords2D = WorldCoords2D {-# UNPACK #-} !Int {-# UNPACK #-} !SwizzCoords2D
  deriving ( Eq, Ord, Show, Typeable, Data, Generic )

-- | Sasme as `WorldCoords2D` but there is no swizzling or rotation information.
data WorldSimpleCoords2D = WorldSimpleCoords2D {-# UNPACK #-} !Int {-# UNPACK #-} !Coords2D
  deriving ( Eq, Ord, Show, Typeable, Data, Generic )

instance Default tile => TilePortalWorldLike (TileWorld tile) where
  type WorldCoords (TileWorld tile) = WorldCoords2D
  type WorldPortalCoords (TileWorld tile) = WorldSimpleCoords2D
  type LevelKey (TileWorld tile) = TileWorldLevelKey
  type Level (TileWorld tile) = WorldLevel tile

  addPortal (WorldCoords2D level_key swizzled) tileportal world =
    world { levels = IM.alter
               (Just . (\node -> node { portals = IM.alter
                           (Just . M.insert (portalOrientation tileportal) tileportal . fromMaybe M.empty)
                           (coords2DToInt source_coords)
                           (portals node) }) . fromMaybe emptyNode)
               level_key
               (levels world) }
   where
    source_coords = toCoords2D swizzled

  setLevel (TileWorldLevelKey index) level world =
    world { levels = IM.insert index (LevelNode { level = level, portals = IM.empty }) (levels world) }

  initial =
    let initial_running_index = 0
     in (TileWorld { runningIndex = initial_running_index, levels = IM.empty },
         TileWorldLevelKey initial_running_index)

instance (Default tile, Eq tile, UA.IArray UA.UArray tile) => LevelLike (TileWorld tile) WorldCoords2D tile where
  tileAt world (WorldCoords2D levelkey swizzcoords) =
    case lvl of
      IMapLevel level -> tileAt level coords
      ArrayLevel level -> tileAt level coords
   where
    coords = toCoords2D swizzcoords
    lvl = level $ fromMaybe emptyNode $ IM.lookup levelkey (levels world)

  empty = fst initial

  fromPairList pairs =
    let (world, _first_level_index) = initial
        level_pairlists = IM.assocs $ splitWorldCoordedToCoords pairs
     in foldl' folder world level_pairlists
   where
    folder world (level_key, pairlist) =
      setLevel (TileWorldLevelKey level_key) (IMapLevel $ fromPairList pairlist) world

-- | Utility function to implement `fromPairList` correctly for `TileWorld`, it
-- splits a pair list constructed on `WorldCoords2D` to `Coords2D`, for each
-- level.
splitWorldCoordedToCoords :: [(WorldCoords2D, a)] -> IM.IntMap [(Coords2D, a)]
splitWorldCoordedToCoords = foldl' folder IM.empty
 where
  folder accum (WorldCoords2D level_key (toCoords2D -> coords), item) =
    IM.alter (Just . (:) (coords, item) . fromMaybe []) level_key accum

-- | Hack type to work around polymorphic type synonyms not allowed in type
-- families.
--
-- If they were allowed, then in `TiledCoordMoving` instance for
-- `WorldCoords2D` we would have `TileWorld` be directly the `Context` for
-- coordinate moving.
newtype AnyTileWorld = AnyTileWorld (forall a. TileWorld a)

instance TiledCoordMoving WorldCoords2D where
  type Context WorldCoords2D = AnyTileWorld

  {-# INLINE toRight #-}
  toRight = moveWorldCoords (toRight ()) [OnLeft]
  toLeft = moveWorldCoords (toLeft ()) [OnRight]
  toUp = moveWorldCoords (toUp ()) [OnBottom]
  toDown = moveWorldCoords (toDown ()) [OnTop]

  toRightUp = moveWorldCoords (toRightUp ()) [OnLeft, OnBottom]
  toRightDown = moveWorldCoords (toRightUp ()) [OnLeft, OnTop]
  toLeftUp = moveWorldCoords (toLeftUp ()) [OnRight, OnBottom]
  toLeftDown = moveWorldCoords (toLeftUp ()) [OnRight, OnTop]


-- | A function that jumps through a portal.
--
-- This function is common code shared by all functions in `TiledCoordMoving`.
{-# INLINABLE moveWorldCoords #-}
moveWorldCoords
  :: (SwizzCoords2D -> SwizzCoords2D)
  -> [TileOrientation]
  -> AnyTileWorld
  -> WorldCoords2D
  -> WorldCoords2D
moveWorldCoords coordinate_move allowed_orientations (AnyTileWorld w) (WorldCoords2D level_key swizzcoords@(SwizzCoords2D _ rot hswizz vswizz))
  = -- Would the resulting coordinates end up inside the portal?
    case IM.lookup (coords2DToInt $ toCoords2D target_coords) src_portals of
      -- If not, then this is just normal movement.
    Nothing      -> WorldCoords2D level_key target_coords
    -- If there is a portal at the target, let's jump through it
    -- (and apply all the fancy portal rotation and swizzling)
    Just portals -> case lookupFirst allowed_orientations portals of
      Nothing -> WorldCoords2D level_key target_coords
      Just (TilePortal _ portal_rot hswizzling vswizzling (WorldSimpleCoords2D target_level_key target_coords))
        -> WorldCoords2D
          target_level_key
          (SwizzCoords2D target_coords
                         (rot <> portal_rot)
                         (hswizzling <> hswizz)
                         (vswizzling <> vswizz)
          )
 where
  target_coords = coordinate_move swizzcoords
  src_portals   = maybe IM.empty portals $ IM.lookup level_key (levels w)

-- | Utility function that returns the first matching key from a map.
{-# INLINE lookupFirst #-}
lookupFirst :: Ord k => [k] -> M.Map k v -> Maybe v
lookupFirst []         _   = Nothing
lookupFirst (x : rest) map = case M.lookup x map of
  Nothing   -> lookupFirst rest map
  jv@Just{} -> jv

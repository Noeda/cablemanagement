{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}

module CM.TileWorld
  (
  -- * World
    TileWorld()
  , WorldCoords2D(..)
  , WorldSimpleCoords2D(..)
  , toSimpleCoords
  -- * Levels in a world.
  , WorldLevel(..)
  , ToWorldLevel(..)
  -- * Type hackery
  , AnyTileWorld(..)
  -- * Re-exports
  , module CM.WorldLike
  )
where

import           Data.Data
import           Data.Default.Class
import           Data.Foldable
import qualified Data.IntMap.Strict            as IM
import qualified Data.Map.Strict               as M
import           Data.Maybe
import           GHC.Generics

import           CM.Coords
import           CM.ImapLevel
import           CM.LiftLevel
import           CM.ArrayLevel
import           CM.Portal
import           CM.WorldLike

-- | Type of a world level.
data WorldLevel tile
  = IMapLevel !(IMapLevel tile)
  | ArrayLevel !(ArrayLevel tile)
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )

-- | Class of things that can be turned into `WorldLevel`.
--
-- This is implemented for level types like `IMapLevel` or `ArrayLevel`.
class ToWorldLevel a tile where
  toWorldLevel :: a tile -> WorldLevel tile

instance ToWorldLevel IMapLevel tile where
  toWorldLevel = IMapLevel

instance ToWorldLevel ArrayLevel tile where
  toWorldLevel = ArrayLevel

-- | This type stores information about a level and its portals.
data LevelNode tile = LevelNode
  { level   :: !(WorldLevel tile)
  , portals :: !(IM.IntMap (M.Map TileOrientation (TilePortal WorldSimpleCoords2D))) }
  deriving ( Eq, Ord, Show, Typeable, Data, Generic )

emptyNode :: Default tile => LevelNode tile
emptyNode = LevelNode {level = IMapLevel empty, portals = IM.empty}

-- | This is the type of tile world, where you can add and remove levels
-- dynamically and link them with portals.
data TileWorld tile = TileWorld
  { levels       :: !(IM.IntMap (LevelNode tile))
  , runningIndex :: !Int }
  deriving ( Eq, Ord, Show, Typeable, Data, Generic )

-- | This is the type of a key than can be referred to a level inside
-- `TileWorld`.
newtype TileWorldLevelKey = TileWorldLevelKey Int
  deriving ( Eq, Ord, Show, Typeable, Data, Generic )

-- | World coordinates.
--
-- World coordinates store the level, position on that level and swizzling
-- information. Swizzle applies tranformation to coordinate system, e.g. x is
-- mirrored.
--
-- This is the type of player coordinates but not much else. Most things should
-- use `WorldSimpleCoords2D` unless the orientation of whatever thing having
-- coordinates matters (most things just care about position, not how they are
-- rotated).
--
-- Note that world coordinates do not evaluate equal even if they point to same
-- location in the world if their orientation/swizzling is different. You can
-- use `toSimpleCoords` before you do comparisons.
data WorldCoords2D = WorldCoords2D {-# UNPACK #-} !Int {-# UNPACK #-} !SwizzCoords2D
  deriving ( Eq, Ord, Show, Typeable, Data, Generic )

-- | Same as `WorldCoords2D` but there is no swizzling or rotation information.
--
-- This stores just level key and coordinates on that level.
data WorldSimpleCoords2D = WorldSimpleCoords2D {-# UNPACK #-} !Int {-# UNPACK #-} !Coords2D
  deriving ( Eq, Ord, Show, Typeable, Data, Generic )

instance ToCoords2D WorldCoords2D where
  {-# INLINE toCoords2D #-}
  toCoords2D (WorldCoords2D _ swizz) = toCoords2D swizz

instance ToCoords2D WorldSimpleCoords2D where
  {-# INLINE toCoords2D #-}
  toCoords2D (WorldSimpleCoords2D _ coords) = coords

{-# INLINE toSimpleCoords #-}
-- | This transforms world coordinates to simple world coordinates.
--
-- This is not reversible; swizzling information is lost.
toSimpleCoords :: WorldCoords2D -> WorldSimpleCoords2D
toSimpleCoords (WorldCoords2D level_key swizzled) =
  WorldSimpleCoords2D level_key (toCoords2D swizzled)

instance Default tile => TilePortalWorldLike (TileWorld tile) where
  type WorldCoords (TileWorld tile) = WorldCoords2D
  type WorldPortalCoords (TileWorld tile) = WorldSimpleCoords2D
  type LevelCoords (TileWorld tile) = Coords2D
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
    (world { levels = IM.insert index (LevelNode { level = level, portals = IM.empty }) (levels world) }
    ,\coords -> WorldCoords2D index (SwizzCoords2D coords Rot0 NoSwizzle NoSwizzle))

  initial =
    let initial_running_index = 0
     in (TileWorld { runningIndex = initial_running_index, levels = IM.empty },
         TileWorldLevelKey initial_running_index)

instance (Default tile, Eq tile) => LevelLike (TileWorld tile) WorldCoords2D tile where
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
      fst $ setLevel (TileWorldLevelKey level_key) (IMapLevel $ fromPairList pairlist) world

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
data AnyTileWorld = forall a. AnyTileWorld (TileWorld a)

-- | The context uses `AnyTileWorld`. You can use `liftLevel` to turn a
-- polymorphic `TileWorld` type to `AnyTileWorld`.
instance TiledCoordMoving WorldCoords2D where
  type Context WorldCoords2D = AnyTileWorld

  {-# INLINE toRight #-}
  toRight = moveWorldCoords (toRight ()) [OnLeft]
  toLeft = moveWorldCoords (toLeft ()) [OnRight]
  toUp = moveWorldCoords (toUp ()) [OnBottom]
  toDown = moveWorldCoords (toDown ()) [OnTop]

  toRightUp = moveWorldCoords (toRightUp ()) [OnLeft, OnBottom]
  toRightDown = moveWorldCoords (toRightDown ()) [OnLeft, OnTop]
  toLeftUp = moveWorldCoords (toLeftUp ()) [OnRight, OnBottom]
  toLeftDown = moveWorldCoords (toLeftDown ()) [OnRight, OnTop]


-- | A function that jumps through a portal.
--
-- This function is common code shared by all functions in `TiledCoordMoving`
-- and should not be called directly by the user.
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

instance LiftLevel (TileWorld tile) (TileWorld tile) where
  {-# INLINE liftLevel #-}
  liftLevel = id

instance LiftLevel (TileWorld tile) AnyTileWorld where
  {-# INLINE liftLevel #-}
  liftLevel = liftLevel'

liftLevel' :: TileWorld a -> AnyTileWorld
liftLevel' = AnyTileWorld

-- | Utility function that returns the first matching key from a map.
{-# INLINE lookupFirst #-}
lookupFirst :: Ord k => [k] -> M.Map k v -> Maybe v
lookupFirst []         _   = Nothing
lookupFirst (x : rest) map = case M.lookup x map of
  Nothing   -> lookupFirst rest map
  jv@Just{} -> jv

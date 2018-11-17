{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}
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
import qualified Data.IntSet                   as IS
import qualified Data.IntMap.Strict            as IM
import qualified Data.Map.Strict               as M
import           Data.Maybe
import           Data.Monoid             hiding ( (<>) )
import           Data.Semigroup
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

instance (Default tile, Eq tile) => LevelLikeIterable (WorldLevel tile) Coords2D tile where
  {-# INLINE listBlocks #-}
  listBlocks (IMapLevel lvl) = listBlocks lvl
  listBlocks (ArrayLevel lvl) = listBlocks lvl

instance (Default tile, Eq tile) => LevelLike (WorldLevel tile) Coords2D tile where
  {-# INLINE tileAt #-}
  tileAt (IMapLevel lvl) coords = tileAt lvl coords
  tileAt (ArrayLevel lvl) coords = tileAt lvl coords
  {-# INLINE setTile #-}
  setTile (IMapLevel lvl) coords block = IMapLevel $ setTile lvl coords block
  setTile (ArrayLevel lvl) coords block = ArrayLevel $ setTile lvl coords block

  fromPairList = IMapLevel . fromPairList
  empty = IMapLevel empty

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

emptyNode :: (Default tile, Eq tile) => LevelNode tile
emptyNode = LevelNode {level = IMapLevel empty, portals = IM.empty}

-- | This is the type of tile world, where you can add and remove levels
-- dynamically and link them with portals.
data TileWorld tile = TileWorld
  { levels       :: !(IM.IntMap (LevelNode tile))
  , tileMemory :: !(M.Map WorldSimpleCoords2D tile)
  , runningIndex :: !Int }
  deriving ( Eq, Ord, Show, Typeable, Data, Generic )

-- | This is the type of a key than can be referred to a level inside
-- `TileWorld`.
type TileWorldLevelKey = Int

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

instance LiftLevel WorldCoords2D WorldSimpleCoords2D where
  {-# INLINE liftLevel #-}
  liftLevel = toSimpleCoords

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

instance TileMemorizer (TileWorld tile) WorldSimpleCoords2D tile where
  {-# INLINE memorizedTileAt #-}
  memorizedTileAt !world !coords = M.lookup coords (tileMemory world)

  {-# INLINE memorizeTile #-}
  memorizeTile !world !coords !tile = world { tileMemory = M.insert coords tile (tileMemory world) }

instance RelativeCoordinable (TileWorld tile) WorldCoords2D where
  {-# INLINE atRelativeCoords #-}
  atRelativeCoords _world (WorldCoords2D level_key (SwizzCoords2D coords rot hswizz vswizz)) relativity =
    WorldCoords2D level_key (SwizzCoords2D (coords .+ applyRotation rot relativity) rot hswizz vswizz)

instance (Default tile, Eq tile) => TilePortalWorldLike (TileWorld tile) where
  type WorldCoords (TileWorld tile) = WorldCoords2D
  type WorldPortalCoords (TileWorld tile) = WorldSimpleCoords2D
  type LevelCoords (TileWorld tile) = Coords2D
  type LevelKey (TileWorld tile) = TileWorldLevelKey
  type LevelSnapshot (TileWorld tile) = LevelNode tile
  type Level (TileWorld tile) = WorldLevel tile

  toPortalCoords _ (WorldCoords2D index swizz) = WorldSimpleCoords2D index (toCoords2D swizz)

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

  setLevel index level world =
    (world { levels = IM.insert index (LevelNode { level = level, portals = IM.empty }) (levels world) }
    ,\coords -> WorldCoords2D index (SwizzCoords2D coords Rot0 NoSwizzle NoSwizzle))

  getLevelSnapshot index world =
    IM.lookup index (levels world)

  setLevelSnapshot index lvl world =
    world { levels = IM.insert index lvl (levels world) }

  initial =
    let initial_running_index = 0
     in (TileWorld { runningIndex = initial_running_index, levels = IM.empty, tileMemory = M.empty },
         initial_running_index)

  addLevel lvl world =
    let index = runningIndex world
     in (world { levels = IM.insert index (LevelNode { level = lvl, portals = IM.empty }) (levels world)
           , runningIndex = index + 1 }
        ,\coords -> WorldCoords2D index (SwizzCoords2D coords Rot0 NoSwizzle NoSwizzle))

instance (Default tile, Eq tile) => LevelLikeIterable (TileWorld tile) WorldCoords2D tile where
  {-# INLINEABLE listBlocks #-}
  listBlocks !world =
    concat $ foldl' folder [] (IM.assocs $ levels world)
   where
    folder accum (level_key, node) =
      ((\(coords, block) -> (WorldCoords2D level_key (SwizzCoords2D coords Rot0 NoSwizzle NoSwizzle), block)) <$>
      listBlocks (level node)):accum

instance (Default tile, Eq tile) => LevelLike (TileWorld tile) WorldCoords2D tile where
  {-# INLINEABLE tileAt #-}
  tileAt !world (WorldCoords2D !levelkey !swizzcoords) =
    case lvl of
      IMapLevel level -> tileAt level coords
      ArrayLevel level -> tileAt level coords
   where
    coords = toCoords2D swizzcoords
    lvl = level $ fromMaybe emptyNode $ IM.lookup levelkey (levels world)

  {-# INLINEABLE setTile #-}
  setTile !world (WorldCoords2D !levelkey !swizzcoords) !block =
    case lvl of
      IMapLevel level -> world { levels = IM.adjust (\node -> node { level = IMapLevel $ setTile level coords block }) levelkey (levels world) }
      ArrayLevel level -> world { levels = IM.adjust (\node -> node { level = ArrayLevel $ setTile level coords block }) levelkey (levels world) }
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
      fst $ setLevel level_key (IMapLevel $ fromPairList pairlist) world

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

instance TiledCoordMoving WorldSimpleCoords2D where
  type Context WorldSimpleCoords2D = AnyTileWorld

  {-# INLINE toRight #-}
  toRight tworld (WorldSimpleCoords2D level_key coords) =
    toSimpleCoords $ toRight tworld (WorldCoords2D level_key (SwizzCoords2D coords Rot0 NoSwizzle NoSwizzle))
  {-# INLINE toLeft #-}
  toLeft tworld (WorldSimpleCoords2D level_key coords) =
    toSimpleCoords $ toLeft tworld (WorldCoords2D level_key (SwizzCoords2D coords Rot0 NoSwizzle NoSwizzle))
  {-# INLINE toUp #-}
  toUp tworld (WorldSimpleCoords2D level_key coords) =
    toSimpleCoords $ toUp tworld (WorldCoords2D level_key (SwizzCoords2D coords Rot0 NoSwizzle NoSwizzle))
  {-# INLINE toDown #-}
  toDown tworld (WorldSimpleCoords2D level_key coords) =
    toSimpleCoords $ toDown tworld (WorldCoords2D level_key (SwizzCoords2D coords Rot0 NoSwizzle NoSwizzle))

  {-# INLINE toRightUp #-}
  toRightUp tworld (WorldSimpleCoords2D level_key coords) =
    toSimpleCoords $ toRightUp tworld (WorldCoords2D level_key (SwizzCoords2D coords Rot0 NoSwizzle NoSwizzle))
  {-# INLINE toRightDown #-}
  toRightDown tworld (WorldSimpleCoords2D level_key coords) =
    toSimpleCoords $ toRightDown tworld (WorldCoords2D level_key (SwizzCoords2D coords Rot0 NoSwizzle NoSwizzle))
  {-# INLINE toLeftUp #-}
  toLeftUp tworld (WorldSimpleCoords2D level_key coords) =
    toSimpleCoords $ toLeftUp tworld (WorldCoords2D level_key (SwizzCoords2D coords Rot0 NoSwizzle NoSwizzle))
  {-# INLINE toLeftDown #-}
  toLeftDown tworld (WorldSimpleCoords2D level_key coords) =
    toSimpleCoords $ toLeftDown tworld (WorldCoords2D level_key (SwizzCoords2D coords Rot0 NoSwizzle NoSwizzle))

-- | The context uses `AnyTileWorld`. You can use `liftLevel` to turn a
-- polymorphic `TileWorld` type to `AnyTileWorld`.
instance TiledCoordMoving WorldCoords2D where
  type Context WorldCoords2D = AnyTileWorld

  {-# INLINE toRight #-}
  toRight = moveWorldCoords (toRight ()) [OnLeft]
  {-# INLINE toLeft #-}
  toLeft = moveWorldCoords (toLeft ()) [OnRight]
  {-# INLINE toUp #-}
  toUp = moveWorldCoords (toUp ()) [OnBottom]
  {-# INLINE toDown #-}
  toDown = moveWorldCoords (toDown ()) [OnTop]

  {-# INLINE toRightUp #-}
  toRightUp = moveWorldCoords (toRightUp ()) [OnLeft, OnBottom]
  {-# INLINE toRightDown #-}
  toRightDown = moveWorldCoords (toRightDown ()) [OnLeft, OnTop]
  {-# INLINE toLeftUp #-}
  toLeftUp = moveWorldCoords (toLeftUp ()) [OnRight, OnBottom]
  {-# INLINE toLeftDown #-}
  toLeftDown = moveWorldCoords (toLeftDown ()) [OnRight, OnTop]

-- | This functions gets the set of all level keys in a world.
--
-- This includes level keys from portals or memory, so this can contain keys
-- for levels that are not actually there.
allLevelKeys :: TileWorld tile -> IS.IntSet
allLevelKeys world =
  -- level keys
  IM.keysSet (levels world)
    <>
  -- keys referred to in level memory
       (IS.fromList $ fmap (\(WorldSimpleCoords2D level_key _) -> level_key)
                           (M.keys (tileMemory world))
       )
    <>
  -- keys in portal target
       (   IS.fromList
       $   concat
       $   concat
       $   fmap
             (\map ->
               fmap
                   (\(TilePortal _ _ _ _ (WorldSimpleCoords2D level_key _)) ->
                     level_key
                   )
                 $ M.elems map
             )
       .   IM.elems
       .   portals
       <$> IM.elems (levels world)
       )


-- | The semigroup instance will treat two worlds as separate. The world on
-- right side will have its level keys re-numbered to prevent clashes with the
-- world on left side. This means all level keys to the second world are
-- invalidated on append.
instance Semigroup (TileWorld tile) where
  world1 <> world2 = combined_world { runningIndex = key2_to_key1 highest_world2_key + 1 }
   where
    combined_world = world1 {
        levels = levels world1 <> remapped_levels,
        tileMemory = tileMemory world1 <> remapped_memory
      }

    -- sidenote: Might be useful to lensify traversals on keys later
    -- Would make remapping level keys much simpler.

    remapped_levels' = IM.mapKeys key2_to_key1 (levels world2)
    remapped_levels = flip fmap remapped_levels' $ \node ->
      node { portals = flip fmap (portals node) $ fmap $ \(TilePortal ori rot hswizz vswizz (WorldSimpleCoords2D tgt coords)) -> TilePortal ori rot hswizz vswizz (WorldSimpleCoords2D (key2_to_key1 tgt) coords) }

    remapped_memory = M.mapKeys (\(WorldSimpleCoords2D key coords) -> WorldSimpleCoords2D (key2_to_key1 key) coords) (tileMemory world2)

    key2_to_key1 key = key - lowest_world2_key + highest_world1_key + 1

    world1_level_keys :: IS.IntSet
    world1_level_keys = allLevelKeys world1

    world2_level_keys :: IS.IntSet
    world2_level_keys = allLevelKeys world2

    lowest_world2_key = if IS.null world2_level_keys then 0 else IS.findMin world2_level_keys
    highest_world2_key = if IS.null world2_level_keys then 0 else IS.findMax world2_level_keys
    highest_world1_key = if IS.null world1_level_keys then 0 else IS.findMax world1_level_keys

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
moveWorldCoords coordinate_move allowed_orientations' (AnyTileWorld w) (WorldCoords2D level_key swizzcoords@(SwizzCoords2D _ rot hswizz vswizz))
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
  allowed_orientations = flip fmap allowed_orientations'
    $ \ori -> applyRotation rot $ applyHSwizz hswizz $ applyVSwizz vswizz ori
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

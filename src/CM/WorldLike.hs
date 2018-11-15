{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}

module CM.WorldLike
  ( TilePortalWorldLike(..)
  , worldFromText
  , WBuilder()
  , worldFromBuilder
  , putLevel
  , putLevelOverrides
  , makePortalGroup
  , PortalGroup()
  )
where

import           Control.Monad.Fix
import           Control.Monad.Trans.State.Strict
import           Data.Char
import           Data.Data
import           Data.Default.Class
import           Data.Foldable
import qualified Data.IntMap.Strict            as IM
import           Data.Maybe
import           Data.Monoid
import           Data.Proxy
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Traversable
import qualified Data.Map.Strict               as M
import           GHC.Generics

import           CM.Coords
import           CM.LevelLike
import           CM.LiftLevel
import           CM.Portal

-- | Typeclass of worlds that have portals in them.
class TilePortalWorldLike w where
  type WorldCoords w
  type WorldPortalCoords w
  type LevelCoords w
  type LevelKey w
  type Level w
  type LevelSnapshot w

  -- | Adds a new portal to the world.
  addPortal :: WorldCoords w                     -- ^ Where to place the portal.
            -> TilePortal (WorldPortalCoords w)  -- ^ Definition of the portal and its target.
            -> w
            -> w

  -- | Adds a level to the world.
  addLevel :: Level w
           -> w
           -> (w, LevelCoords w -> WorldCoords w)

  -- | Replaces a level at some level key in the world.
  --
  -- This will add a new disconnected level if the level doesn't exist.
  --
  -- Returns the new world and a function to go from level coordinates to world
  -- coordinates.
  setLevel :: LevelKey w
           -> Level w
           -> w
           -> (w, LevelCoords w -> WorldCoords w)

  -- | Returns the underlying level in a world, so that it can be used to
  -- replace another level. Use with `setLevelSnapshot`.
  getLevelSnapshot :: LevelKey w -> w -> Maybe (LevelSnapshot w)
  setLevelSnapshot :: LevelKey w -> LevelSnapshot w -> w -> w

  -- | Returns a world with one empty level, and a key to that level.
  initial :: (w, LevelKey w)

  -- | Turns normal coordinates into portal coordinates.
  toPortalCoords :: Proxy w -> WorldCoords w -> WorldPortalCoords w

data WorldBuilderState w = WorldBuilderState
  { world :: !w
  , nextIndex :: !(LevelKey w)
  , portalPlacements :: !(M.Map Char (WorldCoords w)) }

initialBuilder :: TilePortalWorldLike w => WorldBuilderState w
initialBuilder =
  let (w, key) = initial
  in  WorldBuilderState {world = w, nextIndex = key, portalPlacements = M.empty}

data WBuilderState w = WBuilderState
  { portalGroups :: IM.IntMap (M.Map Char TileOrientation)
  , levelBuilds :: IM.IntMap (Level w, M.Map (Int, Char) (Coords2D, TileOrientation))
  , runningIndex :: Int }

emptyWBuilderState :: WBuilderState w
emptyWBuilderState = WBuilderState
  { portalGroups = IM.empty
  , levelBuilds  = IM.empty
  , runningIndex = 0
  }

newtype WBuilder w a = WBuilder (State (WBuilderState w) a)
  deriving ( Functor, Applicative, Monad, MonadFix )

data PortalGroup = PortalGroup Int (M.Map Char TileOrientation)
  deriving ( Eq, Ord, Show, Typeable, Data, Generic )

makePortalGroup :: [(Char, TileOrientation)] -> WBuilder w PortalGroup
makePortalGroup (M.fromList -> pgroup) = WBuilder $ do
  st <- get
  put $ st { portalGroups = IM.insert (runningIndex st) pgroup (portalGroups st)
           , runningIndex = runningIndex st + 1
           }
  return $ PortalGroup (runningIndex st) pgroup

putLevel
  :: forall tile w
   . (CharToTile tile, LevelLike (Level w) Coords2D tile)
  => [PortalGroup]
  -> [Text]
  -> WBuilder w ()
putLevel group texts = putLevelOverrides group texts ([] :: [(Char, tile)])

putLevelOverrides
  :: (CharToTile tile, LevelLike (Level w) Coords2D tile)
  => [PortalGroup]
  -> [Text]
  -> [(Char, tile)]
  -> WBuilder w ()
putLevelOverrides portal_groups texts (M.fromList -> overrides) =
  WBuilder $ modify $ \old -> old
    { levelBuilds  = IM.insert (runningIndex old)
                               (lvl, portal_map)
                               (levelBuilds old)
    , runningIndex = runningIndex old + 1
    }
 where
  lookup_portals ch
    = let lookups = catMaybes $ fmap
            (\(PortalGroup portal_key portal_map) ->
              (,) portal_key <$> M.lookup ch portal_map
            )
            portal_groups
      in
        case lookups of
          []                  -> Nothing
          [(portal_key, rot)] -> Just (portal_key, rot)
          _ ->
            error $ "putLevelOverrides: portal character ambiguous: " <> show ch

  (lvl, portal_map)
    = let
        (pairlist, map) =
          flip execState ([], M.empty)
            $ for_ (zip [0 ..] texts)
            $ \(y, txt) -> for_ (zip [0 ..] $ T.unpack txt) $ \(x, ch) -> do
                case lookup_portals ch of
                  Nothing -> do
                    let !tile =
                          fromMaybe (charToTile ch) $ M.lookup ch overrides
                    modify $ \(pairlist, map) ->
                      ((Coords2D x y, tile) : pairlist, map)
                  Just (portal_key, port) -> modify $ \(pairlist, map) ->
                    ( pairlist
                    , M.insert (portal_key, ch) (Coords2D x y, port) map
                    )
      in  (fromPairList pairlist, map)

toggleLowerUpper :: Char -> Char
toggleLowerUpper ch =
  let lch = toLower ch
      uch = toUpper ch
  in  if lch == uch
        then
          error
          $  "toggleLowerUpper: character "
          <> show ch
          <> " does not have upper/lowercase."
        else (if ch /= lch then lch else uch)

worldFromBuilder
  :: forall w w'
   . ( TilePortalWorldLike w
     , LevelCoords w ~ Coords2D
     , TiledCoordMoving (WorldCoords w)
     , LiftLevel w w'
     , Context (WorldCoords w) ~ w'
     )
  => WBuilder w ()
  -> w
worldFromBuilder (WBuilder stateful) =
  fst
    $ flip
        execState
        ( fst initial :: w
        , M.empty :: M.Map (Int, Char) (WorldCoords w, TileOrientation)
        )
    $ do
        for_ (levelBuilds built) $ \(lvl, lvl_portal_targets) -> do
          (st, portal_targets) <- get
          let (new_world, coordinator) = addLevel lvl st
              portal_assocs =
                M.fromList
                  $ fmap
                      (\((portals_key, ch), (value, rot)) ->
                        ((portals_key, ch), (coordinator value, rot))
                      )
                  $ M.assocs lvl_portal_targets
          put (new_world, M.union portal_targets portal_assocs)
        (_, portal_targets) <- get
        for_ (M.assocs portal_targets)
          $ \((portals_key, portal_ch), (wcoordinates, srot)) ->
              case
                  M.lookup (portals_key, toggleLowerUpper portal_ch)
                           portal_targets
                of
            -- TODO: somehow communicate a warning.
            -- The next condition is hit if you put a portal without destination.
                  Nothing                   -> return ()
                  Just (tcoordinates, trot) -> do
                    modify $ \(world, pt) ->
                      ( addPortal
                        wcoordinates
                        ( TilePortal srot Rot0 NoSwizzle NoSwizzle
                        $ toPortalCoords
                            (Proxy :: Proxy w)
                            (nudge tcoordinates (liftLevel world) trot)
                        )
                        world
                      , pt
                      )
  where built = execState stateful emptyWBuilderState

nudge
  :: TiledCoordMoving coords
  => coords
  -> Context coords
  -> TileOrientation
  -> coords
nudge coords ctx OnTop    = toUp ctx coords
nudge coords ctx OnLeft   = toLeft ctx coords
nudge coords ctx OnRight  = toRight ctx coords
nudge coords ctx OnBottom = toDown ctx coords

worldFromText
  :: forall w w' tile
   . ( LevelCoords w ~ Coords2D
     , TiledCoordMoving (WorldCoords w)
     , LevelLike (Level w) Coords2D tile
     , Default tile
     , CharToTile tile
     , Context (WorldCoords w) ~ w'
     , TilePortalWorldLike w
     , LiftLevel w w'
     )
  => [(Char, TileOrientation, TileRotation, PortalSwizzle, PortalSwizzle)]
  -> [([Text], [(Char, tile)])]
  -> w
worldFromText specportalletters texts =
  world
    $ flip execState initialBuilder
    $ do
    -- Build levels
        for_ texts toLevel
        -- Place portals
        old <- get
        for_ (M.assocs $ portalPlacements old)
             (setPortal (portalPlacements old))
 where
  portalletters
    :: M.Map Char (TileOrientation, TileRotation, PortalSwizzle, PortalSwizzle)
  portalletters = M.fromList $ concatMap
    (\(ch, orientation, rot, xswizzle, yswizzle) ->
      [(ch, (orientation, rot, xswizzle, yswizzle))]
    )
    specportalletters

  setPortal
    :: M.Map Char (WorldCoords w)
    -> (Char, WorldCoords w)
    -> State (WorldBuilderState w) ()
  setPortal portal_placements2 (ch, loc) = do
    let other_ch = if toLower ch == ch then toUpper ch else toLower ch
    old <- get
    let w = world old
    case M.lookup other_ch portal_placements2 of
      Nothing        -> error $ "No matching portal for: " <> show ch
      Just other_loc -> do
        let Just (target_orientation, _tgt_rot, _, _) =
              M.lookup other_ch portalletters
            Just (ori, rot, xswizz, yswizz) = M.lookup ch portalletters
            tgt_loc                         = case target_orientation of
              OnTop    -> toUp (liftLevel w) other_loc
              OnLeft   -> toLeft (liftLevel w) other_loc
              OnRight  -> toRight (liftLevel w) other_loc
              OnBottom -> toDown (liftLevel w) other_loc

        let new_world = addPortal
              loc
              (TilePortal ori
                          rot
                          xswizz
                          yswizz
                          (toPortalCoords (Proxy :: Proxy w) tgt_loc)
              )
              w
        put $ old { world = new_world }


  toLevel (text, M.fromList -> tile_overrides) = do
    let (lined, portals) =
          runState (go text 0) (M.empty :: M.Map Coords2D Char)
        lvl = fromPairList lined :: Level w
    old <- get
    let (new_world, fun) = addLevel lvl (world old)
        transformed_portals =
          M.fromList $ flip fmap (M.assocs portals) $ \(coords, ch) ->
            (ch, fun coords)
    put $ old
      { world            = new_world
      , portalPlacements = M.union transformed_portals (portalPlacements old)
      }
   where
    go []            _ = return []
    go (line : rest) y = do
      pairlist <- for (zip [0 ..] (T.unpack line)) $ \(x, ch) -> do
        let !coords = Coords2D x y
            tile    = fromMaybe (charToTile ch) $ M.lookup ch tile_overrides
        if toLower ch `M.member` portalletters
          then do
            modify $ M.insert coords ch
            return (coords, def)
          else return (coords, tile)
      (pairlist <>) <$> go rest (y + 1)

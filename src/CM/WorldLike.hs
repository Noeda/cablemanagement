{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}

module CM.WorldLike
  ( TilePortalWorldLike(..)
  , worldFromText
  )
where

import           Control.Monad.Trans.State.Strict
import           Data.Char
import           Data.Default.Class
import           Data.Foldable
import           Data.Maybe
import           Data.Monoid
import           Data.Proxy
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Traversable
import qualified Data.Map.Strict               as M

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

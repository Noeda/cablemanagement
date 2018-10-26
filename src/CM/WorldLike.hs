{-# LANGUAGE TypeFamilies #-}

module CM.WorldLike
  ( TilePortalWorldLike(..)
  )
where

import           CM.Portal

-- | Typeclass of worlds that have portals in them.
class TilePortalWorldLike w where
  type WorldCoords w
  type WorldPortalCoords w
  type LevelCoords w
  type LevelKey w
  type Level w

  -- | Adds a new portal to the world.
  addPortal :: WorldCoords w                     -- ^ Where to place the portal.
            -> TilePortal (WorldPortalCoords w)  -- ^ Definition of the portal and its target.
            -> w
            -> w

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

  -- | Returns a world with one empty level, and a key to that level.
  initial :: (w, LevelKey w)

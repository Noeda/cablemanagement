{-# LANGUAGE TypeFamilies #-}

module CM.WorldLike
  ( TilePortalWorldLike(..)
  )
where

import           CM.Portal

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

  initial :: (w, LevelKey w)

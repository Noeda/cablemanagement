{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE GADTs #-}

-- | This module is all about rendering levels on a screen.
--

module CM.LevelRender
  ( -- * Rendering functions
    renderLevel
  , renderBeamcastFOV
  , renderRaycastingView
    -- * Type classes
    --
    -- You may need to implement bunch of these for your types to make them
    -- renderable.
  , EntityView(..)
  , Obstacle(..)
  , TiledRenderer(..)
  , TileToRenderedTile(..)
    -- * Specifying rendering parameters.
  , Coords2DRenderView(..)
  , coords2DRenderView
  , RelativeRenderView(..)
  , relativeRenderView
  -- * Line casting tools
  , bresenham
  , distanceToLine
  )
where

import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State.Strict
import           Data.Data
import           Data.Foldable
import           Data.Int
import qualified Data.IntMap.Strict            as IM
import           Data.Maybe
import           GHC.Generics

import           CM.Coords
import           CM.LevelLike
import           CM.LiftLevel


-- | Implement this class for outputs that draw some kind of discretely spaced
-- tiles.
--
-- This is very similar to `CM.TextIO.TextIO` except more general; this one
-- doesn't assume character display.
class TiledRenderer m displaytile | m -> displaytile where
  displaySize :: m Coords2D
  setTile :: Coords2D -> displaytile -> m ()

data Obscuring = Visible | Obscured
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Enum )

-- | How to convert a tile into a rendered tile for display.
class TileToRenderedTile tile displaytile where
  toRenderedTile :: tile -> Obscuring -> displaytile

-- | Describes where and how much to render on display.
--
-- It is recommended to use `coords2DRenderView` to construct this type in case
-- we add more fields in the future; that function would fill in defaults.
data Coords2DRenderView = Coords2DRenderView
  { topLeftOnDisplay :: !Coords2D  -- ^ Top-left coordinate on display where to render.
  , topLeftOnLevel   :: !Coords2D  -- ^ Which coordinate on level corresponds to top-left coordinate given in `topLeftOnDisplay`.
  , widthHeight      :: !Coords2D  -- ^ Width and height of the rendering.
  }
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )

{-# INLINABLE coords2DRenderView #-}
coords2DRenderView :: Coords2D -> Coords2D -> Coords2D -> Coords2DRenderView
coords2DRenderView = Coords2DRenderView

-- | Describes how to obtain entities for rendering.
class EntityView view entitycoords entitydisplaytile where
  viewEntity :: entitycoords -> view -> Maybe entitydisplaytile

-- Trivial `EntityView` that doesn't render any entities.
instance EntityView () entitycoords entitydisplaytile where
  viewEntity _ _ = Nothing

-- | Describes orientation where on the screen and which position on the game
-- world to use to draw a level.
--
-- It takes in a world-specific coordinates and a square that describe where in
-- the display to render.
data RelativeRenderView coords = RelativeRenderView
  { levelStartCoords :: !coords
  , relativeTopLeft :: !Coords2D
  , relativeViewWidthHeight :: !Coords2D }
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Functor, Foldable, Traversable )

relativeRenderView
  :: coords -> Coords2D -> Coords2D -> RelativeRenderView coords
relativeRenderView = RelativeRenderView

-- | Class of things that can block your view.
--
-- Used by renderers that compute field of view, to determine if some tile is
-- an obstacle.
class Obstacle tile where
  isObstacle :: tile -> Bool

-- | Render a level with no field of view or other fancy stuff on a tiled
-- display.
--
-- This is computationally most efficient renderer.
{-# INLINE renderLevel #-}
renderLevel
  :: forall m displaytile l tile view
   . ( Monad m
     , TiledRenderer m displaytile
     , LevelLike l Coords2D tile
     , TileToRenderedTile tile displaytile
     , EntityView view Coords2D displaytile
     )
  => l
  -> Coords2DRenderView
  -> view
  -> m ()
renderLevel level renderview entityview = do
  Coords2D !tw !th <- displaySize
  for_ [ly .. (ly + h - 1)] $ \(!y) -> for_ [lx .. (lx + w - 1)] $ \(!x) -> do
    let !dx = x - lx + dox
        !dy = y - ly + doy
    when (dx >= 0 && dx < tw && dy >= 0 && dy < th) $ do
      let !src_coords  = Coords2D x y
          !tile        = tileAt level src_coords
          !displaytile = toRenderedTile tile Visible
      case viewEntity src_coords entityview of
        Nothing   -> setTile (Coords2D dx dy) displaytile
        Just tile -> setTile (Coords2D dx dy) tile
 where
  Coords2D !dox !doy = topLeftOnDisplay renderview
  Coords2D !lx  !ly  = topLeftOnLevel renderview
  Coords2D !w   !h   = widthHeight renderview

data RayCastState coord = RayCastState !coord {-# UNPACK #-} !Int16 {-# UNPACK #-} !Int16

-- | Renders a level using a raycasting algorithm with sphere obstacles.
{-# INLINE renderRaycastingView #-}
renderRaycastingView
  :: forall m l liftedl tile view displaytile coords
   . ( Monad m
     , TiledRenderer m displaytile
     , Obstacle tile
     , LevelLike l coords tile
     , TileToRenderedTile tile displaytile
     , EntityView view coords displaytile
     , TiledCoordMoving coords
     , Context coords ~ liftedl
     , LiftLevel l liftedl
     )
  => l
  -> RelativeRenderView coords
  -> view
  -> m ()
renderRaycastingView !world !view !entityview = do
  let !tile = tileAt world (levelStartCoords view)
  drawTile center_x center_y (levelStartCoords view) tile
  -- Cast ray to all top and bottom parts
  for_ [disp_left .. disp_right] $ \target_x -> do
    shootRay (Coords2D target_x 0)
    shootRay (Coords2D target_x disp_bottom)
  -- Cast ray to all left and right parts
  for_ [disp_top + 1 .. disp_bottom - 1] $ \target_y -> do
    shootRay (Coords2D 0 target_y)
    shootRay (Coords2D disp_right target_y)
 where
  -- Figure out widths and heights and where the center of the specified view
  -- is on the display.
  Coords2D !disp_left !disp_top = relativeTopLeft view
  Coords2D !disp_right !disp_bottom =
    relativeTopLeft view .+ relativeViewWidthHeight view .- Coords2D (-1) (-1)
  Coords2D !center_x !center_y = Coords2D ((disp_left + disp_right) `div` 2)
                                          ((disp_top + disp_bottom) `div` 2)

  center_x05 :: Double
  center_x05 = fromIntegral center_x + 0.5
  center_y05 :: Double
  center_y05 = fromIntegral center_y + 0.5

  {-# INLINABLE shootRay #-}
  shootRay (Coords2D !target_x !target_y) =
    void
      $ runExceptT
      $ flip evalStateT (RayCastState (levelStartCoords view) center_x center_y)
      $ bresenham center_x center_y target_x target_y
      $ \(!dx') !dy' -> unless (dx' == center_x && dy' == center_y) $ do
          RayCastState !prev_coord !prev_x !prev_y <- get
          let !mover = fromMaybe (error "impossible") $ movingByLocations
                (Coords2D prev_x prev_y)
                (Coords2D dx' dy')
              !coords = mover (liftLevel world) prev_coord
          put $ RayCastState coords dx' dy'
          let !tile = tileAt world coords
              !dist = distanceToLine (fromIntegral dx' + 0.5 :: Double)
                                     (fromIntegral dy' + 0.5)
                                     center_x05
                                     center_y05
                                     target_x05
                                     target_y05
          -- 0.27 came from trial and error, this value should be between 0.0
          -- and 0.5 or the fov algorithm becomes unsound.
          lift $ lift $ when (dist <= 0.270001) $ drawTile dx' dy' coords tile
          when (isObstacle tile)
            $
            -- Does the ray intersect a tile sphere?
              when (dist <= 0.50001)
            $ lift
            $ throwE ()
   where
    target_x05 :: Double
    target_x05 = fromIntegral target_x + 0.5
    target_y05 :: Double
    target_y05 = fromIntegral target_y + 0.5

  {-# INLINE drawTile #-}
  drawTile !dispx !dispy !coords !tile = case viewEntity coords entityview of
    Nothing   -> setTile (Coords2D dispx dispy) (toRenderedTile tile Visible)
    Just tile -> setTile (Coords2D dispx dispy) tile


{-# INLINE distanceToLine #-}
distanceToLine :: Floating a => a -> a -> a -> a -> a -> a -> a
distanceToLine !x0 !y0 !x1 !y1 !x2 !y2 =
  abs ((y2 - y1) * x0 - (x2 - x1) * y0 + x2 * y1 - y2 * x1)
    / sqrt ((y2 - y1) ** 2 + (x2 - x1) ** 2)

{-# INLINE bresenham #-}
bresenham
  :: (Applicative f, Num a, Ord a)
  => a
  -> a
  -> a
  -> a
  -> (a -> a -> f ())
  -> f ()
bresenham !x1 !y1 !x2 !y2 action
  | abs (y2 - y1) < abs (x2 - x1) = if x1 > x2
    then lowslope (negate x1) y1 (negate x2) y2 (\x y -> action (negate x) y)
    else lowslope x1 y1 x2 y2 action
  | otherwise = if y1 > y2
    then lowslope (negate y1) x1 (negate y2) x2 (\y x -> action x (negate y))
    else lowslope y1 x1 y2 x2 (flip action)

{-# INLINE lowslope #-}
lowslope
  :: (Applicative f, Num a, Ord a)
  => a
  -> a
  -> a
  -> a
  -> (a -> a -> f ())
  -> f ()
lowslope !x1 !y1 !x2 !y2 action = go x1 y1 (2 * deltay - deltax)
 where
  !deltax  = x2 - x1
  !deltay  = if deltay' < 0 then negate deltay' else deltay'
  !deltay' = y2 - y1
  !yi      = if deltay' < 0 then -1 else 1

  go !x !y !d | x <= x2 =
    let (!y', !d') = if d > 0 then (y + yi, d - 2 * deltax) else (y, d)
    in  action x y *> go (x + 1) y' (d' + 2 * deltay)
  go _ _ _ = pure ()
-- | Renders a level using Isaac's beamcasting algorithm
-- http://www.roguebasin.com/index.php?title=Isaac_s_fast_beamcasting_LOS
--
-- This function requires a large number of typeclasses to be implemented for
-- types involved with the rendering.
--
-- `TiledRenderer` is required for base monad to know how to draw things to
-- display.
--
-- `Obstacle` instance is required to know which tiles block view.
--
-- `LevelLike` instance is required to be able to look up tiles on a level or
-- world.
--
-- `TileToRenderedTile` is required to specify how to turn tiles into
-- displayable tiles (where whatever is "displayable" is determined by
-- `TiledRenderer` instance e.g. text or a tiled image).
--
-- `EntityView` is required to look up entities from the map. Entities are
-- rendered on top of level elements if there's an entity present.
--
-- `TiledCoordMoving` is required to define how to move in different directions
-- given some coordinate in a world. This enables the field of view algorithm
-- to traverse through portals or other weird topologies. This renderer does
-- not look up any world tiles with random access; all tiles are fetched by
-- using this instance to move in the world one tile at a time.
--
-- `TiledCoordMoving` is used with associated type `Context` and `LiftLevel`;
-- @LiftLevel l l@ is implemented for all types. If the associated type for
-- your `TiledCoordMoving` is the same as the type of the level, then
-- `LiftLevel` is already implemented for you. However, if the types are
-- different, you need to implement `LiftLevel` to turn the rendered level into
-- a context usable with `TiledCoordMoving`.
{-# INLINE renderBeamcastFOV #-}
renderBeamcastFOV
  :: forall m l liftedl tile view displaytile coords
   . ( Monad m
     , TiledRenderer m displaytile
     , Obstacle tile
     , LevelLike l coords tile
     , TileToRenderedTile tile displaytile
     , EntityView view coords displaytile
     , TiledCoordMoving coords
     , Context coords ~ liftedl
     , LiftLevel l liftedl
     )
  => l
  -> RelativeRenderView coords
  -> view
  -> Int
  -> m ()
renderBeamcastFOV !level !view !entityview !num_beams = do
  -- Render all the quadrants (the arguments are x-mirroring and y-mirroring)
  renderQuadrant False False
  renderQuadrant True  False
  renderQuadrant False True
  renderQuadrant True  True

  -- Render the center tile
  drawTile center_x center_y (levelStartCoords view) Visible
 where
  -- Figure out widths and heights and where the center of the specified view
  -- is on the display.
  Coords2D !disp_w    !disp_h   = relativeViewWidthHeight view
  Coords2D !disp_left !disp_top = relativeTopLeft view
  Coords2D !disp_right !disp_bottom =
    relativeTopLeft view .+ relativeViewWidthHeight view .- Coords2D (-1) (-1)
  Coords2D !center_x !center_y = Coords2D ((disp_left + disp_right) `div` 2)
                                          ((disp_top + disp_bottom) `div` 2)

  -- This function renders a quadrant. Use xmirror' and ymirror' arguments to
  -- control which quadrant.
  renderQuadrant xmirror' ymirror' =
    flip evalStateT
         (IM.singleton (coords2DToInt (Coords2D 0 0)) (levelStartCoords view))
      $ forSlopes_
      $ \slope_idx -> go slope_idx slope_idx 1 0 (num_beams - 1) 0 0
   where
    {-# INLINE xmirror #-}
    xmirror :: Num a => a -> a
    xmirror v = if xmirror' then negate v else v
    {-# INLINE ymirror #-}
    ymirror :: Num a => a -> a
    ymirror v = if ymirror' then negate v else v

    lookupCoords :: Coords2D -> StateT (IM.IntMap coords) m coords
    lookupCoords coords@(Coords2D x y) = do
      imap <- get
      case IM.lookup (coords2DToInt coords) imap of
        Nothing -> do
          -- Determine which direction we should traverse to find coordinates.
          let !x' = signum x
              !y' = signum y
          coords_rec <- lookupCoords (Coords2D (x - x') (y - y'))
          case movingByLocations' (Coords2D (x - x') (y - y')) coords of
            Nothing    -> error "impossible"
            Just mover -> do
              let !levelcoords = mover (liftLevel level) coords_rec
              modify $ IM.insert (coords2DToInt coords) levelcoords
              return levelcoords
        Just coords -> return coords


    -- This is a wrapper around `movingByLocations` (see CM.Coords module) to
    -- take mirroring into account.
    movingByLocations' c1@(Coords2D !x1 !y1) (Coords2D !x2 !y2) =
      let !dx = xmirror $ x2 - x1
          !dy = ymirror $ y2 - y1
      in  movingByLocations c1 (Coords2D (x1 + dx) (y1 + dy))

    -- This is our big and ugly loop function where the real meat of the work
    -- is done. (It may be best to follow the roguebasin link given in the
    -- documentation of this function to understasnd what's happening).
    go !_ !_ !_ !mini !maxi !_ !_ | mini > maxi = return ()
    go !_ !_ !_ !_ !_ !prev_x !prev_y
      | prev_x > (disp_w `div` 2) + 1 || prev_y > (disp_h `div` 2) + 1
      = return ()
    go !slope_idx !v !u !mini !maxi !_ !_ = do
      let !y'' = v `div` num_beams
          !x'' = u - y
          !y   = y''
          !x   = x''
          !y'  = fromIntegral y
          !x'  = fromIntegral x
          !cor = num_beams - (v `mod` num_beams)

      loc1 <- lookupCoords (Coords2D x' y')
      loc2 <- lookupCoords (Coords2D (x' - 1) (y' + 1))

      let !tile_at_loc1 = tileAt level loc1
          !tile_at_loc2 = tileAt level loc2

      !new_mini <- if mini < cor
        then do
          lift $ drawTile (xmirror x' + center_x)
                          (ymirror y' + center_y)
                          loc1
                          Visible
          return $ if isObstacle tile_at_loc1 then cor else mini
        else return mini

      !new_maxi <- if maxi > cor
        then do
          lift $ drawTile (xmirror (x' - 1) + center_x)
                          (ymirror (y' + 1) + center_y)
                          loc2
                          Visible
          return $ if isObstacle tile_at_loc2 then cor else maxi
        else return maxi

      go slope_idx (v + slope_idx) (u + 1) new_mini new_maxi x' y'


  -- Helper function to traverse all slopes
  {-# INLINE forSlopes_ #-}
  forSlopes_ = for_ [1 .. num_beams - 1]

  -- This function is called when we actually want to render something.
  --
  -- dx and dy  are the display position of the tile, 'coords' is the level
  -- coordinates for the tile and 'visibility' is 'Visible' if we are rendering
  -- the tile as currently visible (as opposed not visible which is usually
  -- rendered as a gray tile). Currently this algorithm only renders visible
  -- tiles so visibility is always 'Visible'.
  drawTile !dx !dy !_ !_
    | dx < disp_left || dy < disp_top || dx > disp_right || dy > disp_bottom
    = return () -- Don't draw anything out of bounds.
  drawTile !dx !dy !coords !visibility = case viewEntity coords entityview of
    Nothing ->
      setTile (Coords2D dx dy) (toRenderedTile (tileAt level coords) visibility)
    Just tile -> setTile (Coords2D dx dy) tile

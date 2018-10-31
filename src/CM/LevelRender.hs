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
  , renderNonVisibleTiles
    -- * Type classes
    --
    -- You may need to implement bunch of these for your types to make them
    -- renderable.
  , EntityView(..)
  , Obstacle(..)
  , TiledRenderer(..)
  , TileToRenderedTile(..)
  , Obscuring(..)
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
import           Control.Monad.ST
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State.Strict
import           Data.Data
import           Data.Foldable
import           Data.Int
import qualified Data.IntMap.Strict            as IM
import qualified Data.IntSet                   as IS
import           Data.IORef
import           Data.List                      ( sortBy )
import qualified Data.Map.Strict               as M
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set                      as S
import qualified Data.Vector.Unboxed           as VU
import qualified Data.Vector.Unboxed.Mutable   as VUM
import           GHC.Generics
import           System.IO.Unsafe               ( unsafePerformIO )

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
  isObstacle :: tile -> Int

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

data RayCastState coord = RayCastState !coord {-# UNPACK #-} !Int16 {-# UNPACK #-} !Int16 {-# UNPACK #-} !Int

-- | Stores the rendered coordinates in some rendering function.
newtype RenderedCoordinates = RenderedCoordinates IS.IntSet
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )

-- | Renders tiles that are not visible.
--
-- The render view and rendered coordinates should be exactly what you get from
-- another rendering function.
{-# INLINE renderNonVisibleTiles #-}
renderNonVisibleTiles
  :: forall m l coords tile displaytile memorycoords
   . ( Monad m
     , LevelLike l coords tile
     , TiledRenderer m displaytile
     , TileMemorizer l memorycoords tile
     , LiftLevel coords memorycoords
     , RelativeCoordinable l coords
     , TileToRenderedTile tile displaytile
     )
  => l
  -> RelativeRenderView coords
  -> RenderedCoordinates         -- ^ This is used to communicate which coordinates were rendered already and shouldn't be rendered over in this function.
  -> m ()
renderNonVisibleTiles !world !view !(RenderedCoordinates exclusions) =
  for_ [disp_top .. disp_bottom] $ \(!y) ->
    for_ [disp_left .. disp_right] $ \(!x) -> do
      let !disp_coords = Coords2D x y
      let !level_coords = atRelativeCoords
            world
            (levelStartCoords view)
            (Coords2D (x - center_x) (y - center_y))
      unless (coords2DToInt disp_coords `IS.member` exclusions) $ do
        case memorizedTileAt world $ liftLevel level_coords of
          Nothing             -> return ()
          Just (tile :: tile) -> do
            let !drawtile = toRenderedTile tile Obscured
            setTile disp_coords drawtile
 where
  Coords2D !disp_left !disp_top = relativeTopLeft view
  Coords2D !disp_right !disp_bottom =
    relativeTopLeft view .+ relativeViewWidthHeight view .- Coords2D (-1) (-1)
  Coords2D !center_x !center_y = Coords2D
    (((disp_right - disp_left + 1) `div` 2) + disp_left)
    (((disp_bottom - disp_top + 1) `div` 2) + disp_top)

data SPair a b = SPair !a !b


data Quadrant = TopLeft | TopRight | BottomLeft | BottomRight
  deriving ( Eq, Ord )

-- internal function used for computeRaycastViews
transformQuadrant :: Num a => Quadrant -> a -> a -> (a, a)
transformQuadrant TopLeft     x y = (x, y)
transformQuadrant TopRight    x y = ((-x), y)
transformQuadrant BottomLeft  x y = (x, (-y))
transformQuadrant BottomRight x y = ((-x), (-y))

packCoordinates :: [[Coords2D]] -> PrecomputedRays
packCoordinates coords = PrecomputedRays $ runST $ do
  mvec <- VUM.new vec_len
  flip evalStateT 0 $ for_ reordered $ \coord_list -> do
    idx <- nextIdx
    lift $ VUM.write mvec idx (length coord_list)
    for_ coord_list $ \(Coords2D x y) -> do
      idx1 <- nextIdx
      idx2 <- nextIdx
      lift $ VUM.write mvec idx1 $ fromIntegral x
      lift $ VUM.write mvec idx2 $ fromIntegral y
  VU.unsafeFreeze mvec
 where
  nextIdx :: Monad m => StateT Int m Int
  nextIdx = do
    idx <- get
    put (idx + 1)
    return idx
  vec_len    = sum (fmap length reordered) * 2 + length reordered
  reordered  = reordered'
  reordered' = fmap reverse $ reverse coords

newtype PrecomputedRays = PrecomputedRays (VU.Vector Int)
  deriving ( Show )

precomputedRays :: IORef (M.Map Coords2D PrecomputedRays)
precomputedRays = unsafePerformIO $ newIORef M.empty
{-# NOINLINE precomputedRays #-}

getCachedPrecomputedRays :: Coords2D -> PrecomputedRays
getCachedPrecomputedRays size =
  unsafePerformIO
    $ atomicModifyIORef' precomputedRays
    $ \old -> case M.lookup size old of
        Nothing ->
          let !precomputed = computeRaycastViews size
          in  (M.insert size precomputed old, precomputed)
        Just precomputed -> (old, precomputed)

-- | Computes the rays that would be shot for `renderRaycastingView`.
--
-- This is not exposed outside this module; the pre-rendered rays are cached
-- behind the scenes in a global MVar .
computeRaycastViews :: Coords2D -> PrecomputedRays
computeRaycastViews (Coords2D tw th) =
  packCoordinates
    $ snd
    $ flip execState (initial_uncovered_coords, [] :: [[Coords2D]])
    $ do
    -- Cast ray to all top and bottom parts
        for_ [0 .. (tw `div` 2)] $ \target_x -> do
          shootRay (Coords2D target_x 0)
          shootRay (Coords2D target_x (th - 1))
        for_ [tw - 1, tw - 2 .. (tw `div` 2 + 1)] $ \target_x -> do
          shootRay (Coords2D target_x 0)
          shootRay (Coords2D target_x (th - 1))
        -- Cast ray to all left and right parts
        for_ [1 .. (th `div` 2)] $ \target_y -> do
          shootRay (Coords2D 0 target_y)
          shootRay (Coords2D (tw - 1) target_y)
        for_ [th - 2, th - 3 .. (th `div` 2 + 1)] $ \target_y -> do
          shootRay (Coords2D 0 target_y)
          shootRay (Coords2D (tw - 1) target_y)

        fillGaps
 where
  initial_uncovered_coords :: S.Set Coords2D
  initial_uncovered_coords =
    S.fromList
      $ [ Coords2D x y
        | x <- [0 .. tw - 1]
        , y <- [0 .. th - 1]
        , x /= center_x || y /= center_y
        ]

  Coords2D !center_x !center_y = Coords2D (tw `div` 2) (th `div` 2)
  center_x05 :: Double
  center_x05 = fromIntegral center_x + 0.5
  center_y05 :: Double
  center_y05 = fromIntegral center_y + 0.5

  shootRay tgt = do
    old_st <- get
    shootRay' tgt
    new_st <- get
    when (fst old_st == fst new_st) $ put old_st

  shootRay' (Coords2D target_x target_y) = do
    let !target_x05 = fromIntegral target_x + 0.5
        !target_y05 = fromIntegral target_y + 0.5
    modify $ \(set, lst) -> (set, [] : lst)
    bresenham center_x center_y target_x target_y
      $ \x y -> unless (x == center_x && y == center_y) $ do
          let !dist = distanceToLine (fromIntegral x + 0.5 :: Double)
                                     (fromIntegral y + 0.5)
                                     center_x05
                                     center_y05
                                     target_x05
                                     target_y05
              !remove_it = dist <= 0.27001
          modify $ \(set, (front : rest)) ->
            ( (if remove_it then S.delete (Coords2D x y) set else set)
            , ((Coords2D x y) : front) : rest
            )

  manhattan x y = abs x + abs y

  fillGaps = do
    uncovered_coordinates <- get
    let remaining_coords = S.toList $ fst uncovered_coordinates
        remaining_coords_sorted =
          flip sortBy remaining_coords $ \(Coords2D x1 y1) (Coords2D x2 y2) ->
          -- Visit order (items first in list take precedence):
          -- 1. Which quadrant is it? Top-left, top-right, bottom-left, bottom-right
          -- 2. If quadrant is anything else but top-left, then we apply
          -- transformation to coordinates.
          -- 3. Furthest away in terms of manhattan distance from center.
          -- 4. Maximum x
          -- 5. Maximum y
            let quadrant x y = if
                  | x <= tw `div` 2 && y <= th `div` 2 -> TopLeft
                  | x > tw `div` 2 && y <= th `div` 2  -> TopRight
                  | x <= tw `div` 2 && y > th `div` 2  -> BottomLeft
                  | x > tw `div` 2 && y > th `div` 2   -> BottomRight
                  | otherwise -> error "impossible quadrant!"
                !quadrant1    = quadrant x1 y1
                !quadrant2    = quadrant x2 y2

                (x1', y1')    = transformQuadrant quadrant1 x1 y1
                (x2', y2')    = transformQuadrant quadrant2 x2 y2

                quadrant_cmp  = quadrant1 `compare` quadrant2
                manhattan_cmp = manhattan x2' y2' `compare` manhattan x1' y1'
                x_cmp         = x2' `compare` x1'
                y_cmp         = y2' `compare` y1'
            in  if
                  | quadrant_cmp /= EQ  -> quadrant_cmp
                  | manhattan_cmp /= EQ -> manhattan_cmp
                  | x_cmp /= EQ         -> x_cmp
                  | otherwise           -> y_cmp

    for_ remaining_coords_sorted $ \tgt -> shootRay tgt

-- | Renders a level using a raycasting algorithm with sphere obstacles.
{-# INLINE renderRaycastingView #-}
renderRaycastingView
  :: forall m l liftedl tile view displaytile coords memorycoords
   . ( Monad m
     , TiledRenderer m displaytile
     , Obstacle tile
     , LevelLike l coords tile
     , TileToRenderedTile tile displaytile
     , EntityView view coords displaytile
     , TiledCoordMoving coords
     , Context coords ~ liftedl
     , LiftLevel l liftedl
     , TileMemorizer l memorycoords tile
     , LiftLevel coords memorycoords
     )
  => l
  -> RelativeRenderView coords
  -> view
  -> m (RenderedCoordinates, l)
renderRaycastingView !world !view !entityview =
  fmap (\(SPair !is !world) -> (RenderedCoordinates is, world))
    $ flip execStateT (SPair IS.empty world)
    $ do
        SPair _ !world <- get
        let !tile = tileAt world (levelStartCoords view)
        drawTile center_x center_y (levelStartCoords view) tile
        go 0
 where
  PrecomputedRays !raw_rays = getCachedPrecomputedRays
    (Coords2D (disp_right - disp_left + 1) (disp_bottom - disp_top + 1))

  -- Figure out widths and heights and where the center of the specified view
  -- is on the display.
  Coords2D !disp_left !disp_top = relativeTopLeft view
  Coords2D !disp_right !disp_bottom =
    relativeTopLeft view .+ relativeViewWidthHeight view .- Coords2D (-1) (-1)
  Coords2D !center_x !center_y = Coords2D
    (((disp_right - disp_left + 1) `div` 2) + disp_left)
    (((disp_bottom - disp_top + 1) `div` 2) + disp_top)

  center_x05 :: Double
  center_x05 = fromIntegral center_x + 0.5
  center_y05 :: Double
  center_y05 = fromIntegral center_y + 0.5

  go !idx | idx < VU.length raw_rays = do
    let !len_ray = raw_rays VU.! idx
        !target_x05 =
          fromIntegral (raw_rays VU.! (idx + 1 + (len_ray - 1) * 2))
            + 0.5
            + fromIntegral disp_left
        !target_y05 =
          fromIntegral (raw_rays VU.! (idx + 1 + (len_ray - 1) * 2 + 1))
            + 0.5
            + fromIntegral disp_top
    void
      $ runExceptT
      $ flip evalStateT
             (RayCastState (levelStartCoords view) center_x center_y 10)
      $ for_ [0 .. len_ray - 1]
      $ \ray_idx -> do
          let !dx' =
                fromIntegral (raw_rays VU.! (idx + 1 + ray_idx * 2)) + disp_left
          let
            !dy' =
              fromIntegral (raw_rays VU.! (idx + 1 + ray_idx * 2 + 1))
                + disp_top

          RayCastState !prev_coord !prev_x !prev_y !penetration <- get
          let
            !mover =
              fromMaybe
                  (error $ "impossible: " <> show
                    (prev_x, prev_y, dx', dy', center_x, center_y)
                  )
                $ movingByLocations (Coords2D prev_x prev_y) (Coords2D dx' dy')
            !coords = mover (liftLevel world) prev_coord
          let !tile = tileAt world coords
              !dist = distanceToLine (fromIntegral dx' + 0.5 :: Double)
                                     (fromIntegral dy' + 0.5)
                                     center_x05
                                     center_y05
                                     target_x05
                                     target_y05
              !new_penetration = penetration - isObstacle tile
          put $ RayCastState coords dx' dy' new_penetration
          -- 0.27 came from trial and error, this value should be between 0.0
          -- and 0.5 or the fov algorithm becomes unsound.
          lift $ lift $ when (dist <= 0.27001) $ drawTile dx' dy' coords tile
          when
              (  new_penetration
              <= 0
              || dx'
              >  disp_right
              || dx'
              <  disp_left
              || dy'
              >  disp_bottom
              || dy'
              <  disp_top
              )
            $
              -- Does the ray intersect a tile sphere?
              when (dist <= 0.50001)
            $ lift
            $ throwE ()

    go (idx + 1 + len_ray * 2)

  go !_ = return ()

  {-# INLINE drawTile #-}
  drawTile
    :: Int16 -> Int16 -> coords -> tile -> StateT (SPair IS.IntSet l) m ()
  drawTile !dispx !dispy !coords !tile = do
    modify $ \(SPair !paircoords world) -> SPair
      (IS.insert (coords2DToInt $ Coords2D dispx dispy) paircoords)
      (memorizeTile world (liftLevel coords :: memorycoords) tile)
    lift $ do
      case viewEntity coords entityview of
        Nothing ->
          setTile (Coords2D dispx dispy) (toRenderedTile tile Visible)
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
          return $ if isObstacle tile_at_loc1 > 0 then cor else mini
        else return mini

      !new_maxi <- if maxi > cor
        then do
          lift $ drawTile (xmirror (x' - 1) + center_x)
                          (ymirror (y' + 1) + center_y)
                          loc2
                          Visible
          return $ if isObstacle tile_at_loc2 > 0 then cor else maxi
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

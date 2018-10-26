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
  ( renderLevel
  , renderBeamcastFOV
  , EntityView(..)
  , TiledRenderer(..)
  , TileToRenderedTile(..)
  , Coords2DRenderView(..)
  , coords2DRenderView
  , RelativeRenderView(..)
  , relativeRenderView
  , Obstacle(..)
  )
where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Primitive
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State.Strict
import           Data.Data
import           Data.Int
import           Data.Foldable
import           Data.Maybe
import           Data.Ratio
import qualified Data.Vector                   as V
import qualified Data.Vector.Unboxed           as VU
import qualified Data.Vector.Mutable           as VM
import           GHC.Generics
import           System.IO

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

instance EntityView () entitycoords entitydisplaytile where
  viewEntity _ _ = Nothing

-- | The trivial entity viewer that does not render anything

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

data RelativeRenderView coords = RelativeRenderView
  { levelStartCoords :: !coords
  , relativeTopLeft :: !Coords2D
  , relativeViewWidthHeight :: !Coords2D }
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Functor, Foldable, Traversable )

relativeRenderView
  :: coords -> Coords2D -> Coords2D -> RelativeRenderView coords
relativeRenderView = RelativeRenderView

class Obstacle tile where
  isObstacle :: tile -> Bool

-- | Render a level with permissive field of view algorithm.
renderBeamcastFOV
  :: forall m l liftedl tile view displaytile coords
   . ( Monad m
     , MonadIO m
     , TiledRenderer m displaytile
     , Obstacle tile
     , LevelLike l coords tile
     , TileToRenderedTile tile displaytile
     , EntityView view coords displaytile
     , TiledCoordMoving coords
     , Context coords ~ liftedl
     , LiftLevel l liftedl
     , Show coords
     )
  => l
  -> RelativeRenderView coords
  -> view
  -> Int
  -> m ()
renderBeamcastFOV level view entityview num_beams = do
  -- Render the center tile
  drawTile center_x center_x (levelStartCoords view) Visible

  -- Render all the quadrants
  renderQuadrant False False
  renderQuadrant True  False
  renderQuadrant False True
  renderQuadrant True  True
 where
  Coords2D !disp_w    !disp_h   = relativeViewWidthHeight view
  Coords2D !disp_left !disp_top = relativeTopLeft view
  Coords2D !disp_right !disp_bottom =
    relativeTopLeft view .+ relativeViewWidthHeight view .- (Coords2D (-1) (-1))
  Coords2D !center_x !center_y = Coords2D ((disp_left + disp_right) `div` 2)
                                          ((disp_top + disp_bottom) `div` 2)

  renderQuadrant xmirror' ymirror' = forSlopes_ $ \slope_idx -> do
    go slope_idx slope_idx 1 0 (num_beams - 1) (levelStartCoords view) 0 0
   where
    xmirror :: Num a => a -> a
    xmirror v = if xmirror' then negate v else v
    ymirror :: Num a => a -> a
    ymirror v = if ymirror' then negate v else v

    movingByLocations' c1@(Coords2D x1 y1) (Coords2D x2 y2) =
      let dx = xmirror $ x2 - x1
          dy = ymirror $ y2 - y1
      in  movingByLocations c1 (Coords2D (x1 + dx) (y1 + dy))

    go !_ !_ !_ !mini !maxi !_ !_ !_ | mini > maxi = return ()
    go !_ !_ !_ !_ !_ !_ !prev_x !prev_y
      | prev_x > (disp_w `div` 2) + 1 || prev_y > (disp_h `div` 2) + 1
      = return ()
    go !slope_idx !v !u !mini !maxi !prev1_coords !prev1_x !prev1_y = do
      let
        !y'' = v `div` num_beams
        !x'' = u - y
        !y   = y''
        !x   = x''
        !y'  = fromIntegral y
        !x'  = fromIntegral x
        !cor = num_beams - (v `mod` num_beams)

        loc1_by_base :: Maybe (coords, Context coords -> coords -> coords)
        !loc1_by_base =
          (   (,) prev1_coords
          <$> movingByLocations' (Coords2D prev1_x prev1_y) (Coords2D x' y')
          )

        !loc2_by_base =
          ((,) prev1_coords <$> movingByLocations'
            (Coords2D prev1_x prev1_y)
            (Coords2D (x' - 1) (y' + 1))
          )

      when (isNothing loc1_by_base && isNothing loc2_by_base) $ error $ show
        (prev1_x, prev1_y, x', y', xmirror', ymirror')

      let
        loc1 :: coords
        loc1 =
          let Just (base, mover) = loc1_by_base <|> Just
                ( loc2
                , fromJust $ movingByLocations' (Coords2D (x' - 1) (y' + 1))
                                                (Coords2D x' y')
                )
          in  mover (liftLevel level) base

        loc2 :: coords
        loc2 =
          let Just (base, mover) = loc2_by_base <|> Just
                ( loc1
                , fromJust $ movingByLocations' (Coords2D x' y')
                                                (Coords2D (x' - 1) (y' + 1))
                )
          in  mover (liftLevel level) base

        !tile_at_loc1 = tileAt level loc1
        !tile_at_loc2 = tileAt level loc2

      !new_mini <- if mini < cor
        then do
          drawTile (xmirror x' + center_x) (ymirror y' + center_y) loc1 Visible
          return $ if isObstacle tile_at_loc1 then cor else mini
        else return mini

      !new_maxi <- if maxi > cor
        then do
          drawTile (xmirror (x' - 1) + center_x)
                   (ymirror (y' + 1) + center_y)
                   loc2
                   Visible
          return $ if isObstacle tile_at_loc2 then cor else maxi
        else return maxi

      go slope_idx (v + slope_idx) (u + 1) new_mini new_maxi loc1 x' y'

  {-# INLINE forSlopes_ #-}
  forSlopes_ action = for_ [1 .. num_beams - 1] action

  drawTile !dx !dy !coords !visibility = do
    case viewEntity coords entityview of
      Nothing -> setTile (Coords2D dx dy)
                         (toRenderedTile (tileAt level coords) visibility)
      Just tile -> setTile (Coords2D dx dy) tile

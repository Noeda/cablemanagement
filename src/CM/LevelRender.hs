{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}

-- | This module is all about rendering levels on a screen.
--

module CM.LevelRender
  ( renderLevel
  , EntityView(..)
  , TiledRenderer(..)
  , TileToRenderedTile(..)
  , Coords2DRenderView(..)
  , coords2DRenderView
  )
where

import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Strict
import           Data.Data
import           Data.Foldable
import           GHC.Generics

import           CM.Coords
import           CM.LevelLike


-- | Implement this class for outputs that draw some kind of discretely spaced
-- tiles.
--
-- This is very similar to `CM.TextIO.TextIO` except more general; this one
-- doesn't assume character display.
class TiledRenderer m displaytile | m -> displaytile where
  displaySize :: m Coords2D
  setTile :: Coords2D -> displaytile -> m ()

-- | How to convert a tile into a rendered tile for display.
class TileToRenderedTile tile displaytile where
  toRenderedTile :: tile -> displaytile

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
  viewEntity :: entitycoords -> view -> (Maybe entitydisplaytile, view)

instance EntityView () entitycoords entitydisplaytile where
  viewEntity _ _ = (Nothing, ())

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
  flip evalStateT entityview
    $ for_ [ly .. (ly + h - 1)]
    $ \(!y) -> for_ [lx .. (lx + w - 1)] $ \(!x) -> do
        let !dx = x - lx + dox
            !dy = y - ly + doy
        when (dx >= 0 && dx < tw && dy >= 0 && dy < th) $ do
          let !src_coords  = Coords2D x y
              !tile        = tileAt level src_coords
              !displaytile = toRenderedTile tile
          ent_state <- get
          let (tile, !new_ent_state) = viewEntity src_coords ent_state
          put new_ent_state
          lift $ case tile of
            Nothing   -> setTile (Coords2D dx dy) displaytile
            Just tile -> setTile (Coords2D dx dy) tile
 where
  Coords2D !dox !doy = topLeftOnDisplay renderview
  Coords2D !lx  !ly  = topLeftOnLevel renderview
  Coords2D !w   !h   = widthHeight renderview

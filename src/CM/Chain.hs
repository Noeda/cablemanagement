-- | This module implements THE CHAIN.
--
-- A chain is a long piece of chain on a level or world. You can pull a chain.
--
-- Chain can go around obstacles, it can be wrapped around things, you can
-- retract it, nudge it etc.
--
-- It's rope physics for roguelikes!
--
-- This module has been implemented to work completely with relative
-- coordinates so the chain should work nicely with a portal world and go
-- through portals.
--

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}

module CM.Chain
  ( Chain()
  , PullChainResult(..)
  , chainToList
  , chainLength
  , singlePartChain
  , layNewChain
  , pullChain
  )
where

import           Data.Data
import           Data.Foldable
import           Data.Maybe
import           Data.Monoid
import qualified Data.Sequence                 as SQ
import           Data.Sequence                  ( ViewL(..)
                                                , ViewR(..)
                                                )
import           GHC.Generics

import           CM.Coords

newtype Chain coords = Chain (SQ.Seq coords)
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Functor, Foldable, Traversable )

-- | Lists all coordinates the chain has, starting from source point.
{-# INLINE chainToList #-}
chainToList :: Chain coords -> [coords]
chainToList (Chain chainseq) = toList chainseq

findCandidateDirection
  :: (Eq coords, TiledCoordMoving coords)
  => coords
  -> coords
  -> Context coords
  -> Maybe (Context coords -> coords -> coords)
findCandidateDirection !target !src !context = if
  | target == toRight context src     -> Just toRight
  | target == toLeft context src      -> Just toLeft
  | target == toUp context src        -> Just toUp
  | target == toDown context src      -> Just toDown
  | target == toRightDown context src -> Just toRightDown
  | target == toLeftDown context src  -> Just toLeftDown
  | target == toRightUp context src   -> Just toRightUp
  | target == toLeftUp context src    -> Just toLeftUp
  | otherwise                         -> Nothing

{-# INLINE chainLength #-}
chainLength :: Chain coords -> Int
chainLength (Chain chainseq) = SQ.length chainseq

singlePartChain :: coords -> Chain coords
singlePartChain = Chain . SQ.singleton

-- | Lays new chain from source. The chain is pulled to given coordinate,
-- which must be close the current end of the chain.
{-# INLINABLE layNewChain #-}
layNewChain
  :: (Eq coords, TiledCoordMoving coords)
  => Chain coords
  -> Context coords
  -> coords
  -> Maybe (Chain coords)
layNewChain (Chain !chainseq) !context !pull_position =
  if isJust (findCandidateDirection pull_position chain_end context)
       || pull_position
       == chain_end
    then Just $ Chain $ chainseq <> SQ.singleton pull_position
    else Nothing
  where _ :> (!chain_end) = SQ.viewr chainseq

data PullChainResult coords
  = CableFullyRetracted
  | Pulled !(Chain coords)
  | Unpullable !(Chain coords)
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Functor, Foldable, Traversable )

-- | Pulls cable from some position.
--
-- The cable may not be pullable (maybe it's already at highest tension). In
-- this case, `Nothing` is returned.
{-# INLINE pullChain #-}
pullChain
  :: (Eq coords, TiledCoordMoving coords)
  => Chain coords
  -> Context coords
  -> PullChainResult coords
pullChain chain@(Chain !chainseq) !context = if SQ.length chainseq <= 1
  then CableFullyRetracted
  else case removeChainPiece chain context of
    Nothing     -> Unpullable chain
    Just pulled -> Pulled pulled

{-# INLINABLE removeChainPiece #-}
removeChainPiece
  :: (Eq coords, TiledCoordMoving coords)
  => Chain coords
  -> Context coords
  -> Maybe (Chain coords)
removeChainPiece (Chain !chainseq) !context =
  let
    !removed_chain      = SQ.drop 1 chainseq
    (!chain_start) :< _ = SQ.viewl chainseq
    repositioned_chain  = SQ.update 0 chain_start removed_chain
    connected           = isJust
      (findCandidateDirection chain_start
                              (SQ.index repositioned_chain 0)
                              context
      )
  in
    if SQ.null repositioned_chain || connected
      then Just $ Chain repositioned_chain
      else case removeChainPiece (Chain removed_chain) context of
        Nothing -> Nothing
        Just (Chain removed) ->
          Just $ Chain $ SQ.singleton (SQ.index repositioned_chain 0) <> removed

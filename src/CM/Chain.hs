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
  , chainEnd
  , chainToList
  , chainLength
  , singlePartChain
  , layNewChain
  , layNewChainRestrict
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

chainEnd :: Chain coords -> coords
chainEnd (Chain chainseq) = SQ.index chainseq (SQ.length chainseq - 1)

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

-- | Same as `layNewChain` but puts a limit on the chain length.
--
-- If the chain length would be exceeded, this will try pull the chain. If the
-- chain cannot be pulled, then this returns `Nothing`.
layNewChainRestrict
  :: (Eq coords, TiledCoordMoving coords)
  => Chain coords
  -> Context coords
  -> Int
  -> coords
  -> (coords -> Bool)
  -> Maybe (Chain coords)
layNewChainRestrict chain@(Chain !chainseq) !context !max_len !pull_position !is_permissible
  = case ch of
    Nothing               -> Nothing
    Just (Chain ch_chain) -> if SQ.length chainseq < max_len
      then ch
      else
        case pullChain (Chain $ SQ.reverse ch_chain) context is_permissible of
          Pulled (Chain new_chain) ->
            if SQ.index new_chain (SQ.length new_chain - 1)
                 /= SQ.index chainseq 0
                 || SQ.index new_chain 0
                 /= pull_position
              then Nothing
              else Just $ Chain $ SQ.reverse new_chain
          _ -> Nothing
  where ch = layNewChain chain context pull_position

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
  -> (coords -> Bool)          -- ^ This is used to test permissible places where the chain is allowed to be.
  -> PullChainResult coords
pullChain chain@(Chain !chainseq) !context !is_permissible =
  if SQ.length chainseq <= 1
    then CableFullyRetracted
    else case removeChainPiece chain context is_permissible of
      Nothing     -> Unpullable chain
      Just pulled -> Pulled pulled

{-# INLINABLE removeChainPiece #-}
removeChainPiece
  :: (Eq coords, TiledCoordMoving coords)
  => Chain coords
  -> Context coords
  -> (coords -> Bool)
  -> Maybe (Chain coords)
removeChainPiece (Chain !chainseq) !context !is_permissible = Chain
  <$> go (SQ.update 0 (SQ.index chainseq 0) (SQ.drop 1 chainseq))
 where
  go rest | SQ.length rest <= 1 = return rest
  go rest
    = let
        !first  = SQ.index rest 0
        !second = SQ.index rest 1
      in
        if isJust (findCandidateDirection first second context)
           || first
           == second
        then
          return rest
        else
          let candidates_second =
                [ toLeft context second
                , toRight context second
                , toUp context second
                , toDown context second
                , toLeftUp context second
                , toRightUp context second
                , toLeftDown context second
                , toRightDown context second
                ]
          in
            case
              find
                (\second_cand -> is_permissible second_cand
                  && isJust (findCandidateDirection first second_cand context)
                )
                candidates_second
            of
              Nothing  -> Nothing
              Just dir -> (SQ.singleton first <>)
                <$> go (SQ.update 0 dir $ SQ.drop 1 rest)

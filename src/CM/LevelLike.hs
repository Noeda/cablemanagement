{-# LANGUAGE FunctionalDependencies #-}

module CM.LevelLike
  ( LevelLike(..)
  )
where

class LevelLike l coords block | l -> coords block where
  tileAt       :: l -> coords -> block
  fromPairList :: [(coords, block)] -> l
  empty        :: l

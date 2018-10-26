{-# LANGUAGE MultiParamTypeClasses #-}

module CM.LiftLevel
  ( LiftLevel(..)
  )
where

class LiftLevel l l' where
  liftLevel :: l -> l'

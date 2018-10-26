{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module CM.LiftLevel
  ( LiftLevel(..)
  )
where

class LiftLevel l l' where
  liftLevel :: l -> l'

instance LiftLevel l l where
  liftLevel = id

instance LiftLevel l () where
  liftLevel _ = ()

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module CM.KeyInput
  ( Key(..)
  , KeyInteractiveIO(..)
  , charToKey
  )
where

import           Data.Data
import           GHC.Generics

data Key
  = KeyA
  | KeyB
  | KeyC
  | KeyD
  | KeyE
  | KeyF
  | KeyG
  | KeyH
  | KeyI
  | KeyJ
  | KeyK
  | KeyL
  | KeyM
  | KeyN
  | KeyO
  | KeyP
  | KeyQ
  | KeyR
  | KeyS
  | KeyT
  | KeyU
  | KeyV
  | KeyW
  | KeyX
  | KeyY
  | KeyZ
  | KeyArrowLeft
  | KeyArrowUp
  | KeyArrowRight
  | KeyArrowDown
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic, Enum )

-- | Convenience function to turn a char into a key, if there is an obvious
-- corresponding key.
charToKey :: Char -> Maybe Key
charToKey 'a' = Just KeyA
charToKey 'b' = Just KeyB
charToKey 'c' = Just KeyC
charToKey 'd' = Just KeyD
charToKey 'e' = Just KeyE
charToKey 'f' = Just KeyF
charToKey 'g' = Just KeyG
charToKey 'h' = Just KeyH
charToKey 'i' = Just KeyI
charToKey 'j' = Just KeyJ
charToKey 'k' = Just KeyK
charToKey 'l' = Just KeyL
charToKey 'm' = Just KeyM
charToKey 'n' = Just KeyN
charToKey 'o' = Just KeyO
charToKey 'p' = Just KeyP
charToKey 'q' = Just KeyQ
charToKey 'r' = Just KeyR
charToKey 's' = Just KeyS
charToKey 't' = Just KeyT
charToKey 'u' = Just KeyU
charToKey 'v' = Just KeyV
charToKey 'w' = Just KeyW
charToKey 'x' = Just KeyX
charToKey 'y' = Just KeyY
charToKey 'z' = Just KeyZ
charToKey 'A' = Just KeyA
charToKey 'B' = Just KeyB
charToKey 'C' = Just KeyC
charToKey 'D' = Just KeyD
charToKey 'E' = Just KeyE
charToKey 'F' = Just KeyF
charToKey 'G' = Just KeyG
charToKey 'H' = Just KeyH
charToKey 'I' = Just KeyI
charToKey 'J' = Just KeyJ
charToKey 'K' = Just KeyK
charToKey 'L' = Just KeyL
charToKey 'M' = Just KeyM
charToKey 'N' = Just KeyN
charToKey 'O' = Just KeyO
charToKey 'P' = Just KeyP
charToKey 'Q' = Just KeyQ
charToKey 'R' = Just KeyR
charToKey 'S' = Just KeyS
charToKey 'T' = Just KeyT
charToKey 'U' = Just KeyU
charToKey 'V' = Just KeyV
charToKey 'W' = Just KeyW
charToKey 'X' = Just KeyX
charToKey 'Y' = Just KeyY
charToKey 'Z' = Just KeyZ
charToKey _   = Nothing

class KeyInteractiveIO m where
  waitForKey :: m Key

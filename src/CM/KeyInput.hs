{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module CM.KeyInput
  ( Key(..)
  , KeyInteractiveIO(..)
  , charToKey
#ifdef GHCJS
  , inputKey
#endif
  )
where

import           Data.Data
import           GHC.Generics
#ifdef GHCJS
import System.IO.Unsafe
import Control.Concurrent.STM
import Control.Monad.IO.Class
#endif

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
  | Key0
  | Key1
  | Key2
  | Key3
  | Key4
  | Key5
  | Key6
  | Key7
  | Key8
  | Key9
  | KeyArrowLeft
  | KeyArrowUp
  | KeyArrowRight
  | KeyArrowDown
  | MouseClick !Double !Double
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )

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
charToKey '0' = Just Key0
charToKey '1' = Just Key1
charToKey '2' = Just Key2
charToKey '3' = Just Key3
charToKey '4' = Just Key4
charToKey '5' = Just Key5
charToKey '6' = Just Key6
charToKey '7' = Just Key7
charToKey '8' = Just Key8
charToKey '9' = Just Key9
charToKey _   = Nothing

class KeyInteractiveIO m where
  waitForKey :: m Key

#ifdef GHCJS
inputKey :: TVar (Maybe Key)
inputKey = unsafePerformIO $ newTVarIO Nothing
{-# NOINLINE inputKey #-}

getInputChar :: MonadIO m => m Key
getInputChar = liftIO $ atomically $ readTVar inputKey >>= \case
  Nothing -> retry
  Just ch -> do
    writeTVar inputKey Nothing
    return ch

instance KeyInteractiveIO IO where
  waitForKey = liftIO getInputChar
#else
instance KeyInteractiveIO IO where
  waitForKey = go
   where
    go = do
      ch <- getChar
      case charToKey ch of
        Nothing -> go
        Just key -> return key
#endif

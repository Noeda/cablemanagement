{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module CM.TextIO
  ( TextIO(..)
  , Attributes(..)
  , Color3(..)
  , colorsToAttributes
  , attributesToColors
  , module CM.Coords
  )
where

import           Data.Bits
import           Data.Data
import           Data.Word
import           GHC.Generics

import           CM.Coords
import           CM.LevelRender

class TextIO m where
  terminalSize :: m Coords2D
  setChar :: Attributes -> Char -> Coords2D -> m ()
  flush :: m ()

instance TextIO m => TiledRenderer m (Attributes, Char) where
  displaySize = terminalSize

  {-# INLINE setTile #-}
  setTile coords (atts, ch) = setChar atts ch coords

-- | This describes the foreground and background color of some cell in
-- terminal.
--
-- Only 36 bits are used; each color is represented with 6 bits.
newtype Attributes = Attributes Word64
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )

-- | Describes a color. Only 6 bits are used so the RGB values range between 0
-- and 63.
data Color3 = Color3 !Word8 !Word8 !Word8
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )

-- | Unpacks attributes to a color.
--
-- This is the reverse of `colorsToAttributes`.
attributesToColors :: Attributes -> (Color3, Color3)
attributesToColors (Attributes !value) =
  ( Color3 (fromIntegral $ (value .&. 0xfc0000000) `shiftR` 30)
           (fromIntegral $ (value .&. 0x3f000000) `shiftR` 24)
           (fromIntegral $ (value .&. 0xfc0000) `shiftR` 18)
  , Color3 (fromIntegral $ (value .&. 0x3f000) `shiftR` 12)
           (fromIntegral $ (value .&. 0xfc0) `shiftR` 6)
           (fromIntegral (value .&. 0x3f))
  )
{-# INLINE attributesToColors #-}

-- | Given a foreground and background colors, packs them into `Attributes`.
colorsToAttributes :: Color3 -> Color3 -> Attributes
colorsToAttributes (Color3 !fr !fg !fb) (Color3 !br !bg !bb) =
  Attributes
    $   (fromIntegral (fr .&. 0x3f) `shiftL` 30)
    .|. (fromIntegral (fg .&. 0x3f) `shiftL` 24)
    .|. (fromIntegral (fb .&. 0x3f) `shiftL` 18)
    .|. (fromIntegral (br .&. 0x3f) `shiftL` 12)
    .|. (fromIntegral (bg .&. 0x3f) `shiftL` 6)
    .|. fromIntegral (bb .&. 0x3f)
{-# INLINE colorsToAttributes #-}

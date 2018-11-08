{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module CM.TextIO
  ( TextIO(..)
  , Attributes(..)
  , Color3(..)
  , colorsToAttributes
  , attributesToColors
  , AttributedText()
  , fromText
  , setText
  , atts
  , module CM.Coords
  )
where

import           Control.Monad
import           Data.Bits
import           Data.Data
import           Data.Foldable
import           Data.Semigroup
import           Data.String
import qualified Data.Text                     as T
import           Data.Word
import           GHC.Generics

import           CM.Coords

class TextIO m where
  terminalSize :: m Coords2D
  setChar :: Attributes -> Char -> Coords2D -> m ()
  clear :: m ()
  flush :: m ()

-- | A datatype that's used with `putText`.
--
-- `AttributedText` implements `FromString` so you can use normal string
-- literals to build it.
data AttributedText
  = PlainText !T.Text
  | SetAttributes !Attributes
  | Seq !AttributedText AttributedText
  deriving ( Eq, Ord, Show, Read, Typeable, Data, Generic )

instance IsString AttributedText where
  {-# INLINEABLE fromString #-}
  fromString str = PlainText $ T.pack str

{-# INLINE fromText #-}
fromText :: T.Text -> AttributedText
fromText = PlainText

{-# INLINABLE atts #-}
atts :: Color3 -> Color3 -> AttributedText
atts fg bg = SetAttributes $ colorsToAttributes fg bg

instance Semigroup AttributedText where
  {-# INLINE (<>) #-}
  m1 <> m2 = m1 `mappend` m2

instance Monoid AttributedText where
  {-# INLINE mempty #-}
  mempty = PlainText ""

  {-# INLINE mappend #-}
  PlainText "" `mappend` x = x
  x `mappend` PlainText "" = x
  PlainText x `mappend` PlainText y = PlainText (x `mappend` y)
  SetAttributes _ `mappend` snd@SetAttributes{} = snd
  x `mappend` y = Seq x y

-- | Puts some text on a `TextIO`.
--
-- The text will not word-wrap, so it'll all be on one line.
setText :: forall m . (Monad m, TextIO m) => AttributedText -> Coords2D -> m ()
setText txt coords = void
  $ go (colorsToAttributes (Color3 63 63 63) (Color3 0 0 0)) txt coords
 where
  go :: Attributes -> AttributedText -> Coords2D -> m (Attributes, Coords2D)
  go !attributes txt !coords = case txt of
    PlainText str -> do
      !new_coords <- goText str attributes coords
      return (attributes, new_coords)
    Seq s1 s2 -> do
      (!atts2, coords2) <- go attributes s1 coords
      go atts2 s2 coords2
    SetAttributes !atts -> return (atts, coords)

  goText :: T.Text -> Attributes -> Coords2D -> m Coords2D
  goText !txt !atts (Coords2D !x !y) = do
    for_ [0 .. T.length txt - 1] $ \idx ->
      setChar atts (txt `T.index` idx) (Coords2D (x + fromIntegral idx) y)
    return (Coords2D (x + fromIntegral (T.length txt)) y)

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

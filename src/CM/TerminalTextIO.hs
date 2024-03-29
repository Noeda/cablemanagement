{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}

module CM.TerminalTextIO
  ( withTerminalTextIO
  , withTerminalTextIOHandle
  , getRawTerminalSize
  , withRawTerminalSetup
  , TerminalTextIOHandle(..)
  , TerminalTextIOT()
  , TerminalTextIO
  , module CM.TextIO
  )
where

import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Strict
import qualified Data.Array.IO                 as IA
import qualified Data.Array.Unboxed            as UA
import qualified Data.Array.MArray             as M
import qualified Data.Array.Unsafe             as I
import           Data.Bits
import qualified Data.ByteString.Builder       as BB
import qualified Data.ByteString.Lazy          as BL
import           Data.Char
import           Data.Coerce
import           Data.Data
import           Data.Foldable
import           Data.Word
import           Foreign.Marshal.Alloc
import           Foreign.C.Types
import           Foreign.Ptr
import           Foreign.Storable
import           Foreign.ForeignPtr
import           GHC.Generics
import           System.Console.ANSI     hiding ( getTerminalSize )
import           System.IO

import           CM.Coords
import           CM.KeyInput
import           CM.LevelRender
import           CM.TextIO

foreign import ccall setup_terminal :: Ptr TermiosStruct -> IO ()
foreign import ccall restore_terminal :: Ptr TermiosStruct -> IO ()
foreign import ccall sizeof_termios :: CSize
foreign import ccall get_terminal_size :: Ptr CInt -> Ptr CInt -> IO ()

data TermiosStruct

-- Word64 in this data is marshalled to `Attributes` and back.
data TerminalState = TerminalState
  { displayContents :: !(IA.IOUArray (Word16, Word16) Word64)
  , flushContents :: !(IA.IOUArray (Word16, Word16) Word64)
  , seenTerminalSize :: !Coords2D
  , textHandle :: !TerminalTextIOHandle }

initialTerminalState :: MonadIO m => TerminalTextIOHandle -> m TerminalState
initialTerminalState tio_handle = do
  Coords2D w h <- getTerminalSize tio_handle
  flush_array  <- liftIO
    $ IA.newArray ((0, 0), (fromIntegral (w - 1), fromIntegral (h - 1))) 0
  display_array <- liftIO
    $ IA.newArray ((0, 0), (fromIntegral (w - 1), fromIntegral (h - 1))) 0
  return $ TerminalState
    { displayContents  = display_array
    , flushContents    = flush_array
    , seenTerminalSize = Coords2D w h
    , textHandle       = tio_handle
    }

newtype TerminalTextIOT m a = TerminalTextIOT (StateT TerminalState m a)
  deriving ( Functor, Applicative, Monad, MonadFix, MonadIO, MonadThrow, MonadCatch, MonadMask )

instance MonadTrans TerminalTextIOT where
  {-# INLINE lift #-}
  lift = TerminalTextIOT . lift

type TerminalTextIO = TerminalTextIOT IO

getTerminalSize :: MonadIO m => TerminalTextIOHandle -> m Coords2D
getTerminalSize tio_handle = liftIO $ handleGetsize tio_handle

instance MonadIO m => KeyInteractiveIO (TerminalTextIOT m) where
  waitForKey = liftIO waitForKey

instance MonadIO m => TiledRenderer (TerminalTextIOT m) (Attributes, Char) where
  displaySize = terminalSize

  {-# INLINE drawTile #-}
  drawTile coords (atts, ch) = setChar atts ch coords

  {-# INLINE flushTiles #-}
  flushTiles = flush

  {-# INLINE clearTiles #-}
  clearTiles = clear

instance MonadIO m => TextIO (TerminalTextIOT m) where
  terminalSize = getTerminalSize =<< (TerminalTextIOT $ textHandle <$> get)

  {-# INLINE setChar #-}
  setChar attributes ch (Coords2D !x !y) = TerminalTextIOT $ do
    st <- get
    let Coords2D !tw !th = seenTerminalSize st
    unless (x < 0 || y < 0 || x >= tw || y >= th) $
      liftIO $ M.writeArray
        (flushContents st)
        (fromIntegral x, fromIntegral y)
        (coerce attributes .|. (fromIntegral (ord ch .&. 0xfffffff) `shiftL` 36))

  clear = TerminalTextIOT $ do
    st <- get
    let Coords2D !tw !th = seenTerminalSize st
    liftIO $ for_ [(x, y) | x <- [0..tw-1], y <- [0..th-1]] $ \(x, y) ->
      M.writeArray (flushContents st) (fromIntegral x, fromIntegral y)
        (coerce (colorsToAttributes (Color3 0 0 0) (Color3 0 0 0)) .|.
         (fromIntegral (ord ' ' .&. 0xfffffff) `shiftL` 36))

  flush = TerminalTextIOT $ do
    st <- get
    display_size <- liftIO $ IA.getBounds (displayContents st)
    flush_size@(_, (fw, fh)) <- liftIO $ IA.getBounds (flushContents st)
    if flush_size /= display_size
      then do liftIO $ fullRefresh (flushContents st) (textHandle st)
              new_display_contents <- liftIO $ I.unsafeThaw =<< (M.freeze (flushContents st) :: IO (UA.UArray (Word16, Word16) Word64))
              modify $ \old -> old { displayContents = new_display_contents }
      else liftIO $ do
             partialRefresh (flushContents st) (displayContents st) (textHandle st)
             copyArray (flushContents st) (displayContents st)
    (Coords2D (fromIntegral -> tw) (fromIntegral -> th)) <- getTerminalSize (textHandle st)
    when (tw /= fw+1 || th /= fh+1) $ do
      new_flush_contents <- liftIO $ IA.newArray ((0, 0), (tw-1, th-1)) 0
      modify $ \old -> old { flushContents = new_flush_contents
                           , seenTerminalSize = Coords2D (fromIntegral tw) (fromIntegral th) }

copyArray
  :: (M.Ix i, M.MArray IA.IOUArray v IO)
  => IA.IOUArray i v
  -> IA.IOUArray i v
  -> IO ()
copyArray !src !dst = do
  ixes <- IA.getBounds src
  for_ (M.range ixes) $ \idx -> do
    v <- IA.readArray src idx
    IA.writeArray dst idx v

fullRefresh
  :: IA.IOUArray (Word16, Word16) Word64 -> TerminalTextIOHandle -> IO ()
fullRefresh !flush_contents !tio_handle = do
  flush_size <- IA.getBounds flush_contents
  for_ (M.range flush_size) $ \idx@(!x, !y) -> do
    writeOutputStr tio_handle
      $ setCursorPositionCode (fromIntegral y) (fromIntegral x)
    !w64 <- IA.readArray flush_contents idx
    let (Color3 !fr !fg !fb, Color3 !br !bg !bb) =
          attributesToColors (coerce w64)
        !ch = chr $ fromIntegral $ (w64 .&. 0xfffffff000000000) `shiftR` 36
    writeOutput tio_handle
      $  BB.toLazyByteString
      $  "\x1b[38;2;"
      <> BB.word8Dec (fr `shiftL` 2)
      <> ";"
      <> BB.word8Dec (fg `shiftL` 2)
      <> ";"
      <> BB.word8Dec (fb `shiftL` 2)
      <> "m"
      <> "\x1b[48;2;"
      <> BB.word8Dec (br `shiftL` 2)
      <> ";"
      <> BB.word8Dec (bg `shiftL` 2)
      <> ";"
      <> BB.word8Dec (bb `shiftL` 2)
      <> "m"
      <> BB.charUtf8 ch
  flushOutput tio_handle

partialRefresh
  :: IA.IOUArray (Word16, Word16) Word64
  -> IA.IOUArray (Word16, Word16) Word64
  -> TerminalTextIOHandle
  -> IO ()
partialRefresh !flush_contents !display_contents !tio_handle = do
  flush_size <- IA.getBounds flush_contents
  for_ (M.range flush_size) $ \idx@(!x, !y) -> do
    !flush_w64   <- IA.readArray flush_contents idx
    !display_w64 <- IA.readArray display_contents idx
    when (flush_w64 /= display_w64) $ do
      let (Color3 !fr !fg !fb, Color3 !br !bg !bb) =
            attributesToColors (coerce flush_w64)
          !ch =
            chr $ fromIntegral $ (flush_w64 .&. 0xfffffff000000000) `shiftR` 36
      writeOutputStr tio_handle
        $ setCursorPositionCode (fromIntegral y) (fromIntegral x)
      writeOutput tio_handle
        $  BB.toLazyByteString
        $  "\x1b[38;2;"
        <> BB.word8Dec (fr `shiftL` 2)
        <> ";"
        <> BB.word8Dec (fg `shiftL` 2)
        <> ";"
        <> BB.word8Dec (fb `shiftL` 2)
        <> "m"
        <> "\x1b[48;2;"
        <> BB.word8Dec (br `shiftL` 2)
        <> ";"
        <> BB.word8Dec (bg `shiftL` 2)
        <> ";"
        <> BB.word8Dec (bb `shiftL` 2)
        <> "m"
        <> BB.charUtf8 ch
  flushOutput tio_handle
{-# NOINLINE partialRefresh #-}

data TerminalTextIOHandle = TerminalTextIOHandle
  { writeOutput :: !(BL.ByteString -> IO ())
  , writeOutputStr :: !(String -> IO ())
  , flushOutput :: IO ()
  , handleGetsize :: !(IO Coords2D) }
  deriving ( Typeable, Generic )

getRawTerminalSize :: IO Coords2D
getRawTerminalSize = alloca $ \w_ptr -> alloca $ \h_ptr -> do
  get_terminal_size w_ptr h_ptr
  Coords2D <$> (fromIntegral <$> peek w_ptr) <*> (fromIntegral <$> peek h_ptr)

stdoutTerminalTextIOHandle :: TerminalTextIOHandle
stdoutTerminalTextIOHandle = TerminalTextIOHandle
  { writeOutput    = BL.hPutStr stdout
  , writeOutputStr = hPutStr stdout
  , flushOutput    = return ()
  , handleGetsize  = getRawTerminalSize
  }

withTerminalTextIOHandle
  :: (MonadMask m, MonadIO m)
  => TerminalTextIOHandle
  -> TerminalTextIOT m a
  -> m a
withTerminalTextIOHandle tiohandle (TerminalTextIOT action) = do
  term_state <- initialTerminalState tiohandle
  evalStateT action term_state

withRawTerminalSetup :: (MonadMask m, MonadIO m) => m a -> m a
withRawTerminalSetup action = mask $ \restore -> do
  liftIO hideCursor
  bytes_ptr <- liftIO $ mallocForeignPtrBytes $ fromIntegral sizeof_termios
  liftIO $ withForeignPtr bytes_ptr $ \termios_settings ->
    setup_terminal termios_settings
  let rollback_terminal = liftIO $ do
        setSGR [Reset]
        showCursor
        clearScreen
        withForeignPtr bytes_ptr
          $ \termios_settings -> restore_terminal termios_settings
  finally (restore action) rollback_terminal

withTerminalTextIO :: (MonadMask m, MonadIO m) => TerminalTextIOT m a -> m a
withTerminalTextIO (TerminalTextIOT action) = withRawTerminalSetup $ do
  term_state <- initialTerminalState stdoutTerminalTextIOHandle
  evalStateT action term_state

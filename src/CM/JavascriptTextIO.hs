{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

module CM.JavascriptTextIO
  ( withJavascriptTextIO
  , JavascriptTextIOT()
  , JavascriptTextIO
  )
where

import           Control.Concurrent.MVar
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import qualified Data.Array.IO                 as IA
import           Data.Bits
import           Data.Char
import           Data.Coerce
import           Data.Foldable
import           Data.String                    ( fromString )
import           Data.JSString
import qualified Data.Map.Strict               as M
import           Data.Maybe
import           Data.Traversable
import           Data.Word
import           GHCJS.Foreign.Callback
import           GHCJS.Types
import           System.IO.Unsafe               ( unsafePerformIO )

import           CM.Coords
import           CM.KeyInput
import           CM.TextIO

foreign import javascript "$r = document.createElement('span');" make_span_element :: IO JSVal
foreign import javascript "$r = document.createElement('br');" make_br_element :: IO JSVal
foreign import javascript "$r = document.createElement('div');" make_div_element :: IO JSVal
foreign import javascript "$1.appendChild($2)" append_child :: JSVal -> JSVal -> IO ()
foreign import javascript unsafe "$1.textContent = $2" set_text_content :: JSVal -> JSString -> IO ()
foreign import javascript unsafe "$1.style.cssText = $2" set_style :: JSVal -> JSString -> IO ()
foreign import javascript "$r = document.getElementById('content')" get_content_div :: IO JSVal
foreign import javascript "$1.addEventListener('keydown', $2);" attach_keydown_handler :: JSVal -> Callback (JSVal -> IO ()) -> IO ()
foreign import javascript "$1.removeEventListener('keydown', $2);" detach_keydown_handler :: JSVal -> Callback (JSVal -> IO ()) -> IO ()
foreign import javascript "$r = document.getElementsByTagName('body')[0];" get_body :: IO JSVal
foreign import javascript unsafe "$r = $1.key || String.fromCharCode($1.keyCode);" get_event_char :: JSVal -> IO JSString
--foreign import javascript "console.log($1);" console_log :: JSString -> IO ()
foreign import javascript "$r = 'color: rgb(' + String(($1) * 4) + ',' + String(($2) * 4) + ',' + String(($3) * 4) + ');background: rgb(' + String(($4) * 4) + ',' + String(($5) * 4) + ',' + String(($6) * 4) + ');';"
  to_javascript_colorstyle :: Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> IO JSString

globalSpans :: MVar (M.Map (Int, Int) JSVal)
globalSpans = unsafePerformIO $ newMVar M.empty
{-# NOINLINE globalSpans #-}

inputKey :: TVar (Maybe Key)
inputKey = unsafePerformIO $ newTVarIO Nothing
{-# NOINLINE inputKey #-}

type AttributeArray = IA.IOUArray (Word16, Word16) Word64

newtype JavascriptTextIOT m a = JavascriptTextIOT (ReaderT (AttributeArray, AttributeArray) m a)
  deriving ( Monad, Applicative, Functor )

instance MonadIO m => MonadIO (JavascriptTextIOT m) where
  {-# INLINE liftIO #-}
  liftIO = JavascriptTextIOT . liftIO

type JavascriptTextIO = JavascriptTextIOT IO

terminalW :: Int
terminalW = 80

terminalH :: Int
terminalH = 24

withJavascriptTextIO :: (MonadMask m, MonadIO m) => JavascriptTextIOT m a -> m a
withJavascriptTextIO (JavascriptTextIOT action) = mask $ \restore -> do
  -- Setup DOM stuff

  -- This div holds everything
  term_div <- liftIO make_div_element
  liftIO
    $ set_style term_div "font-family: 'Subbie', monospace; font-size: 1.3em;"

  arr <- liftIO $ IA.newArray
    ((0, 0), (fromIntegral $ terminalW - 1, fromIntegral $ terminalH - 1))
    0
  arr2 <- liftIO $ IA.newArray
    ((0, 0), (fromIntegral $ terminalW - 1, fromIntegral $ terminalH - 1))
    0

  spanner term_div (restore $ runReaderT action (arr, arr2))

spanner :: (MonadMask m, MonadIO m) => JSVal -> m a -> m a
spanner term_div action = do
  -- Spans that show each symbol
  spans <-
    liftIO
    $ fmap M.fromList
    $ for [ (x, y) | x <- [0 .. (terminalW - 1)], y <- [0 .. (terminalH - 1)] ]
    $ \(x, y) -> do
        span <- make_span_element
        set_text_content span "\x00a0"
        return ((x, y), span)

  let _ = spans :: M.Map (Int, Int) JSVal

  -- Attach the spans (and <br />s) to the div
  liftIO $ for_ [0 .. (terminalH - 1)] $ \y -> do
    for_ [0 .. (terminalW - 1)]
      $ \x -> append_child term_div (fromJust $ M.lookup (x, y) spans)
    append_child term_div =<< make_br_element

  content_div <- liftIO get_content_div
  liftIO $ append_child content_div term_div

  liftIO $ modifyMVar_ globalSpans $ \_ -> return spans

  callback <- liftIO $ asyncCallback1 $ \val -> do
    js_str <- get_event_char val
    case unpack js_str of
      [ch] -> case charToKey ch of
        Nothing  -> return ()
        Just key -> atomically $ readTVar inputKey >>= \case
          Nothing -> writeTVar inputKey (Just key)
          Just{}  -> retry
      "ArrowLeft" -> atomically $ readTVar inputKey >>= \case
        Nothing -> writeTVar inputKey (Just KeyArrowLeft)
        Just{}  -> retry
      "ArrowUp" -> atomically $ readTVar inputKey >>= \case
        Nothing -> writeTVar inputKey (Just KeyArrowUp)
        Just{}  -> retry
      "ArrowRight" -> atomically $ readTVar inputKey >>= \case
        Nothing -> writeTVar inputKey (Just KeyArrowRight)
        Just{}  -> retry
      "ArrowDown" -> atomically $ readTVar inputKey >>= \case
        Nothing -> writeTVar inputKey (Just KeyArrowDown)
        Just{}  -> retry
      _ -> return ()

  body <- liftIO get_body
  liftIO $ set_style body "background: #000;"
  liftIO $ attach_keydown_handler body callback

  finally action $ do
    liftIO $ detach_keydown_handler body callback
    liftIO $ releaseCallback callback

getInputChar :: MonadIO m => m Key
getInputChar = liftIO $ atomically $ readTVar inputKey >>= \case
  Nothing -> retry
  Just ch -> do
    writeTVar inputKey Nothing
    return ch

instance MonadIO m => KeyInteractiveIO (JavascriptTextIOT m) where
  waitForKey = liftIO getInputChar

instance MonadIO m => TextIO (JavascriptTextIOT m) where
  terminalSize = return $ Coords2D (fromIntegral terminalW) (fromIntegral terminalH)

  {-# INLINE setChar #-}
  setChar !attributes !ch (Coords2D !x !y) = do
    (!arr, _) <- JavascriptTextIOT ask
    Coords2D !tw !th <- terminalSize
    JavascriptTextIOT $ liftIO $ unless (x < 0 || y < 0 || x >= tw || y >= th) $
      IA.writeArray
        arr
        (fromIntegral x, fromIntegral y)
        (coerce attributes .|. (fromIntegral (ord ch .&. 0xfffffff) `shiftL` 36))

  clear = do
    (!arr, _) <- JavascriptTextIOT ask
    Coords2D !tw !th <- terminalSize
    JavascriptTextIOT $ liftIO $ for_ [(x, y) | x <- [0..tw-1], y <- [0..th-1]] $ \(x, y) ->
      IA.writeArray arr (fromIntegral x, fromIntegral y)
        (coerce (colorsToAttributes (Color3 0 0 0) (Color3 0 0 0)) .|.
         (fromIntegral (ord ' ' .&. 0xfffffff) `shiftL` 36))

  flush = JavascriptTextIOT $ do
    (arr, arr2) <- ask
    display_size <- liftIO $ IA.getBounds arr
    liftIO $ withMVar globalSpans $ \spans ->
     for_ (IA.range display_size) $ \idx@(!x, !y) -> do
      flush_w64 <- IA.readArray arr idx :: IO Word64
      display_w64 <- IA.readArray arr2 idx :: IO Word64
      when (flush_w64 /= display_w64) $ do
        let Just !span = M.lookup (fromIntegral x, fromIntegral y) spans
            (Color3 !fr !fg !fb, Color3 !br !bg !bb) =
              attributesToColors (coerce flush_w64)
            !ch' =
              chr $ fromIntegral $ (flush_w64 .&. 0xfffffff000000000) `shiftR` 36
            !ch = if ch' == ' '
                    then '\x00a0' -- use non-breaking space instead of space
                    else ch'
        style <- to_javascript_colorstyle fr fg fb br bg bb
        set_text_content span $ fromString [ch]
        set_style span style
        IA.writeArray arr2 idx flush_w64

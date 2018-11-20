{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}

module CM.HGameLauncher
  ( runHGameLauncherServer
  , runHGameLauncherClient
  )
where


import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Monad
import qualified Data.ByteString               as B
import qualified Data.ByteString.Builder       as BB
import qualified Data.ByteString.Lazy          as BL
import           Data.Bits
import           Data.Foldable
import           Data.Int
import           Data.Maybe
import           Data.Monoid
import           Data.Word
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import           Network.Simple.TCP
import           System.IO

import           CM.Coords
import           CM.KeyInput
import           CM.TerminalTextIO

type Host = Text
type Port = Word16

runHGameLauncherServer
  :: Host -> Port -> (TChan Key -> TerminalTextIOT IO ()) -> IO ()
runHGameLauncherServer host port action =
  serve (Host $ T.unpack host) (show port) $ \(socket, _sockaddr) -> do
    terminal_size <- newTVarIO (80, 24)
    key_chan      <- newTChanIO
    withAsync (keyReader socket key_chan terminal_size) $ \key_async -> do
      link key_async
      output_buffer <- newTVarIO mempty :: IO (TVar BB.Builder)
      let
        thandle = TerminalTextIOHandle
          { writeOutput    = \bl ->
            atomically $ modifyTVar output_buffer $ \old ->
              old <> BB.lazyByteString bl
          , writeOutputStr = \str ->
            atomically $ modifyTVar output_buffer $ \old ->
              old <> BB.stringUtf8 str
          , flushOutput    = do
            builder <- atomically $ do
              result <- readTVar output_buffer
              writeTVar output_buffer mempty
              return result
            sendLazy socket $ BB.toLazyByteString builder
          , handleGetsize  = atomically $ do
            (w, h) <- readTVar terminal_size
            -- Some denial-of-service protection: don't let terminal become yuuge
            return $ Coords2D (min 200 $ max 2 w) (min 100 $ max 2 h)
          }
      withTerminalTextIOHandle thandle (action key_chan)

keyReader :: Socket -> TChan Key -> TVar (Int16, Int16) -> IO ()
keyReader socket key_channel terminal_size = do
  recv socket 1 >>= \case
    -- key input
    Just bs | bs == B.singleton 0 -> do
      recv socket 1 >>= \case
        Nothing -> return ()
        -- TODO: properly unicode decode to char
        Just b  -> case T.decodeUtf8' b of
          Left{}                  -> again
          Right (T.unpack -> txt) -> do
            let keys = catMaybes $ fmap charToKey txt
            atomically $ for_ keys $ writeTChan key_channel
            again
    -- terminal size
    Just bs | bs == B.singleton 1 -> do
      recv socket 4 >>= \case
        Just bs | B.length bs == 4 -> do
          let w =
                (fromIntegral (bs `B.index` 0) `shiftL` 8 :: Word16)
                  .|. (fromIntegral (bs `B.index` 1) .&. 0x00ff)
              h =
                (fromIntegral (bs `B.index` 2) `shiftL` 8 :: Word16)
                  .|. (fromIntegral (bs `B.index` 3) .&. 0x00ff)
          atomically $ writeTVar terminal_size (fromIntegral w, fromIntegral h)
          again
        _ -> return ()
    _ -> return ()
  where again = keyReader socket key_channel terminal_size


runHGameLauncherClient :: Host -> Port -> IO ()
runHGameLauncherClient host port = do
  connect (T.unpack host) (show port) $ \(socket, _sockaddr) ->
    withRawTerminalSetup
      $ withAsync
          (forever $ do
            Coords2D w' h' <- getRawTerminalSize
            let w = fromIntegral w' :: Word16
                h = fromIntegral h' :: Word16
            send socket
              $ B.pack
              $ [ 1
                , fromIntegral $ (w .&. 0xff00) `shiftR` 8
                , fromIntegral $ (w .&. 0x00ff)
                , fromIntegral $ (h .&. 0xff00) `shiftR` 8
                , fromIntegral $ (h .&. 0x00ff)
                ]
            threadDelay 1000000
          )
      $ \terminal_size_worker ->
          withAsync
              (forever $ B.hGet stdin 1 >>= \bs ->
                sendLazy socket (BL.fromChunks [B.singleton 0, bs])
              )
            $ \key_async -> do
                link terminal_size_worker
                link key_async
                let
                  outputter = do
                    bs <- recv socket 100000
                    case bs of
                      Nothing -> return ()
                      Just bytes ->
                        B.hPutStr stdout bytes >> hFlush stdout >> outputter
                outputter

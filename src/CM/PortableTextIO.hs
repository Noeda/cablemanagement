{-# LANGUAGE CPP #-}

module CM.PortableTextIO
  ( withPortableTextIO
  , module CM.TextIO )
  where

import Control.Monad.Catch
import Control.Monad.IO.Class

import CM.TextIO

#ifdef GHCJS
import CM.JavascriptTextIO
withPortableTextIO :: (MonadMask m, MonadIO m) => JavascriptTextIOT m a -> m a
withPortableTextIO = withJavascriptTextIO
#else
import CM.TerminalTextIO
withPortableTextIO :: (MonadMask m, MonadIO m) => TerminalTextIOT m a -> m a
withPortableTextIO = withTerminalTextIO
#endif

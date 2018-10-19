module Main where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Foldable
import           System.Random

import           CM.TerminalTextIO

main :: IO ()
main = withTerminalTextIO $ forever $ do
  Coords2D tw th <- terminalSize
  for_ [0 .. tw - 1] $ \x -> for_ [0 .. th - 1] $ \y -> do
    let rngcolor = liftIO $ randomRIO (0, 63)
    [r, g, b, br, bg, bb] <- replicateM 6 rngcolor
    ch                    <- liftIO $ randomRIO ('A', 'z')
    setChar (colorsToAttributes (Color3 r g b) (Color3 br bg bb))
            ch
            (Coords2D x y)
  flush
  liftIO $ threadDelay 10000


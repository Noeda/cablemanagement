{-# LANGUAGE BangPatterns #-}

module Main where

import           Control.Monad.Trans.State.Strict

import           CM.LevelRender

main :: IO ()
main = do
  x <- read <$> getLine
  y <- read <$> getLine
  bresenhamTest x y

{-# NOINLINE bresenhamTest #-}
bresenhamTest :: Int -> Int -> IO ()
bresenhamTest !x !y = do
  v <- flip execStateT (0 :: Int) $ bresenham (0 :: Int) 0 x y $ \(!x) !y -> do
    !st <- get
    let !new = st + x + y
    put new
  print v

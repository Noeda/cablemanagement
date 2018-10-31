{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}

module CriterionShim
#ifndef GHCJS
  ( module Criterion
  , module Criterion.Main )
#else
  ( defaultMain
  , whnf
  , bench
  , bgroup
  , Benchmark() )
#endif
  where

#ifdef GHCJS
import Control.Exception
import Control.Monad
import Data.Foldable
import Data.IORef
import Data.Monoid
import Data.Time
import System.Timeout

data Benchmark
  = Benchmark String (IO ())
  | Group String [Benchmark]

defaultMain :: [Benchmark] -> IO ()
defaultMain benchmarks = for_ benchmarks $ \case
  Benchmark name runner ->
    runBenchmark name runner
  Group group benchmarks -> do
    putStrLn $ "Group: " <> show group
    defaultMain benchmarks

bgroup :: String -> [Benchmark] -> Benchmark
bgroup = Group

{-# NOINLINE whnf #-}
whnf :: (a -> b) -> a -> IO ()
whnf fun v = void $ evaluate $ fun v

{-# NOINLINE bench #-}
bench :: String -> IO () -> Benchmark
bench name io = io `seq` Benchmark name io

{-# NOINLINE runBenchmark #-}
runBenchmark :: String -> IO () -> IO ()
runBenchmark str runner = do
  runner
  count <- newIORef (0 :: Int)
  void $ timeout 10000000 (go runner count)
  writeIORef count 0
  void $ timeout 10000000 (go runner count)
  number_of_times_run <- readIORef count
  putStrLn $ "Benchmarked '" <> str <> "' average time: " <> show (10 / fromIntegral number_of_times_run :: Double)
 where
  go runner !count = do
    runner
    modifyIORef' count (+1)
    go runner count
#else
import Criterion
import Criterion.Main
#endif

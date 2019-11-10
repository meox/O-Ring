{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}

module Main where

import qualified Paths_HaskRing as HR

import Options
import Options.Applicative
import Control.Concurrent.Chan 
import Control.Concurrent.MVar

import Control.Monad
import Control.Concurrent
import Data.Version (showVersion)
import Data.Time.Clock 
import qualified Data.Vector as V
import Data.Vector ((!)) 

main :: IO ()
main = updateOptions <$> execParser opts >>= \opt@Options{..} -> if | version   -> putStrLn $ showVersion HR.version 
                                                                    | otherwise -> mainRun opt
    where opts = info (helper <*> parseOptions)
                      (fullDesc <> header "haskring!")


mainRun :: Options -> IO ()
mainRun Options{..} = do

  t' <- getCurrentTime 

  (t'', total) <-
    if unbuffered 
      then do
        (s, e) <- mkRingUnbuff nodes 
        tb <- getCurrentTime
        total <- V.sum <$> (V.forM (V.generate trips (+1)) $ \_ -> putMVar s 0 >> takeMVar e)
        return (tb, total)
      else do
        (s, e) <- mkRing nodes 
        tb <- getCurrentTime
        total <- V.sum <$> (V.forM (V.generate trips (+1)) $ \_ -> writeChan s 0 >> readChan e)
        return (tb, total)

  t''' <- getCurrentTime 

  let tsetup :: Int = round (1000 * (t'' `diffUTCTime` t'))
  let tring  :: Int = round (1000 * (t''' `diffUTCTime` t''))

  if total == nodes*trips
    then putStrLn $ show tsetup <> " " <> show tring <> " " <> show nodes <> " " <> show trips
    else errorWithoutStackTrace "Ring failed!"


mkRing :: Int -> IO (Chan Int, Chan Int)
mkRing n = do
  cap <- getNumCapabilities 
  chans :: V.Vector (Chan Int) <- V.generateM (n+1) $ const newChan
  forM_ [0..n-1] $ \i -> forkOn (n `div` cap) $ do
    let r = chans ! i
        w = chans ! (i+1)
    forever $ readChan r >>= \x -> writeChan w (x+1)

  return (V.head chans, V.last chans)


mkRingUnbuff :: Int -> IO (MVar Int, MVar Int)
mkRingUnbuff n = do
  cap <- getNumCapabilities 
  chans :: V.Vector (MVar Int) <- V.generateM (n+1) $ const newEmptyMVar
  forM_ [0..n-1] $ \i -> forkOn (n `div` cap) (do
    let r = chans ! i
        w = chans ! (i+1)
    forever $ takeMVar r >>= \x -> putMVar w (x+1))
  return (V.head chans, V.last chans)


updateOptions :: Options -> Options
updateOptions Options{..} =
  case args of
    [numberOfNodes, numberOfTrips] -> Options { nodes=read numberOfNodes, trips= read numberOfTrips, ..}
    _ -> Options{..}


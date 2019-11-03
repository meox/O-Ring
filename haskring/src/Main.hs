{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiWayIf #-}

module Main (module Main) where

import qualified Paths_HaskRing as HR

import Options
import Options.Applicative
import Control.Concurrent.Chan 
import Control.Monad (forever, forM_)
import Control.Concurrent (forkIO)
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
  ta <- getCurrentTime 
  (s, e) <- createRing nodes 
  tb <- getCurrentTime
  forM_ [1 .. trips] $ \i -> writeChan s i >> readChan e
  tc <- getCurrentTime

  let t0 :: Int = round (1000 * (tb `diffUTCTime` ta))
  let t1 :: Int = round (1000 * (tc `diffUTCTime` tb))

  putStrLn $ show t0 <> " " <> show t1 <> " " <> show nodes <> " " <> show trips

node' :: Chan Int -> Chan Int -> IO ()
node' s d = do 
  msg <- readChan s 
  writeChan d msg 
  node' s d


node'' :: (?s :: Chan Int, ?d :: Chan Int) => IO ()
node'' = do 
  msg <- readChan ?s 
  writeChan ?d msg 
  node'' 


node''' :: Chan Int -> Chan Int -> IO ()
node''' s d = forever $ readChan s >>=writeChan d


createRing :: Int -> IO (Chan Int, Chan Int)
createRing n = do
   chans :: V.Vector (Chan Int) <- V.generateM (n+1) $ const newChan

   -- basic:
   let node = node'

   -- using forever:
   -- let node = node'''
    
   -- using implicit parameters:
   -- let node s d = let ?s = s  
   --                    ?d = d in node''

   forM_ [0..n-1] $ \i -> forkIO (node (chans ! i) (chans ! (i+1)))

   return (V.head chans, V.last chans)

updateOptions :: Options -> Options
updateOptions Options{..} =
  case args of
    [numberOfNodes, numberOfTrips] -> Options { nodes=read numberOfNodes, trips= read numberOfTrips, ..}
    _ -> Options{..}

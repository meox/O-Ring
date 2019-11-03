{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiWayIf #-}

module Main (module Main) where

-- import System.Environment

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
main = execParser opts >>= \opt@Options{..} -> if | version   -> putStrLn $ showVersion HR.version 
                                                  | otherwise -> mainRun opt
    where opts = info (helper <*> parseOptions)
                      (fullDesc <> header "GitPrompt!")

mainRun :: Options -> IO ()
mainRun Options{..} = do
  t0 <- getCurrentTime 
  (s, e) <- createRing processes
  t1 <- getCurrentTime
  forM_ [1 .. trips] $ \i -> writeChan s i >> readChan e
  t2 <- getCurrentTime

  putStrLn $ "setup: " <> show (t1 `diffUTCTime` t0)
  putStrLn $ "trips: " <> show (t2 `diffUTCTime` t1)


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

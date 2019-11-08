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

data ThreadContext = ThreadContext {
      threadIndex :: {-# UNPACK #-} !Int
    -- ^ The index of this thread in the ring. The parent index can be 
    -- trivially calculated.
    , totalNodes :: {-# UNPACK #-} !Int
    -- ^ The total nodes in the ring.
    , totalTrips :: {-# UNPACK #-} !Int
    -- ^ The number of times each message needs to be passed around.
    , mailbox :: Chan ()
    -- ^ The mailbox for this thread.
    }

newtype Ring = Ring { getRing :: V.Vector ThreadContext }

neighbourIndex :: ThreadContext -> Int
neighbourIndex ThreadContext{..}
  | threadIndex == totalNodes - 1 = 0
  | otherwise = succ threadIndex
{-# INLINE neighbourIndex #-}

sendMessage :: Ring -> Int -> IO ()
sendMessage (Ring ring) threadIndex =
    writeChan (mailbox $ ring V.! threadIndex) ()
{-# INLINE sendMessage #-}

main :: IO ()
main = updateOptions <$> execParser opts >>= \opt@Options{..} -> if | version   -> putStrLn $ showVersion HR.version 
                                                                    | unbuffered -> mainRunUnbuff  opt
                                                                    | otherwise -> mainRun opt
    where opts = info (helper <*> parseOptions)
                      (fullDesc <> header "haskring!")

mainRun :: Options -> IO ()
mainRun opts@Options{..} = do
  setupStarted <- getCurrentTime 
  ring <- newRing opts
  setupEnded <- getCurrentTime

  -- We can already deliver the message to threadIndex = 0, as the mailbox
  -- already exist.
  forM_ [1 .. trips] $ \_ -> sendMessage ring 0

  let lastThread = getRing ring V.! (nodes - 1)
  lastThreadMailbox <- dupChan (mailbox lastThread)

  forM_ (getRing ring) $ \ctx -> spinUpThread ctx ring

  -- Wait for messages on the last element of the ring.
  forM_ [1 .. trips] $ \_ -> readChan lastThreadMailbox

  messageExchangeEnded <- getCurrentTime

  let t0 :: Int = round (1000 * (setupEnded `diffUTCTime` setupStarted))
  let t1 :: Int = round (1000 * (messageExchangeEnded `diffUTCTime` setupEnded))

  putStrLn $ show t0 <> " " <> show t1 <> " " <> show nodes <> " " <> show trips

mainRunUnbuff :: Options -> IO ()
mainRunUnbuff Options{..} = do
  ta <- getCurrentTime 
  (s, e) <- createRingUnbuff nodes 
  tb <- getCurrentTime
  forM_ [1 .. trips] $ \i -> putMVar s i >> takeMVar e
  tc <- getCurrentTime

  let t0 :: Int = round (1000 * (tb `diffUTCTime` ta))
  let t1 :: Int = round (1000 * (tc `diffUTCTime` tb))

  putStrLn $ show t0 <> " " <> show t1 <> " " <> show nodes <> " " <> show trips


node' :: Chan Int -> Chan Int -> IO ()
node' s d = do 
  msg <- readChan s 
  writeChan d msg 
  node' s d

-- | Creates a new 'Ring' from some 'Options'.
newRing :: Options -> IO Ring
newRing Options{..} = Ring . V.fromList <$> (
    forM [0 .. nodes - 1] $ \nodeIndex -> do
        myMailbox       <- newChan
        pure ThreadContext { threadIndex   = nodeIndex
                           , totalNodes    = nodes
                           , totalTrips    = trips
                           , mailbox       = myMailbox
                           })

spinUpThread :: ThreadContext -> Ring -> IO ThreadId
spinUpThread ctx@ThreadContext{..} ring = forkIO loop
  where
      loop :: IO ()
      loop = do
          () <- readChan mailbox
          sendMessage ring (neighbourIndex ctx)
          loop


createRingUnbuff :: Int -> IO (MVar Int, MVar Int)
createRingUnbuff n = do
   chans :: V.Vector (MVar Int) <- V.generateM (n+1) $ const newEmptyMVar

   forM_ [0..n-1] $ \i -> forkIO (forever $ takeMVar (chans ! i) >>= putMVar (chans ! (i+1)))

   return (V.head chans, V.last chans)


updateOptions :: Options -> Options
updateOptions Options{..} =
  case args of
    [numberOfNodes, numberOfTrips] -> Options { nodes=read numberOfNodes, trips= read numberOfTrips, ..}
    _ -> Options{..}

{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module Options
    (
      Options(..)
    , parseOptions
    ) where

import Options.Applicative
import Data.Semigroup ((<>), Semigroup(..))

-- import Control.Applicative

data Options = Options
    {
      processes       :: Int
    , trips           :: Int
    , version         :: Bool
    } deriving (Show)


parseOptions :: Parser Options
parseOptions = do
    version <- switch
            ( long "version"
           <> short 'V'
           <> help "Print version")
    processes <- option auto 
            ( long "processes"
           <> short 'p'
           <> value 1000
           <> help "Specify the number of processes")
    trips <- option auto 
            ( long "trip"
           <> short 't'
           <> value 10
           <> help "Specify the number of trips")
    return Options{..}


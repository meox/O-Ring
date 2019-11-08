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
      nodes           :: Int
    , trips           :: Int
    , version         :: Bool
    , unbuffered      :: Bool 
    , args            :: [String]
    } deriving (Show)


parseOptions :: Parser Options
parseOptions = do
    version <- switch
            ( long "version"
           <> short 'V'
           <> help "Print version")
    unbuffered <- switch
            ( long "unbuffered"
           <> short 'u'
           <> help "Use unbuffered MVar")
    nodes <- option auto
            ( long "nodes"
           <> short 'p'
           <> value 1000
           <> help "Specify the number of nodes")
    trips <- option auto
            ( long "trips"
           <> short 't'
           <> value 10
           <> help "Specify the number of trips")
    args <- many (strArgument (help "N<nodes> M<trips>"))
    return Options{..}


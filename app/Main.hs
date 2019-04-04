{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

module Main where

import Lib

import Control.Applicative
import qualified Data.ByteString.Lazy as BL
import Data.Csv hiding (encodeDefaultOrderedByName)
-- import Data.Csv.Incremental
import Data.Monoid
import GHC.Generics
import qualified Data.Vector as V


main :: IO ()
main = do
  csvData <- BL.readFile "homework.csv"
  xs <- bytestringToVectorPlayer csvData
  case xs of
    Nothing -> putStrLn "failed to parse CSV"
    Just file -> do
      let transformedPlayerVector (header, players)
        = (header, ( V.map playerScores . sortPlayersByShirt) players)
      BL.writeFile
          "homework.csv"
          (vectorPlayerToBytestring $ transformedPlayerVector file)
      printPlayers $ transformedPlayerVector file

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib where

import Control.Applicative
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V
import qualified Data.List as L
import GHC.Generics

data Player =
  Player
    { shirtNumber :: !Int
    , name        :: !String
    , goals       :: !Int
    } deriving Generic

instance FromNamedRecord Player where
  parseNamedRecord r =
    Player
      <$> r .: "shirtNumber"
      <*> r .: "name"
      <*> r .: "goals"

instance ToNamedRecord Player
instance DefaultOrdered Player
instance ToRecord Player

type PlayerVector = (V.Vector Name, V.Vector Player)

bytestringToVectorPlayer :: BL.ByteString -> IO (Maybe PlayerVector)
bytestringToVectorPlayer csvData =
    case decodeByName csvData of
      Left err -> pure $ Nothing
      Right (h, v) -> pure $ Just (h, v)

vectorPlayerToBytestring :: PlayerVector -> BL.ByteString
vectorPlayerToBytestring (header, players) = encodeByName header (V.toList players)

playerScores :: Player -> Player
playerScores player = player {goals = (goals player) + 1}

sortPlayersByShirt :: V.Vector Player -> V.Vector Player
sortPlayersByShirt = V.fromList . L.sortOn shirtNumber . V.toList

printPlayers :: PlayerVector -> IO ()
printPlayers (_, players) =  do
  V.forM_ players $ \ player ->
    putStrLn
      $ name player
      ++ " ("
      ++ show (shirtNumber player)
      ++ ")"
      ++ " has scored "
      ++ show (goals player)
      ++ " goals this season."

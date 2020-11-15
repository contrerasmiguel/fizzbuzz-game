module Game (
      NextPlayer (NextPlayer)
    , nextPlayer
    , noLosers
    ) where

import FizzBuzz (Result (Incorrect))
import Player   (Player)

newtype NextPlayer = NextPlayer Player

nextPlayer :: Player -> [Player] -> Player
nextPlayer currentPlayer players =
    head $ tail $ dropWhile (currentPlayer /=) $ cycle players

noLosers :: Player -> Result -> [Player] -> [Player]
noLosers currentPlayer Incorrect = filter $ (/=) currentPlayer
noLosers _ _ = id

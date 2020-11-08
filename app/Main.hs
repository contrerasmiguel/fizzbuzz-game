module Main where                                                                                                                                                      

import Player (
      Player (Player)
    , PlayerPoolSize (poolSize)
    , availablePlayers
    , intToPlayerPoolSize
    , maxPlayers
    )

import Control.Monad (join, replicateM)
import Data.Function (on)
import Data.List (sortBy)
import Data.Profunctor (lmap)
import System.Random (randomRIO)

newtype NextPlayer = NextPlayer Player

shuffle :: [a] -> IO [a]
shuffle xs = do
    ys <- replicateM (length xs) $ randomRIO (1 :: Int, 100000)
    pure $ fst <$> sortBy (compare `on` snd) (zip xs ys)

shuffledPlayerPool :: IO [Player]
shuffledPlayerPool = (take $ poolSize maxPlayers) <$> shuffle availablePlayers

nextPlayer :: Player -> [Player] -> Player
nextPlayer currentPlayer players =
    head $ tail $ dropWhile ((/=) currentPlayer) $ join $ players <$ [1..]

gameLoop :: Player -> [Player] -> IO ()
gameLoop currentPlayer@(Player _ pName) players = do
    if length players == 1
        then putStrLn $ pName ++ " won!"
        else do
            putStrLn $ pName ++ "'s turn"
            gameLoop (nextPlayer currentPlayer players) $ tail players

main :: IO ()
main = join (lmap head gameLoop) =<< shuffledPlayerPool

module Main where                                                                                                                                                      

import FizzBuzz (
          Question
        , Result (Correct, Incorrect)
        , questions
        , result
        , takeQuestion
    )

import Player (
      Player (Player)
    , PlayerPoolSize (poolSize)
    , Uncertainty (Uncertainty)
    , UncertaintyFactor (UncertaintyFactor)
    , RandomFactor (RandomFactor)
    , availablePlayers
    , answer
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
    head $ tail $ dropWhile ((/=) currentPlayer) $ cycle players

noLosers :: Player -> Result -> [Player] -> [Player]
noLosers currentPlayer Incorrect = filter $ (/=) currentPlayer
noLosers _ _ = id

gameLoop :: Player
         -> [Player]
         -> Uncertainty
         -> UncertaintyFactor
         -> [Question]
         -> IO ()
gameLoop
    currentPlayer@(Player _ pName)
    players
    maxUncertainty@(Uncertainty maxUncertain)
    uncertaintyFactor@(UncertaintyFactor uFactor)
    questions = do
    if length players == 1
        then putStrLn $ pName ++ " won!"
        else do
            let (question, otherQuestions) = takeQuestion questions
            randomF <- randomRIO (1 :: Float, maxUncertain / 100)
            let playerAnswer = answer maxUncertainty uncertaintyFactor (RandomFactor randomF) question
            let pResult = result playerAnswer question
            putStrLn $ "[" ++ show question ++ "]\t" ++ pName ++ ": " ++ show playerAnswer ++ " (" ++ show pResult ++ ")"
            let remainingPlayers = noLosers currentPlayer pResult players
            gameLoop
                (nextPlayer currentPlayer players)
                remainingPlayers
                maxUncertainty
                (UncertaintyFactor $ uFactor + 1)
                otherQuestions

main :: IO ()
main = do
    players <- shuffledPlayerPool
    gameLoop
        (head players)
        players
        (Uncertainty 999)
        (UncertaintyFactor 1)
        questions

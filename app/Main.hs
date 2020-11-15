module Main where

import FizzBuzz (
      Question
    , questions
    , result
    , takeQuestion
    )

import Game (
      Player (Player)
    , Uncertainty (Uncertainty)
    , UncertaintyFactor (UncertaintyFactor)
    , RandomFactor (RandomFactor)
    , availablePlayers
    , answer
    , maxPlayers
    , nextPlayer
    , noLosers
    , playInfo
    , victoryMessage
    )

import System.Random (randomRIO)
import Util          (asManyAs, reorderBy)

shuffle :: [a] -> IO [a]
shuffle xs = reorderBy xs <$> asManyAs xs (randomRIO (1 :: Int, 100000))

shuffledPlayerPool :: IO [Player]
shuffledPlayerPool = take maxPlayers <$> shuffle availablePlayers

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
        then putStrLn $ victoryMessage pName
        else do
            let (question, otherQuestions) = takeQuestion questions
            randomF <- randomRIO (1 :: Float, maxUncertain / 100)
            let playerAnswer = answer maxUncertainty uncertaintyFactor (RandomFactor randomF) question
            let pResult = result playerAnswer question
            putStrLn $ playInfo question pName playerAnswer pResult
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

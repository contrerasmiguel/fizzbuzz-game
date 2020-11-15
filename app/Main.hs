{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad.Reader   (MonadReader (ask), runReaderT)
import Control.Monad.IO.Class (MonadIO (liftIO))

import FizzBuzz (
      Question
    , questions
    , result
    , takeQuestion
    )

import Game (
      Player            (Player)
    , Uncertainty       (Uncertainty)
    , UncertaintyFactor (UncertaintyFactor)
    , RandomFactor      (RandomFactor)
    , availablePlayers
    , answer
    , maxPlayers
    , nextPlayer
    , noLosers
    )

import Output (
      PlayInfo (PlayInfo)
    , showPlay
    , victoryMessage
    )

import System.Random (randomRIO)
import Util          (asManyAs, reorderBy)

shuffle :: [a] -> IO [a]
shuffle xs = reorderBy xs <$> asManyAs xs (randomRIO (1 :: Int, 100000))

shuffledPlayerPool :: IO [Player]
shuffledPlayerPool = take maxPlayers <$> shuffle availablePlayers

gameLoop' :: (MonadReader Uncertainty m, MonadIO m) =>
             Player
          -> [Player]
          -> UncertaintyFactor
          -> [Question]
          -> m ()
gameLoop'
    currentPlayer@(Player _ pName)
    players
    uncertaintyFactor@(UncertaintyFactor uFactor)
    questions = do
    maxUncertainty@(Uncertainty maxU) <- ask
    if length players == 1
        then liftIO $ putStrLn $ victoryMessage pName
        else do
            randomF <- liftIO $ randomRIO (1 :: Float, maxU / 100)
            let (question, otherQuestions) = takeQuestion questions
            let playerAnswer = answer maxUncertainty uncertaintyFactor (RandomFactor randomF) question
            let pResult = result playerAnswer question
            let playInfo = PlayInfo question pName playerAnswer pResult
            let remainingPlayers = noLosers currentPlayer pResult players
            liftIO $ putStrLn $ showPlay playInfo
            gameLoop'
                (nextPlayer currentPlayer players)
                remainingPlayers
                (UncertaintyFactor $ uFactor + 1)
                otherQuestions

main :: IO ()
main = do
    players <- shuffledPlayerPool
    runReaderT (gameLoop'
        (head players)
        players
        (UncertaintyFactor 1)
        questions) (Uncertainty 999)

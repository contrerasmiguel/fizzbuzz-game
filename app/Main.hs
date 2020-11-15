{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader   (MonadReader (ask), runReaderT)
import Control.Monad.State    (MonadState (put), gets, evalStateT)

import FizzBuzz (
      result
    , takeQuestion
    )

import Game (
      GameState (
          GameState
        , remainingPlayers
        , uncertaintyFactor
        , remainingQuestions
        )
    , Player            (Player)
    , Uncertainty       (Uncertainty)
    , UncertaintyFactor (UncertaintyFactor)
    , RandomFactor      (RandomFactor)
    , availablePlayers
    , answer
    , initialState
    , maxPlayers
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

gameLoop :: (MonadState GameState m, MonadReader Uncertainty m, MonadIO m) => m ()
gameLoop = do
    Uncertainty maxU <- ask
    uncertaintyFactor@(UncertaintyFactor uFactor) <- gets uncertaintyFactor
    players <- gets remainingPlayers
    questions <- gets remainingQuestions
    let currentPlayer@(Player _ pName) = head players
    if length players == 1
        then liftIO $ putStrLn $ victoryMessage pName
        else do
            randomF <- liftIO $ randomRIO (1 :: Float, maxU / 100)
            let (question, otherQuestions) = takeQuestion questions
            playerAnswer <- answer uncertaintyFactor (RandomFactor randomF) question
            let pResult = result playerAnswer question
            let playInfo = PlayInfo question pName playerAnswer pResult
            liftIO $ putStrLn $ showPlay playInfo
            put GameState {
                  remainingPlayers = noLosers currentPlayer pResult players
                , uncertaintyFactor = UncertaintyFactor $ uFactor + 1
                , remainingQuestions = otherQuestions
            }
            gameLoop

main :: IO ()
main = shuffledPlayerPool
    >>= (evalStateT (runReaderT gameLoop $ Uncertainty 999) . initialState)

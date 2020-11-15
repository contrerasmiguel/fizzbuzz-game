{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader   (MonadReader (ask), runReaderT)
import Control.Monad.State    (MonadState (put), gets, evalStateT)

import FizzBuzz (result)

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
    , maxUncertainty
    , noLosers
    )

import Output (
      PlayInfo (PlayInfo)
    , showPlay
    , victoryMessage
    )

import System.Random (randomRIO)
import Util (asManyAs, reorderBy)

shuffle :: [a] -> IO [a]
shuffle xs = reorderBy xs <$> asManyAs xs (randomRIO (1 :: Int, 100000))

shuffledPlayers :: IO [Player]
shuffledPlayers = take maxPlayers <$> shuffle availablePlayers

gameLoop :: (MonadState GameState m, MonadReader Uncertainty m, MonadIO m)
             => m ()
gameLoop = do
    Uncertainty maxU          <- ask
    UncertaintyFactor uFactor <- gets uncertaintyFactor
    players                   <- gets remainingPlayers
    questions                 <- gets remainingQuestions

    let currentPlayer@(Player _ pName) = head players
    
    if length players == 1
        then liftIO $ putStrLn $ victoryMessage pName
        else do
            randomF <- liftIO $ randomRIO (1 :: Float, maxU / 100)
            playerAnswer <- answer (RandomFactor randomF)

            let question:otherQuestions = questions
            let pResult = result playerAnswer question
            
            liftIO $ putStrLn $ showPlay $ PlayInfo question
                                                    pName
                                                    playerAnswer
                                                    pResult
            
            put GameState {
                  remainingPlayers = noLosers currentPlayer pResult players
                , uncertaintyFactor = UncertaintyFactor $ uFactor + 1
                , remainingQuestions = otherQuestions
            }
            
            gameLoop

startGameWith :: [Player] -> IO ()
startGameWith = evalStateT (runReaderT gameLoop maxUncertainty) . initialState

main :: IO ()
main = startGameWith =<< shuffledPlayers

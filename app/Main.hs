{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad          (unless)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader   (MonadReader (ask), runReaderT)
import Control.Monad.State    (MonadState, evalStateT)

import Game (
      GameState
    , Player
    , Uncertainty (Uncertainty)
    , availablePlayers
    , gameFinished
    , initialState
    , maxPlayers
    , maxUncertainty
    , nextGameState
    , nextPlay
    )

import System.Random (randomRIO)
import Util (reorderByMany)

shuffle :: [a] -> IO [a]
shuffle = reorderByMany $ randomRIO (1 :: Int, 100000)

shuffledPlayers :: IO [Player]
shuffledPlayers = take maxPlayers <$> shuffle availablePlayers

gameLoop :: (MonadState GameState m, MonadReader Uncertainty m, MonadIO m)
         => m ()
gameLoop = do
    Uncertainty maxU <- ask
    randomFactor <- liftIO $ randomRIO (1 :: Float, maxU / 100)
    play <- nextPlay randomFactor
    liftIO $ print play
    nextGameState play
    unless (gameFinished play) gameLoop

startGameWith :: [Player] -> IO ()
startGameWith = evalStateT (runReaderT gameLoop maxUncertainty) . initialState

main :: IO ()
main = startGameWith =<< shuffledPlayers

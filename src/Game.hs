{-# LANGUAGE FlexibleContexts #-}

module Game (
      GameState (
          GameState
        , remainingPlayers
        , uncertaintyFactor
        , remainingQuestions
        )
    , NextPlayer        (NextPlayer)
    , Play              (LastPlay, RegularPlay)
    , Player            (Player)
    , PlayerName
    , Uncertainty       (Uncertainty)
    , UncertaintyFactor (UncertaintyFactor)
    , RandomFactor      (RandomFactor)
    , answer
    , availablePlayers
    , gameFinished
    , initialState
    , maxPlayers
    , maxUncertainty
    , nextGameState
    , nextPlay
    , nextPlayer
    , noLosers
    ) where

import Control.Monad.Reader (MonadReader (ask))
import Control.Monad.State (MonadState, gets, put)

import FizzBuzz (
      Answer (Words, Number)
    , Question
    , Result (Incorrect)
    , Words (Fizz, Buzz, FizzBuzz)
    , correctAnswer
    , result
    )

import Output (
      PlayInfo (PlayInfo)
    , PlayerName
    , showPlay
    , victoryMessage
    )

newtype NextPlayer   = NextPlayer Player
newtype Uncertainty  = Uncertainty Float
newtype RandomFactor = RandomFactor Float

newtype UncertaintyFactor = UncertaintyFactor Float
    deriving Eq

data Player = Player UncertaintyFactor PlayerName
    deriving Eq

instance Show Player where
    show (Player (UncertaintyFactor uFactor) name) =
        "(" ++ name ++ ", " ++ show uFactor ++ ")"

data GameState = GameState {
      remainingPlayers   :: [Player]
    , uncertaintyFactor  :: UncertaintyFactor
    , remainingQuestions :: [Question]
    }

data Play = LastPlay String
          | RegularPlay Result String

instance Show Play where
    show (RegularPlay _ message) = message
    show (LastPlay message) = message

playerNames :: [PlayerName]
playerNames = [
      "Albus Dumbledore"
    , "Doctor House"
    , "Walter White"
    , "Nicolás Maduro"
    , "Joe Black"
    , "Britney Spears"
    , "Nelson Mandela"
    , "Franz Ferdinand"
    , "John P. Aquaviva"
    , "Władysław Szpilman"
    ]

playerFromName :: PlayerName -> Player
playerFromName = Player $ UncertaintyFactor 0

nextPlayer :: Player -> [Player] -> Player
nextPlayer currentPlayer players =
    head $ tail $ dropWhile (currentPlayer /=) $ cycle players

noLosers :: Player -> Result -> [Player] -> [Player]
noLosers currentPlayer Incorrect = filter $ (/=) currentPlayer
noLosers _ _ = id

availablePlayers :: [Player]
availablePlayers = playerFromName <$> playerNames

maxPlayers :: Int
maxPlayers = length playerNames

randomWords :: Uncertainty -> RandomFactor -> Words
randomWords (Uncertainty maxUncertainty) (RandomFactor rFactor) =
    case rFactor of
        x | x >= 0 && x <= (maxUncertainty / 3 - 1)
            -> Fizz
        x | x >= (maxUncertainty / 3) && x <= (2 * maxUncertainty / 3 - 1)
            -> Buzz
        _   -> FizzBuzz

randomAnswer :: MonadReader Uncertainty m => RandomFactor -> m Answer
randomAnswer rFact@(RandomFactor rFactor) = do
    maxUncertainty@(Uncertainty maxU) <- ask
    pure $ if rFactor > maxU / 2
        then Words $ randomWords maxUncertainty rFact
        else Number $ round rFactor

answer :: (MonadState GameState m, MonadReader Uncertainty m) =>
          RandomFactor
       -> m Answer
answer rFact@(RandomFactor rFactor) = do
    Uncertainty max <- ask
    (UncertaintyFactor uFactor) <- gets uncertaintyFactor
    question <- gets $ head . remainingQuestions
    if rFactor * uFactor < max
        then pure $ correctAnswer question
        else randomAnswer rFact

initialState :: [Player] -> GameState
initialState players = GameState players (UncertaintyFactor 1) questions

maxUncertainty :: Uncertainty
maxUncertainty = Uncertainty 999

questions :: [Question]
questions = [1..]

nextPlay :: (MonadState GameState m, MonadReader Uncertainty m)
     => Float -> m Play
nextPlay randomFactor = do
    players <- gets remainingPlayers
    question <- gets $ head . remainingQuestions
    playerAnswer <- answer $ RandomFactor randomFactor
    let Player _ pName = head players
    let pResult = result playerAnswer question
    pure $ if length players == 1
        then LastPlay $ victoryMessage pName
        else RegularPlay pResult $ showPlay $ PlayInfo question
                                                       pName
                                                       playerAnswer
                                                       pResult

nextGameState :: (MonadState GameState m, MonadReader Uncertainty m)
              => Play -> m ()
nextGameState (LastPlay _) = pure ()
nextGameState (RegularPlay result _) = do
    UncertaintyFactor uFactor <- gets uncertaintyFactor
    players <- gets remainingPlayers
    questions <- gets $ tail . remainingQuestions
    put GameState {
          remainingPlayers = noLosers (head players) result players
        , uncertaintyFactor = UncertaintyFactor $ uFactor + 1
        , remainingQuestions = questions
    }

gameFinished :: Play -> Bool
gameFinished (LastPlay _) = True
gameFinished (RegularPlay _ _) = False

{-# LANGUAGE FlexibleContexts #-}

module Game (
      GameState (
          GameState
        , remainingPlayers
        , uncertaintyFactor
        , remainingQuestions
        )
    , NextPlayer        (NextPlayer)
    , Player            (Player)
    , PlayerName
    , PlayerPoolSize    (poolSize)
    , Uncertainty       (Uncertainty)
    , UncertaintyFactor (UncertaintyFactor)
    , RandomFactor      (RandomFactor)
    , answer
    , availablePlayers
    , initialState
    , intToPlayerPoolSize
    , maxPlayers
    , nextPlayer
    , noLosers
    ) where

import Control.Monad.Reader (MonadReader (ask))

import FizzBuzz (
      Answer (Words, Number)
    , Question
    , Result (Incorrect)
    , Words (Fizz, Buzz, FizzBuzz)
    , correctAnswer
    , questions
    )

type PlayerName = String

newtype NextPlayer =     NextPlayer Player
newtype PlayerPoolSize = PlayerPoolSize { poolSize :: Int }
newtype Uncertainty =    Uncertainty Float
newtype RandomFactor =   RandomFactor Float

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

intToPlayerPoolSize :: Int -> Maybe PlayerPoolSize
intToPlayerPoolSize size | size < 2 || size > length playerNames = Nothing
intToPlayerPoolSize size = Just $ PlayerPoolSize size

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

answer :: MonadReader Uncertainty m =>
          UncertaintyFactor
       -> RandomFactor
       -> Question
       -> m Answer
answer (UncertaintyFactor uFactor)
       rFact@(RandomFactor rFactor)
       question = do
    Uncertainty max <- ask
    if rFactor * uFactor < max
        then pure $ correctAnswer question
        else randomAnswer rFact

initialState :: [Player] -> GameState
initialState players = GameState players (UncertaintyFactor 1) questions

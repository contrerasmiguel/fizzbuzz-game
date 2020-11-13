module Player (
      PlayerPoolSize (poolSize)
    , Player (Player)
    , PlayerName
    , Uncertainty (Uncertainty)
    , UncertaintyFactor (UncertaintyFactor)
    , RandomFactor (RandomFactor)
    , answer
    , availablePlayers
    , intToPlayerPoolSize
    , maxPlayers
    ) where

import FizzBuzz (
      Answer (Words, Number)
    , Question
    , Words (Fizz, Buzz, FizzBuzz)
    , correctAnswer
    )

type PlayerName = String

newtype UncertaintyFactor = UncertaintyFactor Float
    deriving Eq

newtype Uncertainty = Uncertainty Float
newtype RandomFactor = RandomFactor Float
newtype PlayerPoolSize = PlayerPoolSize { poolSize :: Int }

data Player = Player UncertaintyFactor PlayerName
    deriving Eq

instance Show Player where
    show (Player (UncertaintyFactor uFactor) name) =
        "(" ++ name ++ ", " ++ show uFactor ++ ")"

randomWords :: Uncertainty -> RandomFactor -> Words
randomWords (Uncertainty maxUncertainty) (RandomFactor rFactor) =
    case rFactor of
        x | x >= 0 && x <= (maxUncertainty / 3 - 1)
            -> Fizz
        x | x >= (maxUncertainty / 3) && x <= (2 * maxUncertainty / 3 - 1)
            -> Buzz
        _   -> FizzBuzz

randomAnswer :: Uncertainty
             -> RandomFactor
             -> Question
             -> Answer
randomAnswer maxU@(Uncertainty maxUncertainty)
             rFact@(RandomFactor rFactor)
             question =
    if rFactor > maxUncertainty / 2
        then Words (randomWords maxU rFact)
        else Number (round rFactor)

answer :: Uncertainty
       -> UncertaintyFactor
       -> RandomFactor
       -> Question
       -> Answer
answer maxUncertain@(Uncertainty max)
       (UncertaintyFactor uFactor)
       rFact@(RandomFactor rFactor)
       question =
    if rFactor * uFactor < max
        then correctAnswer question
        else randomAnswer maxUncertain rFact question

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

maxPlayers :: PlayerPoolSize
maxPlayers = PlayerPoolSize $ length playerNames

playerFromName :: PlayerName -> Player
playerFromName = Player $ UncertaintyFactor 0

availablePlayers :: [Player]
availablePlayers = playerFromName <$> playerNames

intToPlayerPoolSize :: Int -> Maybe PlayerPoolSize
intToPlayerPoolSize size | size < 2 || size > length playerNames = Nothing
intToPlayerPoolSize size = Just $ PlayerPoolSize size

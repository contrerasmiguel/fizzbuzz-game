module FizzBuzz (
      Answer (Number, Words)
    , Words (Fizz, Buzz, FizzBuzz)
    , Question
    , Result (Correct, Incorrect)
    , correctAnswer
    , result
    ) where

import Numeric.Natural (Natural)

data Answer = Number Natural
            | Words Words
    deriving Eq

instance Show Answer where
    show (Number n) = show n
    show (Words ws) = show ws

data Words = Fizz
           | Buzz
           | FizzBuzz
    deriving Eq

instance Show Words where
    show ws = case ws of
        Fizz -> "fizz"
        Buzz -> "buzz"
        _    -> "fizz buzz"

type Question = Natural

data Result = Incorrect | Correct deriving Show

correctAnswer :: Question -> Answer
correctAnswer n
    | nMultipleOf 15 = Words FizzBuzz
    | nMultipleOf 5 = Words Buzz
    | nMultipleOf 3 = Words Fizz
    | otherwise = Number n
    where
        nMultipleOf m = n `mod` m == 0

result :: Answer -> Question -> Result
result answer question =
    if answer == correctAnswer question
        then Correct
        else Incorrect

module FizzBuzz (
      Answer (Number, Words)
    , Words (Fizz, Buzz, FizzBuzz)
    , Question
    , Result (Correct, Incorrect)
    , correctAnswer
    , questions
    , result
    , takeQuestion
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

questions :: [Question]
questions = [1..]

takeQuestion :: [Question] -> (Question, [Question])
takeQuestion questions = (head questions, tail questions)

correctAnswer :: Question -> Answer
correctAnswer n =
    if n `mod` 3 == 0
        then if n `mod` 5 == 0
            then Words FizzBuzz
            else Words Fizz
        else if n `mod` 5 == 0
            then Words Buzz
            else Number n

result :: Answer -> Question -> Result
result answer question =
    if answer == (correctAnswer question)
        then Correct
        else Incorrect

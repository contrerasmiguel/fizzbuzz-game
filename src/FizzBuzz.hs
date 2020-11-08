module FizzBuzz (
      Answer (Number, Words)
    , Words (Fizz, Buzz, FizzBuzz)
    , Question
    , Result
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

-- answerForNumberIs :: Answer -> Question -> String
-- answerForNumberIs answer question =
--     "The answer for " ++ show question ++ " is " ++ show answer

-- How does it work?
-- answerForNumberIs is of type: Answer -> Question -> String
-- lmap plugs correctAnswer (Question -> Answer) to the "answer" parameter,
-- resulting in Question -> Question -> String. This is, in fact, a monad of
-- type ((->) Question Question), which can be reduced by applying the join
-- function. For this particular monad instance, join results in a function that
-- receives just one question, but uses it twice internally

-- correctAnswerForNumberIs :: Question -> String
-- correctAnswerForNumberIs = join $ lmap correctAnswer answerForNumberIs

-- someFunc :: IO ()
-- someFunc = do
--     putStr "Please, enter a number: "
--     inputText <- getLine
--     let maybeNumber = readMaybe inputText :: Maybe Natural
--     maybe
--         (return ())
--         (putStrLn . correctAnswerForNumberIs)
--         maybeNumber

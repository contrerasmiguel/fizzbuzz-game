module Player () where

import FizzBuzz

data ErrorRate = ErrorRate {
    -- The higher the count, the harder the difficulty
    difficulty :: Float,
    -- It increases when other players say incorrect answers or when the last
    -- word is distracting (fizz, buzz, fizz buzz)
    currentNumberUncertainty :: Float
}

type RandomFactor = Float

maxError = 999

randomWords :: RandomFactor -> Words
randomWords randomFactor =
    case randomFactor of
        x | x >= 0 && x <= (maxError / 3 - 1)
            -> Fizz
        x | x >= (maxError / 3) && x <= (2 * maxError / 3 - 1)
            -> Buzz
        x | x >= (2 * maxError / 3) && x <= maxError
            -> FizzBuzz

randomAnswer :: Question -> RandomFactor -> Answer
randomAnswer question randomFactor =
    if randomFactor > maxError / 2
        then Words (randomWords randomFactor)
        else Number (round randomFactor)

nextPlay :: Question -> ErrorRate -> RandomFactor -> Answer
nextPlay question errorRate randomFactor =
    if randomFactor * (difficulty errorRate
        + currentNumberUncertainty errorRate) < maxError
        then correctAnswer question
        else randomAnswer question randomFactor

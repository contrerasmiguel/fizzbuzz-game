module Output (
      PlayInfo (PlayInfo)
    , PlayerName
    , showPlay
    , victoryMessage
    ) where

import Control.Arrow ((>>>))
import FizzBuzz (Result, Question, Answer)

type PlayerName = String

data PlayInfo = PlayInfo {
      question   :: Question
    , playerName :: PlayerName
    , answer     :: Answer
    , result     :: Result
    }

victoryMessage :: PlayerName -> String
victoryMessage = flip (++) " won!"

showQuestion :: PlayInfo -> String
showQuestion playInfo = "[" ++ show (question playInfo) ++ "]"

showPlayer :: PlayInfo -> String
showPlayer playInfo = '\t' : playerName playInfo ++ ": "

showResult :: PlayInfo -> String
showResult playInfo = " (" ++ show (result playInfo) ++ ")"

showPlay :: PlayInfo -> String
showPlay = pure >>> (<*>) [ showQuestion
                          , showPlayer
                          , show . answer
                          , showResult ] >>> concat

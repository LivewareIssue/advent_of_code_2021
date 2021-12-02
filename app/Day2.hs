module Day2 where

import Data.Attoparsec.Text (Parser, string, decimal, skipSpace, endOfInput, parseOnly)
import qualified Data.Text as Text (lines)
import qualified Data.Text.IO as Text (getContents)
import Control.Applicative ((<|>))
import Lens.Micro (lens, Lens', (&), (+~), (-~), (^.), _1, _2, _3)
import Data.Either (fromRight)
import Text.Printf (printf)
import Data.Kind (Type)

data Direction = Forward | Up | Down

-- A command is a direction and a count, e.g. `forward 5`
data Command = Command !Direction !Integer

-- The puzzle input is a list of commands
type Input = [Command]

-- There are multiple interpretations of commands:
class Interpretation i where
    -- Each interpretation has a state type;
    type State i = (s :: Type) | s -> i
    -- The state type has a horizontal position
    horizontalPosition :: Lens' (State i) Integer
    -- and a depth.
    depth :: Lens' (State i) Integer
    -- How a command affects the state depends on the interpretation
    interpret :: Command -> State i -> State i



data PartOne

instance Interpretation PartOne where
    type State PartOne = (Integer, Integer)
    horizontalPosition = _1
    depth = _2
    interpret (Command direction n) = case direction of
        Forward -> horizontalPosition +~ n
        Up -> depth -~ n
        Down -> depth +~ n



data PartTwo

instance Interpretation PartTwo where
    type State PartTwo = (Integer, Integer, Integer)
    horizontalPosition = _1
    depth = _2
    interpret (Command direction n) state = case direction of
        Forward -> state
            & horizontalPosition +~ n
            & depth +~ state^.aim * n
        Up -> state & aim -~ n
        Down -> state & aim +~ n

aim :: Lens' (State PartTwo) Integer
aim = _3



main :: IO ()
main = do
    commands <- reverse <$> readInput
    printResult $ foldr (interpret @PartOne) (0, 0) commands
    putStrLn mempty
    printResult $ foldr (interpret @PartTwo) (0, 0, 0) commands
    where
        readInput :: IO Input
        readInput = fmap (either error id . parseOnly commandParser)
            . Text.lines
            <$> Text.getContents

        printResult :: Interpretation i => State i -> IO ()
        printResult result = do
            printf "Final horizontal position: %d\n" (result^.horizontalPosition)
            printf "Final depth: %d\n" (result^.depth)
            printf "Answer: %d\n" (result^.horizontalPosition * result^.depth)

directionParser :: Parser Direction
directionParser = ("forward" >> return Forward)
    <|> ("up" >> return Up)
    <|> ("down" >> return Down)

commandParser :: Parser Command
commandParser = do
    d <- directionParser
    skipSpace
    n <- decimal
    endOfInput
    return $! Command d n

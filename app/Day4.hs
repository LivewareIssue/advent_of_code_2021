{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day4 where

import Control.Arrow ((&&&))
import Data.Attoparsec.Text (Parser)
import Data.Distributive (distribute, Distributive)
import Data.Foldable (for_, toList)
import Data.Function (on)
import Data.Functor.Compose (Compose (..))
import Data.Functor.Rep (Representable, distributeRep)
import Data.List (intercalate, find, tails)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Data.Set (Set)
import Data.Vector.Sized (Vector)
import GHC.Natural (Natural)
import Text.Printf (printf)

import qualified Data.Attoparsec.Text as Parser
import qualified Data.Set as Set
import qualified Data.Text.IO as Text
import qualified Data.Vector.Sized as Vector

main :: IO ()
main = do
    contents <- Text.readFile "data/day_4.txt"

    let (draws, boards) = either error id
            $ Parser.parseOnly (input <* Parser.skipSpace <* Parser.endOfInput) contents

        -- Each 'round' is a draw with the state of every board after the draw has been marked
        rounds = zip draws
            $ tail
            $ scanl (\currentBoards n -> fmap @Board (handleDraw n) <$> currentBoards) boards draws

        -- Find the first board to win (with respect to the order in which they appear in the input)
        Just (firstWinningDraw, Board firstWinningBoard) = find (bingo . snd)
            $ concat [(draw,) <$> currentBoards | (draw, currentBoards) <- rounds]

        -- Determine which boards won on each round
        deltas =
            [ (draw, fmap getNumber <$> toList delta)
            | [(_, before),(draw, after)] <- windows 2 rounds
            , let delta = winningSet after `Set.intersection` losingSet before
            , not $ Set.null delta
            ]

        (lastWinningDraw, [Board lastWinningBoard]) = last deltas

    putStrLn "First winning board:\n"
    printColumns firstWinningBoard
    putStrLn ""

    putStr "First winning draw: "
    print firstWinningDraw
    putStrLn ""

    putStrLn "Unmarked numbers:\n"
    print (unmarkedNumbers firstWinningBoard)
    putStrLn ""

    printf "Part 1: %d\n\n\n" (score firstWinningBoard firstWinningDraw)

    putStrLn "Last winning board:\n"
    printColumns lastWinningBoard
    putStrLn ""

    putStr "Last winning draw: "
    print lastWinningDraw
    putStrLn ""

    putStrLn "Unmarked numbers:\n"
    print (unmarkedNumbers lastWinningBoard)
    putStrLn ""

    printf "Part 2: %d\n\n" (score lastWinningBoard lastWinningDraw)

unmarkedNumbers :: Vector m (Vector n Cell) -> [Natural]
unmarkedNumbers = fmap number
    . filter (not . isMarked)
    . concatMap Vector.toList
    . Vector.toList

score :: Vector m (Vector n Cell) -> Natural -> Natural
score board draw = sum (unmarkedNumbers board) * draw

windows :: Int -> [a] -> [[a]]
windows w xs = let n = length xs in map (take w)
    $ take (n - w + 1)
    $ tails xs

equalByNumber :: [Board Cell] -> Set (Board Number)
equalByNumber = Set.fromList
    . fmap (fmap @Board Number)

winningSet :: [Board Cell] -> Set (Board Number)
winningSet = equalByNumber
    . filter bingo

losingSet :: [Board Cell] -> Set (Board Number)
losingSet = equalByNumber
    . filter (not . bingo)

-- Parsing the puzzle input

-- A row is represented as 5 numbers separated by (horizontal) whitespace
row :: Parser (Vector 5 Natural)
row = do
    Parser.skipWhile (== ' ')
    xs <- Parser.decimal `Parser.sepBy1` Parser.skipWhile (== ' ')
    return
        $! fromMaybe (error "expected 5 columns")
        $ Vector.fromList @5 xs

-- A board is represented as 5 rows separated by newlines
board :: Parser (Board Cell)
board = fmap Unmarked
    . Board
    . distribute
    . fromMaybe (error "expected 5 rows")
    . Vector.fromList @5
    <$> (row `Parser.sepBy1` Parser.endOfLine)


input :: Parser ([Natural], [Board Cell])
input = do
    -- First parse the draws as a comma-delimited list of numbers
    draws <- Parser.decimal @Natural `Parser.sepBy1` ","
    Parser.endOfLine
    Parser.endOfLine
    -- Then parse at least one board (separated by whitespace)
    boards <- board `Parser.sepBy1` Parser.skipWhile (== '\n')
    return (draws, boards)


-- Bingo boards

data Cell = Cell
    { number :: Natural
    , isMarked :: Bool
    }
    deriving (Eq, Ord)

newtype Number = Number
    { getNumber :: Cell
    }

instance Show Number where
    show = show . getNumber

instance Eq Number where
    (==) = (==) `on` (number . getNumber)

instance Ord Number where
    compare = comparing (number . getNumber)

pattern Unmarked :: Natural -> Cell
pattern Unmarked n = Cell n False

pattern Marked :: Natural -> Cell
pattern Marked n = Cell n True

mark :: Cell -> Cell
mark (Cell n _) = Marked n

newtype Board a = Board (Vector 5 (Vector 5 a))
    deriving stock (Eq, Ord, Functor)
    deriving Representable via (Compose (Vector 5) (Vector 5))

instance Distributive Board where
    distribute = distributeRep


-- Game rules

handleDraw :: Natural -> Cell -> Cell
handleDraw draw = \case
    Unmarked n | n == draw -> Marked n
    cell -> cell

bingo :: Board Cell -> Bool
bingo (Board columns) = any (all isMarked)
    $ (Vector.++) rows columns
    where
        rows = distribute columns


--- Displaying boards

instance Show Cell where
    show (Marked n) = colorise foreground blue (printf "%2d" n)
        where
            foreground = 9
            blue = 4
            colorise layer color s = printf @(Natural -> Natural -> String -> String)
                "\x1b[%d%dm%s\x1b[0m" layer color s

    show (Unmarked n) = printf "%2d" n

printColumns :: (Show a, Foldable t, Distributive t) => t (t a) -> IO ()
printColumns columns =
    for_ (distribute columns) $ \row -> do
        putStrLn
            $ unwords
            $ toList
            $ show <$> row

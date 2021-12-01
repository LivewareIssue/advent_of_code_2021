module Day1 where

import Data.List (tails)
import Control.Arrow ((&&&))

type Input = [Int]

-- read input from stdin and print the answers to parts 1 and 2
main :: IO ()
main = readInput >>= print . (part1 &&& part2)
    where
        -- read whole of stdin, break on newline, parse each line as an `Int`
        readInput :: IO Input
        readInput = fmap read . lines <$> getContents

        -- the number of increasing windows of width 2
        part1 :: [Int] -> Int
        part1 = length
            . filter (\[x,y] -> x < y)
            . windows 2

        -- calculate a sliding sum of width 3, pipe the results to `part1`
        part2 :: [Int] -> Int
        part2 = part1
            . map sum
            . windows 3

-- sliding window of width `w`
windows :: Int -> [a] -> [[a]]
windows w xs = let n = length xs in map (take w)
    $ take (n - w + 1)
    $ tails xs

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day3 where

import Data.List.NonEmpty (nonEmpty)
import GHC.Natural (Natural)
import Data.IntMap (IntMap)
import GHC.Generics (Generic)
import Data.Functor.Foldable (Base, Recursive (cata), Corecursive (ana))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (mapMaybe)
import qualified Data.IntMap as IntMap
import Data.List (partition, transpose)
import Control.Comonad.Cofree (Cofree ((:<)))
import Control.Comonad (extract)
import qualified Data.Char as Char
import Data.Ord (comparing)
import Data.Foldable (maximumBy, minimumBy)

main :: IO ()
main = do
    puzzleInput <- lines <$> getContents
    let report = fmap Char.digitToInt <$> puzzleInput
    print
        $ powerConsumption report
    print
        $ lifeSupportRating
        $ labelWithLeafCount
        $ buildTrie report
    where
        example =
            [ [0,0,1,0,0]
            , [1,1,1,1,0]
            , [1,0,1,1,0]
            , [1,0,1,1,1]
            , [1,0,1,0,1]
            , [0,1,1,1,1]
            , [0,0,1,1,1]
            , [1,1,1,0,0]
            , [1,0,0,0,0]
            , [1,1,0,0,1]
            , [0,0,0,1,0]
            , [0,1,0,1,0]
            ]

newtype Trie = Trie (IntMap Trie)
    deriving Generic

newtype TrieF a = TrieF (IntMap a)
    deriving (Functor, Generic)

type instance Base Trie = TrieF
instance Recursive Trie where
instance Corecursive Trie where

data Rate = Gamma | Epsilon

rate :: Rate -> [[Int]] -> Int
rate r = toDecimal . fmap bit . transpose
    where
    bit xs = fst
        $ compare (comparing snd)
        $ IntMap.toList
        $ foldr (IntMap.unionWith (+)) IntMap.empty
        $ [IntMap.singleton k 1 | k <- xs]
        where
            compare = case r of
                Gamma -> maximumBy
                Epsilon -> minimumBy

powerConsumption :: [[Int]] -> Int
powerConsumption xs = rate Gamma xs * rate Epsilon xs

headIsZero :: [Int] -> Bool
headIsZero = maybe False ((== 0) . NonEmpty.head) . nonEmpty

nonEmptyTails :: [[a]] -> [[a]]
nonEmptyTails = filter (not . null) . map (drop 1)

buildTrie :: [[Int]] -> Trie
buildTrie = ana go
    where
    go [] = TrieF IntMap.empty
    go xs = TrieF $! case partition headIsZero xs of
        (nonEmptyTails->zeroes, [])
            -> IntMap.singleton 0 zeroes
        ([], nonEmptyTails->ones)
            -> IntMap.singleton 1 ones
        (nonEmptyTails->zeroes, nonEmptyTails->ones)
            -> IntMap.fromAscList
            [ (0, zeroes)
            , (1, ones)
            ]

labelWithLeafCount :: Trie -> Cofree TrieF Int
labelWithLeafCount = cata go
    where
    go trie@(TrieF subtries)
        | IntMap.null subtries = 1 :< trie
        | otherwise = (sum $! extract <$> subtries) :< trie

data Rating = O2 | CO2

rating :: Rating -> Cofree TrieF Int -> [Int]
rating r (_ :< TrieF branches) = case
    [ (extract subtrie, digit:rating r subtrie)
    | (digit, subtrie) <- IntMap.toAscList branches
    ] of
    [] -> []
    [(_, bits)] -> bits
    [(x,zeroes), (y,ones)] -> case r of
        O2  | x > y -> zeroes
            | otherwise -> ones
        CO2 | y < x -> ones
            | otherwise -> zeroes

lifeSupportRating :: Cofree TrieF Int -> Int
lifeSupportRating trie = toDecimal (rating O2 trie) * toDecimal (rating CO2 trie)

toDecimal :: [Int] -> Int
toDecimal bits = sum $!
    [ 2^n
    | (n,b) <- zip [0..] $! reverse bits
    , b /= 0
    ]

module Main where

import Data.List (sort)

parseRange :: String -> (Integer, Integer)
parseRange s =
    let
        (start, rest) = break (== '-') s
    in
        (read start, read (tail rest))

mergeRanges :: [(Integer, Integer)] -> [(Integer, Integer)]
mergeRanges [] = []
mergeRanges [x] = [x]
mergeRanges ((s1, e1):(s2, e2):xs) =
    if s2 <= e1 + 1
    then mergeRanges ((s1, max e1 e2) : xs)
    else (s1, e1) : mergeRanges ((s2, e2) : xs)

solve :: String -> Integer
solve input =
    let
        ls = lines input
        (rangeLines, _) = break (== "") ls
        
        ranges = map parseRange rangeLines
        sortedRanges = sort ranges
        merged = mergeRanges sortedRanges
        
        counts = [ end - start + 1 | (start, end) <- merged ]
    in
        sum counts

main :: IO ()
main = do
    input <- getContents
    print (solve input)
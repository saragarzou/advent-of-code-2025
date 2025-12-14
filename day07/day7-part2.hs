module Main where

import Data.List (sort, groupBy)

processBeam :: Int -> Int -> Integer -> Char -> [(Int, Integer)]
processBeam width col count char
    | char == '^' = 
        let
            left = col - 1
            right = col + 1
            validCols = filter (\x -> x >= 0 && x < width) [left, right]
        in
            [ (c, count) | c <- validCols ]
    | otherwise = 
        [(col, count)]

consolidate :: [(Int, Integer)] -> [(Int, Integer)]
consolidate pairs =
    let
        sorted = sort pairs
        groups = groupBy (\(c1, _) (c2, _) -> c1 == c2) sorted
        sumGroup group = (fst (head group), sum [ c | (_, c) <- group ])
    in
        map sumGroup groups

solve :: String -> Integer
solve input =
    let
        rows = lines input
        width = length (head rows)
        startIndex = head [ i | (i, c) <- zip [0..] (head rows), c == 'S' ]
        
        initialState = [(startIndex, 1)]

        step currentTimelines row =
            let
                nextRaw = concat [ processBeam width c count (row !! c) 
                                 | (c, count) <- currentTimelines ]
                
                nextConsolidated = consolidate nextRaw
            in
                nextConsolidated
        
        finalTimelines = foldl step initialState rows
    in
        sum [ count | (_, count) <- finalTimelines ]

main :: IO ()
main = do
    input <- getContents
    print (solve input)
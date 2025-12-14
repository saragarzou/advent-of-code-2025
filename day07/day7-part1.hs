module Main where

import Data.List (nub, sort)

processBeam :: Int -> Int -> Char -> (Int, [Int])
processBeam width col char
    | char == '^' = 
        let
            left = col - 1
            right = col + 1
            newPos = filter (\x -> x >= 0 && x < width) [left, right]
        in
            (1, newPos)
    | otherwise = (0, [col])

solve :: String -> Int
solve input =
    let
        rows = lines input
        width = length (head rows)
        startIndex = head [ i | (i, c) <- zip [0..] (head rows), c == 'S' ]
        
        initialState = ([startIndex], 0)

        step (activeBeams, totalSplits) row =
            let
                results = [ processBeam width c (row !! c) | c <- activeBeams ]
                splitsInRow = sum [ count | (count, _) <- results ]
                nextBeamsRaw = concat [ pos | (_, pos) <- results ]
                nextBeams = sort (nub nextBeamsRaw)
            in
                (nextBeams, totalSplits + splitsInRow)
        
        (_, finalSplits) = foldl step initialState rows
    in
        finalSplits

main :: IO ()
main = do
    input <- getContents
    print (solve input)
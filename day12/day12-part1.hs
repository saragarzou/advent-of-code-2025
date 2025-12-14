module Main where

import Data.List (isInfixOf)

shapeSizes :: [Int]
shapeSizes = [7, 7, 6, 7, 5, 7]

checkFit :: String -> Bool
checkFit line = 
    let 
        (dimPart, rest) = span (/= ':') line
        (wStr, hStr)    = span (/= 'x') dimPart
        
        w = read wStr :: Int
        h = read (drop 1 hStr) :: Int 
        
        counts = map read (words (drop 1 rest)) :: [Int]
        
        regionArea   = w * h
        presentsArea = sum $ zipWith (*) counts shapeSizes
        
    in presentsArea <= regionArea

solve :: String -> Int
solve input = length $ filter checkFit dataLines
  where
    dataLines = filter (\l -> "x" `isInfixOf` l && ":" `isInfixOf` l) (lines input)

main :: IO ()
main = do
    input <- getContents
    print $ solve input
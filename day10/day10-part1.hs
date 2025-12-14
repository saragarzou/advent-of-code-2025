module Main where

import System.Environment (getArgs)
import Data.List (find, foldl', isPrefixOf)
import Data.Bits (xor, setBit)
import Data.Char (isDigit)

data Machine = Machine {
    target :: Int, 
    buttons :: [Int] 
} deriving (Show)

main :: IO ()
main = do
    content <- getContents
    let machines = map parseLine $ filter (isPrefixOf "[") (lines content)
    
    let totalPresses = sum $ map solveMachine machines
    
    putStrLn $ "Total fewest button presses: " ++ show totalPresses

solveMachine :: Machine -> Int
solveMachine (Machine tgt btns) = 
    let n = length btns
        searchSpace = [ sub | k <- [0..n], sub <- combinations k btns ]
    in case find (\sub -> foldl' xor 0 sub == tgt) searchSpace of
        Just solution -> length solution
        Nothing       -> error "No solution found for machine!"

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations k (x:xs) = map (x:) (combinations (k-1) xs) ++ combinations k xs

parseLine :: String -> Machine
parseLine line = 
    let (lightsPart, rest) = break (== ']') line
        lightsStr = drop 1 lightsPart
        
        targetMask = parseLights lightsStr
        
        buttonsPart = takeWhile (/= '{') rest
        buttonMasks = parseButtons buttonsPart
        
    in Machine targetMask buttonMasks

parseLights :: String -> Int
parseLights s = 
    foldl' (\acc (idx, char) -> if char == '#' then setBit acc idx else acc) 
           0 
           (zip [0..] s)

parseButtons :: String -> [Int]
parseButtons s = 
    let groups = extractParens s
    in map parseGroup groups

extractParens :: String -> [String]
extractParens "" = []
extractParens s = 
    case dropWhile (/= '(') s of
        "" -> []
        ('(':rest) -> 
            let (inner, after) = break (== ')') rest
            in inner : extractParens after
        _ -> [] 

parseGroup :: String -> Int
parseGroup s = 
    let 
        spaced = map (\c -> if c == ',' then ' ' else c) s
        indices = map read (words spaced) :: [Int]
    in foldl' setBit 0 indices
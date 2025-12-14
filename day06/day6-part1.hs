module Main where

import Data.List (transpose, groupBy)
import Data.Char (isSpace)

solveProblem :: [String] -> Integer
solveProblem rows =
    let
        numberLines = init rows
        operatorLine = last rows
        numbers = map read (words (unlines numberLines))
        operator = head (filter (not . isSpace) operatorLine)
    in
        if operator == '+' 
        then sum numbers 
        else product numbers

solve :: String -> Integer
solve input =
    let
        ls = lines input
        maxWidth = maximum (map length ls)
        paddedLines = [ line ++ replicate (maxWidth - length line) ' ' | line <- ls ]
        rotated = transpose paddedLines
        isEmptyRow str = all isSpace str
        groups = groupBy (\a b -> isEmptyRow a == isEmptyRow b) rotated
        problemGroups = filter (not . isEmptyRow . head) groups
        results = [ solveProblem (transpose group) | group <- problemGroups ]
    in
        sum results

main :: IO ()
main = do
    input <- getContents
    print (solve input)
module Main where

import Data.List (transpose, groupBy)
import Data.Char (isSpace)

solveBlock :: [String] -> Integer
solveBlock cols =
    let
        opChar = last (head cols)
        nums = [ read (filter (not . isSpace) (init col)) | col <- cols ]
    in
        if opChar == '+' then sum nums else product nums

solve :: String -> Integer
solve input =
    let
        ls = lines input
        w = maximum (map length ls)
        padded = [ line ++ replicate (w - length line) ' ' | line <- ls ]
        rotated = transpose padded
        
        groups = groupBy (\a b -> all isSpace a == all isSpace b) rotated
        blocks = filter (not . all isSpace . head) groups
    in
        sum (map solveBlock blocks)

main :: IO ()
main = do
    input <- getContents
    print (solve input)
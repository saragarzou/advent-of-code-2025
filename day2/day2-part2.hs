module Main where

import Text.Regex.Posix ((=~))
import Data.List (foldl')

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn delimiter str = case break (== delimiter) str of
    (before, []) -> [before]
    (before, _:after) -> before : splitOn delimiter after

main :: IO ()
main = do
    input <- getContents
    let line = head (lines input)
    let split = splitOn ',' line
    let numbers = concat [ [start..end] | (start, end) <- map parseRange split ]
    let total = foldl' (+) 0 (filter isInvalid numbers)
    putStrLn ("Sum of invalid IDs: " ++ show total)

parseRange :: String -> (Int, Int)
parseRange str = (from, to)
    where
        dashSplit = splitOn '-' str
        from = read (head dashSplit) :: Int
        to = read (dashSplit !! 1) :: Int

isInvalid :: Int -> Bool
isInvalid number = show number =~ "^(.+)\\1+$"
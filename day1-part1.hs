module Main where

import Data.List (foldl')

main :: IO ()
main = do
  input <- getContents
  let (_, finalCount) = foldl' step (50, 0) (lines input)
  putStrLn $ "Password: " ++ show finalCount

step :: (Int, Int) -> String -> (Int, Int)
step (curPos, curCount) instruction = (newPos, newCount)
  where
    dist = read (tail instruction) :: Int
    offset = if head instruction == 'R' then dist else -dist
    newPos = (curPos + offset) `mod` 100
    newCount = if newPos == 0 then curCount + 1 else curCount
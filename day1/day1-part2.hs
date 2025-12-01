module Main where

import Data.List (foldl')

main :: IO ()
main = do
  input <- getContents
  let (finalPos, finalScore) = foldl' processMove (50, 0) (lines input)
  putStrLn $ "Password: " ++ show finalScore

processMove :: (Int, Int) -> String -> (Int, Int)
processMove (curPos, curScore) instruction = (newPos, newScore)
  where
    dir = head instruction
    dist = read (tail instruction) :: Int

    fullLoops = dist `div` 100
    remainder = dist `mod` 100

    rawPos = if dir == 'R' then curPos + remainder else curPos - remainder

    passedZero = curPos /= 0 && (rawPos < 0 || rawPos > 100)
    landedZero = (rawPos `mod` 100) == 0

    pointsGained = fullLoops 
                 + (if passedZero then 1 else 0) 
                 + (if landedZero then 1 else 0)

    newPos = rawPos `mod` 100
    newScore = curScore + pointsGained
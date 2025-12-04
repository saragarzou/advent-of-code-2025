module Main where

findLargest :: Int -> String -> String
findLargest 0 _ = ""
findLargest n str =
    let
        len = length str
        limit = len - n + 1
        window = take limit str
        best = maximum window
        split = break (== best) str
        rest = snd split
    in
        head rest : findLargest (n - 1) (tail rest)

main :: IO ()
main = do
    contents <- getContents
    let rows = lines contents
    let values = [ read (findLargest 12 row) | row <- rows ] :: [Integer]
    print (sum values)
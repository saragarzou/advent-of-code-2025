module Main where

parseRange :: String -> (Integer, Integer)
parseRange s =
    let
        (start, rest) = break (== '-') s
    in
        (read start, read (tail rest))

solve :: String -> Int
solve input =
    let
        ls = lines input
        (rangeLines, rest) = break (== "") ls
        idLines = drop 1 rest

        ranges = [ parseRange r | r <- rangeLines ]
        ids = [ read i | i <- idLines ] :: [Integer]

        isFresh x = any (\(l, h) -> x >= l && x <= h) ranges
        freshIds = filter isFresh ids
    in
        length freshIds

main :: IO ()
main = do
    input <- getContents
    print (solve input)
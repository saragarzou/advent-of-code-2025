module Main where

type Coord = (Int, Int)

parseLine :: String -> Coord
parseLine str =
    let
        (xStr, rest) = break (== ',') str
        yStr = tail rest
    in
        (read xStr, read yStr)

rectArea :: Coord -> Coord -> Int
rectArea (x1, y1) (x2, y2) =
    (abs (x1 - x2) + 1) * (abs (y1 - y2) + 1)

solve :: String -> Int
solve input =
    let
        coords = map parseLine (lines input)
        areas = [ rectArea p1 p2 
                | (i, p1) <- zip [0..] coords
                , (j, p2) <- zip [0..] coords
                , i < j ]
    in
        maximum areas

main :: IO ()
main = do
    input <- getContents
    print (solve input)
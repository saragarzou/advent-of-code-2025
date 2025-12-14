module Main where

type Point = (Int, Int)
type Edge  = (Point, Point)

parseLine :: String -> Point
parseLine str =
    case break (== ',') str of
        (xStr, ',' : yStr) -> (read xStr, read yStr)
        _                  -> error "Invalid input line"

rectArea :: Point -> Point -> Int
rectArea (x1, y1) (x2, y2) =
    (abs (x1 - x2) + 1) * (abs (y1 - y2) + 1)

intersects :: Int -> Int -> Int -> Int -> Edge -> Bool
intersects left right bottom top ((ex1, ey1), (ex2, ey2))
    | ex1 == ex2 = 
        ex1 > left && ex1 < right &&
        overlap bottom top eMinY eMaxY
    | otherwise = 
        ey1 > bottom && ey1 < top &&
        overlap left right eMinX eMaxX
  where
    eMinX = min ex1 ex2
    eMaxX = max ex1 ex2
    eMinY = min ey1 ey2
    eMaxY = max ey1 ey2

    overlap a b c d = max a c < min b d

isInside :: Int -> Int -> [Edge] -> Bool
isInside x y edges =
    odd (length (filter crossesRay edges))
  where
    tx = fromIntegral x + 0.5
    ty = fromIntegral y + 0.5

    crossesRay :: Edge -> Bool
    crossesRay ((ex1, ey1), (ex2, ey2)) =
        let x1 = fromIntegral ex1
            y1 = fromIntegral ey1
            x2 = fromIntegral ex2
            y2 = fromIntegral ey2
        in
        (y1 > ty) /= (y2 > ty)
        &&
        tx < x1 + (ty - y1) * (x2 - x1) / (y2 - y1)

isValid :: [Edge] -> Point -> Point -> Bool
isValid edges p1@(x1, y1) p2@(x2, y2) =
    not wallCuts && inside
  where
    left   = min x1 x2
    right  = max x1 x2
    bottom = min y1 y2
    top    = max y1 y2

    wallCuts = any (intersects left right bottom top) edges
    inside   = isInside left bottom edges

solve :: String -> Int
solve input =
    let points = map parseLine (lines input)

        edges = zip points (tail points ++ [head points])

        rectangles =
            [ (p1, p2)
            | (i, p1) <- zip [0..] points
            , (j, p2) <- zip [0..] points
            , i < j ]

        validAreas =
            [ rectArea p1 p2
            | (p1, p2) <- rectangles
            , isValid edges p1 p2 ]
    in
        maximum validAreas

main :: IO ()
main = do
    input <- getContents
    print (solve input)

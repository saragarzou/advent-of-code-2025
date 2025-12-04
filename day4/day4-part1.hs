module Main where

getPixel :: [String] -> Int -> Int -> Char
getPixel grid r c
    | r < 0 || r >= length grid = '.'
    | c < 0 || c >= length (head grid) = '.'
    | otherwise = (grid !! r) !! c

solve :: String -> Int
solve input = 
    let grid = lines input
        rows = length grid
        cols = length (head grid)
        deltas = [(-1,-1), (-1,0), (-1,1),
                  (0,-1),          (0,1),
                  (1,-1), (1,0), (1,1)]
                  
        isAccessible r c =
            let 
                self = getPixel grid r c
                neighborCount = length [ () | (dr, dc) <- deltas, 
                                         getPixel grid (r+dr) (c+dc) == '@' ]
            in 
                self == '@' && neighborCount < 4
    in
        length [ () | r <- [0..rows-1], c <- [0..cols-1], isAccessible r c ]

main :: IO ()
main = do
    input <- getContents
    print (solve input)
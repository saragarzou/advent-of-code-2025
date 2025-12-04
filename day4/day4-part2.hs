module Main where

deltas :: [(Int, Int)]
deltas = [(-1,-1), (-1,0), (-1,1),
          (0,-1),          (0,1),
          (1,-1), (1,0), (1,1)]

main :: IO ()
main = do
    input <- getContents
    print (simulate (lines input))

simulate :: [String] -> Int
simulate grid =
    if removed == 0 then 0 else removed + simulate nextGrid
    where
        rows = length grid
        cols = length (head grid)

        at r c 
            | r >= 0 && r < rows && c >= 0 && c < cols = (grid !! r) !! c
            | otherwise = '.'

        neighbors r c = length [ () | (dr, dc) <- deltas, at (r+dr) (c+dc) == '@' ]

        nextGrid = [[ if at r c == '@' && neighbors r c < 4 then '.' else at r c 
                    | c <- [0..cols-1] ] | r <- [0..rows-1] ]

        count g = length (filter (== '@') (concat g))
        removed = count grid - count nextGrid
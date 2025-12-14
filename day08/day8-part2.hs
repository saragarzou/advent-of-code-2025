module Main where

import Data.List (sortOn, partition)

type Point = (Int, Int, Int)

dist :: Point -> Point -> Int
dist (x1, y1, z1) (x2, y2, z2) =
    (x1 - x2)^2 + (y1 - y2)^2 + (z1 - z2)^2

parseInput :: String -> [Point]
parseInput input =
    let
        parseLine line =
            let
                (xStr, rest1) = break (== ',') line
                (yStr, rest2) = break (== ',') (tail rest1)
                zStr          = tail rest2
            in (read xStr, read yStr, read zStr)
    in map parseLine (lines input)

getAllEdges :: [Point] -> [(Int, Point, Point)]
getAllEdges points =
    [ (dist p1 p2, p1, p2)
    | (i, p1) <- zip [0..] points
    , (j, p2) <- zip [0..] points
    , i < j
    ]   

processEdges :: [[Point]] -> [(Int, Point, Point)] -> Int
processEdges groups ((_, p1, p2):rest) =
    let 
        (g1List, otherGroups) = partition (\g -> p1 `elem` g) groups
        g1 = head g1List
    in
        if p2 `elem` g1 then
            processEdges groups rest
        else
            let
                (g2List, remainingGroups) = partition (\g -> p2 `elem` g) otherGroups
                g2 = head g2List
                mergedGroup = g1 ++ g2
                newGroups = mergedGroup : remainingGroups
            in
                if length newGroups == 1 then
                    let (x1, _, _) = p1
                        (x2, _, _) = p2
                    in x1 * x2
                else
                    processEdges newGroups rest

solve :: String -> Int
solve input =
    let
        points = parseInput input
        initialGroups = [ [p] | p <- points ]
        edges = getAllEdges points
        sortedEdges = sortOn (\(d, _, _) -> d) edges
    in
        processEdges initialGroups sortedEdges

main :: IO ()
main = do
    input <- getContents
    print (solve input)
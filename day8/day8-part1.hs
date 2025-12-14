module Main where

import Data.List (sortOn, sort, nub)

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

mergeGroups :: [[Point]] -> Point -> Point -> [[Point]]
mergeGroups groups p1 p2 =
    let
        g1 = head [ g | g <- groups, p1 `elem` g ]
        g2 = head [ g | g <- groups, p2 `elem` g ]
    in
        if g1 == g2 then
            groups
        else
            let merged      = g1 ++ g2
                remaining   = [ g | g <- groups, g /= g1, g /= g2 ]
            in merged : remaining

solve :: String -> Int
solve input =
    let
        points        = parseInput input
        initialGroups = [ [p] | p <- points ]
        edges         = getAllEdges points
        sortedEdges   = sortOn (\(d, _, _) -> d) edges
        edgesToUse    = take 1000 sortedEdges

        finalGroups =
            foldl (\gs (_, p1, p2) -> mergeGroups gs p1 p2)
                  initialGroups
                  edgesToUse

        sizes         = map length finalGroups
        topThree      = take 3 (reverse (sort sizes))
    in
        product topThree

main :: IO ()
main = do
    input <- getContents
    print (solve input)
module Main where

import qualified Data.Map as M
import System.IO (getContents)
import Data.List (words, dropWhile)

type Node = String
type Graph = M.Map Node [Node]

parseLine :: String -> (Node, [Node])
parseLine line = 
    let (keyPart, rest) = break (== ':') line
        neighbors = words (dropWhile (`elem` ": ") rest)
    in (keyPart, neighbors)

buildGraph :: String -> Graph
buildGraph input = M.fromList $ map parseLine (lines input)

countPaths :: Graph -> Node -> Node -> Int
countPaths graph start end = lookupMemo start
  where
    memo :: M.Map Node Int
    memo = M.fromList [ (node, countRoutes node) | node <- M.keys graph ]

    countRoutes :: Node -> Int
    countRoutes node = 
        case M.lookup node graph of
            Nothing -> 0 
            Just neighbors -> sum (map lookupMemo neighbors)

    lookupMemo :: Node -> Int
    lookupMemo n 
        | n == end  = 1
        | otherwise = M.findWithDefault 0 n memo

solvePart2 :: Graph -> Int
solvePart2 graph = pathViaDacThenFft + pathViaFftThenDac
  where
    svr_dac = countPaths graph "svr" "dac"
    dac_fft = countPaths graph "dac" "fft"
    fft_out = countPaths graph "fft" "out"

    svr_fft = countPaths graph "svr" "fft"
    fft_dac = countPaths graph "fft" "dac"
    dac_out = countPaths graph "dac" "out"

    pathViaDacThenFft = svr_dac * dac_fft * fft_out

    pathViaFftThenDac = svr_fft * fft_dac * dac_out

main :: IO ()
main = do
    input <- getContents
    let graph = buildGraph input
    
    let result = solvePart2 graph
    
    putStrLn $ "Total paths visiting both 'dac' and 'fft': " ++ show result
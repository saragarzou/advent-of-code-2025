module Main where

import qualified Data.Map as M
import System.IO (getContents)
import Data.List (words, dropWhile)

type Node = String
type Graph = M.Map Node [Node]
type Memo = M.Map Node Int

parseLine :: String -> (Node, [Node])
parseLine line = 
    let (keyPart, rest) = break (== ':') line
        neighbors = words (dropWhile (`elem` ": ") rest)
    in (keyPart, neighbors)

buildGraph :: String -> Graph
buildGraph input = M.fromList $ map parseLine (lines input)

solve :: Graph -> Int
solve graph = lookupMemo "you"
  where
    memo :: Memo
    memo = M.fromList [ (node, countPaths node) | node <- M.keys graph ]

    countPaths :: Node -> Int
    countPaths node = 
        case M.lookup node graph of
            Nothing -> 0 
            Just neighbors -> sum (map lookupMemo neighbors)

    lookupMemo :: Node -> Int
    lookupMemo "out" = 1 
    lookupMemo n = M.findWithDefault 0 n memo

main :: IO ()
main = do
    input <- getContents
    let graph = buildGraph input
    let result = solve graph
    putStrLn $ "Total distinct paths from 'you' to 'out': " ++ show result

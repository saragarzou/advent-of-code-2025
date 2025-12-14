import Data.List (transpose, findIndex, minimumBy)
import Data.Maybe (listToMaybe, catMaybes, mapMaybe)
import Data.Ratio ((%), numerator, denominator)
import Data.Char (isDigit, isSpace)
import Control.Monad (guard)

type Vector = [Rational]
type Matrix = [Vector]
type Target = [Integer]

data Machine = Machine
    { buttons :: [[Int]] 
    , target  :: Target  
    } deriving (Show)

main :: IO ()
main = do
    input <- getContents
    let machines = parseInput input
    let solutions = map solveMachine machines
    
    let totalPresses = sum $ catMaybes solutions
    
    putStrLn $ "Fewest total presses: " ++ show totalPresses

solveMachine :: Machine -> Maybe Integer
solveMachine m = 
    let 
        numRows = length (target m)
        numCols = length (buttons m)
        
        cols = map (\bIndices -> [if r `elem` bIndices then 1 else 0 | r <- [0..numRows-1]]) (buttons m)
        
        matrixA = transpose cols
        vectorB = map fromIntegral (target m) :: [Rational]
        
        augmented = zipWith (++) matrixA (map return vectorB)
        
        rrefMat = rref augmented
        
        pivots = findPivots rrefMat numCols
        freeVars = filter (`notElem` map fst pivots) [0..numCols-1]
        
        bounds = map (\c -> getBound c (target m) (buttons m)) freeVars
        
        candidates = sequence [ [0 .. b] | b <- bounds ]
        
    in 
        if null candidates then 
            checkSolution pivots []
        else
            let validCosts = mapMaybe (checkSolution pivots . zip freeVars) candidates
            in if null validCosts then Nothing else Just (minimum validCosts)

getBound :: Int -> Target -> [[Int]] -> Integer
getBound btnIdx targets buttonDefs =
    let affectedIndices = buttonDefs !! btnIdx
    in if null affectedIndices 
       then 0 
       else minimum [ targets !! i | i <- affectedIndices ]

checkSolution :: [(Int, Vector)] -> [(Int, Integer)] -> Maybe Integer
checkSolution pivotEqs freeAssignments = 
    let 
        calcPivot (pIdx, row) = 
            let constant = last row
                sumFree  = sum [ (row !! fIdx) * fromIntegral val 
                               | (fIdx, val) <- freeAssignments ]
                valRational = constant - sumFree
            in (pIdx, valRational)
            
        pivotVals = map calcPivot pivotEqs
        allVals = pivotVals ++ [ (f, fromIntegral v) | (f,v) <- freeAssignments ]
        
    in do
        guard $ all (\(_, val) -> denominator val == 1 && val >= 0) pivotVals
        return $ sum [ numerator val | (_, val) <- allVals ]

findPivots :: Matrix -> Int -> [(Int, Vector)]
findPivots matrix numCols = 
    let 
        findPivotCol row = listToMaybe [ c | c <- [0..numCols-1], row !! c /= 0 ]
    in 
        catMaybes $ zipWith (\row p -> fmap (\c -> (c, row)) p) matrix (map findPivotCol matrix)

rref :: Matrix -> Matrix
rref mat = fst $ foldl processCol (mat, 0) [0 .. length (head mat) - 2]
  where
    processCol :: (Matrix, Int) -> Int -> (Matrix, Int)
    processCol (m, r) c
        | r >= length m = (m, r)
        | otherwise =
            case findIndex (\row -> row !! c /= 0) (drop r m) of
                Nothing -> (m, r)
                Just idx -> 
                    let pivotRowIdx = r + idx
                        mSwapped = swapRows r pivotRowIdx m
                        pivotRow = mSwapped !! r
                        pivotVal = pivotRow !! c
                        normRow = map (/ pivotVal) pivotRow
                        mNext = [ if i == r then normRow 
                                  else zipWith (-) row (map (* (row !! c)) normRow)
                                | (i, row) <- zip [0..] mSwapped ]
                    in (mNext, r + 1)

    swapRows i j xs 
        | i == j = xs
        | otherwise = 
            let elemI = xs !! i
                elemJ = xs !! j
                update k x 
                    | k == i = elemJ
                    | k == j = elemI
                    | otherwise = x
            in zipWith update [0..] xs

parseInput :: String -> [Machine]
parseInput input = mapMaybe parseLine (lines input)

parseLine :: String -> Maybe Machine
parseLine line = 
    let 
        parts = splitOn '{' line
    in case parts of
        [left, right] -> 
            let 
                targetStr = takeWhile (/= '}') right
                targetVals = parseList targetStr

                buttonPart = dropWhile (/= '(') left
                
                rawGroups = splitOn '(' buttonPart
                
                cleanGroups = filter (any isDigit) rawGroups
                btnContent = map (takeWhile (/= ')')) cleanGroups
                
                btns :: [[Int]]
                btns = map parseList btnContent
                
            in Just $ Machine { buttons = btns, target = targetVals }
            
        _ -> Nothing

parseList :: Read a => String -> [a]
parseList s = 
    let clean = map (\c -> if c == ',' then ' ' else c) s
    in map read (words clean)

splitOn :: Char -> String -> [String]
splitOn delimiter str = case break (== delimiter) str of
    (a, rest) -> 
        if null rest 
        then [a] 
        else a : splitOn delimiter (tail rest)
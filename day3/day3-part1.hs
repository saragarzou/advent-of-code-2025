import Data.Char (digitToInt)
import Data.List (tails)

solveBank :: String -> Int
solveBank bank = maximum [ 10 * digitToInt x + digitToInt y | (x:ys) <- tails bank, y <- ys ]

main :: IO ()
main = do
    input <- getContents
    let total = sum $ map solveBank (lines input)
    print total
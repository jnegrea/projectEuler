import Data.List
import Data.List.Split
readMat :: String -> [[Integer]]
readMat = (map (map (read :: String -> Integer)) . map (splitOn "," )) . words
main :: IO()
main = do
        rawMatrix <- readFile "p13.txt"
        let x = readMat rawMatrix
        let ans = sum (map (!!0) x)
        writeFile "pe83.txt" $ show ans
        print ans


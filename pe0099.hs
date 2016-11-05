
import Data.List
import Data.List.Split
readMat :: String -> [[Double]]
readMat = (map (map (read :: String -> Double)) . map (splitOn "," )) . words
main :: IO()
main = do
    rawList <- readFile "p099_base_exp.txt"
    let numList = readMat rawList
    print $ maximum $ zip (map (\[a,b]->(log(b)+log(log(a)),a,b)) numList) [1..]
    --print numList

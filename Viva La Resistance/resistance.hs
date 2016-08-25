import qualified Data.List.Split as Split
import qualified Data.Map as Map
import Debug.Trace
import Data.Maybe

reconstruct :: [String] -> (String, (String, Int))
reconstruct (x:y:z:[]) = let nextNode    = max x y
                             currentNode = min x y
                         in (nextNode, (currentNode, read z))

getEdges :: [[String]] -> [(String, (String, Int))]
getEdges fileData = map reconstruct fileData

parallelCircuitCalc :: (Real a, Num a, Fractional b) => [a] -> b
parallelCircuitCalc []   = 0.0
parallelCircuitCalc info = (realToFrac (foldr (*) 1 info)) / (denomPermutation [] info 0)

denomPermutation :: (Num a, Real a, Fractional b) => [a] -> [a] -> a -> b
denomPermutation previousData [] result     = realToFrac result
denomPermutation previousData (x:xs) result =
                                denomPermutation (x:previousData) 
                                xs 
                                ((foldr (*) 1 (previousData ++ xs)) + result)

sortByDestination :: (Num a) => [(String, (String, Int))] -> Map.Map String [Int] -> Map.Map String [Int]
sortByDestination [] resultData     = resultData
sortByDestination (x:xs) resultData = 
    let new     = Map.insert (fst x) [(snd $ snd x)] resultData
        updated = Map.insert (fst x) ( (snd $ snd x) : (resultData Map.! (fst x))) resultData
    in if Map.lookup (fst x) resultData == Nothing
       then sortByDestination xs new
       else sortByDestination xs updated

calc :: (Fractional b, Show b) => Map.Map String [Int] -> [String] -> b -> b
calc nodeMap [] result     = result
calc nodeMap (x:xs) result = if (length (fromJust $ Map.lookup x nodeMap)) > 1
                             then trace ("parallel " ++ show result ++ "on length " ++ (show (Map.lookup x nodeMap))) (calc nodeMap xs (result + parallelCircuitCalc (nodeMap Map.! x)))
                             else trace ("serial  " ++ show result ++ "on length " ++ (show (Map.lookup x nodeMap))) (calc nodeMap xs (result + realToFrac (head (nodeMap Map.! x))))

--main :: IO (Map.Map String [Int])
main :: IO Double
main =  do
         fileLines <- readFile "nodes"
         let fileData = tail $ lines fileLines
         let dataSet = getEdges $ (map (Split.splitOn " ") fileData)
         let mapedData = sortByDestination dataSet Map.empty
         --return mapedData
         let result = calc mapedData (Map.keys mapedData) 0.0
         return result
          



















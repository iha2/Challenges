    import qualified Data.Set as Set
    import qualified Data.Map as Map
    import qualified Data.List.Split as Split
    import qualified Data.Text as Text
    import Debug.Trace

    fileLines :: IO [String]
    fileLines = do 
        results <- readFile "nodes"
        let eachLine = lines results
        return eachLine

    zipWithIndex :: [a] -> [(Int, a)]
    zipWithIndex nodes = zip [1..] nodes

    getValue :: Eq a => [(Int, a)] -> a -> Int
    getValue [] y       = -1
    getValue (x:rest) y = if snd x == y
                          then fst x
                          else getValue rest y

    structureData :: [String] -> (Set.Set String, Int)
    structureData []         = (Set.singleton "", -1)
    structureData (x:y:z:[]) = (Set.union (Set.singleton x) (Set.singleton y), read z)

    dataStorer :: (Num a, Eq a) => [(Set.Set String, a)] -> Map.Map (Set.Set String) [a] -> Map.Map (Set.Set String) [a]
    dataStorer [] nodeMap     = nodeMap
    dataStorer (x:xs) nodeMap = let newEntry = Map.insert (fst x) [(snd x)] nodeMap
                                    update   = (Map.insert (fst x) ((snd x) : (nodeMap Map.! (fst x))) nodeMap)
                                in  if (Map.lookup (fst x) nodeMap) == Nothing
                                    then dataStorer xs newEntry
                                    else dataStorer xs update


    denomPermutation :: (Num a, Real a, Fractional b) => [a] -> [a] -> a -> b
    denomPermutation previousData [] result     = realToFrac result
    denomPermutation previousData (x:xs) result =
                                denomPermutation (x:previousData) 
                                  xs 
                                  ((foldr (*) 1 (previousData ++ xs)) + result)


    parallelCircuitCalc :: (Real a, Num a, Fractional b) => [a] -> b
    parallelCircuitCalc []   = 0.0
    parallelCircuitCalc info = (realToFrac (foldr (*) 1 info)) / (denomPermutation [] info 0)


    calculate :: (Show a, Real a, Num a, Fractional b) => Map.Map (Set.Set String) [a] -> [Set.Set String] -> b -> b
    calculate mapData []     result  = result  
    calculate mapData (x:xs) result= 
                                let node = (mapData Map.! x)
                                in if (length node) == 1
                                   then calculate mapData xs (result + (realToFrac (head node)))
                                   else trace (show node) $ calculate mapData xs (result + (parallelCircuitCalc node))

    main :: (Fractional a) => IO a
    main =  do 
        results <- readFile "nodes"
        let allLines = tail $ lines results
        let dataset = map (Split.splitOn " ") allLines
        let structuredData = map structureData dataset
        let dataMap = dataStorer structuredData Map.empty
        print dataMap
        let result = calculate dataMap (Map.keys dataMap) 0.0
        return result

    orderedNodes :: IO [String]
    orderedNodes  = do
                    info <- fileLines
                    let keySet = head info
                    return $ Split.splitOn " " keySet

    allConnections :: IO [[String]]
    allConnections = do
                     info <- fileLines
                     let connections = tail info
                     return $ map (Split.splitOn " ") connections                


    --main = getArgs >>= print . haqify . head
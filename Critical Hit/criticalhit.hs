import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List.Split as Split

fileLines :: IO [String]
fileLines = do 
    results <- readFile "nodes"
    let eachLine = lines results
    return eachLine

zipWithIndex :: [a] -> [(Int, a)]
zipWithIndex nodes = zip [1..] nodes

getValue :: Eq a => [(Int, a)] -> a -> Int
getValue [] y       = -1
getValue (x:rest) y   = if snd x == y
                        then fst x
                        else getValue rest y

allConnections :: IO [String]
allConnections = do
                 info <- fileLines
                 let data = tail info
                 return data

orderedNodes :: IO [String]
orderedNodes  = do
                info <- fileLines
                let keySet = head info
                return $ Split.splitOn " " keySet
                

--main = getArgs >>= print . haqify . head
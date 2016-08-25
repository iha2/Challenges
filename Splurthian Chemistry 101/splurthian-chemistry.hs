-- Easy Problem
    import Data.Char

    passCase :: [Bool]
    passCase = [True, True, True, False, False, False]

    chemicals :: [(String, String)]
    chemicals = [("Spenglerium", "Ee"), ("Zeddemorium", "Zr"), ("Venkmine", "Kn"), ("Stantzon", "Zt"), ("Melintzum", "Nn"), ("Tullium", "Ty")]

    symbolComparator :: String -> String -> Bool
    symbolComparator []  _  = False
    symbolComparator _   [] = True
    symbolComparator (x:xs) symbol
        | toLower x == toLower (head symbol) = symbolComparator xs (tail symbol)
        | otherwise  = symbolComparator xs symbol

    tupleToComparator :: (String, String) -> Bool
    tupleToComparator dataSet = symbolComparator (fst dataSet) (snd dataSet) 

    checkSymbolsSet :: [(String, String)] -> ((String, String) -> Bool) -> [Bool]
    checkSymbolsSet symbolSet comparator = map comparator symbolSet

    result :: [Bool]
    result = checkSymbolsSet chemicals tupleToComparator

    answer = result == passCase






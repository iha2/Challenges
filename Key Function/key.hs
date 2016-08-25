-- key(
--  elements:  an array/list of stuff. number of items is leading array dimension,
--  key: an array/list of stuff.  Same amount of items as "elements".  If null, then defaults to same array as elements,
--  applyfunction:  function that will be called for each group of elements that have the same key.  Optionally, this function could also have the key parameter.  Results are aggregated in order of key appearance.
--  )

    import qualified Data.Map as Map

    key :: (Ord k) => [a] -> [k] -> (k -> [a] -> a) -> Map.Map k a
    key values keys f = Map.mapWithKey f (Map.fromListWith (++) $ [(x, [y]) | (x, y) <- zip keys values])

    --Histogram
    histogramData = [5, 3, 5, 2, 2, 9, 7, 0, 7, 5, 9, 2, 9, 1, 9, 9, 6, 6, 8, 5, 1, 1, 4, 8, 5, 0, 3, 5, 8, 2, 3, 8, 3, 4, 6, 4, 9, 3, 4, 3, 4, 5, 9, 9, 9, 7, 7, 1, 9, 3, 4, 6, 6, 8, 8, 0, 4, 0, 6, 3, 2, 6, 3, 2, 3, 5, 7, 4, 2, 6, 7, 3, 9, 5, 7, 8, 9, 5, 6, 5, 6, 8, 3, 1, 8, 4, 6, 5, 6, 4, 8, 9, 5, 7, 8, 4, 4, 9, 2, 6, 10]

    histogram = Map.toList $ key histogramData histogramData (\x y -> length y)

    -- grouped sum of feild
    fieldKeys = ["a","b","c","d","a","b","c","d","a","b","c","d","a","b","c","d","a","b","c","d"]
    fieldValues = [14,21,82,85,54,96,9,61,43,49,16,34,73,59,36,24,45,89,77,68]
    field = Map.toList $ key fieldValues fieldKeys (\x y -> sum y)

    nub :: (Ord k) => [a] -> [k] -> [(k,a)]
    nub valueSet keySet = Map.toList $ key valueSet keySet (\x y -> head y) 






unsortedList = [3,4,5,6,4,23,1,2,3,4,5,6,67,89,7,5,32,2,2,3,5,6,4,2,2,3,45,6]

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = (quicksort (filter (< x) xs)) ++ [x] ++ (quicksort (filter (>= x) xs))
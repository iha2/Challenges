
data Tree a = Empty | Tree a (Tree a) (Tree a) deriving Show

balancedSampleTree = Tree 5 (Tree 4 Empty Empty) (Tree 5 (Tree 5 (Tree 4 Empty Empty) (Tree 5 Empty (Tree 7 Empty Empty))) (Tree 7 Empty Empty))
subTree1 = (Tree 5 Empty (Tree 7 Empty Empty))
unbalancedSampleTree = Tree 5 (Tree 4 Empty Empty) (Tree 5 Empty (Tree 7 Empty Empty))

maxT :: (Ord a, Num a) => (a,a) -> a
maxT (x,y) = max x y

minT :: (Ord a, Num a) => (a,a) -> a
minT (x,y) = min x y

tplus :: (Num a) => (a,a) -> (a,a)
tplus (x,y) = (x+1, y+1) 

getLengths :: (Ord a, Num a) => Tree a -> (a,a) 
getLengths (Tree x Empty Empty) = (1,1)
getLengths (Tree x y     Empty) = (maxT (tplus (getLengths y)), 1)
getLengths (Tree x Empty z    ) = (1, maxT (tplus (getLengths z)))
getLengths (Tree x y     z    ) = (maxT (tplus (getLengths y)), maxT (tplus (getLengths z)))


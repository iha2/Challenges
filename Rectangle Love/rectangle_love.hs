

data Point     = Point Int Int deriving Show  
data Rectangle = Rectangle Point Point Point Point deriving Show

firstRectangle = Rectangle (Point 12 34) (Point 22 34) (Point 12 12) (Point 22 12)

secondRectangle = Rectangle (Point 19 44) (Point 32 34) (Point 19 24) (Point 32 24)

withinRange :: (Ord a, Num a) => a -> a -> a -> Bool
withinRange p1 p1' p2 = let mx = max p1 p1'
                            mn = min p1 p1'
                        in if (mx - mn) > (mx - p2)
                            then True
                            else False

checkRectangles :: Rectangle -> Rectangle -> Bool
checkRectangles (Rectangle (Point x1 y1) _ (Point x3 y3) _) (Rectangle (Point x1' y1') _ (Point x3' y3') _) = let xs_nested = withinRange x1 x1' x3
                                    ys_nested = withinRange y1 y1' y3
                                in if xs_nested == ys_nested
                                   then True
                                   else False

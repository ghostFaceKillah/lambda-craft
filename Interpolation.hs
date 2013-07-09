interpM :: [[Float]] -> [[Float]]
interpM x = interpCols ( interpRows x)

interpRow :: [[Float]] -> [[Float]]
interpRow x = [ interpolate a|a<-x]

interpCol :: [[Float]] -> [[Float]]
interpCol [] = []
interpCol (x:[]) = [x]
interpCol x = [head x] ++ [zipWith ((\ a b -> (a+b)/2)) (head x) (head (tail x))] ++ interpCols( tail x)

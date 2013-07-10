interpolate :: [Float] -> [Float]
interpolate (x:y:[]) = [x, (x+y)/2, y]
interpolate (x:y:xs) = [x, (x+y)/2] ++ interpolate (y:xs)
interpolate x = x

interpM :: [[Float]] -> [[Float]]
interpM x = interpCol ( interpRow x)

interpRow :: [[Float]] -> [[Float]]
interpRow x = [ interpolate a|a<-x]

interpCol :: [[Float]] -> [[Float]]
interpCol [] = []
interpCol (x:[]) = [x]
interpCol x = [head x] ++ [zipWith ((\ a b -> (a+b)/2)) (head x) (head (tail x))] ++ interpCol( tail x)

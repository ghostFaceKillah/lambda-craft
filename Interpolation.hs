import Control.Monad
import Control.Monad.Random
import System.Random

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

getRandom22Matrix :: Rand StdGen [[Float]]
getRandom22Matrix = do
  one <- getRandomR (-10,10)
  two <- getRandomR (-10,10)
  three <- getRandomR (-10,10)
  four <- getRandomR (-10,10)
  return [[one,two],[three,four]]

getRandomList :: Rand StdGen [Float]
getRandomList = do
  one <- getRandomR (-10,10)
  two <- getRandomR (-10,10)
  three <- getRandomR (-10,10)
  return [one,two,three]
  
-- interpolateRandomMatrix :: Rand StdGen [[Float]] -> Rand StdGen [[Float]]
-- interpolateRandomMatrix x = liftM (interpM) x

interpolateRandRows :: Rand StdGen [Float] -> Rand StdGen [Float]
interpolateRandRows x = do
  y <- x
  rand <-getRandomR (0,10.0)
  let resu = test y where test (a:b:[]) = [a, (a+b)/2 + rand, b]
                          test (a:b:xs) = [a,(a+b)/2 + rand] ++ interpolateRandRows (y:xs)
                          test a = a
  return resu
 


main = do
  x <- evalRandIO $ interpolateRandRows $ getRandomList
  print x

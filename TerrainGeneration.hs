module TerrainGeneration where

import Control.Monad
import Control.Monad.Random
import System.Random
import GHC.Float

cons = 0.4
cons2 = 6.0

getRandom22Matrix :: Rand StdGen [[Float]]
getRandom22Matrix = do
  one <- getRandomR (-cons2,cons2)
  two <- getRandomR (-cons2,cons2)
  three <- getRandomR (-cons2,cons2)
  four <- getRandomR (-cons2,cons2)
  return [[one,two],[three,four]]

interpolateRandRows :: [Float] -> Rand StdGen [Float]
interpolateRandRows [] = return []
interpolateRandRows a = do
  remain <- interpolateRandRows(tail a)
  rand <-getRandomR (-cons,cons)
  let resu = test a where test (x:y:[]) = [x,(x+y)/2 + rand, y]
                          test (x:y:xs) = [x,(x+y)/2 + rand] ++ remain
                          test x = x
  return resu

interpolateRandomCol :: [[Float]] -> Rand StdGen [[Float]]
interpolateRandomCol x = sequence2 ( [interpolateRandRows a | a<-x] )

sequence2 (x:x2) = liftM2 (:) x $ sequence2 x2
sequence2 [] = return []

interpolateRandomCol2 :: [[Float]] -> Rand StdGen [[Float]]
interpolateRandomCol2 [] = return []
interpolateRandomCol2 [x] = return [x]
interpolateRandomCol2 x = do
  remain <- interpolateRandomCol2( tail x)
  between <- addRandomToCol (zipWith ((\ a b -> (a+b)/2)) (head x)  (head (tail x)))
  let listBetween = [between]
  let resu = [head x] ++ listBetween ++ remain
  return resu
  
addRandomToCol :: [Float] -> Rand StdGen [Float]
addRandomToCol x = sequence2 ( [addRandomToNum a | a <- x] )

addRandomToNum :: Float -> Rand StdGen Float
addRandomToNum x = do
  y <- getRandomR (-cons,cons)
  return (x+y)
  
interpolateRandomMatrix :: [[Float]] -> Rand StdGen [[Float]]
interpolateRandomMatrix x =  (interpolateRandomCol x) >>= interpolateRandomCol2

roundRandomMatrix :: [[Float]] -> Rand StdGen [[Int]]
roundRandomMatrix x = return [ [ round z  | z<-y ]  | y <- x ]

turn :: [[Int]] -> [(Int,Int,Int)]
turn x =  turn2 (turn1 x)

turn1 :: [[Int]] -> [(Int,[Int])]
turn1 x = zip [1..] x

turn2 :: [(Int,[Int])] -> [(Int,Int,Int)]
turn2 [] = []
turn2 (x:xs) = turn3 x ++ turn2 xs

turn3 :: (Int, [Int]) -> [(Int,Int,Int)]
turn3 x = [ ((fst x),a,b) | (a,b) <- ( zip (snd x) [1..])]

turnIntoDoubles :: [(Int,Int,Int)] -> [(Double,Double,Double)]
turnIntoDoubles x = [(int2Double a,int2Double b,int2Double c) | (a,b,c) <-x]

getMatrix :: IO [(Double,Double,Double)]
getMatrix = do
  x <- evalRandIO $  getRandom22Matrix >>=  interpolateRandomMatrix >>= interpolateRandomMatrix >>= interpolateRandomMatrix >>= interpolateRandomMatrix >>= interpolateRandomMatrix >>= roundRandomMatrix
  let y = turnIntoDoubles( turn x)
  return y

module TerrainGeneration where

import Control.Monad
import Control.Monad.Random
import System.Random
import GHC.Float
import Data.Array.Unboxed
import Data.Int

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
  let resu = work a where work (x:y:[]) = [x,(x+y)/2 + rand, y]
                          work (x:y:xs) = [x,(x+y)/2 + rand] ++ remain
                          work x = x
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

ranger = 10

bottom :: Int
bottom = -10

work :: Int -> Int -> Int
work a b 
  | a >= b = 1
  | otherwise = 0

turnTerrainContourIntoNodeList :: [[Int]] -> [[[Int]]]
turnTerrainContourIntoNodeList x = [[[ work b c | c <-[(-ranger)..ranger]] |  b <- a]  | a <- x ]

fromNodesListIntoTriplesList :: [[[Int]]] -> [((Int, Int, Int), Int8) ]
fromNodesListIntoTriplesList inp =  [((x,y,z), fromIntegral c) | (x,a) <- zip [bottom, bottom + 1 ..] inp, (z, b) <- zip  [bottom, bottom + 1 ..] a, (y,c) <- zip [bottom, bottom + 1 ..] b]

turnTerrainContourIntoTerrainMatrix :: [[Int]] -> UArray (Int, Int, Int) Int8
turnTerrainContourIntoTerrainMatrix x = array ((bottom,bottom,bottom),(-bottom,-bottom,-bottom)) (fromNodesListIntoTriplesList $ turnTerrainContourIntoNodeList x)

fromTerrainMatrixIntoRenderableList :: UArray (Int, Int, Int) Int8 -> [(Double, Double, Double)]
fromTerrainMatrixIntoRenderableList inp = [(fromIntegral x,fromIntegral y,fromIntegral z) | x <- [(-ranger)..ranger], y <- [(-ranger)..ranger], z <- [(-ranger)..ranger], inp!(x,y,z) == 1]

getMatrix :: IO (UArray (Int, Int, Int) Int8)
getMatrix = do
  x <- evalRandIO $  getRandom22Matrix >>= interpolateRandomMatrix >>= interpolateRandomMatrix >>= interpolateRandomMatrix >>= roundRandomMatrix
  return $ turnTerrainContourIntoTerrainMatrix x

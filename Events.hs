module Events where

import Control.Lens
import Control.Monad
import Control.Monad.State
import Graphics.Rendering.OpenGL (($=))
import Graphics.UI.GLFW as GLFW
import Linear as L
import qualified Graphics.Rendering.OpenGL as GL
import Data.Array.Unboxed
import Data.Int

import GameState

setPolygonMode :: Bool -> IO ()
setPolygonMode flag = GL.polygonMode $= (if flag then (GL.Line, GL.Line) else (GL.Fill, GL.Fill))

processSpace :: GameMonad ()
processSpace = do
  space <- liftIO $ GLFW.getKey ' '
  pressed <- use wasPressed

  if space == GLFW.Press then
    unless pressed $ do
      wasPressed .= True
      stateLine %= not
      liftIO . setPolygonMode =<< use stateLine
  else when pressed $ wasPressed .= False

processEscape :: GameMonad ()
processEscape = do
  p <- liftIO $ GLFW.getKey GLFW.ESC
  when (p == GLFW.Press) $ shouldExit .= True

processKey :: Char -> GameMonad () -> GameMonad ()
processKey key action = (liftM (==GLFW.Press) $ liftIO $ GLFW.getKey key) >>= flip when action

stateCombinator :: [GameMonad a] -> GameMonad ()
stateCombinator = foldr (>>) $ return ()

processMove :: GameMonad ()
processMove = do
  crx    <- liftM2 L.cross (use direction) (use up)
  dirvec <- use direction

  let moveSpeed = 0.2

  let processA = processKey 'A' $ pos -= moveSpeed * crx
  let processD = processKey 'D' $ pos += moveSpeed * crx
  let processW = processKey 'W' $ pos += moveSpeed * dirvec
  let processS = processKey 'S' $ pos -= moveSpeed * dirvec

  stateCombinator [processA, processD, processW, processS]

inWhichCube :: L.V3 Double -> (Int, Int, Int)
inWhichCube x = (truncate ( coord1 x), truncate (coord2 x), truncate (coord3 x))

whatCubeIsUnderUs :: L.V3 Double -> (Int, Int, Int)
whatCubeIsUnderUs x = (truncate ( coord1 x), truncate (coord2 x)  - 1, truncate (coord3 x))

type Position =  L.V3 Double
type Terrain =  UArray (Int, Int, Int) Int8

detectCollisionWithEarth :: Position -> Terrain -> Double
detectCollisionWithEarth pos ter 
                         | ter!(inWhichCube pos) == 1 = 0.5 + ( fromIntegral $ ( \(_,a,_) -> a ) $ inWhichCube pos )
                         | ter!(whatCubeIsUnderUs pos) == 1 = 0.5 + ( fromIntegral $ ( \(_,a,_) -> a ) $ whatCubeIsUnderUs pos )
                         | otherwise = -hellDepth where hellDepth = 1000.0


humanHeight = 0.5

applyGravity :: GameMonad ()
applyGravity = do
  position <- use pos
  terrainMatrix <- use terrain
  g <- use gravity
  v <- use speed
  let newspeed = v + g
      newpos = position + v
  liftIO $ print $ inWhichCube position 
  let bound = detectCollisionWithEarth position terrainMatrix 
  if (coord2 newpos <= bound) then do speed .= L.V3 0 0 0 
                                      pos .= L.V3 (coord1 position) (bound) (coord3 position)
                                      liftIO $ print "on surface"
                              else do speed .= newspeed
                                      pos .= newpos
                                      liftIO $ print "gravity working"


processMouse :: GameMonad ()
processMouse = do
  (GL.Position x y) <- liftIO $  GL.get GLFW.mousePos
  let x1 = fromIntegral x
      y1 = fromIntegral y
      howMuchHorizontally = (x1 - 200)/300
      howMuchVertically = (y1 - 200)/300

  dir <- use direction
  upward <- use up
  -- direction %= rotateXZ(howMuchHorizontally)
  direction %= rotateAroundAxis2 (L.V3 0 1 0) (-howMuchHorizontally)
  up %= rotateAroundAxis2 (L.V3 0 1 0) (-howMuchHorizontally)
  direction %= rotateAroundAxis2  ( vectorProduct dir upward ) (-howMuchVertically)
  up %= rotateAroundAxis2 ( vectorProduct dir upward ) (-howMuchVertically)
  dir2 <- use direction 
  liftIO $ print dir2
  liftIO $ print upward
  liftIO $ print (vectorProduct dir upward)
  liftIO $ GLFW.mousePos $= (GL.Position 200 200)

processRotation :: GameMonad ()
processRotation = stateCombinator [processQ, processE]
  where phi = pi/140
        processQ = processKey 'Q' $ direction %= rotateXZ (-phi)
        processE = processKey 'E' $ direction %= rotateXZ phi

processEvents :: GameMonad ()
processEvents = stateCombinator [processEscape, processSpace, processMove, processRotation, processMouse, applyGravity]

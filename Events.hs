module Events where

import Control.Lens
import Control.Monad
import Control.Monad.State
import Graphics.Rendering.OpenGL (($=))
import Graphics.UI.GLFW as GLFW
import Linear as L
import qualified Graphics.Rendering.OpenGL as GL

import GameState

setPolygonMode :: Bool -> IO ()
setPolygonMode flag = GL.polygonMode $= (if flag then (GL.Line, GL.Line) else (GL.Fill, GL.Fill))

processSpace :: GameMonad ()
processSpace = do
  space <- liftIO $ GLFW.getKey ' '
  pressed <- use wasPressed

  if space == GLFW.Press then do
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

  let moveSpeed = 0.05

  let processA = processKey 'A' $ pos -= moveSpeed * crx
  let processD = processKey 'D' $ pos += moveSpeed * crx
  let processW = processKey 'W' $ pos += moveSpeed * dirvec
  let processS = processKey 'S' $ pos -= moveSpeed * dirvec

  stateCombinator [processA, processD, processW, processS]

processRotation :: GameMonad ()
processRotation = stateCombinator [processQ, processE]
  where phi = pi/360
        processQ = processKey 'Q' $ direction %= rotateXZ (-phi)
        processE = processKey 'E' $ direction %= rotateXZ phi


processEvents :: GameMonad ()
processEvents = stateCombinator [processEscape, processSpace, processMove, processRotation]

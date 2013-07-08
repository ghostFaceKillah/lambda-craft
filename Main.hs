module Main where

-- Global imports
import Control.Lens
import Control.Monad.State
import Control.Monad
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL.GLU as GLU
import Graphics.UI.GLFW as GLFW
import Linear as L

-- Local imports
import GameState

main = do
  GLFW.initialize

  GLFW.openWindow (GL.Size 800 600) [GLFW.DisplayAlphaBits 8] GLFW.Window
  GLFW.windowTitle $= "Lambda-Craft"
  GL.shadeModel    $= GL.Smooth

  GL.lineSmooth $= GL.Enabled
  GL.clearColor $= GL.Color4 0.53 0.57 0.75 0
 
  GLFW.windowSizeCallback $= \ size@(GL.Size w h) -> do
      GL.viewport   $= (GL.Position 0 0, size)
      GL.matrixMode $= GL.Projection
      GL.loadIdentity
      GLU.perspective 90.0 ((fromIntegral w)/(fromIntegral h)) 0.1 100
      GL.matrixMode $= GL.Modelview 0

  runStateT mainLoop initialState

  GLFW.closeWindow
  GLFW.terminate

mainLoop :: GameMonad ()
mainLoop = do
  render
  liftIO GLFW.swapBuffers

  processEvents
  exit <- use shouldExit
  unless exit mainLoop

render :: GameMonad ()
render = do
  state <- get
  liftIO $ do
    GL.clear [GL.ColorBuffer]
    GL.loadIdentity
    -- This is a mongolian part to be changed later
    GLU.lookAt (toVert3 $ _pos state) (toVert3 $ center state) (toVec3 $ _up state)

    GL.renderPrimitive GL.Triangles $ do
      GL.color  $ color3 1 0 0
      GL.vertex $ vertex3 0 3 0
      GL.vertex $ vertex3 0 0 0
      GL.vertex $ vertex3 3 0 0
    
      GL.vertex $ vertex3 0 3 0
      GL.vertex $ vertex3 3 0 0
      GL.vertex $ vertex3 3 3 0

      GL.vertex $ vertex3 0 3 3
      GL.vertex $ vertex3 0 0 3
      GL.vertex $ vertex3 3 0 3
    
      GL.vertex $ vertex3 0 3 3
      GL.vertex $ vertex3 3 0 3
      GL.vertex $ vertex3 3 3 3

      GL.vertex $ vertex3 3 0 0
      GL.vertex $ vertex3 3 0 3
      GL.vertex $ vertex3 3 3 3

      GL.vertex $ vertex3 3 0 0
      GL.vertex $ vertex3 3 3 0
      GL.vertex $ vertex3 3 3 3

      GL.vertex $ vertex3 0 0 0
      GL.vertex $ vertex3 0 0 3
      GL.vertex $ vertex3 0 3 3

      GL.vertex $ vertex3 0 0 0
      GL.vertex $ vertex3 0 3 0
      GL.vertex $ vertex3 0 3 3

      GL.vertex $ vertex3 0 3 0
      GL.vertex $ vertex3 3 3 0
      GL.vertex $ vertex3 0 3 3

      GL.vertex $ vertex3 3 3 3
      GL.vertex $ vertex3 3 3 0
      GL.vertex $ vertex3 0 3 3

      GL.vertex $ vertex3 0 0 0
      GL.vertex $ vertex3 3 0 0
      GL.vertex $ vertex3 0 0 3

      GL.vertex $ vertex3 3 0 3
      GL.vertex $ vertex3 3 0 0
      GL.vertex $ vertex3 0 0 3

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
  dirvec <- use direction
  upvec <- use up
  let crx = L.cross dirvec upvec
  let moveSpeed = 0.05
  let processA = processKey 'A' $ pos -= moveSpeed * crx
  let processD = processKey 'D' $ pos += moveSpeed * crx
  let processW = processKey 'W' $ pos += moveSpeed * dirvec
  let processS = processKey 'S' $ pos -= moveSpeed * dirvec
  stateCombinator [processA, processD, processW, processS]

processRotation :: GameMonad ()
processRotation = stateCombinator [processQ, processE]
  where phi = pi/360
        processQ = processKey 'Q' $ direction %= rotateXZ phi
        processE = processKey 'E' $ direction %= rotateXZ (-phi)

processEvents :: GameMonad ()
processEvents = stateCombinator [processEscape, processSpace, processMove, processRotation]

vertex3 :: GL.GLfloat -> GL.GLfloat -> GL.GLfloat -> GL.Vertex3 GL.GLfloat
vertex3 = GL.Vertex3

color3 :: GL.GLfloat -> GL.GLfloat -> GL.GLfloat -> GL.Color3 GL.GLfloat
color3 = GL.Color3

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
  --if exit then return () else mainLoop

render :: GameMonad ()
render = do
  state <- get
  liftIO $ do
    GL.clear [GL.ColorBuffer]
    GL.loadIdentity
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

--processSpace :: GameState -> IO GameState
--processSpace state = do
--  space <- GLFW.getKey ' '
--  if space == GLFW.Press then do
--    if wasPressed state then return state else do
--      let newState = state { wasPressed = True, stateLine = not (stateLine state) }
--      setPolygonMode $ stateLine newState
--      return newState
--  else if wasPressed state then return $ state { wasPressed = False } else return state

processEscape :: GameMonad ()
processEscape = do
  p <- liftIO $ GLFW.getKey GLFW.ESC
  if p == GLFW.Press then shouldExit .= True else return ()

--processKey :: Char -> (GameState -> GameState) -> GameState -> IO GameState
--processKey key action state = do
--  val <- GLFW.getKey key
--  if val == GLFW.Press then return $ action state else return state

--stateCombinator :: [GameState -> IO GameState] -> GameState -> IO GameState
--stateCombinator = flip $ foldr (=<<) . return

stateCombinator :: [GameMonad a] -> GameMonad ()
stateCombinator = foldr (>>) $ return ()

--processMove :: GameState -> IO GameState
--processMove state = stateCombinator [processA, processD, processW, processS] state
--  where crx = L.cross (direction state) (up state)
--        moveSpeed = 0.05
--        processA = processKey 'A' (\s -> s { pos = (pos s) + (-moveSpeed) * crx })
--        processD = processKey 'D' (\s -> s { pos = (pos s) + moveSpeed * crx })
--        processW = processKey 'W' (\s -> s { pos = (pos s) + moveSpeed * (direction state) })
--        processS = processKey 'S' (\s -> s { pos = (pos s) + (-moveSpeed) * (direction state) })
--
--processRotation :: GameState -> IO GameState
--processRotation = stateCombinator [processQ, processE]
--  where phi = pi/360
--        processQ = processKey 'Q' (\s -> s { direction = rotateXZ phi $ direction s })
--        processE = processKey 'E' (\s -> s { direction = rotateXZ (-phi) $ direction s }) 

processEvents :: GameMonad ()
processEvents = stateCombinator [processEscape]

vertex3 :: GL.GLfloat -> GL.GLfloat -> GL.GLfloat -> GL.Vertex3 GL.GLfloat
vertex3 = GL.Vertex3

color3 :: GL.GLfloat -> GL.GLfloat -> GL.GLfloat -> GL.Color3 GL.GLfloat
color3 = GL.Color3

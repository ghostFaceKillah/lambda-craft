import Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL.GLU as GLU
import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL (($=))
import Data.IORef
import Control.Monad
import System.Environment (getArgs, getProgName)

data State = State {
  stateLine :: Bool,
  wasPressed :: Bool,
  shouldExit :: Bool
}

initialState :: State
initialState = State {
  stateLine  = False,
  wasPressed = False,
  shouldExit = False
}

main = do
  GLFW.initialize
  -- open window
  GLFW.openWindow (GL.Size 800 600) [GLFW.DisplayAlphaBits 8] GLFW.Window
  GLFW.windowTitle $= "Lambda-Craft"
  GL.shadeModel    $= GL.Smooth
  -- enable antialiasing
  GL.lineSmooth $= GL.Enabled
  GL.clearColor $= Color4 0.53 0.57 0.75 0
 
  -- set 2D orthogonal view inside windowSizeCallback because
  -- any change to the Window size should result in different
  -- OpenGL Viewport.
  GLFW.windowSizeCallback $= \ size@(GL.Size w h) -> do
      GL.viewport   $= (GL.Position 0 0, size)
      GL.matrixMode $= GL.Projection
      GL.loadIdentity
      GLU.perspective 90.0 ((fromIntegral w)/(fromIntegral h)) 0.1 100

  -- run the main loop
  -- active lines
  mainLoop initialState

  -- finish up
  GLFW.closeWindow
  GLFW.terminate

mainLoop :: State -> IO State
mainLoop state = do
  GL.clear [GL.ColorBuffer]
  GL.renderPrimitive GL.Triangles $ do
    GL.color  $ color3 1 0 0
    GL.vertex $ vertex3 0.0 3.0 (-10)
    GL.vertex $ vertex3 0.0 0.0 (-10)
    GL.vertex $ vertex3 3.0 0.0 (-10)
  
    GL.vertex $ vertex3 0.0 3.0 (-10)
    GL.vertex $ vertex3 3.0 0.0 (-10)
    GL.vertex $ vertex3 3.0 3.0 (-10)

  GLFW.swapBuffers

  processEvents state

setPolygonMode :: Bool -> IO ()
setPolygonMode flag = GL.polygonMode $= (if flag then (GL.Line, GL.Line) else (GL.Fill, GL.Fill))

processSpace :: State -> IO State
processSpace state = do
  space <- GLFW.getKey ' '
  if space == GLFW.Press then do
    if wasPressed state then return state else do
      let newState = state { wasPressed = True, stateLine = not (stateLine state) }
      setPolygonMode $ stateLine newState
      return newState
  else if wasPressed state then return $ state { wasPressed = False } else return state

processEscape :: State -> IO State
processEscape state = do
  p <- GLFW.getKey GLFW.ESC
  if p == GLFW.Press then return $ state { shouldExit = True} else return state

processEvents :: State -> IO State
processEvents state = do
  newState <- processEscape =<< processSpace state
  
  if shouldExit newState then return newState else mainLoop newState

vertex3 :: GLfloat -> GLfloat -> GLfloat -> GL.Vertex3 GLfloat
vertex3 = GL.Vertex3
 
color3 :: GLfloat -> GLfloat -> GLfloat -> GL.Color3 GLfloat
color3 = GL.Color3

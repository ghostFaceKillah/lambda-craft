module Main where

-- Global imports
import Graphics.Rendering.OpenGL (($=))
import Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL.GLU as GLU
import Graphics.UI.GLFW as GLFW
import Linear as L

-- Local imports
import State

main = do
  GLFW.initialize

  GLFW.openWindow (GL.Size 800 600) [GLFW.DisplayAlphaBits 8] GLFW.Window
  GLFW.windowTitle $= "Lambda-Craft"
  GL.shadeModel    $= GL.Smooth

  GL.lineSmooth $= GL.Enabled
  GL.clearColor $= Color4 0.53 0.57 0.75 0
 
  GLFW.windowSizeCallback $= \ size@(GL.Size w h) -> do
      GL.viewport   $= (GL.Position 0 0, size)
      GL.matrixMode $= GL.Projection
      GL.loadIdentity
      GLU.perspective 90.0 ((fromIntegral w)/(fromIntegral h)) 0.1 100
      GL.matrixMode $= GL.Modelview 0

  mainLoop initialState

  GLFW.closeWindow
  GLFW.terminate

mainLoop :: State -> IO State
mainLoop state = do
  render state
  GLFW.swapBuffers

  newState <- processEvents state
  if shouldExit newState then return newState else mainLoop newState

render :: State -> IO ()
render state = do
  GL.clear [GL.ColorBuffer]
  GL.loadIdentity
  GLU.lookAt (toVert3 $ pos state) (toVert3 $ center state) (toVec3 $ up state)
  GL.renderPrimitive GL.Triangles $ do
    GL.color  $ color3 1 0 0
    GL.vertex $ vertex3 0.0 3.0 0
    GL.vertex $ vertex3 0.0 0.0 0
    GL.vertex $ vertex3 3.0 0.0 0
  
    GL.vertex $ vertex3 0.0 3.0 0
    GL.vertex $ vertex3 3.0 0.0 0
    GL.vertex $ vertex3 3.0 3.0 0

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
  if p == GLFW.Press then return $ state { shouldExit = True } else return state

processMove :: State -> IO State
processMove state = do
  a <- GLFW.getKey 'A'
  let crx = L.cross (direction state) (up state)
  if a == GLFW.Press then do
    let newState = state { pos = (pos state) + 0.05 * crx }
    return newState
  else
    return state

processEvents :: State -> IO State
processEvents state = processMove =<< processEscape =<< processSpace state

vertex3 :: GLfloat -> GLfloat -> GLfloat -> GL.Vertex3 GLfloat
vertex3 = GL.Vertex3

color3 :: GLfloat -> GLfloat -> GLfloat -> GL.Color3 GLfloat
color3 = GL.Color3

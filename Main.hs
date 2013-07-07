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

processKey :: Char -> (State -> State) -> State -> IO State
processKey key action state = do
  val <- GLFW.getKey key
  if val == GLFW.Press then return $ action state else return state

stateCombinator :: [State -> IO State] -> State -> IO State
stateCombinator = flip $ foldr (=<<) . return

processMove :: State -> IO State
processMove state = stateCombinator [processA, processD, processW, processS, processQ, processE] state
  where crx = L.cross (direction state) (up state)
        phi = pi/360
        mphi = -phi
        moveSpeed = 0.05
        processA = processKey 'A' (\s -> s { pos = (pos s) + (-moveSpeed) * crx })
        processD = processKey 'D' (\s -> s { pos = (pos s) + moveSpeed * crx })
        processW = processKey 'W' (\s -> s { pos = (pos s) + moveSpeed * (direction state) })
        processS = processKey 'S' (\s -> s { pos = (pos s) + (-moveSpeed) * (direction state) })
        processQ = processKey 'Q' (\s -> s { direction = L.V3 ((coord1 (direction state))*cos(phi) + (coord3 (direction state))*(-1)*(sin(phi)))  (coord2 (direction state)) ((coord1 (direction state))*sin(phi) + (coord3 (direction state))*(cos(phi))) }) 
        processE = processKey 'E' (\s -> s { direction = L.V3 ((coord1 (direction state))*cos(mphi) + (coord3 (direction state))*(-1)*(sin(mphi)))  (coord2 (direction state)) ((coord1 (direction state))*sin(mphi) + (coord3 (direction state))*(cos(mphi))) }) 

processEvents :: State -> IO State
processEvents state = processMove =<< processEscape =<< processSpace state

vertex3 :: GLfloat -> GLfloat -> GLfloat -> GL.Vertex3 GLfloat
vertex3 = GL.Vertex3

color3 :: GLfloat -> GLfloat -> GLfloat -> GL.Color3 GLfloat
color3 = GL.Color3

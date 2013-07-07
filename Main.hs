import Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL.GLU as GLU
import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL (($=))
import Data.IORef
import Control.Monad
import System.Environment (getArgs, getProgName)

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
  mainLoop

  -- finish up
  GLFW.closeWindow
  GLFW.terminate

mainLoop = do
  GL.clear [GL.ColorBuffer]
  GL.renderPrimitive GL.Triangles $ do
    GL.color $ color3 1 0 0
    GL.vertex $ vertex3 0.0 3.0 (-10)
    GL.vertex $ vertex3 0.0 0.0 (-10)
    GL.vertex $ vertex3 3.0 0.0 (-10)
  
    GL.color $ color3 0 1 0
    GL.vertex $ vertex3 0.0 3.0 (-10)
    GL.vertex $ vertex3 3.0 0.0 (-10)
    GL.vertex $ vertex3 3.0 3.0 (-10)

  GLFW.swapBuffers

  p <- GLFW.getKey GLFW.ESC

  if p == GLFW.Press then return () else mainLoop
  

vertex3 :: GLfloat -> GLfloat -> GLfloat -> GL.Vertex3 GLfloat
vertex3 = GL.Vertex3
 
color3 :: GLfloat -> GLfloat -> GLfloat -> GL.Color3 GLfloat
color3 = GL.Color3

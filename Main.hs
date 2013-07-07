import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL (($=))
import Data.IORef
import Control.Monad
import System.Environment (getArgs, getProgName)

data Action = Action (IO Action)

main = do
  GLFW.initialize
  -- open window
  GLFW.openWindow (GL.Size 400 400) [GLFW.DisplayAlphaBits 8] GLFW.Window
  GLFW.windowTitle $= "GLFW Demo"
  GL.shadeModel    $= GL.Smooth
  -- enable antialiasing
  GL.lineSmooth $= GL.Enabled
  GL.blend      $= GL.Enabled
  GL.blendFunc  $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  GL.lineWidth  $= 1.5
  -- set the color to clear background
  GL.clearColor $= Color4 0 0 0 0
 
  -- set 2D orthogonal view inside windowSizeCallback because
  -- any change to the Window size should result in different
  -- OpenGL Viewport.
  GLFW.windowSizeCallback $= \ size@(GL.Size w h) -> do
      GL.viewport   $= (GL.Position 0 0, size)
      GL.matrixMode $= GL.Projection
      GL.loadIdentity
      GL.ortho2D 0 (realToFrac w) (realToFrac h) 0

  -- run the main loop
  -- active lines
  mainLoop

  -- finish up
  GLFW.closeWindow
  GLFW.terminate

mainLoop = do
  GL.clear [GL.ColorBuffer]
  GL.color $ color3 1 0 0
  GL.renderPrimitive GL.Lines $ do
    GL.vertex (vertex3 (fromIntegral 10) (fromIntegral 10) 0)
    GL.vertex (vertex3 (fromIntegral 20) (fromIntegral 20) 0)

  
  GLFW.swapBuffers

  p <- GLFW.getKey GLFW.ESC

  if p == GLFW.Press then return () else mainLoop
  

vertex3 :: GLfloat -> GLfloat -> GLfloat -> GL.Vertex3 GLfloat
vertex3 = GL.Vertex3
 
color3 :: GLfloat -> GLfloat -> GLfloat -> GL.Color3 GLfloat
color3 = GL.Color3

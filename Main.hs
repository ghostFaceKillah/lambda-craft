module Main where

-- Global imports
import Control.Lens
import Control.Monad
import Control.Monad.State
import Graphics.Rendering.OpenGL (($=))
import Graphics.Rendering.OpenGL.GLU as GLU
import Graphics.UI.GLFW as GLFW
import Linear as L
import qualified Graphics.Rendering.OpenGL as GL

-- Local imports
import Cube
import Events
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

setCamera :: GameMonad ()
setCamera = do
  eye <- use pos
  dir <- use direction
  updir <- use up
  liftIO $ GLU.lookAt (toVert3 eye) (toVert3 $ eye + dir) (toVec3 updir)

render :: GameMonad ()
render = do
  liftIO $ GL.clear [GL.ColorBuffer]
  liftIO GL.loadIdentity
  setCamera

  liftIO $ GL.renderPrimitive GL.Triangles $ do
    GL.color $ color3 1 0 0
    let terrainMatrix = [(a,b,c)| [a,b,c] <- (replicateM 3 [-10..10]), c+b <= 2, a+b <= -3, 0.5*a-b >= 5,   b < 0, b >= -10  ]
    mapM_ renderCube [Cube (L.V3 a b c) | (a,b,c) <- terrainMatrix ]

vertex3 :: GL.GLfloat -> GL.GLfloat -> GL.GLfloat -> GL.Vertex3 GL.GLfloat
vertex3 = GL.Vertex3

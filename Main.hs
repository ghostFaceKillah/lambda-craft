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
import TerrainGeneration

main = do
  GLFW.initialize
  GLFW.openWindow (GL.Size 800 600) [GLFW.DisplayAlphaBits 8] GLFW.Window
  GLFW.windowTitle $= "Lambda-Craft"
  runStateT game initialState
  GLFW.closeWindow
  GLFW.terminate

game :: GameMonad ()
game = do
  initialization
  mainLoop

resizeCallback :: GL.Size -> IO ()
resizeCallback size@(GL.Size w h) = do
  GL.viewport   $= (GL.Position 0 0, size)
  GL.matrixMode $= GL.Projection
  GL.loadIdentity
  GLU.perspective 90.0 ((fromIntegral w)/(fromIntegral h)) 0.1 100
  GL.matrixMode $= GL.Modelview 0

initialization :: GameMonad ()
initialization = do
  liftIO $ do
    GL.lineSmooth $= GL.Enabled
    GL.clearColor $= GL.Color4 0.53 0.57 0.75 0
    GL.shadeModel $= GL.Smooth
    GLFW.windowSizeCallback $= resizeCallback

  terrainMatrix <- liftIO getMatrix
  terrain .= terrainMatrix

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
  liftIO $ GLU.lookAt (toVert3 $ eye ) (toVert3 $ eye + dir ) (toVec3 updir)

render :: GameMonad ()
render = do
  liftIO $ GL.clear [GL.ColorBuffer]
  liftIO GL.loadIdentity
  setCamera
  terrainMatrix <- use terrain
  liftIO $ GL.renderPrimitive GL.Triangles $ do
    GL.color $ color3 1 0 0
    mapM_ renderCube [Cube (L.V3 a b c) | (a,b,c) <- (fromTerrainMatrixIntoRenderableList terrainMatrix) ]

vertex3 :: GL.GLfloat -> GL.GLfloat -> GL.GLfloat -> GL.Vertex3 GL.GLfloat
vertex3 = GL.Vertex3

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
  GL.shadeModel    $= GL.Smooth

  GL.lineSmooth $= GL.Enabled
  GL.clearColor $= GL.Color4 0.53 0.57 0.75 0
 
  GLFW.windowSizeCallback $= \ size@(GL.Size w h) -> do
      GL.viewport   $= (GL.Position 0 0, size)
      GL.matrixMode $= GL.Projection
      GL.loadIdentity
      GLU.perspective 90.0 ((fromIntegral w)/(fromIntegral h)) 0.1 100
      GL.matrixMode $= GL.Modelview 0

  runStateT game initialState

  GLFW.closeWindow
  GLFW.terminate

game :: GameMonad ()
game = do
  initialization
  mainLoop

initialization :: GameMonad ()
initialization = do
  terrainMatrix <- liftIO getMatrix
  terrain .= terrainMatrix
  return ()

mainLoop :: GameMonad ()
mainLoop = do
  render
  liftIO GLFW.swapBuffers

  processEvents
  exit <- use shouldExit
  unless exit mainLoop

setCamera :: GameMonad ()
setCamera = do
  g <- use gravity
  v <- use speed
  eye <- use pos
  dir <- use direction
  updir <- use up
  feetOnGround <- use onGround
  when (not feetOnGround) $ do speed .= v + g
                               pos .= eye + v
  liftIO $ GLU.lookAt (toVert3 eye) (toVert3 $ eye + dir) (toVec3 updir)

detectCollisionWithEarth :: GameMonad ()
detectCollisionWithEarth = do
  eye <- use pos
  ter <- use terrain
  if (detectCollision eye ter) then onGround .= True
                               else onGround .= False
  return ()

detectCollision :: L.V3 Double -> [(Double,Double,Double)] -> Bool
detectCollision (L.V3 a b c) d = not ( null [(x,y,z)| (x,y,z) <- d, abs(a-x) < 0.5, abs(b-y) <0.5, abs(c-z)<0.5 ] )


render :: GameMonad ()
render = do
  liftIO $ GL.clear [GL.ColorBuffer]
  liftIO GL.loadIdentity
  detectCollisionWithEarth
  setCamera

  terrainMatrix <- use terrain

  liftIO $ GL.renderPrimitive GL.Triangles $ do
    GL.color $ color3 1 0 0
    mapM_ renderCube [Cube (L.V3 a b c) | (a,b,c) <- terrainMatrix ]

vertex3 :: GL.GLfloat -> GL.GLfloat -> GL.GLfloat -> GL.Vertex3 GL.GLfloat
vertex3 = GL.Vertex3

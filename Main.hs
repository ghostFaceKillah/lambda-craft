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
  g <- use gravity
  v <- use speed
  eye <- use pos
  dir <- use direction
  updir <- use up
  feetOnGround <- use onGround
  let humanHeight = L.V3 0 1.5 0
  when (not feetOnGround) $ do speed .= v + g
                               pos .= eye + v
  when feetOnGround $ do speed .= L.V3 0 0 0
  liftIO $ GLU.lookAt (toVert3 $ eye+ humanHeight) (toVert3 $ eye + dir + humanHeight) (toVec3 updir)

detectCollisionWithEarth :: GameMonad ()
detectCollisionWithEarth = do
  eye <- use pos
  ter <- use terrain
  let x = fromMatrixIntoTriplesList ter
  if (detectCollision eye x) then onGround .= True
                               else onGround .= False
  return ()

-- move this to events.hs
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
    mapM_ renderCube [Cube (L.V3 a b c) | (a,b,c) <- (fromMatrixIntoTriplesList terrainMatrix) ]

vertex3 :: GL.GLfloat -> GL.GLfloat -> GL.GLfloat -> GL.Vertex3 GL.GLfloat
vertex3 = GL.Vertex3

module Cube where

import Control.Lens
import Foreign.C.Types
import Linear as L
import qualified Graphics.Rendering.OpenGL as GL

data FaceDirection = XP | XM | YP | YM | ZP | ZM deriving (Show, Enum, Bounded)

type CubeCoord = L.V3 Double

data Face = Face CubeCoord FaceDirection deriving Show

data Cube = Cube CubeCoord deriving Show

allDirections :: [FaceDirection]
allDirections = [minBound .. maxBound]

refineCube :: Cube -> [Face]
refineCube (Cube coord) = map (Face coord) allDirections

renderFace :: Face -> IO ()
renderFace (Face coord ZM) = do
  GL.vertex $ vertex3d (coord^._x-0.5) (coord^._y+0.5) (coord^._z - 0.5)
  GL.vertex $ vertex3d (coord^._x+0.5) (coord^._y+0.5) (coord^._z - 0.5)
  GL.vertex $ vertex3d (coord^._x-0.5) (coord^._y-0.5) (coord^._z - 0.5)

  GL.vertex $ vertex3d (coord^._x-0.5) (coord^._y-0.5) (coord^._z - 0.5)
  GL.vertex $ vertex3d (coord^._x+0.5) (coord^._y+0.5) (coord^._z - 0.5)
  GL.vertex $ vertex3d (coord^._x+0.5) (coord^._y-0.5) (coord^._z - 0.5)

vertex3 :: GL.GLfloat -> GL.GLfloat -> GL.GLfloat -> GL.Vertex3 GL.GLfloat
vertex3 = GL.Vertex3

vertex3d :: Double -> Double -> Double -> GL.Vertex3 GL.GLdouble
vertex3d x y z = GL.Vertex3 (CDouble x) (CDouble y) (CDouble z)

color3 :: GL.GLfloat -> GL.GLfloat -> GL.GLfloat -> GL.Color3 GL.GLfloat
color3 = GL.Color3

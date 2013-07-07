module State where

import Graphics.Rendering.OpenGL as GL
import Foreign.C.Types
import Linear as L

data State = State {
  stateLine  :: Bool,
  wasPressed :: Bool,
  shouldExit :: Bool,
  pos        :: L.V3 Double,
  direction  :: L.V3 Double,
  up         :: L.V3 Double
}

center :: State -> L.V3 Double
center state = (pos state) + (direction state)

initialState :: State
initialState = State {
  stateLine  = False,
  wasPressed = False,
  shouldExit = False,
  pos        = L.V3 0 0 10,
  direction  = L.V3 0 0 (-1),
  up         = L.V3 0 1 0
}

convDouble :: Foreign.C.Types.CDouble -> Double
convDouble (Foreign.C.Types.CDouble x) = x

toVert3 :: L.V3 Double -> GL.Vertex3 GLdouble
toVert3 (L.V3 x y z) = GL.Vertex3 (CDouble x) (CDouble y) (CDouble z)

toVec3 :: L.V3 Double -> GL.Vector3 GLdouble
toVec3 (L.V3 x y z) = GL.Vector3 (CDouble x) (CDouble y) (CDouble z)

coord1 :: L.V3 Double -> Double
coord1 (L.V3 a _ _) = a

coord2 :: L.V3 Double -> Double
coord2 (L.V3 _ a _) = a

coord3 :: L.V3 Double -> Double
coord3 (L.V3 _ _ a) = a

rotateXZ :: Double -> L.V3 Double -> L.V3 Double
rotateXZ angle (L.V3 x y z) = L.V3 newx y newz
  where newx = x * cos(angle) - z * sin(angle)
        newz = x * sin(angle) + z * cos(angle)

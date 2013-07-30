{-# LANGUAGE TemplateHaskell #-}
module GameState where

import Control.Monad.Trans.State 
import Foreign.C.Types
import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Linear as L
import Control.Lens
import Data.Array.Unboxed
import Data.Int

data GameState = GameState {
  _stateLine  :: Bool,
  _wasPressed :: Bool,
  _shouldExit :: Bool,
  _pos        :: L.V3 Double,
  _direction  :: L.V3 Double,
  _up         :: L.V3 Double,
  _terrain    :: UArray (Int, Int, Int) Int8,
  _speed      :: L.V3 Double,
  _gravity    :: L.V3 Double,
  _onGround   :: Bool
} deriving Show

makeLenses ''GameState


type GameMonad x = StateT GameState IO x

g_const = 0.025
initialState :: GameState
initialState = GameState {
  _stateLine  = False,
  _wasPressed = False,
  _shouldExit = False,
  _pos        = L.V3 (-10) 5 (-10),
  _direction  = L.V3 0 0 (-1),
  _up         = L.V3 0 1 0,
  _terrain    = array ((0,0,0),(-1,-1,-1)) [],
  _speed      = L.V3 0 0 0,
  _gravity    = L.V3 0 (-g_const) 0,
  _onGround   = False
  
}

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

vectorProduct :: L.V3 Double -> L.V3 Double -> L.V3 Double
vectorProduct (L.V3 a1 a2 a3) (L.V3 b1 b2 b3) = L.V3 (-b2*a3 + b3*a2) (b1*a3 - a1*b3) (-b1*a2 + b2*a2)

rotateAroundAxis :: L.V3 Double ->  Double ->  L.V3 Double -> L.V3 Double
rotateAroundAxis (L.V3 ux uy uz) phi (L.V3 x1 x2 x3) =
   L.V3 (x1*m11 + x2*m12 + x3*m13 ) ( x1*m21 + x2*m22 + x3*m23) ( x1*m31 + x2*m32 + x3*m33) where m11 = cos(phi) + ux*ux*(1-cos(phi))
                                                                                                  m12 = ux*uy*(1-cos(phi)) - uz*sin(phi)
                                                                                                  m13 = ux*uz*(1-cos(phi)) + uy*sin(phi)
                                                                                                  m21 = uy*ux*(1-cos(phi)) + uz*sin(phi)
                                                                                                  m22 = cos(phi) + uy*uy*(1-cos(phi))
                                                                                                  m23 = uy*uz*(1-cos(phi)) - ux*sin(phi)
                                                                                                  m31 = uz*ux*(1-cos(phi)) - uy*sin(phi)
                                                                                                  m32 = uz*uy*(1-cos(phi)) + ux*sin(phi)
                                                                                                  m33 = cos(phi) + uz*uz*(1-cos(phi))
 
normalizeVector :: L.V3 Double -> L.V3 Double
normalizeVector (L.V3 x y z) = L.V3 (rnorm*x) (rnorm*y) (rnorm*z) where rnorm = 1/sqrt(x*x+y*y+z*z)

rotateAroundAxis2 :: L.V3 Double -> Double -> L.V3 Double -> L.V3 Double
rotateAroundAxis2 (L.V3 u v w) phi (L.V3 x y z) = L.V3 a b c where a = (u*(u*x + v*y + w*z)*(1-cos(phi)) + (u*u+v*v+w*w)*x*cos(phi) + sqrt(u*u + v*v + w*w)*(-w*y+v*z)*sin(phi) )/( u*u + v*v + w*w)
                                                                   b = (v*(u*x + v*y + w*z)*(1-cos(phi)) + (u*u+v*v+w*w)*y*cos(phi) + sqrt(u*u + v*v + w*w)*(w*x-u*z)*sin(phi) )/( u*u + v*v + w*w)
                                                                   c = (w*(u*x + v*y + w*z)*(1-cos(phi)) + (u*u+v*v+w*w)*z*cos(phi) + sqrt(u*u + v*v + w*w)*(-v*x-u*y)*sin(phi) )/( u*u + v*v + w*w)
 

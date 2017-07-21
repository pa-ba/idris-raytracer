module Transformation

import LinearAlgebra
%access public export


record Transformation where
  constructor MkTransformation
  matrix, inverse : Matrix

identity : Transformation
identity = 
  MkTransformation
    (MkMatrix 1 0 0 0
              0 1 0 0
              0 0 1 0)
    (MkMatrix 1 0 0 0
              0 1 0 0
              0 0 1 0)
              
translate : (x, y, z : Double) -> Transformation
translate x y z = 
  MkTransformation
    (MkMatrix 1 0 0 x
              0 1 0 y
              0 0 1 z)
    (MkMatrix 1 0 0 (negate x)
              0 1 0 (negate y)
              0 0 1 (negate z))

translateByVector : Vector -> Transformation
translateByVector (MkVector x y z) = translate x y z

scale : (x, y, z : Double) -> Transformation
scale x y z = 
  MkTransformation
    (MkMatrix x 0 0 0
              0 y 0 0
              0 0 z 0)
    (MkMatrix (1/x) 0     0     0
              0     (1/y) 0     0
              0     0     (1/z) 0)

rotateX : (angle : Double) -> Transformation
rotateX angle = 
  MkTransformation
    (MkMatrix 1 0 0 0
              0 (cos angle) (negate (sin angle)) 0
              0 (sin angle)  (cos angle) 0)
    (MkMatrix 1 0 0 0
              0 (cos angle) (sin angle) 0
              0 (negate (sin angle)) (cos angle) 0)

rotateY : (angle : Double) -> Transformation
rotateY angle =
  MkTransformation
    (MkMatrix (cos angle) 0 (sin angle) 0
              0 1 0 0
              (negate (sin angle)) 0 (cos angle) 0)
    (MkMatrix (cos angle) 0 (negate (sin angle)) 0
              0 1 0 0
              (sin angle) 0 (cos angle) 0)

rotateZ : (angle : Double) -> Transformation
rotateZ angle =
  MkTransformation
    (MkMatrix (cos angle) (negate (sin angle)) 0 0
              (sin angle) (cos angle) 0 0
              0 0 1 0)
    (MkMatrix (cos angle) (sin angle) 0 0
              (negate (sin angle)) (cos angle) 0 0
              0 0 1 0)


merge : Transformation -> Transformation -> Transformation
merge (MkTransformation ma inv) (MkTransformation ma' inv') = 
  MkTransformation (ma' * ma) (inv * inv')


mergeAll : List Transformation -> Transformation
mergeAll [] = identity
mergeAll (MkTransformation ma inv :: xs) = run ma inv xs
  where run : Matrix -> Matrix -> List Transformation -> Transformation
        run ma inv [] = MkTransformation ma inv
        run ma inv ((MkTransformation matrix inverse) :: xs) = run (matrix * ma) (inv * inverse) xs




inverseTransformRay : Transformation -> Ray -> Ray
inverseTransformRay (MkTransformation _ inverse) r = transformRay inverse r


inverseTransformPoint : Transformation -> Point -> Point
inverseTransformPoint (MkTransformation _ inverse) r = transformPoint inverse r
 
transformNormal : Transformation -> Vector -> Vector
transformNormal (MkTransformation _ inverse) v = transformNormal inverse v

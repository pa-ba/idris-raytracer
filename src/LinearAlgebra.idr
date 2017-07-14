module LinearAlgebra

%access public export


record Vector where
  constructor MkVector
  x, y, z : Double

%name Vector v, w, u

-- vector dot product
dot : Vector -> Vector -> Double
dot (MkVector x1 y1 z1) (MkVector x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2

-- Vector addition
(+) : Vector -> Vector -> Vector
(+) (MkVector x1 y1 z1) (MkVector x2 y2 z2) = MkVector (x1 + x2) (y1 + y2)  (z1 + z2)

-- Vector subtraction
(-) : Vector -> Vector -> Vector
(-) (MkVector x1 y1 z1) (MkVector x2 y2 z2) = MkVector (x1 - x2) (y1 - y2)  (z1 - z2)


-- scalar multiplication
scale : Double -> Vector -> Vector
scale s (MkVector x y z) = MkVector (s * x) (s * y)  (s * z)

-- scalar multiplication
negate : Vector -> Vector
negate (MkVector x y z) = MkVector (negate x) (negate y)  (negate z)



-- vector cross product
cross : Vector -> Vector -> Vector
cross (MkVector x1 y1 z1) (MkVector x2 y2 z2) = 
  MkVector ((y1 * z2) - (z1 * y2)) ((z1 * x2) - (x1 * z2)) ((x1 * y2) - (y1 * x2))



(/) : Vector -> Double -> Vector
(/) v s = (1/s) `scale` v

magnitude : Vector -> Double
magnitude (MkVector x y z) = sqrt (x * x + y * y + z * z)

normalise : Vector -> Vector
normalise v = v / magnitude v



Point : Type
Point = Vector

mkPoint : (x, y, z : Double) -> Point
mkPoint = MkVector

direction : Point -> Point -> Vector
direction p q = q - p

record Ray where
  constructor MkRay
  origin : Point
  direction : Vector

%name Ray r, s

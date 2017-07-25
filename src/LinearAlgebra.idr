module LinearAlgebra

%access public export
%default total

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

march : Ray -> Double -> Point
march (MkRay origin direction) d = origin + (d `scale` direction)

record Matrix where
  constructor MkMatrix
  xx, yx, zx, ox : Double
  xy, yy, zy, oy : Double
  xz, yz, zz, oz : Double
  
namespace matrix
  (*) : Matrix -> Matrix -> Matrix
  (*) 
    (MkMatrix xx1 yx1 zx1 ox1 xy1 yy1 zy1 oy1 xz1 yz1 zz1 oz1) 
    (MkMatrix xx2 yx2 zx2 ox2 xy2 yy2 zy2 oy2 xz2 yz2 zz2 oz2)
      = MkMatrix
        (xx1 * xx2 + yx1 * xy2 + zx1 * xz2) (xx1 * yx2 + yx1 * yy2 + zx1 * yz2)
        (xx1 * zx2 + yx1 * zy2 + zx1 * zz2) (xx1 * ox2 + yx1 * oy2 + zx1 * oz2 + ox1)

        (xy1 * xx2 + yy1 * xy2 + zy1 * xz2) (xy1 * yx2 + yy1 * yy2 + zy1 * yz2)
        (xy1 * zx2 + yy1 * zy2 + zy1 * zz2) (xy1 * ox2 + yy1 * oy2 + zy1 * oz2 + oy1)

        (xz1 * xx2 + yz1 * xy2 + zz1 * xz2) (xz1 * yz2 + yz1 * yy2 + zz1 * yz2)
        (xz1 * zx2 + yz1 * zy2 + zz1 * zz2) (xz1 * oz2 + yz1 * oy2 + zz1 * oz2 + oz1)

transformPoint : Matrix -> Point -> Point
transformPoint (MkMatrix xx yx zx ox xy yy zy oy xz yz zz oz) (MkVector x y z) = 
  MkVector 
    (xx * x + yx * y + zx * z + ox)
    (xy * x + yy * y + zy * z + oy)
    (xz * x + yz * y + zz * z + oz)

transformVector : Matrix -> Vector -> Vector
transformVector (MkMatrix xx yx zx ox xy yy zy oy xz yz zz oz) (MkVector x y z) = 
  MkVector 
    (xx * x + yx * y + zx * z)
    (xy * x + yy * y + zy * z)
    (xz * x + yz * y + zz * z)
    
transformNormal : Matrix -> Vector -> Vector
transformNormal (MkMatrix xx yx zx ox xy yy zy oy xz yz zz oz) (MkVector x y z) = 
  normalise (MkVector 
    (xx * x + xy * y + xz * z)
    (yx * x + yy * y + yz * z)
    (zx * x + zy * y + zz * z))

transformRay : Matrix -> Ray -> Ray
transformRay m (MkRay origin direction) = MkRay (transformPoint m origin) (transformVector m direction)

 

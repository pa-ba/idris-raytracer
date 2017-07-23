||| This module implements basic shapes like spheres, cylinders etc.

module Shapes.BasicShapes

import public Shapes.Base
import public Shapes.CSG
import public LinearAlgebra
import public Colour
import public Transformation

%access public export
record Sphere where
  constructor MkSphere
  texture : Texture



||| Helper function to solve 2nd degree polynomials
%inline
private
solve2ndD' : (nothing : t) -> (comp : Double -> i) -> (check : i -> Bool) -> (just : i -> t) -> (a, b, c : Double) -> t
solve2ndD' nothing comp check just a b c = 
    let disc = b * b - 4.0 * a * c in
    if disc < 0.0 then nothing
    else
      let e = sqrt disc
          denom = 2.0 * a
          t1 = (-b - e) / denom
          r1 = comp t1 in
      if t1 > 0.0 && check r1 then 
        just r1
      else 
        let t2 = (-b + e) / denom
            r2 = comp t2 in
        if t2 > 0.0 && check r2 then
          just r2
        else nothing
        
||| This function finds the smallest positive solution (if any) of the
||| polynomial 'a x^2 + b x + c = 0'. If there is no solution
||| `Nothing` is returned.
private
solve2ndD : (a, b, c : Double) -> Maybe Double
solve2ndD =  solve2ndD' Nothing id (const True) Just

private
twoPi : Double
twoPi = 2 * Doubles.pi

IsShape Sphere where
  hit (MkSphere tex) (MkRay origin dir) = 
    let a = dir `dot` dir
        b = 2.0 * (origin `dot` dir)
        c = (origin `dot` origin) - 1
    in solve2ndD' Nothing id (const True) just a b c
    where just t =
            let ip = origin + (t `scale` dir)
                mat = case tex of 
                        MkConstTexture mat => mat
                        MkTexture tex => 
                          let v = 1.0 - (acos (y ip) / Doubles.pi) 
                              phi' = atan2 (x ip) (z ip)
                              phi = if phi' < 0.0 then phi' + twoPi else phi'
                              u = phi / twoPi
                          in tex u v
            in Just (MkHit t mat ip)
            

  hitBefore (MkSphere tex) (MkRay origin dir) dBound = 
    let a = dir `dot` dir
        b = 2.0 * (origin `dot` dir) - 1
        c = origin `dot` origin
    in solve2ndD' Nothing id (const True) just a b c
    where just t = if (t < dBound) then Just t else Nothing
    
export
IsSolid Sphere where
  inside (MkSphere _) (MkVector x y z) = x * x + y * y + z * z < 1
            
mkSphere : (centre : Point) -> (radius : Double) -> (texture : Texture) -> Shape
mkSphere centre radius texture = 
  MkShape $ MkTransformedShape (merge (scale radius radius radius) (translateByVector centre)) 
          (MkSphere texture)

||| Type representing a hollow cylinder.
export
record Cylinder where
  constructor MkCylinder
  texture : Texture


IsShape Cylinder where
  hit (MkCylinder tex) ray@(MkRay origin dir) = 
    let a = x dir * x dir + z dir * z dir
        b = 2.0 * (x origin * x dir + z origin * z dir)
        c = (x origin * x origin + z origin * z origin) - 1.0
    in solve2ndD' Nothing comp check just a b c
    where comp : Double -> (Point, Double)
          comp d = (origin + (d `scale` dir), d)
          check (ip,d) = y ip <= 1 && y ip >= -1
          just (ip,d) =
            let mat = case tex of 
                        MkConstTexture mat => mat
                        MkTexture tex => 
                          let phi' = atan2 (x ip) (z ip)
                              phi = if phi' < 0.0 then phi' + twoPi else phi'
                              u = phi / twoPi
                          in tex u ((y ip + 1)/2)
            in Just (MkHit d mat (MkVector (x ip) 0 (z ip)))

  hitBefore (MkCylinder tex) ray@(MkRay origin dir) dBound = 
    let a = x dir * x dir + z dir * z dir
        b = 2.0 * (x origin * x dir + z origin * z dir)
        c = (x origin * x origin + z origin * z origin) - 1.0
    in solve2ndD' Nothing id check (Just) a b c
    where check d = 
            if d >= dBound then False
            else
              let ip = origin + (d `scale` dir) in
              y ip <= 1 && y ip >= -1


||| Construct a hollow cylinder.
mkCylinder : (centre : Point) -> (radius, height : Double) ->(tex : Texture) -> Shape
mkCylinder centre radius height tex = 
  MkShape $ MkTransformedShape
   (merge (scale radius (height / 2) radius) (translateByVector centre))
          (MkCylinder tex)

||| Type representing a flat disc.
export
record Disc where
  constructor MkDisc
  texture : Texture

||| Compute the square.
private
%inline
sq : Double -> Double
sq d = d * d

||| Helper function for implementing the hit function for the disc.
private
%inline
discHit : (just : Point -> Double -> a) -> (nothing : a) -> (ray : Ray) -> a
discHit just nothing ray@(MkRay ori dir) =
  if z dir == 0.0 then nothing
  else 
    let d = (negate (z ori))/z dir in
    if d > 0.0 then
      let ip = march ray d in
      if sq (x ip) + sq(y ip) <= 1 then just ip d
      else nothing
    else nothing
  

IsShape Disc where
  hit (MkDisc tex) = discHit just Nothing
    where just ip d = 
            let mat = case tex of
                        MkConstTexture m => m
                        MkTexture t => t ((x ip + 1)/ 2) ((y ip + 1) / 2)
            in Just (MkHit d mat (MkVector 0 0 1))
  hitBefore (MkDisc _) ray dBound = discHit just Nothing ray
    where just ip d = if d < dBound then Just d else Nothing


||| Construct a flat disc.
mkDisc : Point -> (radius : Double) -> Texture -> Shape
mkDisc centre radius tex = 
  mkShape $ MkTransformedShape (merge (scale radius radius 1.0) (translateByVector centre))  $ MkDisc tex

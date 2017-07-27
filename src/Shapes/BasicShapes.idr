||| This module implements basic shapes like spheres, cylinders etc.

module Shapes.BasicShapes

import public Shapes.Base
import public Shapes.CSG
import public LinearAlgebra
import public Colour
import public Transformation

import Debug.Trace

%access export
%default total


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

Hitable Sphere where
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
        b = 2.0 * (origin `dot` dir)
        c = (origin `dot` origin) - 1
    in solve2ndD' False id (const True) just a b c
    where just t = t < dBound
    
Solid Sphere where
  inside (MkSphere _) (MkVector x y z) = x * x + y * y + z * z < 1
      

mkSphere : (centre : Point) -> (radius : Double) -> (texture : Texture) -> SolidShape
mkSphere centre radius texture = 
  MkSolidShape $ MkTransformedShape (merge (scale radius radius radius) (translateByVector centre)) 
          (MkSphere texture)

||| Type representing a hollow cylinder.

record Cylinder where
  constructor MkCylinder
  texture : Texture


Hitable Cylinder where
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
    in solve2ndD' False id check (const True) a b c
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
  

Hitable Disc where
  hit (MkDisc tex) = discHit just Nothing
    where just ip d = 
            let mat = case tex of
                        MkConstTexture m => m
                        MkTexture t => t ((x ip + 1)/ 2) ((y ip + 1) / 2)
            in Just (MkHit d mat (MkVector 0 0 1))
  hitBefore (MkDisc _) ray dBound = discHit just False ray
    where just ip d = d < dBound


||| Construct a flat disc.
mkDisc : Point -> (radius : Double) -> Texture -> Shape
mkDisc centre radius tex = 
  mkShape $ MkTransformedShape (merge (scale radius radius 1.0) (translateByVector centre))  $ MkDisc tex


record SolidCylinder where
  constructor MkSolidCylinder
  shapes : Group
  
Hitable SolidCylinder where
  hit (MkSolidCylinder gr) ray = hit gr ray
  hitBefore (MkSolidCylinder gr) ray = hitBefore gr ray


||| Helper function for implementing the hit function for the disc.
private
%inline
cylinderDiscHit : (offset : Double) ->(just : Point -> Double -> a) -> (nothing : a) -> (ray : Ray) -> a
cylinderDiscHit offset just nothing ray@(MkRay ori dir) =
  if y dir == 0.0 then nothing
  else 
    let d = (offset - (y ori))/y dir in
    if d > 0.0 then
      let ip = march ray d in
      if sq (x ip) + sq(z ip) <= 1 then just ip d
      else nothing
    else nothing


record CylinderDisc where
  constructor MkCylinderDisc
  offset : Double
  texture : Texture


Hitable CylinderDisc where
  hit (MkCylinderDisc off tex) = cylinderDiscHit off just Nothing
    where just ip d = 
            let mat = case tex of
                        MkConstTexture m => m
                        MkTexture t => t ((x ip + 1)/ 2) ((z ip + 1) / 2)
            in Just (MkHit d mat (MkVector 0 1 0))
  hitBefore (MkCylinderDisc off _) ray dBound = cylinderDiscHit off just False ray
    where just ip d = d < dBound
    
Solid SolidCylinder where
  inside (MkSolidCylinder _) (MkVector x y z) = 
    (-1) <= y && y <= 1 && sq x + sq z <= 1
  
mkSolidCylinder : (centre : Point) -> (radius, height : Double) -> (cyl, top, bot : Texture) -> SolidShape
mkSolidCylinder centre radius height cylTex topTex botTex = 
  let cyl = MkShape (MkCylinder cylTex)
      top = MkShape (MkCylinderDisc 1 topTex)
      bot = MkShape (MkCylinderDisc (-1) botTex) in
  MkSolidShape $ MkTransformedShape
   (merge (scale radius (height / 2) radius) (translateByVector centre))
          (MkSolidCylinder $ MkGroup [cyl,top,bot])

private
data Face = Front | Back | Top | Bottom | Left | Right

private
normalFront : Vector
normalFront = MkVector 0.0 0.0 (-1.0)
private
normalBack : Vector
normalBack = MkVector 0.0 0.0 1.0
private
normalLeft : Vector
normalLeft = MkVector (-1.0) 0.0 0.0
private
normalRight : Vector
normalRight = MkVector 1.0 0.0 0.0
private
normalTop : Vector
normalTop = MkVector 0.0 1.0 0.0
private
normalBottom : Vector
normalBottom = MkVector 0.0 (-1.0) 0.0

record Box where
  constructor MkBox
  low, high : Point
  front, back, top, bottom, left, right : Texture

lookupMaterial : Texture -> (u,v : Double) -> Material
lookupMaterial (MkConstTexture mat) _ _ = mat
lookupMaterial (MkTexture f) u v = f u v

data FaceRes = Res Vector Material

Hitable Box where
  hit (MkBox low high front back top bottom left right) ray =  thehit where
    calculateIntersection : Double -> Face -> Hit
    calculateIntersection distance face =
      let ip = march ray distance
      in case face of
            Front => MkHit distance (lookupMaterial front ((x ip-x low)/(x high-x low)) ((y ip-y low)/(y high-y low))) normalFront
            Back => MkHit distance (lookupMaterial back ((x ip-x low)/(x high-x low)) ((y ip-y low)/(y high-y low))) normalBack
            Left => MkHit distance (lookupMaterial left ((y ip-y low)/(y high-y low)) ((z ip-z low)/(z high-z low))) normalLeft
            Right => MkHit distance (lookupMaterial right ((y ip-y low)/(y high-y low)) ((z ip-z low)/(z high-z low))) normalRight
            Top => MkHit distance (lookupMaterial top ((x ip-x low)/(x high-x low)) ((z ip-z low)/(z high-z low))) normalTop
            Bottom => MkHit distance (lookupMaterial bottom ((x ip-x low)/(x high-x low)) ((z ip-z low)/(z high-z low))) normalBottom
         
    thehit : Maybe Hit
    thehit =
      let o = origin ray
          d = direction ray
          a = 1.0 / x d
          (txl,txh) = 
            if a >= 0.0
            then ((x low - x o) * a, (x high - x o) * a)
            else ((x high - x o) * a, (x low - x o) * a)

          b = 1.0 / y d
          (tyl,tyh) = 
            if b >= 0.0
            then ((y low - y o) * b, (y high - y o) * b)
            else ((y high - y o) * b, (y low - y o) * b)

          c = 1.0 / z d
          (tzl,tzh) = 
            if c >= 0.0
            then ((z low - z o) * c, (z high - z o) * c)
            else ((z high - z o) * c, (z low - z o) * c)
      
          (tl,fl)  =
            if txl > tyl && txl > tzl
            then (txl, if a >= 0.0 then Left else Right)
            else 
              if tyl > tzl
              then (tyl, if b >= 0.0 then Bottom else Top)
              else (tzl, if c >= 0.0 then Back else Front)

          (th,fh)  =
            if txh < tyh && txh < tzh
            then (txh, if a >= 0.0 then Right else Left)
            else 
              if tyh < tzh
              then (tyh, if b >= 0.0 then Top else Bottom)
              else (tzh, if c >= 0.0 then Front else Back)
      in if tl < th && th > 0.0 
         then
           if tl > 0.0 
           then Just (calculateIntersection tl fl)
           else Just (calculateIntersection th fh)
         else Nothing

Solid Box where
  inside (MkBox low high _ _ _ _ _ _) p = 
    x low <= x p && x p <= x high &&
    y low <= y p && y p <=  y high &&
    z low <= z p && z p <=  z high
    
    
mkBox : (low, high : Point) -> 
        (front, back, top, bottom, left, right : Texture) -> SolidShape
mkBox low high front back top bottom left right = 
  MkSolidShape (MkBox low high front back top bottom left right)



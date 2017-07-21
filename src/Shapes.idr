module Shapes

import LinearAlgebra
import Colour
import Transformation

%access public export

record Hit where
  constructor MkHit
  distance : Double
  material : Material
  normal : Vector
  
%name Hit h

interface IsShape s where
  hit : s -> Ray -> Maybe Hit
  shadowHit : s -> Ray -> Bool
  shadowHit s r = 
    case hit s r of
      Just (MkHit distance _ _) => distance <= 1.0
      Nothing => False

interface IsCSG s where
  inside : s -> Point -> Bool

data Shape : Type where
  MkShape : IsShape s => Transformation -> s -> Shape
  


transform : Transformation -> Shape -> Shape
transform tr (MkShape tr' sh) = MkShape (merge tr' tr) sh

hitTransformed : IsShape s => Transformation -> s -> Ray -> Maybe Hit
hitTransformed tr s r =
  case hit s (inverseTransformRay tr r) of
     Nothing => Nothing
     Just h => Just $ record {normal $= transformNormal tr} h
  

shadowHitTransformed : IsShape s => Transformation -> s -> Ray -> Bool
shadowHitTransformed tr s r = shadowHit s (inverseTransformRay tr r)

insideTransformed : IsCSG s => Transformation -> s -> Point -> Bool
insideTransformed t s p = inside s (inverseTransformPoint t p)


data ShapeCSG : Shape -> Type where
  MkShapeCSG : (IsShape t, IsCSG t) =>  {m : Transformation} -> {s : t} -> ShapeCSG (MkShape m s)


data Union : Type where
  MkUnion : (IsShape s1, IsCSG s1, IsShape s2, IsCSG s2) => (t1, t2 : Transformation) -> s1 -> s2 -> Union

IsShape Union where
  hit (MkUnion t1 t2 s1 s2) r = 
    case hitTransformed t1 s1 r of
      Nothing => hitTransformed t2 s2 r
      res @ (Just h1) => 
        case hitTransformed t2 s2 r of
          Nothing => res
          Just h2 => 
            let (h1',h2') = if distance h1 < distance h2 then (h1, h2) else (h2, h1)
                ip = march r (distance h1') in
            ?IsShape_rhs_1
  shadowHit x r = ?IsShape_rhs_2

IsCSG Union where
  inside (MkUnion t1 t2 s1 s2) p = insideTransformed t1 s1 p || insideTransformed t2 s2 p


union : (s1, s2 : Shape) -> {auto c1 : ShapeCSG s1} -> {auto c2 : ShapeCSG s2} -> Shape
union (MkShape m s) (MkShape m' s') {c1 = MkShapeCSG} {c2 = MkShapeCSG} = MkShape identity (MkUnion m m' s s')  
  
namespace Shape
  hit : Shape -> Ray -> Maybe Hit
  hit (MkShape tr s) r = hitTransformed tr s r
  
  shadowHit : Shape -> Ray -> Bool
  shadowHit (MkShape tr s) r = shadowHitTransformed tr s r
  
      
mkShape : IsShape s => s -> Shape
mkShape s = MkShape identity s

record Sphere where
  constructor MkSphere
  texture : Texture

%inline
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

solve2ndD : (a, b, c : Double) -> Maybe Double
solve2ndD =  solve2ndD' Nothing id (const True) Just

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
            

  shadowHit (MkSphere tex) (MkRay origin dir) = 
    let a = dir `dot` dir
        b = 2.0 * (origin `dot` dir) - 1
        c = origin `dot` origin
    in solve2ndD' False id (const True) just a b c
    where just t = t < 1
    
    
IsCSG Sphere where
  inside (MkSphere _) (MkVector x y z) = x * x + y * y + z * z < 1
            
mkSphere : (centre : Point) -> (radius : Double) -> (texture : Texture) -> Shape
mkSphere centre radius texture = 
  MkShape (merge (scale radius radius radius) (translateByVector centre)) 
          (MkSphere texture)

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

  shadowHit (MkCylinder tex) ray@(MkRay origin dir) = 
    let a = x dir * x dir + z dir * z dir
        b = 2.0 * (x origin * x dir + z origin * z dir)
        c = (x origin * x origin + z origin * z origin) - 1.0
    in solve2ndD' False id check (const True) a b c
    where check d = 
            if d >= 1 then False
            else
              let ip = origin + (d `scale` dir) in
              y ip <= 1 && y ip >= -1

mkCylinder : (centre : Point) -> (radius, height : Double) ->(tex : Texture) -> Shape
mkCylinder centre radius height tex = 
  MkShape (merge (scale radius (height / 2) radius) (translateByVector centre))
          (MkCylinder tex)

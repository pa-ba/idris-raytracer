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

mutual
  interface IsShape s where
    hit : s -> Ray -> Maybe Hit
    hitBefore : s -> Ray -> Double -> Maybe Double
    hitBefore s r d = 
      case hit s r of
        Just (MkHit distance _ _) => if (distance <= d) then Just distance else Nothing
        Nothing => Nothing
    transformedShape : s -> DecomposeTrans
    transformedShape _ = defaultTrans
    
  data DecomposeTrans : Type where
    NotTrans : DecomposeTrans
    Trans : IsShape s => Transformation -> s -> DecomposeTrans
    
  defaultTrans : DecomposeTrans
  defaultTrans = NotTrans

  data Shape : Type where
      MkShape : IsShape s => s -> Shape

  data TransformedShape : Type -> Type where
    MkTransformedShape : Transformation -> s -> TransformedShape s
    
IsShape s => IsShape (TransformedShape s) where
  hit (MkTransformedShape tr s) r =
   case hit s (inverseTransformRay tr r) of
      Nothing => Nothing
      Just h => Just $ record {normal $= transformNormal tr} h
      
  hitBefore (MkTransformedShape tr s) r d = hitBefore s (inverseTransformRay tr r) d

      
  transformedShape (MkTransformedShape t s) = Trans t s
 

interface IsCSG s where
  inside : s -> Point -> Bool


IsCSG s => IsCSG (TransformedShape s) where
  inside (MkTransformedShape t s) p = inside s (inverseTransformPoint t p)


data ShapeCSG : Shape -> Type where
  MkShapeCSG : (IsShape t, IsCSG t) => {s : t} -> ShapeCSG (MkShape s)


data Union : Type where
  MkUnion : (IsShape s1, IsCSG s1, IsShape s2, IsCSG s2) => s1 -> s2 -> Union


hitUnionInside : (IsShape t1, IsCSG t1, IsShape t2, IsCSG t2) => t1 -> t2 -> Ray -> Double -> Maybe Hit
hitUnionInside s1 s2 r d = 
  case hit s1 r of
    Nothing => Nothing
    Just h1 => 
      let ip = march r (distance h1) in
      if inside s2 ip then
        hitUnionInside s2 s1 (record {origin = ip} r) (d + distance h1)
      else Just (record {distance = distance h1 + d} h1)

hitUnionInsideBefore : (IsShape t1, IsCSG t1, IsShape t2, IsCSG t2) => t1 -> t2 -> Ray -> Double -> Double -> Maybe Double
hitUnionInsideBefore s1 s2 r dCur dBound = 
 case hitBefore s1 r (dBound - dCur) of
    Nothing => Nothing
    Just d' => 
      let ip = march r d' in
      if inside s2 ip then
        hitUnionInsideBefore s2 s1 (record {origin = ip} r) (dCur + d') dBound
      else Just (dCur + d')


IsShape Union where
  hit (MkUnion s1 s2) r@(MkRay ori dir) = 
    if inside s1 ori then
      hitUnionInside s1 s2 r 0
    else if inside s2 ori then
      hitUnionInside s2 s1 r 0
    else
      case hit s1 r of
        Nothing => hit s2 r
        res1@(Just h1) => 
          case hit s2 r of
            Nothing => res1
            res2@(Just h2) => 
              if abs (distance h1 - distance h2) < 0.00001 then
                Just (record {distance = min (distance h1) (distance h2)} h1)
              else if distance h1 <= distance h2 then res1 else res2
  
  hitBefore (MkUnion s1 s2) r@(MkRay ori dir) dBound = 
    if inside s1 ori then
      hitUnionInsideBefore s1 s2 r 0 dBound
    else if inside s2 ori then
      hitUnionInsideBefore s2 s1 r 0 dBound
    else
      case hitBefore s1 r dBound of
        res1 @(Just d1) => 
          case hitBefore s2 r dBound of
            (Just d2) => Just (min d1 d2)
            Nothing => res1
        Nothing => hitBefore s2 r dBound

IsCSG Union where
  inside (MkUnion s1 s2) p = inside s1 p || inside s2 p


union : (s1, s2 : Shape) -> {auto c1 : ShapeCSG s1} -> {auto c2 : ShapeCSG s2} -> Shape
union (MkShape s) (MkShape s') {c1 = MkShapeCSG} {c2 = MkShapeCSG} = MkShape (MkUnion s s')  
  
namespace Shape
  hit : Shape -> Ray -> Maybe Hit
  hit (MkShape s) r = hit s r
  
  hitBefore : Shape -> Ray -> Double -> Maybe Double
  hitBefore (MkShape s) = hitBefore s

  transform : Transformation -> Shape -> Shape
  transform tr (MkShape s) = 
    case transformedShape s of
      NoTrans => MkShape (MkTransformedShape tr s)
      Trans tr' s => MkShape (MkTransformedShape (merge tr' tr) s)
  
      
mkShape : IsShape s => s -> Shape
mkShape s = MkShape s

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
            

  hitBefore (MkSphere tex) (MkRay origin dir) dBound = 
    let a = dir `dot` dir
        b = 2.0 * (origin `dot` dir) - 1
        c = origin `dot` origin
    in solve2ndD' Nothing id (const True) just a b c
    where just t = if (t < dBound) then Just t else Nothing
    
    
IsCSG Sphere where
  inside (MkSphere _) (MkVector x y z) = x * x + y * y + z * z < 1
            
mkSphere : (centre : Point) -> (radius : Double) -> (texture : Texture) -> Shape
mkSphere centre radius texture = 
  MkShape $ MkTransformedShape (merge (scale radius radius radius) (translateByVector centre)) 
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

mkCylinder : (centre : Point) -> (radius, height : Double) ->(tex : Texture) -> Shape
mkCylinder centre radius height tex = 
  MkShape $ MkTransformedShape
   (merge (scale radius (height / 2) radius) (translateByVector centre))
          (MkCylinder tex)

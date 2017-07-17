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

data Shape : Type where
  MkShape : Maybe Transformation -> (IsShape s => s -> Shape)
  

transformShape : Transformation -> Shape -> Shape
transformShape tr (MkShape Nothign sh) = MkShape (Just tr) sh
transformShape tr (MkShape (Just tr') sh) = MkShape (Just (merge [tr', tr])) sh
  
  
namespace Shape
  hit : Shape -> Ray -> Maybe Hit
  hit (MkShape Nothing s) r = hit s r
  hit (MkShape (Just (MkTransformation _ inverse)) s) r = 
    case hit s (transformRay inverse r) of
       Nothing => Nothing
       Just h => Just $ record {normal $= transformNormal inverse} h
  
  shadowHit : Shape -> Ray -> Bool
  shadowHit (MkShape Nothing s) r = shadowHit s r
  shadowHit (MkShape (Just tr) s) r = shadowHit s (transformRay (inverse tr) r)

  
mkShape : IsShape s => s -> Shape
mkShape s = MkShape Nothing s

record Sphere where
  constructor MkSphere
  radius : Double
  texture : Texture

%inline
solve2ndD' : (nothing : t) -> (just : Double -> t) -> (a, b, c : Double) -> t
solve2ndD' nothing just a b c = 
    let disc = b * b - 4.0 * a * c in
    if disc < 0.0 then nothing
    else
      let e = sqrt disc
          denom = 2.0 * a
          t1 = (-b - e) / denom in
      if t1 > 0.0 then 
        just t1
      else 
        let t2 = (-b + e) / denom in
        if t2 > 0.0 then
          just t2
        else nothing

solve2ndD : (a, b, c : Double) -> Maybe Double
solve2ndD =  solve2ndD' Nothing Just


IsShape Sphere where
  hit (MkSphere radius tex) (MkRay origin dir) = 
    let a = dir `dot` dir
        b = 2.0 * (origin `dot` dir)
        c = (origin `dot` origin) - radius * radius
    in solve2ndD' Nothing just a b c
    where just t =
            case tex of 
              Constant mat@(MkMaterial colour reflection) => 
                Just (MkHit t mat ((origin + (t `scale` dir))/radius))

  shadowHit (MkSphere radius tex) (MkRay origin dir) = 
    let a = dir `dot` dir
        b = 2.0 * (origin `dot` dir)
        c = (origin `dot` origin) - radius * radius
    in solve2ndD' False just a b c
    where just t = t < 1
            
mkSphere : (radius : Double) -> (texture : Texture) -> Shape
mkSphere radius texture = mkShape $ MkSphere radius texture



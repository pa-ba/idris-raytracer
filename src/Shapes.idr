module Shapes

import LinearAlgebra
import Colour

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
  MkShape : IsShape s => s -> Shape


record Sphere where
  constructor MkSphere
  radius : Double
  texture : Texture


solve2ndD' : (comp : Double -> t) -> (check : t -> Bool) -> (a, b, c : Double) -> Maybe t
solve2ndD' comp check a b c = 
    let disc = b * b - 4.0 * a * c in
    if disc < 0.0 then Nothing
    else
      let e = sqrt disc
          denom = 2.0 * a
          t1 = (-b - e) / denom
          r1 = comp t1 in
      if t1 > 0.0 && check r1 then 
        Just r1
      else 
        let t2 = (-b + e) / denom
            r2 = comp t2 in
        if t2 > 0.0 && check r2 then
          Just r2
        else Nothing

solve2ndD : (a, b, c : Double) -> Maybe Double
solve2ndD =  solve2ndD' id (const True)


IsShape Sphere where
  hit (MkSphere radius tex) (MkRay origin dir) = 
    let a = dir `dot` dir
        b = 2.0 * (origin `dot` dir)
        c = (origin `dot` origin) - radius * radius
     in case solve2ndD a b c of
          Nothing => Nothing
          Just t => 
            case tex of 
              Constant mat@(MkMaterial colour reflection) => 
                Just (MkHit t mat ((origin + (t `scale` dir))/radius))


 

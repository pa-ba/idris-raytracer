module Shapes.CSG

import public Shapes.Base
import public LinearAlgebra
import public Colour
import public Transformation

%access public export
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

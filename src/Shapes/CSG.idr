||| Constructive solid geometry. This module allows solid shapes to be
||| composed with Boolean operators like 'union', 'intersection', and
||| 'subtraction'.

module Shapes.CSG

import public Shapes.Base
import public LinearAlgebra
import public Colour
import public Transformation

%access export

||| This interface is supposed to be implemented by solid shapes
||| (making them eligible for CSG).
export
interface IsSolid s where
  inside : s -> Point -> Bool

||| Transformations preserve solidness.
export
IsSolid s => IsSolid (TransformedShape s) where
  inside (MkTransformedShape t s) p = inside s (inverseTransformPoint t p)

||| Inhabitants of this type are evidence that a given type is solid.
public export
data SolidShape : Shape -> Type where
  MkSolidShape : (IsShape t, IsSolid t) => {s : t} -> SolidShape (MkShape s)


||| Type representing the union of two shapes.
export
data Union : Type where
  MkUnion : (IsShape s1, IsSolid s1, IsShape s2, IsSolid s2) => s1 -> s2 -> Union

||| Helper function for the hit function of 'union'
private
hitUnionInside : (IsShape t1, IsSolid t1, IsShape t2, IsSolid t2) => t1 -> t2 -> Ray -> Double -> Maybe Hit
hitUnionInside s1 s2 r d = 
  case hit s1 r of
    Nothing => Nothing
    Just h1 => 
      let ip = march r (distance h1) in
      if inside s2 ip then
        hitUnionInside s2 s1 (record {origin = ip} r) (d + distance h1)
      else Just (record {distance = distance h1 + d} h1)

||| Helper function for the 'inside' function of 'union'.
private
hitUnionInsideBefore : (IsShape t1, IsSolid t1, IsShape t2, IsSolid t2) 
  => t1 -> t2 -> Ray -> Double -> Double -> Maybe Double
hitUnionInsideBefore s1 s2 r dCur dBound = 
 case hitBefore s1 r (dBound - dCur) of
    Nothing => Nothing
    Just d' => 
      let ip = march r d' in
      if inside s2 ip then
         hitUnionInsideBefore s2 s1 (record {origin = ip} r) (dCur + d') dBound
      else Just (dCur + d')

export
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

export
IsSolid Union where
  inside (MkUnion s1 s2) p = inside s1 p || inside s2 p


||| This function constructs the union of two solid shapes.
export
union : (s1, s2 : Shape) -> {auto c1 : SolidShape s1} -> {auto c2 : SolidShape s2} -> Shape
union (MkShape s) (MkShape s') {c1 = MkSolidShape} {c2 = MkSolidShape} = MkShape (MkUnion s s')  



||| Constructive solid geometry. This module allows solid shapes to be
||| composed with Boolean operators like 'union', 'intersection', and
||| 'subtraction'.

module Shapes.CSG

import public Shapes.Base
import public LinearAlgebra
import public Colour
import public Transformation

%access export
%default total

mutual
  ||| This interface is supposed to be implemented by solid shapes
  ||| (making them eligible for CSG).
  export
  interface Hitable s => Solid s where
    inside : s -> Point -> Bool
    transformedSolidShape : s -> DecomposeTransSolid
    transformedSolidShape _ = defaultTransSolid
   
  data DecomposeTransSolid : Type where
    NotTrans : DecomposeTransSolid
    Trans : Solid s => Transformation -> s -> DecomposeTransSolid

  defaultTransSolid : DecomposeTransSolid
  defaultTransSolid = NotTrans


||| Transformations preserve solidness.
export
Solid s => Solid (TransformedShape s) where
  inside (MkTransformedShape t s) p = inside s (inverseTransformPoint t p)

||| Inhabitants of this type are evidence that a given type is solid.
public export
data SolidShape : Type where
  MkSolidShape : (Solid s) => s -> SolidShape

IsShape SolidShape where
  hitShape (MkSolidShape s) = hit s
  hitShapeBefore (MkSolidShape s) = hitBefore s
  transform tr (MkSolidShape s) = 
    case transformedSolidShape s of
      NoTrans => MkSolidShape (MkTransformedShape tr s)
      Trans tr' s => MkSolidShape (MkTransformedShape (merge tr' tr) s)



||| Type representing the union of two shapes.
public export
data Union : Type where
  MkUnion : (Solid s1, Solid s2) => s1 -> s2 -> Union

csgFuel : Nat
csgFuel = 10

||| Helper function for the hit function of 'union'
private
hitUnionInside : (Solid t1, Solid t2) => (fuel : Nat) -> t1 -> t2 -> Ray -> Double -> Maybe Hit
hitUnionInside Z _ _ _ _ = Nothing
hitUnionInside (S fuel) s1 s2 r d = 
  case hit s1 r of
    Nothing => Nothing
    Just h1 => 
      let ip = march r (distance h1) in
      if inside s2 ip then
        hitUnionInside fuel s2 s1 (record {origin = ip} r) (d + distance h1)
      else Just (record {distance = distance h1 + d} h1)

||| Helper function for the 'inside' function of 'union'.
private
hitUnionInsideBefore : (Solid t1, Solid t2) 
  => (fuel : Nat) -> t1 -> t2 -> Ray -> Double -> Double -> Maybe Double
hitUnionInsideBefore Z _ _ _ _ _ = Nothing
hitUnionInsideBefore (S fuel) s1 s2 r dCur dBound = 
 case hitBefore s1 r (dBound - dCur) of
    Nothing => Nothing
    Just d' => 
      let ip = march r d' in
      if inside s2 ip then
         hitUnionInsideBefore fuel s2 s1 (record {origin = ip} r) (dCur + d') dBound
      else Just (dCur + d')

public export
Hitable Union where
  hit (MkUnion s1 s2) r@(MkRay ori dir) = 
    if inside s1 ori then
      hitUnionInside csgFuel s1 s2 r 0
    else if inside s2 ori then
      hitUnionInside csgFuel s2 s1 r 0
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
      hitUnionInsideBefore csgFuel s1 s2 r 0 dBound
    else if inside s2 ori then
      hitUnionInsideBefore csgFuel s2 s1 r 0 dBound
    else
      case hitBefore s1 r dBound of
        res1 @(Just d1) => 
          case hitBefore s2 r dBound of
            (Just d2) => Just (min d1 d2)
            Nothing => res1
        Nothing => hitBefore s2 r dBound

Solid Union where
  inside (MkUnion s1 s2) p = inside s1 p || inside s2 p


||| This function constructs the union of two solid shapes.

union : (s1, s2 : SolidShape) -> SolidShape
union (MkSolidShape s) (MkSolidShape s') = MkSolidShape (MkUnion s s')  


record Group where
  constructor MkGroup
  shapes : ShapeList
  
Hitable Group where
  hit (MkGroup shs) ray = findClosestHit shs ray
  hitBefore (MkGroup shs) ray dbound = run shs
    where run [] = Nothing
          run (sh :: shs') = 
            case hitShapeBefore sh ray dbound of
              Nothing => run shs'
              h => h
  

export
group : ShapeList -> Shape
group = mkShape . MkGroup

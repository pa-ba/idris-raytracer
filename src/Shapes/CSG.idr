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


private
csgFuel : Nat
csgFuel = 100


||| Type representing the union of two shapes.
public export
data Union : Type where
  MkUnion : (Solid s1, Solid s2) => s1 -> s2 -> Union

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
              if distance h1 `eqEpsilon` distance h2 then
                Just (record {distance = min (distance h1) (distance h2)} h1)
              else if distance h1 <= distance h2 then res1 else res2
  
Solid Union where
  inside (MkUnion s1 s2) p = inside s1 p || inside s2 p


||| This function constructs the union of two solid shapes.

union : (s1, s2 : SolidShape) -> SolidShape
union (MkSolidShape s) (MkSolidShape s') = MkSolidShape (MkUnion s s')  

||| Type representing the intersection of two shapes.
public export
data Intersection : Type where
  MkIntersection : (Solid s1, Solid s2) => s1 -> s2 -> Intersection
  


Hitable Intersection where
  hit (MkIntersection s1 s2) ray = run csgFuel ray 0 where
    run : Nat -> Ray -> Double -> Maybe Hit
    run Z _ _ = Nothing
    run (S fuel) ray dist =
      case (hit s1 ray,hit s2 ray) of
        (Just h1, Just h2) => 
          if distance h1 `eqEpsilon` distance h2 then
            Just (record {distance = (distance h1 `min` distance h2) + dist } h1)
          else if distance h1 <= distance h2 then
            let hp1 = march ray (distance h1) in
            if inside s2 hp1 then Just (record {distance = distance h1 + dist} h1)
            else 
              let hp2 = march ray (distance h2) in
              if inside s1 hp2 then Just (record {distance = distance h2 + dist} h2)
              else run fuel (record {origin = hp2 + (2 * kEpsilon `scale` direction ray)} ray)
                       (dist + 2 * kEpsilon + distance h2)
          else 
            let hp2 = march ray (distance h2) in
            if inside s1 hp2 then Just (record {distance = distance h2 + dist} h2)
            else 
              let hp1 = march ray (distance h1) in
              if inside s2 hp1 then Just (record {distance = distance h1 + dist} h1)
              else run fuel (record {origin = hp1 + (2 * kEpsilon `scale` direction ray)} ray)
                       (dist + 2 * kEpsilon + distance h1)
        _ => Nothing

Solid Intersection where
  inside (MkIntersection s1 s2) p = inside s1 p && inside s2 p
  
  
intersection : SolidShape -> SolidShape -> SolidShape
intersection (MkSolidShape s1) (MkSolidShape s2) = MkSolidShape (MkIntersection s1 s2)


||| Type representing the subtraction of two shapes.
public export
data Subtraction : Type where
  MkSubtraction : (Solid s1, Solid s2) => s1 -> s2 -> Subtraction
  

Hitable Subtraction where
  hit (MkSubtraction s1 s2) ray = run csgFuel Nothing 0 ray where
    run : Nat -> Maybe Hit -> Double -> Ray -> Maybe Hit
    run Z _ _ _ = Nothing
    run (S fuel) result dist ray =
      if inside s2 (origin ray) then
        case hit s2 ray of
          Just h2 =>
            let p = march ray (distance h2) in
            if inside s1 p then
              Just (record {distance = distance h2 + dist} h2)
            else
              run fuel Nothing (dist + distance h2 + 2*kEpsilon)
                (record {origin = p + ((2 * kEpsilon) `scale` direction ray)} ray)
          Nothing => noHit
      else noHit where
        noHit : Lazy (Maybe Hit)
        noHit =
          case result of
            Just h => Just (record { distance = dist} h)
            Nothing =>
              case hit s1 ray of
                Nothing => Nothing
                h'@(Just h) => 
                  run fuel h' (dist + distance h)
                      (record {origin = march ray (distance h)} ray)        





Solid Subtraction where
  inside (MkSubtraction s1 s2) p = inside s1 p && not (inside s2 p)
  
  
subtraction : SolidShape -> SolidShape -> SolidShape
subtraction (MkSolidShape s1) (MkSolidShape s2) = MkSolidShape (MkSubtraction s1 s2)



record Group where
  constructor MkGroup
  shapes : ShapeList
  
Hitable Group where
  hit (MkGroup shs) ray = findClosestHit shs ray
  hitBefore (MkGroup shs) ray dbound = run shs
    where run [] = False
          run (sh :: shs') = 
            hitShapeBefore sh ray dbound || run shs'

export
group : ShapeList -> Shape
group = mkShape . MkGroup

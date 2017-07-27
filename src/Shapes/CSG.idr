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
              if distance h1 `eqEpsilon` distance h2 then
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

  hitBefore (MkIntersection s1 s2) ray dBound = run csgFuel ray 0 where
    retDist : Double -> Maybe Double
    retDist dist = if dist < dBound then Just dist else Nothing
    run : Nat -> Ray -> Double -> Maybe Double
    run Z _ _ = Nothing
    run (S fuel) ray dist =
      let dBound' = dBound - dist in
      case (hitBefore s1 ray dBound',hitBefore s2 ray dBound') of
        (Just h1, Just h2) => 
          if h1 `eqEpsilon` h2 then 
            retDist ((h1 `min` h2) + dist)
          else if h1 <= h2 then
            let hp1 = march ray h1 in
            if inside s2 hp1 then 
              retDist (h1 + dist)
            else 
              let hp2 = march ray h2 in
              if inside s1 hp2 then 
                retDist (h2 + dist)
              else 
                let dist' = dist + 2 * kEpsilon + h2 in
                if dist' < dBound then
                  run fuel (record {origin = hp2 + (2 * kEpsilon `scale` direction ray)} ray) dist'
                else Nothing
          else 
            let hp2 = march ray h2 in
            if inside s1 hp2 then retDist (h2 + dist)
            else 
              let hp1 = march ray h1 in
              if inside s2 hp1 then retDist (h1 + dist)
              else 
                let dist' = dist + 2 * kEpsilon + h1 in
                if dist' < dBound then 
                  run fuel (record {origin = hp1 + (2 * kEpsilon `scale` direction ray)} ray) dist'
                else Nothing
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
    mutual
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
            Nothing => run' fuel result dist ray
        else run' fuel  result dist ray
      run' : Nat -> Maybe Hit -> Double -> Ray -> Maybe Hit
      run' fuel result dist ray =
        case result of
          Just h => Just (record { distance = dist} h)
          Nothing =>
            case hit s1 ray of
              Nothing => Nothing
              h'@(Just h) => 
                run fuel h' (dist + distance h)
                    (record {origin = march ray (distance h)} ray)

  hitBefore (MkSubtraction s1 s2) ray dBound = run csgFuel False 0 ray where
    mutual
      run : Nat -> Bool -> Double -> Ray -> Maybe Double
      run Z _ _ _ = Nothing
      run (S fuel) result dist ray =
        if inside s2 (origin ray) then
          case hitBefore s2 ray (dBound - dist) of
            Just h2 =>
              let p = march ray h2
                  dist' = dist + h2 + 2*kEpsilon in
              if dist' < dBound then
                if inside s1 p then
                  Just dist'
                else
                  run fuel False dist'
                    (record {origin = p + ((2 * kEpsilon) `scale` direction ray)} ray)
              else Nothing
            Nothing => run' fuel result dist ray
        else run' fuel  result dist ray
      run' : Nat -> Bool -> Double -> Ray -> Maybe Double
      run' fuel result dist ray =
        if result then Just dist
        else
            case hitBefore s1 ray (dBound - dist) of
              Nothing => Nothing
              Just h =>
                let dist' = dist + h in
                if dist' < dBound then
                  run fuel True dist'
                    (record {origin = march ray h} ray)
                else Nothing



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
    where run [] = Nothing
          run (sh :: shs') = 
            case hitShapeBefore sh ray dbound of
              Nothing => run shs'
              h => h
  

export
group : ShapeList -> Shape
group = mkShape . MkGroup

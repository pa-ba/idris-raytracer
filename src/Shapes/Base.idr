||| This module collects all basic definitions for defining shapes
module Shapes.Base

import public LinearAlgebra
import public Colour
import public Transformation

%access export
%default total

kEpsilon : Double
kEpsilon = 0.00001


||| Equality up to epsilon
eqEpsilon : Double -> Double -> Bool
eqEpsilon d e = abs (d - e) < kEpsilon

||| This type collects the data that we need in case of a ray
||| intersection with a shape.
public export
record Hit where
  constructor MkHit
  distance : Double
  material : Lazy Material
  normal : Lazy Vector
      
%name Hit h

mutual
  public export
  interface Hitable s where
    hit : s -> Ray -> Maybe Hit
    hitBefore : s -> Ray -> Double -> Bool
    hitBefore s r d = 
      case hit s r of
        Just (MkHit distance _ _) => distance <= d
        Nothing => False
    transformedShape : s -> DecomposeTrans
    transformedShape _ = defaultTrans
    
  data DecomposeTrans : Type where
    NotTrans : DecomposeTrans
    Trans : Hitable s => Transformation -> s -> DecomposeTrans
    
  defaultTrans : DecomposeTrans
  defaultTrans = NotTrans

public export
data Shape : Type where
    MkShape : Hitable s => s -> Shape

public export
data TransformedShape : Type -> Type where
  MkTransformedShape : Transformation -> s -> TransformedShape s
    
Hitable s => Hitable (TransformedShape s) where
  hit (MkTransformedShape tr s) r =
   case hit s (inverseTransformRay tr r) of
      Nothing => Nothing
      Just h => Just $ record {normal $= transformNormal tr} h
      
  hitBefore (MkTransformedShape tr s) r d = hitBefore s (inverseTransformRay tr r) d

      
  transformedShape (MkTransformedShape t s) = Trans t s

public export
interface IsShape a where
  hitShape : a -> Ray -> Maybe Hit
  hitShapeBefore : a -> Ray -> Double -> Bool
  transform : Transformation -> a -> a
  

IsShape Shape where
  hitShape (MkShape s) r = hit s r
  
  hitShapeBefore (MkShape s) = hitBefore s

  transform tr (MkShape s) = 
    case transformedShape s of
      NoTrans => MkShape (MkTransformedShape tr s)
      Trans tr' s => MkShape (MkTransformedShape (merge tr' tr) s)
  
||| Construct a shape.    
mkShape : Hitable s => s -> Shape
mkShape s = MkShape s


total
closestHit : Maybe Hit -> Maybe Hit -> Maybe Hit
closestHit Nothing y = y
closestHit x Nothing = x
closestHit h1@(Just (MkHit d1 _ _)) h2@(Just (MkHit d2 _ _)) = 
  if d1 < d2 then h1 else h2


public export
data ShapeList : Type where
  Nil : ShapeList
  (::) : (IsShape s) => s -> ShapeList -> ShapeList

total
findClosestHit : ShapeList -> Ray -> Maybe Hit
findClosestHit shapes ray = run Nothing shapes
  where run : Maybe Hit -> ShapeList -> Maybe Hit
        run h [] = h
        run h (x :: xs) = 
          run (h `closestHit` hitShape x ray) xs

||| This module collects all basic definitions for defining shapes
module Shapes.Base

import public LinearAlgebra
import public Colour
import public Transformation

%access public export

||| This type collects the data that we need in case of a ray
||| intersection with a shape.
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
  
||| Construct a shape.    
mkShape : IsShape s => s -> Shape
mkShape s = MkShape s


total
export
closestHit : Maybe Hit -> Maybe Hit -> Maybe Hit
closestHit Nothing y = y
closestHit x Nothing = x
closestHit h1@(Just (MkHit d1 _ _)) h2@(Just (MkHit d2 _ _)) = 
  if d1 < d2 then h1 else h2


total
export
findClosestHit : List Shape -> Ray -> Maybe Hit
findClosestHit shapes ray = run Nothing shapes
  where run : Maybe Hit -> List Shape -> Maybe Hit
        run h [] = h
        run h (x :: xs) = 
          run (h `closestHit` hit x ray) xs

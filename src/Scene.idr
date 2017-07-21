module Scene

import Shapes
import LinearAlgebra as L
import Light
import Colour
import PPM


%access public export

record Camera where
  constructor MkCamera
  location, lookat : Point
  up : Vector
  zoom, width, height : Double
  pixelWidth, pixelHeight : Nat
  

record Scene where
  constructor MkScene
  shapes : List Shape
  lights : List Light
  ambient : Ambient
  maxReflect : Nat

total
closestHit : Maybe Hit -> Maybe Hit -> Maybe Hit
closestHit Nothing y = y
closestHit x Nothing = x
closestHit h1@(Just (MkHit d1 _ _)) h2@(Just (MkHit d2 _ _)) = 
  if d1 < d2 then h1 else h2


total
findClosestHit : List Shape -> Ray -> Maybe Hit
findClosestHit shapes ray = run Nothing shapes
  where run : Maybe Hit -> List Shape -> Maybe Hit
        run h [] = h
        run h (x :: xs) = 
          run (h `closestHit` hit x ray) xs

total
findShadowHit : List Shape -> Ray -> Double -> Bool
findShadowHit [] r d = False
findShadowHit (x :: xs) r d = isJust (hitBefore x r d) || findShadowHit xs r d


traceShadowRays : List Shape -> List Light -> Ambient -> Point -> Vector -> Colour
traceShadowRays shapes ls (MkAmbient acol) p n = run acol ls
  where run : Colour -> List Light -> Colour
        run acc [] = acc
        run acc ((MkLight location lcol) :: xs) = 
          let d = L.direction p location
              m = magnitude d
              nd = d / m -- normalise d
              r = MkRay p nd in
          if findShadowHit shapes r m
          then run acc xs
          else
           let cos = dot n nd in
           if cos > 0.00001 then run (acc + (cos `scale` lcol)) xs else run acc xs

render : (fileName : String) -> Scene -> Camera -> IO ()
render fileName (MkScene shapes lights ambient maxRefl) 
  (MkCamera location lookat up zoom width height pixelWidth pixelHeight) =
    writePPM fileName pixelWidth pixelHeight tracePixel
  where w : Vector
        w = L.normalise (L.direction lookat location)
        u : Vector
        u = L.normalise  (up `cross` w)
        v : Vector
        v = w `cross` u
        resWidth : Double
        resWidth = width / (cast pixelWidth)
        resHeight : Double
        resHeight = height / (cast pixelHeight)
        shootRay : (reflect : Nat) -> Ray -> Colour
        shootRay reflect ray@(MkRay rLoc rDir) = 
          case findClosestHit shapes ray of
            Just (MkHit distance (MkMaterial colour reflection) normal) => 
              let normal' = if (rDir `dot` normal) < 0 then normal else negate normal
                  p = rLoc + (distance `scale` rDir) + (0.00001 `scale` normal')
                  shading = traceShadowRays shapes lights ambient p normal'
                  colour' = colour * shading in
              if reflection > 0 then
                 case reflect of
                   Z => colour'
                   S reflect' => 
                     let rd = normalise (rDir - ((2.0 * (normal `dot` rDir) `scale` normal')))
                         colour'' = shootRay reflect' (MkRay p rd)
                     in (reflection `scale` colour'') + ((1-reflection) `scale` colour')
                 else colour'
            Nothing => black

        tracePixel : Nat -> Nat -> Colour
        tracePixel x y = 
            let xv = resWidth * (cast x - cast pixelWidth * 0.5)
                yv = resHeight * (cast y - cast pixelHeight * 0.5)
                direction = normalise(((xv `scale` u) + (yv `scale` v)) - (zoom `scale` w))
                ray = MkRay location direction
            in shootRay maxRefl ray

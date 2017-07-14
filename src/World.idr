module World

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
  pixelWidth, pixelHeight : Int
  

record Scene where
  constructor MkScene
  shapes : List Shape
  ambient : Ambient
  lights : List Light

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
        run h (MkShape x :: xs) = 
          run (h `closestHit` hit x ray) xs

total
findShadowHit : List Shape -> Ray -> Bool
findShadowHit [] r = False
findShadowHit (MkShape x :: xs) r = shadowHit x r || findShadowHit xs r


traceShadowRays : List Shape -> List Light -> Ambient -> Point -> Vector -> Colour
traceShadowRays shapes ls (MkAmbient acol) p n = run acol ls
  where run : Colour -> List Light -> Colour
        run acc [] = acc
        run acc ((MkLight location lcol) :: xs) = 
          let d = L.direction p location
              m = magnitude d
              nd = d / m -- normalise d
              r = MkRay p d in
          if findShadowHit shapes r
          then run acc xs
          else
           let cos = dot n nd in
           if cos > 0.00001 then run (acc + (cos `scale` lcol)) xs else run acc xs


render : (fileName : String) -> Scene -> Camera -> IO ()
render fileName (MkScene shapes ambient lights) 
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
        tracePixel : Int -> Int -> RGB
        tracePixel x y = 
            let xv = resWidth * (cast x - cast pixelWidth * 0.5)
                yv = resHeight * (cast y - cast pixelHeight * 0.5)
                direction = normalise(((xv `scale` u) + (yv `scale` v)) - (zoom `scale` w))
                ray = MkRay location direction
                hit = findClosestHit shapes ray
                in case hit of
                 Just (MkHit distance (MkMaterial colour reflection) normal) 
                   => let normal' = if (direction `dot` normal) < 0 then normal else negate normal
                          p = location + (distance `scale` direction) + (0.00001 `scale` normal')
                          shading = traceShadowRays shapes lights ambient p normal'
                          colour' = colour * shading
                      in cast colour'
                 Nothing => MkRGB 0 0 0

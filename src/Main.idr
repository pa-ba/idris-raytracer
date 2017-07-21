module Main

import World
import LinearAlgebra
import Light
import Shapes
import Colour
import Transformation


renderHollowCylinder : IO ()
renderHollowCylinder =
    let light = MkLight (mkPoint 2.0 3.0 4.0) (MkColour 1 1 1)
        ambientLight = MkAmbient (MkColour 0.1 0.1 0.1)
        cylinder = mkCylinder (mkPoint 0 0 0) 2.0 1.0 (MkConstTexture (MkMaterial (MkColour 1 1 0) 0.0))
        camera = MkCamera (mkPoint 0.0 10.0 20.0) (mkPoint 0.0 0.0 0.0) (MkVector 0.0 1.0 (negate 0.5)) 18.0 4.0 4.0 500 500;
        scene = MkScene [cylinder] [light] ambientLight 0
    in render "hollow cylinder.ppm" scene camera

renderSphere : IO ()
renderSphere = 
  let light = MkLight (mkPoint 0.0 0.0 4.0) (MkColour 1 1 1)
      ambientLight = MkAmbient (MkColour 0.1 0.1 0.1)
      sphere = mkSphere (mkPoint 0 0 0) 1.0 (MkConstTexture (MkMaterial (MkColour 0 0 1) 0.0))
      camera = MkCamera (mkPoint 0.0 0.0 4.0) (mkPoint 0.0 0.0 0.0) (MkVector 0.0 1.0 0.0) 1.0 2.0 2.0 500 500
      scene = MkScene [sphere] [light] ambientLight 0
  in render "sphere.ppm" scene camera

renderReflectiveSpheres : IO ()
renderReflectiveSpheres = 
  let light = MkLight (mkPoint 0.0 0.0 2.0) (MkColour 1 1 1)
      ambientLight = MkAmbient (MkColour 0.2 0.2 0.2)
      sphere = mkSphere (mkPoint (-0.8) 0.0 0.0) 0.7 (MkConstTexture (MkMaterial (MkColour 0 0 1) 0.7))
      sphere2 = mkSphere (mkPoint 0.8 0.0 0.0) 0.7 (MkConstTexture (MkMaterial (MkColour 1 0 0) 0.7))
      camera = MkCamera (mkPoint 0.0 0.0 2.0) (mkPoint 0.0 0.0 0.0) (MkVector 0.0 1.0 0.0) 1.0 2.0 2.0 500 500;
      scene = MkScene [sphere,sphere2] [light] ambientLight 4
  in render "reflective spheres.ppm" scene camera


checkered : (white, black : Material) -> Texture
checkered white black = MkTexture tex
  where abs' : Double -> Double -> Double
        abs' s f = if f < 0.0 then 1.0 - (f * s) else f * s
        tex : Double -> Double -> Material
        tex x y =
          if ((prim__fromFloatInt (abs' 64.0 x) + prim__fromFloatInt (abs' 32.0 y)) `mod` 2) == 0
          then white
          else black

renderTexSphere : IO ()
renderTexSphere =
  let texture = checkered (MkMaterial Colour.red 0.5) (MkMaterial Colour.green 0.5)
      light = MkLight (mkPoint 0.0 1.0 4.0) (MkColour 1 1 1)
      ambientLight = MkAmbient (MkColour 0.1 0.1 0.1)
      sphere = transform (rotateX (Doubles.pi/4.0)) (mkSphere (mkPoint 0.0 0.0 0.0) 1.0 texture)
      camera = MkCamera (mkPoint 0.0 1.0 30.0) (mkPoint 0.0 0.0 0.0) (MkVector 0.0 1.0 0.0) 20.0 2.0 2.0 1000 1000;
      scene = MkScene [sphere] [light] ambientLight 3
   in render "textured sphere.ppm" scene camera




l1 : Light
l1 = MkLight (mkPoint 4.0 0.0 4.0) Colour.white

l2 : Light
l2 = MkLight (mkPoint (-4.0) 0.0 4.0) Colour.white

l3 : Light
l3 = MkLight (mkPoint 0.0 0.0 0.0) Colour.white

l4 : Light
l4 = MkLight (mkPoint 2.0 4.0 4.0) Colour.white

l5 : Light
l5 = MkLight (mkPoint 0.0 (-4.0) 0.0) Colour.white

ambientLight : Ambient
ambientLight = MkAmbient (MkColour 0.2 0.2 0.2)

camera : Camera
camera = MkCamera (mkPoint 16.0 16.0 16.0) (mkPoint 0.0 0.0 0.0) (MkVector (-1.0) 1.0 (-1.0)) 8.0 1.4 1.4 500 500
camera2 : Camera
camera2 = MkCamera (mkPoint 0.0 0.0 4.0) (mkPoint 0.0 0.0 0.0) (MkVector 0.0 1.0 0.0) 2.0 2.0 2.0 500 500

sphere : Shape
sphere = mkSphere (mkPoint 0.0 0.0 0.0) 1.3 (MkConstTexture (MkMaterial (Colour.blue) 0.0))

sphere1 : Shape
sphere1 = mkSphere (mkPoint 0.5 0.0 0.0) 1.0 (MkConstTexture (MkMaterial (Colour.blue) 0.0))

sphere2 : Shape
sphere2 = mkSphere (mkPoint (-0.5) 0.0 0.0) 1.0 (MkConstTexture (MkMaterial (Colour.red) 0.0))

sphere3 : Shape
sphere3 = mkSphere (mkPoint (-0.5) 0.0 0.0) 0.2 (MkConstTexture (MkMaterial (Colour.yellow) 0.0))



renderUnion : IO ()
renderUnion =
  let scene = MkScene [union sphere1 sphere2] [l1, l2] (ambientLight) 0
      camera = camera2
  in render "union.ppm" scene camera

main : IO ()
main = do
  -- renderReflectiveSpheres
  -- renderHollowCylinder
  -- renderSphere
  -- renderTexSphere
  renderUnion

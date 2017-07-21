module Examples.CSG

import Scene
import LinearAlgebra
import Light
import Shapes
import Colour
import Transformation

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


%access export

renderUnion : IO ()
renderUnion =
  let scene = MkScene [union sphere1 sphere2] [l1, l2] (ambientLight) 0
      camera = camera2
  in render "union.ppm" scene camera

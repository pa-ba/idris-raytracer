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
        cylinder = mkCylinder (mkPoint 0 0 0) 2.0 1.0 (Constant (MkMaterial (MkColour 1 1 0) 0.0))
        camera = MkCamera (mkPoint 0.0 10.0 20.0) (mkPoint 0.0 0.0 0.0) (MkVector 0.0 1.0 (negate 0.5)) 18.0 4.0 4.0 500 500;
        scene = MkScene [cylinder] [light] ambientLight
    in render "hollow cylinder.ppm" scene camera

renderSphere : IO ()
renderSphere = 
  let light = MkLight (mkPoint 0.0 0.0 4.0) (MkColour 1 1 1)
      ambientLight = MkAmbient (MkColour 0.1 0.1 0.1)
      sphere = mkSphere (mkPoint 0 0 0) 1.0 (Constant (MkMaterial (MkColour 0 0 1) 0.0))
      camera = MkCamera (mkPoint 0.0 0.0 4.0) (mkPoint 0.0 0.0 0.0) (MkVector 0.0 1.0 0.0) 1.0 2.0 2.0 500 500
      scene = MkScene [sphere] [light] ambientLight 
  in render "sphere.ppm" scene camera

main : IO ()
main = renderHollowCylinder
  --renderSphere

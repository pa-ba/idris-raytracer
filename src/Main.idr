module Main

import World
import LinearAlgebra
import Light
import Shapes
import Colour



main : IO ()
main = 
  let light = MkLight (mkPoint 0.0 0.0 4.0) (MkColour 1 1 1)
      ambientLight = MkAmbient (MkColour 0.1 0.1 0.1)
      sphere = MkShape (MkSphere 1.0 (Constant (MkMaterial (MkColour 0 0 1) 0.0)))
      camera = MkCamera (mkPoint 0.0 0.0 4.0) (mkPoint 0.0 0.0 0.0) (MkVector 0.0 1.0 0.0) 1.0 2.0 2.0 1000 1000
      scene = MkScene [sphere] ambientLight [light]
  in render "sphere.ppm" scene camera

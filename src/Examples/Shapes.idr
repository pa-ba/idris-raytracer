module Examples.Shapes

import Scene

%access export 

renderHollowCylinder : IO ()
renderHollowCylinder =
    let light = MkLight (mkPoint 2.0 3.0 4.0) (MkColour 1 1 1)
        ambientLight = MkAmbient (MkColour 0.1 0.1 0.1)
        cylinder = mkCylinder (mkPoint 0 0 0) 2.0 1.0 (MkConstTexture (MkMaterial (MkColour 1 1 0) 0.0))
        camera = MkCamera (mkPoint 0.0 10.0 20.0) (mkPoint 0.0 0.0 0.0) (MkVector 0.0 1.0 (-0.5)) 18.0 4.0 4.0 500 500
        scene = MkScene [cylinder] [light] ambientLight 0
    in render "hollowCylinder.ppm" scene camera

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
  in render "reflectiveSpheres.ppm" scene camera


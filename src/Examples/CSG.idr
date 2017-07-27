module Examples.CSG

import Scene


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


-- If I don't have the texture argument lazy I get a sigfault!
mkUnitBox : Lazy Texture -> SolidShape
mkUnitBox t = mkBox (mkPoint (-1) (-1) (-1)) (mkPoint 1.0 1.0 1.0) t t t t t t

cube : SolidShape
cube = mkUnitBox (MkConstTexture (MkMaterial Colour.red 0.0))


sphere : SolidShape
sphere = mkSphere (mkPoint 0.0 0.0 0.0) 1.3 (MkConstTexture (MkMaterial (Colour.blue) 0.0))

sphere1 : SolidShape
sphere1 = mkSphere (mkPoint 0.5 0.0 0.0) 1.0 (MkConstTexture (MkMaterial (Colour.blue) 0.0))

sphere2 : SolidShape
sphere2 = mkSphere (mkPoint (-0.5) 0.0 0.0) 1.0 (MkConstTexture (MkMaterial (Colour.red) 0.0))

sphere3 : SolidShape
sphere3 = mkSphere (mkPoint (-0.5) 0.0 0.0) 0.2 (MkConstTexture (MkMaterial (Colour.yellow) 0.0))


-- If I don't have the texture argument lazy I get a sigfault!
mkUnitCylinder : Lazy Texture -> SolidShape
mkUnitCylinder t = mkSolidCylinder (mkPoint 0.0 0.0 0.0) 1.0 2.0 t t t



cross : SolidShape
cross =
  let cy = transform (scale 0.7 1.5 0.7) (mkUnitCylinder (MkConstTexture $ MkMaterial yellow 0.0))
      cx = transform (rotateX (pi/2)) cy 
      cz = transform (rotateZ (pi/2)) cy 
  in union cy (union cz cx)

%access export

renderUnion : IO ()
renderUnion =
  let scene = MkScene [CSG.union sphere1 sphere2] [l1, l2] (ambientLight) 0
      camera = camera2
  in render "union.ppm" scene camera
  
  
renderIntersection : IO ()
renderIntersection =
  let scene = MkScene [intersection sphere1 sphere2] [l1, l2 ] ambientLight 0
  in render "intersection.ppm" scene camera2

renderIntersection2 : IO ()
renderIntersection2 =
  let scene = MkScene [intersection cube sphere] [l1, l2] ambientLight 0
  in render "intersection2.ppm" scene camera


renderLantern : IO ()
renderLantern =
  let scene = MkScene [subtraction (intersection cube sphere ) cross] [l1, l2, l3 ] ambientLight 0
  in render "lantern.ppm" scene camera

renderSubtraction : IO ()
renderSubtraction =
  let scene = MkScene [subtraction sphere2 sphere1] [l1, l2 ] ambientLight 0 ;
  in render "subtraction.ppm" scene camera

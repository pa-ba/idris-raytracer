module Examples.Texture

import Scene

%access export

checkered : (scalex, scaley : Double) -> (white, black : Material) -> Texture
checkered scalex scaley white black = MkTexture tex
  where abs' : Double -> Double -> Double
        abs' s f = if f < 0.0 then 1.0 - (f * s) else f * s
        tex : Double -> Double -> Material
        tex x y =
          if ((prim__fromFloatInt (abs' scalex x) + prim__fromFloatInt (abs' scaley y)) `mod` 2) == 0
          then white
          else black

renderTexSphere : IO ()
renderTexSphere =
  let texture = checkered 64 32 (MkMaterial Colour.red 0.5) (MkMaterial Colour.green 0.5)
      light = MkLight (mkPoint 0.0 1.0 4.0) (MkColour 1 1 1)
      ambientLight = MkAmbient (MkColour 0.1 0.1 0.1)
      sphere = transform (rotateX (Doubles.pi/4.0)) (mkSphere (mkPoint 0.0 0.0 0.0) 1.0 texture)
      camera = MkCamera (mkPoint 0.0 1.0 30.0) (mkPoint 0.0 0.0 0.0) (MkVector 0.0 1.0 0.0) 20.0 2.0 2.0 1000 1000;
      scene = MkScene [sphere] [light] ambientLight 3
   in render "texturedSphere.ppm" scene camera

renderDiscs : IO ()
renderDiscs =
  let disc = mkDisc (mkPoint 0.0 0.0 0.0) 0.7 (MkTexture checker) 
      d1 = transform (translate (-0.5) (-0.5) (-0.5)) disc
      d2 = transform (mergeAll [rotateX (pi/(-4.0)),translate 0.5 0.5 0.5]) disc
      light = MkLight (mkPoint 0.0 1.0 4.0) (white)
      ambientLight = MkAmbient (0.1 `scale` white) 
      camera = MkCamera (mkPoint 0.0 0.0 30.0) (mkPoint 0.0 0.0 0.0) (MkVector 0.0 1.0 0.0) 20.0 2.0 2.0 1000 1000;
      scene = MkScene [d1,d2] [light] ambientLight 2
  in render "discs.ppm" scene camera
  where mkMat : Colour -> Material
        mkMat c = MkMaterial c 0.0
        colours : List Material
        colours = map mkMat [green, red,blue,yellow,magenta,orange,cyan,white]
        checker : Double -> Double -> Material
        checker x' y' =
          let x = 2.0*x' - 1.0
              y = 2.0*y' - 1.0
              a = atan2 x y
              a' = if a < 0.0 then a + 2.0 * pi else a
              d = prim__fromFloatInt (4.0*(a' / pi)) + if x * x + y * y <= 0.25 then 4 else 0
          in fromMaybe (MkMaterial white 0.0) (List.index' (cast (d `mod` 8)) colours)

renderSolidCylinder : IO ()
renderSolidCylinder =
  let white = MkMaterial red 0.0
      black = MkMaterial green 0.0
      checker = checkered 64 64 white black
      cbase = mkSolidCylinder (mkPoint 0.0 0.0 0.0) 0.5 1.9 checker
                  checker (MkConstTexture (MkMaterial Colour.white 0.0))
      c = transform (rotateX (pi/4.0)) cbase 
      light = MkLight (mkPoint 0.0 1.0 4.0) Colour.white
      ambientLight = MkAmbient (0.1 `scale` Colour.white)
      camera = MkCamera (mkPoint 0.0 0.0 30.0) (mkPoint 0.0 0.0 0.0) (MkVector 0.0 1.0 0.0) 20.0 2.0 2.0 1000 1000;
      scene = MkScene [c] [light] ambientLight 2
  in render "solidCylinder.ppm" scene camera

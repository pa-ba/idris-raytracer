module Colour

%default total
%access public export

record Colour where
  constructor MkColour
  r, g, b : Double

%name Colour c

Show Colour where
  show (MkColour r g b) = show r ++ ", " ++ show g ++ ", " ++ show b

Num Colour where
  (*) (MkColour r g b) (MkColour r' g' b') = MkColour (r * r') (g * g') (b * b')

  (+) (MkColour r g b) (MkColour r' g' b') = MkColour (r + r') (g + g') (b + b')

  fromInteger i = MkColour n n n
    where n = fromInteger i

scale : Double -> Colour -> Colour
scale s (MkColour r g b) = MkColour (s * r) (s * g) (s * b)




record Material where
  constructor MkMaterial
  colour : Colour
  reflection : Double
  
%name Material mat

public export
data Texture = MkConstTexture Material | MkTexture (Double -> Double -> Material)

record RGB where
  constructor MkRGB
  r, g, b : Int
  
Cast Colour RGB where
  cast (MkColour r g b) = 
    MkRGB (min (cast (sqrt r * 255)) 255) (min (cast (sqrt g * 255)) 255) (min (cast (sqrt b * 255)) 255)


black : Colour
black = MkColour 0 0 0

yellow : Colour
yellow = MkColour 1 1 0

white : Colour
white = MkColour 1 1 1

red : Colour
red = MkColour 1 0 0

green : Colour
green = MkColour 0 0.25 0

blue : Colour
blue = MkColour 0 0 1

magenta : Colour
magenta = MkColour 1 0 1

cyan : Colour
cyan = MkColour 0 1 1

orange : Colour
orange = MkColour 0.42 1 1

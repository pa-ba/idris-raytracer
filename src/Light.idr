module Light

import Colour
import LinearAlgebra

%access public export

record Ambient where
  constructor MkAmbient
  colour : Colour

record Light where
  constructor MkLight
  location : Point
  colour : Colour
  

module Light

import Colour
import LinearAlgebra

%access export
%default total

record Ambient where
  constructor MkAmbient
  colour : Colour

record Light where
  constructor MkLight
  location : Point
  colour : Colour
  

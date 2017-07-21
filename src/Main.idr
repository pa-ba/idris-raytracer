module Main

import Examples.CSG
import Examples.Shapes
import Examples.Texture


main : IO ()
main = do
  renderReflectiveSpheres
  renderHollowCylinder
  renderSphere
  renderTexSphere
  renderUnion

module PPM

import Colour
import Data.Buffer

%default total

private
writeLines : (file : File) -> (width, height : Nat) -> (render : (x, y : Nat) -> Colour) -> IO ()
writeLines file width@(S width') height@(S height') render = 
  do Just buf <- newBuffer (3 * (cast width * cast height))
     | Nothing => putStr "cannot allocate buffer"
     run 0 buf Z width' height'
  where run : Int -> Buffer -> Nat -> Nat -> Nat -> IO ()
        run loc buf rx x y =
          let loc' = loc + 3 in
          case render rx y of
            MkColour r g b => do
              setByte buf loc (prim__truncInt_B8 (min (prim__fromFloatInt ((sqrt r) * 255)) 255))
              setByte buf (loc + 1) (prim__truncInt_B8 (min (prim__fromFloatInt ((sqrt g) * 255)) 255))
              setByte buf (loc + 2) (prim__truncInt_B8 (min (prim__fromFloatInt ((sqrt b) * 255)) 255))
              case x of
                S x' => run loc' buf (S rx) x' y
                Z => case y of
                       S y' => run loc' buf Z width' y'
                       Z => do writeBufferToFile file buf loc'
                               pure ()
writeLines _ _ _ _ = pure ()

export
writePPM : (fileName : String) -> (width, height : Nat) -> (render : (x, y : Nat) -> Colour) -> IO ()
writePPM fileName width height render = do
  Right file <- openFile fileName WriteTruncate
  | Left _ => pure ()
  fPutStrLn file "P6"
  fPutStrLn file (show width ++ " " ++ show height)
  fPutStrLn file "255"
  writeLines file width height render
  pure () 

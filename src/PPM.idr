module PPM

import Colour

%default total

private
writeLines : (file : File) -> (width, height : Nat) -> (render : (x, y : Nat) -> RGB) -> IO ()
writeLines file (S width') (S height') render = run Z width' height'
  where run : Nat -> Nat -> Nat -> IO ()
        run rx x y =
          case render rx y of
            MkRGB r g b => do
              fPutStr file (show r ++ " " ++ show g ++ " " ++ show b ++ "\n")
              case x of
                S x' => run (S rx) x' y
                Z => case y of
                       S y' => run Z width' y'
                       Z => pure ()
writeLines _ _ _ _ = pure ()

export
writePPM : (fileName : String) -> (width, height : Nat) -> (render : (x, y : Nat) -> RGB) -> IO ()
writePPM fileName width height render = do
  Right file <- openFile fileName WriteTruncate
  | Left _ => pure ()
  fPutStrLn file "P3"
  fPutStrLn file (show width ++ " " ++ show height)
  fPutStrLn file "255"
  writeLines file width height render
  pure ()

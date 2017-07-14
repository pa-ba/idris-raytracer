module PPM

import Colour


private
writeLines : (file : File) -> (width', height' : Int) -> (render : (x, y : Int) -> RGB) -> IO ()
writeLines file width' height' render = run 0 height'
  where run : Int -> Int -> IO ()
        run x y =
          case render x y of
            MkRGB r g b => do
              
              fPutStr file (show r ++ " " ++ show g ++ " " ++ show b ++ "\n")
              if x < width' 
                then run (x + 1) y
                else if y > 0
                  then run 0 (y - 1)
                  else pure ()

export
writePPM : (fileName : String) -> (width, height : Int) -> (render : (x, y : Int) -> RGB) -> IO ()
writePPM fileName width height render = do
  Right file <- openFile fileName WriteTruncate
  | Left _ => pure ()
  fPutStrLn file "P3"
  fPutStrLn file (show width ++ " " ++ show height)
  fPutStrLn file "255"
  writeLines file (width-1) (height-1) render
  pure ()

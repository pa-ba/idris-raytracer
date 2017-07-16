module PPM

import Colour
import Data.Buffer
import System
import System.Concurrency.Channels


%inline
floatToByte : Double -> Bits8
floatToByte d = prim__truncInt_B8 (min (prim__fromFloatInt ((sqrt d) * 255)) 255)

parallel : Nat
parallel = 12

private
writeLines : (file : File) -> (width, height : Nat) -> (render : (x, y : Nat) -> Colour) -> IO ()
writeLines file width@(S width') height@(S height') render = 
  do Just buf <- newBuffer (3 * (cast width * cast height))
     | Nothing => putStr "cannot allocate buffer"
     if parallel > 1
       then run' [] 0 buf height'
       else run 0 buf Z width' height' height
     writeBufferToFile file buf (3 * (cast width * cast height))
     pure ()
  where chunkSize : Nat
        chunkSize = (S height') `div` parallel
        run : Int -> Buffer -> Nat -> Nat -> Nat -> Nat -> IO ()
        run _ _ _ _ _ Z = pure ()
        run loc buf rx x y count@(S count') =
          let loc' = loc + 3 in
          case render rx y of
            MkColour r g b => do
              setByte buf loc (floatToByte r)
              setByte buf (loc + 1) (floatToByte g)
              setByte buf (loc + 2) (floatToByte b)
              case x of
                S x' => run loc' buf (S rx) x' y count
                Z => case y of
                       S y' => run loc' buf Z width' y' count'
                       Z => pure ()
        waitForChildren : List Channel -> IO ()
        waitForChildren [] = pure ()
        waitForChildren (s :: ss) = do
          Just () <- unsafeRecv () s
          | Nothing => waitForChildren ss
          waitForChildren ss
        run' : List Channel -> Int -> Buffer -> Nat -> IO ()
        run' ss _ _ Z = waitForChildren ss
        run' ss loc buf y = do
          Just pid <- spawn $ do
            Just sess <- listen 1000000
            | Nothing => putStrLn "cannot listen to parent thread"
            run loc buf Z width' y chunkSize
            unsafeSend sess ()
            pure ()
          | Nothing => putStrLn "cannot spawn thread!"
          Just sess <- connect pid
          | Nothing => putStrLn "cannot connect to child thread"
          run' (sess :: ss) (loc + (3 * cast (S width') * cast chunkSize)) buf (y `minus` chunkSize)
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
  closeFile file
  pure () 

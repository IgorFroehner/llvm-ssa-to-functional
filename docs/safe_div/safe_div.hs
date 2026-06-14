import Data.Bits
import Data.Int
import Data.Word
import GHC.Float (float2Double, double2Float)

safe_div :: Int32 -> Int32 -> Int32
safe_div a0 a1 =
  let
    a2 () =
      let
        a3 = (a1 == 0) :: Bool
        a4 () =
          let
            a5 = (a0 `quot` a1) :: Int32
          in a6 a5
        a6 a7 =
          let
          in a7 
      in if a3
        then a6 (-1)
        else a4 ()
  in a2  ()

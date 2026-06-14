import Data.Bits
import Data.Int
import Data.Word
import GHC.Float (float2Double, double2Float)

is_prime :: Int32 -> Int32
is_prime a0 =
  let
    a1 () =
      let
        a2 = (a0 < 2) :: Bool
        a3 () =
          let
            a4 = (a0 < 4) :: Bool
            a5 = (a0 .&. 1) :: Int32
            a6 = (a5 == 0) :: Bool
            a7 = (a4 || a6) :: Bool
            a8 a9 =
              let
                a10 = (a9 + 1) :: Int32
                a11 = (a10 * a10) :: Int32
                a12 = (a11 > a0) :: Bool
                a13 () =
                  let
                    a14 = (a0 `rem` a10) :: Int32
                    a15 = (a14 == 0) :: Bool
                  in if a15
                    then a16 a12
                    else a8 a10
              in if a12
                then a16 a12
                else a13 ()
            a16 a17 =
              let
                a18 = (if a17 then 1 else 0) :: Int32
              in a19 a18
          in if a7
            then a16 a4
            else a8 2
        a19 a20 =
          let
          in a20 
      in if a2
        then a19 0
        else a3 ()
  in a1  ()

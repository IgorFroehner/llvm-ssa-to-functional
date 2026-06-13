import Data.Bits
import Data.Int
import Data.Word

is_prime :: Int32 -> Int32
is_prime a0 =
  let
    a1 () =
      let
        a2 = (if a0 < 2 then 1 else 0) :: Int8
        a3 () =
          let
            a4 = (if a0 < 4 then 1 else 0) :: Int8
            a5 = (a0 .&. 1) :: Int32
            a6 = (if a5 == 0 then 1 else 0) :: Int8
            a7 = (a4 .|. a6) :: Int8
            a8 a9 =
              let
                a10 = (a9 + 1) :: Int32
                a11 = (a10 * a10) :: Int32
                a12 = (if a11 > a0 then 1 else 0) :: Int8
                a13 () =
                  let
                    a14 = (a0 `rem` a10) :: Int32
                    a15 = (if a14 == 0 then 1 else 0) :: Int8
                  in if a15 /= 0
                    then a16 a12
                    else a8 a10
              in if a12 /= 0
                then a16 a12
                else a13 ()
            a16 a17 =
              let
                a18 = fromIntegral (fromIntegral a17 :: Word8) :: Int32
              in a19 a18
          in if a7 /= 0
            then a16 a4
            else a8 2
        a19 a20 =
          let
          in a20 
      in if a2 /= 0
        then a19 0
        else a3 ()
  in a1  ()

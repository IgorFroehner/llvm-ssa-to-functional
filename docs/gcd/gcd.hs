import Data.Bits

euclides_gcd a0 a1 =
  let
    a2 () =
      let
        a3 a4 a5 =
          let
            a6 = if a5 == 0 then 1 else 0
            a7 () =
              let
                a8 = a4 `mod` a5
              in a3 a5 a8
            a9 () =
              let
              in a4 
          in if a6 /= 0
            then a9 ()
            else a7 ()
      in a3 a0 a1
  in a2  ()

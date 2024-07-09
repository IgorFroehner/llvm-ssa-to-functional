import Data.Bits

safe_div a0 a1 =
  let a2 () =
        let a3 = if a1 == 0 then 1 else 0
            a4 () =
              let a5 = a0 `div` a1
              in a6 a5
            a6 a7 =
              let 
              in a7
        in if a3 /= 0
          then a6 (-1)
          else a4 ()
  in a2 ()

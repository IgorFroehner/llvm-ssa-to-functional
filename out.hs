import Data.Bits

safe_div an ad =
  let
  a1 =
    let
    a1 = if ad == 0 then 1 else 0
    a2 = an `div` ad
    a3 = if a1 /= 0 then (-1) else a2
    in a3 
  in a1

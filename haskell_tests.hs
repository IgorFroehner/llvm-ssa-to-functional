
factorial :: Integer -> Integer
factorial n = let
    go n acc = 
        if n <= 1 
        then acc 
        else
          let
            n' = n - 1
            acc' = n * acc
          in go n' acc'
    in go n 1

-- main_ret.ll
main' = let
  b = foo 42
  in b

foo n = let
  in n

-- safe_div.ll
safe_div n d = let
  a = if d == 0 then 1 else 0
  in if a /= 0
    then -1
    else let
      b = n `div` d
      in b

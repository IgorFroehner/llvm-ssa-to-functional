
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

fib n = let
  a = if n == 0 then 1 else 0 -- icmp eq i32 %n, 0
  in
    if a == 0 then
      let 
        f3 a4 a5 a6 = 
          let
            a7 = a4 - 1
            a8 = a5 + a6
            a9 = if a7 == 0 then 1 else 0
          in
            if a9 == 0 then
              f3 a7 a8 a5
            else
              let f10 a11 = a11 in f10 a8
      in f3 n 0 1
    else 
      let f12 a13 = a13 in f12 1

test = ()

main'' = let in                         -- |
  let ------------------ |                 |
    a =               -- | \               | \
      let             -- | |               | | FunctionDef
        b = foo 42    -- | | Block 1       | /
      in b            -- | /               |
    in a  -------------- | --------------- |

main_ret = let
  in
  let
    a a2 a3 a4 =
      let
        a5 = a2 + a3
        a6 = a5 + a4
      in
        a6
  in a 1 2 3

sequence = let in
  let
    f =
      let
        a = 1
        b = 2
        c = 3
      in
        a + b + c
    in f

safe_div' n d = let in
  let 
    f =
      let
        a = if d == 0 then 1 else 0
        iszero = let
                 in -1
        nonzero = let
                    b = n `div` d
                  in b
        in if a /= 0
          then iszero
          else nonzero
    in f

safe_div'' an ad = let in
  let 
    f =
      let
        a1 = if ad == 0 then 1 else 0
        a2 = an `div` ad
        a3 = if a1 /= 0 then (-1) else a2
      in a3
    in f

safe_div''' an ad = let in
  let 
    f =
      let
        a1 = if ad == 0 then 1 else 0
        a2 = an `div` ad
        a3 = if a1 /= 0 then (-1) else a2
      in a3
    in f

square ax = let in
  let 
    f =
      let
        a1 = ax * ax
      in a1
    in f

no_overflow_square ax = let in
  let 
    f =
      let
        a1 = ax
        a2 = a1 * a1
      in a2
    in f

pow ax ay = let in
  let
    fstart =
      let
        loop_start ai0 ar0 =
          let
            done = if ai0 == ay then 1 else 0
            exit = let in ar0
            loop = let
                arnew = ar0 * ax
                ainew = ai0 + 1
              in if done == 1
                then exit
                else loop_start ainew arnew
          in if done == 1
            then exit
            else loop
      in loop_start 0 1
    in fstart

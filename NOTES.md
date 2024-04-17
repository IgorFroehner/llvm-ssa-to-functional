
# Translation

In the LLVM IR we have the following structure:

```
Function (name, arguments): [
    Blocks (option label): [
        Operations...
    ]
]
```

Then it translated to Haskell like in ANF:

```ruby
Function (name, args, blocks):
    "#{name} #{args} = let in" \
    "  #{translateBlocks blocks}" \
    " in #{name}"

Block (label, phis, stmts, flow):
    "let" \
    "  #{label} #{argsFromPhis phis} = " \
    "    let" \
    "      #{translateStmts stmts}" \
    "      #{inlineCalledBlocks self flow}" \
    "    in #{translateFlows flow}" \
    "  in #{blockCall block}" \

Flow:
    {
        Br (cond, dest1, dest2):
            "if #{cond} == 1" \
            "  then dest1" \
            "  else dest2" \
        Return (var):
            "in #{var}"
    }
```

## Examples

### Select

One of the most basic examples, receiving two arguments and using a select operation.

```llvm
define i64 @safe_div(i64 %n, i64 %d) {
  %1 = icmp eq i64 %d, 0
  %2 = udiv i64 %n, %d
  %3 = select i1 %1, i64 -1, i64 %2
  ret i64 %3
}
```

```haskell
safe_div an ad = let in
  let 
    f =
      let
        a1 = if ad == 0 then 1 else 0
        a2 = an `div` ad
        a3 = if a1 /= 0 then (-1) else a2
      in a3
    in f
```

### Safe Div with Flow

This example already includes block definitions and useage of a branching with the `br` operator.

```llvm
define i64 @safe_div(i64 %n, i64 %d) {
  %1 = icmp eq i64 %d, 0
  br i1 %1, label %iszero, label %nonzero

iszero:
  ret i64 -1

nonzero:
  %2 = udiv i64 %n, %d
  ret i64 %2
}
```

```haskell
safe_div an ad = let in
  let 
    f =
      let
        a1 = if ad == 0 then 1 else 0
        fiszero = let
                      in (-1)
        fnonzero = let
                a2 = an `div` ad
              in a2
      in if a1 == 1
        then fiszero
        else fnonzero
    in f
```

### Square

Two functions and it has a conversion operator.

```llvm
define i32 @square(i32 %x) {
  %1 = mul i32 %x, %x
  ret i32 %1
}

define i64 @no_overflow_square(i32 %x) {
  %1 = sext i32 %x to i64
  %2 = mul i64 %1, %1

  ret i64 %2
}
```

```haskell
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
```

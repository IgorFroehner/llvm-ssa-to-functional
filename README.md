# llvm-ir-to-functional

LLVM funds it's [IR](https://llvm.org/docs/LangRef.html) in [SSA (Single Static Assignment)](https://en.wikipedia.org/wiki/Static_single-assignment_form) form, SSA is proven to be equivalent of 
functional programming and [Administrative Normal Form](https://en.wikipedia.org/wiki/A-normal_form) (ANF) [(Kelsey 1998)](https://doi.org/10.1145/278283.278285) 
[(Chakravarty, Keller, Zadarnowski 2003)](https://doi.org/10.1016/S1571-0661(05)82596-4). This project
explores the translation of the LLVM-IR in SSA form to ANF in Haskell.

### Example

#### Source LLVM-IR

```llvm-ir
define i32 @factorial(i32 %0) {
entry:
  br label %2

2:
  %3 = phi i32 [ 1, %entry ], [ %8, %6 ]
  %4 = phi i32 [ %0, %entry ], [ %7, %6 ]
  %5 = icmp slt i32 %4, 2
  br i1 %5, label %9, label %6

6:
  %7 = add nsw i32 %4, -1
  %8 = mul nsw i32 %3, %4
  br label %2

9:
  %10 = mul nsw i32 %3, 1
  ret i32 %10
}

; int factorial(int n) {
;     if (n <= 1) return 1;
;     else return n * factorial(n - 1);
; }
```

#### Haskell Output

```haskell
factorial a0 =
  let
    aentry () =
      let
        a2 a3 a4 =
          let
            a5 = if a4 < 2 then 1 else 0
            a6 () =
              let
                a7 = a4 + (-1)
                a8 = a3 * a4
              in a2 a8 a7
            a9 () =
              let
                a10 = a3 * 1
              in a10 
          in if a5 == 1
            then a9 ()
            else a6 ()
      in a2 1 a0
  in aentry ()
```


### Build

```
stack build
```

### Usage

```
stack run -- [--dominance-viz | --graph-viz] <file.ll> [-o <output-file>]

    --graph-viz             outputs a graphviz with the control graph with the block labels
    --dominance-viz         outputs a graphviz with the dominator tree of the program's control graph

    -o <output-file>        if provided writes the output to the file given
```

#### Examples

##### Print the translation of the file_path to the terminal

```
stack run -- <file_path.ll> -o <output.hs>
```

##### Generate a DOT File `control-graph.dot` for the Control Graph

```
stack run -- --graph-viz <file_path.ll> -o control-graph.dot
```

##### Generate a DOT File `dominance.dot` with the Dominance Tree of the Control Graph

```
stack run -- --dominance-viz <file_path.ll> -o dominance.dot
```

#### Run Tests

```haskell
stack test
```

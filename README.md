# llvm-ir-to-functional

### Build

```
stack build
```

### Use the executable

#### Do the translation and print it

```
stack run -- <file_path.ll>
```

#### Generate a DOT File `control-graph.dot` for the Control Graph

```
stack run -- --graph-viz <file_path.ll>
```

#### Generate a DOT File `dominance.dot` with the Dominance Tree of the Control Graph

```
stack run -- --dominance-viz <file_path.ll>
```

#### Generate a graphviz file to generate a graphical version of the AST

```
stack run -- --ast <file_path.ll>
```

### Try in GHCi

```
stack ghci
```

#### Some tries on GHCi

```haskell
runAlex "%1 = call @asdf()" parseLLVMIR
```

```haskell
runAlex "call @asdf()" parseLLVMIR
```

#### Run Tests

```haskell
stack test
```

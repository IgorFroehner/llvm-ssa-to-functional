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

#### Beauty Print the Parsed AST

```
stack run -- --beauty <file_path.ll>
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

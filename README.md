# llvm-ir-to-functional

### Build

```
stack build
```

### Use the executable

#### Generate the AST and print it

```
stack run -- <file_path.ll>
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

# TODO:

- [ ] accept float constants as hex
- [ ] fix the strings constants

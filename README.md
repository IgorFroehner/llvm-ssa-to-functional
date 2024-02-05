# llvm-ir-to-functional

#### Build

```
stack build
```

#### Try in GHCi

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

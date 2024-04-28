# llvm-ir-to-functional

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
stack run -- <file_path.ll>
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


# Translation

## LLVM-IR Parsing

This project is parsing a subset of the LLVM-IR language like the following:

```
p := definition <t> @<x>(<xs>) {b} p

b := <x>: <phis> <ss> <flow>

phis := <x> = phi <t> <phi_values>
      | e

phi_values := [<v>, <x>], <phi_values>
            | [<v>, <x>]

ss := s ss | e

s := <x> = <o>
   | <l>

o := <bin_op> ....

l := call <t> @<x>(<vs>)

f := br <rs>
   | ret <t> <value>

rs := <t> <x>, <rs>
   | <t> <x>

xs := <x>, <xs> 
    | <x>
    | e

vs := <v>, <vs>
    | <v>
    | e

t := native llvm types
v := x | c
x := variable or label
c := constant
```

```
program := [<functions>]

function := definition <type> <name> [<argument_definitions>] [basic_blocks]
          | declare <type> <name> [<argument_definitions>]

argument_definition := <type>
                     | <type> <name>

basic_block := <name> [<phi_decs>] [<stmt>] <flow>
             | <name> [<phi_decs>] [<stmt>]

stmt := <name> = <operation>
      | <function_call>

operation := ....

function_call := <name> ( <call_args> )

call_args := <type> <value> , <call_args>
           | <type> <value>

phi_decs := <name> = phi <type> [(<value>,<name>)]

flow := br <type> <name>
      | br [<name>]

<value> := <name>
         | <int_const>
```

## Generated ANF:

```
program := program function
         | function

function := <name> <args> = <lets>

args := <name> <args>
      | <name>

lets := let <name> args in let <decls> <lets> in <flow>

<decls> := <decl> <decls>
         | <decl>

<decl> := name = <binop>

<flow> := in <call>
        | in if <cond> then <call> else <call>

<call> := <name> <values>

<values> := <value> <values>
          | <value>

<value> := <const> | <name>
```

Some details on the implementation

* implicit name of the first block
* when the call that makes the value selection is to the imediate dominator and not to the block itself

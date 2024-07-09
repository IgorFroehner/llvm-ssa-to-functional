
# Translation

Some details on the implementation

* implicit name of the first block
* when the call that makes the value selection is to the imediate dominator and not to the block itself

## LLVM-IR Grammar

Lembrar de arrumar a gramática da LLVM!
Conferir a do ANF também, vai que...

p ::= define t g ({y}) {b}
b ::= x: {phi} {s} f
phi ::= x = phi t {a}
s ::= x = o | x = q
q ::= call t g ({y})
f ::= br t x | br t v, t x, t x | ret t v
a ::= [v, x]
y ::= t x
o ::= beta t v1, v2 | icmp tau t v, v | select t v, t v, t v | mu t x to t

f ::= def x = \{x}.let l in x {v}
l ::= x = \{x}.let {d} {l} in j
d ::= x = e
e ::= v | x {v} | o {v}
j ::= v {v} | if v != 0 then v {v} else v {v}
v ::= x | c

## Translation

F(define t g ({y}) {b}) =
    def F_g(g) = \{F_y(y)}.let F_b(b) in F_s(b)

    onde b = nó inicial de {b}

F_b(x: {phi} {s} f) =
    x = \{F_a(phi)}.let {F_s(s)} {F_b(b')} in F_f(x, f)

    onde {b'} = nós filhos de x no grafo


F_s(x = beta t v1, v2) =                    (F_s recebe s, retorna d)
    ???
F_s(x = icmp tau t v, v) =
    ???
F_s(x = select t1 v1, t2 v2, t3 v3) =
    ???
F_s(x = mu t1 y to t2) =
    x = y

F_s(x = call t g ({y})) =
    ???

F_f(x, br t y) =
    y {F_p(x, phi)}

    onde y: {phi} {s} f = bloco com label y

F_f(x, br t1 v, t2 y2, t3 y3) =
    if v != 0 then y2 {F_p(x, phi2)} else y3 {F_p(x, phi3)}

    onde y2: {phi2} {s2} f2 = bloco com label y2
       e y3: {phi3} {s3} f3 = bloco com label y3

F_f(x, ret t v) =
    v

F_p(x, y = phi t {a}) =                     (recebe label e phi, retorna v)
    v

    onde [v, x] é um elemento de {a}

F_a(x = phi t {a}) = x
F_g(@x) = x
F_s(x: {phi} {s} f) = x

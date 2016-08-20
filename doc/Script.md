
# Datum Script

```
module   ::= top*

top      ::= var var* '=' exp ';'

exp      ::= exp_atom*

exp_atom ::= '(' exp ')'              (parenthesised expression)
          |  'λ' var '→' exp          (lambda abstraction)
          |  '[' exp,* ']'            (list literal)
          |  '/' name/*               (path expression)
          |  var                      (variable)
          |  operator                 (infix operator)
          |  symbol                   (symbol)
          |  literal                  (literal value)

literal  ::= int                      (literal integer)
          |  text                     (literal text)
```


Primitive Types
---------------
```
Tree     : Data
Forest   : Data

List     : Data → Data

Name     : Data

FilePath : Data

Unit     : Data
Bool     : Data
Int      : Data
Float    : Data
Nat      : Data
Decimal  : Data
Text     : Data
Time     : Data
```

Primitive Operators
-------------------

## Load and Store

```
load    : FilePath → S Tree
store   : FilePath → Tree → S Unit
```


## Sampling

```
initial : Nat → Tree → Tree
final   : Nat → Tree → Tree
sample  : Nat → Tree → Tree
```


## Grouping
```
group   : Name → Tree → Tree
```


## Gathering
```
gather  : TreePath → Tree → Tree
```

## Flattening
```
flatten : Tree → Tree
```

## Fields
```
rename-fields  : List Name → Tree → Tree
permute-fields : List Name → Tree → Tree
```

## Combinators
```
_ @ _          : List Name → (Tree   → Tree)   → Tree → Tree
on             : List Name → (Forest → Forest) → Tree → Tree
```


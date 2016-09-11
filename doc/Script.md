
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

## Mapping
```
 Γ ⊢ f                     :: tt_a → tt_b
 Γ ⊢ forest_a              :: Forest (name_r : tt_a # bts)
----------------------------------------------------------
 Γ ⊢ map-keys f forest_a   :: Forest (name_r : tt_b # bts)

```

## Filtering
```
 Γ ⊢ f                      :: tt_a → Bool
 Γ ⊢ forest_a               :: Forest (name_r : tt_a # bts)
----------------------------------------------------------
 Γ ⊢ filter-keys f forest_a :: Forest (name_r : tt_a # bts)
```


## Folding
```
 Γ ⊢ f    :: τ → tt_k → τ
 Γ ⊢ z    :: τ
 Γ ⊢ tree :: Forest (name_r : tt_r                # bts)
      bts = [ ..., name_d : tt_k # bts', ...]
 ---------------------------------------------------------
 Γ ⊢ fold-as-field name_f name_d f z tree
          :: Forest (name_r : {tt_r | name_f : τ} # bts)
```

```
fold-as-field 
   :: (name_f   : Name)
   -> (name_d   : Name)
   -> (tt_k     : Record)
   -> (bts bts' : Array BranchType)
   -> (_        : Member name_d (tt_k, bts') bts)
   -> (τ -> tt_k -> τ)
   -> (Forest (name_r : tt_r # bts))
   ->  Forest (name_r : {tt_r | name_f : τ} # bts)
```


## Grouping
```
 Γ ⊢  tree_a
        :: Forest (name_r : tt # bts)  
 tt = { ..., name_g : τ, ... }
 --------------------------------------------------------------------------
 Γ ⊢  group name_g tree_a
        :: Forest (name_g : { name_g : τ } # [ name_r : tt - name_g # bts ])
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


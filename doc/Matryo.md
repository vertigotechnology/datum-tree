

# Matryoshka Trees

Matryosha is an external representation for datum trees, named after the Russian nesting dolls.

## Grammar:


A complete tree contains a branch type followed by a branch value. 

```
TreeRoot       ::= BranchTypeRoot Branch
Tree           ::= BranchType     Branch
```

## Types
A branch type consists of a dimension name, a tuple types, and some sub-branch types. For the top-level root dimension we allow the dimension name to be elided. If the branch has no sub-branches then only the tuple type needs to be specified.

A tuple type consists of a sequence of named field types. Each field is associated with an atom type.

```
BranchTypeRoot  ::= (Name ':')? '{' TupleType BranchTypes '}'
                 |  (Name ':')?     TupleType

BranchType      ::=  Name ':'   '{' TupleType BranchTypes '}'
                 |   Name ':'       TupleType

BranchTypes     ::= '[' BranchType,* ']'

TupleType       ::= '(' FieldType,* ')'

FieldType       ::= Name \':\' AtomType

AtomType        ::= ...
```


## Values
A branch consists of a tuple followed by some groups of sub branches. A group consists of a dimension name, and optional tuple type, and a list of sub branches.

A tuple consists of a sequence of atoms.

```
Branch          ::= '{' Tuple Group,* '}'
                 |  Tuple

Group           ::= Name ':' TupleType? '[' Branch,* ']'

Tuple           ::= '(' Atom,* ')'

Name            ::= ...

Atom            ::= ...
```

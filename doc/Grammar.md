# Flix Grammar #

An informal, readable version of the Flix grammar. 

NB: Not every program admitted by this grammar is a valid Flix program.

```no-lang
Program         A  =  D ...                         Declaration
            
Declaration     D  =  namespace Name { D ... }      Namespace declaration.
                   |  Enum                          Enum declaration.
                   |  Function                      Function declaration.
                   |  Relation                      Relation declaration.
                   |  Lattice                       Lattice declaration.
                   |  Index                         Index declaration.
                   |  Constraint                    Constraint declaration.


Enum               =  enum { EnumCase, ... }        Enum declaration.
EnumCase           =  case Name(T, ...)             Enum case.


Function           =  fn Name Args? : T = e         Function declaration.
Args               =  ((x: T), ...)                 Formal arguments.


Relation           =  rel Name(x: T, ...)           Relation declaration.


Lattice            =  lat Name(x: T(<>)?, ...)      Lattice declaration.


Index              =  index Name(IndexKey, ...)     Index declaration.
IndexKey           =  { x, ... }                    Index Keys


Value           v  =  ()                            Unit value.
                   |  [Bool]                        Boolean value.
                   |  [Int]                         Int value.
                   |  [Str]                         Str value.
                   |  Name . Tag v                  Tagged value.
                   |  (v, ...)                      Tuple value.


Expr            e  =  v                             Value expression.
                   |  x                             Variable expression.
                   |  e (e, ...)                    Call expression.
                   |  e `e` e                       Infix call expression.
                   |  op e                          Unary expression.
                   |  e op e                        Binary expression.
                   |  let x = e in e                Let expression.
                   |  if (e) then e else e          If-then-else expression.
                   |  switch { SCase ... }          Switch expression.
                   |  match e with { MCase ... }    Match expression.
                   |  Name . Tag e                  Tag expression.
                   |  (e, ...)                      Tuple expression.
                   |  e : T                         Ascribe expression.
                   |  ??? : T                       Error expression.


SCase              =  case e => e                   Switch case.
MCase              =  case p => e                   Match case.


Pattern         p  =  _                             Wildcard pattern.
                   |  x                             Variable pattern.
                   |  ()                            Unit pattern. 
                   |  [Bool]                        Boolean pattern.
                   |  [Int]                         Integer pattern.
                   |  [Str]                         String pattern.
                   |  Name . Tag p                  Tag pattern.
                   |  (p, ...)                      Tuple pattern.


Constraint      C  =  P.                            Fact.
                   |  P :-  P, ... .                Rule.


Predicate       P  =  Name(t, ...)                  Table predicate.
                   |  x != y                        Not Equal predicate. [1]
                   |  x <- t                        Loop predicate. [2]
                   |  x := t                        Alias predicate. [3]


Term            t  =  _                             Wildcard term. [4]
                   |  x                             Variable term.
                   |  v                             Value term.
                   |  Name(t, ...)                  Apply term. [5]


Type            T  =  Name                          Named type.
                   |  Unit                          Unit type.
                   |  Bool                          Bool type.
                   |  Char                          Char type.
                   |  Int8                          Unsigned  8 bit int type.
                   |  Int16                         Unsigned 16 bit int type.
                   |  Int32                         Unsigned 32 bit int type. [6]
                   |  Int64                         Unsigned 64 bit int type.
                   |  Str                           String type.
                   |  Native                        Native type.
                   |  (T, ...)                      Tuple type.
                   |  (T, ...) -> T                 Lambda type.
                   |  Opt[T]                        Option type.
                   |  Lst[T]                        List type.
                   |  Set[T]                        Set type.
                   |  Map[T, T]                     Map type.


Var       x, y, z  =  An infinite set of variables.
Var       op       =  The set of unary and binary operators: +, -, *, /, etc.


[1]: A not equal predicate must appear in the body of a constraint.
[2]: A loop predicate must appear in the body of a constraint.
[3]: An alias predicate must appear in the body of a constraint.
[4]: A wildcard term may only appear in the body of a constraint.
[5]: An apply term may only appear in the head of a constraint.
[6]: The alias `Int` may be used.
```

Here 
`[Bool]` is `true`, `false`,
`[Int]` is `1`, `2`, `3`, ..., `-42`, etc,
`[Str]` is `"f"`, `"foo"`, etc.

# Flix Grammar #

An informal, readable version of the Flix grammar. 

NB: Not every program admitted by this grammar is a valid Flix program.

```no-lang
Program         A  =  D ...                         Declaration
            
Declaration     D  =  namespace Name                Namespace declaration.
                   |                                Function declaration.
                   |                                Law declaration.
                   |                                Signature declaration.
                   |  enum { EnumCase ... }         Enum declaration.
                   |  rel Name()                    Relation declaration.
                   |  lat ()                        Lattice declaration.
                   |  index Name()                  Index declaration.
                   |  c                             Constraint declaration.

EnumCase           = case Name(T, ..., T)           Enum case.

Value           v  =  ()                            Unit
                   |  true | false                  Boolean
                   |  [Int]                         Int
                   |  [Str]                         Str
                   |  Name.v                        Tagged
                   |  (v_1, ..., v_n)               Tuple
               

Expr            e  =  v                             Value
                   |  x                             Var
                   |  e(e_1, ..., e_n)              App
                   |  e_1 `e` e_2                   Infix
                   |  op e                          Unary
                   |  e_1 op e_2                    Binary
                   |  let x = e in e                Let-binding
                   |  if (e) then e else e          If-then-else
                   |  switch { SCase ... }          Switch
                   |  match e with { MCase... }     Match
                   |  Name.e                        Tag expression
                   |  (e, ..., e)                   Tuple expression
                   |  e : T                         Ascribe expression
                   |  ??? : T                       Error expression
                   | bot | top

               
SCase              =  case e => e                   Switch case.
MCase              =  case p => e                   Match case.


Pattern         p  =  _                             Wildcard pattern.
                   |  x                             Variable pattern.
                   |  ()                            Unit pattern. 
                   |  true | false                  Boolean pattern.
                   |  [Int]                         Integer pattern.
                   |  [Str]                         String pattern.
                   |  Name.p                        Tag pattern.
                   |  (p, ..., p)                   Tuple pattern.


Constraint      C  =  P.                            Fact.
                   |  P :-  P, ..., P.              Rule.


Predicate       P  =  Name(t, ..., t)               Table predicate.
                   |  x != y                        Not Equal predicate. [1]
                   |  x <- t                        Loop predicate. [2]
                   |  x := t                        Alias predicate. [3]


Term            t  =  _                             Wildcard term. [4]
                   |  x                             Variable term.
                   |  v                             Value term.
                   |  Name(t, ..., t)               Apply term. [5]


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
                   |  (T, ..., T)                   Tuple type.
                   |  (T, ..., T) -> T              Lambda type.
                   |  Opt[T]                        Option type.
                   |  Lst[T]                        List type.
                   |  Set[T]                        Set type.
                   |  Map[T, T]                     Map type.

[1]: A not equal predicate must appear in the body of a constraint.
[2]: A loop predicate must appear in the body of a constraint.
[3]: An alias predicate must appear in the body of a constraint.
[4]: A wildcard term may only appear in the body of a constraint.
[5]: An apply term may only appear in the head of a constraint.
[6]: The alias `Int` may be used.


```

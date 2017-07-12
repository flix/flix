// Basic :- shouldn't cause an error
rel Foo(c: Int)
rel Bar(c: Int)
Foo(c) :- Foo(c).
Bar(c) :- Bar(c).
Foo(c) :- Bar(c).

// Long chains shouldn't error
rel Foo1(c: Int)
rel Foo2(c: Int)
rel Foo3(c: Int)
rel Foo4(c: Int)
rel Foo5(c: Int)
rel Foo6(c: Int)
rel Foo7(c: Int)
rel Foo8(c: Int)
rel Foo9(c: Int)
rel Foo10(c: Int)
Foo(c) :- Foo(c).
Foo(c) :- Foo1(c).
Foo1(c) :- Foo2(c).
Foo2(c) :- Foo3(c).
Foo3(c) :- Foo4(c).
Foo4(c) :- Foo5(c).
Foo5(c) :- Foo6(c).
Foo6(c) :- Foo7(c).
Foo7(c) :- Foo8(c).
Foo8(c) :- Foo9(c).
Foo9(c) :- Foo10(c).
Foo10(c) :- Bar(c).

// Both A and !A can imply B
rel Baz(c:Int)
Foo(c) :- Bar(c).
Foo(c) :- !Baz(c).
Bar(c) :- Baz(c).
Bar(c) :- !Baz(c).

// Should be stratified since Q depends on P, but but P does not depend on Q
rel P(x: Int)
rel Q(x: Int)
rel R(x: Int)
rel S(x: Int)
P(x) :- R(x).
P(x) :- P(x).
Q(x) :- S(x), !P(x).

// We should be able to handle negation in any order
rel T(x: Int)
rel U(x: Int)
rel V(x: Int)
T(x) :- U(x), V(x).
T(x) :- !U(x), V(x).
T(x) :- U(x), !V(x).
T(x) :- !U(x), !V(x).

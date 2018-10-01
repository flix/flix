rel A[a](x: Str, y: a, z: a)
rel B[a, b](x: Str, y: a, z: b)

@test
def testPolymorphicConstraint01(): Schema { A(Str, Int, Int), B(Str, Void, Void) } =
    A("hello", 123, 456).

@test
def testPolymorphicConstraint02(): Schema { A(Str, Int, Int), B(Str, Void, Void) } =
    A("hello", 123, 456).
    A("world", 321, 654).

@test
def testPolymorphicConstraint03(): Schema { A(Str, Int, Int), B(Str, Void, Void) } =
    let p1 = A("hello", 1234, 56789).;
    let p2 = A("world", true, false).;
    p1

@test
def testPolymorphicConstraint04(): Schema { A(Str, Bool, Bool), B(Str, Void, Void) } =
    let p1 = A("hello", 1234, 56789).;
    let p2 = A("world", true, false).;
    p2

@test
def testPolymorphicConstraint05(): Schema { A(Str, Void, Void), B(Str, Int, Bool) } =
    B("hello", 123, true).

@test
def testPolymorphicConstraint06(): Schema { A(Str, Void, Void), B(Str, Int, Bool) } =
    B("hello", 123, true).
    B("world", 321, false).

@test
def testPolymorphicConstraint07(): Schema { A(Str, Int, Int), B(Str, Bool, Char) } =
    A("hello", 1234, 567).
    B("world", true, 'a').

@test
def testPolymorphicConstraint08(): Schema { A(Str, Int, Int), B(Str, Bool, Int) } =
    A("hello", 1234, 5678).
    B("world", true, 1234).
    B("world", true, 5678).
    A(s, i, i) :- B(s, true, i).

@test
def testPolymorphicConstraint09(): Schema { A(Str, Int, Int), B(Str, Bool, Int) } =
    A("hello", 1234, 5678).
    A(s, i, i) :- B(s, true, i).

@test
def testPolymorphicConstraint10(): Schema { A(Str, Int, Int), B(Str, Bool, Int) } =
    B("world", true, 1234).
    B("world", true, 5678).
    A(s, i, i) :- B(s, true, i).

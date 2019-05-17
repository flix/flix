rel A(x: Int)
rel B(x: Int, y: Str)
rel C(x: Int, y: Str, z: Bool)

@test
def testQuantVar01(): Schema { A } =
    let c = A(x) :- A(x).;
    solve c

@test
def testQuantVar02(): Schema { B } =
    let c = B(x, y) :- B(x, y).;
    solve c

@test
def testQuantVar03(): Schema { C } =
    let c = C(x, y, z) :- C(x, y, z).;
    solve c

@test
def testQuantVar04(): Schema { A, B, C } =
    let c1 = A(x) :- A(x).;
    let c2 = B(x, y) :- B(x, y).;
    let c3 = C(x, y, z) :- C(x, y, z).;
    solve (c1 <+> c2 <+> c3)

@test
def testQuantVar05(): Schema { A, B } =
    let c = A(x) :- B(x, y).;
    solve c

@test
def testQuantVar06(): Schema { A, C } =
    let c = A(x) :- C(x, y, z).;
    solve c
@test
def testQuantVar07(): Schema { B, C } =
    let c = B(x, y) :- C(x, y, z).;
    solve c

@test
def testQuantVar08(): Schema { A, B, C } =
    let c = A(x) :- A(x), B(x, y), C(x, y, z).;
    solve c

@test
def testQuantVar09(): Schema { A, B, C } =
    let c = C(x, y, z) :- A(x), B(x, y), C(x, y, z).;
    solve c

rel A(x: Int)
rel B(x: Int, y: Str)
rel C(x: Int, y: Str, z: Bool)

@test
def testQuantVar01(): #{ A } =
    let c = A(x) :- A(x).;
    solve c

@test
def testQuantVar02(): #{ B } =
    let c = B(x, y) :- B(x, y).;
    solve c

@test
def testQuantVar03(): #{ C } =
    let c = C(x, y, z) :- C(x, y, z).;
    solve c

@test
def testQuantVar04(): #{ A, B, C } =
    let c1 = A(x) :- A(x).;
    let c2 = B(x, y) :- B(x, y).;
    let c3 = C(x, y, z) :- C(x, y, z).;
    solve (c1 <+> c2 <+> c3)

@test
def testQuantVar05(): #{ A, B } =
    let c = A(x) :- B(x, y).;
    solve c

@test
def testQuantVar06(): #{ A, C } =
    let c = A(x) :- C(x, y, z).;
    solve c
@test
def testQuantVar07(): #{ B, C } =
    let c = B(x, y) :- C(x, y, z).;
    solve c

@test
def testQuantVar08(): #{ A, B, C } =
    let c = A(x) :- A(x), B(x, y), C(x, y, z).;
    solve c

@test
def testQuantVar09(): #{ A, B, C } =
    let c = C(x, y, z) :- A(x), B(x, y), C(x, y, z).;
    solve c

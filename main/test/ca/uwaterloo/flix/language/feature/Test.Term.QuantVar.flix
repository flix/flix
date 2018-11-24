rel A(x: Int)
rel B(x: Int, y: Str)
rel C(x: Int, y: Str, z: Bool)

@test
def testQuantVar01(): Unit =
    let c = A(x) :- A(x).;
    let r = solve c;
    ()

@test
def testQuantVar02(): Unit =
    let c = B(x, y) :- B(x, y).;
    let r = solve c;
    ()

@test
def testQuantVar03(): Unit =
    let c = C(x, y, z) :- C(x, y, z).;
    let r = solve c;
    ()

@test
def testQuantVar04(): Unit =
    let c1 = A(x) :- A(x).;
    let c2 = B(x, y) :- B(x, y).;
    let c3 = C(x, y, z) :- C(x, y, z).;
    let rr = solve (c1 <+> c2 <+> c3);
    ()

@test
def testQuantVar05(): Unit =
    let c = A(x) :- B(x, y).;
    let r = solve c;
    ()

@test
def testQuantVar06(): Unit =
    let c = A(x) :- C(x, y, z).;
    let r = solve c;
    ()

@test
def testQuantVar07(): Unit =
    let c = B(x, y) :- C(x, y, z).;
    let r = solve c;
    ()

@test
def testQuantVar08(): Unit =
    let c = A(x) :- A(x), B(x, y), C(x, y, z).;
    let r = solve c;
    ()

@test
def testQuantVar09(): Unit =
    let c = C(x, y, z) :- A(x), B(x, y), C(x, y, z).;
    let r = solve c;
    ()

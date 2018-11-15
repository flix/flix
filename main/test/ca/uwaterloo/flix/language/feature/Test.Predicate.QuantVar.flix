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


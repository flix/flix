rel A(x: Int, y: Str, z: Bool)
rel B(x: Int, y: Str, z: Bool)
rel C(x: Int, y: Str, z: Bool)

@test
def testWildVar01(): Unit =
    let c = A(x, y, z) :- A(x, y, z), B(x, y, z), C(x, y, z).;
    let r = solve c;
    ()

@test
def testWildVar02(): Unit =
    let c = A(x, y, z) :- A(_, y, z), B(x, y, z), C(x, y, z).;
    let r = solve c;
    ()

@test
def testWildVar03(): Unit =
    let c = A(x, y, z) :- A(x, _, z), B(x, y, z), C(x, y, z).;
    let r = solve c;
    ()

@test
def testWildVar04(): Unit =
    let c = A(x, y, z) :- A(x, y, _), B(x, y, z), C(x, y, z).;
    let r = solve c;
    ()

@test
def testWildVar05(): Unit =
    let c = A(x, y, z) :- A(x, y, z), B(_, y, z), C(x, y, z).;
    let r = solve c;
    ()

@test
def testWildVar06(): Unit =
    let c = A(x, y, z) :- A(x, y, z), B(x, _, z), C(x, y, z).;
    let r = solve c;
    ()

@test
def testWildVar07(): Unit =
    let c = A(x, y, z) :- A(x, y, z), B(x, y, _), C(x, y, z).;
    let r = solve c;
    ()

@test
def testWildVar08(): Unit =
    let c = A(x, y, z) :- A(_, _, _), B(x, y, z), C(x, y, z).;
    let r = solve c;
    ()

@test
def testWildVar09(): Unit =
    let c = A(x, y, z) :- A(_, _, _), B(_, _, _), C(x, y, z).;
    let r = solve c;
    ()

@test
def testWildVar10(): Unit =
    let c = A(x, y, z) :- A(_, _, _), B(x, y, z), C(_, _, _).;
    let r = solve c;
    ()

@test
def testWildVar11(): Unit =
    let c = A(x, y, z) :- A(x, _, _), B(_, y, _), C(_, _, z).;
    let r = solve c;
    ()

@test
def testWildVar12(): Unit =
    let c = A(x, y, z) :- A(_, _, z), B(_, y, _), C(x, _, _).;
    let r = solve c;
    ()

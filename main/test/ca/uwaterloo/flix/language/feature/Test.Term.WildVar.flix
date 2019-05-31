rel A(x: Int, y: Str, z: Bool)
rel B(x: Int, y: Str, z: Bool)
rel C(x: Int, y: Str, z: Bool)

@test
def testWildVar01(): Schema { A, B, C } =
    let c = A(x, y, z) :- A(x, y, z), B(x, y, z), C(x, y, z).;
    solve c

@test
def testWildVar02(): Schema { A, B, C } =
    let c = A(x, y, z) :- A(_, y, z), B(x, y, z), C(x, y, z).;
    solve c

@test
def testWildVar03(): Schema { A, B, C } =
    let c = A(x, y, z) :- A(x, _, z), B(x, y, z), C(x, y, z).;
    solve c

@test
def testWildVar04(): Schema { A, B, C } =
    let c = A(x, y, z) :- A(x, y, _), B(x, y, z), C(x, y, z).;
    solve c

@test
def testWildVar05(): Schema { A, B, C } =
    let c = A(x, y, z) :- A(x, y, z), B(_, y, z), C(x, y, z).;
    solve c

@test
def testWildVar06(): Schema { A, B, C } =
    let c = A(x, y, z) :- A(x, y, z), B(x, _, z), C(x, y, z).;
    solve c

@test
def testWildVar07(): Schema { A, B, C } =
    let c = A(x, y, z) :- A(x, y, z), B(x, y, _), C(x, y, z).;
    solve c

@test
def testWildVar08(): Schema { A, B, C } =
    let c = A(x, y, z) :- A(_, _, _), B(x, y, z), C(x, y, z).;
    solve c

@test
def testWildVar09(): Schema { A, B, C } =
    let c = A(x, y, z) :- A(_, _, _), B(_, _, _), C(x, y, z).;
    solve c

@test
def testWildVar10(): Schema { A, B, C } =
    let c = A(x, y, z) :- A(_, _, _), B(x, y, z), C(_, _, _).;
    solve c

@test
def testWildVar11(): Schema { A, B, C } =
    let c = A(x, y, z) :- A(x, _, _), B(_, y, _), C(_, _, z).;
    solve c

@test
def testWildVar12(): Schema { A, B, C } =
    let c = A(x, y, z) :- A(_, _, z), B(_, y, _), C(x, _, _).;
    solve c

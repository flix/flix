rel A(x: Int, y: Int, z: Int)
rel P[a](x: Int, y: a, z: Int)

@test
def testFixpointCompose01(): Schema {  } =
    let c1 = True().;
    solve c1 <+> c1

@test
def testFixpointCompose02(): Schema { } =
    let c1 = True().;
    solve c1 <+> c1 <+> c1

@test
def testFixpointCompose03(): Schema { } =
    let c1 = True().;
    solve (solve c1 <+> c1) <+> (solve c1 <+> c1)

@test
def testFixpointCompose04(): Schema { A } =
    let c1 = A(1, 1, 1).;
    let c2 = A(2, 2, 2).;
    let c3 = A(x, y, z) :- A(x, y, z).;
    solve c1 <+> c2 <+> c3

@test
def testFixpointCompose05(): Schema { A } =
    let c1 = A(1, 1, 1).;
    let c2 = A(2, 2, 2).;
    let c3 = A(x, y, z) :- A(x, y, z).;
    solve (solve (c1 <+> c2) <+> c3)

@test
def testFixpointCompose06(): Schema { A } =
    let c1 = A(1, 1, 1).;
    let c2 = A(2, 2, 2).;
    let c3 = A(x, y, z) :- A(x, y, z).;
    solve (c1 <+> solve (c2 <+> c3))

@test
def testFixpointCompose07(): Schema { A } =
    let c1 = A(1, 1, 1).;
    let c2 = A(2, 2, 2).;
    let c3 = A(x, y, z) :- A(x, y, z).;
    let c4 = A(z, y, x) :- A(x, y, z).;
    solve c1 <+> c2 <+> c3 <+> c4

@test
def testFixpointCompose08(): Schema { A } =
    let c1 = A(1, 1, 1).;
    let c2 = A(2, 2, 2).;
    let c3 = A(x, y, z) :- A(x, y, z).;
    let c4 = A(z, y, x) :- A(x, y, z).;
    solve (solve (c1 <+> c2)) <+> (c3 <+> c4)

@test
def testFixpointCompose09(): Schema { A } =
    let c1 = A(1, 1, 1).;
    let c2 = A(2, 2, 2).;
    let c3 = A(x, y, z) :- A(x, y, z).;
    let c4 = A(z, y, x) :- A(x, y, z).;
    solve (c1 <+> c2) <+> (c2 <+> c1) <+> (c3 <+> c4) <+> (c4 <+> c3)

@test
def testFixpointCompose10(): Schema { P[Int] } =
    let c1 = P(1, 123, 1).;
    let c2 = P(2, 123, 2).;
    solve c1 <+> c2

@test
def testFixpointCompose11(): (Schema { P[Int] }, Schema { P[Str] }) =
    let c1 = P(1, 123, 1).;
    let c2 = P(2, 123, 2).;
    let c3 = P(1, "a", 1).;
    let c4 = P(2, "a", 2).;
    let ra = solve c1 <+> c2;
    let rb = solve c3 <+> c4;
    (ra, rb)

@test
def testFixpointCompose12(): (Schema { P[Int] }, Schema { P[Str] }) =
    let c1 = P(1, 123, 1).;
    let c2 = P(x, 555, z) :- P(x, 555, z).;
    let c3 = P(1, "a", 1).;
    let c4 = P(x, "a", z) :- P(x, "a", z).;
    let ra = solve c1 <+> c2;
    let rb = solve c3 <+> c4;
    (ra, rb)

rel A(x: Int, y: Int, z: Int)

@test
def testFixpointCompose01(): Unit =
    let c1 = true.;
    let rr = solve c1 <+> c1;
    ()

@test
def testFixpointCompose02(): Unit =
    let c1 = true.;
    let rr = solve c1 <+> c1 <+> c1;
    ()

@test
def testFixpointCompose03(): Unit =
    let c1 = true.;
    let rr = solve (solve c1 <+> c1) <+> (solve c1 <+> c1);
    ()

@test
def testFixpointCompose04(): Unit =
    let c1 = A(1, 1, 1).;
    let c2 = A(2, 2, 2).;
    let c3 = A(x, y, z) :- A(x, y, z).;
    let rr = solve c1 <+> c2 <+> c3;
    ()

@test
def testFixpointCompose05(): Unit =
    let c1 = A(1, 1, 1).;
    let c2 = A(2, 2, 2).;
    let c3 = A(x, y, z) :- A(x, y, z).;
    let rr = solve (solve (c1 <+> c2) <+> c3);
    ()

@test
def testFixpointCompose06(): Unit =
    let c1 = A(1, 1, 1).;
    let c2 = A(2, 2, 2).;
    let c3 = A(x, y, z) :- A(x, y, z).;
    let rr = solve (c1 <+> solve (c2 <+> c3));
    ()

@test
def testFixpointCompose07(): Unit =
    let c1 = A(1, 1, 1).;
    let c2 = A(2, 2, 2).;
    let c3 = A(x, y, z) :- A(x, y, z).;
    let c4 = A(z, y, x) :- A(x, y, z).;
    let rr = solve c1 <+> c2 <+> c3 <+> c4;
    ()

@test
def testFixpointCompose08(): Unit =
    let c1 = A(1, 1, 1).;
    let c2 = A(2, 2, 2).;
    let c3 = A(x, y, z) :- A(x, y, z).;
    let c4 = A(z, y, x) :- A(x, y, z).;
    let rr = solve (solve (c1 <+> c2)) <+> (c3 <+> c4);
    ()

@test
def testFixpointCompose09(): Unit =
    let c1 = A(1, 1, 1).;
    let c2 = A(2, 2, 2).;
    let c3 = A(x, y, z) :- A(x, y, z).;
    let c4 = A(z, y, x) :- A(x, y, z).;
    let rr = solve (c1 <+> c2) <+> (c2 <+> c1) <+> (c3 <+> c4) <+> (c4 <+> c3);
    ()

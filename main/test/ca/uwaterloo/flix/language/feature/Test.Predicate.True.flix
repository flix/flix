@test
def testTruePredicate01(): Unit =
    let x = true.;
    ()

@test
def testTruePredicate02(): Unit =
    let x = true.;
    let y = true.;
    ()

@test
def testTruePredicate03(): Unit =
    let x = true.;
    let y = true.;
    let r = x <+> y;
    ()

@test
def testTruePredicate04(): Unit =
    let x = true.;
    let y = true.;
    let u = x <+> y;
    let v = y <+> x;
    ()

@test
def testTruePredicate05(): Unit =
    let x = true.;
    let y = true.;
    let u = x <+> y;
    let v = y <+> x;
    let w = y <+> v;
    ()

@test
def testTruePredicate06(): Unit =
    let x = true.;
    let y = true.;
    let u = solve x;
    let v = solve y;
    ()

@test
def testTruePredicate07(): Unit =
    let x = true.;
    let y = true.;
    let u = check x;
    let v = check y;
    ()

@test
def testTruePredicate08(): Unit =
    let x = true.;
    let y = true.;
    let u = solve (x <+> y);
    let v = solve (y <+> x);
    ()

@test
def testTruePredicate09(): Unit =
    let x = true.;
    let y = true.;
    let u = solve (solve x);
    let w = solve (solve y);
    ()

@test
def testTruePredicate10(): Unit =
    let x = true.;
    let y = true.;
    let u = check (solve x);
    let w = check (solve y);
    ()

@test
def testTruePredicate11(): Unit =
    let x = true.;
    let y = true.;
    let u = check ((solve x) <+> (solve y));
    ()

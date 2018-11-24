@test
def testFalsePredicate01(): Unit =
    let x = false.;
    ()

@test
def testFalsePredicate02(): Unit =
    let x = false.;
    let y = false.;
    ()

@test
def testFalsePredicate03(): Unit =
    let x = false.;
    let y = false.;
    let r = x <+> y;
    ()

@test
def testFalsePredicate04(): Unit =
    let x = false.;
    let y = false.;
    let u = x <+> y;
    let v = y <+> x;
    ()

@test
def testFalsePredicate05(): Unit =
    let x = false.;
    let y = false.;
    let u = x <+> y;
    let v = y <+> x;
    let w = y <+> v;
    ()

@test
def testFalsePredicate06(): Unit =
    let x = false.;
    let y = false.;
    let u = check x;
    let v = check y;
    ()

@test
def testFalsePredicate07(): Unit =
    let x = false.;
    let y = false.;
    let u = check (x <+> y);
    let v = check (y <+> x);
    ()

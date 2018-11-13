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
    let r = union x y;
    ()

@test
def testFalsePredicate04(): Unit =
    let x = false.;
    let y = false.;
    let u = union x y;
    let v = union y x;
    ()

@test
def testFalsePredicate05(): Unit =
    let x = false.;
    let y = false.;
    let u = union x y;
    let v = union y x;
    let w = union y v;
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
    let u = check (union x y);
    let v = check (union y x);
    ()

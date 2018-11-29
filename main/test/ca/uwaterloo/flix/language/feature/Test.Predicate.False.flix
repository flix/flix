@test
def testFalsePredicate01(): Bool =
    let x = false.;
    !(check x)

@test
def testFalsePredicate02(): Bool =
    let x = false.;
    let u = x <+> x;
    !(check u)

@test
def testFalsePredicate03(): Bool =
    let x = false.;
    let y = false.;
    let u = x <+> y;
    !(check u)

@test
def testFalsePredicate04(): Bool =
    let x = false.;
    let y = false.;
    let u = x <+> y;
    let v = y <+> x;
    !(check u <+> v)

@test
def testFalsePredicate05(): Bool =
    let x = false.;
    let y = false.;
    let u = x <+> y;
    let v = y <+> x;
    let w = y <+> v;
    !(check w)

@test
def testFalsePredicate06(): Bool =
    let x = false.;
    let y = false.;
    let u = check x;
    let v = check y;
    !u && !v

@test
def testFalsePredicate07(): Bool =
    let x = false.;
    let y = false.;
    let u = check (x <+> y);
    let v = check (y <+> x);
    !u && !v

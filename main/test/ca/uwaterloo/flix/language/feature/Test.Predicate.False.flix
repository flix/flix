@test
def testFalsePredicate01(): Schema { False } =
    let x = False().;
    solve x

@test
def testFalsePredicate02(): Schema { False } =
    let x = False().;
    let u = x <+> x;
    solve u

@test
def testFalsePredicate03(): Schema { False } =
    let x = False().;
    let y = False().;
    let u = x <+> y;
    solve u

@test
def testFalsePredicate04(): Schema { False } =
    let x = False().;
    let y = False().;
    let u = x <+> y;
    let v = y <+> x;
    solve u <+> v

@test
def testFalsePredicate05(): Schema { False } =
    let x = False().;
    let y = False().;
    let _ = x <+> y;
    let v = y <+> x;
    let w = y <+> v;
    solve w

@test
def testFalsePredicate06(): Schema { False } =
    let x = False().;
    let y = False().;
    let u = solve x;
    let v = solve y;
    u <+> v

@test
def testFalsePredicate07(): Schema { False } =
    let x = False().;
    let y = False().;
    let u = solve (x <+> y);
    let v = solve (y <+> x);
    u <+> v

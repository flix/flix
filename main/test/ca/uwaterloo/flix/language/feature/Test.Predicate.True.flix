@test
def testTruePredicate01(): Bool =
    check true.

@test
def testTruePredicate02(): Bool =
    let x = true.;
    let u = x <+> x;
    check u

@test
def testTruePredicate03(): Bool =
    let x = true.;
    let u = x <+> x <+> x;
    check u

@test
def testTruePredicate04(): Bool =
    let x = true.;
    let y = true.;
    let u = x <+> y;
    check u

@test
def testTruePredicate05(): Bool =
    let x = true.;
    let y = true.;
    let u = (x <+> y) <+> (y <+> x);
    check u

@test
def testTruePredicate06(): Bool =
    let x = true.;
    let y = true.;
    let u = solve x;
    let v = solve y;
    (check u) && (check v)

@test
def testTruePredicate07(): Bool =
    let x = true.;
    let y = true.;
    let u = solve x <+> y;
    let v = solve y <+> x;
    (check u) && (check v)

@test
def testTruePredicate08(): Bool =
    let x = true.;
    let y = true.;
    let u = solve (solve x <+> y);
    let v = solve (solve y <+> x);
    (check u) && (check v)

@test
def testTruePredicate09(): Bool =
    let x = true.;
    let y = true.;
    let u = solve (solve x <+> y);
    let v = solve (solve y <+> x);
    check (solve u <+> v)

@test
def testTruePredicate10(): Bool =
    (true.) |= (true.)

@test
def testTruePredicate11(): Bool =
    (true.) |= (true. true.)

@test
def testTruePredicate12(): Bool =
    (true.) |= (true. true. true.)

@test
def testTruePredicate13(): Bool =
    (true. true. ) |= (true.)

@test
def testTruePredicate14(): Bool =
    (true. true. true.) |= (true.)

@test
def testTruePredicate15(): Bool =
    (solve true.) |= (true.)

@test
def testTruePredicate16(): Bool =
    (solve (solve true.)) |= (true.)

@test
def testTruePredicate17(): Bool =
    (solve (solve (solve true.))) |= (true.)


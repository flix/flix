rel True()

@test
def testTruePredicate01(): #{ True } =
    solve True().

@test
def testTruePredicate02(): #{ True } =
    let x = True().;
    let u = x <+> x;
    solve u

@test
def testTruePredicate03(): #{ True } =
    let x = True().;
    let u = x <+> x <+> x;
    solve u

@test
def testTruePredicate04(): #{ True } =
    let x = True().;
    let y = True().;
    let u = x <+> y;
    solve u

@test
def testTruePredicate05(): #{ True } =
    let x = True().;
    let y = True().;
    let u = (x <+> y) <+> (y <+> x);
    solve u

@test
def testTruePredicate06(): #{ True } =
    let x = True().;
    let y = True().;
    let u = solve x;
    let v = solve y;
    (solve u) <+> (solve v)

@test
def testTruePredicate07(): #{ True } =
    let x = True().;
    let y = True().;
    let u = solve x <+> y;
    let v = solve y <+> x;
    (solve u) <+> (solve v)

@test
def testTruePredicate08(): #{ True } =
    let x = True().;
    let y = True().;
    let u = solve (solve x <+> y);
    let v = solve (solve y <+> x);
    (solve u) <+> (solve v)

@test
def testTruePredicate09(): #{ True } =
    let x = True().;
    let y = True().;
    let u = solve (solve x <+> y);
    let v = solve (solve y <+> x);
    solve (solve u <+> v)

@test
def testTruePredicate10(): Bool =
    (True().) |= (True().)

@test
def testTruePredicate11(): Bool =
    (True().) |= #{ True(). True(). }

@test
def testTruePredicate12(): Bool =
    (True().) |= #{ True(). True(). True(). }

@test
def testTruePredicate13(): Bool =
    #{ True(). True(). } |= (True().)

@test
def testTruePredicate14(): Bool =
    #{ True(). True(). True(). } |= (True().)

@test
def testTruePredicate15(): Bool =
    (solve True().) |= (True().)

@test
def testTruePredicate16(): Bool =
    (solve (solve True().)) |= (True().)

@test
def testTruePredicate17(): Bool =
    (solve (solve (solve True().))) |= (True().)


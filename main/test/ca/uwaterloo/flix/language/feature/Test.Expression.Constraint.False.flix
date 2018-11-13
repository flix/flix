@test
def testTrueAtom01(): Unit =
    let x = false.;
    ()

@test
def testTrueAtom02(): Unit =
    let x = false.;
    let y = false.;
    ()

@test
def testTrueAtom03(): Unit =
    let x = false.;
    let y = false.;
    let r = union x y;
    ()

@test
def testTrueAtom04(): Unit =
    let x = false.;
    let y = false.;
    let u = union x y;
    let v = union y x;
    ()

@test
def testTrueAtom05(): Unit =
    let x = false.;
    let y = false.;
    let u = union x y;
    let v = union y x;
    let w = union y v;
    ()

@test
def testTrueAtom06(): Unit =
    let x = false.;
    let y = false.;
    let u = check x;
    let v = check y;
    ()

@test
def testTrueAtom07(): Unit =
    let x = false.;
    let y = false.;
    let u = check (union x y);
    let v = check (union y x);
    ()

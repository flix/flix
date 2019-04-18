rel Nullary()

@test
def testNullaryAtom01(): Bool =
    Nullary(). |= Nullary().

@test
def testNullaryAtom02(): Bool =
    let x = Nullary().;
    let y = Nullary().;
    (solve x) |= y

@test
def testNullaryAtom03(): Bool =
    let x = Nullary().;
    let y = Nullary().;
    (solve x <+> y) |= Nullary().

rel Rich()
rel Famous()
rel Happy()

@test
def testNullaryAtom04(): Bool =
    let x = Rich().;
    let y = Famous().;
    (solve x <+> y) |= Rich().

@test
def testNullaryAtom05(): Bool =
    let x = Rich().;
    let y = Famous().;
    (solve x <+> y) |= Famous().

@test
def testNullaryAtom06(): Bool =
    let x = Rich().;
    let y = Famous().;
    !((solve x <+> y) |= Happy().)

@test
def testNullaryAtom07(): Bool =
    let x = Rich().;
    let _ = Famous().;
    let c = Happy() :- Rich(), Famous().;
    !((solve x <+> c) |= Happy().)

@test
def testNullaryAtom08(): Bool =
    let _ = Rich().;
    let y = Famous().;
    let c = Happy() :- Rich(), Famous().;
    !((solve y <+> c) |= Happy().)

@test
def testNullaryAtom09(): Bool =
    let x = Rich().;
    let y = Famous().;
    let c = Happy() :- Rich(), Famous().;
    (solve x <+> y <+> c) |= Happy().

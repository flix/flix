pub rel A(x: Int)
pub rel B(x: Int)
pub rel R(x: Int, y: Int)

@test
def testGuardConst01(): Bool =
    let c = R(1, 1) :- if true.;
    let r = solve c;
    r |= R(1, 1).

@test
def testGuardConst02(): Bool =
    let c = R(1, 1) :- if true && true.;
    let r = solve c;
    r |= R(1, 1).

@test
def testGuardConst03(): Bool =
    let c = R(1, 1) :- if 1 == 1.;
    let r = solve c;
    r |= R(1, 1).

@test
def testGuardConst04(): Bool =
    let c = R(1, 1) :- if false.;
    let r = solve c;
    !(r |= R(1, 1).)

@test
def testGuardConst05(): Bool =
    let c = R(1, 1) :- if true && false.;
    let r = solve c;
    !(r |= R(1, 1).)

@test
def testGuardConst06(): Bool =
    let c = R(1, 1) :- if 1 == 2.;
    let r = solve c;
    !(r |= R(1, 1).)

@test
def testGuardConst07(): Bool =
    let c = R(1, 1) :- if true, if true && true, if 1 == 1.;
    let r = solve c;
    r |= R(1, 1).


/////////////////////////////////////////////////////////////////////////////
// Statement expressions                                                   //
/////////////////////////////////////////////////////////////////////////////

def f01(): Unit =
    ();
    ()

@test
def testStatement01(): Bool = f01() `assertEq!` ()

def f02(): Int =
    1;
    2

@test
def testStatement02(): Bool = f02() `assertEq!` 2

def f03(): Int =
    1;
    2;
    3

@test
def testStatement03(): Bool = f03() `assertEq!` 3

def f04(): Int =
    1;
    2;
    3;
    4

@test
def testStatement04(): Bool = f04() `assertEq!` 4

def f05(): Float64 =
    1.0;
    2.0

@test
def testStatement05(): Bool = f05() `assertEq!` 2.0

def f06(): Float64 =
    1.0;
    2.0;
    3.0

@test
def testStatement06(): Bool = f06() `assertEq!` 3.0

def f07(): Float64 =
    1.0;
    2.0;
    3.0;
    4.0

@test
def testStatement07(): Bool = f07() `assertEq!` 4.0

def f08(): Str =
    "a";
    "b"

@test
def testStatement08(): Bool = f08() `assertEq!` "b"

def f09(): Str =
    "a";
    "b";
    "c"

@test
def testStatement09(): Bool = f09() `assertEq!` "c"

def f10(): Str =
    "a";
    "b";
    "c";
    "d"

@test
def testStatement10(): Bool = f10() `assertEq!` "d"

def f11(): Char =
    'a';
    'b'

@test
def testStatement11(): Bool = f11() `assertEq!` 'b'

def f12(): Char =
    'a';
    'b';
    'c'

@test
def testStatement12(): Bool = f12() `assertEq!` 'c'

def f13(): Char =
    'a';
    'b';
    'c';
    'd'

@test
def testStatement13(): Bool = f13() `assertEq!` 'd'

def f14(): List[Int] =
    1 :: Nil;
    1 :: 2 :: Nil

@test
def testStatement14(): Bool = f14() `assertEq!` 1 :: 2 :: Nil

def f15(): List[Int] =
    1 :: Nil;
    1 :: 2 :: Nil;
    1 :: 2 :: 3 :: Nil

@test
def testStatement15(): Bool = f15() `assertEq!` 1 :: 2 :: 3 :: Nil

def f16(): List[Int] =
    1 :: Nil;
    1 :: 2 :: Nil;
    1 :: 2 :: 3 :: Nil;
    1 :: 2 :: 3 :: 4 :: Nil

@test
def testStatement16(): Bool = f16() `assertEq!` 1 :: 2 :: 3 :: 4 :: Nil

def f17(): Int =
    chan Int 100;
    chan Int 100;
    2

@test
def testStatement17(): Bool = f17() `assertEq!` 2

def f18(): Int =
    chan Int 100;
    chan Int 100;
    chan Int 100;
    10

@test
def testStatement18(): Bool = f18() `assertEq!` 10

def f19(): Int =
    chan Int 100;
    chan Int 100;
    chan Int 100;
    chan Int 100;
    42

@test
def testStatement19(): Bool = f19() `assertEq!` 42

def f20(): Int =
    () -> 2;
    2

@test
def testStatement20(): Bool = f20() `assertEq!` 2

def f21(): Int =
    let x = 4;
    let y = 3;
    x

@test
def testStatement21(): Bool = f21() `assertEq!` 4

def f22(): Int =
    let x = () -> 2;
    x()

@test
def testStatement22(): Bool = f22() `assertEq!` 2

def f23(): Int =
    let x = () -> 2;
    let y = x();
    y

@test
def testStatement23(): Bool = f23() `assertEq!` 2

def t01(): Int = 42

def f24(): Int =
    let x = t01();
    let y = (a, b, c) -> a + b + c;
    y(x, 2, 3)

@test
def testStatement24(): Bool = f24() `assertEq!` 47

def f25(): Int =
    let x = t01();
    let y = (a, b) -> if (true) a else b;
    y(2, 1)

@test
def testStatement25(): Bool = f25() `assertEq!` 2

def f26(): Int =
    let ch = chan Int 100;
    ch <- 2 <- 3;
    ch <- 10;
    <- ch

@test
def testStatement26(): Bool = f26() `assertEq!` 2

def f27(): Int =
    let ch1 = chan Int 100;
    let ch2 = chan Int 100;
    ch1 <- 2;
    ch2 <- 3;
    ch1 <- 10;
    <- ch2

@test
def testStatement27(): Bool = f27() `assertEq!` 3

def f28(): Int =
    let ch1 = chan Int 100;
    let ch2 = chan Int 100;
    spawn g28(ch1);
    ch2 <- 3;
    ch2 <- 10;
    <- ch1

def g28(ch: Channel[Int]): Unit =
    ch <- 2;
    ch <- 10;
    ()

@test
def testStatement28(): Bool = f28() `assertEq!` 2

def f50(): Int =
    1.0;
    "Str";
    ();
    'a';
    2

@test
def testStatement50(): Bool = f50() `assertEq!` 2
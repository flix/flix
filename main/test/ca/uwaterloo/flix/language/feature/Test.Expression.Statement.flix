/////////////////////////////////////////////////////////////////////////////
// Statement expressions                                                   //
/////////////////////////////////////////////////////////////////////////////

def f01(): Unit =
    ();;
    ()

@test
def testStatement01(): Bool = f01() `assertEq!` ()

def f02(): Int =
    1;;
    2

@test
def testStatement02(): Bool = f02() `assertEq!` 2

def f03(): Int =
    1;;
    2;;
    3

@test
def testStatement03(): Bool = f03() `assertEq!` 3

def f04(): Int =
    1;;
    2;;
    3;;
    4

@test
def testStatement04(): Bool = f04() `assertEq!` 4

def f05(): Float64 =
    1.0;;
    2.0

@test
def testStatement05(): Bool = f05() `assertEq!` 2.0

def f06(): Float64 =
    1.0;;
    2.0;;
    3.0

@test
def testStatement06(): Bool = f06() `assertEq!` 3.0

def f07(): Float64 =
    1.0;;
    2.0;;
    3.0;;
    4.0

@test
def testStatement07(): Bool = f07() `assertEq!` 4.0

def f08(): Str =
    "a";;
    "b"

@test
def testStatement08(): Bool = f08() `assertEq!` "b"

def f09(): Str =
    "a";;
    "b";;
    "c"

@test
def testStatement09(): Bool = f09() `assertEq!` "c"

def f10(): Str =
    "a";;
    "b";;
    "c";;
    "d"

@test
def testStatement10(): Bool = f10() `assertEq!` "d"

def f11(): Char =
    'a';;
    'b'

@test
def testStatement11(): Bool = f11() `assertEq!` 'b'

def f12(): Char =
    'a';;
    'b';;
    'c'

@test
def testStatement12(): Bool = f12() `assertEq!` 'c'

def f13(): Char =
    'a';;
    'b';;
    'c';;
    'd'

@test
def testStatement13(): Bool = f13() `assertEq!` 'd'

def f14(): List[Int] =
    1 :: Nil;;
    1 :: 2 :: Nil

//@test
//def testStatement14(): Bool = f14() `assertEq!` 1 :: 2 :: Nil

def f15(): List[Int] =
    1 :: Nil;;
    1 :: 2 :: Nil;;
    1 :: 2 :: 3 :: Nil

//@test
//def testStatement15(): Bool = f15() `assertEq!` 1 :: 2 :: 3 :: Nil

def f16(): List[Int] =
    1 :: Nil;;
    1 :: 2 :: Nil;;
    1 :: 2 :: 3 :: Nil;;
    1 :: 2 :: 3 :: 4 :: Nil

//@test
//def testStatement16(): Bool = f16() `assertEq!` 1 :: 2 :: 3 :: 4 :: Nil

def f17(): Channel[Int] =
    channel Int;;
    channel Int

//@test
//def testStatement17(): Bool = f17() `assertEq!` channel Int

def f18(): Channel[Int] =
    channel Int;;
    channel Int;;
    channel Int

//@test
//def testStatement18(): Bool = f18() `assertEq!` channel Int

def f19(): Channel[Int] =
    channel Int;;
    channel Int;;
    channel Int;;
    channel Int

//@test
//def testStatement19(): Bool = f19() `assertEq!` channel Int

def f50(): Int =
    1.0;;
    "Str";;
    ();;
    'a';;
    2

@test
def testStatement50(): Bool = f50() `assertEq!` 2

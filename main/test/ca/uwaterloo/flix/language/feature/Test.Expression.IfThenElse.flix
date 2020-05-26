/////////////////////////////////////////////////////////////////////////////
// Expression.IfThenElse                                                   //
/////////////////////////////////////////////////////////////////////////////

def f01(): Int = if (false) 42 + 10 else 42 - 10

@test
def testIfThenElse01(): Bool = f01() == 32

def f02(): Int = if (true) 42 + 10 else 42 - 10

@test
def testIfThenElse02(): Bool = f02() == 52

def f03(x: Bool): Int = if (x) (if (false) 1 else 2) else (if (true) 3 else 4)

@test
def testIfThenElse03(): Bool = f03(true) == 2

@test
def testIfThenElse04(): Bool = f03(false) == 3

def f04(x: Bool): Int = if (if (!x) true else false) 1234 else 5678

@test
def testIfThenElse05(): Bool = f04(true) == 5678

@test
def testIfThenElse06(): Bool = f04(false) == 1234

def f05(x: Bool, y: Bool): Int = if (x && y) 1234 else 5678

@test
def testIfThenElse07(): Bool = f05(true, true) == 1234

@test
def testIfThenElse08(): Bool = f05(true, false) == 5678

@test
def testIfThenElse09(): Bool = f05(false, true) == 5678

@test
def testIfThenElse10(): Bool = f05(false, false) == 5678

def f06(x: Bool, y: Bool): Int = if (x || y) 1234 else 5678

@test
def testIfThenElse11(): Bool = f06(true, true) == 1234

@test
def testIfThenElse12(): Bool = f06(true, false) == 1234

@test
def testIfThenElse13(): Bool = f06(false, true) == 1234

@test
def testIfThenElse14(): Bool = f06(false, false) == 5678

def f07(x: Int8, y: Int8): Int8 = if (x < y) 12i8 else 56i8

@test
def testIfThenElse15(): Bool = f07(5i8, 24i8) == 12i8

@test
def testIfThenElse16(): Bool = f07(5i8, 5i8) == 56i8

def f08(x: Int16, y: Int16): Int16 = if (x <= y) 1234i16 else 5678i16

@test
def testIfThenElse17(): Bool = f08(500i16, 500i16) == 1234i16

@test
def testIfThenElse18(): Bool = f08(500i16, 200i16) == 5678i16

def f09(x: Int32, y: Int32): Int32 = if (x > y) 12341234i32 else 56785678i32

@test
def testIfThenElse19(): Bool = f09(2400000i32, 500000i32) == 12341234i32

@test
def testIfThenElse20(): Bool = f09(500000i32, 500000i32) == 56785678i32

def f10(x: Int64, y: Int64): Int64 = if (x >= y) 123412341234i64 else 567856785678i64

@test
def testIfThenElse21(): Bool = f10(50000000000i64, 50000000000i64) == 123412341234i64

@test
def testIfThenElse22(): Bool = f10(20000000000i64, 50000000000i64) == 567856785678i64

def f11(x: Int, y: Int): Int = if (x == y) 1234 else 5678

@test
def testIfThenElse25(): Bool = f11(5, 5) == 1234

@test
def testIfThenElse26(): Bool = f11(2, 5) == 5678

def f12(x: Int, y: Int): Int = if (x != y) 1234 else 5678

@test
def testIfThenElse27(): Bool = f12(2, 5) == 1234

@test
def testIfThenElse28(): Bool = f12(5, 5) == 5678

def f13(x: Int): Int = {
    if      (x == 1)    10
    else if (x >= 0)    20
    else                30
}

@test
def testIfThenElse29(): Bool = f13(1) == 10

@test
def testIfThenElse30(): Bool = f13(0) == 20

@test
def testIfThenElse31(): Bool = f13(-1) == 30
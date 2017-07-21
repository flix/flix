/////////////////////////////////////////////////////////////////////////////
// Expression.IfThenElse                                                   //
/////////////////////////////////////////////////////////////////////////////

def f01(): Int = if (false) 42 + 10 else 42 - 10

@test
def ifThenElseTest01(): Bool = assertEq!(f01(), 32)

def f02(): Int = if (true) 42 + 10 else 42 - 10

@test
def ifThenElseTest02(): Bool = assertEq!(f02(), 52)

def f03(x: Bool): Int = if (x) (if (false) 1 else 2) else (if (true) 3 else 4)

@test
def ifThenElseTest03(): Bool = assertEq!(f03(true), 2)

@test
def ifThenElseTest04(): Bool = assertEq!(f03(false), 3)

def f04(x: Bool): Int = if (if (!x) true else false) 1234 else 5678

@test
def ifThenElseTest05(): Bool = assertEq!(f04(true), 5678)

@test
def ifThenElseTest06(): Bool = assertEq!(f04(false), 1234)

def f05(x: Bool, y: Bool): Int = if (x && y) 1234 else 5678

@test
def ifThenElseTest07(): Bool = assertEq!(f05(true, true), 1234)

@test
def ifThenElseTest08(): Bool = assertEq!(f05(true, false), 5678)

@test
def ifThenElseTest09(): Bool = assertEq!(f05(false, true), 5678)

@test
def ifThenElseTest10(): Bool = assertEq!(f05(false, false), 5678)

def f06(x: Bool, y: Bool): Int = if (x || y) 1234 else 5678

@test
def ifThenElseTest11(): Bool = assertEq!(f06(true, true), 1234)

@test
def ifThenElseTest12(): Bool = assertEq!(f06(true, false), 1234)

@test
def ifThenElseTest13(): Bool = assertEq!(f06(false, true), 1234)

@test
def ifThenElseTest14(): Bool = assertEq!(f06(false, false), 5678)

def f07(x: Int8, y: Int8): Int8 = if (x < y) 12i8 else 56i8

@test
def ifThenElseTest15(): Bool = assertEq!(f07(5i8, 24i8), 12i8)

@test
def ifThenElseTest16(): Bool = assertEq!(f07(5i8, 5i8), 56i8)

def f08(x: Int16, y: Int16): Int16 = if (x <= y) 1234i16 else 5678i16

@test
def ifThenElseTest17(): Bool = assertEq!(f08(500i16, 500i16), 1234i16)

@test
def ifThenElseTest18(): Bool = assertEq!(f08(500i16, 200i16), 5678i16)

def f09(x: Int32, y: Int32): Int32 = if (x > y) 12341234i32 else 56785678i32

@test
def ifThenElseTest19(): Bool = assertEq!(f09(2400000i32, 500000i32), 12341234i32)

@test
def ifThenElseTest20(): Bool = assertEq!(f09(500000i32, 500000i32), 56785678i32)

def f10(x: Int64, y: Int64): Int64 = if (x >= y) 123412341234i64 else 567856785678i64

@test
def ifThenElseTest21(): Bool = assertEq!(f10(50000000000i64, 50000000000i64), 123412341234i64)

@test
def ifThenElseTest22(): Bool = assertEq!(f10(20000000000i64, 50000000000i64), 567856785678i64)

def f11(x: Int, y: Int): Int = if (x == y) 1234 else 5678

@test
def ifThenElseTest25(): Bool = assertEq!(f11(5, 5), 1234)

@test
def ifThenElseTest26(): Bool = assertEq!(f11(2, 5), 5678)

def f12(x: Int, y: Int): Int = if (x != y) 1234 else 5678

@test
def ifThenElseTest27(): Bool = assertEq!(f12(2, 5), 1234)

@test
def ifThenElseTest28(): Bool = assertEq!(f12(5, 5), 5678)

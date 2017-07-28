/////////////////////////////////////////////////////////////////////////////
// Match expressions (pattern matching)                                    //
// These don't exist in the ExecutableAst because they're desugared into   //
// primitives (e.g. CheckTag, GetTagValue, GetTupleIndex).                 //
// Note that these are also good tests of the lambda implementation        //
// (MkClosureRef, ApplyClosure, free variables).                           //
/////////////////////////////////////////////////////////////////////////////

def f01(): Int = match () with {
  case _ => 11
}

@test
def matchWildcardTest01(): Bool = assertEq!(f01(), 11)

def f02(): Int = match 42 with {
  case _ => 11
}

@test
def matchWildcardTest02(): Bool = assertEq!(f02(), 11)

def f03(x: Int): Int = match x with {
  case _ => 11
}

@test
def matchWildcardTest03(): Bool = assertEq!(f03(-1), 11)

@test
def matchWildcardTest04(): Bool = assertEq!(f03(0), 11)

@test
def matchWildcardTest05(): Bool = assertEq!(f03(1), 11)

@test
def matchWildcardTest06(): Bool = assertEq!(f03(99999), 11)

def f04(x: Int): Int = match x with {
  case a => 1
}

@test
def matchVarTest01(): Bool = assertEq!(f04(3), 1)

def f05(x: Int): Int = match x with {
  case a => a
}

@test
def matchVarTest02(): Bool = assertEq!(f05(3), 3)

def f06(x: Int): Int = match x with {
  case a => a + 11
}

@test
def matchVarTest03(): Bool = assertEq!(f06(3), 14)

def f07(x: Unit): Bool = match x with {
  case () => true
}

@test
def matchLiteralTest01(): Bool = assertEq!(f07(()), true)

def f08(x: Bool): Int = match x with {
  case true => 30
  case false => 81
}

@test
def matchLiteralTest02(): Bool = assertEq!(f08(true), 30)

@test
def matchLiteralTest03(): Bool = assertEq!(f08(false), 81)

def f09(x: Bool): Int = match x with {
  case true => 30
  case _ => 81
}

@test
def matchLiteralTest04(): Bool = assertEq!(f09(true), 30)

@test
def matchLiteralTest05(): Bool = assertEq!(f09(false), 81)

def f10(x: Int): Str = match x with {
  case -1 => "minus one"
  case 0 => "zero"
  case 1 => "one"
  case _ => "unknown"
}

@test
def matchLiteralTest06(): Bool = assertEq!(f10(-1), "minus one")

@test
def matchLiteralTest07(): Bool = assertEq!(f10(0), "zero")

@test
def matchLiteralTest08(): Bool = assertEq!(f10(1), "one")

@test
def matchLiteralTest09(): Bool = assertEq!(f10(2), "unknown")

@test
def matchLiteralTest10(): Bool = assertEq!(f10(3), "unknown")

def f11(x: Int8): Str = match x with {
  case -128i8 => "min"
  case -2i8 => "a"
  case 6i8 => "b"
  case 127i8 => "max"
  case _ => "unknown"
}

@test
def matchLiteralTest11(): Bool = assertEq!(f11(-128i8), "min")

@test
def matchLiteralTest12(): Bool = assertEq!(f11(-2i8), "a")

@test
def matchLiteralTest13(): Bool = assertEq!(f11(6i8), "b")

@test
def matchLiteralTest14(): Bool = assertEq!(f11(127i8), "max")

@test
def matchLiteralTest15(): Bool = assertEq!(f11(0i8), "unknown")

def f12(x: Int16): Str = match x with {
  case -32768i16 => "min"
  case -211i16 => "a"
  case 623i16 => "b"
  case 32767i16 => "max"
  case _ => "unknown"
}

@test
def matchLiteralTest16(): Bool = assertEq!(f12(-32768i16), "min")

@test
def matchLiteralTest17(): Bool = assertEq!(f12(-211i16), "a")

@test
def matchLiteralTest18(): Bool = assertEq!(f12(623i16), "b")

@test
def matchLiteralTest19(): Bool = assertEq!(f12(32767i16), "max")

@test
def matchLiteralTest20(): Bool = assertEq!(f12(0i16), "unknown")

def f13(x: Int32): Str = match x with {
  case -2147483648i32 => "min"
  case -2136541i32 => "a"
  case 6254523i32 => "b"
  case 2147483647i32 => "max"
  case _ => "unknown"
}

@test
def matchLiteralTest21(): Bool = assertEq!(f13(-2147483648i32), "min")

@test
def matchLiteralTest22(): Bool = assertEq!(f13(-2136541i32), "a")

@test
def matchLiteralTest23(): Bool = assertEq!(f13(6254523i32), "b")

@test
def matchLiteralTest24(): Bool = assertEq!(f13(2147483647i32), "max")

@test
def matchLiteralTest25(): Bool = assertEq!(f13(0i32), "unknown")

def f14(x: Int64): Str = match x with {
  case -9223372036854775808i64 => "min"
  case -213645454545541i64 => "a"
  case 6287816254523i64 => "b"
  case 9223372036854775807i64 => "max"
  case _ => "unknown"
}

@test
def matchLiteralTest26(): Bool = assertEq!(f14(-9223372036854775808i64), "min")

@test
def matchLiteralTest27(): Bool = assertEq!(f14(-213645454545541i64), "a")

@test
def matchLiteralTest28(): Bool = assertEq!(f14(6287816254523i64), "b")

@test
def matchLiteralTest29(): Bool = assertEq!(f14(9223372036854775807i64), "max")

@test
def matchLiteralTest30(): Bool = assertEq!(f14(0i64), "unknown")

def f15(x: Str): Str = match x with {
  case "one" => "un"
  case "two" => "deux"
  case "three" => "trois"
  case _ => "???"
}

@test
def matchLiteralTest31(): Bool = assertEq!(f15("one"), "un")

@test
def matchLiteralTest32(): Bool = assertEq!(f15("two"), "deux")

@test
def matchLiteralTest33(): Bool = assertEq!(f15("three"), "trois")

@test
def matchLiteralTest34(): Bool = assertEq!(f15("four"), "???")

enum Foo16 { case Bar16, case Baz16, case Abc16(Int,Str), case Xyz16 }

def f16(x: Foo16): Int = match x with {
  case Foo16.Bar16 => 1
  case Foo16.Baz16 => 2
  case Foo16.Abc16(42, "hi") => 3
  case _ => 0
}

@test
def matchLiteralTest35(): Bool = assertEq!(f16(Foo16.Bar16), 1)

@test
def matchLiteralTest36(): Bool = assertEq!(f16(Foo16.Baz16), 2)

@test
def matchLiteralTest37(): Bool = assertEq!(f16(Foo16.Abc16(42, "hi")), 3)

@test
def matchLiteralTest38(): Bool = assertEq!(f16(Foo16.Abc16(42, "hi!")), 0)

@test
def matchLiteralTest39(): Bool = assertEq!(f16(Foo16.Abc16(41, "hi")), 0)

@test
def matchLiteralTest40(): Bool = assertEq!(f16(Foo16.Abc16(40, "a")), 0)

@test
def matchLiteralTest41(): Bool = assertEq!(f16(Foo16.Xyz16), 0)

def f17(x: Str, y: Bool): Int = match (x, y) with {
  case ("hi", false) => 1
  case _ => 2
}

@test
def matchLiteralTest42(): Bool = assertEq!(f17("hi", true), 2)

@test
def matchLiteralTest43(): Bool = assertEq!(f17("hi", false), 1)

@test
def matchLiteralTest44(): Bool = assertEq!(f17("abc", true), 2)

@test
def matchLiteralTest45(): Bool = assertEq!(f17("abc", false), 2)

def f18(x: (Int, (Int, Int))): Int = match x with {
  case (4, (12, 8)) => 1
  case (4, (12, 0)) => 2
  case (1, (12, 8)) => 3
  case _ => 4
}

@test
def matchLiteralTest46(): Bool = assertEq!(f18((4, (12, 8))), 1)

@test
def matchLiteralTest47(): Bool = assertEq!(f18((4, (12, 0))), 2)

@test
def matchLiteralTest48(): Bool = assertEq!(f18((1, (12, 8))), 3)

@test
def matchLiteralTest49(): Bool = assertEq!(f18((1, (12, 0))), 4)

def f19(x: Int, y: Int): Int = match x with {
  case 0 => y
  case _ =>  match y with {
    case 0 => x
    case _ => 0
  }
}

@test
def matchLiteralTest50(): Bool = assertEq!(f19(0, 0), 0)

@test
def matchLiteralTest51(): Bool = assertEq!(f19(1, 0), 1)

@test
def matchLiteralTest52(): Bool = assertEq!(f19(0, 2), 2)

@test
def matchLiteralTest53(): Bool = assertEq!(f19(3, 4), 0)

def f20(x: BigInt): Str = match x with {
  case -9223372036854775809ii => "-"
  case 9223372036854775809ii => "+"
  case _ => "unknown"
}

@test
def matchLiteralTest54(): Bool = assertEq!(f20(-9223372036854775809ii), "-")

@test
def matchLiteralTest55(): Bool = assertEq!(f20(9223372036854775809ii), "+")

@test
def matchLiteralTest56(): Bool = assertEq!(f20(6287816254523ii), "unknown")

enum NameAndAge21 { case T21(Str,Int) }

def f21(x: NameAndAge21): Int =
  let NameAndAge21.T21(_, age) = x;
    age

@test
def matchTagTest01(): Bool = assertEq!(f21(NameAndAge21.T21("James", 42)), 42)

@test
def matchTagTest02(): Bool = assertEq!(f21(NameAndAge21.T21("John", 21)), 21)

@test
def matchTagTest03(): Bool = assertEq!(f21(NameAndAge21.T21("James", 5)), 5)

@test
def matchTagTest04(): Bool = assertEq!(f21(NameAndAge21.T21("Mary", 33)), 33)

enum NameAndAge22 { case T22(Str,Int) }

def f22(x: NameAndAge22): Int = match x with {
  case NameAndAge22.T22("James", age) => age
  case _ => -1
}

@test
def matchTagTest05(): Bool = assertEq!(f22(NameAndAge22.T22("James", 42)), 42)

@test
def matchTagTest06(): Bool = assertEq!(f22(NameAndAge22.T22("John", 21)), -1)

@test
def matchTagTest07(): Bool = assertEq!(f22(NameAndAge22.T22("James", 5)), 5)

@test
def matchTagTest08(): Bool = assertEq!(f22(NameAndAge22.T22("Mary", 33)), -1)

enum ConstProp23 { case Top23, case Val23(Int), case Bot23 }

def f23(x: ConstProp23): Int = match x with {
  case ConstProp23.Top23 => -1
  case ConstProp23.Val23(v) => v
  case ConstProp23.Bot23 => -2
}

@test
def matchTagTest09(): Bool = assertEq!(f23(ConstProp23.Top23), -1)

@test
def matchTagTest10(): Bool = assertEq!(f23(ConstProp23.Val23(42)), 42)

@test
def matchTagTest11(): Bool = assertEq!(f23(ConstProp23.Val23(-24)), -24)

@test
def matchTagTest12(): Bool = assertEq!(f23(ConstProp23.Bot23), -2)

enum BoolTag24 { case Top24, case B24(Bool), case Bot24 }

def f24(x: BoolTag24): Int = match x with {
  case BoolTag24.Top24 => 0
  case BoolTag24.B24(b) => if (b) 1 else -1
  case BoolTag24.Bot24 => 0
}

@test
def matchTagTest13(): Bool = assertEq!(f24(BoolTag24.Top24), 0)

@test
def matchTagTest14(): Bool = assertEq!(f24(BoolTag24.B24(true)), 1)

@test
def matchTagTest15(): Bool = assertEq!(f24(BoolTag24.B24(false)), -1)

@test
def matchTagTest16(): Bool = assertEq!(f24(BoolTag24.Bot24), 0)

enum Val25 { case Nip25, case Val25((Str, Int)) }

def f25(x: Val25): Int = match x with {
  case Val25.Nip25 => 0
  case Val25.Val25(v) => match v with {
    case ("x", y) => -1
    case (_, y) => y
  }
}

@test
def matchTagTest17(): Bool = assertEq!(f25(Val25.Nip25), 0)

@test
def matchTagTest18(): Bool = assertEq!(f25(Val25.Val25(("a", 1))), 1)

@test
def matchTagTest19(): Bool = assertEq!(f25(Val25.Val25(("b", 2))), 2)

@test
def matchTagTest20(): Bool = assertEq!(f25(Val25.Val25(("x", 3))), -1)

enum Val26 { case Nip26, case Val26((Str, Int)) }

def f26(x: Val26): Int = match x with {
  case Val26.Nip26 => 0
  case Val26.Val26(v) => match v with {
    case (x, y) => match x with {
      case "x" => -1
      case _ => match y with {
        case z => z
      }
    }
  }
}

@test
def matchTagTest21(): Bool = assertEq!(f26(Val26.Nip26), 0)

@test
def matchTagTest22(): Bool = assertEq!(f26(Val26.Val26(("a", 1))), 1)

@test
def matchTagTest23(): Bool = assertEq!(f26(Val26.Val26(("b", 2))), 2)

@test
def matchTagTest24(): Bool = assertEq!(f26(Val26.Val26(("x", 3))), -1)

enum A27 { case AA27(Int), case AB27(Int) }

enum B27 { case Top27, case BB27(A27), case Bot27 }

def f27(x: B27): Str = match x with {
  case B27.Top27 => "top"
  case B27.BB27(y) => match y with {
    case A27.AA27(a) => match a with {
      case 0 => "a0"
      case _ => "aaa"
    }
    case A27.AB27(b) => match b with {
      case 0 => "b0"
      case z => "bbb"
    }
  }
  case B27.Bot27 => "bot"
}

@test
def matchTagTest25(): Bool = assertEq!(f27(B27.Top27), "top")

@test
def matchTagTest26(): Bool = assertEq!(f27(B27.Bot27), "bot")

@test
def matchTagTest27(): Bool = assertEq!(f27(B27.BB27(A27.AA27(0))), "a0")

@test
def matchTagTest28(): Bool = assertEq!(f27(B27.BB27(A27.AA27(1))), "aaa")

@test
def matchTagTest29(): Bool = assertEq!(f27(B27.BB27(A27.AB27(0))), "b0")

@test
def matchTagTest30(): Bool = assertEq!(f27(B27.BB27(A27.AB27(-1))), "bbb")

enum Val28 { case Nip28, case Val28(Set[Int]) }

def f28(x: Val28): Set[Int] = match x with {
  case Val28.Nip28 => #{0}
  case Val28.Val28(s) => s
}

@test
def matchTagTest31(): Bool = assertEq!(f28(Val28.Nip28), #{0})

@test
def matchTagTest32(): Bool = assertEq!(f28(Val28.Val28(#{1, 2, 3})), #{1, 2, 3})

enum Val29 { case Val29(Int8) }

def f29(): Int8 = match Val29.Val29(32i8) with {
  case Val29.Val29(x) => x
}

@test
def matchTagTest33(): Bool = assertEq!(f29(), 32i8)

enum Val30 { case Val30(Int16) }

def f30(): Int16 = match Val30.Val30(3200i16) with {
  case Val30.Val30(x) => x
}

@test
def matchTagTest34(): Bool = assertEq!(f30(), 3200i16)

enum Val31 { case Val31(Int32) }

def f31(): Int32 = match Val31.Val31(32000000i32) with {
  case Val31.Val31(x) => x
}

@test
def matchTagTest35(): Bool = assertEq!(f31(), 32000000i32)

enum Val32 { case Val32(Int64) }
def f32(): Int64 = match Val32.Val32(320000000000i64) with {
  case Val32.Val32(x) => x
}

@test
def matchTagTest36(): Bool = assertEq!(f32(), 320000000000i64)

enum Val33 { case Val33(Char) }

def f33(): Char = match Val33.Val33('a') with {
  case Val33.Val33(x) => x
}

@test
def matchTagTest37(): Bool = assertEq!(f33(), 'a')

enum Val34 { case Val34(Float32) }

def f34(): Float32 = match Val34.Val34(4.2f32) with {
  case Val34.Val34(x) => x
}

@test
def matchTagTest38(): Bool = assertEq!(f34(), 4.2f32)

enum Val35 { case Val35(Float64) }

def f35(): Float64 = match Val35.Val35(4.2f64) with {
  case Val35.Val35(x) => x
}

@test
def matchTagTest39(): Bool = assertEq!(f35(), 4.2f64)

enum Val36 { case Val36(BigInt) }

def f36(): BigInt = match Val36.Val36(100000000000000000000ii) with {
  case Val36.Val36(x) => x
}

@test
def matchTagTest40(): Bool = assertEq!(f36(), 100000000000000000000ii)

def f41(x: Int, y: Int): Int =
  let (a, b) = (x, y);
    a + b

@test
def matchTupleTest01(): Bool = assertEq!(f41(5, 6), 11)

@test
def matchTupleTest02(): Bool = assertEq!(f41(6, 5), 11)

@test
def matchTupleTest03(): Bool = assertEq!(f41(100, 23), 123)

def f42(x: Int, y: Bool): Str = match (x, y) with {
  case (5, true) => "abc"
  case (5, _) => "def"
  case (_, true) => "ghi"
  case (_, _) => "jkl"
}

@test
def matchTupleTest04(): Bool = assertEq!(f42(5, true), "abc")

@test
def matchTupleTest05(): Bool = assertEq!(f42(5, false), "def")

@test
def matchTupleTest06(): Bool = assertEq!(f42(6, true), "ghi")

@test
def matchTupleTest07(): Bool = assertEq!(f42(0, false), "jkl")

def f43(x: BigInt, y: Int, z: Int): Int = match (x, (y, z)) with {
  case (1ii, (2, 3)) => -1
  case (1ii, (2, _)) => -2
  case (1ii, (_, 3)) => -3
  case (1ii, _) => -4
  case (_, (a, b)) => a + b
}

@test
def matchTupleTest08(): Bool = assertEq!(f43(1ii, 2, 3), -1)

@test
def matchTupleTest09(): Bool = assertEq!(f43(1ii, 2, 4), -2)

@test
def matchTupleTest10(): Bool = assertEq!(f43(1ii, 3, 3), -3)

@test
def matchTupleTest11(): Bool = assertEq!(f43(1ii, 5, 5), -4)

@test
def matchTupleTest12(): Bool = assertEq!(f43(2ii, 2, 3), 5)

@test
def matchTupleTest13(): Bool = assertEq!(f43(2ii, 10, 20), 30)

enum ConstProp44 { case Top44, case Val44(Int), case Bot44 }

def f44(x: ConstProp44, y: ConstProp44): Int = match (x, y) with {
  case (ConstProp44.Top44, ConstProp44.Top44) => 1
  case (ConstProp44.Bot44, ConstProp44.Bot44) => 2
  case (ConstProp44.Val44(v1), ConstProp44.Val44(v2)) => if (v1 == v2) 3 else 4
  case _ => 5
}

@test
def matchTupleTest14(): Bool = assertEq!(f44(ConstProp44.Top44, ConstProp44.Top44), 1)

@test
def matchTupleTest15(): Bool = assertEq!(f44(ConstProp44.Bot44, ConstProp44.Bot44), 2)

@test
def matchTupleTest16(): Bool = assertEq!(f44(ConstProp44.Val44(42), ConstProp44.Val44(42)), 3)

@test
def matchTupleTest17(): Bool = assertEq!(f44(ConstProp44.Val44(42), ConstProp44.Val44(0)), 4)

@test
def matchTupleTest18(): Bool = assertEq!(f44(ConstProp44.Val44(0), ConstProp44.Val44(42)), 4)

@test
def matchTupleTest19(): Bool = assertEq!(f44(ConstProp44.Top44, ConstProp44.Bot44), 5)

enum NameAndAge45 { case T45(Str,Int) }

def f45(x: Int, y: NameAndAge45): Int = match (x, y) with {
  case (1, NameAndAge45.T45("James", _)) => 1
  case (a, NameAndAge45.T45("James", b)) => a + b
  case (_, NameAndAge45.T45(_, 24)) => 2
  case _ => -1
}

@test
def matchTupleTest20(): Bool = assertEq!(f45(1, NameAndAge45.T45("James", 20)), 1)

@test
def matchTupleTest21(): Bool = assertEq!(f45(1, NameAndAge45.T45("John", 53)), -1)

@test
def matchTupleTest22(): Bool = assertEq!(f45(2, NameAndAge45.T45("James", 20)), 22)

@test
def matchTupleTest23(): Bool = assertEq!(f45(2, NameAndAge45.T45("John", 53)), -1)

@test
def matchTupleTest24(): Bool = assertEq!(f45(3, NameAndAge45.T45("Mary", 24)), 2)

@test
def matchTupleTest25(): Bool = assertEq!(f45(3, NameAndAge45.T45("Anne", 18)), -1)

@test
def matchTupleTest26(): Bool = assertEq!(f45(4, NameAndAge45.T45("Charles", 64)), -1)

def f46(x: Int, y: Int): Int = match (x, y) with {
  case (x, y) => match x with {
    case 0 => y
    case _ => match y with {
      case 0 => x
      case _ => 0
    }
  }
}

@test
def matchTupleTest27(): Bool = assertEq!(f46(0, 0), 0)

@test
def matchTupleTest28(): Bool = assertEq!(f46(1, 0), 1)

@test
def matchTupleTest29(): Bool = assertEq!(f46(0, 2), 2)

@test
def matchTupleTest30(): Bool = assertEq!(f46(3, 4), 0)





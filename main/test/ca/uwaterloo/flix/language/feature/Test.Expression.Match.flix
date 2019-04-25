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
def testMatchWildcard01(): Bool = assertEq!(f01(), 11)

def f02(): Int = match 42 with {
  case _ => 11
}

@test
def testMatchWildcard02(): Bool = assertEq!(f02(), 11)

def f03(x: Int): Int = match x with {
  case _ => 11
}

@test
def testMatchWildcard03(): Bool = assertEq!(f03(-1), 11)

@test
def testMatchWildcard04(): Bool = assertEq!(f03(0), 11)

@test
def testMatchWildcard05(): Bool = assertEq!(f03(1), 11)

@test
def testMatchWildcard06(): Bool = assertEq!(f03(99999), 11)

def f04(x: Int): Int = match x with {
  case _ => 1
}

@test
def testMatchVar01(): Bool = assertEq!(f04(3), 1)

def f05(x: Int): Int = match x with {
  case a => a
}

@test
def testMatchVar02(): Bool = assertEq!(f05(3), 3)

def f06(x: Int): Int = match x with {
  case a => a + 11
}

@test
def testMatchVar03(): Bool = assertEq!(f06(3), 14)

def f07(x: Unit): Bool = match x with {
  case () => true
}

@test
def testMatchLiteral01(): Bool = assertEq!(f07(()), true)

def f08(x: Bool): Int = match x with {
  case true => 30
  case false => 81
}

@test
def testMatchLiteral02(): Bool = assertEq!(f08(true), 30)

@test
def testMatchLiteral03(): Bool = assertEq!(f08(false), 81)

def f09(x: Bool): Int = match x with {
  case true => 30
  case _ => 81
}

@test
def testMatchLiteral04(): Bool = assertEq!(f09(true), 30)

@test
def testMatchLiteral05(): Bool = assertEq!(f09(false), 81)

def f10(x: Int): Str = match x with {
  case -1 => "minus one"
  case 0 => "zero"
  case 1 => "one"
  case _ => "unknown"
}

@test
def testMatchLiteral06(): Bool = assertEq!(f10(-1), "minus one")

@test
def testMatchLiteral07(): Bool = assertEq!(f10(0), "zero")

@test
def testMatchLiteral08(): Bool = assertEq!(f10(1), "one")

@test
def testMatchLiteral09(): Bool = assertEq!(f10(2), "unknown")

@test
def testMatchLiteral10(): Bool = assertEq!(f10(3), "unknown")

def f11(x: Int8): Str = match x with {
  case -128i8 => "min"
  case -2i8 => "a"
  case 6i8 => "b"
  case 127i8 => "max"
  case _ => "unknown"
}

@test
def testMatchLiteral11(): Bool = assertEq!(f11(-128i8), "min")

@test
def testMatchLiteral12(): Bool = assertEq!(f11(-2i8), "a")

@test
def testMatchLiteral13(): Bool = assertEq!(f11(6i8), "b")

@test
def testMatchLiteral14(): Bool = assertEq!(f11(127i8), "max")

@test
def testMatchLiteral15(): Bool = assertEq!(f11(0i8), "unknown")

def f12(x: Int16): Str = match x with {
  case -32768i16 => "min"
  case -211i16 => "a"
  case 623i16 => "b"
  case 32767i16 => "max"
  case _ => "unknown"
}

@test
def testMatchLiteral16(): Bool = assertEq!(f12(-32768i16), "min")

@test
def testMatchLiteral17(): Bool = assertEq!(f12(-211i16), "a")

@test
def testMatchLiteral18(): Bool = assertEq!(f12(623i16), "b")

@test
def testMatchLiteral19(): Bool = assertEq!(f12(32767i16), "max")

@test
def testMatchLiteral20(): Bool = assertEq!(f12(0i16), "unknown")

def f13(x: Int32): Str = match x with {
  case -2147483648i32 => "min"
  case -2136541i32 => "a"
  case 6254523i32 => "b"
  case 2147483647i32 => "max"
  case _ => "unknown"
}

@test
def testMatchLiteral21(): Bool = assertEq!(f13(-2147483648i32), "min")

@test
def testMatchLiteral22(): Bool = assertEq!(f13(-2136541i32), "a")

@test
def testMatchLiteral23(): Bool = assertEq!(f13(6254523i32), "b")

@test
def testMatchLiteral24(): Bool = assertEq!(f13(2147483647i32), "max")

@test
def testMatchLiteral25(): Bool = assertEq!(f13(0i32), "unknown")

def f14(x: Int64): Str = match x with {
  case -9223372036854775808i64 => "min"
  case -213645454545541i64 => "a"
  case 6287816254523i64 => "b"
  case 9223372036854775807i64 => "max"
  case _ => "unknown"
}

@test
def testMatchLiteral26(): Bool = assertEq!(f14(-9223372036854775808i64), "min")

@test
def testMatchLiteral27(): Bool = assertEq!(f14(-213645454545541i64), "a")

@test
def testMatchLiteral28(): Bool = assertEq!(f14(6287816254523i64), "b")

@test
def testMatchLiteral29(): Bool = assertEq!(f14(9223372036854775807i64), "max")

@test
def testMatchLiteral30(): Bool = assertEq!(f14(0i64), "unknown")

def f15(x: Str): Str = match x with {
  case "one" => "un"
  case "two" => "deux"
  case "three" => "trois"
  case _ => "???"
}

@test
def testMatchLiteral31(): Bool = assertEq!(f15("one"), "un")

@test
def testMatchLiteral32(): Bool = assertEq!(f15("two"), "deux")

@test
def testMatchLiteral33(): Bool = assertEq!(f15("three"), "trois")

@test
def testMatchLiteral34(): Bool = assertEq!(f15("four"), "???")

enum Foo16 { case Bar16, case Baz16, case Abc16(Int,Str), case Xyz16 }

def f16(x: Foo16): Int = match x with {
  case Foo16.Bar16 => 1
  case Foo16.Baz16 => 2
  case Foo16.Abc16(42, "hi") => 3
  case _ => 0
}

@test
def testMatchLiteral35(): Bool = assertEq!(f16(Foo16.Bar16), 1)

@test
def testMatchLiteral36(): Bool = assertEq!(f16(Foo16.Baz16), 2)

@test
def testMatchLiteral37(): Bool = assertEq!(f16(Foo16.Abc16(42, "hi")), 3)

@test
def testMatchLiteral38(): Bool = assertEq!(f16(Foo16.Abc16(42, "hi!")), 0)

@test
def testMatchLiteral39(): Bool = assertEq!(f16(Foo16.Abc16(41, "hi")), 0)

@test
def testMatchLiteral40(): Bool = assertEq!(f16(Foo16.Abc16(40, "a")), 0)

@test
def testMatchLiteral41(): Bool = assertEq!(f16(Foo16.Xyz16), 0)

def f17(x: Str, y: Bool): Int = match (x, y) with {
  case ("hi", false) => 1
  case _ => 2
}

@test
def testMatchLiteral42(): Bool = assertEq!(f17("hi", true), 2)

@test
def testMatchLiteral43(): Bool = assertEq!(f17("hi", false), 1)

@test
def testMatchLiteral44(): Bool = assertEq!(f17("abc", true), 2)

@test
def testMatchLiteral45(): Bool = assertEq!(f17("abc", false), 2)

def f18(x: (Int, (Int, Int))): Int = match x with {
  case (4, (12, 8)) => 1
  case (4, (12, 0)) => 2
  case (1, (12, 8)) => 3
  case _ => 4
}

@test
def testMatchLiteral46(): Bool = assertEq!(f18((4, (12, 8))), 1)

@test
def testMatchLiteral47(): Bool = assertEq!(f18((4, (12, 0))), 2)

@test
def testMatchLiteral48(): Bool = assertEq!(f18((1, (12, 8))), 3)

@test
def testMatchLiteral49(): Bool = assertEq!(f18((1, (12, 0))), 4)

def f19(x: Int, y: Int): Int = match x with {
  case 0 => y
  case _ =>  match y with {
    case 0 => x
    case _ => 0
  }
}

@test
def testMatchLiteral50(): Bool = assertEq!(f19(0, 0), 0)

@test
def testMatchLiteral51(): Bool = assertEq!(f19(1, 0), 1)

@test
def testMatchLiteral52(): Bool = assertEq!(f19(0, 2), 2)

@test
def testMatchLiteral53(): Bool = assertEq!(f19(3, 4), 0)

def f20(x: BigInt): Str = match x with {
  case -9223372036854775809ii => "-"
  case 9223372036854775809ii => "+"
  case _ => "unknown"
}

@test
def testMatchLiteral54(): Bool = assertEq!(f20(-9223372036854775809ii), "-")

@test
def testMatchLiteral55(): Bool = assertEq!(f20(9223372036854775809ii), "+")

@test
def testMatchLiteral56(): Bool = assertEq!(f20(6287816254523ii), "unknown")

enum NameAndAge21 { case T21(Str,Int) }

def f21(x: NameAndAge21): Int =
  let NameAndAge21.T21(_, age) = x;
    age

@test
def testMatchTag01(): Bool = assertEq!(f21(NameAndAge21.T21("James", 42)), 42)

@test
def testMatchTag02(): Bool = assertEq!(f21(NameAndAge21.T21("John", 21)), 21)

@test
def testMatchTag03(): Bool = assertEq!(f21(NameAndAge21.T21("James", 5)), 5)

@test
def testMatchTag04(): Bool = assertEq!(f21(NameAndAge21.T21("Mary", 33)), 33)

enum NameAndAge22 { case T22(Str,Int) }

def f22(x: NameAndAge22): Int = match x with {
  case NameAndAge22.T22("James", age) => age
  case _ => -1
}

@test
def testMatchTag05(): Bool = assertEq!(f22(NameAndAge22.T22("James", 42)), 42)

@test
def testMatchTag06(): Bool = assertEq!(f22(NameAndAge22.T22("John", 21)), -1)

@test
def testMatchTag07(): Bool = assertEq!(f22(NameAndAge22.T22("James", 5)), 5)

@test
def testMatchTag08(): Bool = assertEq!(f22(NameAndAge22.T22("Mary", 33)), -1)

enum ConstProp23 { case Top23, case Val23(Int), case Bot23 }

def f23(x: ConstProp23): Int = match x with {
  case ConstProp23.Top23 => -1
  case ConstProp23.Val23(v) => v
  case ConstProp23.Bot23 => -2
}

@test
def testMatchTag09(): Bool = assertEq!(f23(ConstProp23.Top23), -1)

@test
def testMatchTag10(): Bool = assertEq!(f23(ConstProp23.Val23(42)), 42)

@test
def testMatchTag11(): Bool = assertEq!(f23(ConstProp23.Val23(-24)), -24)

@test
def testMatchTag12(): Bool = assertEq!(f23(ConstProp23.Bot23), -2)

enum BoolTag24 { case Top24, case B24(Bool), case Bot24 }

def f24(x: BoolTag24): Int = match x with {
  case BoolTag24.Top24 => 0
  case BoolTag24.B24(b) => if (b) 1 else -1
  case BoolTag24.Bot24 => 0
}

@test
def testMatchTag13(): Bool = assertEq!(f24(BoolTag24.Top24), 0)

@test
def testMatchTag14(): Bool = assertEq!(f24(BoolTag24.B24(true)), 1)

@test
def testMatchTag15(): Bool = assertEq!(f24(BoolTag24.B24(false)), -1)

@test
def testMatchTag16(): Bool = assertEq!(f24(BoolTag24.Bot24), 0)

enum Val25 { case Nip25, case Val25((Str, Int)) }

def f25(x: Val25): Int = match x with {
  case Val25.Nip25 => 0
  case Val25.Val25(v) => match v with {
    case ("x", _) => -1
    case (_, y) => y
  }
}

@test
def testMatchTag17(): Bool = assertEq!(f25(Val25.Nip25), 0)

@test
def testMatchTag18(): Bool = assertEq!(f25(Val25.Val25(("a", 1))), 1)

@test
def testMatchTag19(): Bool = assertEq!(f25(Val25.Val25(("b", 2))), 2)

@test
def testMatchTag20(): Bool = assertEq!(f25(Val25.Val25(("x", 3))), -1)

enum Val26 { case Nip26, case Val26((Str, Int)) }

def f26(x: Val26): Int = match x with {
  case Val26.Nip26 => 0
  case Val26.Val26(v) => match v with {
    case (w, y) => match w with {
      case "x" => -1
      case _ => match y with {
        case z => z
      }
    }
  }
}

@test
def testMatchTag21(): Bool = assertEq!(f26(Val26.Nip26), 0)

@test
def testMatchTag22(): Bool = assertEq!(f26(Val26.Val26(("a", 1))), 1)

@test
def testMatchTag23(): Bool = assertEq!(f26(Val26.Val26(("b", 2))), 2)

@test
def testMatchTag24(): Bool = assertEq!(f26(Val26.Val26(("x", 3))), -1)

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
      case _ => "bbb"
    }
  }
  case B27.Bot27 => "bot"
}

@test
def testMatchTag25(): Bool = assertEq!(f27(B27.Top27), "top")

@test
def testMatchTag26(): Bool = assertEq!(f27(B27.Bot27), "bot")

@test
def testMatchTag27(): Bool = assertEq!(f27(B27.BB27(A27.AA27(0))), "a0")

@test
def testMatchTag28(): Bool = assertEq!(f27(B27.BB27(A27.AA27(1))), "aaa")

@test
def testMatchTag29(): Bool = assertEq!(f27(B27.BB27(A27.AB27(0))), "b0")

@test
def testMatchTag30(): Bool = assertEq!(f27(B27.BB27(A27.AB27(-1))), "bbb")

enum Val28 { case Nip28, case Val28(Set[Int]) }

def f28(x: Val28): Set[Int] = match x with {
  case Val28.Nip28 => #{0}
  case Val28.Val28(s) => s
}

@test
def testMatchTag31(): Bool = assertEq!(f28(Val28.Nip28), #{0})

@test
def testMatchTag32(): Bool = assertEq!(f28(Val28.Val28(#{1, 2, 3})), #{1, 2, 3})

enum Val29 { case Val29(Int8) }

def f29(): Int8 = match Val29.Val29(32i8) with {
  case Val29.Val29(x) => x
}

@test
def testMatchTag33(): Bool = assertEq!(f29(), 32i8)

enum Val30 { case Val30(Int16) }

def f30(): Int16 = match Val30.Val30(3200i16) with {
  case Val30.Val30(x) => x
}

@test
def testMatchTag34(): Bool = assertEq!(f30(), 3200i16)

enum Val31 { case Val31(Int32) }

def f31(): Int32 = match Val31.Val31(32000000i32) with {
  case Val31.Val31(x) => x
}

@test
def testMatchTag35(): Bool = assertEq!(f31(), 32000000i32)

enum Val32 { case Val32(Int64) }
def f32(): Int64 = match Val32.Val32(320000000000i64) with {
  case Val32.Val32(x) => x
}

@test
def testMatchTag36(): Bool = assertEq!(f32(), 320000000000i64)

enum Val33 { case Val33(Char) }

def f33(): Char = match Val33.Val33('a') with {
  case Val33.Val33(x) => x
}

@test
def testMatchTag37(): Bool = assertEq!(f33(), 'a')

enum Val34 { case Val34(Float32) }

def f34(): Float32 = match Val34.Val34(4.2f32) with {
  case Val34.Val34(x) => x
}

@test
def testMatchTag38(): Bool = assertEq!(f34(), 4.2f32)

enum Val35 { case Val35(Float64) }

def f35(): Float64 = match Val35.Val35(4.2f64) with {
  case Val35.Val35(x) => x
}

@test
def testMatchTag39(): Bool = assertEq!(f35(), 4.2f64)

enum Val36 { case Val36(BigInt) }

def f36(): BigInt = match Val36.Val36(100000000000000000000ii) with {
  case Val36.Val36(x) => x
}

@test
def testMatchTag40(): Bool = assertEq!(f36(), 100000000000000000000ii)

def f41(x: Int, y: Int): Int =
  let (a, b) = (x, y);
    a + b

@test
def testMatchTuple01(): Bool = assertEq!(f41(5, 6), 11)

@test
def testMatchTuple02(): Bool = assertEq!(f41(6, 5), 11)

@test
def testMatchTuple03(): Bool = assertEq!(f41(100, 23), 123)

def f42(x: Int, y: Bool): Str = match (x, y) with {
  case (5, true) => "abc"
  case (5, _) => "def"
  case (_, true) => "ghi"
  case (_, _) => "jkl"
}

@test
def testMatchTuple04(): Bool = assertEq!(f42(5, true), "abc")

@test
def testMatchTuple05(): Bool = assertEq!(f42(5, false), "def")

@test
def testMatchTuple06(): Bool = assertEq!(f42(6, true), "ghi")

@test
def testMatchTuple07(): Bool = assertEq!(f42(0, false), "jkl")

def f43(x: BigInt, y: Int, z: Int): Int = match (x, (y, z)) with {
  case (1ii, (2, 3)) => -1
  case (1ii, (2, _)) => -2
  case (1ii, (_, 3)) => -3
  case (1ii, _) => -4
  case (_, (a, b)) => a + b
}

@test
def testMatchTuple08(): Bool = assertEq!(f43(1ii, 2, 3), -1)

@test
def testMatchTuple09(): Bool = assertEq!(f43(1ii, 2, 4), -2)

@test
def testMatchTuple10(): Bool = assertEq!(f43(1ii, 3, 3), -3)

@test
def testMatchTuple11(): Bool = assertEq!(f43(1ii, 5, 5), -4)

@test
def testMatchTuple12(): Bool = assertEq!(f43(2ii, 2, 3), 5)

@test
def testMatchTuple13(): Bool = assertEq!(f43(2ii, 10, 20), 30)

enum ConstProp44 { case Top44, case Val44(Int), case Bot44 }

def f44(x: ConstProp44, y: ConstProp44): Int = match (x, y) with {
  case (ConstProp44.Top44, ConstProp44.Top44) => 1
  case (ConstProp44.Bot44, ConstProp44.Bot44) => 2
  case (ConstProp44.Val44(v1), ConstProp44.Val44(v2)) => if (v1 == v2) 3 else 4
  case _ => 5
}

@test
def testMatchTuple14(): Bool = assertEq!(f44(ConstProp44.Top44, ConstProp44.Top44), 1)

@test
def testMatchTuple15(): Bool = assertEq!(f44(ConstProp44.Bot44, ConstProp44.Bot44), 2)

@test
def testMatchTuple16(): Bool = assertEq!(f44(ConstProp44.Val44(42), ConstProp44.Val44(42)), 3)

@test
def testMatchTuple17(): Bool = assertEq!(f44(ConstProp44.Val44(42), ConstProp44.Val44(0)), 4)

@test
def testMatchTuple18(): Bool = assertEq!(f44(ConstProp44.Val44(0), ConstProp44.Val44(42)), 4)

@test
def testMatchTuple19(): Bool = assertEq!(f44(ConstProp44.Top44, ConstProp44.Bot44), 5)

enum NameAndAge45 { case T45(Str,Int) }

def f45(x: Int, y: NameAndAge45): Int = match (x, y) with {
  case (1, NameAndAge45.T45("James", _)) => 1
  case (a, NameAndAge45.T45("James", b)) => a + b
  case (_, NameAndAge45.T45(_, 24)) => 2
  case _ => -1
}

@test
def testMatchTuple20(): Bool = assertEq!(f45(1, NameAndAge45.T45("James", 20)), 1)

@test
def testMatchTuple21(): Bool = assertEq!(f45(1, NameAndAge45.T45("John", 53)), -1)

@test
def testMatchTuple22(): Bool = assertEq!(f45(2, NameAndAge45.T45("James", 20)), 22)

@test
def testMatchTuple23(): Bool = assertEq!(f45(2, NameAndAge45.T45("John", 53)), -1)

@test
def testMatchTuple24(): Bool = assertEq!(f45(3, NameAndAge45.T45("Mary", 24)), 2)

@test
def testMatchTuple25(): Bool = assertEq!(f45(3, NameAndAge45.T45("Anne", 18)), -1)

@test
def testMatchTuple26(): Bool = assertEq!(f45(4, NameAndAge45.T45("Charles", 64)), -1)

def f46(x: Int, y: Int): Int = match (x, y) with {
  case (u, v) => match u with {
    case 0 => v
    case _ => match v with {
      case 0 => u
      case _ => 0
    }
  }
}

@test
def testMatchTuple27(): Bool = assertEq!(f46(0, 0), 0)

@test
def testMatchTuple28(): Bool = assertEq!(f46(1, 0), 1)

@test
def testMatchTuple29(): Bool = assertEq!(f46(0, 2), 2)

@test
def testMatchTuple30(): Bool = assertEq!(f46(3, 4), 0)
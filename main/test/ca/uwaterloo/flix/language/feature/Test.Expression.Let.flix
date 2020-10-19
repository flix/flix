/////////////////////////////////////////////////////////////////////////////
// Expression.Let                                                          //
/////////////////////////////////////////////////////////////////////////////

def f01(): Int = let _ = true; 42

@test
def testLet01(): Bool = f01() == 42

def f02(): Int8 = let x = 42i8; x

@test
def testLet02(): Bool = f02() == 42i8

def f03(): Int16 = let x = 1i16; x + 2i16

@test
def testLet03(): Bool = f03() == 3i16

def f04(): String = let x = false; if (x) "abz" else "xyz"

@test
def testLet04(): Bool = f04() == "xyz"

def f05(): Int = let x = 14 - 3; x + 2

@test
def testLet05(): Bool = f05() == 13

def f06(): Int =
  let x = 14 - 3;
  let y = 2 * 4;
    x + y

@test
def testLet06(): Bool = f06() == 19

def f07(): Int =
  let x = 1;
  let y = x + 2;
  let z = y + 3;
    z

@test
def testLet07(): Bool = f07() == 6

def f08(_: Int, _: Int, _: Int): Int =
  let _ = 1337;
  let y = -101010;
  let _ = 42;
    y

@test
def testLet08(): Bool = f08(-1337, 101010, -42) == -101010

def f09(_: Int, b: Int, _: Int): Int =
  let _ = 1337;
  let _ = -101010;
  let _ = 42;
    b

@test
def testLet09(): Bool = f09(-1337, 101010, -42) == 101010

def f10(): Int64 = let x = 0i64; x

@test
def testLet10(): Bool = f10() == 0i64

def f11(): Int64 =
  let _ = 1337i64;
  let y = -101010i64;
  let _ = 42i64;
    y

@test
def testLet11(): Bool = f11() == -101010i64

def f12(_: Int64, _: Int64, _: Int64): Int64 =
  let _ = 1337i64;
  let y = -101010i64;
  let _ = 42i64;
    y

@test
def testLet12(): Bool = f12(-1337i64, 101010i64, -42i64) == -101010i64

def f13(_: Int32, _: Int64, _: Int64): Int64 =
  let _ = 1337i32;
  let y = -101010i64;
  let _ = 42i64;
    y

@test
def testLet13(): Bool = f13(-1337i32, 101010i64, -42i64) == -101010i64

def f14(_: Int64, b: Int64, _: Int64): Int64 =
  let _ = 1337i64;
  let _ = -101010i64;
  let _ = 42i64;
    b

@test
def testLet14(): Bool = f14(-1337i64, 101010i64, -42i64) == 101010i64

def f15(_: Int32, b: Int64, _: Int64): Int64 =
  let _ = 1337i32;
  let _ = -101010i64;
  let _ = 42i64;
    b

@test
def testLet15(): Bool = f15(-1337i32, 101010i64, -42i64) == 101010i64

enum ConstProp16 { case Val16(Int) }
def f16(): ConstProp16 = let x = ConstProp16.Val16(42); x

@test
def testLet16(): Bool = f16() == ConstProp16.Val16(42)

def f17(): () = let x = (); x

@test
def testLet17(): Bool = f17() == ()

def f18(): String = let x = "helloworld"; x

@test
def testLet18(): Bool = f18() == "helloworld"

def f19(): (Int, Int) = let x = (123, 456); x

@test
def testLet19(): Bool = f19() == (123, 456)

def f20(): Char =
  let _ = 'a';
  let y = 'b';
    y

@test
def testLet20(): Bool = f20() == 'b'

def f21(): Float32 =
  let _ = 1.2f32;
  let y = 3.4f32;
    y

@test
def testLet21(): Bool = f21() == 3.4f32

def f22(): Float64 =
  let _ = 1.2f64;
  let y = 3.4f64;
    y

@test
def testLet22(): Bool = f22() == 3.4f64

def f23(x: Int): Int32 =
  let y = x + 1;
  let z = y + 2;
    z + 3

@test
def testLet23(): Bool = f23(0) == 6

def f24(x: Int): Int64 =
  let _ = x + 1;
  let y = 40i64;
  let z = y + 2i64;
    z

@test
def testLet24(): Bool = f24(0) == 42i64

def f25(): BigInt =
  let _ = 12345678901234567890ii;
  let y = 98765432109876543210ii;
    y

@test
def testLet25(): Bool = f25() == 98765432109876543210ii

def f26(): Int =
  let x = 42;
    x

@test
def testLet26(): Bool = f26() == 42

def f27(): Int =
  let x = 42;
  let y = 21;
    x + y

@test
def testLet27(): Bool = f27() == 42 + 21

def f28(): Int =
  let x = 42;
  let y = 21;
  let z = 11;
    x + y + z

@test
def testLet28(): Bool = f28() == 42 + 21 + 11

def f29(): Int =
  let x = {
    let a = 1;
    let b = 2;
      a + b
   };
   x

@test
def testLet29(): Bool = f29() == 1 + 2

def f30(): Int =
  let x = {
    let a = 1;
    let b = 2;
      a + b
   };
   let y = {
     let c = 3;
     let d = 4;
       c + d
   };
   x + y

@test
def testLet30(): Bool = f30() == 1 + 2 + 3 + 4

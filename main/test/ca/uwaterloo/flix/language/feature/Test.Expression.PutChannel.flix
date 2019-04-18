@test
def testUnbufferedChannel01(): Unit =
  // Case when get is before put
  // Test that nothing is broken
  let c1 = chan Unit 0;
  let c2 = chan Unit 1;
  spawn {<- c1; c2 <- ()};
  sleep(100i64 * Duration.oneMillisecond());
  c1 <- ();
  <- c2

@test
def testUnbufferedChannel02(): Unit =
  // Case when put is before get
  // Test that nothing is broken
  let c = chan Unit 0;
  spawn c <- ();
  sleep(100i64 * Duration.oneMillisecond());
  <- c

@test
def testUnbufferedChannel03(): Bool =
  // Test that put does not put in any elements before a get is ready
  let c1 = chan Unit 0;
  let c2 = chan Unit 1;

  spawn {c1 <- (); c2 <- ()};
  sleep(100i64 * Duration.oneMillisecond());
  let res = select {
    case _ <- c2 => false
    case _ => {
      <- c1;
      true
    }
  };
  res `assertEq!` true

@test
def testUnbufferedChannel04(): Unit =
  // Test that two waiting putters is allowed
  let c1 = chan Unit 0;
  spawn c1 <- ();
  spawn c1 <- ();
  sleep(100i64 * Duration.oneMillisecond());
  <- c1;
  <- c1

@test
def testUnbufferedChannel05(): Unit =
  // Test unbuffered channels with a select
  let c1 = chan Unit 0;
  // Wait on get first
  spawn {sleep(100i64 * Duration.oneMillisecond()); c1 <- ()};
  select {
    case _ <- c1 => ()
  };
  // Wait on put first
  spawn c1 <- ();
  sleep(100i64 * Duration.oneMillisecond());
  select {
    case _ <- c1 => ()
  }


@test
def testPutChannel01(): Channel[Unit] =
  let c = chan Unit 100;
  c <- ()

@test
def testPutChannel02(): Channel[Bool] =
  let c = chan Bool 100;
  c <- true

@test
def testPutChannel03(): Channel[Char] =
  let c = chan Char 100;
  c <- 'a'

@test
def testPutChannel04(): Channel[Float] =
  let c = chan Float 100;
  c <- 3.0

@test
def testPutChannel05(): Channel[Int] =
  let c = chan Int 100;
  c <- 3

@test
def testPutChannel06(): Channel[BigInt] =
  let c = chan BigInt 100;
  c <- 2ii

@test
def testPutChannel07(): Channel[Str] =
  let c = chan Str 100;
  c <- "test"

@test
def testPutChannel08(): Channel[Channel[Int]] =
  let c = chan Channel[Int] 100;
  c <- chan Int 100
@test
def testPutChannel09(): Channel[Array[Str]] =
  let c = chan Array[Str] 100;
  c <- ["1", "2"]

@test
def testPutChannel10(): Channel[Int] =
  let c = chan Int 100;
  c <- 1 <- 2

@test
def testPutChannel11(): Channel[Int] =
  let c = chan Int 100;
  c <- 1 <- 2 <- 3 <- 4

@test
def testPutChannel12(): Channel[Channel[Int]] =
  let c1 = chan Channel[Int] 100;
  let c2 = chan Int 100;
  c1 <- (c2 <- 1)

@test
def testPutChannel13(): Channel[Channel[Int]] =
  let c1 = chan Channel[Int] 100;
  let c2 = chan Int 100;
  c1 <- (c2 <- 1) <- (c2 <- 2)

@test
def testPutChannel14(): Channel[Int] =
  let c1 = chan Channel[Int] 100;
  let c2 = chan Int 100;
  c1 <- (c2 <- 1) <- (c2 <- 2);
  <- c1

@test
def testPutChannel15(): Int =
  let c1 = chan Channel[Int] 100;
  let c2 = chan Int 100;
  c1 <- (c2 <- 1) <- (c2 <- 2);
  <- <- c1

@test
def testPutChannel16(): Channel[Int] =
  let c = chan Int 100;
  c <- 1 + 1

@test
def testPutChannel17(): Channel[Option[Int]] =
  let c = chan Option[Int] 100;
  c <- Some(42)

@test
def testPutChannel18(): Channel[Option[Int]] =
  let c = chan Option[Int] 100;
  c <- None
  
// TESTS FROM PREVIOUS GROUP
  
// Unit
def fUnit01(ch: Channel[Unit]): Channel[Unit] =
    ch <- ()

def putChannelUnit01(): Unit =
    let ch = chan Unit 100;
    let _ = spawn fUnit01(ch);
    <- ch

@test
def testPutChannelUnit01(): Bool = putChannelUnit01() `assertEq!` ()

def fUnit02(ch: Channel[Unit]): Channel[Unit] =
    ch <- ()

def gUnit02(to: Channel[Unit], from: Channel[Unit]): Channel[Unit] =
    to <- <- from

def putChannelUnit02(): Unit =
    let ch1 = chan Unit 100;
    let ch2 = chan Unit 100;
    let _ = spawn fUnit02(ch1);
    let _ = spawn gUnit02(ch2, ch1);
    <- ch2

@test
def testPutChannelUnit02(): Bool = putChannelUnit02() `assertEq!` ()

def fUnit03(ch: Channel[Unit]): Channel[Unit] =
    ch <- ()

def gUnit03(to: Channel[Unit], from: Channel[Unit]): Channel[Unit] =
    to <- <- from

def putChannelUnit03(): Unit =
    let ch1 = chan Unit 100;
    let ch2 = chan Unit 100;
    let ch3 = chan Unit 100;
    let _ = spawn fUnit03(ch1);
    let _ = spawn fUnit03(ch1);
    let _ = spawn gUnit03(ch2, ch1);
    let _ = spawn gUnit03(ch2, ch1);
    let _ = spawn gUnit03(ch3, ch2);
    let _ = <- ch2;
    <- ch3

@test
def testPutChannelUnit03(): Bool = putChannelUnit03() `assertEq!` ()

def fUnit04(ch: Channel[Unit]): Channel[Unit] =
    ch <- ()

def gUnit04(to: Channel[Unit], through: Channel[Unit], from: Channel[Unit]): Channel[Unit] =
    to <- <- (through <- <- from)

def putChannelUnit04(): Unit =
    let ch1 = chan Unit 100;
    let ch2 = chan Unit 100;
    let ch3 = chan Unit 100;
    let _ = spawn fUnit04(ch1);
    let _ = spawn fUnit04(ch1);
    let _ = spawn gUnit04(ch3, ch2, ch1);
    let _ = spawn gUnit04(ch3, ch2, ch1);
    let _ = <- ch3;
    <- ch3

@test
def testPutChannelUnit04(): Bool = putChannelUnit04() `assertEq!` ()

def fUnit05(ch: Channel[Unit], v1: Unit, v2: Unit): Channel[Unit] =
    ch <- v1 <- v2

def putChannelUnit05(): Unit =
    let ch = chan Unit 100;
    let _ = spawn fUnit05(ch, (), ());
    let _ = <- ch;
    <- ch

@test
def testPutChannelUnit05(): Bool = putChannelUnit05() `assertEq!` ()

// Bool
def fBool01(ch: Channel[Bool]): Channel[Bool] =
    ch <- true

def putChannelBool01(): Bool =
    let ch = chan Bool 100;
    let _ = spawn fBool01(ch);
    <- ch

@test
def testPutChannelBool01(): Bool = putChannelBool01() `assertEq!` true

def fBool02(ch: Channel[Bool]): Channel[Bool] =
    ch <- true

def gBool02(to: Channel[Bool], from: Channel[Bool]): Channel[Bool] =
    to <- <- from

def putChannelBool02(): Bool =
    let ch1 = chan Bool 100;
    let ch2 = chan Bool 100;
    let _ = spawn fBool02(ch1);
    let _ = spawn gBool02(ch2, ch1);
    <- ch2

@test
def testPutChannelBool02(): Bool = putChannelBool02() `assertEq!` true

def fBool03(ch: Channel[Bool]): Channel[Bool] =
    ch <- true

def gBool03(to: Channel[Bool], from: Channel[Bool]): Channel[Bool] =
    to <- <- from

def putChannelBool03(): Bool =
    let ch1 = chan Bool 100;
    let ch2 = chan Bool 100;
    let ch3 = chan Bool 100;
    let _ = spawn fBool03(ch1);
    let _ = spawn fBool03(ch1);
    let _ = spawn gBool03(ch2, ch1);
    let _ = spawn gBool03(ch2, ch1);
    let _ = spawn gBool03(ch3, ch2);
    let _ = <- ch2;
    <- ch3

@test
def testPutChannelBool03(): Bool = putChannelBool03() `assertEq!` true

def fBool04(ch: Channel[Bool]): Channel[Bool] =
    ch <- true

def gBool04(to: Channel[Bool], through: Channel[Bool], from: Channel[Bool]): Channel[Bool] =
    to <- <- (through <- <- from)

def putChannelBool04(): Bool =
    let ch1 = chan Bool 100;
    let ch2 = chan Bool 100;
    let ch3 = chan Bool 100;
    let _ = spawn fBool04(ch1);
    let _ = spawn fBool04(ch1);
    let _ = spawn gBool04(ch3, ch2, ch1);
    let _ = spawn gBool04(ch3, ch2, ch1);
    let _ = <- ch3;
    <- ch3

@test
def testPutChannelBool04(): Bool = putChannelBool04() `assertEq!` true

def fBool05(ch: Channel[Bool], v1: Bool, v2: Bool): Channel[Bool] =
    ch <- v1 <- v2

def putChannelBool05(): Bool =
    let ch = chan Bool 100;
    let _ = spawn fBool05(ch, true, true);
    let _ = <- ch;
    <- ch

@test
def testPutChannelBool05(): Bool = putChannelBool05() `assertEq!` true



// Int
def fInt01(ch: Channel[Int8]): Channel[Int8] =
    ch <- 1i8

def putChannelInt01(): Int8 =
    let ch = chan Int8 100;
    let _ = spawn fInt01(ch);
    <- ch

@test
def testPutChannelInt01(): Bool = putChannelInt01() `assertEq!` 1i8

def fInt02(ch: Channel[Int8]): Channel[Int8] =
    ch <- 1i8

def gInt02(to: Channel[Int8], from: Channel[Int8]): Channel[Int8] =
    to <- <- from

def putChannelInt02(): Int8 =
    let ch1 = chan Int8 100;
    let ch2 = chan Int8 100;
    let _ = spawn fInt02(ch1);
    let _ = spawn gInt02(ch2, ch1);
    <- ch2

@test
def testPutChannelInt02(): Bool = putChannelInt02() `assertEq!` 1i8

def fInt03(ch: Channel[Int8]): Channel[Int8] =
    ch <- 1i8

def gInt03(to: Channel[Int8], from: Channel[Int8]): Channel[Int8] =
    to <- <- from

def putChannelInt03(): Int8 =
    let ch1 = chan Int8 100;
    let ch2 = chan Int8 100;
    let ch3 = chan Int8 100;
    let _ = spawn fInt03(ch1);
    let _ = spawn fInt03(ch1);
    let _ = spawn gInt03(ch2, ch1);
    let _ = spawn gInt03(ch2, ch1);
    let _ = spawn gInt03(ch3, ch2);
    let _ = <- ch2;
    <- ch3

@test
def testPutChannelInt03(): Bool = putChannelInt03() `assertEq!` 1i8

def fInt04(ch: Channel[Int8]): Channel[Int8] =
    ch <- 1i8

def gInt04(to: Channel[Int8], through: Channel[Int8], from: Channel[Int8]): Channel[Int8] =
    to <- <- (through <- <- from)

def putChannelInt04(): Int8 =
    let ch1 = chan Int8 100;
    let ch2 = chan Int8 100;
    let ch3 = chan Int8 100;
    let _ = spawn fInt04(ch1);
    let _ = spawn fInt04(ch1);
    let _ = spawn gInt04(ch3, ch2, ch1);
    let _ = spawn gInt04(ch3, ch2, ch1);
    let _ = <- ch3;
    <- ch3

@test
def testPutChannelInt04(): Bool = putChannelInt04() `assertEq!` 1i8

def fInt05(ch: Channel[Int8], v1: Int8, v2: Int8): Channel[Int8] =
    ch <- v1 <- v2

def putChannelInt05(): Int8 =
    let ch = chan Int8 100;
    let _ = spawn fInt05(ch, 1i8, 1i8);
    let _ = <- ch;
    <- ch

@test
def testPutChannelInt05(): Bool = putChannelInt05() `assertEq!` 1i8

def fInt06(ch: Channel[Int16]): Channel[Int16] =
    ch <- 1i16

def putChannelInt06(): Int16 =
    let ch = chan Int16 100;
    let _ = spawn fInt06(ch);
    <- ch

@test
def testPutChannelInt06(): Bool = putChannelInt06() `assertEq!` 1i16

def fInt07(ch: Channel[Int16]): Channel[Int16] =
    ch <- 1i16

def gInt07(to: Channel[Int16], from: Channel[Int16]): Channel[Int16] =
    to <- <- from

def putChannelInt07(): Int16 =
    let ch1 = chan Int16 100;
    let ch2 = chan Int16 100;
    let _ = spawn fInt07(ch1);
    let _ = spawn gInt07(ch2, ch1);
    <- ch2

@test
def testPutChannelInt07(): Bool = putChannelInt07() `assertEq!` 1i16

def fInt08(ch: Channel[Int16]): Channel[Int16] =
    ch <- 1i16

def gInt08(to: Channel[Int16], from: Channel[Int16]): Channel[Int16] =
    to <- <- from

def putChannelInt08(): Int16 =
    let ch1 = chan Int16 100;
    let ch2 = chan Int16 100;
    let ch3 = chan Int16 100;
    let _ = spawn fInt08(ch1);
    let _ = spawn fInt08(ch1);
    let _ = spawn gInt08(ch2, ch1);
    let _ = spawn gInt08(ch2, ch1);
    let _ = spawn gInt08(ch3, ch2);
    let _ = <- ch2;
    <- ch3

@test
def testPutChannelInt08(): Bool = putChannelInt08() `assertEq!` 1i16

def fInt09(ch: Channel[Int16]): Channel[Int16] =
    ch <- 1i16

def gInt09(to: Channel[Int16], through: Channel[Int16], from: Channel[Int16]): Channel[Int16] =
    to <- <- (through <- <- from)

def putChannelInt09(): Int16 =
    let ch1 = chan Int16 100;
    let ch2 = chan Int16 100;
    let ch3 = chan Int16 100;
    let _ = spawn fInt09(ch1);
    let _ = spawn fInt09(ch1);
    let _ = spawn gInt09(ch3, ch2, ch1);
    let _ = spawn gInt09(ch3, ch2, ch1);
    let _ = <- ch3;
    <- ch3

@test
def testPutChannelInt09(): Bool = putChannelInt09() `assertEq!` 1i16

def fInt10(ch: Channel[Int16], v1: Int16, v2: Int16): Channel[Int16] =
    ch <- v1 <- v2

def putChannelInt10(): Int16 =
    let ch = chan Int16 100;
    let _ = spawn fInt10(ch, 1i16, 1i16);
    let _ = <- ch;
    <- ch

@test
def testPutChannelInt10(): Bool = putChannelInt10() `assertEq!` 1i16

def fInt11(ch: Channel[Int32]): Channel[Int32] =
    ch <- 1i32

def putChannelInt11(): Int32 =
    let ch = chan Int32 100;
    let _ = spawn fInt11(ch);
    <- ch

@test
def testPutChannelInt11(): Bool = putChannelInt11() `assertEq!` 1i32

def fInt12(ch: Channel[Int32]): Channel[Int32] =
    ch <- 1i32

def gInt12(to: Channel[Int32], from: Channel[Int32]): Channel[Int32] =
    to <- <- from

def putChannelInt12(): Int32 =
    let ch1 = chan Int32 100;
    let ch2 = chan Int32 100;
    let _ = spawn fInt12(ch1);
    let _ = spawn gInt12(ch2, ch1);
    <- ch2

@test
def testPutChannelInt12(): Bool = putChannelInt12() `assertEq!` 1i32

def fInt13(ch: Channel[Int32]): Channel[Int32] =
    ch <- 1i32

def gInt13(to: Channel[Int32], from: Channel[Int32]): Channel[Int32] =
    to <- <- from

def putChannelInt13(): Int32 =
    let ch1 = chan Int32 100;
    let ch2 = chan Int32 100;
    let ch3 = chan Int32 100;
    let _ = spawn fInt13(ch1);
    let _ = spawn fInt13(ch1);
    let _ = spawn gInt13(ch2, ch1);
    let _ = spawn gInt13(ch2, ch1);
    let _ = spawn gInt13(ch3, ch2);
    let _ = <- ch2;
    <- ch3

@test
def testPutChannelInt13(): Bool = putChannelInt13() `assertEq!` 1i32

def fInt14(ch: Channel[Int32]): Channel[Int32] =
    ch <- 1i32

def gInt14(to: Channel[Int32], through: Channel[Int32], from: Channel[Int32]): Channel[Int32] =
    to <- <- (through <- <- from)

def putChannelInt14(): Int32 =
    let ch1 = chan Int32 100;
    let ch2 = chan Int32 100;
    let ch3 = chan Int32 100;
    let _ = spawn fInt14(ch1);
    let _ = spawn fInt14(ch1);
    let _ = spawn gInt14(ch3, ch2, ch1);
    let _ = spawn gInt14(ch3, ch2, ch1);
    let _ = <- ch3;
    <- ch3

@test
def testPutChannelInt14(): Bool = putChannelInt14() `assertEq!` 1i32

def fInt15(ch: Channel[Int32], v1: Int32, v2: Int32): Channel[Int32] =
    ch <- v1 <- v2

def putChannelInt15(): Int32 =
    let ch = chan Int32 100;
    let _ = spawn fInt15(ch, 1i32, 1i32);
    let _ = <- ch;
    <- ch

@test
def testPutChannelInt15(): Bool = putChannelInt15() `assertEq!` 1i32

def fInt16(ch: Channel[Int64]): Channel[Int64] =
    ch <- 1i64

def putChannelInt16(): Int64 =
    let ch = chan Int64 100;
    let _ = spawn fInt16(ch);
    <- ch

@test
def testPutChannelInt16(): Bool = putChannelInt16() `assertEq!` 1i64

def fInt17(ch: Channel[Int64]): Channel[Int64] =
    ch <- 1i64

def gInt17(to: Channel[Int64], from: Channel[Int64]): Channel[Int64] =
    to <- <- from

def putChannelInt17(): Int64 =
    let ch1 = chan Int64 100;
    let ch2 = chan Int64 100;
    let _ = spawn fInt17(ch1);
    let _ = spawn gInt17(ch2, ch1);
    <- ch2

@test
def testPutChannelInt17(): Bool = putChannelInt17() `assertEq!` 1i64

def fInt18(ch: Channel[Int64]): Channel[Int64] =
    ch <- 1i64

def gInt18(to: Channel[Int64], from: Channel[Int64]): Channel[Int64] =
    to <- <- from

def putChannelInt18(): Int64 =
    let ch1 = chan Int64 100;
    let ch2 = chan Int64 100;
    let ch3 = chan Int64 100;
    let _ = spawn fInt18(ch1);
    let _ = spawn fInt18(ch1);
    let _ = spawn gInt18(ch2, ch1);
    let _ = spawn gInt18(ch2, ch1);
    let _ = spawn gInt18(ch3, ch2);
    let _ = <- ch2;
    <- ch3

@test
def testPutChannelInt18(): Bool = putChannelInt18() `assertEq!` 1i64

def fInt19(ch: Channel[Int64]): Channel[Int64] =
    ch <- 1i64

def gInt19(to: Channel[Int64], through: Channel[Int64], from: Channel[Int64]): Channel[Int64] =
    to <- <- (through <- <- from)

def putChannelInt19(): Int64 =
    let ch1 = chan Int64 100;
    let ch2 = chan Int64 100;
    let ch3 = chan Int64 100;
    let _ = spawn fInt19(ch1);
    let _ = spawn fInt19(ch1);
    let _ = spawn gInt19(ch3, ch2, ch1);
    let _ = spawn gInt19(ch3, ch2, ch1);
    let _ = <- ch3;
    <- ch3

@test
def testPutChannelInt19(): Bool = putChannelInt19() `assertEq!` 1i64

def fInt20(ch: Channel[Int64], v1: Int64, v2: Int64): Channel[Int64] =
    ch <- v1 <- v2

def putChannelInt20(): Int64 =
    let ch = chan Int64 100;
    let _ = spawn fInt20(ch, 1i64, 1i64);
    let _ = <- ch;
    <- ch

@test
def testPutChannelInt20(): Bool = putChannelInt20() `assertEq!` 1i64

def fInt21(ch: Channel[BigInt]): Channel[BigInt] =
    ch <- 1ii

def putChannelInt21(): BigInt =
    let ch = chan BigInt 100;
    let _ = spawn fInt21(ch);
    <- ch

@test
def testPutChannelInt21(): Bool = putChannelInt21() `assertEq!` 1ii

def fInt22(ch: Channel[BigInt]): Channel[BigInt] =
    ch <- 1ii

def gInt22(to: Channel[BigInt], from: Channel[BigInt]): Channel[BigInt] =
    to <- <- from

def putChannelInt22(): BigInt =
    let ch1 = chan BigInt 100;
    let ch2 = chan BigInt 100;
    let _ = spawn fInt22(ch1);
    let _ = spawn gInt22(ch2, ch1);
    <- ch2

@test
def testPutChannelInt22(): Bool = putChannelInt22() `assertEq!` 1ii

def fInt23(ch: Channel[BigInt]): Channel[BigInt] =
    ch <- 1ii

def gInt23(to: Channel[BigInt], from: Channel[BigInt]): Channel[BigInt] =
    to <- <- from

def putChannelInt23(): BigInt =
    let ch1 = chan BigInt 100;
    let ch2 = chan BigInt 100;
    let ch3 = chan BigInt 100;
    let _ = spawn fInt23(ch1);
    let _ = spawn fInt23(ch1);
    let _ = spawn gInt23(ch2, ch1);
    let _ = spawn gInt23(ch2, ch1);
    let _ = spawn gInt23(ch3, ch2);
    let _ = <- ch2;
    <- ch3

@test
def testPutChannelInt23(): Bool = putChannelInt23() `assertEq!` 1ii

def fInt24(ch: Channel[BigInt]): Channel[BigInt] =
    ch <- 1ii

def gInt24(to: Channel[BigInt], through: Channel[BigInt], from: Channel[BigInt]): Channel[BigInt] =
    to <- <- (through <- <- from)

def putChannelInt24(): BigInt =
    let ch1 = chan BigInt 100;
    let ch2 = chan BigInt 100;
    let ch3 = chan BigInt 100;
    let _ = spawn fInt24(ch1);
    let _ = spawn fInt24(ch1);
    let _ = spawn gInt24(ch3, ch2, ch1);
    let _ = spawn gInt24(ch3, ch2, ch1);
    let _ = <- ch3;
    <- ch3

@test
def testPutChannelInt24(): Bool = putChannelInt24() `assertEq!` 1ii

def fInt25(ch: Channel[BigInt], v1: BigInt, v2: BigInt): Channel[BigInt] =
    ch <- v1 <- v2

def putChannelInt25(): BigInt =
    let ch = chan BigInt 100;
    let _ = spawn fInt25(ch, 1ii, 1ii);
    let _ = <- ch;
    <- ch

@test
def testPutChannelInt25(): Bool = putChannelInt25() `assertEq!` 1ii

// Char
def fChar01(ch: Channel[Char]): Channel[Char] =
    ch <- 'a'

def putChannelChar01(): Char =
    let ch = chan Char 100;
    let _ = spawn fChar01(ch);
    <- ch

@test
def testPutChannelChar01(): Bool = putChannelChar01() `assertEq!` 'a'

def fChar02(ch: Channel[Char]): Channel[Char] =
    ch <- 'a'

def gChar02(to: Channel[Char], from: Channel[Char]): Channel[Char] =
    to <- <- from

def putChannelChar02(): Char =
    let ch1 = chan Char 100;
    let ch2 = chan Char 100;
    let _ = spawn fChar02(ch1);
    let _ = spawn gChar02(ch2, ch1);
    <- ch2

@test
def testPutChannelChar02(): Bool = putChannelChar02() `assertEq!` 'a'

def fChar03(ch: Channel[Char]): Channel[Char] =
    ch <- 'a'

def gChar03(to: Channel[Char], from: Channel[Char]): Channel[Char] =
    to <- <- from

def putChannelChar03(): Char =
    let ch1 = chan Char 100;
    let ch2 = chan Char 100;
    let ch3 = chan Char 100;
    let _ = spawn fChar03(ch1);
    let _ = spawn fChar03(ch1);
    let _ = spawn gChar03(ch2, ch1);
    let _ = spawn gChar03(ch2, ch1);
    let _ = spawn gChar03(ch3, ch2);
    let _ = <- ch2;
    <- ch3

@test
def testPutChannelChar03(): Bool = putChannelChar03() `assertEq!` 'a'

def fChar04(ch: Channel[Char]): Channel[Char] =
    ch <- 'a'

def gChar04(to: Channel[Char], through: Channel[Char], from: Channel[Char]): Channel[Char] =
    to <- <- (through <- <- from)

def putChannelChar04(): Char =
    let ch1 = chan Char 100;
    let ch2 = chan Char 100;
    let ch3 = chan Char 100;
    let _ = spawn fChar04(ch1);
    let _ = spawn fChar04(ch1);
    let _ = spawn gChar04(ch3, ch2, ch1);
    let _ = spawn gChar04(ch3, ch2, ch1);
    let _ = <- ch3;
    <- ch3

@test
def testPutChannelChar04(): Bool = putChannelChar04() `assertEq!` 'a'

def fChar05(ch: Channel[Char], v1: Char, v2: Char): Channel[Char] =
    ch <- v1 <- v2

def putChannelChar05(): Char =
    let ch = chan Char 100;
    let _ = spawn fChar05(ch, 'a', 'a');
    let _ = <- ch;
    <- ch

@test
def testPutChannelChar05(): Bool = putChannelChar05() `assertEq!` 'a'



// Float
def fFloat01(ch: Channel[Float32]): Channel[Float32] =
    ch <- 1.0f32

def putChannelFloat01(): Float32 =
    let ch = chan Float32 100;
    let _ = spawn fFloat01(ch);
    <- ch

@test
def testPutChannelFloat01(): Bool = putChannelFloat01() `assertEq!` 1.0f32

def fFloat02(ch: Channel[Float32]): Channel[Float32] =
    ch <- 1.0f32

def gFloat02(to: Channel[Float32], from: Channel[Float32]): Channel[Float32] =
    to <- <- from

def putChannelFloat02(): Float32 =
    let ch1 = chan Float32 100;
    let ch2 = chan Float32 100;
    let _ = spawn fFloat02(ch1);
    let _ = spawn gFloat02(ch2, ch1);
    <- ch2

@test
def testPutChannelFloat02(): Bool = putChannelFloat02() `assertEq!` 1.0f32

def fFloat03(ch: Channel[Float32]): Channel[Float32] =
    ch <- 1.0f32

def gFloat03(to: Channel[Float32], from: Channel[Float32]): Channel[Float32] =
    to <- <- from

def putChannelFloat03(): Float32 =
    let ch1 = chan Float32 100;
    let ch2 = chan Float32 100;
    let ch3 = chan Float32 100;
    let _ = spawn fFloat03(ch1);
    let _ = spawn fFloat03(ch1);
    let _ = spawn gFloat03(ch2, ch1);
    let _ = spawn gFloat03(ch2, ch1);
    let _ = spawn gFloat03(ch3, ch2);
    let _ = <- ch2;
    <- ch3

@test
def testPutChannelFloat03(): Bool = putChannelFloat03() `assertEq!` 1.0f32

def fFloat04(ch: Channel[Float32]): Channel[Float32] =
    ch <- 1.0f32

def gFloat04(to: Channel[Float32], through: Channel[Float32], from: Channel[Float32]): Channel[Float32] =
    to <- <- (through <- <- from)

def putChannelFloat04(): Float32 =
    let ch1 = chan Float32 100;
    let ch2 = chan Float32 100;
    let ch3 = chan Float32 100;
    let _ = spawn fFloat04(ch1);
    let _ = spawn fFloat04(ch1);
    let _ = spawn gFloat04(ch3, ch2, ch1);
    let _ = spawn gFloat04(ch3, ch2, ch1);
    let _ = <- ch3;
    <- ch3

@test
def testPutChannelFloat04(): Bool = putChannelFloat04() `assertEq!` 1.0f32

def fFloat05(ch: Channel[Float32], v1: Float32, v2: Float32): Channel[Float32] =
    ch <- v1 <- v2

def putChannelFloat05(): Float32 =
    let ch = chan Float32 100;
    let _ = spawn fFloat05(ch, 1.0f32, 1.0f32);
    let _ = <- ch;
    <- ch

@test
def testPutChannelFloat05(): Bool = putChannelFloat05() `assertEq!` 1.0f32

def fFloat06(ch: Channel[Float64]): Channel[Float64] =
    ch <- 1.0f64

def putChannelFloat06(): Float64 =
    let ch = chan Float64 100;
    let _ = spawn fFloat06(ch);
    <- ch

@test
def testPutChannelFloat06(): Bool = putChannelFloat06() `assertEq!` 1.0f64

def fFloat07(ch: Channel[Float64]): Channel[Float64] =
    ch <- 1.0f64

def gFloat07(to: Channel[Float64], from: Channel[Float64]): Channel[Float64] =
    to <- <- from

def putChannelFloat07(): Float64 =
    let ch1 = chan Float64 100;
    let ch2 = chan Float64 100;
    let _ = spawn fFloat07(ch1);
    let _ = spawn gFloat07(ch2, ch1);
    <- ch2

@test
def testPutChannelFloat07(): Bool = putChannelFloat07() `assertEq!` 1.0f64

def fFloat08(ch: Channel[Float64]): Channel[Float64] =
    ch <- 1.0f64

def gFloat08(to: Channel[Float64], from: Channel[Float64]): Channel[Float64] =
    to <- <- from

def putChannelFloat08(): Float64 =
    let ch1 = chan Float64 100;
    let ch2 = chan Float64 100;
    let ch3 = chan Float64 100;
    let _ = spawn fFloat08(ch1);
    let _ = spawn fFloat08(ch1);
    let _ = spawn gFloat08(ch2, ch1);
    let _ = spawn gFloat08(ch2, ch1);
    let _ = spawn gFloat08(ch3, ch2);
    let _ = <- ch2;
    <- ch3

@test
def testPutChannelFloat08(): Bool = putChannelFloat08() `assertEq!` 1.0f64

def fFloat09(ch: Channel[Float64]): Channel[Float64] =
    ch <- 1.0f64

def gFloat09(to: Channel[Float64], through: Channel[Float64], from: Channel[Float64]): Channel[Float64] =
    to <- <- (through <- <- from)

def putChannelFloat09(): Float64 =
    let ch1 = chan Float64 100;
    let ch2 = chan Float64 100;
    let ch3 = chan Float64 100;
    let _ = spawn fFloat09(ch1);
    let _ = spawn fFloat09(ch1);
    let _ = spawn gFloat09(ch3, ch2, ch1);
    let _ = spawn gFloat09(ch3, ch2, ch1);
    let _ = <- ch3;
    <- ch3

@test
def testPutChannelFloat09(): Bool = putChannelFloat09() `assertEq!` 1.0f64

def fFloat10(ch: Channel[Float64], v1: Float64, v2: Float64): Channel[Float64] =
    ch <- v1 <- v2

def putChannelFloat10(): Float64 =
    let ch = chan Float64 100;
    let _ = spawn fFloat10(ch, 1.0f64, 1.0f64);
    let _ = <- ch;
    <- ch

@test
def testPutChannelFloat10(): Bool = putChannelFloat10() `assertEq!` 1.0f64



// Str
def fStr01(ch: Channel[Str]): Channel[Str] =
    ch <- "a"

def putChannelStr01(): Str =
    let ch = chan Str 100;
    let _ = spawn fStr01(ch);
    <- ch

@test
def testPutChannelStr01(): Bool = putChannelStr01() `assertEq!` "a"

def fStr02(ch: Channel[Str]): Channel[Str] =
    ch <- "a"

def gStr02(to: Channel[Str], from: Channel[Str]): Channel[Str] =
    to <- <- from

def putChannelStr02(): Str =
    let ch1 = chan Str 100;
    let ch2 = chan Str 100;
    let _ = spawn fStr02(ch1);
    let _ = spawn gStr02(ch2, ch1);
    <- ch2

@test
def testPutChannelStr02(): Bool = putChannelStr02() `assertEq!` "a"

def fStr03(ch: Channel[Str]): Channel[Str] =
    ch <- "a"

def gStr03(to: Channel[Str], from: Channel[Str]): Channel[Str] =
    to <- <- from

def putChannelStr03(): Str =
    let ch1 = chan Str 100;
    let ch2 = chan Str 100;
    let ch3 = chan Str 100;
    let _ = spawn fStr03(ch1);
    let _ = spawn fStr03(ch1);
    let _ = spawn gStr03(ch2, ch1);
    let _ = spawn gStr03(ch2, ch1);
    let _ = spawn gStr03(ch3, ch2);
    let _ = <- ch2;
    <- ch3

@test
def testPutChannelStr03(): Bool = putChannelStr03() `assertEq!` "a"

def fStr04(ch: Channel[Str]): Channel[Str] =
    ch <- "a"

def gStr04(to: Channel[Str], through: Channel[Str], from: Channel[Str]): Channel[Str] =
    to <- <- (through <- <- from)

def putChannelStr04(): Str =
    let ch1 = chan Str 100;
    let ch2 = chan Str 100;
    let ch3 = chan Str 100;
    let _ = spawn fStr04(ch1);
    let _ = spawn fStr04(ch1);
    let _ = spawn gStr04(ch3, ch2, ch1);
    let _ = spawn gStr04(ch3, ch2, ch1);
    let _ = <- ch3;
    <- ch3

@test
def testPutChannelStr04(): Bool = putChannelStr04() `assertEq!` "a"

def fStr05(ch: Channel[Str], v1: Str, v2: Str): Channel[Str] =
    ch <- v1 <- v2

def putChannelStr05(): Str =
    let ch = chan Str 100;
    let _ = spawn fStr05(ch, "a", "a");
    let _ = <- ch;
    <- ch

@test
def testPutChannelStr05(): Bool = putChannelStr05() `assertEq!` "a"

// Channel
def fChannel01(ch: Channel[Int]): Channel[Int] =
    ch <- 1

def gChannel01(ch1: Channel[Channel[Int]], ch2: Channel[Int]): Channel[Channel[Int]] =
    ch1 <- ch2

def putChannelChannelInt01(): Int =
    let ch1 = chan Channel[Int] 100;
    let ch2 = chan Int 100;
    let _ = spawn fChannel01(ch2);
    let _ = spawn gChannel01(ch1, ch2);
    <- <- ch1

@test
def testputChannelChannelInt01(): Bool = putChannelChannelInt01() `assertEq!` 1

def fChannel02(ch: Channel[Int]): Channel[Int] =
    ch <- 1

def gChannel02(ch1: Channel[Channel[Int]], ch2: Channel[Int]): Channel[Channel[Int]] =
    ch1 <- ch2

def hChannel02(to: Channel[Channel[Int]], from: Channel[Channel[Int]]): Channel[Channel[Int]] =
    to <- <- from

def putChannelChannelInt02(): Int =
    let ch1 = chan Channel[Int] 100;
    let ch2 = chan Channel[Int] 100;
    let ch3 = chan Int 100;
    let _ = spawn fChannel02(ch3);
    let _ = spawn gChannel02(ch2, ch3);
    let _ = spawn hChannel02(ch1, ch2);
    <- <- ch1

@test
def testputChannelChannelInt02(): Bool = putChannelChannelInt02() `assertEq!` 1

def fChnnel03(ch: Channel[Int]): Channel[Int] =
    ch <- 1

def gChnnel03(ch1: Channel[Channel[Int]], ch2: Channel[Int]): Channel[Channel[Int]] =
    ch1 <- ch2

def hChnnel03(to: Channel[Channel[Int]], from: Channel[Channel[Int]]): Channel[Channel[Int]] =
    to <- <- from

def putChannelChannelInt03(): Int =
    let ch1 = chan Channel[Int] 100;
    let ch2 = chan Channel[Int] 100;
    let ch3 = chan Channel[Int] 100;
    let ch4 = chan Int 100;
    let ch5 = chan Int 100;
    let _ = spawn fChnnel03(ch4);
    let _ = spawn fChnnel03(ch5);
    let _ = spawn gChnnel03(ch3, ch4);
    let _ = spawn gChnnel03(ch3, ch5);
    let _ = spawn hChnnel03(ch2, ch3);
    let _ = spawn hChnnel03(ch2, ch3);
    let _ = spawn hChnnel03(ch1, ch2);
    let _ = <- ch2;
    <- <- ch1

@test
def testputChannelChannelInt03(): Bool = putChannelChannelInt03() `assertEq!` 1

def fChnnel04(ch: Channel[Int]): Channel[Int] =
    ch <- 1

def gChnnel04(ch1: Channel[Channel[Int]], ch2: Channel[Int]): Channel[Channel[Int]] =
    ch1 <- ch2

def hChnnel04(to: Channel[Channel[Int]], through: Channel[Channel[Int]], from: Channel[Channel[Int]]): Channel[Channel[Int]] =
    to <- <- (through <- <- from)

def putChannelChannelInt04(): Int =
    let ch1 = chan Channel[Int] 100;
    let ch2 = chan Channel[Int] 100;
    let ch3 = chan Channel[Int] 100;
    let ch4 = chan Int 100;
    let ch5 = chan Int 100;
    let _ = spawn fChnnel04(ch4);
    let _ = spawn fChnnel04(ch5);
    let _ = spawn gChnnel04(ch3, ch4);
    let _ = spawn gChnnel04(ch3, ch5);
    let _ = spawn hChnnel04(ch1, ch2, ch3);
    let _ = spawn hChnnel04(ch1, ch2, ch3);
    let _ = <- ch1;
    <- <- ch1

@test
def testputChannelChannelInt04(): Bool = putChannelChannelInt04() `assertEq!` 1

def fChnnel05(ch: Channel[Int]): Channel[Int] =
    ch <- 1

def gChnnel05(ch: Channel[Channel[Int]], v1: Channel[Int], v2: Channel[Int]): Channel[Channel[Int]] =
    ch <- v1 <- v2

def putChannelChannelInt05(): Int =
    let ch1 = chan Channel[Int] 100;
    let ch2 = chan Int 100;
    let ch3 = chan Int 100;
    let _ = spawn fChnnel05(ch2);
    let _ = spawn fChnnel05(ch3);
    let _ = spawn gChnnel05(ch1, ch2, ch3);
    <- <- ch1

@test
def testputChannelChannelInt05(): Bool = putChannelChannelInt05() `assertEq!` 1

// List
def fList01(ch: Channel[List[Int]]): Channel[List[Int]] =
    ch <- 1 :: Nil

def putChannelList01(): List[Int] =
    let ch = chan List[Int] 100;
    let _ = spawn fList01(ch);
    <- ch

@test
def testPutChannelList01(): Bool = putChannelList01() `assertEq!` 1 :: Nil

def fList02(ch: Channel[List[Int]]): Channel[List[Int]] =
    ch <- 1 :: Nil

def gList02(to: Channel[List[Int]], from: Channel[List[Int]]): Channel[List[Int]] =
    to <- <- from

def putChannelList02(): List[Int] =
    let ch1 = chan List[Int] 100;
    let ch2 = chan List[Int] 100;
    let _ = spawn fList02(ch1);
    let _ = spawn gList02(ch2, ch1);
    <- ch2

@test
def testPutChannelList02(): Bool = putChannelList02() `assertEq!` 1 :: Nil

def fList03(ch: Channel[List[Int]]): Channel[List[Int]] =
    ch <- 1 :: Nil

def gList03(to: Channel[List[Int]], from: Channel[List[Int]]): Channel[List[Int]] =
    to <- <- from

def putChannelList03(): List[Int] =
    let ch1 = chan List[Int] 100;
    let ch2 = chan List[Int] 100;
    let ch3 = chan List[Int] 100;
    let _ = spawn fList03(ch1);
    let _ = spawn fList03(ch1);
    let _ = spawn gList03(ch2, ch1);
    let _ = spawn gList03(ch2, ch1);
    let _ = spawn gList03(ch3, ch2);
    let _ = <- ch2;
    <- ch3

@test
def testPutChannelList03(): Bool = putChannelList03() `assertEq!` 1 :: Nil

def fList04(ch: Channel[List[Int]]): Channel[List[Int]] =
    ch <- 1 :: Nil

def gList04(to: Channel[List[Int]], through: Channel[List[Int]], from: Channel[List[Int]]): Channel[List[Int]] =
    to <- <- (through <- <- from)

def putChannelList04(): List[Int] =
    let ch1 = chan List[Int] 100;
    let ch2 = chan List[Int] 100;
    let ch3 = chan List[Int] 100;
    let _ = spawn fList04(ch1);
    let _ = spawn fList04(ch1);
    let _ = spawn gList04(ch3, ch2, ch1);
    let _ = spawn gList04(ch3, ch2, ch1);
    let _ = <- ch3;
    <- ch3

@test
def testPutChannelList04(): Bool = putChannelList04() `assertEq!` 1 :: Nil

def fList05(ch: Channel[List[Int]], v1: List[Int], v2: List[Int]): Channel[List[Int]] =
    ch <- v1 <- v2

def putChannelList05(): List[Int] =
    let ch = chan List[Int] 100;
    let _ = spawn fList05(ch, 1 :: Nil, 1 :: Nil);
    let _ = <- ch;
    <- ch

@test
def testPutChannelList05(): Bool = putChannelList05() `assertEq!` 1 :: Nil
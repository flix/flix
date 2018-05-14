import Char

/////////////////////////////////////////////////////////////////////////////
// Select expressions (channel select)                                     //
/////////////////////////////////////////////////////////////////////////////


// Units
def f01(ch: Channel[Unit]): Channel[Unit] =
  ch <- ()

def selectChannel01(): Unit =
  let ch = channel Unit 1;
  let _ = spawn f01(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannel01(): Bool = selectChannel01() `assertEq!` ()

def f02(ch: Channel[Unit]): Channel[Unit] =
  ch <- ()

def selectChannel02(): Unit =
  let ch = channel Unit 10;
  let _ = spawn f02(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannel02(): Bool = selectChannel02() `assertEq!` ()

def f03(ch: Channel[Unit]): Channel[Unit] =
  ch <- ()

def selectChannel03(): Unit =
  let ch = channel Unit 2147483647;
  let _ = spawn f03(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannel03(): Bool = selectChannel03() `assertEq!` ()

def selectChannel04(): Unit =
  let ch = channel Unit 1;
  let _ = ch <- ();
  select {
    case x <- ch => x
  }

@test
def testSelectChannel04(): Bool = selectChannel04() `assertEq!` ()

def selectChannel05(): Unit =
  let ch = channel Unit 1;
  let _ = ch <- ();
  select {
    case x <- ch => ()
  }

@test
def testSelectChannel05(): Bool = selectChannel05() `assertEq!` ()

def f06(ch: Channel[Unit]): Channel[Unit] =
  ch <- ()

def selectChannel06(): Unit =
  let ch1 = channel Unit 1;
  let ch2 = channel Unit 1;
  let _ = spawn f06(ch1);
  select {
    case x <- ch1 => x
  }

@test
def testSelectChannel06(): Bool = selectChannel06() `assertEq!` ()

def f07(ch: Channel[Unit]): Channel[Unit] =
  ch <- ()

def selectChannel07(): Unit =
  let ch1 = channel Unit 1;
  let ch2 = channel Unit 1;
  let _ = spawn f07(ch2);
  select {
    case x <- ch2 => x
  }

@test
def testSelectChannel07(): Bool = selectChannel07() `assertEq!` ()


// Bool
def f08(ch: Channel[Bool]): Channel[Bool] =
  ch <- true

def selectChannel08(): Bool =
  let ch = channel Bool 1;
  let _ = spawn f08(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannel08(): Bool = selectChannel08() `assertEq!` true

def f09(ch: Channel[Bool]): Channel[Bool] =
  ch <- false

def selectChannel09(): Bool =
  let ch = channel Bool 1;
  let _ = spawn f09(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannel09(): Bool = selectChannel09() `assertEq!` false

def f10(ch: Channel[Bool]): Channel[Bool] =
  ch <- true

def selectChannel10(): Bool =
  let ch = channel Bool 10;
  let _ = spawn f10(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannel10(): Bool = selectChannel10() `assertEq!` true

def f11(ch: Channel[Bool]): Channel[Bool] =
  ch <- false

def selectChannel11(): Bool =
  let ch = channel Bool 10;
  let _ = spawn f11(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannel11(): Bool = selectChannel11() `assertEq!` false

def f12(ch: Channel[Bool]): Channel[Bool] =
  ch <- true

def selectChannel12(): Bool =
  let ch = channel Bool 2147483647;
  let _ = spawn f12(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannel12(): Bool = selectChannel12() `assertEq!` true

def f13(ch: Channel[Bool]): Channel[Bool] =
  ch <- false

def selectChannel13(): Bool =
  let ch = channel Bool 2147483647;
  let _ = spawn f13(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannel13(): Bool = selectChannel13() `assertEq!` false

def selectChannel14(): Bool =
  let ch = channel Bool 1;
  let _ = ch <- true;
  select {
    case x <- ch => x
  }

@test
def testSelectChannel14(): Bool = selectChannel14() `assertEq!` true

def selectChannel15(): Bool =
  let ch = channel Bool 1;
  let _ = ch <- false;
  select {
    case x <- ch => x
  }

@test
def testSelectChannel15(): Bool = selectChannel15() `assertEq!` false

def selectChannel16(): Bool =
  let ch = channel Bool 1;
  let _ = ch <- true;
  select {
    case x <- ch => true
  }

@test
def testSelectChannel16(): Bool = selectChannel16() `assertEq!` true

def selectChannel17(): Bool =
  let ch = channel Bool 1;
  let _ = ch <- false;
  select {
    case x <- ch => false
  }

@test
def testSelectChannel17(): Bool = selectChannel17() `assertEq!` false

def selectChannel18(): Bool =
  let ch = channel Bool 1;
  let _ = ch <- true;
  select {
    case x <- ch => true && true
  }

@test
def testSelectChannel18(): Bool = selectChannel18() `assertEq!` true

def selectChannel19(): Bool =
  let ch = channel Bool 1;
  let _ = ch <- true;
  select {
    case x <- ch => true && false
  }

@test
def testSelectChannel19(): Bool = selectChannel19() `assertEq!` false

def selectChannel20(): Bool =
  let ch = channel Bool 1;
  let _ = ch <- true;
  select {
    case x <- ch => false && false
  }

@test
def testSelectChannel20(): Bool = selectChannel20() `assertEq!` false

def selectChannel21(): Bool =
  let ch = channel Bool 1;
  let _ = ch <- true;
  select {
    case x <- ch => false && true
  }

@test
def testSelectChannel21(): Bool = selectChannel21() `assertEq!` false

def selectChannel22(): Bool =
  let ch = channel Bool 1;
  let _ = ch <- true;
  select {
    case x <- ch => true || true
  }

@test
def testSelectChannel22(): Bool = selectChannel22() `assertEq!` true

def selectChannel23(): Bool =
  let ch = channel Bool 1;
  let _ = ch <- true;
  select {
    case x <- ch => true || false
  }

@test
def testSelectChannel23(): Bool = selectChannel23() `assertEq!` true

def selectChannel24(): Bool =
  let ch = channel Bool 1;
  let _ = ch <- true;
  select {
    case x <- ch => false || false
  }

@test
def testSelectChannel24(): Bool = selectChannel24() `assertEq!` false

def selectChannel25(): Bool =
  let ch = channel Bool 1;
  let _ = ch <- true;
  select {
    case x <- ch => false || true
  }

@test
def testSelectChannel25(): Bool = selectChannel25() `assertEq!` true

def selectChannel26(): Bool =
  let ch = channel Bool 1;
  let _ = ch <- true;
  select {
    case x <- ch => x && true
  }

@test
def testSelectChannel26(): Bool = selectChannel26() `assertEq!` true

def selectChannel27(): Bool =
  let ch = channel Bool 1;
  let _ = ch <- true;
  select {
    case x <- ch => x && false
  }

@test
def testSelectChannel27(): Bool = selectChannel27() `assertEq!` false

def selectChannel28(): Bool =
  let ch = channel Bool 1;
  let _ = ch <- false;
  select {
    case x <- ch => x && false
  }

@test
def testSelectChannel28(): Bool = selectChannel28() `assertEq!` false

def selectChannel29(): Bool =
  let ch = channel Bool 1;
  let _ = ch <- false;
  select {
    case x <- ch => x && true
  }

@test
def testSelectChannel29(): Bool = selectChannel29() `assertEq!` false

def selectChannel30(): Bool =
  let ch = channel Bool 1;
  let _ = ch <- true;
  select {
    case x <- ch => x || true
  }

@test
def testSelectChannel30(): Bool = selectChannel30() `assertEq!` true

def selectChannel31(): Bool =
  let ch = channel Bool 1;
  let _ = ch <- true;
  select {
    case x <- ch => x || false
  }

@test
def testSelectChannel31(): Bool = selectChannel31() `assertEq!` true

def selectChannel32(): Bool =
  let ch = channel Bool 1;
  let _ = ch <- false;
  select {
    case x <- ch => x || false
  }

@test
def testSelectChannel32(): Bool = selectChannel32() `assertEq!` false

def selectChannel33(): Bool =
  let ch = channel Bool 1;
  let _ = ch <- false;
  select {
    case x <- ch => x || true
  }

@test
def testSelectChannel33(): Bool = selectChannel33() `assertEq!` true

def f34(ch: Channel[Bool]): Channel[Bool] =
  ch <- true

def selectChannel34(): Bool =
  let ch1 = channel Bool 1;
  let ch2 = channel Bool 1;
  let _ = spawn f34(ch1);
  select {
    case x <- ch1 => x
  }

@test
def testSelectChannel34(): Bool = selectChannel34() `assertEq!` true

def f35(ch: Channel[Bool]): Channel[Bool] =
  ch <- false

def selectChannel35(): Bool =
  let ch1 = channel Bool 1;
  let ch2 = channel Bool 1;
  let _ = spawn f35(ch1);
  select {
    case x <- ch1 => x
  }

@test
def testSelectChannel35(): Bool = selectChannel35() `assertEq!` false

def f36(ch: Channel[Bool]): Channel[Bool] =
  ch <- true

def selectChannel36(): Bool =
  let ch1 = channel Bool 1;
  let ch2 = channel Bool 1;
  let _ = spawn f36(ch2);
  select {
    case x <- ch2 => x
  }

@test
def testSelectChannel36(): Bool = selectChannel36() `assertEq!` true

def f37(ch: Channel[Bool]): Channel[Bool] =
  ch <- false

def selectChannel37(): Bool =
  let ch1 = channel Bool 1;
  let ch2 = channel Bool 1;
  let _ = spawn f37(ch2);
  select {
    case x <- ch2 => x
  }

@test
def testSelectChannel37(): Bool = selectChannel37() `assertEq!` false

// Int
def f38(ch: Channel[Int]): Channel[Int] =
  ch <- 0

def selectChannel38(): Int =
  let ch = channel Int 1;
  let _ = spawn f38(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannel38(): Bool = selectChannel38() `assertEq!` 0

def f39(ch: Channel[Int]): Channel[Int] =
  ch <- 1

def selectChannel39(): Int =
  let ch = channel Int 1;
  let _ = spawn f39(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannel39(): Bool = selectChannel39() `assertEq!` 1

def f40(ch: Channel[Int]): Channel[Int] =
  ch <- 10

def selectChannel40(): Int =
  let ch = channel Int 1;
  let _ = spawn f40(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannel40(): Bool = selectChannel40() `assertEq!` 10

def f41(ch: Channel[Int]): Channel[Int] =
  ch <- 2147483647

def selectChannel41(): Int =
  let ch = channel Int 1;
  let _ = spawn f41(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannel41(): Bool = selectChannel41() `assertEq!` 2147483647

def f42(ch: Channel[Int]): Channel[Int] =
  ch <- -1

def selectChannel42(): Int =
  let ch = channel Int 1;
  let _ = spawn f42(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannel42(): Bool = selectChannel42() `assertEq!` -1

def f43(ch: Channel[Int]): Channel[Int] =
  ch <- -10

def selectChannel43(): Int =
  let ch = channel Int 1;
  let _ = spawn f43(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannel43(): Bool = selectChannel43() `assertEq!` -10

def f44(ch: Channel[Int]): Channel[Int] =
  ch <- -214748

def selectChannel44(): Int =
  let ch = channel Int 1;
  let _ = spawn f44(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannel44(): Bool = selectChannel44() `assertEq!` -214748

def f45(ch: Channel[Int]): Channel[Int] =
  ch <- 0

def selectChannel45(): Int =
  let ch = channel Int 10;
  let _ = spawn f45(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannel45(): Bool = selectChannel45() `assertEq!` 0

def f46(ch: Channel[Int]): Channel[Int] =
  ch <- 1

def selectChannel46(): Int =
  let ch = channel Int 10;
  let _ = spawn f46(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannel46(): Bool = selectChannel46() `assertEq!` 1

def f47(ch: Channel[Int]): Channel[Int] =
  ch <- 10

def selectChannel47(): Int =
  let ch = channel Int 10;
  let _ = spawn f47(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannel47(): Bool = selectChannel47() `assertEq!` 10

def f48(ch: Channel[Int]): Channel[Int] =
  ch <- 2147483647

def selectChannel48(): Int =
  let ch = channel Int 10;
  let _ = spawn f48(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannel48(): Bool = selectChannel48() `assertEq!` 2147483647

def f49(ch: Channel[Int]): Channel[Int] =
  ch <- -1

def selectChannel49(): Int =
  let ch = channel Int 10;
  let _ = spawn f49(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannel49(): Bool = selectChannel49() `assertEq!` -1

def f50(ch: Channel[Int]): Channel[Int] =
  ch <- -10

def selectChannel50(): Int =
  let ch = channel Int 10;
  let _ = spawn f50(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannel50(): Bool = selectChannel50() `assertEq!` -10

def f51(ch: Channel[Int]): Channel[Int] =
  ch <- -214748

def selectChannel51(): Int =
  let ch = channel Int 10;
  let _ = spawn f51(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannel51(): Bool = selectChannel51() `assertEq!` -214748

def f52(ch: Channel[Int]): Channel[Int] =
  ch <- 0

def selectChannel52(): Int =
  let ch = channel Int 2147483647;
  let _ = spawn f52(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannel52(): Bool = selectChannel52() `assertEq!` 0

def f53(ch: Channel[Int]): Channel[Int] =
  ch <- 1

def selectChannel53(): Int =
  let ch = channel Int 2147483647;
  let _ = spawn f53(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannel53(): Bool = selectChannel53() `assertEq!` 1

def f54(ch: Channel[Int]): Channel[Int] =
  ch <- 10

def selectChannel54(): Int =
  let ch = channel Int 2147483647;
  let _ = spawn f54(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannel54(): Bool = selectChannel54() `assertEq!` 10

def f55(ch: Channel[Int]): Channel[Int] =
  ch <- 2147483647

def selectChannel55(): Int =
  let ch = channel Int 2147483647;
  let _ = spawn f55(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannel55(): Bool = selectChannel55() `assertEq!` 2147483647

def f56(ch: Channel[Int]): Channel[Int] =
  ch <- -1

def selectChannel56(): Int =
  let ch = channel Int 2147483647;
  let _ = spawn f56(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannel56(): Bool = selectChannel56() `assertEq!` -1

def f57(ch: Channel[Int]): Channel[Int] =
  ch <- -10

def selectChannel57(): Int =
  let ch = channel Int 2147483647;
  let _ = spawn f57(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannel57(): Bool = selectChannel57() `assertEq!` -10

def f58(ch: Channel[Int]): Channel[Int] =
  ch <- -214748

def selectChannel58(): Int =
  let ch = channel Int 2147483647;
  let _ = spawn f58(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannel58(): Bool = selectChannel58() `assertEq!` -214748

def selectChannel59(): Int =
  let ch = channel Int 1;
  let _ = ch <- 0;
  select {
    case x <- ch => x
  }

@test
def testSelectChannel59(): Bool = selectChannel59() `assertEq!` 0

def selectChannel60(): Int =
  let ch = channel Int 1;
  let _ = ch <- 1;
  select {
    case x <- ch => x
  }

@test
def testSelectChannel60(): Bool = selectChannel60() `assertEq!` 1

def selectChannel61(): Int =
  let ch = channel Int 1;
  let _ = ch <- 10;
  select {
    case x <- ch => x
  }

@test
def testSelectChannel61(): Bool = selectChannel61() `assertEq!` 10

def selectChannel62(): Int =
  let ch = channel Int 1;
  let _ = ch <- 2147483647;
  select {
    case x <- ch => x
  }

@test
def testSelectChannel62(): Bool = selectChannel62() `assertEq!` 2147483647

def selectChannel63(): Int =
  let ch = channel Int 1;
  let _ = ch <- -1;
  select {
    case x <- ch => x
  }

@test
def testSelectChannel63(): Bool = selectChannel63() `assertEq!` -1

def selectChannel64(): Int =
  let ch = channel Int 1;
  let _ = ch <- -10;
  select {
    case x <- ch => x
  }

@test
def testSelectChannel64(): Bool = selectChannel64() `assertEq!` -10

def selectChannel65(): Int =
  let ch = channel Int 1;
  let _ = ch <- -214748;
  select {
    case x <- ch => x
  }

@test
def testSelectChannel65(): Bool = selectChannel65() `assertEq!` -214748

def selectChannel66(): Int =
  let ch = channel Int 1;
  let _ = ch <- 0;
  select {
    case x <- ch => 0
  }

@test
def testSelectChannel66(): Bool = selectChannel66() `assertEq!` 0

def selectChannel67(): Int =
  let ch = channel Int 1;
  let _ = ch <- 0;
  select {
    case x <- ch => 1
  }

@test
def testSelectChannel67(): Bool = selectChannel67() `assertEq!` 1

def selectChannel68(): Int =
  let ch = channel Int 1;
  let _ = ch <- 0;
  select {
    case x <- ch => 10
  }

@test
def testSelectChannel68(): Bool = selectChannel68() `assertEq!` 10

def selectChannel69(): Int =
  let ch = channel Int 1;
  let _ = ch <- 0;
  select {
    case x <- ch => 2147483647
  }

@test
def testSelectChannel69(): Bool = selectChannel69() `assertEq!` 2147483647

def selectChannel70(): Int =
  let ch = channel Int 1;
  let _ = ch <- 0;
  select {
    case x <- ch => -1
  }

@test
def testSelectChannel70(): Bool = selectChannel70() `assertEq!` -1

def selectChannel71(): Int =
  let ch = channel Int 1;
  let _ = ch <- 0;
  select {
    case x <- ch => -10
  }

@test
def testSelectChannel71(): Bool = selectChannel71() `assertEq!` -10

def selectChannel72(): Int =
  let ch = channel Int 1;
  let _ = ch <- 0;
  select {
    case x <- ch => -214748
  }

@test
def testSelectChannel72(): Bool = selectChannel72() `assertEq!` -214748

def f73(ch: Channel[Int]): Channel[Int] =
  ch <- 0

def selectChannel73(): Int =
  let ch1 = channel Int 1;
  let ch2 = channel Int 1;
  let _ = spawn f73(ch1);
  select {
    case x <- ch1 => x
  }

@test
def testSelectChannel73(): Bool = selectChannel73() `assertEq!` 0

def f74(ch: Channel[Int]): Channel[Int] =
  ch <- 1

def selectChannel74(): Int =
  let ch1 = channel Int 1;
  let ch2 = channel Int 1;
  let _ = spawn f74(ch1);
  select {
    case x <- ch1 => x
  }

@test
def testSelectChannel74(): Bool = selectChannel74() `assertEq!` 1

def f75(ch: Channel[Int]): Channel[Int] =
  ch <- 10

def selectChannel75(): Int =
  let ch1 = channel Int 1;
  let ch2 = channel Int 1;
  let _ = spawn f75(ch1);
  select {
    case x <- ch1 => x
  }

@test
def testSelectChannel75(): Bool = selectChannel75() `assertEq!` 10

def f76(ch: Channel[Int]): Channel[Int] =
  ch <- 2147483647

def selectChannel76(): Int =
  let ch1 = channel Int 1;
  let ch2 = channel Int 1;
  let _ = spawn f76(ch1);
  select {
    case x <- ch1 => x
  }

@test
def testSelectChannel76(): Bool = selectChannel76() `assertEq!` 2147483647

def f77(ch: Channel[Int]): Channel[Int] =
  ch <- -1

def selectChannel77(): Int =
  let ch1 = channel Int 1;
  let ch2 = channel Int 1;
  let _ = spawn f77(ch1);
  select {
    case x <- ch1 => x
  }

@test
def testSelectChannel77(): Bool = selectChannel77() `assertEq!` -1

def f78(ch: Channel[Int]): Channel[Int] =
  ch <- -10

def selectChannel78(): Int =
  let ch1 = channel Int 1;
  let ch2 = channel Int 1;
  let _ = spawn f78(ch1);
  select {
    case x <- ch1 => x
  }

@test
def testSelectChannel78(): Bool = selectChannel78() `assertEq!` -10

def f79(ch: Channel[Int]): Channel[Int] =
  ch <- -214748

def selectChannel79(): Int =
  let ch1 = channel Int 1;
  let ch2 = channel Int 1;
  let _ = spawn f79(ch1);
  select {
    case x <- ch1 => x
  }

@test
def testSelectChannel79(): Bool = selectChannel79() `assertEq!` -214748

def f80(ch: Channel[Int]): Channel[Int] =
  ch <- 0

def selectChannel80(): Int =
  let ch1 = channel Int 1;
  let ch2 = channel Int 1;
  let _ = spawn f80(ch2);
  select {
    case x <- ch1 => x
    case x <- ch2 => x
  }

@test
def testSelectChannel80(): Bool = selectChannel80() `assertEq!` 0

def f81(ch: Channel[Int]): Channel[Int] =
  ch <- 1

def selectChannel81(): Int =
  let ch1 = channel Int 1;
  let ch2 = channel Int 1;
  let _ = spawn f81(ch2);
  select {
    case x <- ch1 => x
    case x <- ch2 => x
  }

@test
def testSelectChannel81(): Bool = selectChannel81() `assertEq!` 1

def f82(ch: Channel[Int]): Channel[Int] =
  ch <- 10

def selectChannel82(): Int =
  let ch1 = channel Int 1;
  let ch2 = channel Int 1;
  let _ = spawn f82(ch2);
  select {
    case x <- ch1 => x
    case x <- ch2 => x
  }

@test
def testSelectChannel82(): Bool = selectChannel82() `assertEq!` 10

def f83(ch: Channel[Int]): Channel[Int] =
  ch <- 2147483647

def selectChannel83(): Int =
  let ch1 = channel Int 1;
  let ch2 = channel Int 1;
  let _ = spawn f83(ch2);
  select {
    case x <- ch1 => x
    case x <- ch2 => x
  }

@test
def testSelectChannel83(): Bool = selectChannel83() `assertEq!` 2147483647

def f84(ch: Channel[Int]): Channel[Int] =
  ch <- -1

def selectChannel84(): Int =
  let ch1 = channel Int 1;
  let ch2 = channel Int 1;
  let _ = spawn f84(ch2);
  select {
    case x <- ch1 => x
    case x <- ch2 => x
  }

@test
def testSelectChannel84(): Bool = selectChannel84() `assertEq!` -1

def f85(ch: Channel[Int]): Channel[Int] =
  ch <- -10

def selectChannel85(): Int =
  let ch1 = channel Int 1;
  let ch2 = channel Int 1;
  let _ = spawn f85(ch2);
  select {
    case x <- ch1 => x
    case x <- ch2 => x
  }

@test
def testSelectChannel85(): Bool = selectChannel85() `assertEq!` -10

def f86(ch: Channel[Int]): Channel[Int] =
  ch <- -214748

def selectChannel86(): Int =
  let ch1 = channel Int 1;
  let ch2 = channel Int 1;
  let _ = spawn f86(ch2);
  select {
    case x <- ch1 => x
    case x <- ch2 => x
  }

@test
def testSelectChannel86(): Bool = selectChannel86() `assertEq!` -214748

def f87(ch: Channel[Option[Int]]): Channel[Option[Int]] =
  ch <- None

def selectChannel87(): Option[Int] =
  let ch = channel Option[Int] 1;
  let _ = spawn f87(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannel87(): Bool = selectChannel87() `assertEq!` None

def f88(ch: Channel[Option[Int]]): Channel[Option[Int]] =
  ch <- Some(42)

def selectChannel88(): Option[Int] =
  let ch = channel Option[Int] 1;
  let _ = spawn f88(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannel88(): Bool = selectChannel88() `assertEq!` Some(42)

def f89(ch: Channel[Option[Int]]): Channel[Option[Int]] =
  ch <- None

def selectChannel89(): Int =
  let ch = channel Option[Int] 1;
  let _ = spawn f89(ch);
  select {
    case x <- ch => match x with {
      case Some(e) => e
      case None => -1
    }
  }

@test
def testSelectChannel89(): Bool = selectChannel89() `assertEq!` -1

def f90(ch: Channel[Option[Int]]): Channel[Option[Int]] =
  ch <- Some(42)

def selectChannel90(): Int =
  let ch = channel Option[Int] 1;
  let _ = spawn f90(ch);
  select {
    case x <- ch => match x with {
      case Some(e) => e
      case None => -1
    }
  }

@test
def testSelectChannel90(): Bool = selectChannel90() `assertEq!` 42















def f10001(ch: Channel[Bool]): Channel[Bool] =
  ch <- true

def selectChannel10001(): Int =
  let ch = channel Bool 1;
  let _ = spawn f10001(ch);
  select {
    case x <- ch => if (x) 3 else 7
  }

@test
def testSelectChannel10001(): Bool = selectChannel10001() `assertEq!` 3

def f10002(ch: Channel[Bool]): Channel[Bool] =
  ch <- false

def selectChannel10002(): Int =
  let ch = channel Bool 1;
  let _ = spawn f10002(ch);
  select {
    case x <- ch => if (x) 3 else 7
  }

@test
def testSelectChannel10002(): Bool = selectChannel10002() `assertEq!` 7

def f10003(ch: Channel[Bool]): Channel[Bool] =
  ch <- true

def selectChannel10003(): Str =
  let ch = channel Bool 1;
  let _ = spawn f10003(ch);
  select {
    case x <- ch => if (x) "a" else "b"
  }

@test
def testSelectChannel10003(): Bool = selectChannel10003() `assertEq!` "a"

def f10004(ch: Channel[Bool]): Channel[Bool] =
  ch <- false

def selectChannel10004(): Str =
  let ch = channel Bool 1;
  let _ = spawn f10004(ch);
  select {
    case x <- ch => if (x) "a" else "b"
  }

@test
def testSelectChannel10004(): Bool = selectChannel10004() `assertEq!` "b"

def f10005(ch: Channel[Bool]): Channel[Bool] =
  ch <- true

def selectChannel10005(): Char =
  let ch = channel Bool 1;
  let _ = spawn f10005(ch);
  select {
    case x <- ch => if (x) 'a' else 'b'
  }

@test
def testSelectChannel10005(): Bool = selectChannel10005() `assertEq!` 'a'

def f10006(ch: Channel[Bool]): Channel[Bool] =
  ch <- false

def selectChannel10006(): Char =
  let ch = channel Bool 1;
  let _ = spawn f10006(ch);
  select {
    case x <- ch => if (x) 'a' else 'b'
  }

@test
def testSelectChannel10006(): Bool = selectChannel10006() `assertEq!` 'b'

def f10007(ch: Channel[Int]): Channel[Int] =
  ch <- 1

def selectChannel10007(): Int =
  let ch1 = channel Int 1;
  let ch2 = channel Str 1;
  let _ = spawn f10007(ch1);
  select {
    case x <- ch1 => x
    case x <- ch2 => -1
  }

@test
def testSelectChannel10007(): Bool = selectChannel10007() `assertEq!` 1

def f10008(ch: Channel[Str]): Channel[Str] =
  ch <- "a"

def selectChannelf10008(): Int =
  let ch1 = channel Int 1;
  let ch2 = channel Str 1;
  let _ = spawn f10008(ch2);
  select {
    case x <- ch1 => x
    case x <- ch2 => -1
  }

@test
def testSelectChannelf10008(): Bool = selectChannelf10008() `assertEq!` -1

def f10009(ch: Channel[Int]): Channel[Int] =
  ch <- 1

def selectChannel10009(): Str =
  let ch1 = channel Int 1;
  let ch2 = channel Str 1;
  let _ = spawn f10009(ch1);
  select {
    case x <- ch1 => "z"
    case x <- ch2 => x
  }

@test
def testSelectChannel10009(): Bool = selectChannel10009() `assertEq!` "z"

def f10010(ch: Channel[Str]): Channel[Str] =
  ch <- "a"

def selectChannelf10010(): Str =
  let ch1 = channel Int 1;
  let ch2 = channel Str 1;
  let _ = spawn f10010(ch2);
  select {
    case x <- ch1 => "z"
    case x <- ch2 => x
  }

@test
def testSelectChannelf10010(): Bool = selectChannelf10010() `assertEq!` "a"







/*
// Test units
def testSelectChannelUnit(): Unit =
  let ch: Channel[Unit] = channel Unit 1;
  let _ = ch <- ();
  select {
      case x <- ch => x
  }

@test
def testSelectChannel01(): Bool = testSelectChannelUnit() `assertEq!` ()

// Test bools
def testSelectChannelBool(var: Bool): Bool =
  let ch: Channel[Bool] = channel Bool 1;
  let _ = ch <- var;
  select {
      case x <- ch => x
  }

@test
def testSelectChannel02(): Bool = testSelectChannelBool(true) `assertEq!` true

@test
def testSelectChannel03(): Bool = testSelectChannelBool(false) `assertEq!` false

// Test integers
def testSelectChannelInt(var: Int): Int =
  let ch: Channel[Int] = channel Int 1;
  let _ = ch <- var;
  select {
      case x <- ch => x
  }

@test
def testSelectChannel04(): Bool = testSelectChannelInt(0) `assertEq!` 0

@test
def testSelectChannel6(): Bool = testSelectChannelInt(-1) `assertEq!` -1

@test
def testSelectChannel06(): Bool = testSelectChannelInt(1) `assertEq!` 1

@test
def testSelectChannel07(): Bool = testSelectChannelInt(-100) `assertEq!` -100

@test
def testSelectChannel08(): Bool = testSelectChannelInt(100) `assertEq!` 100

// Test floats
def testSelectChannelFloat(var: Float64): Float64 =
  let ch: Channel[Float64] = channel Float64 1;
  let _ = ch <- var;
  select {
     case x <- ch => x
  }

// @test
// def testSelectChannel09(): Bool = testSelectChannelFloat32(0f32) `assertEq!` 0f32
//
// @test
// def testSelectChannel10(): Bool = testSelectChannelFloat32(-1.1f32) `assertEq!` -1.1f32
//
// @test
// def testSelectChannel11(): Bool = testSelectChannelFloat32(1.1f32) `assertEq!` 1.1f32

// Test strings
def testSelectChannelStr(var: Str): Str =
  let ch: Channel[Str] = channel Str 1;
  let _ = ch <- var;
  select {
      case x <- ch => x
  }

@test
def testSelectChannel12(): Bool = testSelectChannelStr("") `assertEq!` ""

@test
def testSelectChannel13(): Bool = testSelectChannelStr("str") `assertEq!` "str"

// Test chars
def testSelectChannelChar(var: Char): Char =
  let ch: Channel[Char] = channel Char 1;
  let _ = ch <- var;
  select {
      case x <- ch => x
  }

@test
def testSelectChannel14(): Bool = testSelectChannelChar('a') `assertEq!` 'a'

@test
def testSelectChannel15(): Bool = testSelectChannelChar('b') `assertEq!` 'b'

// Test tuples
def testSelectChannelIntStrTuple(var: (Int, Str)): (Int, Str) =
  let ch: Channel[(Int, Str)] = channel (Int, Str);
  let _ = ch <- var;
  select {
      case x <- ch => x
  }

def testSelectChannelIntIntTuple(var: (Int, Int)): (Int, Int) =
  let ch: Channel[(Int, Int)] = channel (Int, Int);
  let _ = ch <- var;
  select {
      case x <- ch => x
  }

def testSelectChannelIntIntIntIntTuple(var: (Int, Int, Int, Int)): (Int, Int, Int, Int) =
  let ch: Channel[(Int, Int, Int, Int)] = channel (Int, Int, Int, Int);
  let _ = ch <- var;
  select {
      case x <- ch => x
  }

@test
def testSelectChannel16(): Bool = testSelectChannelIntStrTuple((2, "Str")) `assertEq!` (2, "Str")

@test
def testSelectChannel17(): Bool = testSelectChannelIntIntTuple((2, 10)) `assertEq!` (2, 10)

@test
def testSelectChannel18(): Bool = testSelectChannelIntIntIntIntTuple((2, 10, 12, 0)) `assertEq!` (2, 10, 12, 0)

// Test lists
def testSelectChannelIntList(var: List[Int]): List[Int] =
  let ch: Channel[List[Int]] = channel List[Int] 1;
  let _ = ch <- var;
  select {
      case x <- ch => x
  }

def testSelectChannelStrList(var: List[Str]): List[Str] =
  let ch: Channel[List[Str]] = channel List[Str] 1;
  let _ = ch <- var;
  select {
      case x <- ch => x
  }

@test
def testSelectChannel19(): Bool = testSelectChannelIntList(Nil) `assertEq!` Nil

@test
def testSelectChannel20(): Bool = testSelectChannelIntList(1 :: Nil) `assertEq!` 1 :: Nil

@test
def testSelectChannel21(): Bool = testSelectChannelStrList("str" :: Nil) `assertEq!` "str" :: Nil

@test
def testSelectChannel22(): Bool = testSelectChannelStrList("str" :: "trs" :: Nil) `assertEq!` "str" :: "trs" :: Nil

// Test functions
def testSelectChannelIntIntFunction(var: Int -> Int): Int -> Int =
  let ch: Channel[Int -> Int] = channel Int -> Int 1;
  let _ = ch <- var;
  select {
      case x <- ch => x
  }

def testSelectChannelIntStrFunction(var: Int -> Str): Int -> Str =
  let ch: Channel[Int -> Str] = channel Int -> Str 1;
  let _ = ch <- var;
  select {
      case x <- ch => x
  }

def testSelectChannelIntIntIntFunction(var: Int -> Int -> Int): Int -> Int -> Int =
  let ch: Channel[Int -> Int -> Int] = channel Int -> Int -> Int 1;
  let _ = ch <- var;
  select {
      case x <- ch => x
  }

def testSelectChannelTupleIntIntIntFunction(var: (Int, Int) -> Int): (Int, Int) -> Int =
  let ch: Channel[(Int, Int) -> Int] = channel (Int, Int) -> Int 1;
  let _ = ch <- var;
  select {
      case x <- ch => x
  }

@test
def testSelectChannel23(): Bool = testSelectChannelIntIntFunction(x -> x)(2) `assertEq!` 2

@test
def testSelectChannel24(): Bool = testSelectChannelIntStrFunction(x -> "2")(2) `assertEq!` "2"

//@test
//def testSelectChannel25(): Bool = testSelectChannelIntIntIntFunction((x -> y) -> x + y)(2, 3) `assertEq!` 5

//@test
//def testSelectChannel26(): Bool = testSelectChannelTupleIntIntIntFunction((x -> y) -> x + y)((2, 3)) `assertEq!` 5

// Default
//def testSelectChannelDefault(): Int =
//  let ch: Channel[Unit] = channel Unit;
//  ch <- ();;
//  select {
//    case _ <- ch => 2
//  }

//@test
//def testSelectChannel29(): Bool = testSelectChannelDefault() `assertEq!` 2

// Multiple channel types

def testSelectChannelMultipleTypes(): Str =
  let ch1: Channel[Char] = channel Char;
  let ch2: Channel[Str] = channel Str;
  let _ = spawn putStr(ch2, "a");
  select {
    case x <- ch1 => Char.toString(x)
    case x <- ch2 => x
  }

@test
def testSelectChannel30(): Bool = testSelectChannelMultipleTypes() `assertEq!` "a"

@test
def select_int_channel_first(): Bool =
  let ch1 = channel Int;
  let ch2 = channel Int;
  let _ = spawn putInt(ch1, 2);
  let r = select {
    case x <- ch1 => x
    case x <- ch2 => x
  };
  r `assertEq!` 2

@test
def select_int_channel_last(): Bool =
  let ch1 = channel Int;
  let ch2 = channel Int;
  let _ = spawn putInt(ch1, 2);
  let r = select {
    case x <- ch1 => x + 1
    case x <- ch2 => x + 2
  };
  r `assertEq!` 3


@test
def select_string_channel_first(): Bool =
  let ch1 = channel Str;
  let ch2 = channel Str;
  let _ = spawn put;
  let r = select {
    case x <- ch1 => x + "1"
    case x <- ch2 => x + "2"
  };
  let _ = ch1 <- "str";
  r `assertEq!` "str1"


@test
def select_string_channel_last(): Bool =
  let ch1 = channel Str;
  let ch2 = channel Str;
  let r = select {
    case x <- ch1 => x + "1"
    case x <- ch2 => x + "2"
  };
  let _ = ch2 <- "str";
  r `assertEq!` "str2"
*/

@test
def testSelectChannel1(): Int =
  let c1 = chan Int 100;
  c1 <- 2;
  select {
    case x <- c1 => x
  }

@test
def testSelectChannel2(): Bool = {
  let c1 = chan Unit 1;
  let c2 = chan Unit 1;
  let c3 = chan Bool 2;

  spawn {
    select {
      case _ <- c1 => ()
      case _ <- c2 => ()
    }
  };

  // Sleep before putting element in c1, so select will give a Condition to c1 and c2
  sleep(100i64 * Duration.oneMillisecond());
  c1 <- ();

  // Spawn c2 <- () as this will potentially deadlock if selectLock is not released
  spawn {
    c2 <- ();
    c3 <- true
  };

  // Sleep to give time to the other thread to put into c3 if it does not deadlock
  sleep(100i64 * Duration.oneMillisecond());
  c3 <- false;

  <- c3 `assertEq!` true
}

@test
def testSelectChannel3(): Bool = {
  let selectChannel = chan Unit 1;
  let resultChannel = chan Unit 1;
  spawn resultChannel <- doSelect(selectChannel);
  spawn resultChannel <- doSelect(selectChannel);
  // Sleep to ensure both selects are waiting for input
  sleep(100i64 * Duration.oneMillisecond());
  // Put an element into the selectChannel, and get result from the chosen select
  selectChannel <- ();
  <- resultChannel;
  // Now put a second element into the selectChannel
  // If the test works, this will allow the non-selected select to put an element into resultChannel
  selectChannel <- ();
  // Sleep to make sure the non-selected select has run
  sleep(100i64 * Duration.oneMillisecond());
  let result = select {
    case _ <- resultChannel => true
    case _ => false
  };
  result `assertEq!` true
}

def doSelect(c: Channel[Unit]): Unit = {
  select {
    case _ <- c => ()
  }
}

// TESTS FROM THE PREVIOUS GROUP

/////////////////////////////////////////////////////////////////////////////
// Select expressions (channel select)                                     //
/////////////////////////////////////////////////////////////////////////////


// Units
def fUnit01(ch: Channel[Unit]): Channel[Unit] =
  ch <- ()

def selectChannelUnit01(): Unit =
  let ch = chan Unit 100;
  let _ = spawn fUnit01(ch);
  select {
    case y <- ch => y
  }

@test
def testSelectChannelUnit01(): Bool = selectChannelUnit01() `assertEq!` ()

def fUnit02(ch: Channel[Unit]): Channel[Unit] =
  ch <- ()

def selectChannelUnit02(): Unit =
  let ch = chan Unit 100;
  let _ = spawn fUnit02(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannelUnit02(): Bool = selectChannelUnit02() `assertEq!` ()

def fUnit03(ch: Channel[Unit]): Channel[Unit] =
  ch <- ()

def selectChannelUnit03(): Unit =
  let ch = chan Unit 100;
  let _ = spawn fUnit03(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannelUnit03(): Bool = selectChannelUnit03() `assertEq!` ()

def selectChannelUnit04(): Unit =
  let ch = chan Unit 100;
  let _ = ch <- ();
  select {
    case x <- ch => x
  }

@test
def testSelectChannelUnit04(): Bool = selectChannelUnit04() `assertEq!` ()

def selectChannelUnit05(): Unit =
  let ch = chan Unit 100;
  let _ = ch <- ();
  select {
    case _ <- ch => ()
  }

@test
def testSelectChannelUnit05(): Bool = selectChannelUnit05() `assertEq!` ()

def fUnit06(ch: Channel[Unit]): Channel[Unit] =
  ch <- ()

def selectChannelUnit06(): Unit =
  let ch1 = chan Unit 100;
  let _ = spawn fUnit06(ch1);
  select {
    case x <- ch1 => x
  }

@test
def testSelectChannelUnit06(): Bool = selectChannelUnit06() `assertEq!` ()

def fUnit07(ch: Channel[Unit]): Channel[Unit] =
  ch <- ()

def selectChannelUnit07(): Unit =
  let ch2 = chan Unit 100;
  let _ = spawn fUnit07(ch2);
  select {
    case x <- ch2 => x
  }

@test
def testSelectChannelUnit07(): Bool = selectChannelUnit07() `assertEq!` ()


// Bool
def fBool01(ch: Channel[Bool]): Channel[Bool] =
  ch <- true

def selectChannelBool01(): Bool =
  let ch = chan Bool 100;
  let _ = spawn fBool01(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannelBool01(): Bool = selectChannelBool01() `assertEq!` true

def fBool02(ch: Channel[Bool]): Channel[Bool] =
  ch <- false

def selectChannelBool02(): Bool =
  let ch = chan Bool 100;
  let _ = spawn fBool02(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannelBool02(): Bool = selectChannelBool02() `assertEq!` false

def fBool03(ch: Channel[Bool]): Channel[Bool] =
  ch <- true

def selectChannelBool03(): Bool =
  let ch = chan Bool 100;
  let _ = spawn fBool03(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannelBool03(): Bool = selectChannelBool03() `assertEq!` true

def fBool04(ch: Channel[Bool]): Channel[Bool] =
  ch <- false

def selectChannelBool04(): Bool =
  let ch = chan Bool 100;
  let _ = spawn fBool04(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannelBool04(): Bool = selectChannelBool04() `assertEq!` false

def fBool05(ch: Channel[Bool]): Channel[Bool] =
  ch <- true

def selectChannelBool05(): Bool =
  let ch = chan Bool 100;
  let _ = spawn fBool05(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannelBool05(): Bool = selectChannelBool05() `assertEq!` true

def fBool06(ch: Channel[Bool]): Channel[Bool] =
  ch <- false

def selectChannelBool06(): Bool =
  let ch = chan Bool 100;
  let _ = spawn fBool06(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannelBool06(): Bool = selectChannelBool06() `assertEq!` false

def selectChannelBool07(): Bool =
  let ch = chan Bool 100;
  let _ = ch <- true;
  select {
    case x <- ch => x
  }

@test
def testSelectChannelBool07(): Bool = selectChannelBool07() `assertEq!` true

def selectChannelBool08(): Bool =
  let ch = chan Bool 100;
  let _ = ch <- false;
  select {
    case x <- ch => x
  }

@test
def testSelectChannelBool08(): Bool = selectChannelBool08() `assertEq!` false

def selectChannelBool09(): Bool =
  let ch = chan Bool 100;
  let _ = ch <- true;
  select {
    case _ <- ch => true
  }

@test
def testSelectChannelBool09(): Bool = selectChannelBool09() `assertEq!` true

def selectChannelBool10(): Bool =
  let ch = chan Bool 100;
  let _ = ch <- false;
  select {
    case _ <- ch => false
  }

@test
def testSelectChannelBool10(): Bool = selectChannelBool10() `assertEq!` false

def selectChannelBool11(): Bool =
  let ch = chan Bool 100;
  let _ = ch <- true;
  select {
    case _ <- ch => true && true
  }

@test
def testSelectChannelBool11(): Bool = selectChannelBool11() `assertEq!` true

def selectChannelBool12(): Bool =
  let ch = chan Bool 100;
  let _ = ch <- true;
  select {
    case _ <- ch => true && false
  }

@test
def testSelectChannelBool12(): Bool = selectChannelBool12() `assertEq!` false

def selectChannelBool13(): Bool =
  let ch = chan Bool 100;
  let _ = ch <- true;
  select {
    case _ <- ch => false && false
  }

@test
def testSelectChannelBool13(): Bool = selectChannelBool13() `assertEq!` false

def selectChannelBool14(): Bool =
  let ch = chan Bool 100;
  let _ = ch <- true;
  select {
    case _ <- ch => false && true
  }

@test
def testSelectChannelBool14(): Bool = selectChannelBool14() `assertEq!` false

def selectChannelBool15(): Bool =
  let ch = chan Bool 100;
  let _ = ch <- true;
  select {
    case _ <- ch => true || true
  }

@test
def testSelectChannelBool15(): Bool = selectChannelBool15() `assertEq!` true

def selectChannelBool16(): Bool =
  let ch = chan Bool 100;
  let _ = ch <- true;
  select {
    case _ <- ch => true || false
  }

@test
def testSelectChannelBool16(): Bool = selectChannelBool16() `assertEq!` true

def selectChannelBool17(): Bool =
  let ch = chan Bool 100;
  let _ = ch <- true;
  select {
    case _ <- ch => false || false
  }

@test
def testSelectChannelBool17(): Bool = selectChannelBool17() `assertEq!` false

def selectChannelBool18(): Bool =
  let ch = chan Bool 100;
  let _ = ch <- true;
  select {
    case _ <- ch => false || true
  }

@test
def testSelectChannelBool18(): Bool = selectChannelBool18() `assertEq!` true

def selectChannelBool19(): Bool =
  let ch = chan Bool 100;
  let _ = ch <- true;
  select {
    case x <- ch => x && true
  }

@test
def testSelectChannelBool19(): Bool = selectChannelBool19() `assertEq!` true

def selectChannelBool20(): Bool =
  let ch = chan Bool 100;
  let _ = ch <- true;
  select {
    case x <- ch => x && false
  }

@test
def testSelectChannelBool20(): Bool = selectChannelBool20() `assertEq!` false

def selectChannelBool21(): Bool =
  let ch = chan Bool 100;
  let _ = ch <- false;
  select {
    case x <- ch => x && false
  }

@test
def testSelectChannelBool21(): Bool = selectChannelBool21() `assertEq!` false

def selectChannelBool22(): Bool =
  let ch = chan Bool 100;
  let _ = ch <- false;
  select {
    case x <- ch => x && true
  }

@test
def testSelectChannelBool22(): Bool = selectChannelBool22() `assertEq!` false

def selectChannelBool23(): Bool =
  let ch = chan Bool 100;
  let _ = ch <- true;
  select {
    case x <- ch => x || true
  }

@test
def testSelectChannelBool23(): Bool = selectChannelBool23() `assertEq!` true

def selectChannelBool24(): Bool =
  let ch = chan Bool 100;
  let _ = ch <- true;
  select {
    case x <- ch => x || false
  }

@test
def testSelectChannelBool24(): Bool = selectChannelBool24() `assertEq!` true

def selectChannelBool25(): Bool =
  let ch = chan Bool 100;
  let _ = ch <- false;
  select {
    case x <- ch => x || false
  }

@test
def testSelectChannelBool25(): Bool = selectChannelBool25() `assertEq!` false

def selectChannelBool26(): Bool =
  let ch = chan Bool 100;
  let _ = ch <- false;
  select {
    case x <- ch => x || true
  }

@test
def testSelectChannelBool26(): Bool = selectChannelBool26() `assertEq!` true

def fBool27(ch: Channel[Bool]): Channel[Bool] =
  ch <- true

def selectChannelBool27(): Bool =
  let ch1 = chan Bool 100;
  let _ = spawn fBool27(ch1);
  select {
    case x <- ch1 => x
  }

@test
def testSelectChannelBool27(): Bool = selectChannelBool27() `assertEq!` true

def fBool28(ch: Channel[Bool]): Channel[Bool] =
  ch <- false

def selectChannelBool28(): Bool =
  let ch1 = chan Bool 100;
  let _ = spawn fBool28(ch1);
  select {
    case x <- ch1 => x
  }

@test
def testSelectChannelBool28(): Bool = selectChannelBool28() `assertEq!` false

def fBool29(ch: Channel[Bool]): Channel[Bool] =
  ch <- true

def selectChannelBool29(): Bool =
  let ch2 = chan Bool 100;
  let _ = spawn fBool29(ch2);
  select {
    case x <- ch2 => x
  }

@test
def testSelectChannelBool29(): Bool = selectChannelBool29() `assertEq!` true

def fBool30(ch: Channel[Bool]): Channel[Bool] =
  ch <- false

def selectChannelBool30(): Bool =
  let ch2 = chan Bool 100;
  let _ = spawn fBool30(ch2);
  select {
    case x <- ch2 => x
  }

@test
def testSelectChannelBool30(): Bool = selectChannelBool30() `assertEq!` false

// Int
def fInt01(ch: Channel[Int]): Channel[Int] =
  ch <- 0

def selectChannelInt01(): Int =
  let ch = chan Int 100;
  let _ = spawn fInt01(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannelInt01(): Bool = selectChannelInt01() `assertEq!` 0

def fInt02(ch: Channel[Int]): Channel[Int] =
  ch <- 1

def selectChannelInt02(): Int =
  let ch = chan Int 100;
  let _ = spawn fInt02(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannelInt02(): Bool = selectChannelInt02() `assertEq!` 1

def fInt03(ch: Channel[Int]): Channel[Int] =
  ch <- 10

def selectChannelInt03(): Int =
  let ch = chan Int 100;
  let _ = spawn fInt03(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannelInt03(): Bool = selectChannelInt03() `assertEq!` 10

def fInt04(ch: Channel[Int]): Channel[Int] =
  ch <- 2147483647

def selectChannelInt04(): Int =
  let ch = chan Int 100;
  let _ = spawn fInt04(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannelInt04(): Bool = selectChannelInt04() `assertEq!` 2147483647

def fInt05(ch: Channel[Int]): Channel[Int] =
  ch <- -1

def selectChannelInt05(): Int =
  let ch = chan Int 100;
  let _ = spawn fInt05(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannelInt05(): Bool = selectChannelInt05() `assertEq!` -1

def fInt06(ch: Channel[Int]): Channel[Int] =
  ch <- -10

def selectChannelInt06(): Int =
  let ch = chan Int 100;
  let _ = spawn fInt06(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannelInt06(): Bool = selectChannelInt06() `assertEq!` -10

def fInt07(ch: Channel[Int]): Channel[Int] =
  ch <- -214748

def selectChannelInt07(): Int =
  let ch = chan Int 100;
  let _ = spawn fInt07(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannelInt07(): Bool = selectChannelInt07() `assertEq!` -214748

def fInt08(ch: Channel[Int]): Channel[Int] =
  ch <- 0

def selectChannelInt08(): Int =
  let ch = chan Int 100;
  let _ = spawn fInt08(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannelInt08(): Bool = selectChannelInt08() `assertEq!` 0

def fInt09(ch: Channel[Int]): Channel[Int] =
  ch <- 1

def selectChannelInt09(): Int =
  let ch = chan Int 100;
  let _ = spawn fInt09(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannelInt09(): Bool = selectChannelInt09() `assertEq!` 1

def fInt10(ch: Channel[Int]): Channel[Int] =
  ch <- 10

def selectChannelInt10(): Int =
  let ch = chan Int 100;
  let _ = spawn fInt10(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannelInt10(): Bool = selectChannelInt10() `assertEq!` 10

def fInt11(ch: Channel[Int]): Channel[Int] =
  ch <- 2147483647

def selectChannelInt11(): Int =
  let ch = chan Int 100;
  let _ = spawn fInt11(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannelInt11(): Bool = selectChannelInt11() `assertEq!` 2147483647

def fInt12(ch: Channel[Int]): Channel[Int] =
  ch <- -1

def selectChannelInt12(): Int =
  let ch = chan Int 100;
  let _ = spawn fInt12(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannelInt12(): Bool = selectChannelInt12() `assertEq!` -1

def fInt13(ch: Channel[Int]): Channel[Int] =
  ch <- -10

def selectChannelInt13(): Int =
  let ch = chan Int 100;
  let _ = spawn fInt13(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannelInt13(): Bool = selectChannelInt13() `assertEq!` -10

def fInt14(ch: Channel[Int]): Channel[Int] =
  ch <- -214748

def selectChannelInt14(): Int =
  let ch = chan Int 100;
  let _ = spawn fInt14(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannelInt14(): Bool = selectChannelInt14() `assertEq!` -214748

def fInt15(ch: Channel[Int]): Channel[Int] =
  ch <- 0

def selectChannelInt15(): Int =
  let ch = chan Int 100;
  let _ = spawn fInt15(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannelInt15(): Bool = selectChannelInt15() `assertEq!` 0

def fInt16(ch: Channel[Int]): Channel[Int] =
  ch <- 1

def selectChannelInt16(): Int =
  let ch = chan Int 100;
  let _ = spawn fInt16(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannelInt16(): Bool = selectChannelInt16() `assertEq!` 1

def fInt17(ch: Channel[Int]): Channel[Int] =
  ch <- 10

def selectChannelInt17(): Int =
  let ch = chan Int 100;
  let _ = spawn fInt17(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannelInt17(): Bool = selectChannelInt17() `assertEq!` 10

def fInt18(ch: Channel[Int]): Channel[Int] =
  ch <- 2147483647

def selectChannelInt18(): Int =
  let ch = chan Int 100;
  let _ = spawn fInt18(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannelInt18(): Bool = selectChannelInt18() `assertEq!` 2147483647

def fInt19(ch: Channel[Int]): Channel[Int] =
  ch <- -1

def selectChannelInt19(): Int =
  let ch = chan Int 100;
  let _ = spawn fInt19(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannelInt19(): Bool = selectChannelInt19() `assertEq!` -1

def fInt20(ch: Channel[Int]): Channel[Int] =
  ch <- -10

def selectChannelInt20(): Int =
  let ch = chan Int 100;
  let _ = spawn fInt20(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannelInt20(): Bool = selectChannelInt20() `assertEq!` -10

def fInt21(ch: Channel[Int]): Channel[Int] =
  ch <- -214748

def selectChannelInt21(): Int =
  let ch = chan Int 100;
  let _ = spawn fInt21(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannelInt21(): Bool = selectChannelInt21() `assertEq!` -214748

def selectChannelInt22(): Int =
  let ch = chan Int 100;
  let _ = ch <- 0;
  select {
    case x <- ch => x
  }

@test
def testSelectChannelInt22(): Bool = selectChannelInt22() `assertEq!` 0

def selectChannelInt23(): Int =
  let ch = chan Int 100;
  let _ = ch <- 1;
  select {
    case x <- ch => x
  }

@test
def testSelectChannelInt23(): Bool = selectChannelInt23() `assertEq!` 1

def selectChannelInt24(): Int =
  let ch = chan Int 100;
  let _ = ch <- 10;
  select {
    case x <- ch => x
  }

@test
def testSelectChannelInt24(): Bool = selectChannelInt24() `assertEq!` 10

def selectChannelInt25(): Int =
  let ch = chan Int 100;
  let _ = ch <- 2147483647;
  select {
    case x <- ch => x
  }

@test
def testSelectChannelInt25(): Bool = selectChannelInt25() `assertEq!` 2147483647

def selectChannelInt26(): Int =
  let ch = chan Int 100;
  let _ = ch <- -1;
  select {
    case x <- ch => x
  }

@test
def testSelectChannelInt26(): Bool = selectChannelInt26() `assertEq!` -1

def selectChannelInt27(): Int =
  let ch = chan Int 100;
  let _ = ch <- -10;
  select {
    case x <- ch => x
  }

@test
def testSelectChannelInt27(): Bool = selectChannelInt27() `assertEq!` -10

def selectChannelInt28(): Int =
  let ch = chan Int 100;
  let _ = ch <- -214748;
  select {
    case x <- ch => x
  }

@test
def testSelectChannelInt28(): Bool = selectChannelInt28() `assertEq!` -214748

def selectChannelInt29(): Int =
  let ch = chan Int 100;
  let _ = ch <- 0;
  select {
    case _ <- ch => 0
  }

@test
def testSelectChannelInt29(): Bool = selectChannelInt29() `assertEq!` 0

def selectChannelInt30(): Int =
  let ch = chan Int 100;
  let _ = ch <- 0;
  select {
    case _ <- ch => 1
  }

@test
def testSelectChannelInt30(): Bool = selectChannelInt30() `assertEq!` 1

def selectChannelInt31(): Int =
  let ch = chan Int 100;
  let _ = ch <- 0;
  select {
    case _ <- ch => 10
  }

@test
def testSelectChannelInt31(): Bool = selectChannelInt31() `assertEq!` 10

def selectChannelInt32(): Int =
  let ch = chan Int 100;
  let _ = ch <- 0;
  select {
    case _ <- ch => 2147483647
  }

@test
def testSelectChannelInt32(): Bool = selectChannelInt32() `assertEq!` 2147483647

def selectChannelInt33(): Int =
  let ch = chan Int 100;
  let _ = ch <- 0;
  select {
    case _ <- ch => -1
  }

@test
def testSelectChannelInt33(): Bool = selectChannelInt33() `assertEq!` -1

def selectChannelInt34(): Int =
  let ch = chan Int 100;
  let _ = ch <- 0;
  select {
    case _ <- ch => -10
  }

@test
def testSelectChannelInt34(): Bool = selectChannelInt34() `assertEq!` -10

def selectChannelInt35(): Int =
  let ch = chan Int 100;
  let _ = ch <- 0;
  select {
    case _ <- ch => -214748
  }

@test
def testSelectChannelInt35(): Bool = selectChannelInt35() `assertEq!` -214748

def fInt36(ch: Channel[Int]): Channel[Int] =
  ch <- 0

def selectChannelInt36(): Int =
  let ch1 = chan Int 100;
  let _ = spawn fInt36(ch1);
  select {
    case x <- ch1 => x
  }

@test
def testSelectChannelInt36(): Bool = selectChannelInt36() `assertEq!` 0

def fInt37(ch: Channel[Int]): Channel[Int] =
  ch <- 1

def selectChannelInt37(): Int =
  let ch1 = chan Int 100;
  let _ = spawn fInt37(ch1);
  select {
    case x <- ch1 => x
  }

@test
def testSelectChannelInt37(): Bool = selectChannelInt37() `assertEq!` 1

def fInt38(ch: Channel[Int]): Channel[Int] =
  ch <- 10

def selectChannelInt38(): Int =
  let ch1 = chan Int 100;
  let _ = spawn fInt38(ch1);
  select {
    case x <- ch1 => x
  }

@test
def testSelectChannelInt38(): Bool = selectChannelInt38() `assertEq!` 10

def fInt39(ch: Channel[Int]): Channel[Int] =
  ch <- 2147483647

def selectChannelInt39(): Int =
  let ch1 = chan Int 100;
  let _ = spawn fInt39(ch1);
  select {
    case x <- ch1 => x
  }

@test
def testSelectChannelInt39(): Bool = selectChannelInt39() `assertEq!` 2147483647

def fInt40(ch: Channel[Int]): Channel[Int] =
  ch <- -1

def selectChannelInt40(): Int =
  let ch1 = chan Int 100;
  let _ = spawn fInt40(ch1);
  select {
    case x <- ch1 => x
  }

@test
def testSelectChannelInt40(): Bool = selectChannelInt40() `assertEq!` -1

def fInt41(ch: Channel[Int]): Channel[Int] =
  ch <- -10

def selectChannelInt41(): Int =
  let ch1 = chan Int 100;
  let _ = spawn fInt41(ch1);
  select {
    case x <- ch1 => x
  }

@test
def testSelectChannelInt41(): Bool = selectChannelInt41() `assertEq!` -10

def fInt42(ch: Channel[Int]): Channel[Int] =
  ch <- -214748

def selectChannelInt42(): Int =
  let ch1 = chan Int 100;
  let _ = spawn fInt42(ch1);
  select {
    case x <- ch1 => x
  }

@test
def testSelectChannelInt42(): Bool = selectChannelInt42() `assertEq!` -214748

def fInt43(ch: Channel[Int]): Channel[Int] =
  ch <- 0

def selectChannelInt43(): Int =
  let ch1 = chan Int 100;
  let ch2 = chan Int 100;
  let _ = spawn fInt43(ch2);
  select {
    case x <- ch1 => x
    case x <- ch2 => x
  }

@test
def testSelectChannelInt43(): Bool = selectChannelInt43() `assertEq!` 0

def fInt44(ch: Channel[Int]): Channel[Int] =
  ch <- 1

def selectChannelInt44(): Int =
  let ch1 = chan Int 100;
  let ch2 = chan Int 100;
  let _ = spawn fInt44(ch2);
  select {
    case x <- ch1 => x
    case x <- ch2 => x
  }

@test
def testSelectChannelInt44(): Bool = selectChannelInt44() `assertEq!` 1

def fInt45(ch: Channel[Int]): Channel[Int] =
  ch <- 10

def selectChannelInt45(): Int =
  let ch1 = chan Int 100;
  let ch2 = chan Int 100;
  let _ = spawn fInt45(ch2);
  select {
    case x <- ch1 => x
    case x <- ch2 => x
  }

@test
def testSelectChannelInt45(): Bool = selectChannelInt45() `assertEq!` 10

def fInt46(ch: Channel[Int]): Channel[Int] =
  ch <- 2147483647

def selectChannelInt46(): Int =
  let ch1 = chan Int 100;
  let ch2 = chan Int 100;
  let _ = spawn fInt46(ch2);
  select {
    case x <- ch1 => x
    case x <- ch2 => x
  }

@test
def testSelectChannelInt46(): Bool = selectChannelInt46() `assertEq!` 2147483647

def fInt47(ch: Channel[Int]): Channel[Int] =
  ch <- -1

def selectChannelInt47(): Int =
  let ch1 = chan Int 100;
  let ch2 = chan Int 100;
  let _ = spawn fInt47(ch2);
  select {
    case x <- ch1 => x
    case x <- ch2 => x
  }

@test
def testSelectChannelInt47(): Bool = selectChannelInt47() `assertEq!` -1

def fInt48(ch: Channel[Int]): Channel[Int] =
  ch <- -10

def selectChannelInt48(): Int =
  let ch1 = chan Int 100;
  let ch2 = chan Int 100;
  let _ = spawn fInt48(ch2);
  select {
    case x <- ch1 => x
    case x <- ch2 => x
  }

@test
def testSelectChannelInt48(): Bool = selectChannelInt48() `assertEq!` -10

def fInt49(ch: Channel[Int]): Channel[Int] =
  ch <- -214748

def selectChannelInt49(): Int =
  let ch1 = chan Int 100;
  let ch2 = chan Int 100;
  let _ = spawn fInt49(ch2);
  select {
    case x <- ch1 => x
    case x <- ch2 => x
  }

@test
def testSelectChannelInt49(): Bool = selectChannelInt49() `assertEq!` -214748

// Str
def fStr01(ch: Channel[Str]): Channel[Str] =
  ch <- ""

def selectChannelStr01(): Str =
  let ch = chan Str 100;
  let _ = spawn fStr01(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannelStr01(): Bool = selectChannelStr01() `assertEq!` ""

def fStr02(ch: Channel[Str]): Channel[Str] =
  ch <- "a"

def selectChannelStr02(): Str =
  let ch = chan Str 100;
  let _ = spawn fStr02(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannelStr02(): Bool = selectChannelStr02() `assertEq!` "a"

def fStr03(ch: Channel[Str]): Channel[Str] =
  ch <- "str"

def selectChannelStr03(): Str =
  let ch = chan Str 100;
  let _ = spawn fStr03(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannelStr03(): Bool = selectChannelStr03() `assertEq!` "str"

def fStr04(ch: Channel[Str]): Channel[Str] =
  ch <- ""

def selectChannelStr04(): Str =
  let ch = chan Str 100;
  let _ = spawn fStr04(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannelStr04(): Bool = selectChannelStr04() `assertEq!` ""

def fStr05(ch: Channel[Str]): Channel[Str] =
  ch <- "str"

def selectChannelStr05(): Str =
  let ch = chan Str 100;
  let _ = spawn fStr05(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannelStr05(): Bool = selectChannelStr05() `assertEq!` "str"

def fStr06(ch: Channel[Str]): Channel[Str] =
  ch <- ""

def selectChannelStr06(): Str =
  let ch = chan Str 100;
  let _ = spawn fStr06(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannelStr06(): Bool = selectChannelStr06() `assertEq!` ""

def fStr07(ch: Channel[Str]): Channel[Str] =
  ch <- "str"

def selectChannelStr07(): Str =
  let ch = chan Str 100;
  let _ = spawn fStr07(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannelStr07(): Bool = selectChannelStr07() `assertEq!` "str"

def selectChannelStr08(): Str =
  let ch = chan Str 100;
  let _ = ch <- "";
  select {
    case x <- ch => x
  }

@test
def testSelectChannelStr08(): Bool = selectChannelStr08() `assertEq!` ""

def selectChannelStr09(): Str =
  let ch = chan Str 100;
  let _ = ch <- "str";
  select {
    case x <- ch => x
  }

@test
def testSelectChannelStr09(): Bool = selectChannelStr09() `assertEq!` "str"

def selectChannelStr10(): Str =
  let ch = chan Str 100;
  let _ = ch <- "";
  select {
    case _ <- ch => ""
  }

@test
def testSelectChannelStr10(): Bool = selectChannelStr10() `assertEq!` ""

def selectChannelStr11(): Str =
  let ch = chan Str 100;
  let _ = ch <- "";
  select {
    case _ <- ch => "str"
  }

@test
def testSelectChannelStr11(): Bool = selectChannelStr11() `assertEq!` "str"

def fStr12(ch: Channel[Str]): Channel[Str] =
  ch <- ""

def selectChannelStr12(): Str =
  let ch1 = chan Str 100;
  let _ = spawn fStr12(ch1);
  select {
    case x <- ch1 => x
  }

@test
def testSelectChannelStr12(): Bool = selectChannelStr12() `assertEq!` ""

def fStr13(ch: Channel[Str]): Channel[Str] =
  ch <- "str"

def selectChannelStr13(): Str =
  let ch1 = chan Str 100;
  let _ = spawn fStr13(ch1);
  select {
    case x <- ch1 => x
  }

@test
def testSelectChannelStr13(): Bool = selectChannelStr13() `assertEq!` "str"

def fStr14(ch: Channel[Str]): Channel[Str] =
  ch <- ""

def selectChannelStr14(): Str =
  let ch1 = chan Str 100;
  let ch2 = chan Str 100;
  let _ = spawn fStr14(ch2);
  select {
    case x <- ch1 => x
    case x <- ch2 => x
  }

@test
def testSelectChannelStr14(): Bool = selectChannelStr14() `assertEq!` ""

def fStr15(ch: Channel[Str]): Channel[Str] =
  ch <- "str"

def selectChannelStr15(): Str =
  let ch1 = chan Str 100;
  let ch2 = chan Str 100;
  let _ = spawn fStr15(ch2);
  select {
    case x <- ch1 => x
    case x <- ch2 => x
  }

@test
def testSelectChannelStr15(): Bool = selectChannelStr15() `assertEq!` "str"

// Char
def fChar01(ch: Channel[Char]): Channel[Char] =
  ch <- 'a'

def selectChannelChar01(): Char =
  let ch = chan Char 100;
  let _ = spawn fChar01(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannelChar01(): Bool = selectChannelChar01() `assertEq!` 'a'

def fChar02(ch: Channel[Char]): Channel[Char] =
  ch <- 'c'

def selectChannelChar02(): Char =
  let ch = chan Char 100;
  let _ = spawn fChar02(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannelChar02(): Bool = selectChannelChar02() `assertEq!` 'c'

def fChar03(ch: Channel[Char]): Channel[Char] =
  ch <- 'a'

def selectChannelChar03(): Char =
  let ch = chan Char 100;
  let _ = spawn fChar03(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannelChar03(): Bool = selectChannelChar03() `assertEq!` 'a'

def fChar04(ch: Channel[Char]): Channel[Char] =
  ch <- 'c'

def selectChannelChar04(): Char =
  let ch = chan Char 100;
  let _ = spawn fChar04(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannelChar04(): Bool = selectChannelChar04() `assertEq!` 'c'

def fChar05(ch: Channel[Char]): Channel[Char] =
  ch <- 'a'

def selectChannelChar05(): Char =
  let ch = chan Char 100;
  let _ = spawn fChar05(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannelChar05(): Bool = selectChannelChar05() `assertEq!` 'a'

def fChar06(ch: Channel[Char]): Channel[Char] =
  ch <- 'c'

def selectChannelChar06(): Char =
  let ch = chan Char 100;
  let _ = spawn fChar06(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannelChar06(): Bool = selectChannelChar06() `assertEq!` 'c'

def selectChannelChar07(): Char =
  let ch = chan Char 100;
  let _ = ch <- 'a';
  select {
    case x <- ch => x
  }

@test
def testSelectChannelChar07(): Bool = selectChannelChar07() `assertEq!` 'a'

def selectChannelChar08(): Char =
  let ch = chan Char 100;
  let _ = ch <- 'c';
  select {
    case x <- ch => x
  }

@test
def testSelectChannelChar08(): Bool = selectChannelChar08() `assertEq!` 'c'

def selectChannelChar09(): Char =
  let ch = chan Char 100;
  let _ = ch <- 'z';
  select {
    case _ <- ch => 'a'
  }

@test
def testSelectChannelChar09(): Bool = selectChannelChar09() `assertEq!` 'a'

def selectChannelChar10(): Char =
  let ch = chan Char 100;
  let _ = ch <- 'a';
  select {
    case _ <- ch => 'c'
  }

@test
def testSelectChannelChar10(): Bool = selectChannelChar10() `assertEq!` 'c'

def fChar11(ch: Channel[Char]): Channel[Char] =
  ch <- 'a'

def selectChannelChar11(): Char =
  let ch1 = chan Char 100;
  let _ = spawn fChar11(ch1);
  select {
    case x <- ch1 => x
  }

@test
def testSelectChannelChar11(): Bool = selectChannelChar11() `assertEq!` 'a'

def fChar12(ch: Channel[Char]): Channel[Char] =
  ch <- 'c'

def selectChannelChar12(): Char =
  let ch1 = chan Char 100;
  let _ = spawn fChar12(ch1);
  select {
    case x <- ch1 => x
  }

@test
def testSelectChannelChar12(): Bool = selectChannelChar12() `assertEq!` 'c'

def fChar13(ch: Channel[Char]): Channel[Char] =
  ch <- 'a'

def selectChannelChar13(): Char =
  let ch1 = chan Char 100;
  let ch2 = chan Char 100;
  let _ = spawn fChar13(ch2);
  select {
    case x <- ch1 => x
    case x <- ch2 => x
  }

@test
def testSelectChannelChar13(): Bool = selectChannelChar13() `assertEq!` 'a'

def fChar14(ch: Channel[Char]): Channel[Char] =
  ch <- 'c'

def selectChannelChar14(): Char =
  let ch1 = chan Char 100;
  let ch2 = chan Char 100;
  let _ = spawn fChar14(ch2);
  select {
    case x <- ch1 => x
    case x <- ch2 => x
  }

@test
def testSelectChannelChar14(): Bool = selectChannelChar14() `assertEq!` 'c'

// Option
def fOption01(ch: Channel[Option[Int]]): Channel[Option[Int]] =
  ch <- None

def selectChannelOption01(): Option[Int] =
  let ch = chan Option[Int] 100;
  let _ = spawn fOption01(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannelOption01(): Bool = selectChannelOption01() `assertEq!` None

def fOption02(ch: Channel[Option[Int]]): Channel[Option[Int]] =
  ch <- Some(42)

def selectChannelOption02(): Option[Int] =
  let ch = chan Option[Int] 100;
  let _ = spawn fOption02(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannelOption02(): Bool = selectChannelOption02() `assertEq!` Some(42)

def fOption03(ch: Channel[Option[Int]]): Channel[Option[Int]] =
  ch <- None

def selectChannelOption03(): Int =
  let ch = chan Option[Int] 100;
  let _ = spawn fOption03(ch);
  select {
    case x <- ch => match x with {
      case Some(e) => e
      case None => -1
    }
  }

@test
def testSelectChannelOption03(): Bool = selectChannelOption03() `assertEq!` -1

def fOption04(ch: Channel[Option[Int]]): Channel[Option[Int]] =
  ch <- Some(42)

def selectChannelOption04(): Int =
  let ch = chan Option[Int] 100;
  let _ = spawn fOption04(ch);
  select {
    case x <- ch => match x with {
      case Some(e) => e
      case None => -1
    }
  }

@test
def testSelectChannelOption04(): Bool = selectChannelOption04() `assertEq!` 42

def fOption05(ch: Channel[Option[Str]]): Channel[Option[Str]] =
  ch <- None

def selectChannelOption05(): Option[Str] =
  let ch = chan Option[Str] 100;
  let _ = spawn fOption05(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannelOption05(): Bool = selectChannelOption05() `assertEq!` None

def fOption06(ch: Channel[Option[Str]]): Channel[Option[Str]] =
  ch <- Some("Str")

def selectChannelOption06(): Option[Str] =
  let ch = chan Option[Str] 100;
  let _ = spawn fOption06(ch);
  select {
    case x <- ch => x
  }

@test
def testSelectChannelOption06(): Bool = selectChannelOption06() `assertEq!` Some("Str")

def foption07(ch: Channel[Option[Str]]): Channel[Option[Str]] =
  ch <- None

def selectChanneloption07(): Str =
  let ch = chan Option[Str] 100;
  let _ = spawn foption07(ch);
  select {
    case x <- ch => match x with {
      case Some(e) => e
      case None => ""
    }
  }

@test
def testSelectChanneloption07(): Bool = selectChanneloption07() `assertEq!` ""

def fOption08(ch: Channel[Option[Str]]): Channel[Option[Str]] =
  ch <- Some("str")

def selectChannelOption08(): Str =
  let ch = chan Option[Str] 100;
  let _ = spawn fOption08(ch);
  select {
    case x <- ch => match x with {
      case Some(e) => e
      case None => ""
    }
  }

@test
def testSelectChannelOption08(): Bool = selectChannelOption08() `assertEq!` "str"










def fIf01(ch: Channel[Bool]): Channel[Bool] =
  ch <- true

def selectChannelIf01(): Int =
  let ch = chan Bool 100;
  let _ = spawn fIf01(ch);
  select {
    case x <- ch => if (x) 3 else 7
  }

@test
def testSelectChannelIf01(): Bool = selectChannelIf01() `assertEq!` 3

def fIf02(ch: Channel[Bool]): Channel[Bool] =
  ch <- false

def selectChannelIf02(): Int =
  let ch = chan Bool 100;
  let _ = spawn fIf02(ch);
  select {
    case x <- ch => if (x) 3 else 7
  }

@test
def testSelectChannelIf02(): Bool = selectChannelIf02() `assertEq!` 7

def fIf03(ch: Channel[Bool]): Channel[Bool] =
  ch <- true

def selectChannelIf03(): Str =
  let ch = chan Bool 100;
  let _ = spawn fIf03(ch);
  select {
    case x <- ch => if (x) "a" else "b"
  }

@test
def testSelectChannelIf03(): Bool = selectChannelIf03() `assertEq!` "a"

def fIf04(ch: Channel[Bool]): Channel[Bool] =
  ch <- false

def selectChannelIf04(): Str =
  let ch = chan Bool 100;
  let _ = spawn fIf04(ch);
  select {
    case x <- ch => if (x) "a" else "b"
  }

@test
def testSelectChannelIf04(): Bool = selectChannelIf04() `assertEq!` "b"

def fIf05(ch: Channel[Bool]): Channel[Bool] =
  ch <- true

def selectChannelIf05(): Char =
  let ch = chan Bool 100;
  let _ = spawn fIf05(ch);
  select {
    case x <- ch => if (x) 'a' else 'b'
  }

@test
def testSelectChannelIf05(): Bool = selectChannelIf05() `assertEq!` 'a'

def fIf06(ch: Channel[Bool]): Channel[Bool] =
  ch <- false

def selectChannelIf06(): Char =
  let ch = chan Bool 100;
  let _ = spawn fIf06(ch);
  select {
    case x <- ch => if (x) 'a' else 'b'
  }

@test
def testSelectChannelIf06(): Bool = selectChannelIf06() `assertEq!` 'b'

def fMultipleChannels01(ch: Channel[Int]): Channel[Int] =
  ch <- 1

def selectChannelMultipleChannels01(): Int =
  let ch1 = chan Int 100;
  let ch2 = chan Str 100;
  let _ = spawn fMultipleChannels01(ch1);
  select {
    case x <- ch1 => x
    case _ <- ch2 => -1
  }

@test
def testSelectChannelMultipleChannels01(): Bool = selectChannelMultipleChannels01() `assertEq!` 1

def fMultipleChannels02(ch: Channel[Str]): Channel[Str] =
  ch <- "a"

def selectChannelMultipleChannels02(): Int =
  let ch1 = chan Int 100;
  let ch2 = chan Str 100;
  let _ = spawn fMultipleChannels02(ch2);
  select {
    case x <- ch1 => x
    case _ <- ch2 => -1
  }

@test
def testSelectChannelMultipleChannels02(): Bool = selectChannelMultipleChannels02() `assertEq!` -1

def fMultipleChannels03(ch: Channel[Int]): Channel[Int] =
  ch <- 1

def selectChannelMultipleChannels03(): Str =
  let ch1 = chan Int 100;
  let ch2 = chan Str 100;
  let _ = spawn fMultipleChannels03(ch1);
  select {
    case _ <- ch1 => "z"
    case x <- ch2 => x
  }

@test
def testSelectChannelMultipleChannels03(): Bool = selectChannelMultipleChannels03() `assertEq!` "z"

def fMultipleChannels04(ch: Channel[Str]): Channel[Str] =
  ch <- "a"

def selectChannelMultipleChannels04(): Str =
  let ch1 = chan Int 100;
  let ch2 = chan Str 100;
  let _ = spawn fMultipleChannels04(ch2);
  select {
    case _ <- ch1 => "z"
    case x <- ch2 => x
  }

@test
def testSelectChannelMultipleChannels04(): Bool = selectChannelMultipleChannels04() `assertEq!` "a"

def fMultipleSelects01(): Int =
  let ch1 = chan Int 100;
  let ch2 = chan Int 100;
  let ch3 = chan Int 100;
  let ch4 = chan Int 100;
  let _ = spawn gMultipleSelects01(ch1, ch2, ch3);
  let _ = spawn hMultipleSelects01(ch1, ch2, ch3, ch4);
  select {
    case x <- ch1 => x
    case x <- ch2 => x
    case x <- ch4 => x
  }

def gMultipleSelects01(ch1: Channel[Int], ch2: Channel[Int], ch3: Channel[Int]): () =
  let _ = ch1 <- 1;
  let _ = ch2 <- 2;
  let _ = ch3 <- 3;
  ()

def hMultipleSelects01(ch1: Channel[Int], ch2: Channel[Int], ch3: Channel[Int], ch4: Channel[Int]): Unit =
  let y = select {
    case x <- ch2 => x
    case x <- ch3 => x
    case x <- ch1 => x
  };
  let _ = ch4 <- y;
  ()

@test
def testSelectChannelMultipleSelects01(): Bool =
  let x = fMultipleSelects01();
  (x `assertEq!` 1) || (x `assertEq!` 2) || (x `assertEq!` 3)

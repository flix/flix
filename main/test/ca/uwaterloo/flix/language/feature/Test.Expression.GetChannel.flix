@test
def testGetChannel01(): Unit =
  let c = chan Unit;
  c <- ();
  <- c

@test
def testGetChannel02(): Bool =
  let c = chan Bool;
  c <- true;
  <- c

@test
def testGetChannel03(): Char =
  let c = chan Char;
  c <- 'c';
  <- c

@test
def testGetChannel04(): Float =
  let c = chan Float;
  c <- 2.0;
  <- c

@test
def testGetChannel05(): Int =
  let c = chan Int;
  c <- 3;
  <- c

@test
def testGetChannel06(): BigInt =
  let c = chan BigInt;
  c <- 2ii;
  <- c

@test
def testGetChannel07(): Str =
  let c = chan Str;
  c <- "asd";
  <- c

@test
def testGetChannel08(): Channel[Int] =
  let c = chan Channel[Int];
  c <- chan Int;
  <- c

@test
def testGetChannel09(): Array[Str] =
  let c = chan Array[Str];
  c <- ["a", "b"];
  <- c

@test
def testGetChannel10(): Int =
  let c = chan Int;
  c <- 1 <- 2;
  <- c;
  <- c;

@test
def testGetChannel11(): Int =
  let c = chan Int;
  c <- 1 <- 2;
  c <- 1 <- 2;
  <- c;
  <- c;
  <- c;
  <- c;

@test
def testGetChannel12(): Int =
  let c = chan Int;
  c <- 1;
  <- (c <- <- c)

@test
def testGetChannel13(): Int =
  let c = chan Int;
  c <- 0 <- 0 <- 1 <- 0;
  (2 * <- c - <- c) / <- c

@test
def testGetChannel14(): Int =
  let c1 = chan Int;
  c1 <- 1;
  let c2 = chan Channel[Int];
  c2 <- c1;
  <- <- c2

@test
def testGetChannel15(): Bool =
  let c1 = chan Option[Str];
  c1 <- Some("test");
  Option.getWithDefault(<- c1, "") `assertEq!` "test"

@test
def testGetChannel16(): Int =
  let c1 = chan Option[Int];
  c1 <- None;
  let x = <- c1;
  Option.getWithDefault(x, 42)

// TODO SJ: This does not work. We are waiting forever without getting anything
//@test
//def testGetChannel17(): Option[Str] =
//  let c1 = chan Option[Str];
//  c1 <- None;
//  let x = <- c1;
//  x
@test
def testPutChannel01(): Channel[Unit] =
  let c = chan Unit;
  c <- ()

@test
def testPutChannel02(): Channel[Bool] =
  let c = chan Bool;
  c <- true

@test
def testPutChannel03(): Channel[Char] =
  let c = chan Char;
  c <- 'a'

@test
def testPutChannel04(): Channel[Float] =
  let c = chan Float;
  c <- 3.0

@test
def testPutChannel05(): Channel[Int] =
  let c = chan Int;
  c <- 3

@test
def testPutChannel06(): Channel[BigInt] =
  let c = chan BigInt;
  c <- 2ii

@test
def testPutChannel07(): Channel[Str] =
  let c = chan Str;
  c <- "test"

@test
def testPutChannel08(): Channel[Channel[Int]] =
  let c = chan Channel[Int];
  c <- chan Int

@test
def testPutChannel09(): Channel[Array[Str]] =
  let c = chan Array[Str];
  c <- ["1", "2"]

@test
def testPutChannel10(): Channel[Int] =
  let c = chan Int;
  c <- 1 <- 2

@test
def testPutChannel11(): Channel[Int] =
  let c = chan Int;
  c <- 1 <- 2 <- 3 <- 4

@test
def testPutChannel12(): Channel[Channel[Int]] =
  let c1 = chan Channel[Int];
  let c2 = chan Int;
  c1 <- (c2 <- 1)

@test
def testPutChannel13(): Channel[Channel[Int]] =
  let c1 = chan Channel[Int];
  let c2 = chan Int;
  c1 <- (c2 <- 1) <- (c2 <- 2)

@test
def testPutChannel14(): Channel[Int] =
  let c1 = chan Channel[Int];
  let c2 = chan Int;
  c1 <- (c2 <- 1) <- (c2 <- 2);
  <- c1

@test
def testPutChannel15(): Int =
  let c1 = chan Channel[Int];
  let c2 = chan Int;
  c1 <- (c2 <- 1) <- (c2 <- 2);
  <- <- c1

@test
def testPutChannel16(): Channel[Int] =
  let c = chan Int;
  c <- 1 + 1

@test
def testPutChannel17(): Channel[Option[Int]] =
  let c = chan Option[Int];
  c <- Some(42)

@test
def testPutChannel18(): Channel[Option[Int]] =
  let c = chan Option[Int];
  c <- None
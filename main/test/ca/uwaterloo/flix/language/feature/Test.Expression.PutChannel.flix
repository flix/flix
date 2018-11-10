@test
def testPutOnUnit(): Channel[Unit] =
    let c = newch Unit;
    c <- ()

@test
def testPutOnBool(): Channel[Bool] =
    let c = newch Bool;
    c <- true

// TODO SJ: this doesnt work
//@test
//def testPutOnChar(): Channel[Char] =
//    let c = newch Char;
//    c <- 'a'

@test
def testPutOnFloat(): Channel[Float] =
    let c = newch Float;
    c <- 3.0

@test
def testPutOnInt(): Channel[Int] =
    let c = newch Int;
    c <- 3

@test
def testPutOnBigInt(): Channel[BigInt] =
    let c = newch BigInt;
    c <- 2ii

@test
def testPutOnStr(): Channel[Str] =
    let c = newch Str;
    c <- "test"

@test
def testPutOnChannel(): Channel[Channel[Int]] =
    let c = newch Channel[Int];
    c <- newch Int

@test
def testPutOnArray(): Channel[Array[Str]] =
    let c = newch Array[Str];
    c <- ["1", "2"]

@test
def testChainedPut1(): Channel[Int] =
    let c = newch Int;
    c <- 1 <- 2

@test
def testChainedPut2(): Channel[Int] =
    let c = newch Int;
    c <- 1 <- 2 <- 3 <- 4

@test
def testPutOnMultipleChannels1(): Channel[Channel[Int]] =
    let c1 = newch Channel[Int];
    let c2 = newch Int;
    c1 <- (c2 <- 1)

@test
def testPutOnMultipleChannels2(): Channel[Channel[Int]] =
    let c1 = newch Channel[Int];
    let c2 = newch Int;
    // TODO SJ: What does this do?
    c1 <- (c2 <- 1) <- (c2 <- 2)

@test
def testPutCombinedOperations1(): Channel[Int] =
    let c = newch Int;
    c <- 1 + 1

//TODO SJ: this doesnt work
//@test
//def testPutCombinedOperations2(): Channel[Bool] =
//    let c = newch Bool;
//    c <- c == newch Bool
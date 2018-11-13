//TODO SJ: fix naming

@test
def testPutOnUnit(): Channel[Unit] =
    let c = chan Unit;
    c <- ()

@test
def testPutOnBool(): Channel[Bool] =
    let c = chan Bool;
    c <- true

// TODO SJ: this doesnt work
//@test
//def testPutOnChar(): Channel[Char] =
//    let c = chan Char;
//    c <- 'a'

@test
def testPutOnFloat(): Channel[Float] =
    let c = chan Float;
    c <- 3.0

@test
def testPutOnInt(): Channel[Int] =
    let c = chan Int;
    c <- 3

@test
def testPutOnBigInt(): Channel[BigInt] =
    let c = chan BigInt;
    c <- 2ii

@test
def testPutOnStr(): Channel[Str] =
    let c = chan Str;
    c <- "test"

@test
def testPutOnChannel(): Channel[Channel[Int]] =
    let c = chan Channel[Int];
    c <- chan Int

@test
def testPutOnArray(): Channel[Array[Str]] =
    let c = chan Array[Str];
    c <- ["1", "2"]

@test
def testChainedPut1(): Channel[Int] =
    let c = chan Int;
    c <- 1 <- 2

@test
def testChainedPut2(): Channel[Int] =
    let c = chan Int;
    c <- 1 <- 2 <- 3 <- 4

@test
def testPutOnMultipleChannels1(): Channel[Channel[Int]] =
    let c1 = chan Channel[Int];
    let c2 = chan Int;
    c1 <- (c2 <- 1)

@test
def testPutOnMultipleChannels2(): Channel[Channel[Int]] =
    let c1 = chan Channel[Int];
    let c2 = chan Int;
    // TODO SJ: What does this do?
    c1 <- (c2 <- 1) <- (c2 <- 2)

@test
def testPutCombinedOperations1(): Channel[Int] =
    let c = chan Int;
    c <- 1 + 1

//TODO SJ: this doesnt work
//@test
//def testPutCombinedOperations2(): Channel[Bool] =
//    let c = chan Bool;
//    let someBool = c == chan Bool;
//    c <- someBool
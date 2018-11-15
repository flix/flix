//TODO SJ: fix naming

@test
def testGetOnUnit(): Unit =
    let c = chan Unit;
    c <- ();
    <- c

@test
def testGetOnBool(): Bool =
    let c = chan Bool;
    c <- true;
    <- c

//TODO SJ: this doesnt work
@test
def testGetOnChar(): Char =
    let c = chan Char;
    c <- 'c';
    <- c

@test
def testGetOnFloat(): Float =
    let c = chan Float;
    c <- 2.0;
    <- c

@test
def testGetOnInt(): Int =
    let c = chan Int;
    c <- 3;
    <- c

@test
def testGetOnBigInt(): BigInt =
    let c = chan BigInt;
    c <- 2ii;
    <- c

@test
def testGetOnStr(): Str =
    let c = chan Str;
    c <- "asd";
    <- c

@test
def testGetOnChannel(): Channel[Int] =
    let c = chan Channel[Int];
    c <- chan Int;
    <- c

@test
def testGetOnArray(): Array[Str] =
    let c = chan Array[Str];
    c <- ["a", "b"];
    <- c

@test
def testBiggerSize1(): Int =
    let c = chan Int;
    c <- 1 <- 2;
    <- c;
    <- c;

@test
def testBiggerSize2(): Int =
    let c = chan Int;
    c <- 1 <- 2;
    c <- 1 <- 2;
    <- c;
    <- c;
    <- c;
    <- c;

@test
def testGetPutCombined(): Int =
    let c = chan Int;
    c <- 1;
    /* TODO SJ: this (...) is probably okay
    since we want <-x binding strongly in
    most reasonably cases */
    <- (c <- <- c)

@test
def testGetInline(): Int =
    let c = chan Int;
    c <- 0 <- 0 <- 1 <- 0;
    //TODO SJ: Is it on purpose we dont allow "<-c" but only "<- c"
    (2 * <- c - <- c) / <- c

@test
def testNestedGet(): Int =
    let c1 = chan Int;
    c1 <- 1;
    let c2 = chan Channel[Int];
    c2 <- c1;
    <- <- c2
@test
def testGetOnUnit(): Unit =
    let c = newch Unit;
    c <- ();
    <- c

@test
def testGetOnBool(): Bool =
    let c = newch Bool;
    c <- true;
    <- c

//TODO SJ: this doesnt work
//@test
//def testGetOnChar(): Char =
//    let c = newch Char;
//    c <- 'c';
//    <- c

@test
def testGetOnFloat(): Float =
    let c = newch Float;
    c <- 2.0;
    <- c

@test
def testGetOnInt(): Int =
    let c = newch Int;
    c <- 3;
    <- c

@test
def testGetOnBigInt(): BigInt =
    let c = newch BigInt;
    c <- 2ii;
    <- c

@test
def testGetOnStr(): Str =
    let c = newch Str;
    c <- "asd";
    <- c

@test
def testGetOnChannel(): Channel[Int] =
    let c = newch Channel[Int];
    c <- newch Int;
    <- c

@test
def testGetOnArray(): Array[Str] =
    let c = newch Array[Str];
    c <- ["a", "b"];
    <- c

@test
def testBiggerSize1(): Int =
    let c = newch Int;
    c <- 1 <- 2;
    <- c;
    <- c;

@test
def testBiggerSize2(): Int =
    let c = newch Int;
    c <- 1 <- 2;
    c <- 1 <- 2;
    <- c;
    <- c;
    <- c;
    <- c;

@test
def testGetPutCombined(): Int =
    let c = newch Int;
    c <- 1;
    /* TODO SJ: this (...) is probably okay
    since we want <-x binding strongly in
    most reasonably cases */
    <- (c <- <- c)

@test
def testGetInline(): Int =
    let c = newch Int;
    c <- 0 <- 0 <- 1 <- 0;
    //TODO SJ: Is it on purpose we dont allow "<-c" but only "<- c"
    (2 * <- c - <- c) / <- c

@test
def testNestedGet(): Int =
    let c1 = newch Int;
    c1 <- 1;
    let c2 = newch Channel[Int];
    c2 <- c1;
    <- <- c2
def testAll(): Int =
    if (!test1()) 1 else
    if (!test2()) 2 else
    if (!test3()) 3 else
    if (!test4()) 4 else
    if (!test5()) 5 else
    if (!test6()) 6 else
    if (!test7()) 7 else
    if (!test8()) 8 else
    if (!test9()) 9 else
    if (!test10()) 10 else
    if (!test11()) 11 else
    -1

// PUT
def test1(): Bool =
    let c = newch Int;
    c <- 24;
    true

def test2(): Bool =
    let c = newch Int;
    c <- 24;
    c <- 25;
    true

def test3(): Bool =
    let c = newch Bool;
    c <- true;
    c <- false;
    true

def test4(): Bool =
    let c = newch Array[Int];
    c <- [1,2];
    c <- [2,3];
    true

def test5(): Bool =
    let c = newch Int;
    c <- 3 <- 4;
    true

// GET
def test6(): Bool =
    let c = newch Int;
    c <- 4;
    <- c == 4

def test7(): Bool =
    let c = newch Bool;
    c <- false;
    ! <- c

def test8(): Bool =
    let c = newch Int;
    c <- 1 <- 2;
    <- c == 1

def test9(): Bool =
    let c = newch Int;
    c <- 1;
    <- c + 1 == 2

def test10(): Bool =
    let c = newch Channel[Int];
    let c2 = newch Int;
    c2 <- 1;
    c <- c2;
    <- <- c == 1

def test11(): Bool =
    let c = newch Channel[Int];
    let c2 = newch Int;
    c2 <- 1;
    c <- c2;
    <- c <- 2;
    true
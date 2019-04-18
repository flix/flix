@test
def testStm01(): Int =
    f();
    123

@test
def testStm02(): Int =
    f();
    g();
    123

@test
def testStm03(): Int =
    f();
    g();
    h();
    123

@test
def testStm04(): Int =
    let x = 123;
    f();
    x

@test
def testStm05(): Int =
    f();
    let x = 123;
    x

@test
def testStm06(): Int =
    let x = 123;
    let y = 456;
    f();
    g();
    x + y

@test
def testStm07(): Int =
    let x = 123;
    f();
    let y = 456;
    g();
    x + y

@test
def testStm08(): Int =
    let x = 123;
    f();
    g();
    let y = 456;
    x + y

@test
def testStm09(): Int =
    f();
    let x = 123;
    g();
    let y = 456;
    x + y

@test
def testStm10(): Int =
    f();
    g();
    let x = 123;
    let y = 456;
    x + y

// TODO
//@test
//def testStm11(): Int =
//    let x = {
//        f();
//        123
//    };
//    x

// TODO
//@test
//def testStm12(): Int =
//    let x = {
//        f();
//        g();
//        123
//    };
//    h();
//    x

def f(): Unit = ()

def g(): Unit = ()

def h(): Unit = ()

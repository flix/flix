@test
def testGetChannel01(): Unit =
  let c = chan Unit 100;
  c <- ();
  <- c

@test
def testGetChannel02(): Bool =
  let c = chan Bool 100;
  c <- true;
  <- c

@test
def testGetChannel03(): Char =
  let c = chan Char 100;
  c <- 'c';
  <- c

@test
def testGetChannel04(): Float =
  let c = chan Float 100;
  c <- 2.0;
  <- c

@test
def testGetChannel05(): Int =
  let c = chan Int 100;
  c <- 3;
  <- c

@test
def testGetChannel06(): BigInt =
  let c = chan BigInt 100;
  c <- 2ii;
  <- c

@test
def testGetChannel07(): Str =
  let c = chan Str 100;
  c <- "asd";
  <- c

@test
def testGetChannel08(): Channel[Int] =
  let c = chan Channel[Int] 100;
  c <- chan Int 100;
  <- c

@test
def testGetChannel09(): Array[String] @ Impure =
  let c = chan Array[Str] 100;
  c <- ["a", "b"];
  <- c

@test
def testGetChannel10(): Int =
  let c = chan Int 100;
  c <- 1 <- 2;
  <- c;
  <- c

@test
def testGetChannel11(): Int =
  let c = chan Int 100;
  c <- 1 <- 2;
  c <- 1 <- 2;
  <- c;
  <- c;
  <- c;
  <- c

@test
def testGetChannel12(): Int =
  let c = chan Int 100;
  c <- 1;
  <- (c <- <- c)

@test
def testGetChannel13(): Int =
  let c = chan Int 100;
  c <- 0 <- 0 <- 1 <- 0;
  (2 * <- c - <- c) / <- c

@test
def testGetChannel14(): Int =
  let c1 = chan Int 100;
  c1 <- 1;
  let c2 = chan Channel[Int] 100;
  c2 <- c1;
  <- <- c2

@test
def testGetChannel15(): Bool =
  let c1 = chan Option[Str] 100;
  c1 <- Some("test");
  Option.getWithDefault(<- c1, "") `assertEq!` "test"

@test
def testGetChannel16(): Int =
  let c1 = chan Option[Int] 100;
  c1 <- None;
  let x = <- c1;
  Option.getWithDefault(x, 42)

// TODO SJ: This does not work. We are waiting forever without getting anything
//@test
//def testGetChannel17(): Option[Str] =
//  let c1 = chan Option[Str] 100;
//  c1 <- None;
//  let x = <- c1;
//  x

// TESTS FROM PREVIOUS GROUP

def getChannelUnit01(): Unit =
  let ch = chan Unit 100;
  let _ = ch <- ();
  <- ch

@test
def testGetChannelUnit01(): Bool = getChannelUnit01() `assertEq!` ()

def getChannelUnit02(): Unit =
  let ch = chan Unit 100;
  let _ = ch <- ();
  <- ch

@test
def testGetChannelUnit02(): Bool = getChannelUnit02() `assertEq!` ()

def fUnit03(ch: Channel[Unit]): Channel[Unit] =
  ch <- ()

def getChannelUnit03(): Unit @ Impure =
  let ch = chan Unit 100;
  let _ = spawn fUnit03(ch);
  <- ch

@test
def testGetChannelUnit03(): Bool = getChannelUnit03() `assertEq!` ()

def fUnit04(ch: Channel[Unit]): Channel[Unit] =
  ch <- ()

def getChannelUnit04(): Unit @ Impure =
  let ch = chan Unit 100;
  let _ = spawn fUnit04(ch);
  <- ch

@test
def testGetChannelUnit04(): Bool = getChannelUnit04() `assertEq!` ()

def getChannelBool01(): Bool =
  let ch = chan Bool 100;
  let _ = ch <- true;
  <- ch

@test
def testGetChannelBool01(): Bool = getChannelBool01() `assertEq!` true

def getChannelBool02(): Bool =
  let ch = chan Bool 100;
  let _ = ch <- false;
  <- ch

@test
def testGetChannelBool02(): Bool = getChannelBool02() `assertEq!` false

def fBool03(ch: Channel[Bool]): Channel[Bool] =
  ch <- true

def getChannelBool03(): Bool @ Impure =
  let ch = chan Bool 100;
  let _ = spawn fBool03(ch);
  <- ch

@test
def testGetChannelBool03(): Bool = getChannelBool03() `assertEq!` true

def fBool04(ch: Channel[Bool]): Channel[Bool] =
  ch <- false

def getChannelBool04(): Bool @ Impure =
  let ch = chan Bool 100;
  let _ = spawn fBool04(ch);
  <- ch

@test
def testGetChannelBool04(): Bool = getChannelBool04() `assertEq!` false

def getChannelInt01(): Int =
  let ch = chan Int 100;
  let _ = ch <- 2;
  <- ch

@test
def testGetChannelInt01(): Bool = getChannelInt01() `assertEq!` 2
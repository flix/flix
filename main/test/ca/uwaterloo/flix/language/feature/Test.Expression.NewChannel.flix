@test
def testNewChannel01(): Channel[Unit] = chan Unit 100
@test
def testNewChannel02(): Channel[Bool] = chan Bool 100
@test
def testNewChannel03(): Channel[Char] = chan Char 100
@test
def testNewChannel04(): Channel[Float] = chan Float 100
@test
def testNewChannel05(): Channel[Int] = chan Int 100
@test
def testNewChannel06(): Channel[BigInt] = chan BigInt 100
@test
def testNewChannel07(): Channel[Str] = chan Str 100
@test
def testNewChannel08(): Channel[Channel[Int]] = chan Channel[Int] 100
@test
def testNewChannel09(): Channel[Array[Int]] = chan Array[Int] 100
@test
def testNewChannel10(): Channel[Channel[Channel[Channel[Str]]]] = chan Channel[Channel[Channel[Str]]] 100
@test
def testNewChannel11(): Channel[Option[Int]] = chan Option[Int] 100
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

def getChannelUnit03(): Unit =
  let ch = chan Unit 100;
  // TODO SJ: Spawning a function doesn't work in the compiler
  let _ = spawn fUnit03(ch);
  <- ch

@test
def testGetChannelUnit03(): Bool = getChannelUnit03() `assertEq!` ()

def fUnit04(ch: Channel[Unit]): Channel[Unit] =
  ch <- ()

def getChannelUnit04(): Unit =
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

def getChannelBool03(): Bool =
  let ch = chan Bool 100;
  let _ = spawn fBool03(ch);
  <- ch

@test
def testGetChannelBool03(): Bool = getChannelBool03() `assertEq!` true

def fBool04(ch: Channel[Bool]): Channel[Bool] =
  ch <- false

def getChannelBool04(): Bool =
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
def getChannelUnit01(): Unit =
  let ch = channel Unit 1;
  let _ = ch <- ();
  <- ch

@test
def testGetChannelUnit01(): Bool = getChannelUnit01() `assertEq!` ()

def getChannelUnit02(): Unit =
  let ch = channel Unit 10;
  let _ = ch <- ();
  <- ch

@test
def testGetChannelUnit02(): Bool = getChannelUnit02() `assertEq!` ()

def fUnit03(ch: Channel[Unit]): Channel[Unit] =
  ch <- ()

def getChannelUnit03(): Unit =
  let ch = channel Unit 1;
  let _ = spawn fUnit03(ch);
  <- ch

@test
def testGetChannelUnit03(): Bool = getChannelUnit03() `assertEq!` ()

def fUnit04(ch: Channel[Unit]): Channel[Unit] =
  ch <- ()

def getChannelUnit04(): Unit =
  let ch = channel Unit 1;
  let _ = spawn fUnit04(ch);
  <- ch

@test
def testGetChannelUnit04(): Bool = getChannelUnit04() `assertEq!` ()

def getChannelBool01(): Bool =
  let ch = channel Bool 1;
  let _ = ch <- true;
  <- ch

@test
def testGetChannelBool01(): Bool = getChannelBool01() `assertEq!` true

def getChannelBool02(): Bool =
  let ch = channel Bool 1;
  let _ = ch <- false;
  <- ch

@test
def testGetChannelBool02(): Bool = getChannelBool02() `assertEq!` false

def fBool03(ch: Channel[Bool]): Channel[Bool] =
  ch <- true

def getChannelBool03(): Bool =
  let ch = channel Bool 1;
  let _ = spawn fBool03(ch);
  <- ch

@test
def testGetChannelBool03(): Bool = getChannelBool03() `assertEq!` true

def fBool04(ch: Channel[Bool]): Channel[Bool] =
  ch <- false

def getChannelBool04(): Bool =
  let ch = channel Bool 1;
  let _ = spawn fBool04(ch);
  <- ch

@test
def testGetChannelBool04(): Bool = getChannelBool04() `assertEq!` false

def getChannelInt01(): Int =
  let ch = channel Int 1;
  let _ = ch <- 2;
  <- ch

@test
def testGetChannelInt01(): Bool = getChannelInt01() `assertEq!` 2

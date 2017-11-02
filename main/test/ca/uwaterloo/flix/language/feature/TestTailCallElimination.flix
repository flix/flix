@test
def testTCE01(): Bool = odd(1)

@test
def testTCE02(): Bool = even(12)

@test
def testTCE03(): Bool = odd(123)

@test
def testTCE04(): Bool = even(1234)

@test
def testTCE05(): Bool = odd(12345)

@test
def testTCE06(): Bool = even(123456)

@test
def testTCE07(): Bool = odd(1234567)

def odd(n: Int): Bool = match n with {
    case 0 => false
    case 1 => true
    case x => even(x - 1)
}

def even(n: Int): Bool = match n with {
    case 0 => true
    case 1 => false
    case x => odd(x - 1)
}

@test
def testTCE08(): Bool = a(42)

def a(x: Int): Bool = b(x + 1)

def b(x: Int): Bool = c(x - 1)

def c(x: Int): Bool = x == 42

@test
def testTCE09(): Bool = a1(42, z -> z + 1)

def a1(x: Int, f: Int -> Int): Bool = b1(f(x), y -> y - 1)

def b1(x: Int, f: Int -> Int): Bool = c1(f(x), y -> y == 42)

def c1(x: Int, f: Int -> Bool): Bool = f(x)
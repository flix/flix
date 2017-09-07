@test
def testPostfix01(): Bool = assertEq!(123.abs(), 123)

@test
def testPostfix02(): Bool = assertEq!((-123).abs(), 123)

@test
def testPostfix03(): Bool = assertEq!(123.inc(), 123 + 1)

@test
def testPostfix04(): Bool = assertEq!(123.dec(), 123 - 1)

@test
def testPostfix05(): Bool = assertEq!(123.inc().inc(), 123 + 2)

@test
def testPostfix06(): Bool = assertEq!(123.dec().dec(), 123 - 2)

@test
def testPostfix07(): Bool = assertEq!(123.inc().dec(), 123)

def abs(x: Int): Int = if(x >= 0) x else -x
def inc(x: Int): Int = x + 1
def dec(x: Int): Int = x - 1

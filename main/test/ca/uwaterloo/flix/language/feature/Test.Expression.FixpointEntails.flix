rel A(x: Int, y: Int)

@test
def testFixpointEntails01(): Bool =
    A(0, 0). |= A(0, 0).

@test
def testFixpointEntails02(): Bool =
    A(0, 1). |= A(0, 1).

@test
def testFixpointEntails03(): Bool =
    A(1, 1). |= A(1, 1).

@test
def testFixpointEntails04(): Bool =
    !(A(0, 0). |= A(0, 1).)

@test
def testFixpointEntails05(): Bool =
    !(A(0, 0). |= A(1, 0).)

@test
def testFixpointEntails06(): Bool =
    !(A(0, 0). |= A(1, 1).)

@test
def testFixpointEntails07(): Bool =
    (A(0, 0). A(0, 0).) |= A(0, 0).

@test
def testFixpointEntails08(): Bool =
    (A(0, 0). A(0, 0). A(0, 0).) |= A(0, 0).

@test
def testFixpointEntails09(): Bool =
    A(0, 0). |= (A(0, 0). A(0, 0).)

@test
def testFixpointEntails10(): Bool =
    A(0, 0). |= (A(0, 0). A(0, 0). A(0, 0).)

@test
def testFixpointEntails11(): Bool =
    (A(0, 0). B(0, 0).) |= A(0, 0).

@test
def testFixpointEntails12(): Bool =
    (B(0, 0). A(0, 0).) |= A(0, 0).

@test
def testFixpointEntails13(): Bool =
    (B(0, 0). A(0, 0). B(1, 1).) |= A(0, 0).

@test
def testFixpointEntails14(): Bool =
    (A(0, 0). B(0, 0).) |= (A(0, 0). B(0, 0).)

@test
def testFixpointEntails15(): Bool =
    (A(0, 0). B(0, 0).) |= (B(0, 0). A(0, 0).)


rel A(x: Int, y: Int, z: Int)
rel B(x: Int, y: Int, z: Int)

@test
def testAtomWithParam01(): Bool =
    A(1, 2, 3). |= A(1, 2, 3).

@test
def testAtomWithParam02(): Bool =
    A<()>(1, 2, 3). |= A(1, 2, 3).

@test
def testAtomWithParam03(): Bool =
    A(1, 2, 3). |= A<()>(1, 2, 3).

@test
def testAtomWithParam04(): Bool =
    A<()>(1, 2, 3). |= A<()>(1, 2, 3).

@test
def testAtomWithParam05(): Bool =
    A<000>(1, 2, 3). |= A<000>(1, 2, 3).

@test
def testAtomWithParam06(): Bool =
    !(A<000>(1, 2, 3). |= A<111>(1, 2, 3).)

@test
def testAtomWithParam07(): Bool =
    !(A<111>(1, 2, 3). |= A<000>(1, 2, 3).)

@test
def testAtomWithParam08(): Bool =
    A<"abc">(1, 2, 3). |= A<"abc">(1, 2, 3).

@test
def testAtomWithParam09(): Bool =
    !(A<"abc">(1, 2, 3). |= A<"def">(1, 2, 3).)




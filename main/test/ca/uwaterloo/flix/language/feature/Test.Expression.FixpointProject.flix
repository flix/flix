rel A(x: Int, y: Int, z: Int)
rel B(x: Int, y: Int, z: Int)

@test
def testProject01(): Bool =
    let c1 = A(1, 1, 1).;
    (project A c1) |= A(1, 1, 1).

@test
def testProject02(): Bool =
    let c1 = A(1, 1, 1).;
    !((project B c1) |= A(1, 1, 1).)

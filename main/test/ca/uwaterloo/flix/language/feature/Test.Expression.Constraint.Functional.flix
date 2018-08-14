@test
def testFunctional01(): Bool = check {
    R(x) :- x <- zer().
}

@test
def testFunctional02(): Bool = check {
    R(x) :- x <- one().
}

@test
def testFunctional03(): Bool = check {
    R(x) :- x <- two().
}

@test
def testFunctional04(): Bool = check {
    A(1). A(2). A(3).

    R(x) :- A(x), y <- zer().
}

@test
def testFunctional05(): Bool = check {
    A(1). A(2). A(3).

    R(x) :- A(x), y <- one().
}

@test
def testFunctional06(): Bool = check {
    R(add(x, y)) :- x <- one(), y <- two().
}

def zer(): Array[Int] = []
def one(): Array[Int] = [21]
def two(): Array[Int] = [21, 42]
def add(x: Int, y: Int): Int = x + y

rel A(x: Int)
rel B(x: Int)
rel R(x: Int)

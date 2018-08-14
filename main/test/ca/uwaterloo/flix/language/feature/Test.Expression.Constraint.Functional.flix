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


def zer(): Array[Int] = []
def one(): Array[Int] = [21]
def two(): Array[Int] = [21, 42]

rel A(x: Int)
rel B(x: Int)
rel R(x: Int)

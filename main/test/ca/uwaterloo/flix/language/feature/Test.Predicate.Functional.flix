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
    A(1). A(2). A(3).

    R(x) :- A(x), y <- two().
}

@test
def testFunctional07(): Bool = check {
    F(x) :- x <- zerOption().
}

@test
def testFunctional08(): Bool = check {
    F(x) :- x <- oneOption().
}

@test
def testFunctional09(): Bool = check {
    F(x) :- x <- twoOption().
}

def zer(): Array[Int] = []
def one(): Array[Int] = [21]
def two(): Array[Int] = [21, 42]

def zerOption(): Array[Option[Int]] = []
def oneOption(): Array[Option[Int]] = [None]
def twoOption(): Array[Option[Int]] = [None, Some(123)]

rel A(x: Int)
rel B(x: Int)
rel R(x: Int)

rel F(x: Option[Int])

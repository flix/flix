// TODO: See testBackend and testSolver

rel A(x: Str)

def isOdd(x: Int): Bool = (x % 2) == 1

@test
def testFilter01(): Bool = check {
    A("a") :- isOdd(0).
    A("b") :- isOdd(1).
    A("c") :- isOdd(2).
}

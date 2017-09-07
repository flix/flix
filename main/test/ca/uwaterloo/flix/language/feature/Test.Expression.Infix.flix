@test
def testInfix01(): Bool = assertEq!(1 `add` 2, 1 + 2)

@test
def testInfix02(): Bool = assertEq!(1 `sub` 2, 1 - 2)

@test
def testInfix03(): Bool = assertEq!(1 `add` 2 `add` 3, (1 + 2) + 3)

@test
def testInfix04(): Bool = assertEq!(1 `add` 2 `sub` 3, (1 + 2) - 3)


def add(x: Int, y: Int): Int = x + y
def sub(x: Int, y: Int): Int = x - y

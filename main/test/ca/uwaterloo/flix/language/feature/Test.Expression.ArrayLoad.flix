@test
def testArrayLoad01(): Int = let x = [| 1 |]; x[0]

@test
def testArrayLoad02(): Int = let x = [| 1, 2 |]; x[0] + x[1]

@test
def testArrayLoad03(): Int = let x = [| 1, 2, 3 |]; x[0] + x[1] + x[2]

@test
def testArrayLoad04(): Int = let x = [| 1, 2, 3, 4, 5, 6, 7, 8, 9 |]; x[0]

@test
def testArrayLoad05(): Int = let x = [| 1, 2, 3, 4, 5, 6, 7, 8, 9 |]; x[8]

@test
def testArrayLoad06(): Int = let x = [| 1, 1, 1, 1, 1, 1, 1, 1, 1 |]; x[0]

@test
def testArrayLoad07(): Int = let x = [| 1, 1, 1, 1, 1, 1, 1, 1, 1 |]; x[8]

@test
def testArrayLoad08(): Bool = let x = [| true |]; x[0]

@test
def testArrayLoad09(): Bool = let x = [| false |]; !x[0]

@test
def testArrayLoad10(): Bool = let x = [| true, false |]; x[0]

@test
def testArrayLoad11(): Bool = let x = [| false, true |]; !x[0]

@test
def testArrayLoad12(): Bool = let x = [| true, true, false, false |]; x[0]

@test
def testArrayLoad13(): Str = let x = [| "hello" |]; x[0]

@test
def testArrayLoad14(): Str = let x = [| "world" |]; x[0]

@test
def testArrayLoad15(): Str = let x = [| "hello", "world" |]; x[0]

@test
def testArrayLoad16(): Str = let x = [| "world", "hello" |]; x[0]

@test
def testArrayLoad17(): Int = let x = [| x -> x + 1, y -> y - 1, z -> z * 2 |]; x[0](42)

@test
def testArrayLoad18(): Array[Int] = let x = [| [| |] |]; x[0]

@test
def testArrayLoad19(): Array[Int] = let x = [| [| |], [| |] |]; x[0]

@test
def testArrayLoad20(): Array[Int] = let x = [| [| |], [| |], [| |] |]; x[0]

@test
def testArrayLoad21(): Array[Int] = let x = [| [| 1 |], [| 2 |], [| 3 |] |]; x[0]

@test
def testArrayLoad22(): Int = let x = [| [| 1 |], [| 2 |], [| 3 |] |]; x[0][0] + x[1][0] + x[2][0]

@test
def testArrayLoad23(): Int = let x = [| [| 1, 2, 3 |], [| 4, 5, 6 |], [| 7, 8, 9 |] |]; x[0][0] + x[1][1] + x[2][2]

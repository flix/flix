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
def testArrayLoad06(): Bool = let x = [| true |]; x[0]

@test
def testArrayLoad07(): Bool = let x = [| true, false |]; x[0]

@test
def testArrayLoad08(): Bool = let x = [| true, false, true, false |]; x[0] || x[1] || x[2] || x[3]

@test
def testArrayLoad09(): Int = let x = [| x -> x + 1, y -> y - 1, z -> z * 2 |]; x[0](42)

@test
def testArrayLoad10(): Int = let x = [| [|1, 2, 3|] |]; x[0][0] + x[0][1] + x[0][2]

@test
def testArrayLoad11(): Int = let x = [| [|1, 2, 3|], [|4, 5, 6|], [|7, 8, 9|] |]; x[0][0] + x[1][1] + x[2][2]

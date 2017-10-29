@test
def testArrayStore01(): Int = let x = [| 1 |]; x[0] := 42

@test
def testArrayStore02(): Int = let x = [| 0, 1, 2 |]; x[0] := 42

@test
def testArrayStore03(): Int = let x = [| 0, 1, 2 |]; x[0] := 21 + 42

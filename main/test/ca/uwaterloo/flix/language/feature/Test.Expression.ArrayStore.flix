// TODO: These tests require syntax support for ArrayStore.

@test
def testArrayStore01(): Unit = let x = [| 1 |]; x[0] := 42

@test
def testArrayStore02(): Unit = let x = [| 1, 2 |]; x[0] := 42

@test
def testArrayStore03(): Unit = let x = [| 1, 2, 3 |]; x[0] := 42

@test
def testArrayStore04(): Unit = let x = [| 1, 2, 3, 4, 5, 6, 7, 8, 9 |]; x[0] := 42

@test
def testArrayStore05(): Unit = let x = [| 1, 2, 3, 4, 5, 6, 7, 8, 9 |]; x[8] := 42

@test
def testArrayStore06(): Unit = let x = [| true |]; x[0] := false

@test
def testArrayStore07(): Unit = let x = [| false |]; x[0] := true

@test
def testArrayStore08(): Unit = let x = [| true, false |]; x[0] := true

@test
def testArrayStore09(): Unit = let x = [| false, true |]; x[0] := true

@test
def testArrayStore10(): Unit = let x = [| "hello" |]; x[0] := "world"

@test
def testArrayStore11(): Unit = let x = [| "world" |]; x[0] := "hello"

@test
def testArrayStore12(): Unit = let x = [| "hello", "world" |]; x[0] := "goodbye world"

@test
def testArrayStore13(): Unit = let x = [| [| |] |]; x[0] := [| 1 |]

@test
def testArrayStore14(): Unit = let x = [| [| 1 |] |]; x[0] := [| 2 |]

@test
def testArrayStore15(): Unit = let x = [| [| 1, 2, 3 |] |]; x[0] := [| 4, 5, 6 |]

@test
def testArrayStore16(): Unit = let x = [| [| |], [| |] |]; x[1] := [| 1, 2, 3 |]

@test
def testArrayStore17(): Unit = let x = [| [| 1, 2, 3 |], [| 4, 5, 6 |] |]; x[1] := [| |]


@test
def testArray01(): Array[Int] = [| |]

@test
def testArray02(): Array[Int] = [| 1 |]

@test
def testArray03(): Array[Int] = [| 1, 2 |]

@test
def testArray04(): Array[Int] = [| 1, 2, 3 |]

@test
def testArray05(): Array[Int] = [| 1, 2, 3, 4, 5, 6, 7, 8, 9 |]

@test
def testArray06(): Array[Int] = [| 1, 1, 1, 1, 1, 1, 1, 1, 1 |]

@test
def testArray07(): Array[Bool] = [| |]

@test
def testArray08(): Array[Bool] = [| true |]

@test
def testArray09(): Array[Bool] = [| false |]

@test
def testArray10(): Array[Bool] = [| true, false |]

@test
def testArray11(): Array[Bool] = [| false, true |]

@test
def testArray12(): Array[Bool] = [| true, true, false, false |]

@test
def testArray13(): Array[Str] = [| |]

@test
def testArray14(): Array[Str] = [| "hello" |]

@test
def testArray15(): Array[Str] = [| "world" |]

@test
def testArray16(): Array[Str] = [| "hello", "world" |]

@test
def testArray17(): Array[Str] = [| "world", "hello" |]

@test
def testArray18(): Array[Array[Int]] = [| |]

@test
def testArray19(): Array[Array[Int]] = [| [| |] |]

@test
def testArray20(): Array[Array[Int]] = [| [| |], [| |] |]

@test
def testArray21(): Array[Array[Int]] = [| [| |], [| |], [| |] |]

@test
def testArray22(): Array[Array[Int]] = [| [| 1 |], [| 2 |], [| 3 |] |]

@test
def testArray23(): Array[Array[Int]] = [| [| 1 |], [| 1, 2 |], [| 1, 2, 3 |] |]

@test
def testArray24(): Array[Array[Int]] = [| [| 1, 2, 3 |], [| 1, 2, 3 |], [| 1, 2, 3 |] |]

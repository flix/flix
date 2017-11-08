@test
def testArrayLit01(): Array[Int] = [| |]

@test
def testArrayLit02(): Array[Int] = [| 1 |]

@test
def testArrayLit03(): Array[Int] = [| 1, 2 |]

@test
def testArrayLit04(): Array[Int] = [| 1, 2, 3 |]

@test
def testArrayLit05(): Array[Int] = [| 1, 2, 3, 4, 5, 6, 7, 8, 9 |]

@test
def testArrayLit06(): Array[Int] = [| 1, 1, 1, 1, 1, 1, 1, 1, 1 |]

@test
def testArrayLit07(): Array[Bool] = [| |]

@test
def testArrayLit08(): Array[Bool] = [| true |]

@test
def testArrayLit09(): Array[Bool] = [| false |]

@test
def testArrayLit10(): Array[Bool] = [| true, false |]

@test
def testArrayLit11(): Array[Bool] = [| false, true |]

@test
def testArrayLit12(): Array[Bool] = [| true, true, false, false |]

@test
def testArrayLit13(): Array[Str] = [| |]

@test
def testArrayLit14(): Array[Str] = [| "hello" |]

@test
def testArrayLit15(): Array[Str] = [| "world" |]

@test
def testArrayLit16(): Array[Str] = [| "hello", "world" |]

@test
def testArrayLit17(): Array[Str] = [| "world", "hello" |]

@test
def testArrayLit18(): Array[Array[Int]] = [| |]

@test
def testArrayLit19(): Array[Array[Int]] = [| [| |] |]

@test
def testArrayLit20(): Array[Array[Int]] = [| [| |], [| |] |]

@test
def testArrayLit21(): Array[Array[Int]] = [| [| |], [| |], [| |] |]

@test
def testArrayLit22(): Array[Array[Int]] = [| [| 1 |], [| 2 |], [| 3 |] |]

@test
def testArrayLit23(): Array[Array[Int]] = [| [| 1 |], [| 1, 2 |], [| 1, 2, 3 |] |]

@test
def testArrayLit24(): Array[Array[Int]] = [| [| 1, 2, 3 |], [| 1, 2, 3 |], [| 1, 2, 3 |] |]

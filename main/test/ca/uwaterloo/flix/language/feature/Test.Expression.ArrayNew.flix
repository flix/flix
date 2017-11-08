@test
def testArrayNew01(): Array[Int] = [| 42; 0 |]

@test
def testArrayNew02(): Array[Int] = [| 42; 1 |]

@test
def testArrayNew03(): Array[Int] = [| 42; 2 |]

@test
def testArrayNew04(): Array[Int] = [| 42; 3 |]

@test
def testArrayNew05(): Array[Int] = [| 42; 500 |]

@test
def testArrayNew06(): Array[Bool] = [| true; 42 |]

@test
def testArrayNew07(): Array[Bool] = [| false; 42 |]

@test
def testArrayNew08(): Array[Str] = [| "Hello World"; 42 |]

@test
def testArrayNew09(): Array[Array[Str]] = [| [| "Hello World"; 21 |]; 42 |]

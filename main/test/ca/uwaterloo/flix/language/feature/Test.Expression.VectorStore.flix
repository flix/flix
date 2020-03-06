@test
def testVectorStore01(): Unit & Impure = let x = [|1|]; x[|0|] = 1

@test
def testVectorStore02(): Unit & Impure = let x = [|1; 1|]; x[|0|] = 2

@test
def testVectorStore03(): Unit & Impure = let x = [|1; 2|]; x[|1|] = 42

@test
def testVectorStore04(): Unit & Impure = let x = [|1; 3|]; x[|2|] = -84

@test
def testVectorStore05(): Unit & Impure = let x = [|1; 4|]; x[|3|] = -1000

@test
def testVectorStore06(): Unit & Impure = let x = [|1; 20|]; x[|19|] = 1000000

@test
def testVectorStore07(): Unit & Impure = let x = [|true|]; x[|0|] = false

@test
def testVectorStore08(): Unit & Impure = let x = [|false; 1|]; x[|0|] = true

@test
def testVectorStore09(): Unit & Impure = let x = [|true; 2|]; x[|1|] = false

@test
def testVectorStore10(): Unit & Impure = let x = [|false; 3|]; x[|2|] = true

@test
def testVectorStore11(): Unit & Impure = let x = [|true; 5|]; x[|3|] = false

@test
def testVectorStore12(): Unit & Impure = let x = [|'c'; 1|]; x[|0|] = 'j'

@test
def testVectorStore13(): Unit & Impure = let x = [|'a'; 2|]; x[|1|] = '6'

@test
def testVectorStore14(): Unit & Impure = let x = [|'c'; 3|]; x[|2|] = '!'

@test
def testVectorStore15(): Unit & Impure = let x = [|'a'; 20|]; x[|19|] = ' '

@test
def testVectorStore16(): Unit & Impure = let x = [|1i8; 1|]; x[|0|] = 11i8

@test
def testVectorStore17(): Unit & Impure = let x = [|1i8; 3|]; x [|2|] = 0i8

@test
def testVectorStore18(): Unit & Impure = let x = [|4i8; 4|]; x[|3|] = 21i8

@test
def testVectorStore19(): Unit  & Impure = let x = [|1i16; 1|]; x[|0|] = 01i16

@test
def testVectorStore20(): Unit & Impure = let x = [|3i16; 3|]; x [|1|] = 201i16

@test
def testVectorStore21(): Unit & Impure = let x = [|4i16; 4|]; x[|2|] = 0i16

@test
def testVectorStore22(): Unit & Impure = let x = [|1i64; 1|]; x[|0|] = 5i64

@test
def testVectorStore23(): Unit & Impure = let x = [|3i64; 3|]; x[|1|] = 100i64

@test
def testVectorStore24(): Unit & Impure = let x = [|4i64; 4|]; x[|3|] = 5i64

@test
def testVectorStore25(): Unit & Impure = let x = [|1.0f32; 1|]; x[|0|] = 1.1f32

@test
def testVectorStore26(): Unit & Impure = let x = [|3.0f32; 3|]; x[|2|] = 0.9f32

@test
def testVectorStore27(): Unit & Impure = let x = [|3.0f32; 4|]; x[|0|] = 9.99f32

@test
def testVectorStore28(): Unit & Impure = let x = [|1.0f64; 1|]; x[|0|] = 10.01f64

@test
def testVectorStore29(): Unit & Impure = let x = [|3.0f64; 3|]; x[|2|] = 0.30f64

@test
def testVectorStore30(): Unit & Impure = let x = [|4.0f64; 4|]; x[|3|] = 5.5f64

@test
def testVectorStore31(): Unit & Impure = let x = [|"Hello"; 1|]; x[|0|] = "World"

@test
def testVectorStore32(): Unit & Impure = let x = [|"World"; 2|]; x[|1|] = "world!"

@test
def testVectorStore33(): Unit & Impure = let x = [|"World"; 3|]; x[|2|] = "42"

@test
def testVectorStore34(): Unit & Impure = let x = [|[|1, 2, 3|]; 1|]; x[|0|] = [|3, 2, 1|]

@test
def testVectorStore35(): Unit & Impure = let x = [|[|1, 2|]; 2|]; x[|1|] = [|2, 1|]

@test
def testVectorStore36(): Unit & Impure = let x = [|[|1, 2, 3|]; 3|]; x[|2|] = [|3; 3|]

@test
def testVectorStore37(): Unit & Impure = let x = [|[|2; 2|]; 2|]; x[|1|] = [|1, 1|]

@test
def testVectorStore38(): Unit & Impure = let x = [| [ 1, 2, 3 ], [ 1, 2, 3] |]; x[|1|][1] = 42

@test
def testVectorStore39(): Unit & Impure = let x = [|[7 :: 10 :: Nil], [7 :: Nil]|]; x[|1|] = [19 :: Nil]

@test
def testVectorStore40(): Unit & Impure = let x = [| [(1, 2)], [(3, 4)] |]; x[|1|] = [(19, 10)]

@test
def testVectorStore41(): Unit & Impure = let x = [|[None, Some(5)], [Some(55), Some(42)]|]; x[|1|] = [None, None]

@test
def testVectorStore42(): Unit & Impure = let x = [|[|1, 2, 3|], [|1, 2, 3|], [|1, 2, 3|]|]; x[|2|][|2|] = 42

@test
def testVectorStore43(): Vector[(Int, Int), 2] & Impure = let x = [|(1, 2), (3, 4)|]; let _ = x[|0|] = (2, 3); x

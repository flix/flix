@test
def testVectorstore01(): Unit = let x = [|1|]; x[|0|] = 1

@test
def testVectorstore02(): Unit = let x = [|1; 1|]; x[|0|] = 2

@test
def testVectorstore03(): Unit = let x = [|1; 2|]; x[|1|] = 42

@test
def testVectorstore04(): Unit = let x = [|1; 3|]; x[|2|] = -84

@test
def testVectorstore05(): Unit = let x = [|1; 4|]; x[|3|] = -1000

@test
def testVectorstore06(): Unit = let x = [|1; 20|]; x[|19|] = 1000000

@test
def testVectorstore07(): Unit = let x = [|true|]; x[|0|] = false

@test
def testVectorstore08(): Unit = let x = [|false; 1|]; x[|0|] = true

@test
def testVectorstore09(): Unit = let x = [|true; 2|]; x[|1|] = false

@test
def testVectorstore10(): Unit = let x = [|false; 3|]; x[|2|] = true

@test
def testVectorstore11(): Unit = let x = [|true; 5|]; x[|3|] = false

@test
def testVectorstore12(): Unit = let x = [|'c'; 1|]; x[|0|] = 'j'

@test
def testVectorstore13(): Unit = let x = [|'a'; 2|]; x[|1|] = '6'

@test
def testVectorstore14(): Unit = let x = [|'c'; 3|]; x[|2|] = '!'

@test
def testVectorstore15(): Unit = let x = [|'a'; 20|]; x[|19|] = ' '

@test
def testVectorstore16(): Unit = let x = [|1i8; 1|]; x[|0|] = 11i8

@test
def testVectorstore17(): Unit = let x = [|1i8; 3|]; x [|2|] = 0i8

@test
def testVectorstore18(): Unit = let x = [|4i8; 4|]; x[|3|] = 21i8

@test
def testVectorstore19(): Unit  = let x = [|1i16; 1|]; x[|0|] = 01i16

@test
def testVectorstore20(): Unit = let x = [|3i16; 3|]; x [|1|] = 201i16

@test
def testVectorstore21(): Unit = let x = [|4i16; 4|]; x[|2|] = 0i16

@test
def testVectorstore22(): Unit = let x = [|1i64; 1|]; x[|0|] = 5i64

@test
def testVectorstore23(): Unit = let x = [|3i64; 3|]; x[|1|] = 100i64

@test
def testVectorstore24(): Unit = let x = [|4i64; 4|]; x[|3|] = 5i64

@test
def testVectorstore25(): Unit = let x = [|1.0f32; 1|]; x[|0|] = 1.1f32

@test
def testVectorstore26(): Unit = let x = [|3.0f32; 3|]; x[|2|] = 0.9f32

@test
def testVectorstore27(): Unit = let x = [|3.0f32; 4|]; x[|0|] = 9.99f32

@test
def testVectorstore28(): Unit = let x = [|1.0f64; 1|]; x[|0|] = 10.01f64

@test
def testVectorstore29(): Unit = let x = [|3.0f64; 3|]; x[|2|] = 0.30f64

@test
def testVectorstore30(): Unit = let x = [|4.0f64; 4|]; x[|3|] = 5.5f64

@test
def testVectorstore31(): Unit = let x = [|"Hello"; 1|]; x[|0|] = "World"

@test
def testVectorstore32(): Unit = let x = [|"World"; 2|]; x[|1|] = "world!"

@test
def testVectorstore33(): Unit = let x = [|"World"; 3|]; x[|2|] = "42"

@test
def testVectorstore34(): Unit = let x = [|[|1, 2, 3|]; 1|]; x[|0|] = [|3, 2, 1|]

@test
def testVectorstore35(): Unit = let x = [|[|1, 2|]; 2|]; x[|1|] = [|2, 1|]

@test
def testVectorstore36(): Unit = let x = [|[|1, 2, 3|]; 3|]; x[|2|] = [|3; 3|]

@test
def testVectorstore37(): Unit = let x = [|[|2; 2|]; 2|]; x[|1|] = [|1, 1|]

@test
def testVectorstore38(): Unit = let x = [| [ 1, 2, 3 ], [ 1, 2, 3] |]; x[|1|][1] = 42

@test
def testVectorstore39(): Unit = let x = [|[7 :: 10 :: Nil], [7 :: Nil]|]; x[|1|] = [19 :: Nil]

@test
def testVectorstore40(): Unit = let x = [| [(1, 2)], [(3, 4)] |]; x[|1|] = [(19, 10)]

@test
def testVectorstore41(): Unit = let x = [|[None, Some(5)], [Some(55), Some(42)]|]; x[|1|] = [None, None]

@test
def testVectorstore42(): Unit = let x = [|[|1, 2, 3|], [|1, 2, 3|], [|1, 2, 3|]|]; x[|2|][|2|] = 42

@test
def testVectorstore43(): Vector[(Int, Int), 2] = let x = [|(1, 2), (3, 4)|]; let _ = x[|0|] = (2, 3); x
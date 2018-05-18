@test
def testArrayStore01(): Unit = let x = [1]; x[0] = 42

@test
def testArrayStore02(): Unit = let x = [1, 2, 3]; x[2] = 42

@test
def testArrayStore03(): Unit = let x = [1, 2, 3, 4]; x[2] = 42

@test
def testArrayStore04(): Unit = let x = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]; x[10] = 42

@test
def testArrayStore05(): Unit = let x = [1, 2]; x[1] = 42

@test
def testArrayStore06(): Unit = let x = [true]; x[0] = false

@test
def testArrayStore07(): Unit = let x = [false]; x[0] = true

@test
def testArrayStore08(): Unit = let x = [true, false]; x[1] = true

@test
def testArrayStore9(): Unit = let x = [false, true, false]; x[2] = false

@test
def testArrayStore10(): Unit = let x = [false, false, false, false, true]; x[3] = true

@test
def testArrayStore11(): Unit = let x = ['c']; x[0] = 'e'

@test
def testArrayStore12(): Unit = let x = ['a', 'b']; x[0] = 'e'

@test
def testArrayStore13(): Unit = let x = ['a', 'b', 'c']; x[1] = 'e'

@test
def testArrayStore14(): Unit = let x = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n']; x[9] = 'e'

@test
def testArrayStore15(): Unit = let x = [1i8]; x[0] = 42i8

@test
def testArrayStore16(): Unit = let x = [1i8, 2i8, 3i8]; x[1] = 42i8

@test
def testArrayStore17(): Unit = let x = [1i8, 2i8, 3i8, 4i8]; x[2] = 42i8

@test
def testArrayStore18(): Unit = let x = [1i16]; x[0] = 42i16

@test
def testArrayStore19(): Unit = let x = [1i16, 2i16, 3i16]; x[1] = 42i16

@test
def testArrayStore20(): Unit = let x = [1i16, 2i16, 3i16, 4i16]; x[2] = 42i16

@test
def testArrayStore21(): Unit = let x = [1i64]; x[0] = 42i64

@test
def testArrayStore22(): Unit = let x = [1i64, 2i64, 3i64]; x[1] = 42i64

@test
def testArrayStore23(): Unit = let x = [1i64, 2i64, 3i64, 4i64]; x[2] = 42i64

@test
def testArrayStore24(): Unit = let x = [1.0f32]; x[0] = 42.0f32

@test
def testArrayStore25(): Unit = let x = [1.0f32, 2.0f32, 3.0f32]; x[1] = 42.0f32

@test
def testArrayStore26(): Unit = let x = [1.0f32, 2.0f32, 3.0f32, 4.0f32]; x[2] = 42.0f32

@test
def testArrayStore27(): Unit = let x = [1.0f64]; x[0] = 42.0f64

@test
def testArrayStore28(): Unit = let x = [1.0f64, 2.0f64, 3.0f64]; x[1] = 42.0f64

@test
def testArrayStore29(): Unit = let x = [1.0f64, 2.0f64, 3.0f64, 4.0f64]; x[3] = 42.0f64

@test
def testArrayStore30(): Unit = let x = ["Hello"]; x[0] = "Test"

@test
def testArrayStore31(): Unit = let x = ["Hello", "World"]; x[1] = "Test"

@test
def testArrayStore32(): Unit = let x = ["World", "Hello", "World"]; x[2] = "Test"

@test
def testArrayStore33(): Unit = let x = [[1, 2, 3]]; x[0][1] = 42

@test
def testArrayStore34(): Unit = let x = [[1, 2], [1, 2]]; x[1][0] = 42

@test
def testArrayStore35(): Unit = let x = [[1], [1, 2, 3, 4, 5, 6, 7, 8]]; x[1][5] = 42

@test
def testArrayStore36(): Unit = let x = [[1, 2, 3, 4, 5], [2]]; x[0][3] = 42

@test
def testArrayStore37(): Unit = let x = [[1, 2, 3], [1, 2, 3], [1, 2, 3]]; x[2][2] = 42

@test
def testArrayStore38(): Unit = let x = [[|7 ; 10|], [|7 ; 10|]]; x[1] = [|19 ; 10|]

@test
def testArrayStore39(): Unit = let x = [[(7 , 10)], [(7 , 10)]]; x[1] = [(19 , 10)]

@test
def testArrayStore40(): Unit = let x = [[7 :: 10 :: Nil], [7 :: Nil]]; x[1] = [19 :: Nil]

@test
def testArrayStore41(): Unit = let x = [None, None, None, Some(4)]; x[1] = Some(6)
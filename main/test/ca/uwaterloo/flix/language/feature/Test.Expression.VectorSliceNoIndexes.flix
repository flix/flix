@test
def testVectorSliceNoIndexes01(): Vector[Int, 3] = let x = [|1, 2, 3|]; x[|..|]

@test
def testVectorSliceNoIndexes02(): Vector[Int, 5] = let x = [|1, 2, 3, 4, 5|]; x[|..|]

@test
def testVectorSliceNoIndexes03(): Vector[Int, 20] = let x = [|1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20|]; x[|..|]

@test
def testVectorSliceNoIndexes04(): Vector[Int, 1] = let x = [|1|]; x[|..|]

@test
def testVectorSliceNoIndexes05(): Vector[Bool, 2] = let x = [|true, false|]; x[|..|]

@test
def testVectorSliceNoIndexes06(): Vector[Bool, 3] = let x = [|false, true, false|]; x[|..|]

@test
def testVectorSliceNoIndexes07(): Vector[Bool, 5] = let x = [|false, false, false, false, true|]; x[|..|]

@test
def testVectorSliceNoIndexes08(): Vector[Char, 2] = let x = [|'a', 'b'|]; x[|..|]

@test
def testVectorSliceNoIndexes09(): Vector[Char, 3] = let x = [|'a', 'b', 'c'|]; x[|..|]

@test
def testVectorSliceNoIndexes10(): Vector[Char, 14] = let x = [|'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n'|]; x[|..|]

@test
def testVectorSliceNoIndexes11(): Vector[Int8, 3] = let x = [|1i8, 2i8, 3i8|]; x[|..|]

@test
def testVectorSliceNoIndexes12(): Vector[Int8, 4] = let x = [|1i8, 2i8, 3i8, 4i8|]; x[|..|]

@test
def testVectorSliceNoIndexes13(): Vector[Int16, 3] = let x = [|1i16, 2i16, 3i16|]; x[|..|]

@test
def testVectorSliceNoIndexes14(): Vector[Int16, 4] = let x = [|1i16, 2i16, 3i16, 4i16|]; x[|..|]

@test
def testVectorSliceNoIndexes15(): Vector[Int64, 3] = let x = [|1i64, 2i64, 3i64|]; x[|..|]

@test
def testVectorSliceNoIndexes16(): Vector[Int64, 1] = let x = [|1i64|]; x[|..|]

@test
def testVectorSliceNoIndexes17(): Vector[Float32, 3] = let x = [|1.0f32, 2.0f32, 3.0f32|]; x[|..|]

@test
def testVectorSliceNoIndexes18(): Vector[Float32, 4] = let x = [|1.0f32, 2.0f32, 3.0f32, 4.0f32|]; x[|..|]

@test
def testVectorSliceNoIndexes19(): Vector[Float64, 3] = let x = [|1.0f64, 2.0f64, 3.0f64|]; x[|..|]

@test
def testVectorSliceNoIndexes20(): Vector[Float64, 4] = let x = [|1.0f64, 2.0f64, 3.0f64, 4.0f64|]; x[|..|]

@test
def testVectorSliceNoIndexes21(): Vector[Str, 2] = let x = [|"Hello", "World"|]; x[|..|]

@test
def testVectorSliceNoIndexes22(): Vector[Str, 3] = let x = [|"World", "Hello", "World"|]; x[|..|]

@test
def testVectorSliceNoIndexes23(): Vector[Vector[Int, 2], 2] = let x = [|[|1, 2|], [|1, 2|]|]; x[|..|]

@test
def testVectorSliceNoIndexes24(): Vector[Vector[Int, 3], 2] = let x = [|[|1, 2, 3|], [|1, 2, 3|]|]; x[|..|]

@test
def testVectorSliceNoIndexes25(): Vector[Vector[Int, 1], 2] = let x = [|[|1|], [|2|]|]; x[|..|]

@test
def testVectorSliceNoIndexes26(): Vector[Vector[Int, 3], 3] = let x = [|[|1, 2, 3|], [|1, 2, 3|], [|1, 2, 3|]|]; x[|..|]

@test
def testVectorSliceNoIndexes27(): Vector[Int, 10] = let x = [|1, 2, 3, 4, 5, 6, 7, 8, 9, 10|]; x[|..|]

@test
def testVectorSliceNoIndexes28(): Vector[Array[Int], 3] = let x = [|[2, 1], [2, 1], [2, 1]|]; x[|..|]

@test
def testVectorSliceNoIndexes29(): Vector[List[(Int)], 3] = let x = [|(1 :: Nil), (2 :: Nil), (3 :: Nil)|]; x[|..|]

@test
def testVectorSliceNoIndexes30(): Vector[(Int, Int), 3] = let x = [|(1, 2), (3, 4), (5, 6)|]; x[|..|]

@test
def testVectorSliceNoIndexes31(): Vector[Option[Int], 3] = let x = [|Some(65), None, None|]; x[|..|]
@test
def testVectorSlice01(): Vector[Int, 2] = let x = [|1, 2, 3|]; x[|0..2|]

@test
def testVectorSlice02(): Vector[Int, 1] = let x = [|1, 2, 3, 4, 5|]; x[|2..3|]

@test
def testVectorSlice03(): Vector[Int, 3] = let x = [|1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20|]; x[|15..18|]

@test
def testVectorSlice04(): Vector[Int, 1] = let x = [|1, 2|]; x[|1..2|]

@test
def testVectorSlice05(): Vector[Bool, 1] = let x = [|true, false|]; x[|0..1|]

@test
def testVectorSlice06(): Vector[Bool, 1] = let x = [|false, true, false|]; x[|2..3|]

@test
def testVectorSlice07(): Vector[Bool, 2] = let x = [|false, false, false, false, true|]; x[|1..3|]

@test
def testVectorSlice08(): Vector[Char, 1] = let x = [|'a', 'b'|]; x[|0..1|]

@test
def testVectorSlice09(): Vector[Char, 2] = let x = [|'a', 'b', 'c'|]; x[|0..2|]

@test
def testVectorSlice10(): Vector[Char, 4] = let x = [|'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n'|]; x[|2..6|]

@test
def testVectorSlice11(): Vector[Int8, 1] = let x = [|1i8, 2i8, 3i8|]; x[|1..2|]

@test
def testVectorSlice12(): Vector[Int8, 2] = let x = [|1i8, 2i8, 3i8, 4i8|]; x[|2..4|]

@test
def testVectorSlice13(): Vector[Int16, 1] = let x = [|1i16, 2i16, 3i16|]; x[|1..2|]

@test
def testVectorSlice14(): Vector[Int16, 2] = let x = [|1i16, 2i16, 3i16, 4i16|]; x[|2..4|]

@test
def testVectorSlice15(): Vector[Int64, 1] = let x = [|1i64, 2i64, 3i64|]; x[|1..2|]

@test
def testVectorSlice16(): Vector[Int64, 2] = let x = [|1i64, 2i64, 3i64, 4i64|]; x[|2..4|]

@test
def testVectorSlice17(): Vector[Float32, 1] = let x = [|1.0f32, 2.0f32, 3.0f32|]; x[|1..2|]

@test
def testVectorSlice18(): Vector[Float32, 2] = let x = [|1.0f32, 2.0f32, 3.0f32, 4.0f32|]; x[|2..4|]

@test
def testVectorSlice19(): Vector[Float64, 1] = let x = [|1.0f64, 2.0f64, 3.0f64|]; x[|1..2|]

@test
def testVectorSlice20(): Vector[Float64, 1] = let x = [|1.0f64, 2.0f64, 3.0f64, 4.0f64|]; x[|3..4|]

@test
def testVectorSlice21(): Vector[Str, 1] = let x = [|"Hello", "World"|]; x[|1..2|]

@test
def testVectorSlice22(): Vector[Str, 3] = let x = [|"World", "Hello", "World"|]; x[|0..3|]

@test
def testVectorSlice23(): Vector[Vector[Int, 2], 1] = let x = [|[|1, 2|], [|1, 2|]|]; x[|1..2|]

@test
def testVectorSlice24(): Vector[Vector[Int, 3], 2] = let x = [|[|1, 2, 3|], [|1, 2, 3|]|]; x[|0..2|]

@test
def testVectorSlice25(): Vector[Vector[Int, 1], 1] = let x = [|[|1|], [|2|]|]; x[|0..1|]

@test
def testVectorSlice26(): Vector[Vector[Int, 3], 2] = let x = [|[|1, 2, 3|], [|1, 2, 3|], [|1, 2, 3|]|]; x[|0..2|]

@test
def testVectorSlice27(): Vector[Int, 10] = let x = [|1, 2, 3, 4, 5, 6, 7, 8, 9, 10|]; x[|0..10|]

@test
def testVectorSlice28(): Vector[Array[Int], 2] = let x = [|[2, 1], [2, 1], [2, 1]|]; x[|1..3|]

@test
def testVectorSlice29(): Vector[List[(Int)], 2] = let x = [|(1 :: Nil), (2 :: Nil), (3 :: Nil)|]; x[|1..3|]

@test
def testVectorSlice30(): Vector[(Int, Int), 2] = let x = [|(1, 2), (3, 4), (5, 6)|]; x[|1..3|]

@test
def testVectorSlice31(): Vector[Option[Int], 2] = let x = [|Some(65), None, None|]; x[|1..3|]



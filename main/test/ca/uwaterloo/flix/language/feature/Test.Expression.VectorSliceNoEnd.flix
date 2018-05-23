@test
def testVectorSliceNoEnd01(): Vector[Int, 3] = let x = [|1, 2, 3|]; x[|0..|]

@test
def testVectorSliceNoEnd02(): Vector[Int, 3] = let x = [|1, 2, 3, 4, 5|]; x[|2..|]

@test
def testVectorSliceNoEnd03(): Vector[Int, 5] = let x = [|1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20|]; x[|15..|]

@test
def testVectorSliceNoEnd04(): Vector[Int, 1] = let x = [|1, 2|]; x[|1..|]

@test
def testVectorSliceNoEnd05(): Vector[Bool, 2] = let x = [|true, false|]; x[|0..|]

@test
def testVectorSliceNoEnd06(): Vector[Bool, 2] = let x = [|false, true, false|]; x[|1..|]

@test
def testVectorSliceNoEnd07(): Vector[Bool, 4] = let x = [|false, false, false, false, true|]; x[|1..|]

@test
def testVectorSliceNoEnd08(): Vector[Char, 2] = let x = [|'a', 'b'|]; x[|0..|]

@test
def testVectorSliceNoEnd09(): Vector[Char, 3] = let x = [|'a', 'b', 'c'|]; x[|0..|]

@test
def testVectorSliceNoEnd10(): Vector[Char, 12] = let x = [|'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n'|]; x[|2..|]

@test
def testVectorSliceNoEnd11(): Vector[Int8, 2] = let x = [|1i8, 2i8, 3i8|]; x[|1..|]

@test
def testVectorSliceNoEnd12(): Vector[Int8, 3] = let x = [|1i8, 2i8, 3i8, 4i8|]; x[|1..|]

@test
def testVectorSliceNoEnd13(): Vector[Int16, 2] = let x = [|1i16, 2i16, 3i16|]; x[|1..|]

@test
def testVectorSliceNoEnd14(): Vector[Int16, 2] = let x = [|1i16, 2i16, 3i16, 4i16|]; x[|2..|]

@test
def testVectorSliceNoEnd15(): Vector[Int64, 2] = let x = [|1i64, 2i64, 3i64|]; x[|1..|]

@test
def testVectorSliceNoEnd16(): Vector[Int64, 2] = let x = [|1i64, 2i64, 3i64, 4i64|]; x[|2..|]

@test
def testVectorSliceNoEnd17(): Vector[Float32, 2] = let x = [|1.0f32, 2.0f32, 3.0f32|]; x[|1..|]

@test
def testVectorSliceNoEnd18(): Vector[Float32, 2] = let x = [|1.0f32, 2.0f32, 3.0f32, 4.0f32|]; x[|2..|]

@test
def testVectorSliceNoEnd19(): Vector[Float64, 2] = let x = [|1.0f64, 2.0f64, 3.0f64|]; x[|1..|]

@test
def testVectorSliceNoEnd20(): Vector[Float64, 1] = let x = [|1.0f64, 2.0f64, 3.0f64, 4.0f64|]; x[|3..|]

@test
def testVectorSliceNoEnd21(): Vector[Str, 1] = let x = [|"Hello", "World"|]; x[|1..|]

@test
def testVectorSliceNoEnd22(): Vector[Str, 1] = let x = [|"World", "Hello", "World"|]; x[|2..|]

@test
def testVectorSliceNoEnd23(): Vector[Vector[Int, 2], 1] = let x = [|[|1, 2|], [|1, 2|]|]; x[|1..|]

@test
def testVectorSliceNoEnd24(): Vector[Vector[Int, 3], 2] = let x = [|[|1, 2, 3|], [|1, 2, 3|]|]; x[|0..|]

@test
def testVectorSliceNoEnd25(): Vector[Vector[Int, 1], 2] = let x = [|[|1|], [|2|]|]; x[|0..|]

@test
def testVectorSliceNoEnd26(): Vector[Vector[Int, 3], 3] = let x = [|[|1, 2, 3|], [|1, 2, 3|], [|1, 2, 3|]|]; x[|0..|]

@test
def testVectorSliceNoEnd27(): Vector[Int, 10] = let x = [|1, 2, 3, 4, 5, 6, 7, 8, 9, 10|]; x[|0..|]

@test
def testVectorSliceNoEnd28(): Vector[Array[Int], 3] = let x = [|[2, 1], [2, 1], [2, 1]|]; x[|0..|]

@test
def testVectorSliceNoEnd29(): Vector[List[(Int)], 3] = let x = [|(1 :: Nil), (2 :: Nil), (3 :: Nil)|]; x[|0..|]

@test
def testVectorSliceNoEnd30(): Vector[(Int, Int), 3] = let x = [|(1, 2), (3, 4), (5, 6)|]; x[|0..|]

@test
def testVectorSliceNoEnd31(): Vector[Option[Int], 3] = let x = [|Some(42), Some(6), Some(5)|]; x[|0..|]
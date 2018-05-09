@test
def testVectorSliceNoStart01(): Vector[Int, 2] = let x = [|1, 2, 3|]; x[|..2|]

@test
def testVectorSliceNoStart02(): Vector[Int, 3] = let x = [|1, 2, 3, 4, 5|]; x[|..3|]

@test
def testVectorSliceNoStart03(): Vector[Int, 18] = let x = [|1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20|]; x[|..18|]

@test
def testVectorSliceNoStart04(): Vector[Int, 2] = let x = [|1, 2|]; x[|..2|]

@test
def testVectorSliceNoStart05(): Vector[Bool, 1] = let x = [|true, false|]; x[|..1|]

@test
def testVectorSliceNoStart06(): Vector[Bool, 3] = let x = [|false, true, false|]; x[|..3|]

@test
def testVectorSliceNoStart07(): Vector[Bool, 3] = let x = [|false, false, false, false, true|]; x[|..3|]

@test
def testVectorSliceNoStart08(): Vector[Char, 1] = let x = [|'a', 'b'|]; x[|..1|]

@test
def testVectorSliceNoStart09(): Vector[Char, 2] = let x = [|'a', 'b', 'c'|]; x[|..2|]

@test
def testVectorSliceNoStart10(): Vector[Char, 6] = let x = [|'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n'|]; x[|..6|]

@test
def testVectorSliceNoStart11(): Vector[Int8, 2] = let x = [|1i8, 2i8, 3i8|]; x[|..2|]

@test
def testVectorSliceNoStart12(): Vector[Int8, 4] = let x = [|1i8, 2i8, 3i8, 4i8|]; x[|..4|]

@test
def testVectorSliceNoStart13(): Vector[Int16, 2] = let x = [|1i16, 2i16, 3i16|]; x[|..2|]

@test
def testVectorSliceNoStart14(): Vector[Int16, 4] = let x = [|1i16, 2i16, 3i16, 4i16|]; x[|..4|]

@test
def testVectorSliceNoStart15(): Vector[Int64, 2] = let x = [|1i64, 2i64, 3i64|]; x[|..2|]

@test
def testVectorSliceNoStart16(): Vector[Int64, 4] = let x = [|1i64, 2i64, 3i64, 4i64|]; x[|..4|]

@test
def testVectorSliceNoStart17(): Vector[Float32, 2] = let x = [|1.0f32, 2.0f32, 3.0f32|]; x[|..2|]

@test
def testVectorSliceNoStart18(): Vector[Float32, 4] = let x = [|1.0f32, 2.0f32, 3.0f32, 4.0f32|]; x[|..4|]

@test
def testVectorSliceNoStart19(): Vector[Float64, 2] = let x = [|1.0f64, 2.0f64, 3.0f64|]; x[|..2|]

@test
def testVectorSliceNoStart20(): Vector[Float64, 4] = let x = [|1.0f64, 2.0f64, 3.0f64, 4.0f64|]; x[|..4|]

@test
def testVectorSliceNoStart21(): Vector[Str, 2] = let x = [|"Hello", "World"|]; x[|..2|]

@test
def testVectorSliceNoStart22(): Vector[Str, 3] = let x = [|"World", "Hello", "World"|]; x[|..3|]

@test
def testVectorSliceNoStart23(): Vector[Vector[Int, 2], 2] = let x = [|[|1, 2|], [|1, 2|]|]; x[|..2|]

@test
def testVectorSliceNoStart24(): Vector[Vector[Int, 3], 2] = let x = [|[|1, 2, 3|], [|1, 2, 3|]|]; x[|..2|]

@test
def testVectorSliceNoStart25(): Vector[Vector[Int, 1], 1] = let x = [|[|1|], [|2|]|]; x[|..1|]

@test
def testVectorSliceNoStart26(): Vector[Vector[Int, 3], 3] = let x = [|[|1, 2, 3|], [|1, 2, 3|], [|1, 2, 3|]|]; x[|..3|]

@test
def testVectorSliceNoStart27(): Vector[Int, 10] = let x = [|1, 2, 3, 4, 5, 6, 7, 8, 9, 10|]; x[|..10|]


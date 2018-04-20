@test
def testVectorSlice01(): Vector[Int, 3] = let x [|1,2,3|]; x[|0..2|]

@test
def testVectorSlice02(): Vector[Int, 5] = let x [|1,2,3,4,5|]; x[|2..3|]

@test
def testVectorSlice03(): Vector[Int, 20] = let x [|1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20|]; x[|15..18|]

@test
def testArraySlice04(): Vector[Int, 2] = let x = [|1,2|]; x[|1..2|]

@test
def testArraySlice05(): Vector[Bool, 2] = let x = [true, false]; x[|0..1|]

@test
def testArraySlice06(): Vector[Bool, 3] = let x = [false, true, false]; x[|2..4|]

@test
def testArraySlice07(): Vector[Bool, 5] = let x = [|false, false, false, false, true|]; x[|1..3|]

@test
def testArraySlice08(): Vector[Char, 2] = let x = [|'a', 'b'|]; x[|0..1|]

@test
def testArraySlice09(): Vector[Char, 3] = let x = [|'a', 'b', 'c'|]; x[|0..2|]

@test
def testArraySlice10(): Vector[Char, 14] = let x = [|'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n'|]; x[|2..6|]

@test
def testArraySlice11(): Vector[Int8, 3] = let x = [|1i8, 2i8, 3i8|]; x[|1..2|]

@test
def testArraySlice12(): Vector[Int8, 4] = let x = [|1i8,2i8,3i8,4i8|]; x[|2..4|]

@test
def testArraySlice13(): [Int16, 3] = let x = [|1i16, 2i16, 3i16|]; x[|1..2|]

@test
def testArraySlice14(): Vector[Int16, 4] = let x = [|1i16,2i16,3i16,4i16|]; x[|2..4|]

@test
def testArraySlice15(): Vector[Int64, 3] = let x = [|1i64, 2i64, 3i64|]; x[|1..2|]

@test
def testArraySlice16(): Vector[Int64, 4] = let x = [|1i64,2i64,3i64,4i64|]; x[|2..4|]

@test
def testArraySlice17(): Vector[Float32, 3] = let x = [|1.0f32, 2.0f32, 3.0f32|]; x[|1..2|]

@test
def testArraySlice18(): Vector[Float32, 4] = let x = [|1.0f32,2.0f32,3.0f32,4.0f32|]; x[|2..4|]

@test
def testArraySlice19(): Vector[Float64, 3] = let x = [|1.0f64, 2.0f64, 3.0f64|]; x[|1..2|]

@test
def testArraySlice20(): Vector[Float64, 4] = let x = [|1.0f64,2.0f64,3.0f64,4.0f64|]; x[|3..4|]

@test
def testArraySlice21(): Vector[Str, 2] = let x = [|"Hello", "World"|]; x[|1..2|]

@test
def testArraySlice22(): Vector[Str, 3] = let x = [|"World", "Hello", "World"|]; x[|2..4|]

@test
def testArraySlice23(): Vector[Vector[Int, 2], 2] = let x = [|[1,2], [1,2]|]; x[1..2]

@test
def testArraySlice24(): Vector[Vector[Int, 2], 3] = let x = [|[1,2,3], [1,2,3]|]; x[0..2]

@test
def testArraySlice25(): Vector[Vector[Int, 2], 1] = let x = [|[1], [2]|]; x[0..0]

@test
def testArraySlice26(): Vector[Vector[Int, 3], 3] = let x = [|[1,2,3], [1,2,3], [1,2,3]|]; x[0..3]

@test
def testArraySlice27(): Vector[Int, 10] = let x = [|1,2,3,4,5,6,7,8,9,10|]; x[0..Vlength[x]]


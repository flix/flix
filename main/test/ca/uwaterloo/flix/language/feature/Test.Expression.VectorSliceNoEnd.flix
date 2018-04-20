@test
def testVectorSliceNoEnd01(): Vector[Int, 3] = let x = [|1,2,3|]; x[|0..|]

@test
def testVectorSliceNoEnd02(): Vector[Int, 3] = let x = [|1,2,3,4,5|]; x[|2..|]

@test
def testVectorSliceNoEnd03(): Vector[Int, 5] = let x = [|1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20|]; x[|15..|]

@test
def testArraySliceNoEnd04(): Vector[Int, 1] = let x = [|1,2|]; x[|1..|]

@test
def testArraySliceNoEnd05(): Vector[Bool, 2] = let x = [true, false]; x[|0..|]

@test
def testArraySliceNoEnd06(): Vector[Bool, 1] = let x = [false, true, false]; x[|2..|]

@test
def testArraySliceNoEnd07(): Vector[Bool, 4] = let x = [|false, false, false, false, true|]; x[|1..|]

@test
def testArraySliceNoEnd08(): Vector[Char, 2] = let x = [|'a', 'b'|]; x[|0..|]

@test
def testArraySliceNoEnd09(): Vector[Char, 2] = let x = [|'a', 'b', 'c'|]; x[|0..|]

@test
def testArraySliceNoEnd10(): Vector[Char, 12] = let x = [|'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n'|]; x[|2..|]

@test
def testArraySliceNoEnd11(): Vector[Int8, 2] = let x = [|1i8, 2i8, 3i8|]; x[|1..|]

@test
def testArraySliceNoEnd12(): Vector[Int8, 2] = let x = [|1i8,2i8,3i8,4i8|]; x[|2..|]

@test
def testArraySliceNoEnd13(): [Int16, 2] = let x = [|1i16, 2i16, 3i16|]; x[|1..|]

@test
def testArraySliceNoEnd14(): Vector[Int16, 2] = let x = [|1i16,2i16,3i16,4i16|]; x[|2..|]

@test
def testArraySliceNoEnd15(): Vector[Int64, 2] = let x = [|1i64, 2i64, 3i64|]; x[|1..|]

@test
def testArraySliceNoEnd16(): Vector[Int64, 2] = let x = [|1i64,2i64,3i64,4i64|]; x[|2..|]

@test
def testArraySliceNoEnd17(): Vector[Float32, 2] = let x = [|1.0f32, 2.0f32, 3.0f32|]; x[|1..|]

@test
def testArraySliceNoEnd18(): Vector[Float32, 2] = let x = [|1.0f32,2.0f32,3.0f32,4.0f32|]; x[|2..|]

@test
def testArraySliceNoEnd19(): Vector[Float64, 2] = let x = [|1.0f64, 2.0f64, 3.0f64|]; x[|1..|]

@test
def testArraySliceNoEnd20(): Vector[Float64, 1] = let x = [|1.0f64,2.0f64,3.0f64,4.0f64|]; x[|3..|]

@test
def testArraySliceNoEnd21(): Vector[Str, 1] = let x = [|"Hello", "World"|]; x[|1..|]

@test
def testArraySliceNoEnd22(): Vector[Str, 1] = let x = [|"World", "Hello", "World"|]; x[|2..|]

@test
def testArraySliceNoEnd23(): Vector[Vector[Int, 2], 1] = let x = [|[1,2], [1,2]|]; x[|1..|]

@test
def testArraySliceNoEnd24(): Vector[Vector[Int, 2], 2] = let x = [|[1,2,3], [1,2,3]|]; x[|0..|]

@test
def testArraySliceNoEnd25(): Vector[Vector[Int, 2], 1] = let x = [|[1], [2]|]; x[|0..|]

@test
def testArraySliceNoEnd26(): Vector[Vector[Int, 3], 3] = let x = [|[1,2,3], [1,2,3], [1,2,3]|]; x[|0..|]

@test
def testArraySliceNoEnd27(): Vector[Int, 9] = let x = [|1,2,3,4,5,6,7,8,9,10|]; x[|0..|]


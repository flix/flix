@test
def testArraySlice01(): Array[Int] = let x = [1, 2, 3]; x[0..2]

@test
def testArraySlice02(): Array[Int] = let x = [1, 2, 3, 4]; x[2..3]

@test
def testArraySlice03(): Array[Int] = let x = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]; x[10..15]

@test
def testArraySlice04(): Array[Int] = let x = [1, 2]; x[1..2]

@test
def testArraySlice05(): Array[Bool] = let x = [true, false]; x[0..1]

@test
def testArraySlice06(): Array[Bool] = let x = [false, true, false]; x[2..3]

@test
def testArraySlice07(): Array[Bool] = let x = [false, false, false, false, true]; x[1..3]

@test
def testArraySlice08(): Array[Char] = let x = ['a', 'b']; x[0..1]

@test
def testArraySlice09(): Array[Char] = let x = ['a', 'b', 'c']; x[0..2]

@test
def testArraySlice10(): Array[Char] = let x = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n']; x[2..6]

@test
def testArraySlice11(): Array[Int8] = let x = [1i8, 2i8, 3i8]; x[1..2]

@test
def testArraySlice12(): Array[Int8] = let x = [1i8, 2i8, 3i8, 4i8]; x[2..4]

@test
def testArraySlice13(): Array[Int16] = let x = [1i16, 2i16, 3i16]; x[1..2]

@test
def testArraySlice14(): Array[Int16] = let x = [1i16, 2i16, 3i16, 4i16]; x[2..4]

@test
def testArraySlice15(): Array[Int64] = let x = [1i64, 2i64, 3i64]; x[1..2]

@test
def testArraySlice16(): Array[Int64] = let x = [1i64, 2i64, 3i64, 4i64]; x[2..4]

@test
def testArraySlice17(): Array[Float32] = let x = [1.0f32, 2.0f32, 3.0f32]; x[1..2]

@test
def testArraySlice18(): Array[Float32] = let x = [1.0f32, 2.0f32, 3.0f32, 4.0f32]; x[2..4]

@test
def testArraySlice19(): Array[Float64] = let x = [1.0f64, 2.0f64, 3.0f64]; x[1..2]

@test
def testArraySlice20(): Array[Float64] = let x = [1.0f64, 2.0f64, 3.0f64, 4.0f64]; x[3..4]

@test
def testArraySlice21(): Array[Str] = let x = ["Hello", "World"]; x[1..2]

@test
def testArraySlice22(): Array[Str] = let x = ["World", "Hello", "World"]; x[2..3]

@test
def testArraySlice23(): Array[Array[Int]] = let x = [[1, 2], [1, 2]]; x[1..2]

@test
def testArraySlice24(): Array[Array[Int]] = let x = [[1], [1, 2, 3, 4, 5, 6, 7, 8]]; x[0..2]

@test
def testArraySlice25(): Array[Array[Int]] = let x = [[1, 2, 3, 4, 5], [2]]; x[0..1]

@test
def testArraySlice26(): Array[Array[Int]] = let x = [[1, 2, 3], [1, 2, 3], [1, 2, 3]]; x[0..3]

@test
def testArraySlice27(): Array[Vector[Int, 3]] = let x = [[|1, 2, 3|], [|1, 2, 3|] ]; x[1..2]

@test
def testArraySlice28(): Array[(List[Int])] = let x = [(1 :: Nil), (2 :: Nil)]; x[1..2]

@test
def testArraySlice29(): Array[(Bool, Bool)] = let x = [(true, true), (true, false), (true, true)]; x[1..2]

@test
def testArraySlice30(): Array[Option[Int]] = let x = [Some(1), Some(2), Some(3), Some(4), None]; x[1..2]
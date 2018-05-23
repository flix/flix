@test
def testArraySliceNoStartIndexNoStartIndex01(): Array[Int] = let x = [1, 2, 3]; x[..2]

@test
def testArraySliceNoStartIndex02(): Array[Int] = let x = [1, 2, 3, 4]; x[..3]

@test
def testArraySliceNoStartIndex03(): Array[Int] = let x = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]; x[..15]

@test
def testArraySliceNoStartIndex04(): Array[Int] = let x = [1, 2]; x[..2]

@test
def testArraySliceNoStartIndex05(): Array[Bool] = let x = [true, false]; x[..1]

@test
def testArraySliceNoStartIndex06(): Array[Bool] = let x = [false, true, false]; x[..3]

@test
def testArraySliceNoStartIndex07(): Array[Bool] = let x = [false, false, false, false, true]; x[..3]

@test
def testArraySliceNoStartIndex08(): Array[Char] = let x = ['a', 'b']; x[..1]

@test
def testArraySliceNoStartIndex09(): Array[Char] = let x = ['a', 'b', 'c']; x[..2]

@test
def testArraySliceNoStartIndex10(): Array[Char] = let x = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h']; x[..6]

@test
def testArraySliceNoStartIndex11(): Array[Int8] = let x = [1i8, 2i8, 3i8]; x[..2]

@test
def testArraySliceNoStartIndex12(): Array[Int8] = let x = [1i8, 2i8, 3i8, 4i8]; x[..4]

@test
def testArraySliceNoStartIndex13(): Array[Int16] = let x = [1i16, 2i16, 3i16]; x[..2]

@test
def testArraySliceNoStartIndex14(): Array[Int16] = let x = [1i16, 2i16, 3i16, 4i16]; x[..4]

@test
def testArraySliceNoStartIndex15(): Array[Int64] = let x = [1i64, 2i64, 3i64]; x[..2]

@test
def testArraySliceNoStartIndex16(): Array[Int64] = let x = [1i64, 2i64, 3i64, 4i64]; x[..4]

@test
def testArraySliceNoStartIndex17(): Array[Float32] = let x = [1.0f32, 2.0f32, 3.0f32]; x[..2]

@test
def testArraySliceNoStartIndex18(): Array[Float32] = let x = [1.0f32, 2.0f32, 3.0f32, 4.0f32]; x[..4]

@test
def testArraySliceNoStartIndex19(): Array[Float64] = let x = [1.0f64, 2.0f64, 3.0f64]; x[..2]

@test
def testArraySliceNoStartIndex20(): Array[Float64] = let x = [1.0f64, 2.0f64, 3.0f64, 4.0f64]; x[..4]

@test
def testArraySliceNoStartIndex21(): Array[Str] = let x = ["Hello", "World"]; x[..2]

@test
def testArraySliceNoStartIndex22(): Array[Str] = let x = ["World", "Hello", "World"]; x[..3]

@test
def testArraySliceNoStartIndex23(): Array[Array[Int]] = let x = [[1, 2], [1, 2]]; x[..2]

@test
def testArraySliceNoStartIndex24(): Array[Array[Int]] = let x = [[1], [1, 2, 3, 4, 5, 6, 7, 8]]; x[..2]

@test
def testArraySliceNoStartIndex25(): Array[Array[Int]] = let x = [[1, 2, 3, 4, 5], [2]]; x[..1]

@test
def testArraySliceNoStartIndex26(): Array[Array[Int]] = let x = [[1, 2, 3], [1, 2, 3], [1, 2, 3]]; x[..3]

@test
def testArraySliceNoStartIndex27(): Array[Vector[Int, 3]] = let x = [[|1, 2, 3|], [|1, 2, 3|] ]; x[0..]

@test
def testArraySliceNoStartIndex28(): Array[(List[Int])] = let x = [(1 :: Nil), (2 :: Nil)]; x[0..]

@test
def testArraySliceNoStartIndex29(): Array[(Bool, Bool)] = let x = [(true, true), (true, false), (true, true)]; x[0..]

@test
def testArraySliceNoStartIndex30(): Array[Option[Int]] = let x = [None, None, Some(1)]; x[0..]
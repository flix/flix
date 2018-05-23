@test
def testArraySliceNoIndexes01(): Array[Int] = let x = [1, 2, 3]; x[..]

@test
def testArraySliceNoIndexes02(): Array[Int] = let x = [1, 2, 3, 4]; x[..]

@test
def testArraySliceNoIndexes03(): Array[Int] = let x = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]; x[..]

@test
def testArraySliceNoIndexes04(): Array[Int] = let x = [1, 2]; x[..]

@test
def testArraySliceNoIndexes05(): Array[Bool] = let x = [true, false]; x[..]

@test
def testArraySliceNoIndexes06(): Array[Bool] = let x = [false, true, false]; x[..]

@test
def testArraySliceNoIndexes07(): Array[Bool] = let x = [false, false, false, false, true]; x[..]

@test
def testArraySliceNoIndexes08(): Array[Char] = let x = ['a', 'b']; x[..]

@test
def testArraySliceNoIndexes09(): Array[Char] = let x = ['a', 'b', 'c']; x[..]

@test
def testArraySliceNoIndexes10(): Array[Char] = let x = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n']; x[..]

@test
def testArraySliceNoIndexes11(): Array[Int8] = let x = [1i8, 2i8, 3i8]; x[..]

@test
def testArraySliceNoIndexes12(): Array[Int8] = let x = [1i8, 2i8, 3i8, 4i8]; x[..]

@test
def testArraySliceNoIndexes13(): Array[Int16] = let x = [1i16, 2i16, 3i16]; x[..]

@test
def testArraySliceNoIndexes14(): Array[Int16] = let x = [1i16, 2i16, 3i16, 4i16]; x[..]

@test
def testArraySliceNoIndexes15(): Array[Int64] = let x = [1i64, 2i64, 3i64]; x[..]

@test
def testArraySliceNoIndexes16(): Array[Int64] = let x = [1i64, 2i64, 3i64, 4i64]; x[..]

@test
def testArraySliceNoIndexes17(): Array[Float32] = let x = [1.0f32, 2.0f32, 3.0f32]; x[..]

@test
def testArraySliceNoIndexes18(): Array[Float32] = let x = [1.0f32, 2.0f32, 3.0f32, 4.0f32]; x[..]

@test
def testArraySliceNoIndexes19(): Array[Float64] = let x = [1.0f64, 2.0f64, 3.0f64]; x[..]

@test
def testArraySliceNoIndexes20(): Array[Float64] = let x = [1.0f64, 2.0f64, 3.0f64, 4.0f64]; x[..]

@test
def testArraySliceNoIndexes21(): Array[Str] = let x = ["Hello", "World"]; x[..]

@test
def testArraySliceNoIndexes22(): Array[Str] = let x = ["World", "Hello", "World"]; x[..]

@test
def testArraySliceNoIndexes23(): Array[Array[Int]] = let x = [[1, 2], [1, 2]]; x[..]

@test
def testArraySliceNoIndexes24(): Array[Array[Int]] = let x = [[1], [1, 2, 3, 4, 5, 6, 7, 8]]; x[..]

@test
def testArraySliceNoIndexes25(): Array[Array[Int]] = let x = [[1, 2, 3, 4, 5], [2]]; x[..]

@test
def testArraySliceNoIndexes26(): Array[Array[Int]] = let x = [[1, 2, 3], [1, 2, 3], [1, 2, 3]]; x[..]

@test
def testArraySliceNoIndexes27(): Array[Int] = let x = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]; x[..]

@test
def testArraySliceNoIndexes28(): Array[Vector[Int, 3]] = let x = [[|1, 2, 3|], [|1, 2, 3|] ]; x[..]

@test
def testArraySliceNoIndexes29(): Array[(List[Int])] = let x = [(1 :: Nil), (2 :: Nil)]; x[..]

@test
def testArraySliceNoIndexes30(): Array[(Bool, Bool)] = let x = [(true, true), (true, false), (true, true)]; x[..]

@test
def testArraySliceNoIndexes31(): Array[Option[Int]] = let x = [None, None, Some(2), Some(2)]; x[..]



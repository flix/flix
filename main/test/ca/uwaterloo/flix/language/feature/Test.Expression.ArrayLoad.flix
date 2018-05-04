@test
def testArrayLoad01(): Int = let x = [1]; x[0]

@test
def testArrayLoad02(): Int = let x = [1, 2, 3]; x[2]

@test
def testArrayLoad03(): Int = let x = [1,2,3,4]; x[2]

@test
def testArrayLoad04(): Int = let x = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]; x[10]

@test
def testArrayLoad05(): Int = let x = [1,2]; x[1]

@test
def testArrayLoad06(): Bool = let x = [true]; x[0]

@test
def testArrayLoad07(): Bool = let x = [false]; x[0]

@test
def testArrayLoad08(): Bool = let x = [true, false]; x[1]

@test
def testArrayLoad09(): Bool = let x = [false, true, false]; x[2]

@test
def testArrayLoad10(): Bool = let x = [false, false, false, false, true]; x[3]

@test
def testArrayLoad11(): Char = let x = ['c']; x[0]

@test
def testArrayLoad12(): Char = let x = ['a', 'b']; x[0]

@test
def testArrayLoad13(): Char = let x = ['a', 'b', 'c']; x[1]

@test
def testArrayLoad14(): Char = let x = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n']; x[9]

@test
def testArrayLoad15(): Int8 = let x = [1i8]; x[0]

@test
def testArrayLoad16(): Int8 = let x = [1i8, 2i8, 3i8]; x[1]

@test
def testArrayLoad17(): Int8 = let x = [1i8,2i8,3i8,4i8]; x[2]

@test
def testArrayLoad18(): Int16 = let x = [1i16]; x[0]

@test
def testArrayLoad19(): Int16 = let x = [1i16, 2i16, 3i16]; x[1]

@test
def testArrayLoad20(): Int16 = let x = [1i16,2i16,3i16,4i16]; x[2]

@test
def testArrayLoad21(): Int64 = let x = [1i64]; x[0]

@test
def testArrayLoad22(): Int64 = let x = [1i64, 2i64, 3i64]; x[1]

@test
def testArrayLoad23(): Int64 = let x = [1i64,2i64,3i64,4i64]; x[2]

@test
def testArrayLoad24(): Float32 = let x = [1.0f32]; x[0]

@test
def testArrayLoad25(): Float32 = let x = [1.0f32, 2.0f32, 3.0f32]; x[1]

@test
def testArrayLoad26(): Float32 = let x = [1.0f32,2.0f32,3.0f32,4.0f32]; x[2]

@test
def testArrayLoad27(): Float64 = let x = [1.0f64]; x[0]

@test
def testArrayLoad28(): Float64 = let x = [1.0f64, 2.0f64, 3.0f64]; x[1]

@test
def testArrayLoad29(): Float64 = let x = [1.0f64,2.0f64,3.0f64,4.0f64]; x[3]

@test
def testArrayLoad30(): Str = let x = ["Hello"]; x[0]

@test
def testArrayLoad31(): Str = let x = ["Hello", "World"]; x[1]

@test
def testArrayLoad32(): Str = let x = ["World", "Hello", "World"]; x[2]

@test
def testArrayLoad33(): Int = let x = [[1,2,3]]; x[0][1]

@test
def testArrayLoad34(): Int = let x = [[1,2], [1,2]]; x[1][0]

@test
def testArrayLoad35(): Int = let x = [[1], [1,2,3,4,5,6,7,8]]; x[1][5]

@test
def testArrayLoad36(): Int = let x = [[1,2,3,4,5], [2]]; x[0][3]

@test
def testArrayLoad37(): Int = let x = [[1,2,3], [1,2,3], [1,2,3]]; x[2][2]


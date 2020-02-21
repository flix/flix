@test
def testVectorLit01(): Vector[Unit, 0] & Impure = [||]

@test
def testVectorLit02(): Vector[Int, 1] & Impure = [|1|]

@test
def testVectorLit03(): Vector[Int, 2] & Impure = [|1, 2|]

@test
def testVectorLit04(): Vector[Int, 3] & Impure = [|1, 2, 3|]

@test
def testVectorLit05(): Vector[Int, 4] & Impure = [|1, 2, 3, 4|]

@test
def testVectorLit06(): Vector[Int, 20] & Impure = [|1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20|]

@test
def testVectorLit07(): Vector[Bool, 1] & Impure = [|true|]

@test
def testVectorLit08(): Vector[Bool, 1] & Impure = [|false|]

@test
def testVectorLit09(): Vector[Bool, 2] & Impure = [|true, false|]

@test
def testVectorLit10(): Vector[Bool, 3] & Impure = [|false, true, false|]

@test
def testVectorLit11(): Vector[Bool, 5] & Impure = [|false, false, false, false, true|]

@test
def testVectorLit12(): Vector[Char, 1] & Impure = [|'c'|]

@test
def testVectorLit13(): Vector[Char, 2] & Impure = [|'a', 'b'|]

@test
def testVectorLit14(): Vector[Char, 3] & Impure = [|'a', 'b', 'c'|]

@test
def testVectorLit15(): Vector[Char, 14] & Impure = [|'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n'|]

@test
def testVectorLit16(): Vector[Int8, 1] & Impure = [|1i8|]

@test
def testVectorLit17(): Vector[Int8, 3] & Impure = [|1i8, 2i8, 3i8|]

@test
def testVectorLit18(): Vector[Int8, 4] & Impure = [|1i8, 2i8, 3i8, 4i8|]

@test
def testVectorLit19(): Vector[Int16, 1] & Impure = [|1i16|]

@test
def testVectorLit20(): Vector[Int16, 3] & Impure = [|1i16, 2i16, 3i16|]

@test
def testVectorLit21(): Vector[Int16, 4] & Impure = [|1i16, 2i16, 3i16, 4i16|]

@test
def testVectorLit22(): Vector[Int64, 1] & Impure = [|1i64|]

@test
def testVectorLit23(): Vector[Int64, 3] & Impure = [|1i64, 2i64, 3i64|]

@test
def testVectorLit24(): Vector[Int64, 4] & Impure = [|1i64, 2i64, 3i64, 4i64|]

@test
def testVectorLit25(): Vector[Float32, 1] & Impure = [|1.0f32|]

@test
def testVectorLit26(): Vector[Float32, 3] & Impure = [|1.0f32, 2.0f32, 3.0f32|]

@test
def testVectorLit27(): Vector[Float32, 4] & Impure = [|1.0f32, 2.0f32, 3.0f32, 4.0f32|]

@test
def testVectorLit28(): Vector[Float64, 1] & Impure = [|1.0f64|]

@test
def testVectorLit29(): Vector[Float64, 3] & Impure = [|1.0f64, 2.0f64, 3.0f64|]

@test
def testVectorLit30(): Vector[Float64, 4] & Impure = [|1.0f64, 2.0f64, 3.0f64, 4.0f64|]

@test
def testVectorLit31(): Vector[Str, 1] & Impure = [|"Hello"|]

@test
def testVectorLit32(): Vector[Str, 2] & Impure = [|"Hello", "World"|]

@test
def testVectorLit33(): Vector[Str, 3] & Impure = [|"World", "Hello", "World"|]

@test
def testVectorLit34(): Vector[Vector[Int, 3], 1] & Impure = [|[|1, 2, 3|]|]

@test
def testVectorLit35(): Vector[Vector[Int, 2], 2] & Impure = [|[|1, 2|], [|1, 2|]|]

@test
def testVectorLit36(): Vector[Vector[Int, 3], 3] & Impure = [|[|1, 2, 3|], [|1, 2, 3|], [|1, 2, 3|]|]

@test
def testVectorLit37(): Vector[(Int, Int), 1] & Impure = [|(2, 1)|]

@test
def testVectorLit38(): Vector[(Int, Int), 2] & Impure = [|(2, 1), (2, 1)|]

@test
def testVectorLit39(): Vector[(List[Int]), 1] & Impure = [|(Nil)|]

@test
def testVectorLit40(): Vector[(List[Int]), 1] & Impure = [|(1 :: 2 :: 3 :: 4 :: Nil)|]

@test
def testVectorLit41(): Vector[Option[Int], 1] & Impure = [|None|]

@test
def testVectorLit42(): Vector[Option[Int], 3] & Impure = [|None, Some(1), Some(2)|]

@test
def testVectorLit43(): Vector[Array[Int], 2] & Impure = [|[2, 1], [2, 1]|]

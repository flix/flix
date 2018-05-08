@test
def testVectorNew01(): Vector[Unit, 5] = [|();5|]

@test
def testVectorNew02(): Vector[Int, 1] = [|1; 1|]

@test
def testVectorNew03(): Vector[Int, 2] = [|1; 2|]

@test
def testVectorNew04(): Vector[Int, 3] = [|1; 3|]

@test
def testVectorNew05(): Vector[Int, 4] = [|1; 4|]

@test
def testVectorNew06(): Vector[Int,20] = [|1; 20|]

@test
def testVectorNew07(): Vector[Bool,1] = [|true; 1|]

@test
def testVectorNew08(): Vector[Bool, 1] = [|false; 1|]

@test
def testVectorNew09(): Vector[Bool, 2] = [|true; 2|]

@test
def testVectorNew10(): Vector[Bool, 3] = [|false; 3|]

@test
def testVectorNew11(): Vector[Bool, 5] = [|true; 5|]

@test
def testVectorNew12(): Vector[Char, 1] = [|'c'; 1|]

@test
def testVectorNew13(): Vector[Char, 2] = [|'a'; 2|]

@test
def testVectorNew14(): Vector[Char, 3] = [|'c'; 3|]

@test
def testVectorNew15(): Vector[Char, 20] = [|'a'; 20|]

@test
def testVectorNew16(): Vector[Int8, 1] = [|1i8; 1|]

@test
def testVectorNew17(): Vector[Int8, 3] = [|1i8; 3|]

@test
def testVectorNew18(): Vector[Int8, 4] = [|4i8; 4|]

@test
def testVectorNew19(): Vector[Int16, 1] = [|1i16; 1|]

@test
def testVectorNew20(): Vector[Int16, 3] = [|3i16; 3|]

@test
def testVectorNew21(): Vector[Int16, 4] = [|4i16; 4|]

@test
def testVectorNew22(): Vector[Int64, 1] = [|1i64; 1|]

@test
def testVectorNew23(): Vector[Int64, 3] = [|3i64; 3|]

@test
def testVectorNew24(): Vector[Int64, 4] = [|4i64; 4|]

@test
def testVectorNew25(): Vector[Float32, 1] = [|1.0f32; 1|]

@test
def testVectorNew26(): Vector[Float32, 3] = [|3.0f32; 3|]

@test
def testVectorNew27(): Vector[Float32, 4] = [|3.0f32; 4|]

@test
def testVectorNew28(): Vector[Float64, 1] = [|1.0f64; 1|]

@test
def testVectorNew29(): Vector[Float64, 3] = [|3.0f64; 3|]

@test
def testVectorNew30(): Vector[Float64, 4] = [|4.0f64; 4|]

@test
def testVectorNew31(): Vector[Str, 1] = [|"Hello"; 1|]

@test
def testVectorNew32(): Vector[Str, 2] = [|"World"; 2|]

@test
def testVectorNew33(): Vector[Str, 3] = [|"World"; 3|]

@test
def testVectorNew34(): Vector[(Str), 3] = [|("Hello") ; 3|]

@test
def testVectorNew35(): Vector[(List[Int]), 2] = [|(1 :: Nil) ; 2|]

enum Color {
   case Red,
   case Blue,
   case Green
}

@test
def testVectorNew36(): Vector[Color, 3] = [|Color.Red ; 3|]

@test
def testVectorNew37(): Vector[Array[Int], 1] = [|[3 ; 1]|]

@test
def testVectorNew38(): Vector[Vector[Int, 2], 2] = [|[|1, 2|]; 2|]

@test
def testVectorNew39(): Vector[Vector[Int, 3], 3] = [|[|1, 2, 3|]; 3|]

@test
def testVectorNew40(): Vector[Vector[Int, 2], 2] = [|[|2; 2|]; 2|]

@test
def testVectorNew41(): Vector[Vector[Int, 3], 1] = [|[|1, 2, 3|]; 1|]
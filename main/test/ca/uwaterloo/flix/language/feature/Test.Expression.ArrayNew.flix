@test
def testArrayNew01(): Array[Int] = [10; 0]

@test
def testArrayNew02(): Array[Int] = [1; 1]

@test
def testArrayNew03(): Array[Int] = [0; 2]

@test
def testArrayNew04(): Array[Int] = [0; 113]

@test
def testArrayNew05(): Array[Int] = [0; 500]

@test
def testArrayNew06(): Array[Unit] = [(); 5]

@test
def testArrayNew07(): Array[Bool] = [true; 5]

@test
def testArrayNew08(): Array[Bool] = [false; 5]

@test
def testArrayNew09(): Array[Char] = ['a'; 5]

@test
def testArrayNew10(): Array[Int8] = [10i8; 5]

@test
def testArrayNew11(): Array[Int16] = [10i16; 5]

@test
def testArrayNew12(): Array[Int32] = [10; 5]

@test
def testArrayNew13(): Array[Int64] = [10i64; 5]

@test
def testArrayNew14(): Array[BigInt] = [10ii; 5]

@test
def testArrayNew15(): Array[Float32] = [3.0f32; 5]

@test
def testArrayNew16(): Array[Float64] = [3.0f64; 5]

@test
def testArrayNew17(): Array[Str] = ["foo"; 5]

@test
def testArrayNew18(): Array[Array[Int]] = [[10; 10]; 5]

@test
def testArrayNew19(): Array[(Int, Int)] = [(1, 2); 5]

@test
def testArrayNew20(): Array[(List[Int])] = [(1 :: Nil); 5]

@test
def testArrayNew21(): Array[Option[Int]] = [None; 1]

@test
def testArrayNew22(): Array[Vector[Int, 2]] = [[|3 ; 2|]]
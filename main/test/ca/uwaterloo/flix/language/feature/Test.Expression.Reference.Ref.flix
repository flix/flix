@test
def testRef01(): Ref[Unit] = ref ()

@test
def testRef02(): Ref[Bool] = ref true

@test
def testRef03(): Ref[Bool] = ref false

@test
def testRef04(): Ref[Char] = ref 'a'

@test
def testRef05(): Ref[Float32] = ref 0.0f32

@test
def testRef06(): Ref[Float64] = ref 0.0f64

@test
def testRef07(): Ref[Int8] = ref 0i8

@test
def testRef08(): Ref[Int16] = ref 0i16

@test
def testRef09(): Ref[Int32] = ref 0i32

@test
def testRef10(): Ref[Int64] = ref 0i64

@test
def testRef11(): Ref[BigInt] = ref 0ii

@test
def testRef12(): Ref[Str] = ref "hello world"

@test
def testRef13(): Ref[(Int, Int, Int)] = ref (1, 2, 3)

@test
def testRef14(): Ref[Option[Int]] = ref None : Option[Int]

@test
def testRef15(): Ref[Option[Int]] = ref Some(42)

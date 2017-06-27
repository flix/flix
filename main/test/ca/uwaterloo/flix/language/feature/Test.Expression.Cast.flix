@test
def testCast01(): Unit = () as Unit

@test
def testCast02(): Bool = true as Bool

@test
def testCast03(): Bool = !false as Bool

@test
def testCast04(): Char = 'a' as Char

@test
def testCast05(): Float32 = 0.0f32 as Float32

@test
def testCast06(): Float64 = 0.0f64 as Float64

@test
def testCast07(): Int8 = 0i8 as Int8

@test
def testCast08(): Int16 = 0i16 as Int16

@test
def testCast09(): Int32 = 0i32 as Int32

@test
def testCast10(): Int64 = 0i64 as Int64

@test
def testCast11(): BigInt = 0ii as BigInt

@test
def testCast12(): Str = "hello world" as Str

@test
def testCast13(): (Int, Int, Int) = (1, 2, 3) as (Int, Int, Int)

@test
def testCast14(): Option[Int] = Some(42) as Option[Int]

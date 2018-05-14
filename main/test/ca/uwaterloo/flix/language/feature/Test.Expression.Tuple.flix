@test
def testTuple01(): (Bool, Bool) = (true, true)

@test
def testTuple02(): (Bool, Bool, Bool) = (true, true, false)

@test
def testTuple03(): (Str, Str, Str, Str) = ("un", "deux", "trois", "quatre")

@test
def testTuple04(): (Str, Bool, Int64, (), Int8) = ("un", false, 12345i64, (), -2i8)

@test
def testTuple05(): (Option[Int], Option[Int]) = (None, Some(42))

@test
def testTuple06(): ((Int, Int), (Str, Str)) = ((123, 456), ("654", "321"))

@test
def testTuple07(): (BigInt, Bool, Str) = (40ii + 2ii, !(-12 < 22), if (true) "hi" else "hello")

@test
def testTuple08(): (Char, Float32, Float64) = ('a', 1.2f32, 3.4f64)

@test
def testTuple09(): (Set[Int], Set[Char]) = (#{1, 2, 3}, #{'a', 'b'})



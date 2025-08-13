mod Test.Exp.ExtMatch {

    ///////////////////////////////////////////////////////////////////////////
    /// Flix should support extensible variant tags with CLOSED rows.
    ///////////////////////////////////////////////////////////////////////////

    @Test
    def testExtTagUnit01(): #| A(Unit) |# = xvar A(())

    @Test
    def testExtTagBool01(): #| A(Bool) |# = xvar A(true)

    @Test
    def testExtTagBool02(): #| B(Bool) |# = xvar B(false)

    @Test
    def testExtTagBool03(): #| C(Bool) |# = xvar C(true)

    @Test
    def testExtTagChar01(): #| A(Char) |# = xvar A('a')

    @Test
    def testExtTagChar02(): #| B(Char) |# = xvar B('Z')

    @Test
    def testExtTagChar03(): #| C(Char) |# = xvar C('0')

    @Test
    def testExtTagInt8_01(): #| A(Int8) |# = xvar A(0i8)

    @Test
    def testExtTagInt8_02(): #| B(Int8) |# = xvar B(127i8)

    @Test
    def testExtTagInt8_03(): #| C(Int8) |# = xvar C(-128i8)

    @Test
    def testExtTagInt16_01(): #| A(Int16) |# = xvar A(0i16)

    @Test
    def testExtTagInt16_02(): #| B(Int16) |# = xvar B(32767i16)

    @Test
    def testExtTagInt16_03(): #| C(Int16) |# = xvar C(-32768i16)

    @Test
    def testExtTagInt32_01(): #| A(Int32) |# = xvar A(0i32)

    @Test
    def testExtTagInt32_02(): #| B(Int32) |# = xvar B(2147483647i32)

    @Test
    def testExtTagInt32_03(): #| C(Int32) |# = xvar C(-2147483648i32)

    @Test
    def testExtTagInt64_01(): #| A(Int64) |# = xvar A(0i64)

    @Test
    def testExtTagInt64_02(): #| B(Int64) |# = xvar B(9223372036854775807i64)

    @Test
    def testExtTagInt64_03(): #| C(Int64) |# = xvar C(-9223372036854775808i64)

    @Test
    def testExtTagFloat32_01(): #| A(Float32) |# = xvar A(0.0f32)

    @Test
    def testExtTagFloat32_02(): #| B(Float32) |# = xvar B(3.14f32)

    @Test
    def testExtTagFloat32_03(): #| C(Float32) |# = xvar C(-1.5f32)

    @Test
    def testExtTagFloat64_01(): #| A(Float64) |# = xvar A(0.0f64)

    @Test
    def testExtTagFloat64_02(): #| B(Float64) |# = xvar B(3.141592653589793f64)

    @Test
    def testExtTagFloat64_03(): #| C(Float64) |# = xvar C(-2.718281828f64)

    @Test
    def testExtTagString01(): #| A(String) |# = xvar A("")

    @Test
    def testExtTagString02(): #| B(String) |# = xvar B("Hello, World!")

    @Test
    def testExtTagString03(): #| C(String) |# = xvar C("Flix")

    @Test
    def testExtTagPair01(): #| A((Int32, String)) |# = xvar A((42i32, "test"))

    @Test
    def testExtTagPair02(): #| B((Bool, Char)) |# = xvar B((true, 'x'))

    @Test
    def testExtTagPair03(): #| C((Float64, Int8)) |# = xvar C((3.14f64, 127i8))

    @Test
    def testExtTagPair04(): #| D((String, Bool)) |# = xvar D(("hello", false))

    @Test
    def testExtTagPair05(): #| E((Int16, Float32)) |# = xvar E((1000i16, 2.5f32))

    ///////////////////////////////////////////////////////////////////////////
    /// Flix should support extensible variant tags with OPEN rows.
    ///////////////////////////////////////////////////////////////////////////

    // NOTE: The following functions are polymorphic and hence cannot be marked with @Test.

    def _testExtTagUnit01(): #| A(Unit) | r |# = xvar A(())

    def _testExtTagBool01(): #| A(Bool) | r |# = xvar A(true)

    def _testExtTagBool02(): #| B(Bool) | r |# = xvar B(false)

    def _testExtTagBool03(): #| C(Bool) | r |# = xvar C(true)

    def _testExtTagChar01(): #| A(Char) | r |# = xvar A('a')

    def _testExtTagChar02(): #| B(Char) | r |# = xvar B('Z')

    def _testExtTagChar03(): #| C(Char) | r |# = xvar C('0')

    def _testExtTagInt8_01(): #| A(Int8) | r |# = xvar A(0i8)

    def _testExtTagInt8_02(): #| B(Int8) | r |# = xvar B(127i8)

    def _testExtTagInt8_03(): #| C(Int8) | r |# = xvar C(-128i8)

    def _testExtTagInt16_01(): #| A(Int16) | r |# = xvar A(0i16)

    def _testExtTagInt16_02(): #| B(Int16) | r |# = xvar B(32767i16)

    def _testExtTagInt16_03(): #| C(Int16) | r |# = xvar C(-32768i16)

    def _testExtTagInt32_01(): #| A(Int32) | r |# = xvar A(0i32)

    def _testExtTagInt32_02(): #| B(Int32) | r |# = xvar B(2147483647i32)

    def _testExtTagInt32_03(): #| C(Int32) | r |# = xvar C(-2147483648i32)

    def _testExtTagInt64_01(): #| A(Int64) | r |# = xvar A(0i64)

    def _testExtTagInt64_02(): #| B(Int64) | r |# = xvar B(9223372036854775807i64)

    def _testExtTagInt64_03(): #| C(Int64) | r |# = xvar C(-9223372036854775808i64)

    def _testExtTagFloat32_01(): #| A(Float32) | r |# = xvar A(0.0f32)

    def _testExtTagFloat32_02(): #| B(Float32) | r |# = xvar B(3.14f32)

    def _testExtTagFloat32_03(): #| C(Float32) | r |# = xvar C(-1.5f32)

    def _testExtTagFloat64_01(): #| A(Float64) | r |# = xvar A(0.0f64)

    def _testExtTagFloat64_02(): #| B(Float64) | r |# = xvar B(3.141592653589793f64)

    def _testExtTagFloat64_03(): #| C(Float64) | r |# = xvar C(-2.718281828f64)

    def _testExtTagString01(): #| A(String) | r |# = xvar A("")

    def _testExtTagString02(): #| B(String) | r |# = xvar B("Hello, World!")

    def _testExtTagString03(): #| C(String) | r |# = xvar C("Flix")

    def _testExtTagPair01(): #| A((Int32, String)) | r |# = xvar A((42i32, "test"))

    def _testExtTagPair02(): #| B((Bool, Char)) | r |# = xvar B((true, 'x'))

    def _testExtTagPair03(): #| C((Float64, Int8)) | r |# = xvar C((3.14f64, 127i8))

    def _testExtTagPair04(): #| D((String, Bool)) | r |# = xvar D(("hello", false))

    def _testExtTagPair05(): #| E((Int16, Float32)) | r |# = xvar E((1000i16, 2.5f32))

    ///////////////////////////////////////////////////////////////////////////
    /// Flix should support extensible variants with multiple arities.
    ///////////////////////////////////////////////////////////////////////////

    @Test
    def testExtTagArity01(): #| A |# = xvar A

    @Test
    def testExtTagArity02(): #| A(Bool, Char) |# = xvar A(false, 'x')

    @Test
    def testExtTagArity03(): #| A(Int32, String, Float64) |# = xvar A(42i32, "hello", 2.718f64)

    ///////////////////////////////////////////////////////////////////////////
    /// Flix should support extensible variants with multiple tags.
    ///////////////////////////////////////////////////////////////////////////

    @Test
    def testExtTagMultiple01(): #| A(Bool), B(Char) |# = if (true) xvar A(true) else xvar B('a')

    @Test
    def testExtTagMultiple02(): #| A(Int32), B(String, Bool) |# = if (false) xvar A(42i32) else xvar B("hello", true)

    @Test
    def testExtTagMultiple03(): #| A(Float64), B |# = if (true) xvar A(3.14f64) else xvar B

    @Test
    def testExtTagMultiple04(): #| A, B(Int8, Char, Bool) |# = if (false) xvar A else xvar B(127i8, 'Z', false)

    @Test
    def testExtTagMultiple05(): #| A(Int16, Float32), B(String) |# = if (true) xvar A(1000i16, 2.5f32) else xvar B("Flix")

    @Test
    def testExtTagMultiple06(): #| A(Char, Int64, Unit), B |# = if (false) xvar A('x', -9223372036854775808i64, ()) else xvar B

    @Test
    def testExtTagMultiple07(): #| A, B(String, Int32, Bool, Float64) |# = if (true) xvar A else xvar B("test", 123i32, true, 2.718f64)

    @Test
    def testExtTagMultiple08(): #| A(Float32, Char), B(Int8) |# = if (false) xvar A(1.5f32, 'y') else xvar B(-128i8)

}


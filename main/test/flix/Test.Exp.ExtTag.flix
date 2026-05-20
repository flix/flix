mod Test.Exp.ExtTag {

    ///////////////////////////////////////////////////////////////////////////
    /// Flix should support extensible variants with CLOSED rows.
    ///////////////////////////////////////////////////////////////////////////

    @CompileTest
    def testExtTagUnit01(): #| A(Unit) |# = xvar A(())

    @CompileTest
    def testExtTagUnit02(): #| A(Unit) |# = xvar A()

    @CompileTest
    def testExtTagBool01(): #| A(Bool) |# = xvar A(true)

    @CompileTest
    def testExtTagBool02(): #| A(Bool) |# = xvar A(false)

    @CompileTest
    def testExtTagChar01(): #| A(Char) |# = xvar A('a')

    @CompileTest
    def testExtTagChar02(): #| A(Char) |# = xvar A('Z')

    @CompileTest
    def testExtTagChar03(): #| A(Char) |# = xvar A('0')

    @CompileTest
    def testExtTagInt8_01(): #| A(Int8) |# = xvar A(0i8)

    @CompileTest
    def testExtTagInt8_02(): #| A(Int8) |# = xvar A(127i8)

    @CompileTest
    def testExtTagInt8_03(): #| A(Int8) |# = xvar A(-128i8)

    @CompileTest
    def testExtTagInt16_01(): #| A(Int16) |# = xvar A(0i16)

    @CompileTest
    def testExtTagInt16_02(): #| A(Int16) |# = xvar A(32767i16)

    @CompileTest
    def testExtTagInt16_03(): #| A(Int16) |# = xvar A(-32768i16)

    @CompileTest
    def testExtTagInt32_01(): #| A(Int32) |# = xvar A(0i32)

    @CompileTest
    def testExtTagInt32_02(): #| A(Int32) |# = xvar A(2147483647i32)

    @CompileTest
    def testExtTagInt32_03(): #| A(Int32) |# = xvar A(-2147483648i32)

    @CompileTest
    def testExtTagInt64_01(): #| A(Int64) |# = xvar A(0i64)

    @CompileTest
    def testExtTagInt64_02(): #| A(Int64) |# = xvar A(9223372036854775807i64)

    @CompileTest
    def testExtTagInt64_03(): #| A(Int64) |# = xvar A(-9223372036854775808i64)

    @CompileTest
    def testExtTagFloat32_01(): #| A(Float32) |# = xvar A(0.0f32)

    @CompileTest
    def testExtTagFloat32_02(): #| A(Float32) |# = xvar A(3.14f32)

    @CompileTest
    def testExtTagFloat32_03(): #| A(Float32) |# = xvar A(-1.5f32)

    @CompileTest
    def testExtTagFloat64_01(): #| A(Float64) |# = xvar A(0.0f64)

    @CompileTest
    def testExtTagFloat64_02(): #| A(Float64) |# = xvar A(3.141592653589793f64)

    @CompileTest
    def testExtTagFloat64_03(): #| A(Float64) |# = xvar A(-2.718281828f64)

    @CompileTest
    def testExtTagString01(): #| A(String) |# = xvar A("")

    @CompileTest
    def testExtTagString02(): #| A(String) |# = xvar A("Hello, World!")

    @CompileTest
    def testExtTagString03(): #| A(String) |# = xvar A("Flix")

    @CompileTest
    def testExtTagPair01(): #| A((Int32, String)) |# = xvar A((42i32, "test"))

    @CompileTest
    def testExtTagPair02(): #| A((Bool, Char)) |# = xvar A((true, 'x'))

    @CompileTest
    def testExtTagPair03(): #| A((Float64, Int8)) |# = xvar A((3.14f64, 127i8))

    @CompileTest
    def testExtTagPair04(): #| A((String, Bool)) |# = xvar A(("hello", false))

    @CompileTest
    def testExtTagPair05(): #| A((Int16, Float32)) |# = xvar A((1000i16, 2.5f32))

    ///////////////////////////////////////////////////////////////////////////
    /// Flix should support extensible variants with OPEN rows.
    ///////////////////////////////////////////////////////////////////////////

    // NOTE: The following functions are polymorphic and hence cannot be marked with @CompileTest.

    def _testExtTagUnit01(): #| A(Unit) | r |# = xvar A(())

    def _testExtTagBool01(): #| A(Bool) | r |# = xvar A(true)

    def _testExtTagChar01(): #| A(Char) | r |# = xvar A('a')

    def _testExtTagInt8_01(): #| A(Int8) | r |# = xvar A(0i8)

    def _testExtTagInt16_01(): #| A(Int16) | r |# = xvar A(0i16)

    def _testExtTagInt32_01(): #| A(Int32) | r |# = xvar A(0i32)

    def _testExtTagInt64_01(): #| A(Int64) | r |# = xvar A(0i64)

    def _testExtTagFloat32_01(): #| A(Float32) | r |# = xvar A(0.0f32)

    def _testExtTagFloat64_01(): #| A(Float64) | r |# = xvar A(0.0f64)

    def _testExtTagString01(): #| A(String) | r |# = xvar A("")

    def _testExtTagPair01(): #| A((Int32, String)) | r |# = xvar A((42i32, "test"))

    def _testExtTagPair02(): #| B((Bool, Char)) | r |# = xvar B((true, 'x'))

    def _testExtTagPair03(): #| C((Float64, Int8)) | r |# = xvar C((3.14f64, 127i8))

    def _testExtTagPair04(): #| D((String, Bool)) | r |# = xvar D(("hello", false))

    def _testExtTagPair05(): #| E((Int16, Float32)) | r |# = xvar E((1000i16, 2.5f32))

    ///////////////////////////////////////////////////////////////////////////
    /// Flix should support extensible variants with multiple arities.
    ///////////////////////////////////////////////////////////////////////////

    @CompileTest
    def testExtTagArity01(): #| A |# = xvar A

    @CompileTest
    def testExtTagArity02(): #| A(Bool, Char) |# = xvar A(false, 'x')

    @CompileTest
    def testExtTagArity03(): #| A(Int32, String, Float64) |# = xvar A(42i32, "hello", 2.718f64)

    ///////////////////////////////////////////////////////////////////////////
    /// Flix should support extensible variants with multiple variants.
    ///////////////////////////////////////////////////////////////////////////

    @CompileTest
    def testExtTagMultiple01(): #| A(Bool), B(Char) |# = if (true) xvar A(true) else xvar B('a')

    @CompileTest
    def testExtTagMultiple02(): #| A(Int32), B(String, Bool) |# = if (false) xvar A(42i32) else xvar B("hello", true)

    @CompileTest
    def testExtTagMultiple03(): #| A(Float64), B |# = if (true) xvar A(3.14f64) else xvar B

    @CompileTest
    def testExtTagMultiple04(): #| A, B(Int8, Char, Bool) |# = if (false) xvar A else xvar B(127i8, 'Z', false)

    @CompileTest
    def testExtTagMultiple05(): #| A(Int16, Float32), B(String) |# = if (true) xvar A(1000i16, 2.5f32) else xvar B("Flix")

    @CompileTest
    def testExtTagMultiple06(): #| A(Char, Int64, Unit), B |# = if (false) xvar A('x', -9223372036854775808i64, ()) else xvar B

    @CompileTest
    def testExtTagMultiple07(): #| A, B(String, Int32, Bool, Float64) |# = if (true) xvar A else xvar B("test", 123i32, true, 2.718f64)

    @CompileTest
    def testExtTagMultiple08(): #| A(Float32, Char), B(Int8) |# = if (false) xvar A(1.5f32, 'y') else xvar B(-128i8)

}


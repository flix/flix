mod Test.Exp.CheckedTypeCast {

    import java.io.Serializable
    import java.lang.Integer
    import java.lang.Object
    import java.lang.{StringBuilder => JStringBuilder}

    @CompileTest
    def testCheckedTypeCast01(): Object = checked_cast(null)

    @CompileTest
    def testCheckedTypeCast02(): String = checked_cast(null)

    @CompileTest
    def testCheckedTypeCast03(): Integer = checked_cast(null)

    @CompileTest
    def testCheckedTypeCast04(): Serializable = checked_cast(null)

    @CompileTest
    def testCheckedTypeCast05(): Array[String, Static] = checked_cast(null)

    @CompileTest
    def testCheckedTypeCast06(): Serializable \ IO = {
        checked_cast(new JStringBuilder())
    }

    @CompileTest
    def testCheckedTypeCast07(): Object \ IO = {
        checked_cast(new JStringBuilder())
    }

    @CompileTest
    def testCheckedTypeCast08(): Object = checked_cast("ABC")

    @CompileTest
    def testCheckedTypeCast09(): Object = region rc {
        checked_cast(Array#{} @ rc)
    }

    @CompileTest
    def testCheckedTypeCast10(): Object = checked_cast(Box.box(42i8))

    @CompileTest
    def testCheckedTypeCast11(): Object = checked_cast(Box.box(42i16))

    @CompileTest
    def testCheckedTypeCast12(): Object = checked_cast(Box.box(42i32))

    @CompileTest
    def testCheckedTypeCast13(): Object = checked_cast(Box.box(42i64))

    @CompileTest
    def testCheckedTypeCast14(): Object = checked_cast(Box.box(42.0f32))

    @CompileTest
    def testCheckedTypeCast15(): Object = checked_cast(Box.box(42.0f64))

    @CompileTest
    def testCheckedTypeCast16(): Object = checked_cast(Box.box('c'))

    @CompileTest
    def testCheckedTypeCast17(): Object = checked_cast(Box.box(true))

    @CompileTest
    def testCheckedTypeCast18(): Object = upcast(42)

    type alias Num = Int32

    def upcast(x: Num): Object = checked_cast(Box.box(x))

}

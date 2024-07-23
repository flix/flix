mod Test.Exp.Jvm.InvokeConstructor2 {

    import java.lang.Boolean
    import java.lang.Byte
    import java.lang.Character
    import java.lang.Double
    import java.lang.Float
    import java.lang.Integer
    import java.lang.Long
    import java.lang.Object
    import java.lang.Short
    import java.lang.{String => JString}
    import java.util.{Locale$Builder => Builder}

    @test
    def testInvokeConstructor2_01(): Boolean \ IO =
        new Boolean(true)

    @test
    def testInvokeConstructor2_02(): Character \ IO =
        new Character('a')

    @test
    def testInvokeConstructor2_03(): Float \ IO =
        new Float(123.456f32)

    @test
    def testInvokeConstructor2_04(): Double \ IO =
        new Double(123.456f64)

    @test
    def testInvokeConstructor2_05(): Byte \ IO =
        new Byte(123i8)

    @test
    def testInvokeConstructor2_06(): Short \ IO =
        new Short(123i16)

    @test
    def testInvokeConstructor2_07(): Integer \ IO =
        new Integer(123i32)

    @test
    def testInvokeConstructor2_08(): Long \ IO =
        new Long(123i64)

    @test
    def testInvokeConstructor2_09(): JString \ IO =
        new JString("Hello World")

    @test
    def testInvokeObjectConstructor2_01(): Object \ IO =
        let obj = new JString("Hello World");
        (checked_cast(obj) : Object)

    @test
    def testInvokeStaticNestedConstructor2_01(): Builder \ IO =
        new Builder()
}

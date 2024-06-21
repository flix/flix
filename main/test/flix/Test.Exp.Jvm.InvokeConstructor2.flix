mod Test.Exp.Jvm.InvokeConstructor2 {

    import java.lang.{Boolean, Character, Float, Double, Byte, Long, Short, Integer, String};
    import java.util.{Locale$Builder => Builder}

    @test
    def testInvokeConstructor2_01(): ##java.lang.Boolean \ IO =
        new Boolean(true)

    @test
    def testInvokeConstructor2_02(): ##java.lang.Character \ IO =
        new Character('a')

    @test
    def testInvokeConstructor2_03(): ##java.lang.Float \ IO =
        new Float(123.456f32)

    @test
    def testInvokeConstructor2_04(): ##java.lang.Double \ IO =
        new Double(123.456f64)

    @test
    def testInvokeConstructor2_05(): ##java.lang.Byte \ IO =
        new Byte(123i8)

    @test
    def testInvokeConstructor2_06(): ##java.lang.Short \ IO =
        new Short(123i16)

    @test
    def testInvokeConstructor2_07(): ##java.lang.Integer \ IO =
        new Integer(123i32)

    @test
    def testInvokeConstructor2_08(): ##java.lang.Long \ IO =
        new Long(123i64)

    @test
    def testInvokeConstructor2_09(): ##java.lang.String \ IO =
        new String("Hello World")

    @test
    def testInvokeObjectConstructor2_01(): ##java.lang.Object \ IO =
        let obj = new String("Hello World");
        (checked_cast(obj) : ##java.lang.Object)

    @test
    def testInvokeStaticNestedConstructor2_01(): ##java.util.Locale$Builder \ IO =
        new Builder()
}

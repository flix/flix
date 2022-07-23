namespace Test/Exp/Jvm/NewObject {

  def implementSerializable(): ##java.io.Serializable & Impure =
    object ##java.io.Serializable { }

  def implementSerializableAgain(): ##java.io.Serializable & Impure =
    object ##java.io.Serializable { }

  @test
  def testImplementInterface01(): Bool & Impure =
    import java.lang.Object.toString(): String & Pure;
    let anon = implementSerializable() as ##java.lang.Object;
    toString(anon) |> String.startsWith(prefix = "Anon")

  @test
  def testImplementInterface02(): Bool & Impure =
    import java.lang.Object.hashCode(): Int32 & Pure;
    let anon = implementSerializable() as ##java.lang.Object;
    let anon2 = implementSerializable() as ##java.lang.Object;
    hashCode(anon) != hashCode(anon2)

  @test
  def testImplementInterface03(): Bool & Impure =
    import java.lang.Object.equals(##java.lang.Object): Bool & Pure;
    let anon = implementSerializable() as ##java.lang.Object;
    let anon2 = implementSerializable() as ##java.lang.Object;
    not equals(anon, anon2)

  @test
  def testImplementInterface04(): Bool & Impure =
    import java.lang.Object.getClass(): ##java.lang.Class & Pure;
    import java.lang.Object.equals(##java.lang.Object): Bool & Pure;
    let anon = implementSerializable() as ##java.lang.Object;
    let anon2 = implementSerializable() as ##java.lang.Object;
    equals(getClass(anon) as ##java.lang.Object, getClass(anon2) as ##java.lang.Object)

  @test
  def testImplementInterface05(): Bool & Impure =
    import java.lang.Object.getClass(): ##java.lang.Class & Pure;
    import java.lang.Object.equals(##java.lang.Object): Bool & Pure;
    let anon = implementSerializable() as ##java.lang.Object;
    let anon2 = implementSerializableAgain() as ##java.lang.Object;
    not equals(getClass(anon) as ##java.lang.Object, getClass(anon2) as ##java.lang.Object)

  @test 
  def testPrimitiveTypes01(): Bool & Impure =
    import static flix.test.TestPrimitiveTypes.runTest(##flix.test.TestPrimitiveTypes): Bool;
    let anon = object ##flix.test.TestPrimitiveTypes {
      def takesAndReturnsVoid(_this: ##flix.test.TestPrimitiveTypes): Unit = ()
      def takesAndReturnsBoolean(_this: ##flix.test.TestPrimitiveTypes, x: Bool): Bool = println("${x}"); not x
      def takesAndReturnsChar(_this: ##flix.test.TestPrimitiveTypes, x: Char): Char = println("${x}"); Char.toUpperCase(x)
      def takesAndReturnsByte(_this: ##flix.test.TestPrimitiveTypes, x: Int8): Int8 = println("${x}"); x + 1i8
      def takesAndReturnsShort(_this: ##flix.test.TestPrimitiveTypes, x: Int16): Int16 = println("${x}"); x + 1i16
      def takesAndReturnsInt(_this: ##flix.test.TestPrimitiveTypes, x: Int32): Int32 = println("${x}"); x + 1
      def takesAndReturnsLong(_this: ##flix.test.TestPrimitiveTypes, x: Int64): Int64 = println("${x}"); x + 1i64
      def takesAndReturnsFloat(_this: ##flix.test.TestPrimitiveTypes, x: Float32): Float32 = println("${x}"); x + 1.23f32
      def takesAndReturnsDouble(_this: ##flix.test.TestPrimitiveTypes, x: Float64): Float64 = println("${x}"); x + 1.23

      /// def allTheTypes(_this: ##flix.test.TestPrimitiveTypes, a: Bool, b: Char, c: Int8, d: Int16, e: Int32, f: Int64, g: Float32, h: Float64): String = 
      ///   "${a}, ${b}, ${c}, ${d}, ${e}, ${f}, ${g}, ${h}"
    };
    runTest(anon)
}

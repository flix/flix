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
      def takesAndReturnsBoolean(_this: ##flix.test.TestPrimitiveTypes, x: Bool): Bool = not x
      def takesAndReturnsChar(_this: ##flix.test.TestPrimitiveTypes, x: Char): Char = Char.toUpperCase(x)
      def takesAndReturnsByte(_this: ##flix.test.TestPrimitiveTypes, x: Int8): Int8 = x + 1i8
      def takesAndReturnsShort(_this: ##flix.test.TestPrimitiveTypes, x: Int16): Int16 = x + 1i16
      def takesAndReturnsInt(_this: ##flix.test.TestPrimitiveTypes, x: Int32): Int32 = x + 1
      def takesAndReturnsLong(_this: ##flix.test.TestPrimitiveTypes, x: Int64): Int64 = x + 1i64
      def takesAndReturnsFloat(_this: ##flix.test.TestPrimitiveTypes, x: Float32): Float32 = x + 1.23f32
      def takesAndReturnsDouble(_this: ##flix.test.TestPrimitiveTypes, x: Float64): Float64 = x + 1.23

      def allTheTypes(_this: ##flix.test.TestPrimitiveTypes, a: Bool, b: Char, c: Int8, d: Int16, e: Int32, f: Int64, g: Float32, h: Float64): String = 
        "${a}, ${b}, ${c}, ${d}, ${e}, ${f}, ${g}, ${h}"
    };
    runTest(anon)

  @test
  def testOverloadedMethods01(): Bool & Impure =
    import static flix.test.TestOverloadedMethods.runTest(##flix.test.TestOverloadedMethods): Bool;
    let anon = object ##flix.test.TestOverloadedMethods {
      def overloadedMethod(_this: ##flix.test.TestOverloadedMethods): Int32 = 42
      def overloadedMethod(_this: ##flix.test.TestOverloadedMethods, x: Int32): Int32 = x + 1
      def overloadedMethod(_this: ##flix.test.TestOverloadedMethods, x: String, y: Float64, z: Float64): String = "${x}${y / z}"
    };
    runTest(anon)

  @test
  def testDefaultMethods01(): Bool & Impure =
    import flix.test.TestDefaultMethods.methodWithNoImplementation(Int32): Int32;
    import flix.test.TestDefaultMethods.methodWithDefaultImplementation(Int32): Int32;
    let anon = object ##flix.test.TestDefaultMethods {
      def methodWithNoImplementation(_this: ##flix.test.TestDefaultMethods, x: Int32): Int32 = x + 1
    };
    methodWithNoImplementation(anon, 1) == 2 and methodWithDefaultImplementation(anon, 1) == 43

  @test
  def testDefaultMethods02(): Bool & Impure =
    import flix.test.TestDefaultMethods.methodWithNoImplementation(Int32): Int32;
    import flix.test.TestDefaultMethods.methodWithDefaultImplementation(Int32): Int32;
    let anon = object ##flix.test.TestDefaultMethods {
      def methodWithNoImplementation(_this: ##flix.test.TestDefaultMethods, x: Int32): Int32 = x + 1
      def methodWithDefaultImplementation(_this: ##flix.test.TestDefaultMethods, x: Int32): Int32 = x + 2
    };
    methodWithNoImplementation(anon, 1) == 2 and methodWithDefaultImplementation(anon, 1) == 3

  @test
  def testGenerics01(): Bool & Impure =
    import static flix.test.TestGenerics.runTest(##flix.test.TestGenerics): Bool;
    import java.lang.Integer.toString(): String;
    let anon = object ##flix.test.TestGenerics {
      def methodOne(_this: ##flix.test.TestGenerics, x: ##java.lang.Object): ##java.lang.Object = 
        let str = x as String;
        "${str}, ${str}" as ##java.lang.Object
      def methodTwo(_this: ##flix.test.TestGenerics, x: ##java.lang.Object, y: ##java.lang.Object): ##java.lang.Object =
        let str = x as String;
        let int = y as ##java.lang.Integer;
        "${str}, ${toString(int)}" as ##java.lang.Object
    };
    runTest(anon)

  @test
  def testClassWithDefaultConstructor01(): Bool & Impure =
    import flix.test.TestClassWithDefaultConstructor.abstractMethod(Int32): Int32;
    import flix.test.TestClassWithDefaultConstructor.concreteMethod(String): String;
    let anon = object ##flix.test.TestClassWithDefaultConstructor {
      def abstractMethod(_this: ##flix.test.TestClassWithDefaultConstructor, x: Int32): Int32 = x + 1
    };
    abstractMethod(anon, 1) == 2 and
      concreteMethod(anon, "bar") == "foobar"

  @test
  def testClassWithDefaultConstructor02(): Bool & Impure =
    import flix.test.TestClassWithDefaultConstructor.abstractMethod(Int32): Int32;
    import flix.test.TestClassWithDefaultConstructor.concreteMethod(String): String;
    let anon = object ##flix.test.TestClassWithDefaultConstructor {
      def abstractMethod(_this: ##flix.test.TestClassWithDefaultConstructor, x: Int32): Int32 = x + 1
      def concreteMethod(_this: ##flix.test.TestClassWithDefaultConstructor, y: String): String = "flix: ${y}"
    };
    abstractMethod(anon, 1) == 2 and
      concreteMethod(anon, "bar") == "flix: bar"
}

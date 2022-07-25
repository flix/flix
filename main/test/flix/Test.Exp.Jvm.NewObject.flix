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
  def testVoidInterface01(): Bool & Impure =
    import static flix.test.TestVoidInterface.runTest(##flix.test.TestVoidInterface): Bool;
    let anon = object ##flix.test.TestVoidInterface {
      def testMethod(_this: ##flix.test.TestVoidInterface): Unit = ()
    };
    runTest(anon)

  @test
  def testBoolInterface01(): Bool & Impure =
    import static flix.test.TestBoolInterface.runTest(##flix.test.TestBoolInterface): Bool;
    let anon = object ##flix.test.TestBoolInterface {
      def testMethod(_this: ##flix.test.TestBoolInterface, x: Bool): Bool = not x
    };
    runTest(anon)

  @test
  def testCharInterface01(): Bool & Impure =
    import static flix.test.TestCharInterface.runTest(##flix.test.TestCharInterface): Bool;
    let anon = object ##flix.test.TestCharInterface {
      def testMethod(_this: ##flix.test.TestCharInterface, x: Char): Char = Char.toUpperCase(x)
    };
    runTest(anon)

  @test
  def testInt16Interface01(): Bool & Impure =
    import static flix.test.TestInt16Interface.runTest(##flix.test.TestInt16Interface): Bool;
    let anon = object ##flix.test.TestInt16Interface {
      def testMethod(_this: ##flix.test.TestInt16Interface, x: Int16): Int16 = x + 1i16
    };
    runTest(anon)

  @test
  def testInt32Interface01(): Bool & Impure =
    import static flix.test.TestInt32Interface.runTest(##flix.test.TestInt32Interface): Bool;
    let anon = object ##flix.test.TestInt32Interface {
      def testMethod(_this: ##flix.test.TestInt32Interface, x: Int32): Int32 = x + 1
    };
    runTest(anon)

  @test
  def testInt64Interface01(): Bool & Impure =
    import static flix.test.TestInt64Interface.runTest(##flix.test.TestInt64Interface): Bool;
    let anon = object ##flix.test.TestInt64Interface {
      def testMethod(_this: ##flix.test.TestInt64Interface, x: Int64): Int64 = x + 1i64
    };
    runTest(anon)

  @test
  def testFloat32Interface01(): Bool & Impure =
    import static flix.test.TestFloat32Interface.runTest(##flix.test.TestFloat32Interface): Bool;
    let anon = object ##flix.test.TestFloat32Interface {
      def testMethod(_this: ##flix.test.TestFloat32Interface, x: Float32): Float32 = x + 0.23f32
    };
    runTest(anon)

  @test
  def testFloat64Interface01(): Bool & Impure =
    import static flix.test.TestFloat64Interface.runTest(##flix.test.TestFloat64Interface): Bool;
    let anon = object ##flix.test.TestFloat64Interface {
      def testMethod(_this: ##flix.test.TestFloat64Interface, x: Float64): Float64 = x + 0.23f64
    };
    runTest(anon)

  @test 
  def testStackOffsets01(): Bool & Impure =
    import static flix.test.TestStackOffsets.runTest(##flix.test.TestStackOffsets): Bool;
    let anon = object ##flix.test.TestStackOffsets {
      def testMethod(_this: ##flix.test.TestStackOffsets, a: Bool, b: Char, c: Int8, d: Int16, e: Int32, f: Int64, g: Float32, h: Float64): String = 
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
  def testGenericInterface01(): Bool & Impure =
    import static flix.test.TestGenericInterface.runTest(##flix.test.TestGenericInterface): Bool;
    let anon = object ##flix.test.TestGenericInterface {
      def testMethod(_this: ##flix.test.TestGenericInterface, x: ##java.lang.Object): ##java.lang.Object = 
        let str = x as String;
        "${str}, ${str}" as ##java.lang.Object
    };
    runTest(anon)

  @test
  def testGenericMethod01(): Bool & Impure =
    import static flix.test.TestGenericMethod.runTest(##flix.test.TestGenericMethod): Bool;
    let anon = object ##flix.test.TestGenericMethod {
      def testMethod(_this: ##flix.test.TestGenericMethod, x: ##java.lang.Object): ##java.lang.Object = 
        let str = x as String;
        "${str}, ${str}" as ##java.lang.Object
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

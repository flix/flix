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
      def takesAndReturnsInt(_this: ##flix.test.TestPrimitiveTypes, x: Int32): Int32 = x + 1
    };
    runTest(anon)
}

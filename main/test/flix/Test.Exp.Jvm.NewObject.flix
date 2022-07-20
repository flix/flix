namespace Test/Exp/Jvm/NewObject {

  def implementSerializable(): ##java.io.Serializable & Impure =
    object ##java.io.Serializable { }

  def implementSerializableAgain(): ##java.io.Serializable & Impure =
    object ##java.io.Serializable { }

  def implementIterable(): ##java.lang.Iterable & Impure =
    object ##java.lang.Iterable { 
      def iterator(_this: ##java.lang.Object): ##java.util.Iterator & Impure = () as ##java.util.Iterator
    }

  def implementComparable(): ##java.lang.Comparable & Impure =
    object ##java.lang.Comparable {
      def compareTo(_this: ##java.lang.Object, _that: ##java.lang.Object): Int32 & Pure = 0
    }

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
  def testImplementInterface06(): Bool & Impure =
    import java.lang.Object.toString(): String & Pure;
    let anon = implementIterable() as ##java.lang.Object;
    toString(anon) |> String.startsWith(prefix = "Anon")

  @test
  def testImplementInterface07(): Bool & Impure =
    import java.lang.Iterable.iterator(): ##java.util.Iterator & Impure;
    let anon = implementIterable();
    try {
      let _it = iterator(anon);
      false
    } catch {
      case _: ##java.lang.UnsupportedOperationException => true
    }
  
  @test
  def testImplementInterface08(): Bool & Impure =
    import java.lang.Comparable.compareTo(##java.lang.Object): Int32 & Pure;
    let anon = implementComparable();
    try {
      compareTo(anon, anon as ##java.lang.Object) == 0
    } catch {
      case _: ##java.lang.UnsupportedOperationException => true
    }
}

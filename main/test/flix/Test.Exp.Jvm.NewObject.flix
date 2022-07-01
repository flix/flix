namespace Test/Exp/Jvm/NewObject {

  @test
  def testImplementInterface01(): Bool & Impure =
    import java.lang.Object.toString(): String & Pure;
    let anon = object ##java.io.Serializable { } as ##java.lang.Object;
    toString(anon) |> String.startsWith(prefix = "HardcodedAnon@")

  @test
  def implementInterface02(): Bool & Impure =
    import java.lang.Object.hashCode(): Int32 & Pure;
    let anon = object ##java.io.Serializable { } as ##java.lang.Object;
    let anon2 = object ##java.io.Serializable { } as ##java.lang.Object;
    hashCode(anon) != hashCode(anon2)

  @test
  def implementInterface03(): Bool & Impure =
    import java.lang.Object.equals(##java.lang.Object): Bool & Pure;
    let anon = object ##java.io.Serializable { } as ##java.lang.Object;
    let anon2 = object ##java.io.Serializable { } as ##java.lang.Object;
    not equals(anon, anon2)

  @test
  def implementInterface04(): Bool & Impure =
    import java.lang.Object.toString(): String & Pure;
    let anon = object ##java.lang.Iterable { } as ##java.lang.Object;
    toString(anon) |> String.startsWith(prefix = "HardcodedAnon@")
}

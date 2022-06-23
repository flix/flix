namespace Test/Exp/Jvm/NewObject {

  @test
  def testImplementInterface01(): Bool & Impure =
    import java.lang.Object.toString(): String & Pure;
    let anon = object ##java.io.Serializable { } as ##java.lang.Object;
    toString(anon) |> String.startsWith(prefix = "HardcodedAnon@")
}

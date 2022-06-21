namespace Test/Exp/Jvm/NewObject {

  @test
  def testImplementInterface01(): Bool & Impure =
    let _anon = object ##java.io.Serializable { };
    true
}

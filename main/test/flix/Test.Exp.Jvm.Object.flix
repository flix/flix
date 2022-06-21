namespace Test/Exp/Jvm/Object {

  @test
  def testImplementInterface01(): Bool & Impure =
    let _anon = object ##java.io.Serializable { };
    true
}

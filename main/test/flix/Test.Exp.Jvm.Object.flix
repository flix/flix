namespace Test/Exp/Jvm/Object {

  @test
  def testImplementInterface01(): Bool & Impure =
    let anon = object ##flix.test.TestInterface { };
    true
}

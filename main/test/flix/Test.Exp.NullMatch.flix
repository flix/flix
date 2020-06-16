namespace Test/Exp/NullMatch {

    @test
    def testNullMatch01(): Bool =
        match? null {
          case null => true
          case _    => false
        }

}

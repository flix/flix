namespace Test/Exp/New {

    @test
    def testNew01(): MutList[Int32, static] & Impure =
        new MutList(static): MutList[Int32, static]

}

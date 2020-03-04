namespace Test/Exp/Binary/Spaceship {

    @test
    def testBinarySpaceship01(): Bool = () <=> () == 0

    @test
    def testBinarySpaceship02(): Bool = true <=> true == 0

    @test
    def testBinarySpaceship03(): Bool = true <=> false > 0

    @test
    def testBinarySpaceship04(): Bool = false <=> true < 0

    @test
    def testBinarySpaceship05(): Bool = true <=> true == 0


}
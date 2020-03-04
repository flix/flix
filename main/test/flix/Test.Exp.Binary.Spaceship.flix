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

    @test
    def testBinarySpaceship06(): Bool = 'a' <=> 'a' == 0

    @test
    def testBinarySpaceship07(): Bool = 'a' <=> 'z' < 0

    @test
    def testBinarySpaceship08(): Bool = 'z' <=> 'a' > 0

    @test
    def testBinarySpaceship09(): Bool = 21.42f32 <=> 21.42f32 == 0

    @test
    def testBinarySpaceship10(): Bool = 123.456f32 <=> 456.789f32 < 0

    @test
    def testBinarySpaceship11(): Bool = 456.789f32 <=> 123.456f32 > 0

    @test
    def testBinarySpaceship12(): Bool = 21.42f64 <=> 21.42f64 == 0

    @test
    def testBinarySpaceship13(): Bool = 123.456f64 <=> 456.789f64 < 0

    @test
    def testBinarySpaceship14(): Bool = 456.789f64 <=> 123.456f64 > 0

    @test
    def testBinarySpaceship15(): Bool = 123i8 <=> 123i8 == 0

    @test
    def testBinarySpaceship16(): Bool = 123i8 <=> 127i8 < 0

    @test
    def testBinarySpaceship17(): Bool = 127i8 <=> 123i8 > 0


}
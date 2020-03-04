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

    @test
    def testBinarySpaceship18(): Bool = 21i16 <=> 21i16 == 0

    @test
    def testBinarySpaceship19(): Bool = 21i16 <=> 42i16 < 0

    @test
    def testBinarySpaceship20(): Bool = 42i16 <=> 21i16 > 0

    @test
    def testBinarySpaceship21(): Bool = 21i32 <=> 21i32 == 0

    @test
    def testBinarySpaceship22(): Bool = 21i32 <=> 42i32 < 0

    @test
    def testBinarySpaceship23(): Bool = 42i32 <=> 21i32 > 0

    @test
    def testBinarySpaceship24(): Bool = 21i64 <=> 21i64 == 0

    @test
    def testBinarySpaceship25(): Bool = 21i64 <=> 42i64 < 0

    @test
    def testBinarySpaceship26(): Bool = 42i64 <=> 21i64 > 0

    @test
    def testBinarySpaceship27(): Bool = 21ii <=> 21ii == 0

    @test
    def testBinarySpaceship28(): Bool = 21ii <=> 42ii < 0

    @test
    def testBinarySpaceship29(): Bool = 42ii <=> 21ii > 0

    @test
    def testBinarySpaceship30(): Bool = "hello" <=> "hello" == 0

    @test
    def testBinarySpaceship31(): Bool = "hello" <=> "world" < 0

    @test
    def testBinarySpaceship32(): Bool = "world" <=> "hello" > 0

    @test
    def testBinarySpaceship33(): Bool = None <=> None == 0

    @test
    def testBinarySpaceship34(): Bool = Some(123) <=> Some(123) == 0

    @test
    def testBinarySpaceship35(): Bool = None <=> Some(123) < 0

    @test
    def testBinarySpaceship36(): Bool = Some(123) <=> None > 0

}
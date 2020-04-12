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

    @test
    def testBinarySpaceship37(): Bool = Ok(123) <=> Ok(123) == 0

    @test
    def testBinarySpaceship38(): Bool = Ok(123) <=> Ok(456) < 0

    @test
    def testBinarySpaceship39(): Bool = Ok(456) <=> Ok(123) > 0

    @test
    def testBinarySpaceship40(): Bool = Err(123) <=> Err(123) == 0

    @test
    def testBinarySpaceship41(): Bool = Err(123) <=> Err(456) < 0

    @test
    def testBinarySpaceship42(): Bool = Err(456) <=> Err(123) > 0

    @test
    def testBinarySpaceship43(): Bool = Ok(123) <=> Err(123) < 0

    @test
    def testBinarySpaceship44(): Bool = Err(123) <=> Ok(123) > 0

    @test
    def testBinarySpaceship45(): Bool = None <=> Some(Ok(123)) < 0

    @test
    def testBinarySpaceship46(): Bool = Some(Ok(123)) <=> None > 0

    @test
    def testBinarySpaceship47(): Bool = Some(Ok(123)) <=> Some(Ok(123)) == 0

    @test
    def testBinarySpaceship48(): Bool = Some(Ok(123)) <=> Some(Ok(456)) < 0

    @test
    def testBinarySpaceship49(): Bool = Some(Ok(456)) <=> Some(Ok(123)) > 0

    @test
    def testBinarySpaceship50(): Bool = Some(Ok(123)) <=> Some(Err("hello")) < 0

    @test
    def testBinarySpaceship51(): Bool = Some(Err("hello")) <=> Some(Ok(123)) > 0

    @test
    def testBinarySpaceship52(): Bool = Some(Ok(None)) <=> Some(Ok(None)) == 0

    @test
    def testBinarySpaceship53(): Bool = Some(Ok(None)) <=> Some(Ok(Some(123))) < 0

    @test
    def testBinarySpaceship54(): Bool = Some(Ok(Some(123))) <=> Some(Ok(None)) > 0

    @test
    def testBinarySpaceship55(): Bool = (111, 222) <=> (111, 222) == 0

    @test
    def testBinarySpaceship56(): Bool = (111, 111) <=> (111, 222) < 0

    @test
    def testBinarySpaceship57(): Bool = (111, 222) <=> (111, 111) > 0

    @test
    def testBinarySpaceship58(): Bool = (111, 111) <=> (222, 111) < 0

    @test
    def testBinarySpaceship59(): Bool = (222, 111) <=> (111, 111) > 0

    @test
    def testBinarySpaceship60(): Bool = (1, 2, 3) <=> (1, 2, 3) == 0

    @test
    def testBinarySpaceship61(): Bool = (1, 2, 3) <=> (1, 2, 4) < 0

    @test
    def testBinarySpaceship62(): Bool = (1, 2, 4) <=> (1, 2, 3) > 0

    @test
    def testBinarySpaceship63(): Bool = (1, 0, 3) <=> (1, 2, 3) < 0

    @test
    def testBinarySpaceship64(): Bool = (1, 2, 3) <=> (1, 0, 3) > 0

    @test
    def testBinarySpaceship65(): Bool = (1, 2, 3) <=> (4, 2, 3) < 0

    @test
    def testBinarySpaceship66(): Bool = (4, 2, 3) <=> (1, 2, 3) > 0

    @test
    def testBinarySpaceship67(): Bool = Red <=> Red == 0

    @test
    def testBinarySpaceship68(): Bool = Blu <=> Blu == 0

    @test
    def testBinarySpaceship69(): Bool = Red <=> Blu < 0

    @test
    def testBinarySpaceship70(): Bool = Blu <=> Red > 0

    @test
    def testBinarySpaceship71(): Bool = Some(Red) <=> Some(Blu) < 0

    @test
    def testBinarySpaceship72(): Bool = Some(Blu) <=> Some(Red) > 0

    enum Color {
        case Red,
        case Blu
    }

    def __cmp(c1: Color, c2: Color): Int32 = match (c1, c2) {
        case (Red, Red) =>  0
        case (Blu, Blu) =>  0
        case (Red, Blu) => -1
        case (Blu, Red) => +1
    }

}
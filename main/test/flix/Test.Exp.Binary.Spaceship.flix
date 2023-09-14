mod Test.Exp.Binary.Spaceship {
    use Comparison.{EqualTo, GreaterThan, LessThan}

    @Test
    def testBinarySpaceship01(): Bool = (() <=> ()) == EqualTo

    @Test
    def testBinarySpaceship02(): Bool = (true <=> true) == EqualTo

    @Test
    def testBinarySpaceship03(): Bool = (true <=> false) == GreaterThan

    @Test
    def testBinarySpaceship04(): Bool = (false <=> true) == LessThan

    @Test
    def testBinarySpaceship05(): Bool = (true <=> true) == EqualTo

    @Test
    def testBinarySpaceship06(): Bool = ('a' <=> 'a') == EqualTo

    @Test
    def testBinarySpaceship07(): Bool = ('a' <=> 'z') == LessThan

    @Test
    def testBinarySpaceship08(): Bool = ('z' <=> 'a') == GreaterThan

    @Test
    def testBinarySpaceship09(): Bool = (21.42f32 <=> 21.42f32) == EqualTo

    @Test
    def testBinarySpaceship10(): Bool = (123.456f32 <=> 456.789f32) == LessThan

    @Test
    def testBinarySpaceship11(): Bool = (456.789f32 <=> 123.456f32) == GreaterThan

    @Test
    def testBinarySpaceship12(): Bool = (21.42f64 <=> 21.42f64) == EqualTo

    @Test
    def testBinarySpaceship13(): Bool = (123.456f64 <=> 456.789f64) == LessThan

    @Test
    def testBinarySpaceship14(): Bool = (456.789f64 <=> 123.456f64) == GreaterThan

    @Test
    def testBinarySpaceship15(): Bool = (123i8 <=> 123i8) == EqualTo

    @Test
    def testBinarySpaceship16(): Bool = (123i8 <=> 127i8) == LessThan

    @Test
    def testBinarySpaceship17(): Bool = (127i8 <=> 123i8) == GreaterThan

    @Test
    def testBinarySpaceship18(): Bool = (21i16 <=> 21i16) == EqualTo

    @Test
    def testBinarySpaceship19(): Bool = (21i16 <=> 42i16) == LessThan

    @Test
    def testBinarySpaceship20(): Bool = (42i16 <=> 21i16) == GreaterThan

    @Test
    def testBinarySpaceship21(): Bool = (21i32 <=> 21i32) == EqualTo

    @Test
    def testBinarySpaceship22(): Bool = (21i32 <=> 42i32) == LessThan

    @Test
    def testBinarySpaceship23(): Bool = (42i32 <=> 21i32) == GreaterThan

    @Test
    def testBinarySpaceship24(): Bool = (21i64 <=> 21i64) == EqualTo

    @Test
    def testBinarySpaceship25(): Bool = (21i64 <=> 42i64) == LessThan

    @Test
    def testBinarySpaceship26(): Bool = (42i64 <=> 21i64) == GreaterThan

    @Test
    def testBinarySpaceship27(): Bool = (21ii <=> 21ii) == EqualTo

    @Test
    def testBinarySpaceship28(): Bool = (21ii <=> 42ii) == LessThan

    @Test
    def testBinarySpaceship29(): Bool = (42ii <=> 21ii) == GreaterThan

    @Test
    def testBinarySpaceship30(): Bool = ("hello" <=> "hello") == EqualTo

    @Test
    def testBinarySpaceship31(): Bool = ("hello" <=> "world") == LessThan

    @Test
    def testBinarySpaceship32(): Bool = ("world" <=> "hello") == GreaterThan

    @Test
    def testBinarySpaceship33(): Bool = ((None: Option[Unit]) <=> None) == EqualTo

    @Test
    def testBinarySpaceship34(): Bool = (Some(123) <=> Some(123)) == EqualTo

    @Test
    def testBinarySpaceship35(): Bool = ((None: Option[Int32]) <=> Some(123)) == LessThan

    @Test
    def testBinarySpaceship36(): Bool = (Some(123) <=> None) == GreaterThan

    @Test
    def testBinarySpaceship37(): Bool = ((Ok(123): Result[Unit, _]) <=> Ok(123)) == EqualTo

    @Test
    def testBinarySpaceship38(): Bool = ((Ok(123): Result[Unit, _]) <=> Ok(456)) == LessThan

    @Test
    def testBinarySpaceship39(): Bool = ((Ok(456): Result[Unit, _]) <=> Ok(123)) == GreaterThan

    @Test
    def testBinarySpaceship40(): Bool = ((Err(123): Result[_, Unit]) <=> Err(123)) == EqualTo

    @Test
    def testBinarySpaceship41(): Bool = ((Err(123): Result[_, Unit]) <=> Err(456)) == LessThan

    @Test
    def testBinarySpaceship42(): Bool = ((Err(456): Result[_, Unit]) <=> Err(123)) == GreaterThan

    @Test
    def testBinarySpaceship43(): Bool = (Ok(123) <=> Err(123)) == LessThan

    @Test
    def testBinarySpaceship44(): Bool = (Err(123) <=> Ok(123)) == GreaterThan

    @Test
    def testBinarySpaceship45(): Bool = ((None: Option[Result[Unit, _]]) <=> Some(Ok(123))) == LessThan

    @Test
    def testBinarySpaceship46(): Bool = ((Some(Ok(123)): Option[Result[Unit, _]]) <=> None) == GreaterThan

    @Test
    def testBinarySpaceship47(): Bool = ((Some(Ok(123)): Option[Result[Unit, _]]) <=> Some(Ok(123))) == EqualTo

    @Test
    def testBinarySpaceship48(): Bool = ((Some(Ok(123)): Option[Result[Unit, _]]) <=> Some(Ok(456))) == LessThan

    @Test
    def testBinarySpaceship49(): Bool = ((Some(Ok(456)): Option[Result[Unit, _]]) <=> Some(Ok(123))) == GreaterThan

    @Test
    def testBinarySpaceship50(): Bool = (Some(Ok(123)) <=> Some(Err("hello"))) == LessThan

    @Test
    def testBinarySpaceship51(): Bool = (Some(Err("hello")) <=> Some(Ok(123))) == GreaterThan

    @Test
    def testBinarySpaceship52(): Bool = ((Some(Ok(None)): Option[Result[Unit, Option[Unit]]]) <=> Some(Ok(None))) == EqualTo

    @Test
    def testBinarySpaceship53(): Bool = ((Some(Ok(None)): Option[Result[Unit, Option[Int32]]]) <=> Some(Ok(Some(123)))) == LessThan

    @Test
    def testBinarySpaceship54(): Bool = ((Some(Ok(Some(123))): Option[Result[Unit, Option[Int32]]]) <=> Some(Ok(None))) == GreaterThan

    @Test
    def testBinarySpaceship55(): Bool = ((111, 222) <=> (111, 222)) == EqualTo

    @Test
    def testBinarySpaceship56(): Bool = ((111, 111) <=> (111, 222)) == LessThan

    @Test
    def testBinarySpaceship57(): Bool = ((111, 222) <=> (111, 111)) == GreaterThan

    @Test
    def testBinarySpaceship58(): Bool = ((111, 111) <=> (222, 111)) == LessThan

    @Test
    def testBinarySpaceship59(): Bool = ((222, 111) <=> (111, 111)) == GreaterThan

    @Test
    def testBinarySpaceship60(): Bool = ((1, 2, 3) <=> (1, 2, 3)) == EqualTo

    @Test
    def testBinarySpaceship61(): Bool = ((1, 2, 3) <=> (1, 2, 4)) == LessThan

    @Test
    def testBinarySpaceship62(): Bool = ((1, 2, 4) <=> (1, 2, 3)) == GreaterThan

    @Test
    def testBinarySpaceship63(): Bool = ((1, 0, 3) <=> (1, 2, 3)) == LessThan

    @Test
    def testBinarySpaceship64(): Bool = ((1, 2, 3) <=> (1, 0, 3)) == GreaterThan

    @Test
    def testBinarySpaceship65(): Bool = ((1, 2, 3) <=> (4, 2, 3)) == LessThan

    @Test
    def testBinarySpaceship66(): Bool = ((4, 2, 3) <=> (1, 2, 3)) == GreaterThan

    @Test
    def testBinarySpaceship67(): Bool = (Color.Red <=> Color.Red) == EqualTo

    @Test
    def testBinarySpaceship68(): Bool = (Color.Blu <=> Color.Blu) == EqualTo

    @Test
    def testBinarySpaceship69(): Bool = (Color.Red <=> Color.Blu) == LessThan

    @Test
    def testBinarySpaceship70(): Bool = (Color.Blu <=> Color.Red) == GreaterThan

    @Test
    def testBinarySpaceship71(): Bool = (Some(Color.Red) <=> Some(Color.Blu)) == LessThan

    @Test
    def testBinarySpaceship72(): Bool = (Some(Color.Blu) <=> Some(Color.Red)) == GreaterThan

    enum Color {
        case Red,
        case Blu
    }

    instance Eq[Color] {
        pub def eq(c1: Color, c2: Color): Bool = match (c1, c2) {
            case (Color.Red, Color.Red) => true
            case (Color.Blu, Color.Blu) => true
            case (Color.Red, Color.Blu) => false
            case (Color.Blu, Color.Red) => false
        }
    }

    instance Order[Color] {
        pub def compare(c1: Color, c2: Color): Comparison = match (c1, c2) {
            case (Color.Red, Color.Red) => EqualTo
            case (Color.Blu, Color.Blu) => EqualTo
            case (Color.Red, Color.Blu) => LessThan
            case (Color.Blu, Color.Red) => GreaterThan
        }
    }

}

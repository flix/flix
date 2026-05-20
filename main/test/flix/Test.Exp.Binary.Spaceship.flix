mod Test.Exp.Binary.Spaceship {

    use Assert.assertEq
    use Comparison.{EqualTo, GreaterThan, LessThan}

    @Test
    def testBinarySpaceship01(): Unit \ Assert =
        assertEq(expected = EqualTo, () <=> ())

    @Test
    def testBinarySpaceship02(): Unit \ Assert =
        assertEq(expected = EqualTo, true <=> true)

    @Test
    def testBinarySpaceship03(): Unit \ Assert =
        assertEq(expected = GreaterThan, true <=> false)

    @Test
    def testBinarySpaceship04(): Unit \ Assert =
        assertEq(expected = LessThan, false <=> true)

    @Test
    def testBinarySpaceship05(): Unit \ Assert =
        assertEq(expected = EqualTo, true <=> true)

    @Test
    def testBinarySpaceship06(): Unit \ Assert =
        assertEq(expected = EqualTo, 'a' <=> 'a')

    @Test
    def testBinarySpaceship07(): Unit \ Assert =
        assertEq(expected = LessThan, 'a' <=> 'z')

    @Test
    def testBinarySpaceship08(): Unit \ Assert =
        assertEq(expected = GreaterThan, 'z' <=> 'a')

    @Test
    def testBinarySpaceship09(): Unit \ Assert =
        assertEq(expected = EqualTo, 21.42f32 <=> 21.42f32)

    @Test
    def testBinarySpaceship10(): Unit \ Assert =
        assertEq(expected = LessThan, 123.456f32 <=> 456.789f32)

    @Test
    def testBinarySpaceship11(): Unit \ Assert =
        assertEq(expected = GreaterThan, 456.789f32 <=> 123.456f32)

    @Test
    def testBinarySpaceship12(): Unit \ Assert =
        assertEq(expected = EqualTo, 21.42f64 <=> 21.42f64)

    @Test
    def testBinarySpaceship13(): Unit \ Assert =
        assertEq(expected = LessThan, 123.456f64 <=> 456.789f64)

    @Test
    def testBinarySpaceship14(): Unit \ Assert =
        assertEq(expected = GreaterThan, 456.789f64 <=> 123.456f64)

    @Test
    def testBinarySpaceship15(): Unit \ Assert =
        assertEq(expected = EqualTo, 123i8 <=> 123i8)

    @Test
    def testBinarySpaceship16(): Unit \ Assert =
        assertEq(expected = LessThan, 123i8 <=> 127i8)

    @Test
    def testBinarySpaceship17(): Unit \ Assert =
        assertEq(expected = GreaterThan, 127i8 <=> 123i8)

    @Test
    def testBinarySpaceship18(): Unit \ Assert =
        assertEq(expected = EqualTo, 21i16 <=> 21i16)

    @Test
    def testBinarySpaceship19(): Unit \ Assert =
        assertEq(expected = LessThan, 21i16 <=> 42i16)

    @Test
    def testBinarySpaceship20(): Unit \ Assert =
        assertEq(expected = GreaterThan, 42i16 <=> 21i16)

    @Test
    def testBinarySpaceship21(): Unit \ Assert =
        assertEq(expected = EqualTo, 21i32 <=> 21i32)

    @Test
    def testBinarySpaceship22(): Unit \ Assert =
        assertEq(expected = LessThan, 21i32 <=> 42i32)

    @Test
    def testBinarySpaceship23(): Unit \ Assert =
        assertEq(expected = GreaterThan, 42i32 <=> 21i32)

    @Test
    def testBinarySpaceship24(): Unit \ Assert =
        assertEq(expected = EqualTo, 21i64 <=> 21i64)

    @Test
    def testBinarySpaceship25(): Unit \ Assert =
        assertEq(expected = LessThan, 21i64 <=> 42i64)

    @Test
    def testBinarySpaceship26(): Unit \ Assert =
        assertEq(expected = GreaterThan, 42i64 <=> 21i64)

    @Test
    def testBinarySpaceship27(): Unit \ Assert =
        assertEq(expected = EqualTo, 21ii <=> 21ii)

    @Test
    def testBinarySpaceship28(): Unit \ Assert =
        assertEq(expected = LessThan, 21ii <=> 42ii)

    @Test
    def testBinarySpaceship29(): Unit \ Assert =
        assertEq(expected = GreaterThan, 42ii <=> 21ii)

    @Test
    def testBinarySpaceship30(): Unit \ Assert =
        assertEq(expected = EqualTo, "hello" <=> "hello")

    @Test
    def testBinarySpaceship31(): Unit \ Assert =
        assertEq(expected = LessThan, "hello" <=> "world")

    @Test
    def testBinarySpaceship32(): Unit \ Assert =
        assertEq(expected = GreaterThan, "world" <=> "hello")

    @Test
    def testBinarySpaceship33(): Unit \ Assert =
        assertEq(expected = EqualTo, (None: Option[Unit]) <=> None)

    @Test
    def testBinarySpaceship34(): Unit \ Assert =
        assertEq(expected = EqualTo, Some(123) <=> Some(123))

    @Test
    def testBinarySpaceship35(): Unit \ Assert =
        assertEq(expected = LessThan, (None: Option[Int32]) <=> Some(123))

    @Test
    def testBinarySpaceship36(): Unit \ Assert =
        assertEq(expected = GreaterThan, Some(123) <=> None)

    @Test
    def testBinarySpaceship37(): Unit \ Assert =
        assertEq(expected = EqualTo, (Ok(123): Result[Unit, _]) <=> Ok(123))

    @Test
    def testBinarySpaceship38(): Unit \ Assert =
        assertEq(expected = LessThan, (Ok(123): Result[Unit, _]) <=> Ok(456))

    @Test
    def testBinarySpaceship39(): Unit \ Assert =
        assertEq(expected = GreaterThan, (Ok(456): Result[Unit, _]) <=> Ok(123))

    @Test
    def testBinarySpaceship40(): Unit \ Assert =
        assertEq(expected = EqualTo, (Err(123): Result[_, Unit]) <=> Err(123))

    @Test
    def testBinarySpaceship41(): Unit \ Assert =
        assertEq(expected = LessThan, (Err(123): Result[_, Unit]) <=> Err(456))

    @Test
    def testBinarySpaceship42(): Unit \ Assert =
        assertEq(expected = GreaterThan, (Err(456): Result[_, Unit]) <=> Err(123))

    @Test
    def testBinarySpaceship43(): Unit \ Assert =
        assertEq(expected = LessThan, Ok(123) <=> Err(123))

    @Test
    def testBinarySpaceship44(): Unit \ Assert =
        assertEq(expected = GreaterThan, Err(123) <=> Ok(123))

    @Test
    def testBinarySpaceship45(): Unit \ Assert =
        assertEq(expected = LessThan, (None: Option[Result[Unit, _]]) <=> Some(Ok(123)))

    @Test
    def testBinarySpaceship46(): Unit \ Assert =
        assertEq(expected = GreaterThan, (Some(Ok(123)): Option[Result[Unit, _]]) <=> None)

    @Test
    def testBinarySpaceship47(): Unit \ Assert =
        assertEq(expected = EqualTo, (Some(Ok(123)): Option[Result[Unit, _]]) <=> Some(Ok(123)))

    @Test
    def testBinarySpaceship48(): Unit \ Assert =
        assertEq(expected = LessThan, (Some(Ok(123)): Option[Result[Unit, _]]) <=> Some(Ok(456)))

    @Test
    def testBinarySpaceship49(): Unit \ Assert =
        assertEq(expected = GreaterThan, (Some(Ok(456)): Option[Result[Unit, _]]) <=> Some(Ok(123)))

    @Test
    def testBinarySpaceship50(): Unit \ Assert =
        assertEq(expected = LessThan, Some(Ok(123)) <=> Some(Err("hello")))

    @Test
    def testBinarySpaceship51(): Unit \ Assert =
        assertEq(expected = GreaterThan, Some(Err("hello")) <=> Some(Ok(123)))

    @Test
    def testBinarySpaceship52(): Unit \ Assert =
        assertEq(expected = EqualTo, (Some(Ok(None)): Option[Result[Unit, Option[Unit]]]) <=> Some(Ok(None)))

    @Test
    def testBinarySpaceship53(): Unit \ Assert =
        assertEq(expected = LessThan, (Some(Ok(None)): Option[Result[Unit, Option[Int32]]]) <=> Some(Ok(Some(123))))

    @Test
    def testBinarySpaceship54(): Unit \ Assert =
        assertEq(expected = GreaterThan, (Some(Ok(Some(123))): Option[Result[Unit, Option[Int32]]]) <=> Some(Ok(None)))

    @Test
    def testBinarySpaceship55(): Unit \ Assert =
        assertEq(expected = EqualTo, (111, 222) <=> (111, 222))

    @Test
    def testBinarySpaceship56(): Unit \ Assert =
        assertEq(expected = LessThan, (111, 111) <=> (111, 222))

    @Test
    def testBinarySpaceship57(): Unit \ Assert =
        assertEq(expected = GreaterThan, (111, 222) <=> (111, 111))

    @Test
    def testBinarySpaceship58(): Unit \ Assert =
        assertEq(expected = LessThan, (111, 111) <=> (222, 111))

    @Test
    def testBinarySpaceship59(): Unit \ Assert =
        assertEq(expected = GreaterThan, (222, 111) <=> (111, 111))

    @Test
    def testBinarySpaceship60(): Unit \ Assert =
        assertEq(expected = EqualTo, (1, 2, 3) <=> (1, 2, 3))

    @Test
    def testBinarySpaceship61(): Unit \ Assert =
        assertEq(expected = LessThan, (1, 2, 3) <=> (1, 2, 4))

    @Test
    def testBinarySpaceship62(): Unit \ Assert =
        assertEq(expected = GreaterThan, (1, 2, 4) <=> (1, 2, 3))

    @Test
    def testBinarySpaceship63(): Unit \ Assert =
        assertEq(expected = LessThan, (1, 0, 3) <=> (1, 2, 3))

    @Test
    def testBinarySpaceship64(): Unit \ Assert =
        assertEq(expected = GreaterThan, (1, 2, 3) <=> (1, 0, 3))

    @Test
    def testBinarySpaceship65(): Unit \ Assert =
        assertEq(expected = LessThan, (1, 2, 3) <=> (4, 2, 3))

    @Test
    def testBinarySpaceship66(): Unit \ Assert =
        assertEq(expected = GreaterThan, (4, 2, 3) <=> (1, 2, 3))

    @Test
    def testBinarySpaceship67(): Unit \ Assert =
        assertEq(expected = EqualTo, Color.Red <=> Color.Red)

    @Test
    def testBinarySpaceship68(): Unit \ Assert =
        assertEq(expected = EqualTo, Color.Blu <=> Color.Blu)

    @Test
    def testBinarySpaceship69(): Unit \ Assert =
        assertEq(expected = LessThan, Color.Red <=> Color.Blu)

    @Test
    def testBinarySpaceship70(): Unit \ Assert =
        assertEq(expected = GreaterThan, Color.Blu <=> Color.Red)

    @Test
    def testBinarySpaceship71(): Unit \ Assert =
        assertEq(expected = LessThan, Some(Color.Red) <=> Some(Color.Blu))

    @Test
    def testBinarySpaceship72(): Unit \ Assert =
        assertEq(expected = GreaterThan, Some(Color.Blu) <=> Some(Color.Red))

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

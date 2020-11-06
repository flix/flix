namespace Test/Exp/Tuple {

    @test
    def testUnitTuple01(): () = ()

    @test
    def testBoolTuple01(): (Bool, Bool) = (true, false)

    @test
    def testBoolTuple02(): (Bool, Bool, Bool) = (true, false, true)

    @test
    def testCharTuple01(): (Char, Char) = ('a', 'z')

    @test
    def testCharTuple02(): (Char, Char, Char) = ('a', 'k', 'z')

    @test
    def testFloat32Tuple01(): (Float32, Float32) = (123.456f32, 456.789f32)

    @test
    def testFloat32Tuple02(): (Float32, Float32, Float32) = (123.456f32, 456.789f32, 789.123f32)

    @test
    def testFloat64Tuple01(): (Float64, Float64) = (123.456f64, 456.789f64)

    @test
    def testFloat64Tuple02(): (Float64, Float64, Float64) = (123.456f64, 456.789f64, 789.123f64)

    @test
    def testInt8Tuple01(): (Int8, Int8) = (1i8, 2i8)

    @test
    def testInt8Tuple02(): (Int8, Int8, Int8) = (1i8, 2i8, 3i8)

    @test
    def testInt16Tuple01(): (Int16, Int16) = (1i16, 2i16)

    @test
    def testInt16Tuple02(): (Int16, Int16, Int16) = (1i16, 2i16, 3i16)

    @test
    def testInt32Tuple01(): (Int32, Int32) = (1i32, 2i32)

    @test
    def testInt32Tuple02(): (Int32, Int32, Int32) = (1i32, 2i32, 3i32)

    @test
    def testInt64Tuple01(): (Int64, Int64) = (1i64, 2i64)

    @test
    def testInt64Tuple02(): (Int64, Int64, Int64) = (1i64, 2i64, 3i64)


///   /**
///     * A type constructor that represent the type of arbitrary-precision integers.
///     */
///   case object BigInt extends TypeConstructor {
///     def kind: Kind = Kind.Star
///   }

///   /**
///     * A type constructor that represent the type of strings.
///     */
///   case object Str extends TypeConstructor {
///     def kind: Kind = Kind.Star
///   }

///   /**
///     * A type constructor that represents the type of functions.
///     */
///   case class Arrow(arity: Int) extends TypeConstructor {
///     def kind: Kind = Kind.Bool ->: Kind.mkArrow(arity)
///   }

///   /**
///     * A type constructor that represents the type of empty records.
///     */
///   case object RecordEmpty extends TypeConstructor {
///     def kind: Kind = Kind.Record
///   }

///   /**
///     * A type constructor that represents the type of extended records.
///     */
///   case class RecordExtend(field: Name.Field) extends TypeConstructor {
///     /**
///       * The shape of an extended record is { field: type | rest }
///       */
///     def kind: Kind = Kind.Star ->: Kind.Record ->: Kind.Record
///   }

///   /**
///     * A type constructor that represents the type of empty schemas.
///     */
///   case object SchemaEmpty extends TypeConstructor {
///     def kind: Kind = Kind.Schema
///   }

///   /**
///     * A type constructor that represents the type of extended schemas.
///     */
///   case class SchemaExtend(pred: Name.Pred) extends TypeConstructor {
///     /**
///       * The shape of an extended schema is { name: type | rest }
///       */
///     def kind: Kind = Kind.Star ->: Kind.Schema ->: Kind.Schema
///   }

///   /**
///     * A type constructor that represent the type of arrays.
///     */
///   case object Array extends TypeConstructor {
///     /**
///       * The shape of an array is Array[t].
///       */
///     def kind: Kind = Kind.Star ->: Kind.Star
///   }

///   /**
///     * A type constructor that represent the type of channels.
///     */
///   case object Channel extends TypeConstructor {
///     /**
///       * The shape of a channel is Channel[t].
///       */
///     def kind: Kind = Kind.Star ->: Kind.Star
///   }

///   /**
///     * A type constructor that represent the type of lazy expressions.
///     */
///   case object Lazy extends TypeConstructor {
///     /**
///       * The shape of lazy is Lazy[t].
///       */
///     def kind: Kind = Kind.Star ->: Kind.Star
///   }

///   /**
///     * A type constructor that represent the type of tags.
///     */
///   case class Tag(sym: Symbol.EnumSym, tag: Name.Tag) extends TypeConstructor {
///     /**
///       * The shape of a tag is "like" a function `caseType` -> (`resultType`) -> *.
///       */
///     def kind: Kind = Kind.Star ->: Kind.Star ->: Kind.Star
///   }


    @test
    def testTupleTuple01(): (Int, (Int, Int)) = (1, (2, 3))

    @test
    def testTupleTuple02(): ((Int, Int), Int) = ((1, 2), 3)

    @test
    def testTupleTuple03(): ((Int, Int), (Int, Int)) = ((1, 2), (3, 4))

    @test
    def testTupleTuple04(): (Int, (Int, (Int, Int))) = (1, (2, (3, 4)))

    @test
    def testTupleTuple05(): (((Int, Int), Int), Int) = (((1, 2), 3), 4)

    @test
    def testTupleTuple06(): ((Int, Int), (Int, Int), (Int, Int)) = ((1, 2), (3, 4), (5, 6))

    @test
    def testOptionTuple01(): (Option[Int], Option[Int]) = (None, None)

    @test
    def testOptionTuple02(): (Option[Int], Option[Int]) = (None, Some(2))

    @test
    def testOptionTuple03(): (Option[Int], Option[Int]) = (Some(1), None)

    @test
    def testOptionTuple04(): (Option[Int], Option[Int]) = (Some(1), Some(2))

    @test
    def testOptionTuple05(): Option[(Int, Int)] = None

    @test
    def testOptionTuple06(): Option[(Int, Int)] = Some((1, 2))

    @test
    def testResultTuple01(): (Result[Int, Int], Result[Int, Int]) = (Ok(1), Ok(2))

    @test
    def testResultTuple02(): (Result[Int, Int], Result[Int, Int]) = (Ok(1), Err(2))

    @test
    def testResultTuple03(): (Result[Int, Int], Result[Int, Int]) = (Err(1), Ok(2))

    @test
    def testResultTuple04(): (Result[Int, Int], Result[Int, Int]) = (Err(1), Err(2))

    @test
    def testResultTuple05(): Result[Int, (Int, Int)] = Ok(1)

    @test
    def testResultTuple06(): Result[Int, (Int, Int)] = Err((1, 2))

    @test
    def testListTuple01(): (List[Int], List[Int]) = (Nil, Nil)

    @test
    def testListTuple02(): (List[Int], List[Int]) = (1 :: Nil, 2 :: Nil)

    @test
    def testListTuple03(): List[(Int, Int)] = Nil

    @test
    def testListTuple04(): List[(Int, Int)] = (1, 2) :: Nil

    @test
    def testListTuple05(): List[(Int, Int)] = (1, 2) :: (3, 4) :: Nil

    enum Option[t] {
        case None,
        case Some(t)
    }

    enum Result[t, e] {
        case Ok(t),
        case Err(e)
    }

    enum List[t] {
        case Nil,
        case Cons(t, List[t])
    }

}

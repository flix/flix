///////////////////////////////////////////////////////////////////////////////
/// Unit                                                                    ///
///////////////////////////////////////////////////////////////////////////////
rel UnitRel(x: Unit)

@test
def testUnitLit01(): Unit =
    let x = UnitRel(()).;
    let r = solve x;
    ()


///////////////////////////////////////////////////////////////////////////////
/// Bool                                                                    ///
///////////////////////////////////////////////////////////////////////////////
rel BoolRel(x: Bool)

@test
def testBoolLit01(): Unit =
    let x = BoolRel(true).;
    let r = solve x;
    ()

@test
def testBoolLit02(): Unit =
    let x = BoolRel(false).;
    let r = solve x;
    ()


///////////////////////////////////////////////////////////////////////////////
/// Char                                                                    ///
///////////////////////////////////////////////////////////////////////////////
rel CharRel(x: Char)

@test
def testCharLit01(): Unit =
    let x = CharRel(' ').;
    let r = solve x;
    ()

@test
def testCharLit02(): Unit =
    let x = CharRel('a').;
    let r = solve x;
    ()

@test
def testCharLit03(): Unit =
    let x = CharRel('x').;
    let r = solve x;
    ()


///////////////////////////////////////////////////////////////////////////////
/// Float32                                                                 ///
///////////////////////////////////////////////////////////////////////////////
rel Float32Rel(x: Float32)

@test
def testFloat32Lit01(): Unit =
    let x = Float32Rel(0.0f32).;
    let r = solve x;
    ()

@test
def testFloat32Lit02(): Unit =
    let x = Float32Rel(-123.456f32).;
    let r = solve x;
    ()


@test
def testFloat32Lit03(): Unit =
    let x = Float32Rel(123.567f32).;
    let r = solve x;
    ()


///////////////////////////////////////////////////////////////////////////////
/// Float64                                                                 ///
///////////////////////////////////////////////////////////////////////////////
rel Float64Rel(x: Float64)

@test
def testFloat64Lit01(): Unit =
    let x = Float64Rel(0.0f64).;
    let r = solve x;
    ()

@test
def testFloat64Lit02(): Unit =
    let x = Float64Rel(-123.456f64).;
    let r = solve x;
    ()


@test
def testFloat64Lit03(): Unit =
    let x = Float64Rel(123.567f64).;
    let r = solve x;
    ()


///////////////////////////////////////////////////////////////////////////////
/// Int8                                                                    ///
///////////////////////////////////////////////////////////////////////////////
rel Int8Rel(x: Int8)

@test
def testInt8Lit01(): Unit =
    let x = Int8Rel(0i8).;
    let r = solve x;
    ()

@test
def testInt8Lit02(): Unit =
    let x = Int8Rel(123i8).;
    let r = solve x;
    ()

@test
def testInt8Lit03(): Unit =
    let x = Int8Rel(-123i8).;
    let r = solve x;
    ()


///////////////////////////////////////////////////////////////////////////////
/// Int16                                                                   ///
///////////////////////////////////////////////////////////////////////////////
rel Int16Rel(x: Int16)

@test
def testInt16Lit01(): Unit =
    let x = Int16Rel(0i16).;
    let r = solve x;
    ()

@test
def testInt16Lit02(): Unit =
    let x = Int16Rel(123i16).;
    let r = solve x;
    ()

@test
def testInt16Lit03(): Unit =
    let x = Int16Rel(-123i16).;
    let r = solve x;
    ()


///////////////////////////////////////////////////////////////////////////////
/// Int32                                                                   ///
///////////////////////////////////////////////////////////////////////////////
rel Int32Rel(x: Int32)

@test
def testInt32Lit01(): Unit =
    let x = Int32Rel(0i32).;
    let r = solve x;
    ()

@test
def testInt32Lit02(): Unit =
    let x = Int32Rel(123i32).;
    let r = solve x;
    ()

@test
def testInt32Lit03(): Unit =
    let x = Int32Rel(-123i32).;
    let r = solve x;
    ()


///////////////////////////////////////////////////////////////////////////////
/// Int64                                                                   ///
///////////////////////////////////////////////////////////////////////////////
rel Int64Rel(x: Int64)

@test
def testInt64Lit01(): Unit =
    let x = Int64Rel(0i64).;
    let r = solve x;
    ()

@test
def testInt64Lit02(): Unit =
    let x = Int64Rel(123i64).;
    let r = solve x;
    ()

@test
def testInt64Lit03(): Unit =
    let x = Int64Rel(-123i64).;
    let r = solve x;
    ()


///////////////////////////////////////////////////////////////////////////////
/// BigInt                                                                  ///
///////////////////////////////////////////////////////////////////////////////
rel BigIntRel(x: BigInt)

@test
def testBigIntLit01(): Unit =
    let x = BigIntRel(0ii).;
    let r = solve x;
    ()

@test
def testBigIntLit02(): Unit =
    let x = BigIntRel(123ii).;
    let r = solve x;
    ()

@test
def testBigIntLit03(): Unit =
    let x = BigIntRel(-123ii).;
    let r = solve x;
    ()


///////////////////////////////////////////////////////////////////////////////
/// Str                                                                     ///
///////////////////////////////////////////////////////////////////////////////
rel StrRel(x: Str)

@test
def testStrLit01(): Unit =
    let x = StrRel("").;
    let r = solve x;
    ()

@test
def testStrLit02(): Unit =
    let x = StrRel("hello").;
    let r = solve x;
    ()

@test
def testStrLit03(): Unit =
    let x = StrRel("hello world!").;
    let r = solve x;
    ()


///////////////////////////////////////////////////////////////////////////////
/// Option                                                                  ///
///////////////////////////////////////////////////////////////////////////////
rel OptionRel(x: Option[Int])

// @test // TODO
def testOptionLit01(): Unit =
    let x = OptionRel(None).;
    let r = solve x;
    ()

// @test // TODO
def testOptionLit02(): Unit =
    let x = OptionRel(Some(123)).;
    let r = solve x;
    ()


///////////////////////////////////////////////////////////////////////////////
/// Result                                                                  ///
///////////////////////////////////////////////////////////////////////////////
rel ResultRel(x: Result[Int, Str])

// @test // TODO
def testResultLit01(): Unit =
    let x = ResultRel(Ok(123)).;
    let r = solve x;
    ()

// @test // TODO
def testResultLit02(): Unit =
    let x = ResultRel(Err("hello world!")).;
    let r = solve x;
    ()


///////////////////////////////////////////////////////////////////////////////
/// List                                                                    ///
///////////////////////////////////////////////////////////////////////////////
//rel ListRel(x: List[Int]) // TODO

//@test
//def testListLit01(): Unit =
//    let x = ListRel(Nil).;
//    let r = solve x;
//    ()
//
//@test
//def testListLit02(): Unit =
//    let x = ListRel(1 :: Nil).;
//    let r = solve x;
//    ()
//
//@test
//def testListLit03(): Unit =
//    let x = ListRel(1 :: 2 :: 3 :: Nil).;
//    let r = solve x;
//    ()
//


///////////////////////////////////////////////////////////////////////////////
/// Tuple                                                                   ///
///////////////////////////////////////////////////////////////////////////////
rel TupleRel(x: (Bool, Char, Int))

// @test // TODO
def testTupleLit01(): Unit =
    let x = TupleRel((true, 'a', 123)).;
    let r = solve x;
    ()

// @test // TODO
def testTupleLit02(): Unit =
    let x = TupleRel((false, 'x', 789)).;
    let r = solve x;
    ()


///////////////////////////////////////////////////////////////////////////////
/// General                                                                 ///
///////////////////////////////////////////////////////////////////////////////
rel A(x: Int, y: Int, z: Str)

@test
def testLit01(): #{ A } =
    let x = A(1, 2, "hello").;
    let y = A(3, 4, "world!").;
    solve x <+> y

@test
def testLit02(): #{ A } =
    let x = A(1, 1, "hello").;
    let y = A(1, 1, "world!").;
    solve x <+> y

@test
def testLit03(): #{ A } =
    let x = A(1, 1, "hello world").;
    let y = A(1, 1, v) :- A(1, 1, v).;
    solve x <+> y

@test
def testLit04(): #{ A } =
    let x = A(1, 1, "abc").;
    let y = A(w, w, "abc") :- A(w, w, "abc").;
    solve x <+> y

///////////////////////////////////////////////////////////////////////////////
/// Unit                                                                    ///
///////////////////////////////////////////////////////////////////////////////
rel UnitRel(x: Unit)

@test
def testUnitLit01(): #{ UnitRel } =
    let x = UnitRel(()).;
    solve x


///////////////////////////////////////////////////////////////////////////////
/// Bool                                                                    ///
///////////////////////////////////////////////////////////////////////////////
rel BoolRel(x: Bool)

@test
def testBoolLit01(): #{ BoolRel } =
    let x = BoolRel(true).;
    solve x

@test
def testBoolLit02(): #{ BoolRel } =
    let x = BoolRel(false).;
    solve x


///////////////////////////////////////////////////////////////////////////////
/// Char                                                                    ///
///////////////////////////////////////////////////////////////////////////////
rel CharRel(x: Char)

@test
def testCharLit01(): #{ CharRel } =
    let x = CharRel(' ').;
    solve x

@test
def testCharLit02(): #{ CharRel } =
    let x = CharRel('a').;
    solve x

@test
def testCharLit03(): #{ CharRel } =
    let x = CharRel('x').;
    solve x


///////////////////////////////////////////////////////////////////////////////
/// Float32                                                                 ///
///////////////////////////////////////////////////////////////////////////////
rel Float32Rel(x: Float32)

@test
def testFloat32Lit01(): #{ Float32Rel } =
    let x = Float32Rel(0.0f32).;
    solve x

@test
def testFloat32Lit02(): #{ Float32Rel } =
    let x = Float32Rel(-123.456f32).;
    solve x

@test
def testFloat32Lit03(): #{ Float32Rel } =
    let x = Float32Rel(123.567f32).;
    solve x


///////////////////////////////////////////////////////////////////////////////
/// Float64                                                                 ///
///////////////////////////////////////////////////////////////////////////////
rel Float64Rel(x: Float64)

@test
def testFloat64Lit01(): #{ Float64Rel } =
    let x = Float64Rel(0.0f64).;
    solve x

@test
def testFloat64Lit02(): #{ Float64Rel } =
    let x = Float64Rel(-123.456f64).;
    solve x


@test
def testFloat64Lit03(): #{ Float64Rel } =
    let x = Float64Rel(123.567f64).;
    solve x


///////////////////////////////////////////////////////////////////////////////
/// Int8                                                                    ///
///////////////////////////////////////////////////////////////////////////////
rel Int8Rel(x: Int8)

@test
def testInt8Lit01(): #{ Int8Rel } =
    let x = Int8Rel(0i8).;
    solve x

@test
def testInt8Lit02(): #{ Int8Rel } =
    let x = Int8Rel(123i8).;
    solve x

@test
def testInt8Lit03(): #{ Int8Rel } =
    let x = Int8Rel(-123i8).;
    solve x


///////////////////////////////////////////////////////////////////////////////
/// Int16                                                                   ///
///////////////////////////////////////////////////////////////////////////////
rel Int16Rel(x: Int16)

@test
def testInt16Lit01(): #{ Int16Rel } =
    let x = Int16Rel(0i16).;
    solve x

@test
def testInt16Lit02(): #{ Int16Rel } =
    let x = Int16Rel(123i16).;
    solve x

@test
def testInt16Lit03(): #{ Int16Rel } =
    let x = Int16Rel(-123i16).;
    solve x


///////////////////////////////////////////////////////////////////////////////
/// Int32                                                                   ///
///////////////////////////////////////////////////////////////////////////////
rel Int32Rel(x: Int32)

@test
def testInt32Lit01(): #{ Int32Rel } =
    let x = Int32Rel(0i32).;
    solve x

@test
def testInt32Lit02(): #{ Int32Rel } =
    let x = Int32Rel(123i32).;
    solve x

@test
def testInt32Lit03(): #{ Int32Rel } =
    let x = Int32Rel(-123i32).;
    solve x


///////////////////////////////////////////////////////////////////////////////
/// Int64                                                                   ///
///////////////////////////////////////////////////////////////////////////////
rel Int64Rel(x: Int64)

@test
def testInt64Lit01(): #{ Int64Rel } =
    let x = Int64Rel(0i64).;
    solve x

@test
def testInt64Lit02(): #{ Int64Rel } =
    let x = Int64Rel(123i64).;
    solve x

@test
def testInt64Lit03(): #{ Int64Rel } =
    let x = Int64Rel(-123i64).;
    solve x


///////////////////////////////////////////////////////////////////////////////
/// BigInt                                                                  ///
///////////////////////////////////////////////////////////////////////////////
rel BigIntRel(x: BigInt)

@test
def testBigIntLit01(): #{ BigIntRel } =
    let x = BigIntRel(0ii).;
    solve x

@test
def testBigIntLit02(): #{ BigIntRel } =
    let x = BigIntRel(123ii).;
    solve x

@test
def testBigIntLit03(): #{ BigIntRel } =
    let x = BigIntRel(-123ii).;
    solve x


///////////////////////////////////////////////////////////////////////////////
/// Str                                                                     ///
///////////////////////////////////////////////////////////////////////////////
rel StrRel(x: Str)

@test
def testStrLit01(): #{ StrRel } =
    let x = StrRel("").;
    solve x

@test
def testStrLit02(): #{ StrRel } =
    let x = StrRel("hello").;
    solve x

@test
def testStrLit03(): #{ StrRel } =
    let x = StrRel("hello world!").;
    solve x


///////////////////////////////////////////////////////////////////////////////
/// Option                                                                  ///
///////////////////////////////////////////////////////////////////////////////
rel OptionRel(x: Option[Int])

@test
def testOptionLit01(): #{ OptionRel } =
    let x = OptionRel(None).;
    solve x

@test
def testOptionLit02(): #{ OptionRel } =
    let x = OptionRel(Some(123)).;
    solve x

@test
def testOptionLit03(): #{ OptionRel } =
    let x = OptionRel(None).;
    let y = OptionRel(Some(123)).;
    solve x <+> y

@test
def testOptionLit04(): #{ OptionRel } =
    let x = OptionRel(None).;
    let y = OptionRel(Some(123)).;
    let z = OptionRel(Some(456)).;
    solve x <+> y <+> z

///////////////////////////////////////////////////////////////////////////////
/// Result                                                                  ///
///////////////////////////////////////////////////////////////////////////////
rel ResultRel(x: Result[Int, Str])

@test
def testResultLit01(): #{ ResultRel } =
    let x = ResultRel(Ok(123)).;
    solve x

@test
def testResultLit02(): #{ ResultRel } =
    let x = ResultRel(Err("hello world!")).;
    solve x

@test
def testResultLit03(): #{ ResultRel } =
    let x = ResultRel(Ok(123)).;
    let y = ResultRel(Err("hello world!")).;
    solve x <+> y

@test
def testResultLit04(): #{ ResultRel } =
    let x = ResultRel(Ok(123)).;
    let y = ResultRel(Ok(456)).;
    let z = ResultRel(Err("hello world!")).;
    solve x <+> y <+> z

@test
def testResultLit05(): #{ ResultRel } =
    let x = ResultRel(Ok(123)).;
    let y = ResultRel(Err("hello world!")).;
    let z = ResultRel(Err("Goodbye world!")).;
    solve x <+> y <+> z

@test
def testResultLit06(): #{ ResultRel } =
    let x = ResultRel(Ok(123)).;
    let y = ResultRel(Ok(456)).;
    let z = ResultRel(Err("hello world!")).;
    let w = ResultRel(Err("Goodbye world!")).;
    solve x <+> y <+> z <+> w

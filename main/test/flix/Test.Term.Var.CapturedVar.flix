namespace Test/Term/Lit/Var/CapturedVar {

    ///////////////////////////////////////////////////////////////////////////////
    /// Unit                                                                    ///
    ///////////////////////////////////////////////////////////////////////////////
    @test
    def testUnitCapturedVar01(): Bool =
        let c = ();
        let x = R(c).;
        let y = R(c) :- R(c).;
        let r = solve x <+> y;
        r |= R(c).

    ///////////////////////////////////////////////////////////////////////////////
    /// Bool                                                                    ///
    ///////////////////////////////////////////////////////////////////////////////
    @test
    def testBoolCapturedVar01(): Bool =
        let c = true;
        let x = R(c).;
        let y = R(c) :- R(c).;
        let r = solve x <+> y;
        r |= R(c).

    @test
    def testBoolCapturedVar02(): Bool =
        let c = false;
        let x = R(c).;
        let y = R(c) :- R(c).;
        let r = solve x <+> y;
        r |= R(c).

    ///////////////////////////////////////////////////////////////////////////////
    /// Char                                                                    ///
    ///////////////////////////////////////////////////////////////////////////////
    @test
    def testCharCapturedVar01(): Bool =
        let c = ' ';
        let x = R(c).;
        let y = R(c) :- R(c).;
        let r = solve x <+> y;
        r |= R(c).

    @test
    def testCharCapturedVar02(): Bool =
        let c = 'a';
        let x = R(c).;
        let y = R(c) :- R(c).;
        let r = solve x <+> y;
        r |= R(c).

    @test
    def testCharCapturedVar03(): Bool =
        let c = 'x';
        let x = R(c).;
        let y = R(c) :- R(c).;
        let r = solve x <+> y;
        r |= R(c).

    ///////////////////////////////////////////////////////////////////////////////
    /// Float32                                                                 ///
    ///////////////////////////////////////////////////////////////////////////////
    @test
    def testFloat32CapturedVar01(): Bool =
        let c = 0.0f32;
        let x = R(c).;
        let y = R(c) :- R(c).;
        let r = solve x <+> y;
        r |= R(c).

    @test
    def testFloat32CapturedVar02(): Bool =
        let c = 123.456f32;
        let x = R(c).;
        let y = R(c) :- R(c).;
        let r = solve x <+> y;
        r |= R(c).

    @test
    def testFloat32CapturedVar03(): Bool =
        let c = -123.456f32;
        let x = R(c).;
        let y = R(c) :- R(c).;
        let r = solve x <+> y;
        r |= R(c).

    ///////////////////////////////////////////////////////////////////////////////
    /// Float64                                                                 ///
    ///////////////////////////////////////////////////////////////////////////////
    @test
    def testFloat64CapturedVar01(): Bool =
        let c = 0.0f64;
        let x = R(c).;
        let y = R(c) :- R(c).;
        let r = solve x <+> y;
        r |= R(c).

    @test
    def testFloat64CapturedVar02(): Bool =
        let c = 123.456f64;
        let x = R(c).;
        let y = R(c) :- R(c).;
        let r = solve x <+> y;
        r |= R(c).

    @test
    def testFloat64CapturedVar03(): Bool =
        let c = -123.456f64;
        let x = R(c).;
        let y = R(c) :- R(c).;
        let r = solve x <+> y;
        r |= R(c).

    ///////////////////////////////////////////////////////////////////////////////
    /// Int8                                                                    ///
    ///////////////////////////////////////////////////////////////////////////////
    @test
    def testInt8CapturedVar01(): Bool =
        let c = 0i8;
        let x = R(c).;
        let y = R(c) :- R(c).;
        let r = solve x <+> y;
        r |= R(c).

    @test
    def testInt8CapturedVar02(): Bool =
        let c = 123i8;
        let x = R(c).;
        let y = R(c) :- R(c).;
        let r = solve x <+> y;
        r |= R(c).

    @test
    def testInt8CapturedVar03(): Bool =
        let c = -123i8;
        let x = R(c).;
        let y = R(c) :- R(c).;
        let r = solve x <+> y;
        r |= R(c).

    ///////////////////////////////////////////////////////////////////////////////
    /// Int16                                                                   ///
    ///////////////////////////////////////////////////////////////////////////////
    @test
    def testInt16CapturedVar01(): Bool =
        let c = 0i16;
        let x = R(c).;
        let y = R(c) :- R(c).;
        let r = solve x <+> y;
        r |= R(c).

    @test
    def testInt16CapturedVar02(): Bool =
        let c = 123i16;
        let x = R(c).;
        let y = R(c) :- R(c).;
        let r = solve x <+> y;
        r |= R(c).

    @test
    def testInt16CapturedVar03(): Bool =
        let c = -123i16;
        let x = R(c).;
        let y = R(c) :- R(c).;
        let r = solve x <+> y;
        r |= R(c).

    ///////////////////////////////////////////////////////////////////////////////
    /// Int32                                                                   ///
    ///////////////////////////////////////////////////////////////////////////////
    @test
    def testInt32CapturedVar01(): Bool =
        let c = 0i32;
        let x = R(c).;
        let y = R(c) :- R(c).;
        let r = solve x <+> y;
        r |= R(c).

    @test
    def testInt32CapturedVar02(): Bool =
        let c = 123i32;
        let x = R(c).;
        let y = R(c) :- R(c).;
        let r = solve x <+> y;
        r |= R(c).

    @test
    def testInt32CapturedVar03(): Bool =
        let c = -123i32;
        let x = R(c).;
        let y = R(c) :- R(c).;
        let r = solve x <+> y;
        r |= R(c).

    ///////////////////////////////////////////////////////////////////////////////
    /// Int64                                                                   ///
    ///////////////////////////////////////////////////////////////////////////////
    @test
    def testInt64CapturedVar01(): Bool =
        let c = 0i64;
        let x = R(c).;
        let y = R(c) :- R(c).;
        let r = solve x <+> y;
        r |= R(c).

    @test
    def testInt64CapturedVar02(): Bool =
        let c = 123i64;
        let x = R(c).;
        let y = R(c) :- R(c).;
        let r = solve x <+> y;
        r |= R(c).

    @test
    def testInt64CapturedVar03(): Bool =
        let c = -123i64;
        let x = R(c).;
        let y = R(c) :- R(c).;
        let r = solve x <+> y;
        r |= R(c).

    ///////////////////////////////////////////////////////////////////////////////
    /// BigInt                                                                  ///
    ///////////////////////////////////////////////////////////////////////////////
    @test
    def testBigIntCapturedVar01(): Bool =
        let c = 0ii;
        let x = R(c).;
        let y = R(c) :- R(c).;
        let r = solve x <+> y;
        r |= R(c).

    @test
    def testBigIntCapturedVar02(): Bool =
        let c = 123ii;
        let x = R(c).;
        let y = R(c) :- R(c).;
        let r = solve x <+> y;
        r |= R(c).

    @test
    def testBigIntCapturedVar03(): Bool =
        let c = -123ii;
        let x = R(c).;
        let y = R(c) :- R(c).;
        let r = solve x <+> y;
        r |= R(c).

    ///////////////////////////////////////////////////////////////////////////////
    /// Str                                                                     ///
    ///////////////////////////////////////////////////////////////////////////////
    @test
    def testStrCapturedVar01(): Bool =
        let c = "";
        let x = R(c).;
        let y = R(c) :- R(c).;
        let r = solve x <+> y;
        r |= R(c).

    @test
    def testStrCapturedVar02(): Bool =
        let c = "hello";
        let x = R(c).;
        let y = R(c) :- R(c).;
        let r = solve x <+> y;
        r |= R(c).

    @test
    def testStrCapturedVar03(): Bool =
        let c = "hello world!";
        let x = R(c).;
        let y = R(c) :- R(c).;
        let r = solve x <+> y;
        r |= R(c).

    ///////////////////////////////////////////////////////////////////////////////
    /// Option                                                                  ///
    ///////////////////////////////////////////////////////////////////////////////
    @test
    def testOptionCapturedVar01(): Bool =
        let c: Option[Int] = None;
        let x = R(c).;
        let y = R(c) :- R(c).;
        let r = solve x <+> y;
        r |= R(c).

    @test
    def testOptionCapturedVar02(): Bool =
        let c = Some(123);
        let x = R(c).;
        let y = R(c) :- R(c).;
        let r = solve x <+> y;
        r |= R(c).

    ///////////////////////////////////////////////////////////////////////////////
    /// Result                                                                  ///
    ///////////////////////////////////////////////////////////////////////////////
    @test
    def testResultCapturedVar01(): Bool =
        let c: Result[Int, String] = Ok(123);
        let x = R(c).;
        let y = R(c) :- R(c).;
        let r = solve x <+> y;
        r |= R(c).

    @test
    def testResultCapturedVar02(): Bool =
        let c: Result[Int, String] = Err("goodbye world!");
        let x = R(c).;
        let y = R(c) :- R(c).;
        let r = solve x <+> y;
        r |= R(c).

    ///////////////////////////////////////////////////////////////////////////////
    /// Tuple                                                                   ///
    ///////////////////////////////////////////////////////////////////////////////
    @test
    def testTupleCapturedVar01(): Bool =
        let c = (true, 'a', 42);
        let x = R(c).;
        let y = R(c) :- R(c).;
        let r = solve x <+> y;
        r |= R(c).

    @test
    def testTupleCapturedVar02(): Bool =
        let c = (false, 'x', -42);
        let x = R(c).;
        let y = R(c) :- R(c).;
        let r = solve x <+> y;
        r |= R(c).

}

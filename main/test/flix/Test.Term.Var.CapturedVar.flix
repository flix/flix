mod Test.Term.Lit.Var.CapturedVar {

    ///////////////////////////////////////////////////////////////////////////////
    /// Unit                                                                    ///
    ///////////////////////////////////////////////////////////////////////////////
    @test
    def testUnitCapturedVar01(): Bool =
        let c = ();
        let x = #{ R(c). };
        let y = #{ R(c) :- R(c). };
        let r = query x, y select 42 from R(c);
        not Vector.isEmpty(r)

    ///////////////////////////////////////////////////////////////////////////////
    /// Bool                                                                    ///
    ///////////////////////////////////////////////////////////////////////////////
    @test
    def testBoolCapturedVar01(): Bool =
        let c = true;
        let x = #{ R(c). };
        let y = #{ R(c) :- R(c). };
        let r = query x, y select 42 from R(c);
        not Vector.isEmpty(r)

    @test
    def testBoolCapturedVar02(): Bool =
        let c = false;
        let x = #{ R(c). };
        let y = #{ R(c) :- R(c). };
        let r = query x, y select 42 from R(c);
        not Vector.isEmpty(r)

    ///////////////////////////////////////////////////////////////////////////////
    /// Char                                                                    ///
    ///////////////////////////////////////////////////////////////////////////////
    @test
    def testCharCapturedVar01(): Bool =
        let c = ' ';
        let x = #{ R(c). };
        let y = #{ R(c) :- R(c). };
        let r = query x, y select 42 from R(c);
        not Vector.isEmpty(r)

    @test
    def testCharCapturedVar02(): Bool =
        let c = 'a';
        let x = #{ R(c). };
        let y = #{ R(c) :- R(c). };
            let r = query x, y select 42 from R(c);
        not Vector.isEmpty(r)

    @test
    def testCharCapturedVar03(): Bool =
        let c = 'x';
        let x = #{ R(c). };
        let y = #{ R(c) :- R(c). };
        let r = query x, y select 42 from R(c);
        not Vector.isEmpty(r)

    ///////////////////////////////////////////////////////////////////////////////
    /// Float32                                                                 ///
    ///////////////////////////////////////////////////////////////////////////////
    @test
    def testFloat32CapturedVar01(): Bool =
        let c = 0.0f32;
        let x = #{ R(c). };
        let y = #{ R(c) :- R(c). };
        let r = query x, y select 42 from R(c);
        not Vector.isEmpty(r)

    @test
    def testFloat32CapturedVar02(): Bool =
        let c = 123.456f32;
        let x = #{ R(c). };
        let y = #{ R(c) :- R(c). };
        let r = query x, y select 42 from R(c);
        not Vector.isEmpty(r)

    @test
    def testFloat32CapturedVar03(): Bool =
        let c = -123.456f32;
        let x = #{ R(c). };
        let y = #{ R(c) :- R(c). };
        let r = query x, y select 42 from R(c);
        not Vector.isEmpty(r)

    ///////////////////////////////////////////////////////////////////////////////
    /// Float64                                                                 ///
    ///////////////////////////////////////////////////////////////////////////////
    @test
    def testFloat64CapturedVar01(): Bool =
        let c = 0.0f64;
        let x = #{ R(c). };
        let y = #{ R(c) :- R(c). };
        let r = query x, y select 42 from R(c);
        not Vector.isEmpty(r)

    @test
    def testFloat64CapturedVar02(): Bool =
        let c = 123.456f64;
        let x = #{ R(c). };
        let y = #{ R(c) :- R(c). };
        let r = query x, y select 42 from R(c);
        not Vector.isEmpty(r)

    @test
    def testFloat64CapturedVar03(): Bool =
        let c = -123.456f64;
        let x = #{ R(c). };
        let y = #{ R(c) :- R(c). };
        let r = query x, y select 42 from R(c);
        not Vector.isEmpty(r)

    ///////////////////////////////////////////////////////////////////////////////
    /// Int8                                                                    ///
    ///////////////////////////////////////////////////////////////////////////////
    @test
    def testInt8CapturedVar01(): Bool =
        let c = 0i8;
        let x = #{ R(c). };
        let y = #{ R(c) :- R(c). };
        let r = query x, y select 42 from R(c);
        not Vector.isEmpty(r)

    @test
    def testInt8CapturedVar02(): Bool =
        let c = 123i8;
        let x = #{ R(c). };
        let y = #{ R(c) :- R(c). };
        let r = query x, y select 42 from R(c);
        not Vector.isEmpty(r)

    @test
    def testInt8CapturedVar03(): Bool =
        let c = -123i8;
        let x = #{ R(c). };
        let y = #{ R(c) :- R(c). };
        let r = query x, y select 42 from R(c);
        not Vector.isEmpty(r)

    ///////////////////////////////////////////////////////////////////////////////
    /// Int16                                                                   ///
    ///////////////////////////////////////////////////////////////////////////////
    @test
    def testInt16CapturedVar01(): Bool =
        let c = 0i16;
        let x = #{ R(c). };
        let y = #{ R(c) :- R(c). };
        let r = query x, y select 42 from R(c);
        not Vector.isEmpty(r)

    @test
    def testInt16CapturedVar02(): Bool =
        let c = 123i16;
        let x = #{ R(c). };
        let y = #{ R(c) :- R(c). };
        let r = query x, y select 42 from R(c);
        not Vector.isEmpty(r)

    @test
    def testInt16CapturedVar03(): Bool =
        let c = -123i16;
        let x = #{ R(c). };
        let y = #{ R(c) :- R(c). };
        let r = query x, y select 42 from R(c);
        not Vector.isEmpty(r)

    ///////////////////////////////////////////////////////////////////////////////
    /// Int32                                                                   ///
    ///////////////////////////////////////////////////////////////////////////////
    @test
    def testInt32CapturedVar01(): Bool =
        let c = 0i32;
        let x = #{ R(c). };
        let y = #{ R(c) :- R(c). };
        let r = query x, y select 42 from R(c);
        not Vector.isEmpty(r)

    @test
    def testInt32CapturedVar02(): Bool =
        let c = 123i32;
        let x = #{ R(c). };
        let y = #{ R(c) :- R(c). };
        let r = query x, y select 42 from R(c);
        not Vector.isEmpty(r)

    @test
    def testInt32CapturedVar03(): Bool =
        let c = -123i32;
        let x = #{ R(c). };
        let y = #{ R(c) :- R(c). };
        let r = query x, y select 42 from R(c);
        not Vector.isEmpty(r)

    ///////////////////////////////////////////////////////////////////////////////
    /// Int64                                                                   ///
    ///////////////////////////////////////////////////////////////////////////////
    @test
    def testInt64CapturedVar01(): Bool =
        let c = 0i64;
        let x = #{ R(c). };
        let y = #{ R(c) :- R(c). };
        let r = query x, y select 42 from R(c);
        not Vector.isEmpty(r)

    @test
    def testInt64CapturedVar02(): Bool =
        let c = 123i64;
        let x = #{ R(c). };
        let y = #{ R(c) :- R(c). };
        let r = query x, y select 42 from R(c);
        not Vector.isEmpty(r)

    @test
    def testInt64CapturedVar03(): Bool =
        let c = -123i64;
        let x = #{ R(c). };
        let y = #{ R(c) :- R(c). };
        let r = query x, y select 42 from R(c);
        not Vector.isEmpty(r)

    ///////////////////////////////////////////////////////////////////////////////
    /// BigInt                                                                  ///
    ///////////////////////////////////////////////////////////////////////////////
    @test
    def testBigIntCapturedVar01(): Bool =
        let c = 0ii;
        let x = #{ R(c). };
        let y = #{ R(c) :- R(c). };
        let r = query x, y select 42 from R(c);
        not Vector.isEmpty(r)

    @test
    def testBigIntCapturedVar02(): Bool =
        let c = 123ii;
        let x = #{ R(c). };
        let y = #{ R(c) :- R(c). };
        let r = query x, y select 42 from R(c);
        not Vector.isEmpty(r)

    @test
    def testBigIntCapturedVar03(): Bool =
        let c = -123ii;
        let x = #{ R(c). };
        let y = #{ R(c) :- R(c). };
        let r = query x, y select 42 from R(c);
        not Vector.isEmpty(r)

    ///////////////////////////////////////////////////////////////////////////////
    /// Str                                                                     ///
    ///////////////////////////////////////////////////////////////////////////////
    @test
    def testStrCapturedVar01(): Bool =
        let c = "";
        let x = #{ R(c). };
        let y = #{ R(c) :- R(c). };
        let r = query x, y select 42 from R(c);
        not Vector.isEmpty(r)

    @test
    def testStrCapturedVar02(): Bool =
        let c = "hello";
        let x = #{ R(c). };
        let y = #{ R(c) :- R(c). };
        let r = query x, y select 42 from R(c);
        not Vector.isEmpty(r)

    @test
    def testStrCapturedVar03(): Bool =
        let c = "hello world!";
        let x = #{ R(c). };
        let y = #{ R(c) :- R(c). };
        let r = query x, y select 42 from R(c);
        not Vector.isEmpty(r)

    ///////////////////////////////////////////////////////////////////////////////
    /// Option                                                                  ///
    ///////////////////////////////////////////////////////////////////////////////
    @test
    def testOptionCapturedVar01(): Bool =
        let c: Option[Int32] = None;
        let x = #{ R(c). };
        let y = #{ R(c) :- R(c). };
        let r = query x, y select 42 from R(c);
        not Vector.isEmpty(r)

    @test
    def testOptionCapturedVar02(): Bool =
        let c = Some(123);
        let x = #{ R(c). };
        let y = #{ R(c) :- R(c). };
        let r = query x, y select 42 from R(c);
        not Vector.isEmpty(r)

    ///////////////////////////////////////////////////////////////////////////////
    /// Result                                                                  ///
    ///////////////////////////////////////////////////////////////////////////////
    @test
    def testResultCapturedVar01(): Bool =
        let c: Result[String, Int32] = Ok(123);
        let x = #{ R(c). };
        let y = #{ R(c) :- R(c). };
        let r = query x, y select 42 from R(c);
        not Vector.isEmpty(r)

    @test
    def testResultCapturedVar02(): Bool =
        let c: Result[String, Int32] = Err("goodbye world!");
        let x = #{ R(c). };
        let y = #{ R(c) :- R(c). };
        let r = query x, y select 42 from R(c);
        not Vector.isEmpty(r)

    ///////////////////////////////////////////////////////////////////////////////
    /// Tuple                                                                   ///
    ///////////////////////////////////////////////////////////////////////////////
    @test
    def testTupleCapturedVar01(): Bool =
        let c = (true, 'a', 42);
        let x = #{ R(c). };
        let y = #{ R(c) :- R(c). };
        let r = query x, y select 42 from R(c);
        not Vector.isEmpty(r)

    @test
    def testTupleCapturedVar02(): Bool =
        let c = (false, 'x', -42);
        let x = #{ R(c). };
        let y = #{ R(c) :- R(c). };
        let r = query x, y select 42 from R(c);
        not Vector.isEmpty(r)

}

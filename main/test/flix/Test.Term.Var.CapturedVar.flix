mod Test.Term.Var.CapturedVar {

    use Assert.assertMemberOf

    ///////////////////////////////////////////////////////////////////////////////
    /// Unit                                                                    ///
    ///////////////////////////////////////////////////////////////////////////////
    @Test
    def testUnitCapturedVar01(): Unit \ Assert =
        let c = ();
        let x = #{ R(c). };
        let y = #{ R(c) :- R(c). };
        let r = query x, y select 42 from R(c);
        assertMemberOf(42, r)

    ///////////////////////////////////////////////////////////////////////////////
    /// Bool                                                                    ///
    ///////////////////////////////////////////////////////////////////////////////
    @Test
    def testBoolCapturedVar01(): Unit \ Assert =
        let c = true;
        let x = #{ R(c). };
        let y = #{ R(c) :- R(c). };
        let r = query x, y select 42 from R(c);
        assertMemberOf(42, r)

    @Test
    def testBoolCapturedVar02(): Unit \ Assert =
        let c = false;
        let x = #{ R(c). };
        let y = #{ R(c) :- R(c). };
        let r = query x, y select 42 from R(c);
        assertMemberOf(42, r)

    ///////////////////////////////////////////////////////////////////////////////
    /// Char                                                                    ///
    ///////////////////////////////////////////////////////////////////////////////
    @Test
    def testCharCapturedVar01(): Unit \ Assert =
        let c = ' ';
        let x = #{ R(c). };
        let y = #{ R(c) :- R(c). };
        let r = query x, y select 42 from R(c);
        assertMemberOf(42, r)

    @Test
    def testCharCapturedVar02(): Unit \ Assert =
        let c = 'a';
        let x = #{ R(c). };
        let y = #{ R(c) :- R(c). };
        let r = query x, y select 42 from R(c);
        assertMemberOf(42, r)

    @Test
    def testCharCapturedVar03(): Unit \ Assert =
        let c = 'x';
        let x = #{ R(c). };
        let y = #{ R(c) :- R(c). };
        let r = query x, y select 42 from R(c);
        assertMemberOf(42, r)

    ///////////////////////////////////////////////////////////////////////////////
    /// Float32                                                                 ///
    ///////////////////////////////////////////////////////////////////////////////
    @Test
    def testFloat32CapturedVar01(): Unit \ Assert =
        let c = 0.0f32;
        let x = #{ R(c). };
        let y = #{ R(c) :- R(c). };
        let r = query x, y select 42 from R(c);
        assertMemberOf(42, r)

    @Test
    def testFloat32CapturedVar02(): Unit \ Assert =
        let c = 123.456f32;
        let x = #{ R(c). };
        let y = #{ R(c) :- R(c). };
        let r = query x, y select 42 from R(c);
        assertMemberOf(42, r)

    @Test
    def testFloat32CapturedVar03(): Unit \ Assert =
        let c = -123.456f32;
        let x = #{ R(c). };
        let y = #{ R(c) :- R(c). };
        let r = query x, y select 42 from R(c);
        assertMemberOf(42, r)

    ///////////////////////////////////////////////////////////////////////////////
    /// Float64                                                                 ///
    ///////////////////////////////////////////////////////////////////////////////
    @Test
    def testFloat64CapturedVar01(): Unit \ Assert =
        let c = 0.0f64;
        let x = #{ R(c). };
        let y = #{ R(c) :- R(c). };
        let r = query x, y select 42 from R(c);
        assertMemberOf(42, r)

    @Test
    def testFloat64CapturedVar02(): Unit \ Assert =
        let c = 123.456f64;
        let x = #{ R(c). };
        let y = #{ R(c) :- R(c). };
        let r = query x, y select 42 from R(c);
        assertMemberOf(42, r)

    @Test
    def testFloat64CapturedVar03(): Unit \ Assert =
        let c = -123.456f64;
        let x = #{ R(c). };
        let y = #{ R(c) :- R(c). };
        let r = query x, y select 42 from R(c);
        assertMemberOf(42, r)

    ///////////////////////////////////////////////////////////////////////////////
    /// Int8                                                                    ///
    ///////////////////////////////////////////////////////////////////////////////
    @Test
    def testInt8CapturedVar01(): Unit \ Assert =
        let c = 0i8;
        let x = #{ R(c). };
        let y = #{ R(c) :- R(c). };
        let r = query x, y select 42 from R(c);
        assertMemberOf(42, r)

    @Test
    def testInt8CapturedVar02(): Unit \ Assert =
        let c = 123i8;
        let x = #{ R(c). };
        let y = #{ R(c) :- R(c). };
        let r = query x, y select 42 from R(c);
        assertMemberOf(42, r)

    @Test
    def testInt8CapturedVar03(): Unit \ Assert =
        let c = -123i8;
        let x = #{ R(c). };
        let y = #{ R(c) :- R(c). };
        let r = query x, y select 42 from R(c);
        assertMemberOf(42, r)

    ///////////////////////////////////////////////////////////////////////////////
    /// Int16                                                                   ///
    ///////////////////////////////////////////////////////////////////////////////
    @Test
    def testInt16CapturedVar01(): Unit \ Assert =
        let c = 0i16;
        let x = #{ R(c). };
        let y = #{ R(c) :- R(c). };
        let r = query x, y select 42 from R(c);
        assertMemberOf(42, r)

    @Test
    def testInt16CapturedVar02(): Unit \ Assert =
        let c = 123i16;
        let x = #{ R(c). };
        let y = #{ R(c) :- R(c). };
        let r = query x, y select 42 from R(c);
        assertMemberOf(42, r)

    @Test
    def testInt16CapturedVar03(): Unit \ Assert =
        let c = -123i16;
        let x = #{ R(c). };
        let y = #{ R(c) :- R(c). };
        let r = query x, y select 42 from R(c);
        assertMemberOf(42, r)

    ///////////////////////////////////////////////////////////////////////////////
    /// Int32                                                                   ///
    ///////////////////////////////////////////////////////////////////////////////
    @Test
    def testInt32CapturedVar01(): Unit \ Assert =
        let c = 0i32;
        let x = #{ R(c). };
        let y = #{ R(c) :- R(c). };
        let r = query x, y select 42 from R(c);
        assertMemberOf(42, r)

    @Test
    def testInt32CapturedVar02(): Unit \ Assert =
        let c = 123i32;
        let x = #{ R(c). };
        let y = #{ R(c) :- R(c). };
        let r = query x, y select 42 from R(c);
        assertMemberOf(42, r)

    @Test
    def testInt32CapturedVar03(): Unit \ Assert =
        let c = -123i32;
        let x = #{ R(c). };
        let y = #{ R(c) :- R(c). };
        let r = query x, y select 42 from R(c);
        assertMemberOf(42, r)

    ///////////////////////////////////////////////////////////////////////////////
    /// Int64                                                                   ///
    ///////////////////////////////////////////////////////////////////////////////
    @Test
    def testInt64CapturedVar01(): Unit \ Assert =
        let c = 0i64;
        let x = #{ R(c). };
        let y = #{ R(c) :- R(c). };
        let r = query x, y select 42 from R(c);
        assertMemberOf(42, r)

    @Test
    def testInt64CapturedVar02(): Unit \ Assert =
        let c = 123i64;
        let x = #{ R(c). };
        let y = #{ R(c) :- R(c). };
        let r = query x, y select 42 from R(c);
        assertMemberOf(42, r)

    @Test
    def testInt64CapturedVar03(): Unit \ Assert =
        let c = -123i64;
        let x = #{ R(c). };
        let y = #{ R(c) :- R(c). };
        let r = query x, y select 42 from R(c);
        assertMemberOf(42, r)

    ///////////////////////////////////////////////////////////////////////////////
    /// BigInt                                                                  ///
    ///////////////////////////////////////////////////////////////////////////////
    @Test
    def testBigIntCapturedVar01(): Unit \ Assert =
        let c = 0ii;
        let x = #{ R(c). };
        let y = #{ R(c) :- R(c). };
        let r = query x, y select 42 from R(c);
        assertMemberOf(42, r)

    @Test
    def testBigIntCapturedVar02(): Unit \ Assert =
        let c = 123ii;
        let x = #{ R(c). };
        let y = #{ R(c) :- R(c). };
        let r = query x, y select 42 from R(c);
        assertMemberOf(42, r)

    @Test
    def testBigIntCapturedVar03(): Unit \ Assert =
        let c = -123ii;
        let x = #{ R(c). };
        let y = #{ R(c) :- R(c). };
        let r = query x, y select 42 from R(c);
        assertMemberOf(42, r)

    ///////////////////////////////////////////////////////////////////////////////
    /// Str                                                                     ///
    ///////////////////////////////////////////////////////////////////////////////
    @Test
    def testStrCapturedVar01(): Unit \ Assert =
        let c = "";
        let x = #{ R(c). };
        let y = #{ R(c) :- R(c). };
        let r = query x, y select 42 from R(c);
        assertMemberOf(42, r)

    @Test
    def testStrCapturedVar02(): Unit \ Assert =
        let c = "hello";
        let x = #{ R(c). };
        let y = #{ R(c) :- R(c). };
        let r = query x, y select 42 from R(c);
        assertMemberOf(42, r)

    @Test
    def testStrCapturedVar03(): Unit \ Assert =
        let c = "hello world!";
        let x = #{ R(c). };
        let y = #{ R(c) :- R(c). };
        let r = query x, y select 42 from R(c);
        assertMemberOf(42, r)

    ///////////////////////////////////////////////////////////////////////////////
    /// Option                                                                  ///
    ///////////////////////////////////////////////////////////////////////////////
    @Test
    def testOptionCapturedVar01(): Unit \ Assert =
        let c: Option[Int32] = None;
        let x = #{ R(c). };
        let y = #{ R(c) :- R(c). };
        let r = query x, y select 42 from R(c);
        assertMemberOf(42, r)

    @Test
    def testOptionCapturedVar02(): Unit \ Assert =
        let c = Some(123);
        let x = #{ R(c). };
        let y = #{ R(c) :- R(c). };
        let r = query x, y select 42 from R(c);
        assertMemberOf(42, r)

    ///////////////////////////////////////////////////////////////////////////////
    /// Result                                                                  ///
    ///////////////////////////////////////////////////////////////////////////////
    @Test
    def testResultCapturedVar01(): Unit \ Assert =
        let c: Result[String, Int32] = Ok(123);
        let x = #{ R(c). };
        let y = #{ R(c) :- R(c). };
        let r = query x, y select 42 from R(c);
        assertMemberOf(42, r)

    @Test
    def testResultCapturedVar02(): Unit \ Assert =
        let c: Result[String, Int32] = Err("goodbye world!");
        let x = #{ R(c). };
        let y = #{ R(c) :- R(c). };
        let r = query x, y select 42 from R(c);
        assertMemberOf(42, r)

    ///////////////////////////////////////////////////////////////////////////////
    /// Tuple                                                                   ///
    ///////////////////////////////////////////////////////////////////////////////
    @Test
    def testTupleCapturedVar01(): Unit \ Assert =
        let c = (true, 'a', 42);
        let x = #{ R(c). };
        let y = #{ R(c) :- R(c). };
        let r = query x, y select 42 from R(c);
        assertMemberOf(42, r)

    @Test
    def testTupleCapturedVar02(): Unit \ Assert =
        let c = (false, 'x', -42);
        let x = #{ R(c). };
        let y = #{ R(c) :- R(c). };
        let r = query x, y select 42 from R(c);
        assertMemberOf(42, r)

}

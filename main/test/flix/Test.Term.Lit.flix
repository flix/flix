namespace Test/Term/Lit {

    ///////////////////////////////////////////////////////////////////////////////
    /// General                                                                 ///
    ///////////////////////////////////////////////////////////////////////////////
    @test
    def testLit01(): #{ A(Int, Int, String) } =
        let x = A(1, 2, "hello").;
        let y = A(3, 4, "world!").;
        solve x <+> y

    @test
    def testLit02(): #{ A(Int, Int, String) } =
        let x = A(1, 1, "hello").;
        let y = A(1, 1, "world!").;
        solve x <+> y

    @test
    def testLit03(): #{ A(Int, Int, String) } =
        let x = A(1, 1, "hello world").;
        let y = A(1, 1, v) :- A(1, 1, v).;
        solve x <+> y

    @test
    def testLit04(): #{ A(Int, Int, String) } =
        let x = A(1, 1, "abc").;
        let y = A(w, w, "abc") :- A(w, w, "abc").;
        solve x <+> y

    ///////////////////////////////////////////////////////////////////////////////
    /// Unit                                                                    ///
    ///////////////////////////////////////////////////////////////////////////////
    @test
    def testUnitLit01(): #{ R(Unit) } =
        let x = R(()).;
        solve x

    ///////////////////////////////////////////////////////////////////////////////
    /// Bool                                                                    ///
    ///////////////////////////////////////////////////////////////////////////////
    @test
    def testBoolLit01(): #{ R(Bool) } =
        let x = R(true).;
        solve x

    @test
    def testBoolLit02(): #{ R(Bool) } =
        let x = R(false).;
        solve x

    ///////////////////////////////////////////////////////////////////////////////
    /// Char                                                                    ///
    ///////////////////////////////////////////////////////////////////////////////
    @test
    def testCharLit01(): #{ R(Char) } =
        let x = R(' ').;
        solve x

    @test
    def testCharLit02(): #{ R(Char) } =
        let x = R('a').;
        solve x

    @test
    def testCharLit03(): #{ R(Char) } =
        let x = R('x').;
        solve x

    ///////////////////////////////////////////////////////////////////////////////
    /// Float32                                                                 ///
    ///////////////////////////////////////////////////////////////////////////////
    @test
    def testFloat32Lit01(): #{ R(Float32) } =
        let x = R(0.0f32).;
        solve x

    @test
    def testFloat32Lit02(): #{ R(Float32) } =
        let x = R(-123.456f32).;
        solve x

    @test
    def testFloat32Lit03(): #{ R(Float32) } =
        let x = R(123.567f32).;
        solve x

    ///////////////////////////////////////////////////////////////////////////////
    /// Float64                                                                 ///
    ///////////////////////////////////////////////////////////////////////////////
    @test
    def testFloat64Lit01(): #{ R(Float64) } =
        let x = R(0.0f64).;
        solve x

    @test
    def testFloat64Lit02(): #{ R(Float64)  } =
        let x = R(-123.456f64).;
        solve x

    @test
    def testFloat64Lit03(): #{ R(Float64)  } =
        let x = R(123.567f64).;
        solve x

    ///////////////////////////////////////////////////////////////////////////////
    /// Int8                                                                    ///
    ///////////////////////////////////////////////////////////////////////////////
    rel Int8Rel(x: Int8)

    @test
    def testInt8Lit01(): #{ R(Int8) } =
        let x = R(0i8).;
        solve x

    @test
    def testInt8Lit02(): #{ R(Int8) } =
        let x = R(123i8).;
        solve x

    @test
    def testInt8Lit03(): #{ R(Int8) } =
        let x = R(-123i8).;
        solve x

    ///////////////////////////////////////////////////////////////////////////////
    /// Int16                                                                   ///
    ///////////////////////////////////////////////////////////////////////////////
    @test
    def testInt16Lit01(): #{ R(Int16) } =
        let x = R(0i16).;
        solve x

    @test
    def testInt16Lit02(): #{ R(Int16) } =
        let x = R(123i16).;
        solve x

    @test
    def testInt16Lit03(): #{ R(Int16) } =
        let x = R(-123i16).;
        solve x


    ///////////////////////////////////////////////////////////////////////////////
    /// Int32                                                                   ///
    ///////////////////////////////////////////////////////////////////////////////
    @test
    def testInt32Lit01(): #{ R(Int32) } =
        let x = R(0i32).;
        solve x

    @test
    def testInt32Lit02(): #{ R(Int32) } =
        let x = R(123i32).;
        solve x

    @test
    def testInt32Lit03(): #{ R(Int32) } =
        let x = R(-123i32).;
        solve x

    ///////////////////////////////////////////////////////////////////////////////
    /// Int64                                                                   ///
    ///////////////////////////////////////////////////////////////////////////////
    @test
    def testInt64Lit01(): #{ R(Int64) } =
        let x = R(0i64).;
        solve x

    @test
    def testInt64Lit02(): #{ R(Int64) } =
        let x = R(123i64).;
        solve x

    @test
    def testInt64Lit03(): #{ R(Int64) } =
        let x = R(-123i64).;
        solve x

    ///////////////////////////////////////////////////////////////////////////////
    /// BigInt                                                                  ///
    ///////////////////////////////////////////////////////////////////////////////
    @test
    def testBigIntLit01(): #{ R(BigInt) } =
        let x = R(0ii).;
        solve x

    @test
    def testBigIntLit02(): #{ R(BigInt) } =
        let x = R(123ii).;
        solve x

    @test
    def testBigIntLit03(): #{ R(BigInt) } =
        let x = R(-123ii).;
        solve x

    ///////////////////////////////////////////////////////////////////////////////
    /// String                                                                  ///
    ///////////////////////////////////////////////////////////////////////////////
    @test
    def testStringLit01(): #{ R(String) } =
        let x = R("").;
        solve x

    @test
    def testStringLit02(): #{ R(String) } =
        let x = R("hello").;
        solve x

    @test
    def testStringLit03(): #{ R(String) } =
        let x = R("hello world!").;
        solve x

}

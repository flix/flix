mod Test.Term.Lit {

    ///////////////////////////////////////////////////////////////////////////////
    /// Unit                                                                    ///
    ///////////////////////////////////////////////////////////////////////////////
    @test
    def testUnitLit01(): #{ R(Unit) } =
        let f = #A(()).;
        let p = #R(x) :- A(x).;
        solve f, p project R

    ///////////////////////////////////////////////////////////////////////////////
    /// Bool                                                                    ///
    ///////////////////////////////////////////////////////////////////////////////
    @test
    def testBoolLit01(): #{ R(Bool) } =
        let f = #A(false).;
        let p = #R(x) :- A(x).;
        solve f, p project R

    @test
    def testBoolLit02(): #{ R(Bool) } =
        let f = #A(true).;
        let p = #R(x) :- A(x).;
        solve f, p project R

    ///////////////////////////////////////////////////////////////////////////////
    /// Char                                                                    ///
    ///////////////////////////////////////////////////////////////////////////////
    @test
    def testCharLit01(): #{ R(Char) } =
        let f = #A(' ').;
        let p = #R(x) :- A(x).;
        solve f, p project R

    @test
    def testCharLit02(): #{ R(Char) } =
        let f = #A('a').;
        let p = #R(x) :- A(x).;
        solve f, p project R

    @test
    def testCharLit03(): #{ R(Char) } =
        let f = #A('x').;
        let p = #R(x) :- A(x).;
        solve f, p project R

    ///////////////////////////////////////////////////////////////////////////////
    /// Int8                                                                    ///
    ///////////////////////////////////////////////////////////////////////////////
    @test
    def testInt8Lit01(): #{ R(Int8) } =
        let x = #R(0i8).;
        solve x

    @test
    def testInt8Lit02(): #{ R(Int8) } =
        let x = #R(123i8).;
        solve x

    @test
    def testInt8Lit03(): #{ R(Int8) } =
        let x = #R(-123i8).;
        solve x

    ///////////////////////////////////////////////////////////////////////////////
    /// Int16                                                                   ///
    ///////////////////////////////////////////////////////////////////////////////
    @test
    def testInt16Lit01(): #{ R(Int16) } =
        let x = #R(0i16).;
        solve x

    @test
    def testInt16Lit02(): #{ R(Int16) } =
        let x = #R(123i16).;
        solve x

    @test
    def testInt16Lit03(): #{ R(Int16) } =
        let x = #R(-123i16).;
        solve x


    ///////////////////////////////////////////////////////////////////////////////
    /// Int32                                                                   ///
    ///////////////////////////////////////////////////////////////////////////////
    @test
    def testInt32Lit01(): #{ R(Int32) } =
        let x = #R(0i32).;
        solve x

    @test
    def testInt32Lit02(): #{ R(Int32) } =
        let x = #R(123i32).;
        solve x

    @test
    def testInt32Lit03(): #{ R(Int32) } =
        let x = #R(-123i32).;
        solve x

    ///////////////////////////////////////////////////////////////////////////////
    /// Int64                                                                   ///
    ///////////////////////////////////////////////////////////////////////////////
    @test
    def testInt64Lit01(): #{ R(Int64) } =
        let x = #R(0i64).;
        solve x

    @test
    def testInt64Lit02(): #{ R(Int64) } =
        let x = #R(123i64).;
        solve x

    @test
    def testInt64Lit03(): #{ R(Int64) } =
        let x = #R(-123i64).;
        solve x

    ///////////////////////////////////////////////////////////////////////////////
    /// BigInt                                                                  ///
    ///////////////////////////////////////////////////////////////////////////////
    @test
    def testBigIntLit01(): #{ R(BigInt) } =
        let x = #R(0ii).;
        solve x

    @test
    def testBigIntLit02(): #{ R(BigInt) } =
        let x = #R(123ii).;
        solve x

    @test
    def testBigIntLit03(): #{ R(BigInt) } =
        let x = #R(-123ii).;
        solve x

    ///////////////////////////////////////////////////////////////////////////////
    /// String                                                                  ///
    ///////////////////////////////////////////////////////////////////////////////
    @test
    def testStringLit01(): #{ R(String) } =
        let x = #R("").;
        solve x

    @test
    def testStringLit02(): #{ R(String) } =
        let x = #R("hello").;
        solve x

    @test
    def testStringLit03(): #{ R(String) } =
        let x = #R("hello world!").;
        solve x

}

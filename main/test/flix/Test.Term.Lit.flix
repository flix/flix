mod Test.Term.Lit {

    ///////////////////////////////////////////////////////////////////////////////
    /// Unit                                                                    ///
    ///////////////////////////////////////////////////////////////////////////////
    @Test
    def testUnitLit01(): #{ R(Unit) } =
        let f = #{ A(()). };
        let p = #{ R(x) :- A(x). };
        solve f, p project R

    ///////////////////////////////////////////////////////////////////////////////
    /// Bool                                                                    ///
    ///////////////////////////////////////////////////////////////////////////////
    @Test
    def testBoolLit01(): #{ R(Bool) } =
        let f = #{ A(false). };
        let p = #{ R(x) :- A(x). };
        solve f, p project R

    @Test
    def testBoolLit02(): #{ R(Bool) } =
        let f = #{ A(true). };
        let p = #{ R(x) :- A(x). };
        solve f, p project R

    ///////////////////////////////////////////////////////////////////////////////
    /// Char                                                                    ///
    ///////////////////////////////////////////////////////////////////////////////
    @Test
    def testCharLit01(): #{ R(Char) } =
        let f = #{ A(' '). };
        let p = #{ R(x) :- A(x). };
        solve f, p project R

    @Test
    def testCharLit02(): #{ R(Char) } =
        let f = #{ A('a'). };
        let p = #{ R(x) :- A(x). };
        solve f, p project R

    @Test
    def testCharLit03(): #{ R(Char) } =
        let f = #{ A('x'). };
        let p = #{ R(x) :- A(x). };
        solve f, p project R

    ///////////////////////////////////////////////////////////////////////////////
    /// Int8                                                                    ///
    ///////////////////////////////////////////////////////////////////////////////
    @Test
    def testInt8Lit01(): #{ R(Int8) } =
        let x = #{ R(0i8). };
        solve x

    @Test
    def testInt8Lit02(): #{ R(Int8) } =
        let x = #{ R(123i8). };
        solve x

    @Test
    def testInt8Lit03(): #{ R(Int8) } =
        let x = #{ R(-123i8). };
        solve x

    ///////////////////////////////////////////////////////////////////////////////
    /// Int16                                                                   ///
    ///////////////////////////////////////////////////////////////////////////////
    @Test
    def testInt16Lit01(): #{ R(Int16) } =
        let x = #{ R(0i16). };
        solve x

    @Test
    def testInt16Lit02(): #{ R(Int16) } =
        let x = #{ R(123i16). };
        solve x

    @Test
    def testInt16Lit03(): #{ R(Int16) } =
        let x = #{ R(-123i16). };
        solve x


    ///////////////////////////////////////////////////////////////////////////////
    /// Int32                                                                   ///
    ///////////////////////////////////////////////////////////////////////////////
    @Test
    def testInt32Lit01(): #{ R(Int32) } =
        let x = #{ R(0i32). };
        solve x

    @Test
    def testInt32Lit02(): #{ R(Int32) } =
        let x = #{ R(123i32). };
        solve x

    @Test
    def testInt32Lit03(): #{ R(Int32) } =
        let x = #{ R(-123i32). };
        solve x

    ///////////////////////////////////////////////////////////////////////////////
    /// Int64                                                                   ///
    ///////////////////////////////////////////////////////////////////////////////
    @Test
    def testInt64Lit01(): #{ R(Int64) } =
        let x = #{ R(0i64). };
        solve x

    @Test
    def testInt64Lit02(): #{ R(Int64) } =
        let x = #{ R(123i64). };
        solve x

    @Test
    def testInt64Lit03(): #{ R(Int64) } =
        let x = #{ R(-123i64). };
        solve x

    ///////////////////////////////////////////////////////////////////////////////
    /// BigInt                                                                  ///
    ///////////////////////////////////////////////////////////////////////////////
    @Test
    def testBigIntLit01(): #{ R(BigInt) } =
        let x = #{ R(0ii). };
        solve x

    @Test
    def testBigIntLit02(): #{ R(BigInt) } =
        let x = #{ R(123ii). };
        solve x

    @Test
    def testBigIntLit03(): #{ R(BigInt) } =
        let x = #{ R(-123ii). };
        solve x

    ///////////////////////////////////////////////////////////////////////////////
    /// String                                                                  ///
    ///////////////////////////////////////////////////////////////////////////////
    @Test
    def testStringLit01(): #{ R(String) } =
        let x = #{ R(""). };
        solve x

    @Test
    def testStringLit02(): #{ R(String) } =
        let x = #{ R("hello"). };
        solve x

    @Test
    def testStringLit03(): #{ R(String) } =
        let x = #{ R("hello world!"). };
        solve x

}

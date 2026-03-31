mod Test.Term.Lit {

    use Assert.{assertEq, assertMemberOf}

    ///////////////////////////////////////////////////////////////////////////////
    /// Unit                                                                    ///
    ///////////////////////////////////////////////////////////////////////////////

    @Test
    def testUnitLit01(): Unit \ Assert =
        let pp = #{
            A(()).
        };
        assertEq(expected = Vector#{42}, query pp select 42 from A(()))

    @Test
    def testUnitLit02(): Unit \ Assert =
        let pp = #{ };
        assertEq(expected = Vector#{}, query pp select 42 from A(()))

    @Test
    def testUnitLit03(): Unit \ Assert =
        let pp = #{
            A().
        };
        assertEq(expected = Vector#{42}, query pp select 42 from A())

    @Test
    def testUnitLit04(): Unit \ Assert =
        let pp = #{ };
        assertEq(expected = Vector#{}, query pp select 42 from A())

    @Test
    def testUnitLit05(): Unit \ Assert =
        let pp = #{
            A().
        };
        assertEq(expected = Vector#{42}, query pp select 42 from A(()))

    @Test
    def testUnitLit06(): Unit \ Assert =
        let pp = #{
            A(()).
        };
        assertEq(expected = Vector#{42}, query pp select 42 from A())

    @Test
    def testUnitLit07(): Unit \ Assert =
        let pp = #{
            A().
            B(42) :- A(()).
        };
        assertEq(expected = Vector#{42}, query pp select x from B(x))

    ///////////////////////////////////////////////////////////////////////////////
    /// Bool                                                                    ///
    ///////////////////////////////////////////////////////////////////////////////
    @CompileTest
    def testBoolLit01(): #{ R(Bool) } =
        let f = #{ A(false). };
        let p = #{ R(x) :- A(x). };
        solve f, p project R

    @CompileTest
    def testBoolLit02(): #{ R(Bool) } =
        let f = #{ A(true). };
        let p = #{ R(x) :- A(x). };
        solve f, p project R

    ///////////////////////////////////////////////////////////////////////////////
    /// Char                                                                    ///
    ///////////////////////////////////////////////////////////////////////////////
    @CompileTest
    def testCharLit01(): #{ R(Char) } =
        let f = #{ A(' '). };
        let p = #{ R(x) :- A(x). };
        solve f, p project R

    @CompileTest
    def testCharLit02(): #{ R(Char) } =
        let f = #{ A('a'). };
        let p = #{ R(x) :- A(x). };
        solve f, p project R

    @CompileTest
    def testCharLit03(): #{ R(Char) } =
        let f = #{ A('x'). };
        let p = #{ R(x) :- A(x). };
        solve f, p project R

    ///////////////////////////////////////////////////////////////////////////////
    /// Int8                                                                    ///
    ///////////////////////////////////////////////////////////////////////////////
    @CompileTest
    def testInt8Lit01(): #{ R(Int8) } =
        let x = #{ R(0i8). };
        solve x

    @CompileTest
    def testInt8Lit02(): #{ R(Int8) } =
        let x = #{ R(123i8). };
        solve x

    @CompileTest
    def testInt8Lit03(): #{ R(Int8) } =
        let x = #{ R(-123i8). };
        solve x

    ///////////////////////////////////////////////////////////////////////////////
    /// Int16                                                                   ///
    ///////////////////////////////////////////////////////////////////////////////
    @CompileTest
    def testInt16Lit01(): #{ R(Int16) } =
        let x = #{ R(0i16). };
        solve x

    @CompileTest
    def testInt16Lit02(): #{ R(Int16) } =
        let x = #{ R(123i16). };
        solve x

    @CompileTest
    def testInt16Lit03(): #{ R(Int16) } =
        let x = #{ R(-123i16). };
        solve x


    ///////////////////////////////////////////////////////////////////////////////
    /// Int32                                                                   ///
    ///////////////////////////////////////////////////////////////////////////////
    @CompileTest
    def testInt32Lit01(): #{ R(Int32) } =
        let x = #{ R(0i32). };
        solve x

    @CompileTest
    def testInt32Lit02(): #{ R(Int32) } =
        let x = #{ R(123i32). };
        solve x

    @CompileTest
    def testInt32Lit03(): #{ R(Int32) } =
        let x = #{ R(-123i32). };
        solve x

    ///////////////////////////////////////////////////////////////////////////////
    /// Int64                                                                   ///
    ///////////////////////////////////////////////////////////////////////////////
    @CompileTest
    def testInt64Lit01(): #{ R(Int64) } =
        let x = #{ R(0i64). };
        solve x

    @CompileTest
    def testInt64Lit02(): #{ R(Int64) } =
        let x = #{ R(123i64). };
        solve x

    @CompileTest
    def testInt64Lit03(): #{ R(Int64) } =
        let x = #{ R(-123i64). };
        solve x

    ///////////////////////////////////////////////////////////////////////////////
    /// BigInt                                                                  ///
    ///////////////////////////////////////////////////////////////////////////////
    @CompileTest
    def testBigIntLit01(): #{ R(BigInt) } =
        let x = #{ R(0ii). };
        solve x

    @CompileTest
    def testBigIntLit02(): #{ R(BigInt) } =
        let x = #{ R(123ii). };
        solve x

    @CompileTest
    def testBigIntLit03(): #{ R(BigInt) } =
        let x = #{ R(-123ii). };
        solve x

    ///////////////////////////////////////////////////////////////////////////////
    /// String                                                                  ///
    ///////////////////////////////////////////////////////////////////////////////
    @CompileTest
    def testStringLit01(): #{ R(String) } =
        let x = #{ R(""). };
        solve x

    @CompileTest
    def testStringLit02(): #{ R(String) } =
        let x = #{ R("hello"). };
        solve x

    @CompileTest
    def testStringLit03(): #{ R(String) } =
        let x = #{ R("hello world!"). };
        solve x


    ///////////////////////////////////////////////////////////////////////////////
    /// List                                                                    ///
    ///////////////////////////////////////////////////////////////////////////////

    @Test
    def testLitList01(): Unit \ Assert =
        let p = #{
            A((Nil: List[Int32])).
            B((Nil: List[Int32])).
            R(x) :- A(x), B(x).
        };
        let r = query p select x from R(x);
        assertMemberOf(Nil, r)

    @Test
    def testLitList02(): Unit \ Assert =
        let p = #{
            A((Nil: List[String])).
            B((Nil: List[String])).
            R(x) :- A(x), B(x).
        };
        let r = query p select x from R(x);
        assertMemberOf(Nil, r)

    @Test
    def testLitList03(): Unit \ Assert =
        let p = #{
            A(1 :: 2 :: Nil).
            B(1 :: 2 :: Nil).
            R(x) :- A(x), B(x).
        };
        let r = query p select x from R(x);
        assertMemberOf(1 :: 2 :: Nil, r)

    @Test
    def testLitList04(): Unit \ Assert =
        let p = #{
            A("hello" :: "world" :: Nil).
            B("hello" :: "world" :: Nil).
            R(x) :- A(x), B(x).
        };
        let r = query p select x from R(x);
        assertMemberOf("hello" :: "world" :: Nil, r)


    ///////////////////////////////////////////////////////////////////////////////
    /// Option                                                                  ///
    ///////////////////////////////////////////////////////////////////////////////

    @Test
    def testLitOption01(): Unit \ Assert =
        let p = #{
            A((None: Option[Int32])).
            B((None: Option[Int32])).
            R(x) :- A(x), B(x).
        };
        let r = query p select x from R(x);
        assertMemberOf(None, r)

    @Test
    def testLitOption02(): Unit \ Assert =
        let p = #{
            A((None: Option[String])).
            B((None: Option[String])).
            R(x) :- A(x), B(x).
        };
        let r = query p select x from R(x);
        assertMemberOf(None, r)

    @Test
    def testLitOption03(): Unit \ Assert =
        let p = #{
            A(Some(123)).
            B(Some(123)).
            R(x) :- A(x), B(x).
        };
        let r = query p select x from R(x);
        assertMemberOf(Some(123), r)

    @Test
    def testLitOption04(): Unit \ Assert =
        let p = #{
            A(Some("Hello World")).
            B(Some("Hello World")).
            R(x) :- A(x), B(x).
        };
        let r = query p select x from R(x);
        assertMemberOf(Some("Hello World"), r)

    ///////////////////////////////////////////////////////////////////////////////
    /// Result                                                                  ///
    ///////////////////////////////////////////////////////////////////////////////

    @Test
    def testLitResult01(): Unit \ Assert =
        let p = #{
            A((Ok(123): Result[Int32, Int32])).
            B((Ok(123): Result[Int32, Int32])).
            R(x) :- A(x), B(x).
        };
        let r = query p select x from R(x);
        assertMemberOf(Ok(123), r)

    @Test
    def testLitResult02(): Unit \ Assert =
        let p = #{
            A((Ok("Hello World"): Result[Int32, String])).
            B((Ok("Hello World"): Result[Int32, String])).
            R(x) :- A(x), B(x).
        };
        let r = query p select x from R(x);
        assertMemberOf(Ok("Hello World"), r)

    @Test
    def testLitResult03(): Unit \ Assert =
        let p = #{
            A(Ok(123)).
            B(Ok(123)).
            A(Err(456)).
            B(Err(456)).
            R(x) :- A(x), B(x).
        };
        let r = query p select x from R(x);
        assertMemberOf(Ok(123), r)


    ///////////////////////////////////////////////////////////////////////////////
    /// Set                                                                     ///
    ///////////////////////////////////////////////////////////////////////////////

    @Test
    def testLitSet01(): Unit \ Assert =
        let p = #{
            A((Set#{}: Set[Int32])).
            B((Set#{}: Set[Int32])).
            R(x) :- A(x), B(x).
        };
        let r = query p select x from R(x);
        assertMemberOf(Set#{}, r)

    @Test
    def testLitSet02(): Unit \ Assert =
        let p = #{
            A(Set#{1}).
            B(Set#{1}).
            R(x) :- A(x), B(x).
        };
        let r = query p select x from R(x);
        assertMemberOf(Set#{1}, r)

    @Test
    def testLitSet03(): Unit \ Assert =
        let p = #{
            A(Set#{1, 2}).
            B(Set#{2, 1}).
            R(x) :- A(x), B(x).
        };
        let r = query p select x from R(x);
        assertMemberOf(Set#{1, 2}, r)

    @Test
    def testLitSet04(): Unit \ Assert =
        let p = #{
            A(Set#{1, 2, 3}).
            B(Set#{3, 2, 1}).
            R(x) :- A(x), B(x).
        };
        let r = query p select x from R(x);
        assertMemberOf(Set#{1, 2, 3}, r)


}

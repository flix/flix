mod Test.TypeAlias.Rel {

    //rel C(x: Int32, y: Int32, z: Int32)

    //@test
    //def testTypeAlias01(): #{A} = #{
      //  A(1). A(2). A(3).
    //}

    //@test
    //def testTypeAlias02(): #{A, B} = #{
      //  A(1). A(2). A(3).
        //B(1, 2). B(2, 3).
    //}

    //@test
    //def testTypeAlias03(): #{A, B, C} = #{
      //  A(1). A(2). A(3).
      //  B(1, 2). B(2, 3).
      //  C(1, 2, 3).
    //}

    //rel D(x: List[Int32])
    //rel E(x: Option[Int32])
    //rel F(x: Result[String, Bool])

    //@test
    //def testTypeAlias04(): #{D} = #{
     //   D(Nil). D(1 :: Nil).
    //}

    //@test
    //def testTypeAlias05(): #{D, E} = #{
      //  D(Nil). D(1 :: Nil).
       // E(None). E(Some(123)).
   // }

    //@test
    //def testTypeAlias06(): #{D, E, F} = #{
     //   D(Nil). D(1 :: Nil).
      //  E(None). E(Some(123)).
        //F(Ok(true)). F(Err("Hello World!")).
    //}

   // rel G[a](x: a)
    //rel H[a, b](x: a, y: b)
    //rel I[a, b, c](x: a, y: b, z: c)

   // @test
    //def testPolyTypeAlias01(): #{G[Int32]} = #{
      //  G(1). G(2). G(3).
    //}

   // @test
    //def testPolyTypeAlias02(): #{G[String]} = #{
      //  G("x"). G("x"). G("x").
    //}

    //@test
    //def testPolyTypeAlias03(): #{H[Bool, Int32]} = #{
      //  H(true, 21). H(false, 42).
    //}

    //@test
    //def testPolyTypeAlias04(): #{H[Int32, Bool]} = #{
      //  H(21, true). H(42, false).
    //}

    //@test
    //def testPolyTypeAlias05(): #{I[Bool, Int32, String]} = #{
      //  I(true, 21, "Hello"). I(false, 42, "World!").
    //}

   // @test
    //def testPolyTypeAlias06(): #{I[Int32, String, Bool]} = #{
      //  I(21, "Hello", true). I(42, "World!", false).
    //}

    //@test
    //def testPolyTypeAlias07(): #{G[Option[Int32]]} = #{
      //  G(None). G(Some(21)). G(Some(42)).
    //}

    //@test
    //def testPolyTypeAlias08(): #{G[Int32], H[Bool, Int32], I[Bool, Int32, String]} = #{
      //  G(1). G(2). G(3).
        //H(true, 21). H(false, 42).
        //I(true, 21, "Hello"). I(false, 42, "World!").
    //}

   // @test
    //def testIgnoredTypeAlias01(): #{A(String)} = #{
      //  A("x"). A("x"). A("x").
    //}

    @test
    def testIgnoredTypeAlias02(): #{A(String), B(String)} = #{
        A("x"). A("x"). A("x").
        B("x"). B("x"). B("x").
    }

    @test
    def testIgnoredTypeAlias03(): #{A(String), B(String), C(String)} = #{
        A("x"). A("x"). A("x").
        B("x"). B("x"). B("x").
        C("x"). C("x"). C("x").
    }

    //@test
    //def testMixed01(): #{A, B(String), C(String)} = #{
      //  A(1). A(2). A(3).
        //B("x"). B("x"). B("x").
        //C("x"). C("x"). C("x").
    //}

    //@test
    //def testMixed02(): #{A(String), B, C(String)} = #{
      //  A("x"). A("x"). A("x").
        //B(1, 2). B(2, 3).
        //C("x"). C("x"). C("x").
   // }

    //@test
    //def testMixed03(): #{A(String), B(String), C} = #{
      //  A("x"). A("x"). A("x").
        //B("x"). B("x"). B("x").
        //C(1, 2, 3).
    //}

}

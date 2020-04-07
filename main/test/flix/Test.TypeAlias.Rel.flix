namespace Test/TypeAlias/Rel {

    rel A(x: Int)
    rel B(x: Int, y: Int)
    rel C(x: Int, y: Int, z: Int)

    @test
    def testTypeAlias01(): #{A} = #{
        A(1). A(2). A(3).
    }

    @test
    def testTypeAlias02(): #{A, B} = #{
        A(1). A(2). A(3).
        B(1, 2). B(2, 3).
    }

    @test
    def testTypeAlias03(): #{A, B, C} = #{
        A(1). A(2). A(3).
        B(1, 2). B(2, 3).
        C(1, 2, 3).
    }

    @test
    def testIgnoredTypeAlias01(): #{A(String)} = #{
        A("x"). A("x"). A("x").
    }

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

    @test
    def testMixed01(): #{A, B(String), C(String)} = #{
        A(1). A(2). A(3).
        B("x"). B("x"). B("x").
        C("x"). C("x"). C("x").
    }

    @test
    def testMixed02(): #{A(String), B, C(String)} = #{
        A("x"). A("x"). A("x").
        B(1, 2). B(2, 3).
        C("x"). C("x"). C("x").
    }

    @test
    def testMixed03(): #{A(String), B(String), C} = #{
        A("x"). A("x"). A("x").
        B("x"). B("x"). B("x").
        C(1, 2, 3).
    }

}

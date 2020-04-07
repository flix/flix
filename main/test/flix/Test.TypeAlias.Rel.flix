namespace Test/TypeAlias/Rel {

    rel A(x: Int)
    rel B(x: Int, y: Int)
    rel C(x: Int, y: Int, z: Int)

    @test
    def typeAlias01(): #{A} = #{
        A(1). A(2). A(3).
    }

    @test
    def typeAlias02(): #{A, B} = #{
        A(1). A(2). A(3).
        B(1, 2). B(2, 3).
    }

    @test
    def typeAlias03(): #{A, B, C} = #{
        A(1). A(2). A(3).
        B(1, 2). B(2, 3).
        C(1, 2, 3).
    }

    @test
    def ignoredTypeAlias01(): #{A(String)} = #{
        A("x"). A("x"). A("x").
    }

    @test
    def ignoredTypeAlias02(): #{A(String), B(String)} = #{
        A("x"). A("x"). A("x").
        B("x"). B("x"). B("x").
    }

    @test
    def ignoredTypeAlias03(): #{A(String), B(String), C(String)} = #{
        A("x"). A("x"). A("x").
        B("x"). B("x"). B("x").
        C("x"). C("x"). C("x").
    }

}

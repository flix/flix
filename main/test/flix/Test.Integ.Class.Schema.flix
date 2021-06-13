namespace Test/Integ/Class/Schema {
    rel R(x: Int, y: Bool)

    lawless class C[a] {
        pub def f(_x: a): #{ R(Int, Bool) } = #{
            R(1, true).
        }
    }

    lawless class D[a] {
        pub def f(x: a): #{ R(Int, Bool) }
    }

    instance C[Int]

    instance D[Int] {
        pub def f(_x: Int): #{ R(Int, Bool) } = #{
            R(2, false).
        }
    }

    @test
    def testSchemaInSig01(): Bool = {
        let _ = Test/Integ/Class/Schema/C.f(5);
        true
    }

    @test
    def testSchemaInSig02(): Bool = {
        let _ = Test/Integ/Class/Schema/D.f(5);
        true
    }
}
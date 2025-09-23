mod Test.Integ.Trait.Schema {
    trait C[a] {
        pub def f(_x: a): #{ R(Int32, Bool) } = #{
            R(1, true).
        }
    }

    trait D[a] {
        pub def f(x: a): #{ R(Int32, Bool) }
    }

    instance C[Int32]

    instance D[Int32] {
        pub def f(_x: Int32): #{ R(Int32, Bool) } = #{
            R(2, false).
        }
    }

    @test
    def testSchemaInSig01(): Bool = {
        let _ = Test.Integ.Trait.Schema.C.f(5);
        true
    }

    @test
    def testSchemaInSig02(): Bool = {
        let _ = Test.Integ.Trait.Schema.D.f(5);
        true
    }
}

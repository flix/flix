mod Test.Integ.Class.Schema {
    class C[a] {
        pub def f(_x: a): #{ R(Int32, Bool) } = #{
            R(1, true).
        }
    }

    class D[a] {
        pub def f(x: a): #{ R(Int32, Bool) }
    }

    instance C[Int32]

    instance D[Int32] {
        pub def f(_x: Int32): #{ R(Int32, Bool) } = #{
            R(2, false).
        }
    }

    @Test
    def testSchemaInSig01(): Bool = {
        let _ = Test.Integ.Class.Schema.C.f(5);
        true
    }

    @Test
    def testSchemaInSig02(): Bool = {
        let _ = Test.Integ.Class.Schema.D.f(5);
        true
    }
}

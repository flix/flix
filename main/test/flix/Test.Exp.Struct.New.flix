mod Test.Exp.Struct.New {
    struct S[r] {
        a: Int32
    }
    struct S2[r] {
        a: S[r]
    }
    @test
    def foo(): Unit =
        region rc {
            let s = new S {a = 5} @ rc;
            let s2 = new S2 {a = s} @ rc;
            ()
        }

}

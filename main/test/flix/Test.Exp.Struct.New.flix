mod Test.Exp.Struct.New {

    struct S1[r] {
        _a: Ref[Int32, r]
    }
    @test
    def foo(): Unit = {
        region rc {
            new S1 { _a = Ref.fresh(rc, 3) } @ rc;
            ()
        }
    }
}

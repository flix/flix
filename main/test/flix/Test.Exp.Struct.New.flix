mod Test.Exp.Struct.New {
    struct R[r2] { }
    struct S[r2] { a: R[r2] }
    def foo(): Unit = {
        region rc {
            new S { a = new R {} @ rc } @ rc;
            ()
        }
    }
}

mod Test.Exp.Match.Struct {
    enum Box[r: Eff](Ref[Bool, r])

    @test
    def testMatchRef01(): Bool = region rc {
        match Box.Box(Ref.fresh(rc, true)) {
            case Box.Box(r) => Ref.get(r)
        }
    }
}

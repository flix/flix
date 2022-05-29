namespace Test/Exp/New {

    @test
    def testNew01(): MutList[Int32, static] & Impure =
        new MutList(static): MutList[Int32, static]

    @test
    def testNew02(): MutList[Int32, static] & Impure =
        let l = new MutList(static);
        MutList.push!(1, l);
        l

    @test
    def testNew03(): MutList[Int32, static] & Impure =
        let l = new MutList(static);
        MutList.push!(1, l);
        MutList.push!(2, l);
        MutList.push!(3, l);
        l

    @test
    def testNew04(): Unit = region r {
        let l = new MutList(r);
        ()
    }

    @test
    def testNew05(): Int32 = region r {
        let l = new MutList(r);
        MutList.push!(1, l);
        match MutList.pop!(l) {
            case Some(x) => x
            case None    => unreachable!()
        }
    }

}

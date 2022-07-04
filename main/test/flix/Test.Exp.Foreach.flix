namespace Test/Exp/Foreach {

    @test
    def testForeach01(): Bool = region r {
        let y = ref 0 @ r;
        foreach (x <- 1 :: Nil) y := x + 1;
        deref y == 2
    }

}

namespace Test/Exp/Foreach {

    @test
    def testForeach01(): Int32 = // Dummy program that just checks compiler phases
        foreach (x <- xs) x + 1

}

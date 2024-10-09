mod TestMonadic {
    use Monadic.{Arbitrary, Gen, applyGen};

    def sampleNTimes(n: Int32, size: Int32, r: Int32, gen: Monadic.Gen[a], verbose: Bool): List[a] \ IO with Arbitrary[a], ToString[a] =
    match n {
        case 0 => 
            println("Test finished!");
            Nil
        case _ => 
            let (r1, r2) = splitRand(r); // Old rand
            let res = Monadic.applyGen(gen, size, r1);
            if (verbose) 
                println(res)
            else 
                ();
            res :: sampleNTimes(n - 1, size, r2, gen, verbose)
}
    pub def testRandomListDistribution(): Unit \ IO =
        let gen: Gen[List[Int32]] = Monadic.Arbitrary.arbitrary();
        let numberTests = 1000;
        let ll = sampleNTimes(numberTests, 20, 124456, gen, true);
        let res = List.foldLeft((acc, x) -> acc + List.length(x), 0, ll);
        let avg = Int32.toFloat64(res) / Int32.toFloat64(numberTests);
        println("Average length of lists: ${avg}");
        println("Monadic 1 testing done")
}
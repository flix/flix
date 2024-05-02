mod TestFlixCheck1 {
    use FlixCheck1.{Gen, Arbitrary, applyGen}

    def sampleNTimes(n: Int32, size:Int32, r: Int32, gen: Gen[a], verbose: Bool): List[a] \ IO with Arbitrary[a], ToString[a] =
        match n {
            case 0 => 
                println("Test finished!");
                Nil
            case _ => 
                let (r1, r2) = splitRand(r);
                let res = applyGen(gen, size, r1);
                if (verbose) 
                    println(res)
                else 
                    ();
                res :: sampleNTimes(n - 1, size, r2, gen, verbose)
    }

    // Testing the statistical distribution of the random number generator
    // The sums should be close to 0, but for now they are not. This is a known issue.
    pub def sampleListWithBoolsAndInts(): Unit \ IO =
        let seeds = List#{1234567891, 543212345, 975312468, 864213579, 555555555};
        foreach(seed <- seeds)
            let gen: Gen[(Bool, Int32)] = Arbitrary.arbitrary();
            // Number of tests
            let ntimes = 2000;
            // The range of the integers, from -i to i
            let i = 5;
            let l: List[(Bool, Int32)] = sampleNTimes(ntimes, i, seed, gen, false);
            
            let intlist : List[Int32] = List.map(x -> snd(x), l);
            let boollist : List[Bool] = List.map(x -> fst(x), l);
            let sumForInt = List.foldLeft((acc, x) -> acc + x, 0, intlist);
            let sumForBool = List.foldLeft((acc, x) -> if (x) acc + 1 else acc - 1, 0, boollist);
            println("Total sum should be within the bounds ${ntimes * -i} to ${ntimes * i}");
            println("Expected sum is 0, Sum for integers: ${sumForInt}");
            println("Expected sum is 0, Sum for booleans: ${sumForBool}")

    /// testing the distribution of the random number generator, but with the new verbose printing
    pub def testGenIntListVerbose(): Unit \ IO =
        let gen: Gen[List[Int32]] = Arbitrary.arbitrary();
        let _ = sampleNTimes(10, 5, 70001, gen, true);
        println("Done")
}
use SplittableRandom.{split, freshWithSeed};

def sampleNTimes(n: Int32, size: Int32, r: Rand, gen: Gen[a], verbose: Bool): List[a] \ IO with Arbitrary[a], ToString[a] =
match n {
    case 0 => 
        println("Test finished");
        Nil
    case _ => 
        let (r1, r2) = split(r); //new rand
        let res = applyGen(gen, size, r1);
        if (verbose) 
            println(res)
        else 
            ();
        res :: sampleNTimes(n - 1, size, r2, gen, verbose)
}
pub def testRandomListDistribution(): Unit \ IO =
    let gen: Gen[List[Int32]] = Arbitrary.arbitrary();
    let numberTests = 1000;
    let size = 50;
    let ll = sampleNTimes(numberTests, size, freshWithSeed(1292645i64), gen, false);
    let res = List.foldLeft((acc, x) -> acc + List.length(x), 0, ll);
    let avgListLength = Int32.toFloat64(res) / Int32.toFloat64(numberTests);
    println("Expected average length of lists: ${size/2}");
    println("Average length of lists: ${avgListLength}");
    println("Monadic 2 testing done")

pub def testListOfString(): Unit \ IO =
    let gen: Gen[List[String]] = Arbitrary.arbitrary();
    let ll = sampleNTimes(10, 5, freshWithSeed(1292645i64), gen, false);
    foreach (l <- ll) {
        println(l)
    }


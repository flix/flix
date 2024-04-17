use Gen.{Gen, Arbitrary, applyGen, Rand};
use SplittableRandom.{split, freshWithSeed};

def sampleNTimes(n: Int32, size: Int32, r: Rand, gen: Gen[a], verbose: Bool): List[a] \ IO with Arbitrary[a], ToString[a] =
match n {
    case 0 => 
        println("Test finished!");
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
    let ll = sampleNTimes(numberTests, 20, freshWithSeed(1292645i64), gen, true);
    let res = List.foldLeft((acc, x) -> acc + List.length(x), 0, ll);
    let avg = Int32.toFloat64(res) / Int32.toFloat64(numberTests);
    println("Average length of lists: ${avg}");
    println("Monadic 2 testing done")

pub def testListOfString(): Unit \ IO =
    let gen: Gen[List[String]] = Arbitrary.arbitrary();
    let ll = sampleNTimes(5, 10, freshWithSeed(1292645i64), gen, false);
    foreach (l <- ll) {
        println(l)
    }


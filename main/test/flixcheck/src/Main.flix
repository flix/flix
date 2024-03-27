// use FlixCheck1.{Arbitrary, Gen};
use FlixCheck2.{Arbitrary, Gen, applyGen};

/// Splitting a random value using Linear Congruential Generator.
def splitRand(r: Int32): (Int32, Int32) =
    if (r == 0)
        splitRand(1)
    else
        let r1 = Int32.remainder(520332806 * r, 536870909);
        let r2 = Int32.remainder(246049789 * r, 268435399);
        (r1, r2)

def main(): Unit \ IO =

    // let gen: Gen[List[Int32]] = Arbitrary.arbitrary();
    // let ll = testNTimes(100, 6, 70077, gen, true);
    // let lengthsum = List.foldLeft((acc, x) -> acc + List.length(x), 0, ll);
    // println(lengthsum)

    // let gen: Gen[Int32] = Arbitrary.arbitrary();
    // let ll = testNTimes(100, 10, 43568, gen, true);
    // let sumOfInts = List.foldLeft((acc, x) -> acc + x, 0, ll);
    // println(sumOfInts)

    // let gen: Gen[Float32] = Arbitrary.arbitrary();
    // let ll = testNTimes(200, 10, 123456789, gen, true);
    // let sumOfPos = List.foldLeft((acc, x) -> if (x > 0.0f32) acc + 1 else acc, 0, ll);
    // println(sumOfPos)
    
    let listGen: Gen[List[Int32]] = FlixCheck2.vector(20);
    println(applyGen(listGen, 1000, 123456786))
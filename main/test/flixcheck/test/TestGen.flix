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

pub def testFloat32(): Unit \ IO =
    let gen: Gen[Float32] = Arbitrary.arbitrary();
    let ll = sampleNTimes(10, 20, freshWithSeed(Time.Epoch.microseconds()), gen, false);
    foreach (l <- ll) {
            println(l)
        }

pub def testFloat32_2(): Unit \ IO =
    let gen: Gen[Float32] = Arbitrary.arbitrary();
    def prop(x: Float32): Bool = x >= -18.0f32 and x <= 18.0f32;
    flixCheck(forAll(gen, prop))

pub def testFloat64(): Unit \ IO =
    let gen: Gen[Float64] = Arbitrary.arbitrary();
    let ll = sampleNTimes(10, 20, freshWithSeed(Time.Epoch.microseconds()), gen, false);
    foreach (l <- ll) {
            println(l)
        }
pub def testInt32(): Unit \ IO =
    let gen: Gen[Int32] = Arbitrary.arbitrary();
    let ll = sampleNTimes(10, 20, freshWithSeed(Time.Epoch.microseconds()), gen, false);
    foreach (l <- ll) {
            println(l)
        }

pub def testInt32_2(): Unit \ IO =
    let gen: Gen[Int32] = Arbitrary.arbitrary();
    def prop(x: Int32): Bool = x >= -18 and x <= 18;
    flixCheck(forAll(gen, prop))

pub def testInt16(): Unit \ IO =
    let gen: Gen[Int16] = Arbitrary.arbitrary();
    let ll = sampleNTimes(10, 20, freshWithSeed(Time.Epoch.microseconds()), gen, false);
    foreach (l <- ll) {
            println(l)
        }
pub def testInt16_2(): Unit \ IO =
    let gen: Gen[Int16] = Arbitrary.arbitrary();
    def prop(x: Int16): Bool = x >= -18i16 and x <= 18i16;
    flixCheck(forAll(gen, prop))

pub def testInt8(): Unit \ IO =
    let gen: Gen[Int8] = Arbitrary.arbitrary();
    let ll = sampleNTimes(10, 20, freshWithSeed(Time.Epoch.microseconds()), gen, false);
    foreach (l <- ll) {
            println(l)
        }
pub def testInt8_2(): Unit \ IO =
    let gen: Gen[Int8] = Arbitrary.arbitrary();
    def prop(x: Int8): Bool = x >= -18i8 and x <= 18i8;
    flixCheck(forAll(gen, prop))

pub def testInt8Bounds(): Unit \ IO =
    let gen: Gen[Int8] = chooseInt8(-127, 25);
    def prop(x: Int8): Bool = x >= -128i8 and x <= 127i8;
    verboseCheck(forAll(gen, prop))

pub def testInt16Bounds(): Unit \ IO =
    let gen: Gen[Int16] = chooseInt16(-32767, 32767);
    def prop(x: Int16): Bool = x >= -32768i16 and x <= 32767i16;
    flixCheck(forAll(gen, prop))

pub def testInt64Bounds(): Unit \ IO =
    let gen: Gen[Int64] = chooseInt64(Int64.minValue(), Int64.maxValue());
    def prop(x: Int64): Bool = x >= -9223372036854775808i64 and x <= 9223372036854775806i64;
    flixCheck(forAll(gen, prop))

pub def testInt64(): Unit \ IO =
        let gen: Gen[Int64] = Arbitrary.arbitrary();
    let ll = sampleNTimes(100, 2000000000, freshWithSeed(Time.Epoch.microseconds()), gen, false);
    foreach (l <- ll) {
            println(l)
        }
pub def testBigInt(): Unit \ IO =
    let gen: Gen[BigInt] = Arbitrary.arbitrary();
    let ll = sampleNTimes(10, 20000, freshWithSeed(Time.Epoch.microseconds()), gen, false);
    foreach (l <- ll) {
            println(l)
        }
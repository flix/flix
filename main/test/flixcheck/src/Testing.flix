use Gen.{Gen}
use SplittableRandom.{split}

///
/// The configuration of a test
///
/// maxTest: The maximum number of tests to run
///
/// maxFail: The maximum number of failed tests before the arguments are considered exhausted
///
/// size: A function that defines the size of the test arguments
///
/// every: The every function returns a string with the number of tests and the arguments of the test
///
/// initSeed: The initial seed used for the random number generator
type alias Config = {
    maxTest = Int32,
    maxFail = Int32,
    size = Int32 -> Int32,
    every = Int32 -> List[String] -> String,
    initSeed = Option[Int64]
}
/// Generates the test arguments for the test
pub def generate(size: Int32, rand: SplittableRandom, gen: Gen[a]): a \ IO =
    match gen {
        case Gen.Gen(g) => 
            let (r1, _) = split(rand);
            g(size, r1)  
    }

/// A default setting for the config
pub def default(): Config = {
    // The maximum number of tests to run is 100 by default 
    maxTest = 100,
    // The maximum number of failed tests before the test is considered failed, 1000 by default
    maxFail = 1000,
    // The size of the test is n/2 + 3 by default
    size = n -> n/2 + 3,
    // The every function returns a string with the number of tests and the arguments of the test
    every = _ -> _ -> "",
    // The initial seed is None by default, and hence produces a new random seed
    initSeed = None
}

/// A function that creates a config with the given parameters
pub def createConfig(maxTest: Int32, maxFail: Int32, size: Int32 -> Int32, every: Int32 -> List[String] -> String, initSeed: Option[Int64]): Config = {
    maxTest = maxTest,
    maxFail = maxFail,
    size = size,
    every = every,
    initSeed = initSeed
}

/// A verbose version of the default config, prints more detailed information about the tests
pub def verbose(): Config = 
    let func = (n -> args -> 
        "${n}" + ":  " + String.intercalate(", ", args) + "\n"
    );
    {every = func | default()}

/// Standard flixCheck function. Uses default config.
pub def flixCheck(x: a): Unit \ IO with Testable[a] = 
    check(default(), x)

/// Standard verbose version of flixCheck. Uses default config.
pub def verboseCheck(x: a): Unit \ IO with Testable[a] = 
    check(verbose(), x)

/// The actual testing function
/// Should be used by the programmer when a specific test size or max test amount is desired
pub def check(config: Config, x: a): Unit \ IO with Testable[a] = 
    match config.initSeed {
        case None => 
            let seed = Time.Epoch.microseconds();
            tests({initSeed = Some(seed) | config}, evaluate(x), SplittableRandom.freshWithSeed(seed), 0, 0, Nil)
        case Some(s) => 
            tests({initSeed = Some(s) | config}, evaluate(x), SplittableRandom.freshWithSeed(s), 0, 0, Nil)
    }

///The actual testing function when a specific seed is desired
pub def checkWithSeed(config: Config, x: a, seed: Int64): Unit \ IO with Testable[a] = 
    let configWithSeed = {initSeed = Some(seed) | config};
    let seededRng = SplittableRandom.freshWithSeed(seed);
    tests(configWithSeed, evaluate(x), seededRng, 0, 0, Nil)

/// 
pub def tests(config: Config, gen: Gen[TResult], rng: SplittableRandom, nTest: Int32, nFail: Int32, stamps: List[List[String]]): Unit \ IO = 
    if (nTest == config.maxTest) {
        done("OK, passed", nTest, stamps)
    } else if (nFail == config.maxFail) {
        done("Arguments exhausted after", nTest, stamps)
    } else {
        let (r1, r2) = split(rng);
        let result = generate(config.size(nTest), r2, gen);
        System.StdOut.print(config.every(nTest, result.arguments));
        match result.ok {
            case None =>
                tests(config, gen, r1, nTest, (nFail + 1), stamps)
            case Some(true) =>
                tests(config, gen, r1, (nTest + 1), nFail, (result.stamp :: stamps))
            case Some(false) =>
                if (result.arguments == Nil) {
                    println("Falsifiable after " + "${nTest + 1}" + " test(s):\n" + "No arguments.");
                    printSeed(config.initSeed)
                } else {
                    println("Falsifiable after " + "${nTest + 1}" + " test(s) with argument(s):\n" + String.intercalate("\n", result.arguments));
                    printSeed(config.initSeed)
                }
        }
    }

/// Prints the seed used in the test
pub def printSeed(seed: Option[Int64]): Unit \ IO = 
    match seed {
        case None => println("No seed used")
        case Some(s) => println("Using seed: ${s}")
    }

/// Prints the result of the test
pub def done(msg: String, nTest: Int32, stamps: List[List[String]]): Unit \ IO = 
    // Helper functions
    // percentage calculates the distribution of the numbers used in the tests 
    def percentage(n: Int32, m: Int32): String =
        "${(100.0f32 * Int32.toFloat32(n)) / Int32.toFloat32(m)}" + "%";
    def entry(pair: (Int32, List[String])): String =
        percentage(fst(pair), nTest) + " " + String.intercalate(", ", snd(pair));
    def pairLength(xs: List[List[String]]): (Int32, List[String]) =
        match xs {
            case Nil => (0, Nil),
            case head :: _ => (List.length(xs), head)
        };
    let table = (
        List.filter(x -> not List.isEmpty(x), stamps) |>
        List.sort |>
        List.groupBy(Eq.eq) |>
        List.map(pairLength) |>
        List.sort |>
        List.reverse |>
        List.map(entry) |>
        display
    );
    println(msg + " " + "${nTest}" + " tests" + table)

/// Defines how the test results are displayed
pub def display(l: List[String]): String =
    match l {
        case Nil => ".\n",
        case x :: Nil => " " + "(${x}).\n",
        case xs => ".\n" + String.intercalate("\n", List.map(s -> s + ".", xs))
    }

    

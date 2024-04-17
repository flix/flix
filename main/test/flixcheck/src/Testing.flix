
use Testable.{Testable, TFunc, TResult, evaluate}
use Gen.{Gen}
use SplittableRandom.{split}

type alias Config = {
    maxTest = Int32,
    maxFail = Int32,
    size = Int32 -> Int32,
    every = Int32 -> List[String] -> String
}

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
    
    every = _ -> _ -> ""
}

pub def createConfig(maxTest: Int32, maxFail: Int32, size: Int32 -> Int32, every: Int32 -> List[String] -> String): Config = {
    maxTest = maxTest,
    maxFail = maxFail,
    size = size,
    every = every
}

pub def verbose(): Config = 
    let func = (n -> args -> 
        "${n}" + ":  " + String.intercalate(", ", args) + "\n"
    );
    {every = func | default()}

pub def flixCheck(x: a): Unit \ IO with Testable[a] = 
    check(default(), x)

pub def verboseCheck(x: a): Unit \ IO with Testable[a] = 
    check(verbose(), x)

pub def check(config: Config, x: a): Unit \ IO with Testable[a] = 
    let freshRng = SplittableRandom.fresh();
    tests(config, evaluate(x), freshRng, 0, 0, Nil)

pub def checkWithSeed(config: Config, x: a, seed: Int64): Unit \ IO with Testable[a] = 
    let seededRng = SplittableRandom.freshWithSeed(seed);
    tests(config, evaluate(x), seededRng, 0, 0, Nil)

pub def tests(config: Config, gen: Gen[TResult], rng: SplittableRandom, nTest: Int32, nFail: Int32, stamps: List[List[String]]): Unit \ IO = 
    if (nTest == config.maxTest) 
        done("OK, passed", nTest, stamps)
    else if (nFail == config.maxFail)
        done("Arguments exhausted after", nTest, stamps)
    else
        let (r1, r2) = split(rng);
        let result = generate(config.size(nTest), r2, gen);
        System.StdOut.print(config.every(nTest, result.arguments));
        match result.ok {
            case None =>
                tests(config, gen, r1, nTest, (nFail + 1), stamps)
            case Some(true) =>
                tests(config, gen, r1, (nTest + 1), nFail, (result.stamp :: stamps))
            case Some(false) =>
                if (result.arguments == Nil)
                    println("Falsifiable after " + "${nTest + 1}" + " test(s):\n" + "No arguments.")
                else
                    println("Falsifiable after " + "${nTest + 1}" + " test(s):\n" + String.intercalate("\n", result.arguments))
        }

pub def done(msg: String, nTest: Int32, stamps: List[List[String]]): Unit \ IO = 
    // Helper functions
    /// percentage calculates the distribution of the numbers used in the tests 
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

// Overvej om outputtet skal se anderledes ud
pub def display(l: List[String]): String =
    match l {
        case Nil => ".\n",
        case x :: Nil => " " + "(${x}).\n",
        case xs => ".\n" + String.intercalate("\n", List.map(s -> s + ".", xs))
    }

    

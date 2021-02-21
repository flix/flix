/*
 *  Copyright 2021 Magnus Madsen
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions not
 *  limitations under the License.
 */
namespace Test {

    use ToString.toString;

    ///
    /// An algebraic data type that represents a suite of tests.
    ///
    pub enum TestSuite {
        // TODO: Use TestPath
        case TestSuite(Map[List[String], TestGroup])
    }

    pub enum  TestGroup {
        case TestGroup(Map[String, List[Test]])
    }

    ///
    /// A fully qualified name to a test namespace.
    ///
    pub opaque type TestPath = List[String]

    instance Monoid[TestSuite] {
        def empty(): TestSuite = TestSuite(Map#{})

        def combine(x: TestSuite, y: TestSuite): TestSuite =
            let TestSuite(g1) = x;
            let TestSuite(g2) = y;
            TestSuite(Monoid.combine(g1, g2))
    }


    instance Monoid[TestGroup] {
        def empty(): TestGroup = TestGroup(Map#{})

        def combine(x: TestGroup, y: TestGroup): TestGroup =
            let TestGroup(s1) = x;
            let TestGroup(s2) = y;
            TestGroup(Monoid.combine(s1, s2))
    }


    ///
    /// Represents a single test.
    ///
    pub enum Test {
        case EqTest(Unit ~> TestOutcome)
    }

    ///
    /// Represents the outcome of a test.
    ///
    enum TestOutcome {
        case Success({time: Int32}),
        case Failure({actual: String, expect: String, time: Int32})
        // TODO: Exception
        // TODO: Timeout
    }

    ///
    /// Returns a test suite with the given fully-qualified name `fqn` and test map `m`.
    ///
    pub def defSuite(fqn: String, m: Map[String, Test]): TestSuite =
        let path = (String.splitOn(fqn, ".")) as & Pure;
        let tests = TestGroup(Map.map(v -> v :: Nil, m));
        TestSuite(Map.empty() |> Map.insert(path, tests))

    ///
    /// Returns an executable test case.
    ///
    /// A test consists of an actual value and an expected value.
    ///
    pub def defineTest[a : Eq : ToString](r: {actual: Lazy[a], expect: Lazy[a]}): Test =
        let runner = () -> {
                let expect = force r.expect;
                let (actual, time) = time(() -> force r.actual);
                if (expect == actual)
                    Success({time = time})
                else
                    Failure({actual = toString(actual), expect = toString(expect), time = time})
        };
        EqTest(runner)


    pub def runSuite(s: TestSuite): Int32 & Impure = {
        let TestSuite(m) = s;
        printTestHeader();
        let testOutComes = Map.mapWithKey(evalSuite, m);
        printFailingTestHeader();
        printFailingTests(testOutComes);
        printSummary(testOutComes);
        0
    }

    def evalSuite(path: List[String], g: TestGroup): Map[String, TestOutcome] & Impure = {
        beforeSuite(path, g);
        let TestGroup(innerM) = g;
        let result = Map.mapWithKey(evalTestGroup, innerM);
        afterSuite(path, result);
        result
    }

    def printFailingTests(o: Map[List[String], Map[String, TestOutcome]]): Unit & Impure = {
        let f = (fqn: List[String], outcomes: Map[String, TestOutcome]) -> {
            let g = (k: String, v: TestOutcome) -> {
                match v {
                    case Success(_) => () // nop
                    case Failure(r) =>
                        println("${red("+")} ${getName(fqn, k)} (unexpected value)");
                        println("    expect:  ${green(r.expect)}");
                        println("    actual:  ${yellow(r.actual)}");
                        println("")
                }
            };
            Map.foreach(g, outcomes)
        };
        Map.foreach(f, o)
    }

    def printSummary(o: Map[List[String], Map[String, TestOutcome]]): Unit & Impure = {
        let totalTests = Map.foldLeft((acc, m) -> acc + Map.size(m), 0, o);
        let totalSuccesses = Map.foldLeft((acc, m) -> acc + successful(m), 0, o);
        let totalFailures = Map.foldLeft((acc, m) -> acc + failures(m), 0, o);
        let color = if (totalFailures > 0) red else green;
        println("Tests: ${totalTests}, Passed: ${totalSuccesses}, Failed: ${totalFailures}." |> color)
    }

    def evalTestGroup(testName: String, testList: List[Test]): TestOutcome & Impure = {
        match testList {
            case t :: Nil =>
                let result = runTest(t);
                match result {
                    case Success(r) => {
                        onTestSuccess(testName, r.time);
                        result
                    }
                    case Failure(r) => {
                        onTestFailure(testName, 0);
                        result
                    }
                }
            case _ => ?fail
        }
    }

    def printTestHeader(): Unit & Impure = {
        println("------------------------ Running Tests ---------------------- ")
    }

    def printFailingTestHeader(): Unit & Impure = {
        println("------------------------ Failing Tests ---------------------- ")
    }

    def getName(l: List[String], s: String): String =
        List.foldRight((x, acc) -> x + "." + acc, s, l)

    ///
    /// Callback executed before a test suite is evaluated.
    ///
    def beforeSuite(path: List[String], innerM: TestGroup): Unit & Impure = {
        let namePart: String = gray(List.foldLeft((acc: String, elm: String) -> acc + "/" + elm, "", path));
        let numberOfTests: Int32 = size(innerM);
        let testsPart = "(${numberOfTests} tests)";
        println("${namePart} ${testsPart}")
    }

    def size(g: TestGroup): Int32 =
        let TestGroup(m) = g;
        Map.size(m)

    ///
    /// Callback executed after a test suite is evaluated.
    ///
    def afterSuite(_path: List[String], innerM: Map[String, TestOutcome]): Unit & Impure = {
        let numTests = Map.size(innerM);
        let numSuccess = successful(innerM);
        let numFailed = failures(innerM);
        println("  Tests: ${numTests}, Passed: ${numSuccess}, Failed: ${numFailed}.");
        println("")
    }

    ///
    /// Callback for when a test succeeds.
    ///
    def onTestSuccess(testName: String, testTime: Int32): Unit & Impure =
        let statusPart = "${green("+")}";
        let namePart = testName;
        let timePart = gray("${testTime} ms");
        println("  ${statusPart} ${namePart} ${timePart}")

    ///
    /// Callback for when a test fails.
    ///
    def onTestFailure(testName: String, _testTime: Int32): Unit & Impure =
        let statusPart = "${red("-")}";
        let namePart = testName: String;
        let messagePart = "(${red("unexpected value")})";
        println("  ${statusPart} ${namePart} ${messagePart}")

    ///
    /// Runs the given test `t`.
    ///
    def runTest(t: Test): TestOutcome & Impure = match t {
        case EqTest(f) => f()
    }

    ///
    /// Returns a pair with the result of `f` and the time it took.
    ///
    def time(f: Unit -> a): (a, Int32) & Impure = {
        import java.lang.System:nanoTime() as nanoTime;
        let t = nanoTime();
        let r = f();
        let e = nanoTime() - t;
        (r, Int64.clampToInt32(e / 1_000_000i64, 0, Int32.maxValue()))
    }

    ///
    /// Returns `true` if the given outcome is a success.
    ///
    def isSuccess(o: TestOutcome): Bool = match o {
        case Success(_) => true
        case Failure(_) => false
    }

    ///
    /// Returns `true` if the given outcome is a failure.
    ///
    def isFailure(o: TestOutcome): Bool = match o {
        case Success(_) => false
        case Failure(_) => true
    }

    ///
    /// Returns the number of successful outcomes in the given outcome map `m`.
    ///
    def successful(m: Map[String, TestOutcome]): Int32 =
        Map.count((_, v) -> isSuccess(v), m)

    ///
    /// Returns the number of failed outcomes in the given outcome map `m`.
    ///
    def failures(m: Map[String, TestOutcome]): Int32 =
        Map.count((_, v) -> isFailure(v), m)


    ///
    /// Returns the string `s` wrapped in green escape codes.
    ///
    pub def green(s: String): String  =
        use Console/Color.green;
        green(s)

    ///
    /// Returns the string `s` wrapped in yellow escape codes.
    ///
    pub def yellow(s: String): String  =
        use Console/Color.yellow;
        yellow(s)

    ///
    /// Returns the string `s` wrapped in red escape codes.
    ///
    pub def red(s: String): String  =
        use Console/Color.red;
        red(s)

    ///
    /// Returns the string `s` wrapped in gray escape codes.
    ///
    pub def gray(s: String): String =
        use Console.rgb;
        rgb((100, 100, 100), s)

}

/*
 *  Copyright 2021 Magnus Madsen
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions not
 *  limitations under the License.
 */

pub def <|(f: a -> b & e, x: a): b & e = f(x)

namespace TestList {

    use Test.{TestSuite};
    use Test.defSuite;
    use Test.defineTest;

    pub def tests(): TestSuite = tests1() `Monoid.combine` tests2() `Monoid.combine` tests3()

    pub def tests1(): TestSuite = defSuite("List") <| Map#{
        "head01" -> defineTest({
            actual = lazy List.head(1 :: Nil),
            expect = lazy Some(1)
        }),

        "head02" -> defineTest({
            actual = lazy List.head(1 :: 2 :: Nil),
            expect = lazy Some(21)
        }),

        "head03" -> defineTest({
            actual = lazy List.head(1 :: Nil),
            expect = lazy Some(1)
        }),

        "head04" -> defineTest({
            actual = lazy List.head(1 :: 2 :: 3 :: Nil),
            expect = lazy Some(1)
        })
    }

    pub def tests2(): TestSuite = defSuite("List") <| Map#{
        "isEmpty01" -> defineTest({
            actual = lazy (List.range(1, 100_000) |> List.isEmpty),
            expect = lazy false
        })
    }

    pub def tests3(): TestSuite = defSuite("Option") <| Map#{
        "isEmpty01" -> defineTest({
            actual = lazy Option.isEmpty(None),
            expect = lazy true
        }),

        "isEmpty02" -> defineTest({
            actual = lazy Some(123),
            expect = lazy None
        })
    }

}

def main(_args: Array[String]): Int32 & Impure =
    Test.runSuite(TestList.tests())



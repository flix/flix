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
    pub enum Suite {
        case Suite(String, Map[String, Test])
    }


    ///
    /// TODO: DOC
    ///
    pub opaque type TestPath = List[String]

    pub enum MySuite {
        // TODO: Use TestPath
        case MySuite(Map[List[String], Map[String, List[Test]]])
    }

    instance Monoid[MySuite] {
        def empty(): MySuite = MySuite(Map#{})

        def combine(x: MySuite, y: MySuite): MySuite =
            let MySuite(s1) = x;
            let MySuite(s2) = y;
            MySuite(Monoid.combine(s1, s2))
    }

    ///
    /// An algebraic data type that represents different types of tests.
    ///
    pub enum Test {
        case EqTest(Unit ~> TestOutcome)
    }

    ///
    /// Returns a suite of test cases with the given `name` and test case list.
    ///
    pub def defineSuite(fqn: String, t: Map[String, Test]): MySuite =
        let path = (String.split(fqn, "\.") |> Array.toList) as & Pure;
        let tests = Map.map(v -> v :: Nil, t);
        MySuite(Map.empty() |> Map.insert(path, tests))

    ///
    /// Returns an executable test case.
    ///
    /// A test consists of an actual value and an expected value.
    ///
    pub def defineTest[a: Eq : ToString](r: {actual: Lazy[a], expect: Lazy[a]}): Test =
        let runner = () -> {
                let expect = force r.expect;
                let (actual, time) = time(() -> force r.actual);
                if (expect == actual)
                    Success({time = time})
                else
                    Failure({
                        expected    = toString(expect),
                        got         = toString(actual),
                        diff        = "no diff"
                    })
        };
        EqTest(runner)


    pub def runAll2(s: MySuite, cb: Callbacks): Int32 & Impure = {
        use Console/Color.blue;
        use Console/Color.red;
        use Console/Color.green;
        use Console/Color.yellow;
        use Console.rgb;
        let gray = rgb((100, 100, 100));
        let MySuite(m) = s;
        println("------------------------ Running Tests ---------------------- ");
        let g = (path: List[String], innerM: Map[String, List[Test]]) -> {
            let suiteName2: String = gray(List.foldLeft((acc: String, elm: String) -> acc + "." + elm, "", path));
            let numberOfTests = Map.size(innerM: Map[String, List[Test]]) : Int32;
            println("${suiteName2}: Number of tests: ${numberOfTests}");
            let evalTest = (testName: String, testList: List[Test]) -> {
                match testList {
                    case t :: Nil =>
                        match runTest(t) {
                            case Success(r) => (cb.onTestSuccess)(testName, r.time)
                            case Failure(_) =>
                                let statusPart = "${red("-")}";
                                let namePart = testName: String;
                                let messagePart = "(${red("unexpected value")})";
                                println("  ${statusPart} ${namePart} ${messagePart}")
                        }

                    case _ => ?fail
                }
            };
            Map.foreach(evalTest, innerM)
        };
        println("");
        Map.foreach(g, m);
        0
    }

    pub def green(s: String): String  =
        use Console/Color.green;
        green(s)

    pub def gray(s: String): String =
        use Console.rgb;
        rgb((100, 100, 100), s)

    type alias Callbacks = {
        onTestSuccess: (String, Int32) ~> Unit
    }

    ///
    /// Callback for when a test completes successfully.
    ///
    pub def onTestSuccess(testName: String, testTime: Int32): Unit & Impure =
        let statusPart = "${green("+")}";
        let namePart = testName;
        let timePart = gray("${testTime} ms");
        println("  ${statusPart} ${namePart} ${timePart}")

    ///
    /// Runs all tests in the given suite `s`.
    ///
    pub def runAll(s: Suite): Int32 & Impure = {
        use Console/Color.blue;
        use Console/Color.red;
        use Console/Color.green;
        use Console/Color.yellow;
        use Console.rgb;
        let gray = rgb((100, 100, 100));
        let Suite(suiteName, tests) = s;
        println("------------------------ Running Tests ---------------------- ");
        println("${suiteName} (${Map.size(tests)} tests):");
        let m = Map.map(runTest, tests);
        let g = (n: String, o: TestOutcome) -> match o {
            case Success(r)    =>
                let statusPart = "${green("+")}";
                let namePart = n: String;
                let timePart = gray("${r.time} ms");
                println("  ${statusPart} ${namePart} ${timePart}")
            case Failure(_) =>
                let statusPart = "${red("-")}";
                let namePart = n: String;
                let messagePart = "(${red("unexpected value")})";
                println("  ${statusPart} ${namePart} ${messagePart}")
        };
        println("");
        Map.foreach(g, m);
        println("");
        println("------------------------ Failing Tests ---------------------- ");
        let g2 = (n: String, o: TestOutcome) -> match o {
            case Success(_) => () // NOP
            case Failure(r) =>
                println("  ${red(n)}");
                println("      expected:   ${green(r.expected)}");
                println("      got     :   ${yellow(r.got)}");
                println("")
        };
        println("");
        Map.foreach(g2, m);
        println("");
        println("${suiteName}: Succeeded: ${successful(m)}. Failed: ${failures(m)}.");
        0
    }

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
    /// Represents the outcome of a test.
    ///
    enum TestOutcome {
        case Success({time: Int32}),
        case Failure({expected: String, got: String, diff: String})
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

///
/// A type class for values that can be diffed.
///
class Diff[a] {
    ///
    /// Returns a string representation of the difference between `x` and `y`.
    ///
    pub def diff(x: a, y: a): String
}

instance Diff[Option[a]] with [a : ToString] {
    pub def diff(x: Option[a], y: Option[a]): String = "${x} - ${y}"
}


pub def <|(f: a -> b & e, x: a): b & e = f(x)

namespace TestList {

    use Test.defineSuite;
    use Test.defineTest;

    pub def tests(): Test.MySuite = tests1() `Monoid.combine` tests2() `Monoid.combine` tests3()

    pub def tests1(): Test.MySuite = defineSuite("List") <| Map#{
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

    pub def tests2(): Test.MySuite = defineSuite("List") <| Map#{
        "isEmpty01" -> defineTest({
            actual = lazy (List.range(1, 100_000) |> List.isEmpty),
            expect = lazy false
        })
    }

    pub def tests3(): Test.MySuite = defineSuite("Option") <| Map#{
        "isEmpty01" -> defineTest({
            actual = lazy Option.isEmpty(None),
            expect = lazy true
        })
    }

}

def main(_args: Array[String]): Int32 & Impure =
    Test.runAll2(TestList.tests(), {
        onTestSuccess = Test.onTestSuccess
     })



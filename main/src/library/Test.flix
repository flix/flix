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
    /// An algebraic data type that represents different types of tests.
    ///
    pub enum Test {
        case EqTest(Unit ~> TestOutcome)
    }

    ///
    /// Returns a suite of test cases with the given `name` and test case list.
    ///
    pub def defineSuite(name: String, tests: Map[String, Test]): Suite = Suite(name, tests)

    ///
    /// Returns an executable test case.
    ///
    /// A test consists of an actual value and an expected value.
    ///
    pub def defineTest[a: Eq : Diff : ToString](r: {actual: Lazy[a], expect: Lazy[a]}): Test =
        let runner = () -> {
                let expect = force r.expect;
                let (actual, time) = time(() -> force r.actual);
                if (expect == actual)
                    Success({time = time})
                else
                    Failure({
                        expected    = toString(expect),
                        got         = toString(actual),
                        diff        = Diff.diff(actual, expect)
                    })
        };
        EqTest(runner)

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
                let timePart = gray("${r.time} ms");
                println("  ${statusPart} ${n: String} ${timePart}")
            case Failure(r) =>
                let statusPart = "${red("-")}";
                println("  ${statusPart} ${n: String} (expected: ${green(r.expected)}, but got: ${yellow(r.got)})")
        };
        Map.foreach(g, m);
        println("${suiteName}: Succeeded: ${successful(m)}. Failed: ${failures(m)}.");
        println("------------------------ Failing Tests ---------------------- ");
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
        (r, Int64.clampToInt32(e / 1000_000i64, 0, Int32.maxValue()))
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

    pub def tests(): Test.Suite = defineSuite("TestList") <| Map#{
        "List.head01" -> defineTest({
            actual = lazy List.head(1 :: Nil),
            expect = lazy Some(1)
        }),

        "List.head02" -> defineTest({
            actual = lazy List.head(1 :: 2 :: Nil),
            expect = lazy Some(21)
        }),

        "List.head03" -> defineTest({
            actual = lazy List.head(1 :: Nil),
            expect = lazy Some(1)
        }),

        "List.head04" -> defineTest({
            actual = lazy List.head(1 :: 2 :: 3 :: Nil),
            expect = lazy Some(1)
        })
    }

}

def main(_args: Array[String]): Int32 & Impure =
    Test.runAll(TestList.tests())



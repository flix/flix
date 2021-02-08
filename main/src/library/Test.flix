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
        case EqTest(Unit -> TestOutcome)
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
    /// The type constraints ensure that:
    /// - the actual and expected value can be compared for equality.
    /// - the actual and expected value can be diffed.
    ///
    pub def defineTest[a: Eq : Diff : ToString](r: {actual: Lazy[a], expect: Lazy[a]}): Test =
        EqTest(
            (_: Unit) ->
                let actual = force r.actual;
                let expect = force r.expect;
                if (actual == expect)
                    Success
                else
                    Failure({
                        expected = ToString.toString(expect),
                        got = ToString.toString(actual),
                        diff = Diff.diff(actual, expect)})
        )

    ///
    /// Runs all tests in the given suite `s`.
    ///
    pub def runAll(s: Suite): Int32 & Impure = {
        let Suite(suiteName, tests) = s;
        println("${suiteName} (${Map.size(tests)} tests):");
        let f = (t: Test) -> match t {
            case EqTest(test) => test()
        };
        let m = Map.map(f, tests);
        let g = (n: String, o: TestOutcome) -> match o {
            case Success    => println("  ${n: String}: Success.")
            case Failure(r) => println("  ${n: String}: FAILURE: expected: ${r.expected}, but got: ${r.got}")
        };
        Map.foreach(g, m);
        println("Succeeded: 0");
        0
    }

    ///
    /// Represents the outcome of a test:
    ///
    enum TestOutcome {
        case Success,
        case Failure({expected: String, got: String, diff: String})
    }

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

// TODO: Add <| ?

namespace TestList {

    use Test.defineSuite;
    use Test.defineTest;

    pub def tests(): Test.Suite = defineSuite("TestList", Map#{
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
    })

}

def main(_args: Array[String]): Int32 & Impure =
    Test.runAll(TestList.tests())

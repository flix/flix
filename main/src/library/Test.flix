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
        case EqTest(Unit -> Result[Unit, String])
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
    pub def defineTest[a: Eq : Diff](r: {actual: Lazy[a], expect: Lazy[a]}): Test =
        EqTest(
            (_: Unit) ->
                let actual = force r.actual;
                let expect = force r.expect;
                if (actual == expect)
                    Ok(())
                else
                    Err(Diff.diff(actual, expect))
        )

    ///
    /// Runs all tests in the given suite `s`.
    ///
    pub def runAll(s: Suite): Int32 & Impure = {
        let Suite(suiteName, tests) = s;
        println("${suiteName} (${Map.size(tests)} tests):");
        let f = (n: String, t: Test) -> match t {
            case EqTest(test) => match test() {
                case Ok(_) =>  println("  OK ${n: String}")
                case Err(_) => println("  ERROR: ${n: String}")
            }
        };
        Map.foreach(f, tests);
        0
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


namespace Blargh {

    pub enum Test {
        case Test
    }

    pub enum Suite {
        case Suite(List[Test])
        case ParallelSuite(List[Test])
    }

    class Diffable[a] {
        def diff(x: a, y: a): String
    }



    pub def defineTest[a : Eq, r :# Record](test: {name: String, expected: Lazy[a], actual: Lazy[a] | r}): Test = ???

    pub def defineTestWithOps[a : Eq, r :# Record, r2 :# Record](test: {name: String, expected: Lazy[a], actual: Lazy[a] | r}, opts: {timeout: Option[Int] | r2}): Test = ???

    pub def disableTest(r: {name: String, expected: Lazy[a], actual: Lazy[a]}): Test = ???

    // TODO: parallel

    pub def defaultTest(): {} = {}

    class DefTest[r :# Record] {
        pub def defTest(): { retry: Int | r }
    }

    pub def defaultOpts(): { timeout: Option[Int] } = { timeout = None }

    // TODO: Use a map?
    pub /* const */ def listSuite(): Suite = Suite(
        defineTest({
            +name        = "List.head01",
            +actual      = lazy List.head(1 :: Nil),
            +expected    = lazy Some(1)
            | defaultTest()
        })

        ::

        disableTest({
            name        = "List.head02",
            actual      = lazy List.head(1 :: 2 :: Nil),
            expected    = lazy Some(1)
        })

        ::

        defineTest({
            name        = "List.head03",
            actual      = lazy List.head(1 :: Nil),
            expected    = lazy Some(1),
            retry       = 3
        })

        ::

        defineTestWithOps({
            name        = "List.head04",
            actual      = lazy List.head(1 :: 2 :: 3 :: Nil),
            expected    = lazy Some(1)
        })({timeout = Some(123)})

        ::

        Nil
    )


}

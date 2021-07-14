/*
 * Copyright 2021 Jakob Schneider Villumsen
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

namespace TestMutDeque {

    /////////////////////////////////////////////////////////////////////////////
    // new                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def new01(): Bool & Impure =
        let MutDeque(a, f, b) = MutDeque.new();
        Array.length(deref a) == 8 and
        deref f == 0 and
        deref b == 0


    /////////////////////////////////////////////////////////////////////////////
    // popFront                                                                //
    /////////////////////////////////////////////////////////////////////////////
/*
    def testpopFront_withEmptyDeque_expectFailure(): Int32 & Impure = {
        let mdq = MutDeque.new();

        impureAssert(() ->
            MutDeque.popFront(mdq) == None
            and MutDeque.popFront(mdq) == None
            and MutDeque.popFront(mdq) == None
            and MutDeque.popFront(mdq) == None
        )
    }


    def testMutDequepopFront_withNonEmptyDeque_expectSuccess(): Int32 & Impure = {
        let mdq = MutDeque(ref [1, 2, 3, -1], ref 0, ref 3);

        impureAssert(() ->
            let MutDeque(_, front, back) = mdq;
            deref front == 0
            and deref back == 3
            and MutDeque.popFront(mdq) == Some(1)
            and deref front == 1
            and deref back == 3
        )
    }

    def testMutDequepopFront_withNonEmptyDequeUntilEmpty_expectFailure(): Int32 & Impure = {
        let mq = MutDeque(ref [1, 2, 3, -1], ref 0, ref 3);
        let MutDeque(_, front, back) = mq;

        impureAssert(() ->
            deref front == 0
            and MutDeque.popFront(mq) == Some(1)
            and deref front == 1
            and MutDeque.popFront(mq) == Some(2)
            and deref front == 2
            and MutDeque.popFront(mq) == Some(3)
            and deref front == 3
            and deref back == 3
            and MutDeque.popFront(mq) == None
        )
    }


    def testpushBack_withEmptyDeque_expectSuccess(): Int32 & Impure = {
        impureAssert(() ->
            let mdq = MutDeque.new();
            let MutDeque(arr, front, back) = mdq;

            match
            deref front == 0
            and deref back == 0
            and Array.length(deref arr) == 0 {

                case true =>
                    MutDeque.pushBack("a", mdq);

                    deref front == 0
                    and deref back == 1
                    and Array.length(deref arr) == 2
                    and MutDeque.popFront(mdq) == Some("a")
                    and MutDeque.popFront(mdq) == None

                case _ => false
            }
        )
    }


    def testpushBack_withFullDeque_expectSuccess(): Int32 & Impure = {
        impureAssert(() ->
            let mdq = MutDeque.new();
            let MutDeque(arr, front, back) = mdq;
            MutDeque.pushBack("a", mdq);
            MutDeque.pushBack("b", mdq);

            match
            Array.length(deref arr) == 4
            and deref front == 0
            and deref back == 2 {

                case true =>
                    MutDeque.pushBack("c", mdq);
                    MutDeque.pushBack("d", mdq);

                    match
                    Array.length(deref arr) == 8
                    and deref front == 0
                    and deref back == 4 {

                        case true =>
                        MutDeque.popFront(mdq) == Some("a")
                        and MutDeque.popFront(mdq) == Some("b")
                        and MutDeque.popFront(mdq) == Some("c")
                        and MutDeque.popFront(mdq) == Some("d")

                        case _ => false
                    }

                case _ => false
            }
        )
    }


    def testpushBack_withExpansion_expectSuccess(): Int32 & Impure = {
        impureAssert(() ->
            let mdq = MutDeque.new();
            let MutDeque(arr, front, back) = mdq;

            match
            Array.length(deref arr) == 0 {
                case true =>
                    MutDeque.pushBack("a", mdq);

                    match
                    Array.length(deref arr) == 2 {
                        case true =>
                            MutDeque.pushBack("a", mdq);

                            match
                            Array.length(deref arr) == 4 {
                                case true =>
                                    MutDeque.pushBack("a", mdq);
                                    MutDeque.pushBack("a", mdq);
                                    MutDeque.pushBack("a", mdq);
                                    MutDeque.pushBack("a", mdq);

                                    match
                                    Array.length(deref arr) == 8
                                    and deref front == 0
                                    and deref back == 6 {
                                        case true =>
                                            MutDeque.popFront(mdq);
                                            MutDeque.popFront(mdq);
                                            MutDeque.popFront(mdq);
                                            MutDeque.popFront(mdq);
                                            MutDeque.popFront(mdq);
                                            MutDeque.popFront(mdq);

                                            match
                                            MutDeque.popFront(mdq) == None
                                            and Array.length(deref arr) == 8
                                            and deref front == 6 {
                                                case true =>
                                                    MutDeque.pushBack("a", mdq);

                                                    Array.length(deref arr) == 8
                                                    and MutDeque.count(x -> x == "a", mdq) == 1

                                                case _ => false
                                            }

                                        case _ => false
                                    }


                                case _ => false
                            }

                        case _ => false
                    }

                case _ => false
            }
        )
    }


    def testpopBack_withEmptyDeque_expectFailure(): Int32 & Impure = {
        impureAssert(() ->
            MutDeque.popBack(MutDeque.new()) == None
            and MutDeque.popBack(MutDeque.new()) == None
            and MutDeque.popBack(MutDeque.new()) == None
            and MutDeque.popBack(MutDeque.new()) == None
        )
    }


    def testpopBack_withNoneEmptyDeque_expectSuccess(): Int32 & Impure = {
        impureAssert(() ->
            let mdq = MutDeque.new();
            let MutDeque(_, front, back) = mdq;
            MutDeque.pushBack("a", mdq);
            MutDeque.pushBack("b", mdq);
            MutDeque.pushBack("c", mdq);

            deref front == 0
            and deref back == 3
            and MutDeque.popBack(mdq) == Some("c")
            and deref back == 2
            and MutDeque.popBack(mdq) == Some("b")
            and deref back == 1
            and MutDeque.popBack(mdq) == Some("a")
            and deref back == 0
            and MutDeque.isEmpty(mdq)
        )
    }


    def testpushFront_withEmptyDeque_expectSuccess(): Int32 & Impure = {
        impureAssert(() ->
            let mdq = MutDeque.new();
            let MutDeque(arr, front, back) = mdq;

            match
            deref front == 0
            and deref back == 0
            and Array.length(deref arr) == 0 {

                case true =>
                    MutDeque.pushFront("a", mdq);

                    match
                    deref front == 1
                    and deref back == 0
                    and Array.length(deref arr) == 2
                    and MutDeque.popFront(mdq) == Some("a")
                    and deref front == 0
                    and MutDeque.popFront(mdq) == None {

                        case true =>
                            MutDeque.pushFront("b", mdq);

                            deref front == 1
                            and deref back == 0
                            and Array.length(deref arr) == 2
                            and MutDeque.popBack(mdq) == Some("b")
                            and deref front == 1
                            and deref back == 1
                            and MutDeque.popBack(mdq) == None
                            and deref back == 1

                        case _ => false
                    }

                case _ => false
            }
        )
    }


    def testpushFront_withFullDeque_expectSuccess(): Int32 & Impure = {
        impureAssert(() ->
            let mdq = MutDeque.new();
            let MutDeque(arr, front, back) = mdq;
            MutDeque.pushFront("a", mdq);
            MutDeque.pushFront("b", mdq);

            match
            Array.length(deref arr) == 4
            and deref front == 3
            and deref back == 1 {

                case true =>
                    MutDeque.pushFront("c", mdq);
                    MutDeque.pushFront("d", mdq);

                    match
                    Array.length(deref arr) == 8
                    and deref front == 7
                    and deref back == 3 {

                        case true =>
                        MutDeque.popFront(mdq) == Some("d")
                        and MutDeque.popFront(mdq) == Some("c")
                        and MutDeque.popFront(mdq) == Some("b")
                        and MutDeque.popFront(mdq) == Some("a")
                        and MutDeque.isEmpty(mdq)
                        and MutDeque.popFront(mdq) == None

                        case _ => false
                    }

                case _ => false
            }
        )
    }

    def testcount_withEmptyDeque_expectSuccess(): Int32 & Impure = {
        impureAssert(() ->
            let mdq = MutDeque.new();
            MutDeque.count(i -> i == 1, mdq) == 0
        )
    }

    def testcount_withAlwaysTrueCondition_expectSuccess(): Int32 & Impure = {
        impureAssert(() ->
            let mdq = MutDeque.new();
            MutDeque.pushBack("b", mdq);
            MutDeque.pushBack("b", mdq);
            MutDeque.pushBack("b", mdq);
            MutDeque.count(_ -> true, mdq) == 3
        )
    }

    def testcount_withAlwaysFalseCondition_expectSuccess(): Int32 & Impure = {
        impureAssert(() ->
            let mdq = MutDeque.new();
            MutDeque.pushBack("b", mdq);
            MutDeque.pushBack("b", mdq);
            MutDeque.pushBack("b", mdq);
            MutDeque.count(_ -> false, mdq) == 0
        )
    }

    def testcount_withNumberOfALetters_expectSuccess(): Int32 & Impure = {
        impureAssert(() ->
            let mdq = MutDeque.new();
            MutDeque.pushBack("a", mdq);
            MutDeque.pushBack("b", mdq);
            MutDeque.pushBack("a", mdq);
            MutDeque.count(x -> x == "a", mdq) == 2
            and MutDeque.count(x -> x == "b", mdq) == 1
        )
    }

    def testcount_withNumberOfAsAndPushAndPop_expectSuccess(): Int32 & Impure = {
        impureAssert(() ->
            let mdq = MutDeque.new();
            MutDeque.pushBack("a", mdq);
            MutDeque.pushBack("b", mdq);
            MutDeque.pushBack("a", mdq);

            match
            MutDeque.count(x -> x == "a", mdq) == 2
            and MutDeque.count(x -> x == "b", mdq) == 1 {

                case true =>
                    MutDeque.popFront(mdq);
                    MutDeque.count(x -> x == "a", mdq) == 1
                    and MutDeque.count(x -> x == "b", mdq) == 1

                case _ => false
            }
        )
    }

    def testAllOperations_expectSuccess(): Int32 & Impure = {
        impureAssert(() ->
            let mdq = MutDeque.new();
            let MutDeque(_arr, _front, _back) = mdq;
            MutDeque.pushFront("a", mdq);
            MutDeque.pushBack("b", mdq);
            MutDeque.pushFront("c", mdq);

            MutDeque.count(x -> x == "b", mdq) == 1
            and MutDeque.popBack(mdq) == Some("b")
            and MutDeque.count(x -> x == "b", mdq) == 0
            and MutDeque.popBack(mdq) == Some("a")
            and MutDeque.popFront(mdq) == Some("c")
            and MutDeque.isEmpty(mdq)
        )
    }
    */
}

/*
namespace TestUtil {
    pub def impureAssert(x: Unit -> Bool & Impure): Int32 & Impure = {
        if (x()) 0 else 1
    }

    pub def pureAssert(x: Unit -> Bool): Int32 = {
        if (x()) 0 else 1
    }

    pub def collectFailures(results: List[Int32]): List[(Int32, Int32)] = {
        results
        |> List.mapWithIndex((res, idx) -> (idx + 1, res))
        |> List.filter(x -> snd(x) == 1)
    }

    pub def printErrors(name: String, results: List[Int32]): Unit & Impure = {
        collectFailures(results)
        |> List.foreach(x -> println("TEST FAILED: ${name}, test ${Int32.toString(fst(x))}"))
    }
}
*/
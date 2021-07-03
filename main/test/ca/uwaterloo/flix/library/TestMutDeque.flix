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

use List.{filter, head, foreach};
use Array.{length => lengthOfArray};
use Console.{printLine => println};
use TestUtil.{impureAssert, printErrors};
use MutDeque.{popFront, popBack, pushFront, pushBack, count, isEmpty};


namespace TestMutDeque {
    pub def testAll(): Int32 & Impure = {

        let testResults: List[Int32] =
            testInitialParameters_expectSuccess() ::
            testPopFront_withEmptyDeque_expectFailure() ::
            testPopFront_withNonEmptyDeque_expectSuccess() ::
            testPopFront_withNonEmptyDequeUntilEmpty_expectFailure() ::
            testPushBack_withEmptyDeque_expectSuccess() ::
            testPushBack_withFullDeque_expectSuccess() ::
            testPushBack_withExpansion_expectSuccess() ::
            testPopBack_withEmptyDeque_expectFailure() ::
            testPopBack_withNoneEmptyDeque_expectSuccess() ::
            testPushFront_withEmptyDeque_expectSuccess() ::
            testPushFront_withFullDeque_expectSuccess() ::
            testCount_withAlwaysTrueCondition_expectSuccess() ::
            testCount_withNumberOfALetters_expectSuccess() ::
            testCount_withEmptyDeque_expectSuccess() ::
            testCount_withAlwaysFalseCondition_expectSuccess() ::
            testCount_withNumberOfAsAndPushAndPop_expectSuccess() ::
            testAllOperations_expectSuccess() ::
            Nil;

        printErrors("TestMutDeque")(testResults);

        match testResults |> filter(x -> x == 1) |> head {
            case Some(_) =>
                println("Tests exited with errors.");
                1
            case _       =>
                println("All tests passed.");
                0
        }
    }


    def testInitialParameters_expectSuccess(): Int32 & Impure = {
        let MutDeque(a, s, e) = MutDeque.new();

        impureAssert(() ->
            lengthOfArray(deref a) == 0
            && deref s == 0
            && deref e == 0
        )
    }


    def testPopFront_withEmptyDeque_expectFailure(): Int32 & Impure = {
        let mdq = MutDeque.new();

        impureAssert(() ->
            popFront(mdq) == None
            && popFront(mdq) == None
            && popFront(mdq) == None
            && popFront(mdq) == None
        )
    }


    def testPopFront_withNonEmptyDeque_expectSuccess(): Int32 & Impure = {
        let mdq = MutDeque(ref [1, 2, 3, -1], ref 0, ref 3);

        impureAssert(() ->
            let MutDeque(_, front, back) = mdq;
            deref front == 0
            && deref back == 3
            && popFront(mdq) == Some(1)
            && deref front == 1
            && deref back == 3
        )
    }

    def testPopFront_withNonEmptyDequeUntilEmpty_expectFailure(): Int32 & Impure = {
        let mq = MutDeque(ref [1, 2, 3, -1], ref 0, ref 3);
        let MutDeque(_, front, back) = mq;

        impureAssert(() ->
            deref front == 0
            && popFront(mq) == Some(1)
            && deref front == 1
            && popFront(mq) == Some(2)
            && deref front == 2
            && popFront(mq) == Some(3)
            && deref front == 3
            && deref back == 3
            && popFront(mq) == None
        )
    }


    def testPushBack_withEmptyDeque_expectSuccess(): Int32 & Impure = {
        impureAssert(() ->
            let mdq = MutDeque.new();
            let MutDeque(arr, front, back) = mdq;

            match
            deref front == 0
            && deref back == 0
            && Array.length(deref arr) == 0 {

                case true =>
                    pushBack("a", mdq);

                    deref front == 0
                    && deref back == 1
                    && Array.length(deref arr) == 2
                    && popFront(mdq) == Some("a")
                    && popFront(mdq) == None

                case _ => false
            }
        )
    }


    def testPushBack_withFullDeque_expectSuccess(): Int32 & Impure = {
        impureAssert(() ->
            let mdq = MutDeque.new();
            let MutDeque(arr, front, back) = mdq;
            pushBack("a", mdq);
            pushBack("b", mdq);

            match
            Array.length(deref arr) == 4
            && deref front == 0
            && deref back == 2 {

                case true =>
                    pushBack("c", mdq);
                    pushBack("d", mdq);

                    match
                    Array.length(deref arr) == 8
                    && deref front == 0
                    && deref back == 4 {

                        case true =>
                        popFront(mdq) == Some("a")
                        && popFront(mdq) == Some("b")
                        && popFront(mdq) == Some("c")
                        && popFront(mdq) == Some("d")

                        case _ => false
                    }

                case _ => false
            }
        )
    }


    def testPushBack_withExpansion_expectSuccess(): Int32 & Impure = {
        impureAssert(() ->
            let mdq = MutDeque.new();
            let MutDeque(arr, front, back) = mdq;

            match
            lengthOfArray(deref arr) == 0 {
                case true =>
                    pushBack("a", mdq);

                    match
                    lengthOfArray(deref arr) == 2 {
                        case true =>
                            pushBack("a", mdq);

                            match
                            lengthOfArray(deref arr) == 4 {
                                case true =>
                                    pushBack("a", mdq);
                                    pushBack("a", mdq);
                                    pushBack("a", mdq);
                                    pushBack("a", mdq);

                                    match
                                    lengthOfArray(deref arr) == 8
                                    && deref front == 0
                                    && deref back == 6 {
                                        case true =>
                                            popFront(mdq);
                                            popFront(mdq);
                                            popFront(mdq);
                                            popFront(mdq);
                                            popFront(mdq);
                                            popFront(mdq);

                                            match
                                            popFront(mdq) == None
                                            && lengthOfArray(deref arr) == 8
                                            && deref front == 6 {
                                                case true =>
                                                    pushBack("a", mdq);

                                                    lengthOfArray(deref arr) == 8
                                                    && count(x -> x == "a", mdq) == 1

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


    def testPopBack_withEmptyDeque_expectFailure(): Int32 & Impure = {
        impureAssert(() ->
            popBack(MutDeque.new()) == None
            && popBack(MutDeque.new()) == None
            && popBack(MutDeque.new()) == None
            && popBack(MutDeque.new()) == None
        )
    }


    def testPopBack_withNoneEmptyDeque_expectSuccess(): Int32 & Impure = {
        impureAssert(() ->
            let mdq = MutDeque.new();
            let MutDeque(_, front, back) = mdq;
            pushBack("a", mdq);
            pushBack("b", mdq);
            pushBack("c", mdq);

            deref front == 0
            && deref back == 3
            && popBack(mdq) == Some("c")
            && deref back == 2
            && popBack(mdq) == Some("b")
            && deref back == 1
            && popBack(mdq) == Some("a")
            && deref back == 0
            && isEmpty(mdq)
        )
    }


    def testPushFront_withEmptyDeque_expectSuccess(): Int32 & Impure = {
        impureAssert(() ->
            let mdq = MutDeque.new();
            let MutDeque(arr, front, back) = mdq;

            match
            deref front == 0
            && deref back == 0
            && Array.length(deref arr) == 0 {

                case true =>
                    pushFront("a", mdq);

                    match
                    deref front == 1
                    && deref back == 0
                    && Array.length(deref arr) == 2
                    && popFront(mdq) == Some("a")
                    && deref front == 0
                    && popFront(mdq) == None {

                        case true =>
                            pushFront("b", mdq);

                            deref front == 1
                            && deref back == 0
                            && Array.length(deref arr) == 2
                            && popBack(mdq) == Some("b")
                            && deref front == 1
                            && deref back == 1
                            && popBack(mdq) == None
                            && deref back == 1

                        case _ => false
                    }

                case _ => false
            }
        )
    }


    def testPushFront_withFullDeque_expectSuccess(): Int32 & Impure = {
        impureAssert(() ->
            let mdq = MutDeque.new();
            let MutDeque(arr, front, back) = mdq;
            pushFront("a", mdq);
            pushFront("b", mdq);

            match
            Array.length(deref arr) == 4
            && deref front == 3
            && deref back == 1 {

                case true =>
                    pushFront("c", mdq);
                    pushFront("d", mdq);

                    match
                    Array.length(deref arr) == 8
                    && deref front == 7
                    && deref back == 3 {

                        case true =>
                        popFront(mdq) == Some("d")
                        && popFront(mdq) == Some("c")
                        && popFront(mdq) == Some("b")
                        && popFront(mdq) == Some("a")
                        && isEmpty(mdq)
                        && popFront(mdq) == None

                        case _ => false
                    }

                case _ => false
            }
        )
    }

    def testCount_withEmptyDeque_expectSuccess(): Int32 & Impure = {
        impureAssert(() ->
            let mdq = MutDeque.new();
            count(i -> i == 1, mdq) == 0
        )
    }

    def testCount_withAlwaysTrueCondition_expectSuccess(): Int32 & Impure = {
        impureAssert(() ->
            let mdq = MutDeque.new();
            pushBack("b", mdq);
            pushBack("b", mdq);
            pushBack("b", mdq);
            count(_ -> true, mdq) == 3
        )
    }

    def testCount_withAlwaysFalseCondition_expectSuccess(): Int32 & Impure = {
        impureAssert(() ->
            let mdq = MutDeque.new();
            pushBack("b", mdq);
            pushBack("b", mdq);
            pushBack("b", mdq);
            count(_ -> false, mdq) == 0
        )
    }

    def testCount_withNumberOfALetters_expectSuccess(): Int32 & Impure = {
        impureAssert(() ->
            let mdq = MutDeque.new();
            pushBack("a", mdq);
            pushBack("b", mdq);
            pushBack("a", mdq);
            count(x -> x == "a", mdq) == 2
            && count(x -> x == "b", mdq) == 1
        )
    }

    def testCount_withNumberOfAsAndPushAndPop_expectSuccess(): Int32 & Impure = {
        impureAssert(() ->
            let mdq = MutDeque.new();
            pushBack("a", mdq);
            pushBack("b", mdq);
            pushBack("a", mdq);

            match
            count(x -> x == "a", mdq) == 2
            && count(x -> x == "b", mdq) == 1 {

                case true =>
                    popFront(mdq);
                    count(x -> x == "a", mdq) == 1
                    && count(x -> x == "b", mdq) == 1

                case _ => false
            }
        )
    }

    def testAllOperations_expectSuccess(): Int32 & Impure = {
        impureAssert(() ->
            let mdq = MutDeque.new();
            let MutDeque(_arr, _front, _back) = mdq;
            pushFront("a", mdq);
            pushBack("b", mdq);
            pushFront("c", mdq);

            count(x -> x == "b", mdq) == 1
            && popBack(mdq) == Some("b")
            && count(x -> x == "b", mdq) == 0
            && popBack(mdq) == Some("a")
            && popFront(mdq) == Some("c")
            && isEmpty(mdq)
        )
    }
}

namespace TestUtil {
    pub def impureAssert(x: Unit -> Bool & Impure): Int32 & Impure = {
        if (x()) 0 else 1
    }

    pub def pureAssert(x: Unit -> Bool): Int32 = {
        if (x()) 0 else 1
    }

    pub def collectFailures(results: List[Int32]): List[(Int32, Int32)] = {
        results
        |> mapWithIndex((res, idx) -> (idx + 1, res))
        |> filter(x -> snd(x) == 1)
    }

    pub def printErrors(name: String, results: List[Int32]): Unit & Impure = {
        collectFailures(results)
        |> foreach(x -> println("TEST FAILED: ${name}, test ${Int32.toString(fst(x))}"))
    }
}
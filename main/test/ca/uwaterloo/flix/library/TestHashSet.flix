/*
 * Copyright 2017 Liam Palmer
 * Copyright 2024 Jonathan Lindegaard Starup
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

mod TestSet {

use ToString.toString
use Hash.hash

/////////////////////////////////////////////////////////////////////////////
// Collectable.collect                                                     //
/////////////////////////////////////////////////////////////////////////////
@test
def collect01(): Bool = region rc {
    Collectable.collect((Iterator.empty(rc): Iterator[Int32, rc, rc])) == HashSet.empty()
}

@test
def collect02(): Bool = region rc {
    Collectable.collect(Iterator.singleton(rc, 1)) == HashSet.singleton(1)
}

@test
def collect03(): Bool = region rc {
    Collectable.collect(Iterator.range(rc, 1, 3)) == HashSet.collect(List#{1, 2})
}

/////////////////////////////////////////////////////////////////////////////
// size                                                                    //
/////////////////////////////////////////////////////////////////////////////
@test
def size01(): Bool = HashSet.size(HashSet.empty()) == 0

@test
def size02(): Bool = HashSet.size(HashSet.singleton(1)) == 1

@test
def size03(): Bool = HashSet.size(HashSet.collect(List#{1, 2})) == 2

@test
def size04(): Bool = HashSet.size(HashSet.collect(List#{1, 2, 3})) == 3

@test
def size05(): Bool = HashSet.size(HashSet.collect(List#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 14})) == 13

/////////////////////////////////////////////////////////////////////////////
// empty                                                                   //
/////////////////////////////////////////////////////////////////////////////
@test
def empty01(): Bool = (HashSet.empty(): HashSet[Unit]) == HashSet.empty()

/////////////////////////////////////////////////////////////////////////////
// singleton                                                               //
/////////////////////////////////////////////////////////////////////////////
@test
def singleton01(): Bool = HashSet.singleton(1) == HashSet.singleton(1)

@test
def singleton02(): Bool = HashSet.singleton(2) == HashSet.singleton(2)

@test
def singleton03(): Bool = HashSet.singleton(true) == HashSet.singleton(true)

@test
def singleton04(): Bool = HashSet.singleton(false) == HashSet.singleton(false)

/////////////////////////////////////////////////////////////////////////////
// insert                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def insert01(): Bool = HashSet.insert(1, HashSet.empty()) == HashSet.singleton(1)

@test
def insert02(): Bool = HashSet.insert(1, HashSet.singleton(1)) == HashSet.singleton(1)

@test
def insert03(): Bool = HashSet.insert(1, HashSet.singleton(2)) == HashSet.collect(List#{1, 2})

@test
def insert04(): Bool = HashSet.insert(1, HashSet.collect(List#{2, 3})) == HashSet.collect(List#{1, 2, 3})

@test
def insert05(): Bool = HashSet.insert(1, HashSet.collect(List#{1, 2, 3})) == HashSet.collect(List#{1, 2, 3})

@test
def insert06(): Bool = HashSet.insert(1, HashSet.collect(List#{2, 1, 3})) == HashSet.collect(List#{2, 1, 3})

@test
def insert07(): Bool = HashSet.insert(1, HashSet.collect(List#{2, 1, 3, 7, 8, 9, 10})) == HashSet.collect(List#{2, 1, 3, 7, 8, 9, 10})

@test
def insert08(): Bool = HashSet.insert(11, HashSet.collect(List#{2, 1, 3, 7, 8, 9, 10, 35})) == HashSet.collect(List#{11, 2, 1, 3, 7, 8, 9, 10, 35})

/////////////////////////////////////////////////////////////////////////////
// remove                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def remove01(): Bool = HashSet.remove(1, HashSet.empty()) == HashSet.empty()

@test
def remove02(): Bool = HashSet.remove(1, HashSet.singleton(1)) == HashSet.empty()

@test
def remove03(): Bool = HashSet.remove(1, HashSet.singleton(2)) == HashSet.singleton(2)

@test
def remove04(): Bool = HashSet.remove(2, HashSet.collect(List#{2, 3})) == HashSet.singleton(3)

@test
def remove05(): Bool = HashSet.remove(3, HashSet.collect(List#{2, 3})) == HashSet.singleton(2)

@test
def remove06(): Bool = HashSet.remove(1, HashSet.collect(List#{2, 3})) == HashSet.collect(List#{2, 3})

@test
def remove07(): Bool = HashSet.remove(0, HashSet.collect(List#{1, 2, 3, 4, 5, 6, 7})) == HashSet.collect(List#{1, 2, 3, 4, 5, 6, 7})

@test
def remove08(): Bool = HashSet.remove(1, HashSet.collect(List#{1, 2, 3, 4, 5, 6, 7})) == HashSet.collect(List#{2, 3, 4, 5, 6, 7})

@test
def remove09(): Bool = HashSet.remove(2, HashSet.collect(List#{1, 2, 3, 4, 5, 6, 7})) == HashSet.collect(List#{1, 3, 4, 5, 6, 7})

@test
def remove10(): Bool = HashSet.remove(6, HashSet.collect(List#{1, 2, 3, 4, 5, 6, 7})) == HashSet.collect(List#{1, 2, 3, 4, 5, 7})

@test
def remove11(): Bool = HashSet.remove(7, HashSet.collect(List#{1, 2, 3, 4, 5, 6, 7})) == HashSet.collect(List#{1, 2, 3, 4, 5, 6})

@test
def remove12(): Bool = HashSet.remove(8, HashSet.collect(List#{1, 2, 3, 4, 5, 6, 7})) == HashSet.collect(List#{1, 2, 3, 4, 5, 6, 7})

/////////////////////////////////////////////////////////////////////////////
// isEmpty                                                                 //
/////////////////////////////////////////////////////////////////////////////
@test
def isEmpty01(): Bool = HashSet.isEmpty((HashSet.empty(): HashSet[Unit])) == true

@test
def isEmpty02(): Bool = HashSet.isEmpty(HashSet.singleton(1)) == false

@test
def isEmpty03(): Bool = HashSet.isEmpty(HashSet.collect(List#{1, 2})) == false

@test
def isEmpty04(): Bool = HashSet.isEmpty(HashSet.collect(List#{1, 2, 3})) == false

@test
def isEmpty05(): Bool = HashSet.isEmpty(HashSet.collect(List#{1, 2, 3, 4, 5, 6, 7, 8})) == false

/////////////////////////////////////////////////////////////////////////////
// nonEmpty                                                                //
/////////////////////////////////////////////////////////////////////////////
@test
def nonEmpty01(): Bool = HashSet.nonEmpty((HashSet.empty(): HashSet[Unit])) == false

@test
def nonEmpty02(): Bool = HashSet.nonEmpty(HashSet.singleton(1)) == true

@test
def nonEmpty03(): Bool = HashSet.nonEmpty(HashSet.collect(List#{1, 2})) == true

@test
def nonEmpty04(): Bool = HashSet.nonEmpty(HashSet.collect(List#{1, 2, 3})) == true

@test
def nonEmpty05(): Bool = HashSet.nonEmpty(HashSet.collect(List#{1, 2, 3, 4, 5, 6, 7, 8})) == true

/////////////////////////////////////////////////////////////////////////////
// memberOf                                                                //
/////////////////////////////////////////////////////////////////////////////
@test
def memberOf01(): Bool = HashSet.memberOf(1, HashSet.empty()) == false

@test
def memberOf02(): Bool = HashSet.memberOf(1, HashSet.singleton(2)) == false

@test
def memberOf03(): Bool = HashSet.memberOf(1, HashSet.singleton(1)) == true

@test
def memberOf04(): Bool = HashSet.memberOf(1, HashSet.collect(List#{1, 2})) == true

@test
def memberOf05(): Bool = HashSet.memberOf(2, HashSet.collect(List#{1, 2})) == true

@test
def memberOf06(): Bool = HashSet.memberOf(0, HashSet.collect(List#{1, 2})) == false

@test
def memberOf07(): Bool = HashSet.memberOf(3, HashSet.collect(List#{1, 2})) == false

@test
def memberOf08(): Bool = HashSet.memberOf(0, HashSet.collect(List#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10})) == false

@test
def memberOf09(): Bool = HashSet.memberOf(1, HashSet.collect(List#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10})) == true

@test
def memberOf10(): Bool = HashSet.memberOf(2, HashSet.collect(List#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10})) == true

@test
def memberOf11(): Bool = HashSet.memberOf(10, HashSet.collect(List#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10})) == true

@test
def memberOf12(): Bool = HashSet.memberOf(12, HashSet.collect(List#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10})) == false

/////////////////////////////////////////////////////////////////////////////
// find                                                                    //
/////////////////////////////////////////////////////////////////////////////
@test
def find01(): Bool = HashSet.find(i -> i > 2, HashSet.empty()) == None

@test
def find02(): Bool = HashSet.find(i -> i > 2, HashSet.singleton(1)) == None

@test
def find03(): Bool = HashSet.find(i -> i > 2, HashSet.singleton(3)) == Some(3)

@test
def find04(): Bool = HashSet.find(i -> i > 2, HashSet.collect(List#{2, 1})) == None

@test
def find05(): Bool = HashSet.find(i -> i > 2, HashSet.collect(List#{-6, 6})) == Some(6)

@test
def find06(): Bool = HashSet.find(i -> i > 2, HashSet.collect(List#{6, -6})) == Some(6)

@test
def find07(): Bool = HashSet.find(i -> i > 2, HashSet.collect(List#{7, 6})) == Some(6)

/////////////////////////////////////////////////////////////////////////////
// isSubsetOf                                                              //
/////////////////////////////////////////////////////////////////////////////
@test
def isSubsetOf01(): Bool = HashSet.isSubsetOf((HashSet.empty(): HashSet[Unit]), HashSet.empty()) == true

@test
def isSubsetOf02(): Bool = HashSet.isSubsetOf(HashSet.empty(), HashSet.singleton(1)) == true

@test
def isSubsetOf03(): Bool = HashSet.isSubsetOf(HashSet.singleton(1), HashSet.singleton(1)) == true

@test
def isSubsetOf04(): Bool = HashSet.isSubsetOf(HashSet.singleton(1), HashSet.singleton(2)) == false

@test
def isSubsetOf05(): Bool = HashSet.isSubsetOf(HashSet.empty(), HashSet.collect(List#{1, 2})) == true

@test
def isSubsetOf06(): Bool = HashSet.isSubsetOf(HashSet.singleton(1), HashSet.collect(List#{1, 2})) == true

@test
def isSubsetOf07(): Bool = HashSet.isSubsetOf(HashSet.singleton(2), HashSet.collect(List#{1, 2})) == true

@test
def isSubsetOf08(): Bool = HashSet.isSubsetOf(HashSet.singleton(3), HashSet.collect(List#{1, 2})) == false

@test
def isSubsetOf09(): Bool = HashSet.isSubsetOf(HashSet.collect(List#{1, 2}), HashSet.collect(List#{1, 2})) == true

@test
def isSubsetOf10(): Bool = HashSet.isSubsetOf(HashSet.collect(List#{2, 1}), HashSet.collect(List#{1, 2})) == true

@test
def isSubsetOf11(): Bool = HashSet.isSubsetOf(HashSet.collect(List#{3, 1}), HashSet.collect(List#{1, 2})) == false

@test
def isSubsetOf12(): Bool = HashSet.isSubsetOf(HashSet.collect(List#{1, 2, 3}), HashSet.collect(List#{1, 2})) == false

@test
def isSubsetOf13(): Bool = HashSet.isSubsetOf(HashSet.singleton(10), HashSet.collect(List#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10})) == true

@test
def isSubsetOf14(): Bool = HashSet.isSubsetOf(HashSet.collect(List#{9, 1}), HashSet.collect(List#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10})) == true

@test
def isSubsetOf15(): Bool = HashSet.isSubsetOf(HashSet.collect(List#{6, 5, 8}), HashSet.collect(List#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10})) == true

@test
def isSubsetOf16(): Bool = HashSet.isSubsetOf(HashSet.collect(List#{10, 2, 3, 6, -1, 4}), HashSet.collect(List#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10})) == false

/////////////////////////////////////////////////////////////////////////////
// isProperSubsetOf                                                        //
/////////////////////////////////////////////////////////////////////////////
@test
def isProperSubsetOf01(): Bool = HashSet.isProperSubsetOf((HashSet.empty(): HashSet[Unit]), HashSet.empty()) == false

@test
def isProperSubsetOf02(): Bool = HashSet.isProperSubsetOf(HashSet.empty(), HashSet.singleton(1)) == true

@test
def isProperSubsetOf03(): Bool = HashSet.isProperSubsetOf(HashSet.singleton(1), HashSet.singleton(1)) == false

@test
def isProperSubsetOf04(): Bool = HashSet.isProperSubsetOf(HashSet.singleton(1), HashSet.singleton(2)) == false

@test
def isProperSubsetOf05(): Bool = HashSet.isProperSubsetOf(HashSet.empty(), HashSet.collect(List#{1, 2})) == true

@test
def isProperSubsetOf06(): Bool = HashSet.isProperSubsetOf(HashSet.singleton(1), HashSet.collect(List#{1, 2})) == true

@test
def isProperSubsetOf07(): Bool = HashSet.isProperSubsetOf(HashSet.singleton(2), HashSet.collect(List#{1, 2})) == true

@test
def isProperSubsetOf08(): Bool = HashSet.isProperSubsetOf(HashSet.singleton(3), HashSet.collect(List#{1, 2})) == false

@test
def isProperSubsetOf09(): Bool = HashSet.isProperSubsetOf(HashSet.collect(List#{1, 2}), HashSet.collect(List#{1, 2})) == false

@test
def isProperSubsetOf10(): Bool = HashSet.isProperSubsetOf(HashSet.collect(List#{2, 1}), HashSet.collect(List#{1, 2})) == false

@test
def isProperSubsetOf11(): Bool = HashSet.isProperSubsetOf(HashSet.collect(List#{3, 1}), HashSet.collect(List#{1, 2})) == false

@test
def isProperSubsetOf12(): Bool = HashSet.isProperSubsetOf(HashSet.collect(List#{1, 2, 3}), HashSet.collect(List#{1, 2})) == false

@test
def isProperSubsetOf13(): Bool = HashSet.isProperSubsetOf(HashSet.singleton(10), HashSet.collect(List#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10})) == true

@test
def isProperSubsetOf14(): Bool = HashSet.isProperSubsetOf(HashSet.collect(List#{9, 1}), HashSet.collect(List#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10})) == true

@test
def isProperSubsetOf15(): Bool = HashSet.isProperSubsetOf(HashSet.collect(List#{6, 5, 8}), HashSet.collect(List#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10})) == true

@test
def isProperSubsetOf16(): Bool = HashSet.isProperSubsetOf(HashSet.collect(List#{10, 2, 3, 6, -1, 4}), HashSet.collect(List#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10})) == false

@test
def isProperSubsetOf17(): Bool = HashSet.isProperSubsetOf(HashSet.collect(List#{10, 2, 3, 6, 1, 4, 5, 9, 8, 7}), HashSet.collect(List#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10})) == false

/////////////////////////////////////////////////////////////////////////////
// fold                                                                    //
/////////////////////////////////////////////////////////////////////////////

@test
def fold01(): Bool = HashSet.fold((HashSet.empty(): HashSet[String])) == ""

@test
def fold02(): Bool = HashSet.fold((HashSet.empty(): HashSet[Unit])) == ()

@test
def fold03(): Bool = HashSet.fold(HashSet.collect(List#{"a", "b", "c"})) == "abc"

@test
def fold04(): Bool = HashSet.fold(HashSet.collect(List#{("a", "b"), ("c", "d")})) == ("ac", "bd")

/////////////////////////////////////////////////////////////////////////////
// foldLeft                                                                //
/////////////////////////////////////////////////////////////////////////////
@test
def foldLeft01(): Bool = HashSet.foldLeft((i, e) -> (i - e)*(e `Int32.remainder` 2 + 1), 100, HashSet.empty()) == 100

@test
def foldLeft02(): Bool = HashSet.foldLeft((i, e) -> (i - e)*(e `Int32.remainder` 2 + 1), 100, HashSet.singleton(1)) == 198

@test
def foldLeft03(): Bool = HashSet.foldLeft((i, e) -> (i - e)*(e `Int32.remainder` 2 + 1), 100, HashSet.collect(List#{2, 1})) == 196

@test
def foldLeft04(): Bool = HashSet.foldLeft((i, e) -> (i - e)*(e `Int32.remainder` 2 + 1), 100, HashSet.collect(List#{3, 2, 1})) == 386

/////////////////////////////////////////////////////////////////////////////
// foldRight                                                               //
/////////////////////////////////////////////////////////////////////////////
@test
def foldRight01(): Bool = HashSet.foldRight((e, acc) -> (acc - e) * (e `Int32.remainder` 2 + 1), 100, HashSet.empty()) == 100

@test
def foldRight02(): Bool = HashSet.foldRight((e, acc) -> (acc - e) * (e `Int32.remainder` 2 + 1), 100, HashSet.singleton(1)) == 198

@test
def foldRight03(): Bool = HashSet.foldRight((e, acc) -> (acc - e) * (e `Int32.remainder` 2 + 1), 100, HashSet.collect(List#{2, 1})) == 194

@test
def foldRight04(): Bool = HashSet.foldRight((e, acc) -> (acc - e) * (e `Int32.remainder` 2 + 1), 100, HashSet.collect(List#{3, 2, 1})) == 382

/////////////////////////////////////////////////////////////////////////////
// foldRightWithCont                                                       //
/////////////////////////////////////////////////////////////////////////////
// @test
// def foldRightWithCont01(): Bool = HashSet.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.remainder` 2 + 1), 100, HashSet.empty()) == 100

// @test
// def foldRightWithCont02(): Bool = HashSet.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.remainder` 2 + 1), 100, HashSet.singleton(1)) == 198

// @test
// def foldRightWithCont03(): Bool = HashSet.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.remainder` 2 + 1), 100, HashSet.collect(List#{2, 1})) == 194

// @test
// def foldRightWithCont04(): Bool = HashSet.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.remainder` 2 + 1), 100, HashSet.collect(List#{3, 2, 1})) == 382

/////////////////////////////////////////////////////////////////////////////
// foldMap                                                                 //
/////////////////////////////////////////////////////////////////////////////

@test
def foldMap01(): Bool = HashSet.foldMap(x -> 2 * x, HashSet.empty()) == 0

@test
def foldMap02(): Bool = HashSet.foldMap(x -> 2 * x, HashSet.collect(List#{1, 2})) == 6

@test
def foldMap03(): Bool = HashSet.foldMap(x -> if (x == "a") "b" else x, HashSet.singleton("a")) == "b"

@test
def foldMap04(): Bool = HashSet.foldMap(x -> if (x == "c") "b" else x, HashSet.collect(List#{"a", "b", "c"})) == "abb"

@test
def foldMap05(): Bool = HashSet.foldMap(Int32.toString, HashSet.collect(List#{1, 2, 3})) == "123"

/////////////////////////////////////////////////////////////////////////////
// reduce                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def reduce01(): Bool = HashSet.reduce((a, b) -> a+b, (HashSet.empty(): HashSet[Int32])) == None

@test
def reduce02(): Bool = HashSet.reduce((a, b) -> a+b, HashSet.singleton(1)) == Some(1)

@test
def reduce03(): Bool = HashSet.reduce((a, b) -> a+b, HashSet.collect(List#{2, 1})) == Some(3)

@test
def reduce04(): Bool = HashSet.reduce((a, b) -> a+b, HashSet.collect(List#{3, 2, 1})) == Some(6)

@test
def reduce05(): Bool = HashSet.reduce((a, b) -> a+b, HashSet.collect(List#{4, 3, 2, 1})) == Some(10)

/////////////////////////////////////////////////////////////////////////////
// count                                                                   //
/////////////////////////////////////////////////////////////////////////////
// @test
// def count01(): Bool = HashSet.count(i -> i > 3, HashSet.empty()) == 0

// @test
// def count02(): Bool = HashSet.count(i -> i > 3, HashSet.singleton(1)) == 0

// @test
// def count03(): Bool = HashSet.count(i -> i > 3, HashSet.singleton(4)) == 1

// @test
// def count04(): Bool = HashSet.count(i -> i > 3, HashSet.collect(List#{2, 1})) == 0

// @test
// def count05(): Bool = HashSet.count(i -> i > 3, HashSet.collect(List#{8, 1})) == 1

// @test
// def count06(): Bool = HashSet.count(i -> i > 3, HashSet.collect(List#{1, 8})) == 1

// @test
// def count07(): Bool = HashSet.count(i -> i > 3, HashSet.collect(List#{7, 6})) == 2

// @test
// def count08(): Bool = region rc {
//     let range = List.range(0, 2000);
//     let s = range |> List.toSet;
//     let a = MutList.empty(rc);
//     discard HashSet.count(x -> { let b = MutList.empty(rc); MutList.push!(x, b); MutList.append!(MutList.toList(b), a); true }, s);
//     MutList.toList(a) == range
// }


    /////////////////////////////////////////////////////////////////////////////
    // sum                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    // @test
    // def sum01(): Bool =
    //     HashSet.empty() |> HashSet.sum == 0

    // @test
    // def sum02(): Bool =
    //     HashSet.singleton(1) |> HashSet.sum == 1

    // @test
    // def sum03(): Bool =
    //     HashSet.collect(List#{1, 2, 3}) |> HashSet.sum == 6

    // @test
    // def sum04(): Bool =
    //     HashSet.collect(List#{1, 2, 3, -3}) |> HashSet.sum == 3

    // @test
    // def sum05(): Bool =
    //     HashSet.collect(List#{1, 2, -3, -4}) |> HashSet.sum == -4

    // @test
    // def sum06(): Bool =
    //     HashSet.collect(List#{10, -10}) |> HashSet.sum == 0

    // @test
    // def sum07(): Bool =
    //     HashSet.range(1, 101) |> HashSet.sum == 5050


    /////////////////////////////////////////////////////////////////////////////
    // sumWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    // @test
    // def sumWith01(): Bool =
    //     HashSet.empty() |> HashSet.sumWith(x -> x + 1) == 0

    // @test
    // def sumWith02(): Bool =
    //     HashSet.singleton(1) |> HashSet.sumWith(x -> x + 1) == 2

    // @test
    // def sumWith03(): Bool =
    //     HashSet.collect(List#{1, 2, 3}) |> HashSet.sumWith(x -> x + 1) == 9

    // @test
    // def sumWith04(): Bool =
    //     HashSet.collect(List#{1, 2, 3, -3}) |> HashSet.sumWith(x -> x + 1) == 7

    // @test
    // def sumWith05(): Bool =
    //     HashSet.collect(List#{-1, -2, -3, -4}) |> HashSet.sumWith(x -> x + 1) == -6

    // @test
    // def sumWith06(): Bool =
    //     HashSet.collect(List#{10, -10}) |> HashSet.sumWith(x -> x + 1) == 2

    // @test
    // def sumWith08(): Bool = region rc {
    //     let range = List.range(0, 2000);
    //     let m = range |> List.toSet;
    //     let a = MutList.empty(rc);
    //     discard HashSet.sumWith(k -> { let b = MutList.empty(rc); MutList.push!(k, b); MutList.append!(MutList.toList(b), a); 1 }, m);
    //     MutList.toList(a) == range
    // }


/////////////////////////////////////////////////////////////////////////////
// flatten                                                                 //
/////////////////////////////////////////////////////////////////////////////
@test
def flatten01(): Bool = (HashSet.flatten(HashSet.empty()): HashSet[Unit]) == HashSet.empty()

@test
def flatten02(): Bool = HashSet.flatten((HashSet.singleton(HashSet.empty()): HashSet[HashSet[Unit]])) == HashSet.empty()

@test
def flatten03(): Bool = HashSet.flatten(HashSet.singleton(HashSet.singleton(1))) == HashSet.singleton(1)

@test
def flatten04(): Bool = HashSet.flatten(HashSet.collect(List#{HashSet.collect(List#{1, 2})})) == HashSet.collect(List#{1, 2})

@test
def flatten05(): Bool = (HashSet.flatten(HashSet.collect(List#{HashSet.empty(), HashSet.empty()})): HashSet[Unit]) == HashSet.empty()

@test
def flatten06(): Bool = HashSet.flatten(HashSet.collect(List#{HashSet.singleton(1), HashSet.empty()})) == HashSet.singleton(1)

@test
def flatten07(): Bool = HashSet.flatten(HashSet.collect(List#{HashSet.empty(), HashSet.singleton(1)})) == HashSet.singleton(1)

@test
def flatten08(): Bool = HashSet.flatten(HashSet.collect(List#{HashSet.singleton(1), HashSet.singleton(2)})) == HashSet.collect(List#{1, 2})

@test
def flatten09(): Bool = HashSet.flatten(HashSet.collect(List#{HashSet.singleton(1), HashSet.singleton(1)})) == HashSet.singleton(1)

@test
def flatten10(): Bool = HashSet.flatten(HashSet.collect(List#{HashSet.collect(List#{1, 2}), HashSet.collect(List#{3, 4, 5})})) == HashSet.collect(List#{1, 2, 3, 4, 5})

@test
def flatten11(): Bool = HashSet.flatten(HashSet.collect(List#{HashSet.collect(List#{1, 2}), HashSet.collect(List#{3, 4, 3})})) == HashSet.collect(List#{1, 2, 3, 4})

@test
def flatten12(): Bool = HashSet.flatten(HashSet.collect(List#{HashSet.collect(List#{1, 2}), HashSet.collect(List#{3, 4, 1})})) == HashSet.collect(List#{1, 2, 3, 4})

@test
def flatten13(): Bool = HashSet.flatten(HashSet.collect(List#{HashSet.singleton(1), HashSet.collect(List#{2, 3}), HashSet.singleton(4)})) == HashSet.collect(List#{1, 2, 3, 4})

@test
def flatten14(): Bool = HashSet.flatten(HashSet.collect(List#{HashSet.singleton(1), HashSet.collect(List#{2, 3}), HashSet.singleton(1)})) == HashSet.collect(List#{1, 2, 3})

/////////////////////////////////////////////////////////////////////////////
// exists                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def exists01(): Bool = HashSet.exists(x -> x `Int32.remainder` 8 == 7, HashSet.empty()) == false

@test
def exists02(): Bool = HashSet.exists(x -> x `Int32.remainder` 8 == 7, HashSet.singleton(5)) == false

@test
def exists03(): Bool = HashSet.exists(x -> x `Int32.remainder` 8 == 7, HashSet.singleton(7)) == true

@test
def exists04(): Bool = HashSet.exists(x -> x `Int32.remainder` 8 == 7, HashSet.singleton(15)) == true

@test
def exists05(): Bool = HashSet.exists(x -> x `Int32.remainder` 8 == 7, HashSet.collect(List#{1, 44})) == false

@test
def exists06(): Bool = HashSet.exists(x -> x `Int32.remainder` 8 == 7, HashSet.collect(List#{11, 71})) == true

@test
def exists07(): Bool = HashSet.exists(x -> x `Int32.remainder` 8 == 7, HashSet.collect(List#{71, 12})) == true

@test
def exists08(): Bool = HashSet.exists(x -> x `Int32.remainder` 8 == 7, HashSet.collect(List#{71, 79})) == true

@test
def exists09(): Bool = HashSet.exists(x -> x `Int32.remainder` 8 == 7, HashSet.collect(List#{11, -1, -14, -2, 84, 113})) == false

@test
def exists10(): Bool = HashSet.exists(x -> x `Int32.remainder` 8 == 7, HashSet.collect(List#{11, -1, 31, -14, -2, 84, 111})) == true

@test
def exists11(): Bool = HashSet.exists(x -> x `Int32.remainder` 8 == 7, HashSet.collect(List#{11, -1, -14, -2, 84, 111, 38})) == true

/////////////////////////////////////////////////////////////////////////////
// forAll                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def forAll01(): Bool = HashSet.forAll(x -> x `Int32.remainder` 8 == 7, HashSet.empty()) == true

@test
def forAll02(): Bool = HashSet.forAll(x -> x `Int32.remainder` 8 == 7, HashSet.singleton(5)) == false

@test
def forAll03(): Bool = HashSet.forAll(x -> x `Int32.remainder` 8 == 7, HashSet.singleton(7)) == true

@test
def forAll04(): Bool = HashSet.forAll(x -> x `Int32.remainder` 8 == 7, HashSet.singleton(15)) == true

@test
def forAll05(): Bool = HashSet.forAll(x -> x `Int32.remainder` 8 == 7, HashSet.collect(List#{1, 44})) == false

@test
def forAll06(): Bool = HashSet.forAll(x -> x `Int32.remainder` 8 == 7, HashSet.collect(List#{11, 71})) == false

@test
def forAll07(): Bool = HashSet.forAll(x -> x `Int32.remainder` 8 == 7, HashSet.collect(List#{71, 12})) == false

@test
def forAll08(): Bool = HashSet.forAll(x -> x `Int32.remainder` 8 == 7, HashSet.collect(List#{71, 79})) == true

@test
def forAll09(): Bool = HashSet.forAll(x -> x `Int32.remainder` 8 == 7, HashSet.collect(List#{7, 15, 23, 84, 111})) == false

@test
def forAll10(): Bool = HashSet.forAll(x -> x `Int32.remainder` 8 == 7, HashSet.collect(List#{7, 15, 23, 111, 3})) == false

@test
def forAll11(): Bool = HashSet.forAll(x -> x `Int32.remainder` 8 == 7, HashSet.collect(List#{7, 15, 23, 119, 111})) == true

/////////////////////////////////////////////////////////////////////////////
// union                                                                   //
/////////////////////////////////////////////////////////////////////////////
@test
def union01(): Bool = HashSet.union((HashSet.empty(): HashSet[Unit]), HashSet.empty()) == HashSet.empty()

@test
def union02(): Bool = HashSet.union(HashSet.singleton(1), HashSet.empty()) == HashSet.singleton(1)

@test
def union03(): Bool = HashSet.union(HashSet.empty(), HashSet.singleton(2)) == HashSet.singleton(2)

@test
def union04(): Bool = HashSet.union(HashSet.singleton(1), HashSet.singleton(1)) == HashSet.singleton(1)

@test
def union05(): Bool = HashSet.union(HashSet.singleton(1), HashSet.singleton(-1)) == HashSet.collect(List#{1, -1})

@test
def union06(): Bool = HashSet.union(HashSet.empty(), HashSet.collect(List#{-1, 9})) == HashSet.collect(List#{-1, 9})

@test
def union07(): Bool = HashSet.union(HashSet.singleton(9), HashSet.collect(List#{-1, 9})) == HashSet.collect(List#{-1, 9})

@test
def union08(): Bool = HashSet.union(HashSet.singleton(4), HashSet.collect(List#{-1, 9})) == HashSet.collect(List#{4, -1, 9})

@test
def union09(): Bool = HashSet.union(HashSet.collect(List#{9, -1}), HashSet.collect(List#{-1, 9})) == HashSet.collect(List#{-1, 9})

@test
def union10(): Bool = HashSet.union(HashSet.collect(List#{9, 5}), HashSet.collect(List#{-1, 9})) == HashSet.collect(List#{5, -1, 9})

@test
def union11(): Bool = HashSet.union(HashSet.collect(List#{6, 5}), HashSet.collect(List#{-1, 9})) == HashSet.collect(List#{6, 5, -1, 9})

@test
def union12(): Bool = HashSet.union(HashSet.collect(List#{6, -99}), HashSet.collect(List#{6, 5, -1, 9, 43, 7, 8, -99})) == HashSet.collect(List#{6, 5, -1, 9, 43, 7, 8, -99})

@test
def union13(): Bool = HashSet.union(HashSet.collect(List#{6, -99, -1, 5, 22}), HashSet.collect(List#{6, 5, -1, 9, 43, 7, 8, -99})) == HashSet.collect(List#{22, 6, 5, -1, 9, 43, 7, 8, -99})

@test
def union14(): Bool = HashSet.union(HashSet.collect(List#{-2, -3, -4}), HashSet.collect(List#{6, 5, -1, 9, 43, 7, 8, -99})) == HashSet.collect(List#{-2, -3, -4, 6, 5, -1, 9, 43, 7, 8, -99})

/////////////////////////////////////////////////////////////////////////////
// intersection                                                            //
/////////////////////////////////////////////////////////////////////////////
@test
def intersection01(): Bool = HashSet.intersection((HashSet.empty(): HashSet[Unit]), HashSet.empty()) == HashSet.empty()

@test
def intersection02(): Bool = HashSet.intersection(HashSet.singleton(1), HashSet.empty()) == HashSet.empty()

@test
def intersection03(): Bool = HashSet.intersection(HashSet.empty(), HashSet.singleton(2)) == HashSet.empty()

@test
def intersection04(): Bool = HashSet.intersection(HashSet.singleton(1), HashSet.singleton(2)) == HashSet.empty()

@test
def intersection05(): Bool = HashSet.intersection(HashSet.singleton(1), HashSet.singleton(1)) == HashSet.singleton(1)

@test
def intersection06(): Bool = HashSet.intersection(HashSet.empty(), HashSet.collect(List#{1, 2})) == HashSet.empty()

@test
def intersection07(): Bool = HashSet.intersection(HashSet.collect(List#{1, 2}), HashSet.empty()) == HashSet.empty()

@test
def intersection08(): Bool = HashSet.intersection(HashSet.singleton(2), HashSet.collect(List#{1, 2})) == HashSet.singleton(2)

@test
def intersection09(): Bool = HashSet.intersection(HashSet.singleton(1), HashSet.collect(List#{1, 2})) == HashSet.singleton(1)

@test
def intersection10(): Bool = HashSet.intersection(HashSet.collect(List#{2, 1}), HashSet.collect(List#{1, 2})) == HashSet.collect(List#{2, 1})

@test
def intersection11(): Bool = HashSet.intersection(HashSet.collect(List#{1, 2}), HashSet.collect(List#{1, 2})) == HashSet.collect(List#{1, 2})

@test
def intersection12(): Bool = HashSet.intersection(HashSet.collect(List#{3, 2}), HashSet.collect(List#{1, 2})) == HashSet.singleton(2)

@test
def intersection13(): Bool = HashSet.intersection(HashSet.collect(List#{3, 55}), HashSet.collect(List#{1, 2})) == HashSet.empty()

@test
def intersection14(): Bool = HashSet.intersection(HashSet.collect(List#{3, 55, 11, 87, 22, 34, -87, 23}), HashSet.collect(List#{1, 2, 84, -87, 87, 3, 44})) == HashSet.collect(List#{3, 87, -87})

@test
def intersection15(): Bool = HashSet.intersection(HashSet.collect(List#{3, 55, 11, 87, 22, 34, -87, 23}), HashSet.collect(List#{23, 1, 2, 84, 87, 3})) == HashSet.collect(List#{3, 87, 23})

/////////////////////////////////////////////////////////////////////////////
// difference                                                              //
/////////////////////////////////////////////////////////////////////////////
@test
def difference01(): Bool = HashSet.difference((HashSet.empty(): HashSet[Unit]), HashSet.empty()) == HashSet.empty()

@test
def difference02(): Bool = HashSet.difference(HashSet.empty(), HashSet.singleton(2)) == HashSet.empty()

@test
def difference03(): Bool = HashSet.difference(HashSet.singleton(1), HashSet.empty()) == HashSet.singleton(1)

@test
def difference04(): Bool = HashSet.difference(HashSet.singleton(1), HashSet.singleton(2)) == HashSet.singleton(1)

@test
def difference05(): Bool = HashSet.difference(HashSet.singleton(1), HashSet.singleton(1)) == HashSet.empty()

@test
def difference06(): Bool = HashSet.difference(HashSet.collect(List#{1, 2}), HashSet.empty()) == HashSet.collect(List#{1, 2})

@test
def difference07(): Bool = HashSet.difference(HashSet.collect(List#{1, 2}), HashSet.singleton(1)) == HashSet.singleton(2)

@test
def difference08(): Bool = HashSet.difference(HashSet.collect(List#{1, 2}), HashSet.singleton(2)) == HashSet.singleton(1)

@test
def difference09(): Bool = HashSet.difference(HashSet.collect(List#{1, 2}), HashSet.collect(List#{8, 2, 4})) == HashSet.singleton(1)

@test
def difference10(): Bool = HashSet.difference(HashSet.collect(List#{1, 2}), HashSet.collect(List#{3, 1, 2, 4})) == HashSet.empty()

@test
def difference11(): Bool = HashSet.difference(HashSet.collect(List#{1, 2}), HashSet.collect(List#{3, 11, 21, 4})) == HashSet.collect(List#{1, 2})

@test
def difference12(): Bool = HashSet.difference(HashSet.collect(List#{1, 2, 87, 4, 5, 6, 86, 92, 111, -1}), HashSet.collect(List#{-1, 92, 4, 5, 1, 2, 86})) == HashSet.collect(List#{87, 6, 111})

@test
def difference13(): Bool = HashSet.difference(HashSet.collect(List#{1, 2, 87, 4, 5, 6, 86, 92, 111, -1}), HashSet.collect(List#{-1, 92, 4, 5, 1, 2, 86, 99, 6})) == HashSet.collect(List#{87, 111})

/////////////////////////////////////////////////////////////////////////////
// subsets                                                                 //
/////////////////////////////////////////////////////////////////////////////
@test
def subsets01(): Bool = HashSet.subsets((HashSet.empty(): HashSet[Unit])) == HashSet.singleton(HashSet.empty())

@test
def subsets02(): Bool = HashSet.subsets(HashSet.singleton(1)) == HashSet.collect(List#{HashSet.singleton(1), HashSet.empty()})

@test
def subsets03(): Bool = HashSet.subsets(HashSet.collect(List#{1, 2})) == HashSet.collect(List#{HashSet.collect(List#{1, 2}), HashSet.singleton(1), HashSet.singleton(2), HashSet.empty()})

@test
def subsets04(): Bool = HashSet.subsets(HashSet.collect(List#{1, 2, 3})) == HashSet.collect(List#{HashSet.collect(List#{1, 2, 3}), HashSet.collect(List#{1, 2}), HashSet.collect(List#{1, 3}), HashSet.singleton(1), HashSet.collect(List#{2, 3}), HashSet.singleton(2), HashSet.singleton(3), HashSet.empty()})

/////////////////////////////////////////////////////////////////////////////
// filter                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def filter01(): Bool = HashSet.filter(x -> x `Int32.remainder` 2 == 0, HashSet.empty()) == HashSet.empty()

@test
def filter02(): Bool = HashSet.filter(x -> x `Int32.remainder` 2 == 0, HashSet.singleton(1)) == HashSet.empty()

@test
def filter03(): Bool = HashSet.filter(x -> x `Int32.remainder` 2 == 0, HashSet.singleton(2)) == HashSet.singleton(2)

@test
def filter04(): Bool = HashSet.filter(x -> x `Int32.remainder` 2 == 0, HashSet.collect(List#{1, 3})) == HashSet.empty()

@test
def filter05(): Bool = HashSet.filter(x -> x `Int32.remainder` 2 == 0, HashSet.collect(List#{8, 3})) == HashSet.singleton(8)

@test
def filter06(): Bool = HashSet.filter(x -> x `Int32.remainder` 2 == 0, HashSet.collect(List#{-1, 32})) == HashSet.singleton(32)

@test
def filter07(): Bool = HashSet.filter(x -> x `Int32.remainder` 2 == 0, HashSet.collect(List#{12, 34})) == HashSet.collect(List#{12, 34})

@test
def filter08(): Bool = HashSet.filter(x -> x `Int32.remainder` 2 == 0, HashSet.collect(List#{-33, -1, 12, 1, 34, 88, 7, 77, 31})) == HashSet.collect(List#{12, 34, 88})

@test
def filter09(): Bool = HashSet.filter(x -> x `Int32.remainder` 2 == 0, HashSet.collect(List#{-33, -1, 12, 1, 34, 88, 7, 77, 31, 7, -92, 841})) == HashSet.collect(List#{12, 34, 88, -92})

/////////////////////////////////////////////////////////////////////////////
// map                                                                     //
/////////////////////////////////////////////////////////////////////////////
@test
def map01(): Bool = HashSet.map(x -> x `Int32.remainder` 2 == 0, HashSet.empty()) == HashSet.empty()

@test
def map02(): Bool = HashSet.map(x -> x `Int32.remainder` 2 == 0, HashSet.singleton(1)) == HashSet.singleton(false)

@test
def map03(): Bool = HashSet.map(x -> x `Int32.remainder` 2 == 0, HashSet.singleton(2)) == HashSet.singleton(true)

@test
def map04(): Bool = HashSet.map(x -> x `Int32.remainder` 2 == 0, HashSet.collect(List#{1, -1})) == HashSet.singleton(false)

@test
def map05(): Bool = HashSet.map(x -> x `Int32.remainder` 2 == 0, HashSet.collect(List#{1, -12})) == HashSet.collect(List#{false, true})

@test
def map06(): Bool = HashSet.map(x -> x `Int32.remainder` 2 == 0, HashSet.collect(List#{16, -1})) == HashSet.collect(List#{true, false})

@test
def map07(): Bool = HashSet.map(x -> x `Int32.remainder` 2 == 0, HashSet.collect(List#{12, -12})) == HashSet.singleton(true)

@test
def map08(): Bool = HashSet.map(x -> x `Int32.remainder` 2 == 0, HashSet.collect(List#{12, -12})) == HashSet.singleton(true)

@test
def map09(): Bool = HashSet.map(x -> x `Int32.remainder` 2 == 0, HashSet.collect(List#{12, -12, 1, 14})) == HashSet.collect(List#{false, true})

@test
def map10(): Bool = HashSet.map(x -> x `Int32.remainder` 2 == 0, HashSet.collect(List#{12, -12, 1, 14, 7, 88, -91})) == HashSet.collect(List#{true, false})

@test
def map11(): Bool = HashSet.map(x -> x `Int32.remainder` 2 == 0, HashSet.collect(List#{12, -12, 122, 14})) == HashSet.singleton(true)

@test
def map12(): Bool = HashSet.map(x -> x `Int32.remainder` 2 == 0, HashSet.collect(List#{123, -123, 1223, 141})) == HashSet.singleton(false)

@test
def map13(): Bool = HashSet.map(x -> x `Int32.remainder` 9, HashSet.collect(List#{11, 5, 16, 4})) == HashSet.collect(List#{2, 5, 7, 4})

@test
def map14(): Bool = HashSet.map(x -> x `Int32.remainder` 9, HashSet.collect(List#{0, 5, 1, -9, -8})) == HashSet.collect(List#{5, 1, 0, -8})

@test
def map15(): Bool = HashSet.map(x -> x `Int32.remainder` 9, HashSet.collect(List#{0, 5, 1, 10, 7, 19, 28, 2})) == HashSet.collect(List#{0, 5, 7, 1, 2})

@test
def map16(): Bool = region rc {
    let range = List.range(0, 2000);
    let s = HashSet.collect(range);
    let a = MutList.empty(rc);
    discard HashSet.map(x -> { let b = MutList.empty(rc); MutList.push!(x, b); MutList.append!(MutList.toList(b), a); x }, s);
    MutList.toList(a) == range
}


/////////////////////////////////////////////////////////////////////////////
// flatMap                                                                 //
/////////////////////////////////////////////////////////////////////////////
@test
def flatMap01(): Bool = HashSet.flatMap(x -> if (x `Int32.remainder` 2 == 0) HashSet.empty() else HashSet.singleton(x), HashSet.empty()) == HashSet.empty()

@test
def flatMap02(): Bool = HashSet.flatMap(x -> if (x `Int32.remainder` 2 == 0) HashSet.empty() else HashSet.singleton(x), HashSet.singleton(2)) == HashSet.empty()

@test
def flatMap03(): Bool = HashSet.flatMap(x -> if (x `Int32.remainder` 2 == 0) HashSet.empty() else HashSet.singleton(x), HashSet.singleton(1)) == HashSet.singleton(1)

@test
def flatMap04(): Bool = HashSet.flatMap(x -> if (x `Int32.remainder` 2 == 0) HashSet.empty() else HashSet.singleton(x), HashSet.collect(List#{2, 4})) == HashSet.empty()

@test
def flatMap05(): Bool = HashSet.flatMap(x -> if (x `Int32.remainder` 2 == 0) HashSet.empty() else HashSet.singleton(x), HashSet.collect(List#{2, 13})) == HashSet.singleton(13)

@test
def flatMap06(): Bool = HashSet.flatMap(x -> if (x `Int32.remainder` 2 == 0) HashSet.empty() else HashSet.singleton(x), HashSet.collect(List#{15, -8})) == HashSet.singleton(15)

@test
def flatMap07(): Bool = HashSet.flatMap(x -> if (x `Int32.remainder` 2 == 0) HashSet.empty() else HashSet.singleton(x), HashSet.collect(List#{1, 11})) == HashSet.collect(List#{1, 11})

@test
def flatMap08(): Bool = HashSet.flatMap(x -> HashSet.collect(List#{x, 2*x}), HashSet.collect(List#{1, 4, 8, 2})) == HashSet.collect(List#{1, 8, 16, 2, 4})

@test
def flatMap09(): Bool = HashSet.flatMap(x -> HashSet.collect(List#{x, 3*x}), HashSet.collect(List#{1, 8, 3, 2, 9, -5, -1})) == HashSet.collect(List#{1, 8, 24, 3, 2, 6, 9, 27, -5, -15, -1, -3})

@test
def flatMap10(): Bool = HashSet.flatMap(x -> HashSet.collect(List#{x, 2*x, 3*x}), HashSet.collect(List#{1, 4, 3, 2})) == HashSet.collect(List#{1, 8, 12, 3, 9, 2, 4, 6})

/////////////////////////////////////////////////////////////////////////////
// filterMap                                                               //
/////////////////////////////////////////////////////////////////////////////

@test
def filterMap01(): Bool =
    HashSet.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, HashSet.empty()) == HashSet.empty()

@test
def filterMap02(): Bool =
    HashSet.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, HashSet.singleton(1)) == HashSet.empty()

@test
def filterMap03(): Bool =
    HashSet.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, HashSet.singleton(2)) == HashSet.singleton(1)

@test
def filterMap04(): Bool =
    HashSet.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, HashSet.collect(List#{1, 3})) == HashSet.empty()

@test
def filterMap05(): Bool =
    HashSet.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, HashSet.collect(List#{1, 4})) == HashSet.singleton(2)

@test
def filterMap06(): Bool =
    HashSet.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, HashSet.collect(List#{-1, 6})) == HashSet.singleton(3)

@test
def filterMap07(): Bool =
    HashSet.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, HashSet.collect(List#{6, 8})) == HashSet.collect(List#{3, 4})

@test
def filterMap08(): Bool =
    let s1 = HashSet.collect(List#{0, 1, 2, 3, 4, 5, 10});
    let s2 = HashSet.collect(List#{0, 1, 2, 5});
    HashSet.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, s1) == s2

/////////////////////////////////////////////////////////////////////////////
// replace                                                                 //
/////////////////////////////////////////////////////////////////////////////
@test
def replace01(): Bool = HashSet.replace(src = 3, dst = 4, HashSet.empty()) == HashSet.empty()

@test
def replace02(): Bool = HashSet.replace(src = 3, dst = 4, HashSet.singleton(1)) == HashSet.singleton(1)

@test
def replace03(): Bool = HashSet.replace(src = 3, dst = 4, HashSet.singleton(3)) == HashSet.singleton(4)

@test
def replace04(): Bool = HashSet.replace(src = 3, dst = 4, HashSet.singleton(4)) == HashSet.singleton(4)

@test
def replace05(): Bool = HashSet.replace(src = 3, dst = 4, HashSet.collect(List#{1, 2})) == HashSet.collect(List#{1, 2})

@test
def replace06(): Bool = HashSet.replace(src = 3, dst = 4, HashSet.collect(List#{1, 3})) == HashSet.collect(List#{1, 4})

@test
def replace07(): Bool = HashSet.replace(src = 3, dst = 4, HashSet.collect(List#{3, 2})) == HashSet.collect(List#{4, 2})

@test
def replace08(): Bool = HashSet.replace(src = 3, dst = 4, HashSet.collect(List#{3, 4})) == HashSet.singleton(4)

/////////////////////////////////////////////////////////////////////////////
// partition                                                               //
/////////////////////////////////////////////////////////////////////////////
@test
def partition01(): Bool = HashSet.partition(x -> x `Int32.remainder` 2 == 0, HashSet.empty()) == (HashSet.empty(), HashSet.empty())

@test
def partition02(): Bool = HashSet.partition(x -> x `Int32.remainder` 2 == 0, HashSet.singleton(1)) == (HashSet.empty(), HashSet.singleton(1))

@test
def partition03(): Bool = HashSet.partition(x -> x `Int32.remainder` 2 == 0, HashSet.singleton(2)) == (HashSet.singleton(2), HashSet.empty())

@test
def partition04(): Bool = HashSet.partition(x -> x `Int32.remainder` 2 == 0, HashSet.collect(List#{1, 3})) == (HashSet.empty(), HashSet.collect(List#{1, 3}))

@test
def partition05(): Bool = HashSet.partition(x -> x `Int32.remainder` 2 == 0, HashSet.collect(List#{1, 2})) == (HashSet.singleton(2), HashSet.singleton(1))

@test
def partition06(): Bool = HashSet.partition(x -> x `Int32.remainder` 2 == 0, HashSet.collect(List#{2, 1})) == (HashSet.singleton(2), HashSet.singleton(1))

@test
def partition07(): Bool = HashSet.partition(x -> x `Int32.remainder` 2 == 0, HashSet.collect(List#{2, -4})) == (HashSet.collect(List#{2, -4}), HashSet.empty())

@test
def partition08(): Bool = HashSet.partition(x -> x `Int32.remainder` 2 == 0, HashSet.collect(List#{2, -11, 89, -4, 11, -6, 84})) == (HashSet.collect(List#{2, -4, -6, 84}), HashSet.collect(List#{-11, 89, 11}))

@test
def partition09(): Bool = HashSet.partition(x -> x `Int32.remainder` 2 == 0, HashSet.collect(List#{84, -6, 11, -4, 89, -11, 2})) == (HashSet.collect(List#{84, -6, -4, 2}), HashSet.collect(List#{11, 89, -11}))

@test
def partition10(): Bool = HashSet.partition(x -> x `Int32.remainder` 2 == 0, HashSet.collect(List#{1, 2, 3, 4, 5, 6, 7, 8})) == (HashSet.collect(List#{2, 4, 6, 8}), HashSet.collect(List#{1, 3, 5, 7}))

/////////////////////////////////////////////////////////////////////////////
// toList                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def toList01(): Bool = HashSet.toList((HashSet.empty(): HashSet[Unit])) == Nil

@test
def toList02(): Bool = HashSet.toList(HashSet.singleton(1)) == 1 :: Nil

@test
def toList03(): Bool = HashSet.toList(HashSet.collect(List#{1, 2})) == 1 :: 2 :: Nil

@test
def toList04(): Bool = HashSet.toList(HashSet.collect(List#{1, 2, 3})) == 1 :: 2 :: 3 :: Nil

/////////////////////////////////////////////////////////////////////////////
// toArray                                                                 //
/////////////////////////////////////////////////////////////////////////////
@test
def toArray01(): Bool = region rc {
    Array.sameElements(HashSet.toArray(rc, (HashSet.empty(): HashSet[Unit])), (Array#{} @ rc))
}

@test
def toArray02(): Bool = region rc {
    Array.sameElements(HashSet.toArray(rc, HashSet.singleton(1)), Array#{1} @ rc)
}

@test
def toArray03(): Bool = region rc {
    Array.sameElements(HashSet.toArray(rc, HashSet.collect(List#{1, 2})), Array#{1, 2} @ rc)
}

@test
def toArray04(): Bool = region rc {
    Array.sameElements(HashSet.toArray(rc, HashSet.collect(List#{1, 2, 3})), Array#{1, 2, 3} @ rc)
}

@test
def toArray05(): Bool = region rc {
    Array.sameElements(HashSet.toArray(rc, HashSet.collect(List#{3, 2, 1})), Array#{1, 2, 3} @ rc)
}

/////////////////////////////////////////////////////////////////////////////
// toVector                                                                //
/////////////////////////////////////////////////////////////////////////////
@test
def toVector01(): Bool = HashSet.toVector((HashSet.empty(): HashSet[Unit])) == Vector.empty()

@test
def toVector02(): Bool = HashSet.toVector(HashSet.singleton(1)) == Vector#{1}

@test
def toVector03(): Bool = HashSet.toVector(HashSet.collect(List#{1, 2})) == Vector#{1, 2}

@test
def toVector04(): Bool = HashSet.toVector(HashSet.collect(List#{1, 2, 3})) == Vector#{1, 2, 3}

@test
def toVector05(): Bool = HashSet.toVector(HashSet.collect(List#{3, 2, 1})) == Vector#{1, 2, 3}

/////////////////////////////////////////////////////////////////////////////
// toChain                                                                 //
/////////////////////////////////////////////////////////////////////////////

@test
def toChain01(): Bool =
    HashSet.toChain((HashSet.empty(): HashSet[Int32])) == (Chain.empty(): Chain[Int32])

@test
def toChain02(): Bool =
    HashSet.toChain(HashSet.singleton(1)) == Chain.singleton(1)

@test
def toChain03(): Bool =
    HashSet.toChain(HashSet.collect(List#{1, 2})) == List.toChain(1 :: 2 :: Nil)

@test
def toChain04(): Bool =
    HashSet.toChain(HashSet.collect(List#{1, 2, 3})) == List.toChain(1 :: 2 :: 3 :: Nil)

/////////////////////////////////////////////////////////////////////////////
// toMap                                                                   //
/////////////////////////////////////////////////////////////////////////////
@test
def toMap01(): Bool = HashSet.toMap((HashSet.empty(): HashSet[(Unit, Unit)])) == Map#{}

@test
def toMap02(): Bool = HashSet.toMap(HashSet.collect(List#{(1, true)})) == Map#{1 => true}

@test
def toMap03(): Bool = HashSet.toMap(HashSet.collect(List#{(1, true), (2, false)})) == Map#{1 => true, 2 => false}

@test
def toMap04(): Bool = HashSet.toMap(HashSet.collect(List#{(1, true), (1, false)})) == Map#{1 => true}

@test
def toMap05(): Bool = HashSet.toMap(HashSet.collect(List#{(1, true), (2, false), (3, true)})) == Map#{1 => true, 2 => false, 3 => true}

/////////////////////////////////////////////////////////////////////////////
// toMapWith                                                               //
/////////////////////////////////////////////////////////////////////////////

@test
def toMapWith02(): Bool =
    HashSet.toMapWith(x -> 0 * x, HashSet.empty()) == Map#{}

@test
def toMapWith03(): Bool =
    HashSet.toMapWith(x -> 2 * x, HashSet.collect(List#{1, 3, 6})) == Map#{1 => 2, 3 => 6, 6 => 12}

@test
def toMapWith04(): Bool =
    HashSet.toMapWith(x -> x + 10, HashSet.collect(List#{1, 3, 6})) == Map#{1 => 11, 3 => 13, 6 => 16}

@test
def toMapWith05(): Bool =
    HashSet.toMapWith(x -> x + 10, HashSet.collect(List#{7, 1, 3, 6})) == Map#{1 => 11, 3 => 13, 6 => 16, 7 => 17}

/////////////////////////////////////////////////////////////////////////////
// toMutDeque                                                              //
/////////////////////////////////////////////////////////////////////////////

@test
def toMutDeque01(): Bool = region rc {
    let s: HashSet[Int32] = HashSet.empty();
    let d1 = HashSet.toMutDeque(rc, s);

    let d2 = MutDeque.empty(rc);

    d1 `MutDeque.sameElements` d2
}

@test
def toMutDeque02(): Bool = region rc {
    let s = HashSet.singleton(1);
    let d1 = HashSet.toMutDeque(rc, s);

    let d2 = MutDeque.empty(rc);
    MutDeque.pushBack(1, d2);

    d1 `MutDeque.sameElements` d2
}

@test
def toMutDeque03(): Bool = region rc {
    let s = HashSet.collect(List#{1, 3, 6});
    let d1 = HashSet.toMutDeque(rc, s);

    let d2 = MutDeque.empty(rc);
    MutDeque.pushBack(3, d2);
    MutDeque.pushBack(6, d2);
    MutDeque.pushFront(1, d2);

    d1 `MutDeque.sameElements` d2
}

@test
def toMutDeque04(): Bool = region rc {
    let s = HashSet.collect(List#{7, 1, 3, 6});
    let d1 = HashSet.toMutDeque(rc, s);

    let d2 = MutDeque.empty(rc);
    MutDeque.pushFront(7, d2);
    MutDeque.pushFront(6, d2);
    MutDeque.pushFront(3, d2);
    MutDeque.pushFront(1, d2);

    d1 `MutDeque.sameElements` d2
}

/////////////////////////////////////////////////////////////////////////////
// eq                                                                      //
/////////////////////////////////////////////////////////////////////////////
@test
def eq01(): Bool = (HashSet.empty(): HashSet[Unit]) == HashSet.empty()

@test
def eq02(): Bool = HashSet.singleton(1) != HashSet.empty()

@test
def eq03(): Bool = HashSet.empty() != HashSet.singleton(1)

@test
def eq04(): Bool = HashSet.collect(List#{1, 2}) != HashSet.empty()

@test
def eq05(): Bool = HashSet.empty() != HashSet.collect(List#{1, 2})

@test
def eq06(): Bool = HashSet.singleton(1) != HashSet.singleton(2)

@test
def eq07(): Bool = HashSet.singleton(1) == HashSet.singleton(1)

@test
def eq08(): Bool = HashSet.collect(List#{1, 2, 3}) != HashSet.empty()

@test
def eq09(): Bool = HashSet.collect(List#{1, 2}) != HashSet.singleton(1)

@test
def eq10(): Bool = HashSet.singleton(1) != HashSet.collect(List#{1, 2})

@test
def eq11(): Bool = HashSet.empty() != HashSet.collect(List#{1, 2, 3})

@test
def eq12(): Bool = HashSet.collect(List#{1, 2}) != HashSet.collect(List#{1, 3})

@test
def eq13(): Bool = HashSet.collect(List#{1, 2}) == HashSet.collect(List#{2, 1})

@test
def eq14(): Bool = HashSet.collect(List#{1, 2}) == HashSet.collect(List#{1, 2})

@test
def eq15(): Bool = HashSet.collect(List#{1, 2, 3}) != HashSet.collect(List#{1, 2, 4})

@test
def eq16(): Bool = HashSet.collect(List#{1, 2, 3}) == HashSet.collect(List#{1, 2, 3})

@test
def eq17(): Bool = HashSet.collect(List#{1, 2, 3}) == HashSet.collect(List#{2, 3, 1})

@test
def eq18(): Bool = HashSet.collect(List#{1, 2, 3}) == HashSet.collect(List#{3, 1, 2})

@test
def eq19(): Bool = HashSet.collect(List#{1, 2, 3}) == HashSet.collect(List#{2, 1, 3})

/////////////////////////////////////////////////////////////////////////////
// forEach                                                                 //
/////////////////////////////////////////////////////////////////////////////

@test
def forEach01(): Bool = region rc {
    let ri = ref 21 @ rc;
    HashSet.forEach(x -> Ref.put(x, ri), HashSet.empty());
    21 == deref ri
}

@test
def forEach02(): Bool = region rc {
    let ri = ref 21 @ rc;
    HashSet.forEach(x -> Ref.put(x, ri), HashSet.singleton(42));
    42 == deref ri
}

/////////////////////////////////////////////////////////////////////////////
// forEachWithIndex                                                        //
/////////////////////////////////////////////////////////////////////////////

// @test
// def forEachWithIndex01(): Bool = region rc {
//     let ri = ref 21 @ rc;
//     HashSet.forEachWithIndex((i, _) -> Ref.put(i, ri), HashSet.empty());
//     21 == deref ri
// }

// @test
// def forEachWithIndex02(): Bool = region rc {
//     let ri = ref 21 @ rc;
//     HashSet.forEachWithIndex((i, _) -> Ref.put(i, ri), HashSet.singleton(42));
//     0 == deref ri
// }

/////////////////////////////////////////////////////////////////////////////
// unfold                                                                  //
/////////////////////////////////////////////////////////////////////////////

@test
def unfold01(): Bool =
    HashSet.unfold(s -> if (true) None else Some(s + 48, s + 1), 0) == HashSet.empty()

@test
def unfold02(): Bool =
    HashSet.unfold(s -> if (s > 0) None else Some(s + 48, s + 1), 0) == HashSet.singleton(48)

@test
def unfold03(): Bool =
    HashSet.unfold(s -> if (s > 1) None else Some(s + 48, s + 1), 0) == HashSet.collect(List#{48, 49})

@test
def unfold04(): Bool =
    HashSet.unfold(s -> if (s >= 10) None else Some(s + 48, s + 1), 0) == HashSet.collect(List#{48, 49, 50, 51, 52, 53, 54, 55, 56, 57})

@test
def unfold05(): Bool =
    HashSet.unfold(s -> if (s >= 10) None else Some(s + 48, s + 1), 5) == HashSet.collect(List#{53, 54, 55, 56, 57})

@test
def unfold06(): Bool =
    HashSet.unfold(s -> if (s >= 10) None else Some(s + 48, s + 2), 0) == HashSet.collect(List#{48, 50, 52, 54, 56})

/////////////////////////////////////////////////////////////////////////////
// unfoldWithIter                                                          //
/////////////////////////////////////////////////////////////////////////////

@test
def unfoldWithIter01(): Bool = region rc {
    let x = ref 0 @ rc;
    let step = () ->
        if (true)
            None
        else {
            let c = Some(deref x + 48);
            Ref.put(deref x + 1, x);
            c
        };
    HashSet.unfoldWithIter(step) == HashSet.empty()
}

@test
def unfoldWithIter02(): Bool = region rc {
    let x = ref 0 @ rc;
    let step = () ->
        if (deref x > 0)
            None
        else {
            let c = Some(deref x + 48);
            Ref.put(deref x + 1, x);
            c
        };
    HashSet.unfoldWithIter(step) == HashSet.singleton(48)
}

@test
def unfoldWithIter03(): Bool = region rc {
    let x = ref 0 @ rc;
    let step = () ->
        if (deref x > 1)
            None
        else {
            let c = Some(deref x + 48);
            Ref.put(deref x + 1, x);
            c
        };
    HashSet.unfoldWithIter(step) == HashSet.collect(List#{48, 49})
}

@test
def unfoldWithIter04(): Bool = region rc {
    let x = ref 0 @ rc;
    let step = () ->
        if (deref x >= 10)
            None
        else {
            let c = Some(deref x + 48);
            Ref.put(deref x + 1, x);
            c
        };
    HashSet.unfoldWithIter(step) == HashSet.collect(List#{48, 49, 50, 51, 52, 53, 54, 55, 56, 57})
}

@test
def unfoldWithIter05(): Bool = region rc {
    let x = ref 5 @ rc;
    let step = () ->
        if (deref x >= 10)
            None
        else {
            let c = Some(deref x + 48);
            Ref.put(deref x + 1, x);
            c
        };
    HashSet.unfoldWithIter(step) == HashSet.collect(List#{53, 54, 55, 56, 57})
}

@test
def unfoldWithIter06(): Bool = region rc {
    let x = ref 0 @ rc;
    let step = () ->
        if (deref x >= 10)
            None
        else {
            let c = Some(deref x + 48);
            Ref.put(deref x + 2, x);
            c
        };
    HashSet.unfoldWithIter(step) == HashSet.collect(List#{48, 50, 52, 54, 56})
}

    /////////////////////////////////////////////////////////////////////////////
    // toString                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toString01(): Bool =
        toString(HashSet.singleton(1)) == "HashSet(1)"

    @test
    def toString02(): Bool =
        toString(HashSet.collect(List#{1, 2, 3})) == "HashSet(1, 2, 3)"

    @test
    def toString03(): Bool =
        toString(HashSet.collect(List#{1, 2})) == "HashSet(1, 2)"

    @test
    def toString04(): Bool =
        toString(HashSet.collect(List#{97, 2, 3, 4, 0})) == "HashSet(0, 2, 3, 4, 97)"

    @test
    def toString05(): Bool =
        toString(HashSet.collect(List#{HashSet.collect(List#{1, 2}), HashSet.collect(List#{4, 6})})) == "HashSet(HashSet(4, 6), HashSet(1, 2))"

    /////////////////////////////////////////////////////////////////////////////
    // hash                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def hash01(): Bool = hash((HashSet.empty(): HashSet[Unit])) == hash((HashSet.empty(): HashSet[Unit]))

    @test
    def hash02(): Bool = hash(HashSet.collect(List#{1, 2})) == hash(HashSet.collect(List#{1, 2}))

    @test
    def hash03(): Bool = hash(HashSet.collect(List#{'a', 'b', 'c', 'd'})) == hash(HashSet.collect(List#{'a', 'b', 'c', 'd'}))

    @test
    def hash04(): Bool = hash(HashSet.collect(List#{'a', 'b', 'c', 'e'})) != hash(HashSet.collect(List#{'a', 'b', 'c', 'd'}))

    @test
    def hash05(): Bool = hash(HashSet.singleton(false)) != hash(HashSet.singleton(true))

    /////////////////////////////////////////////////////////////////////////////
    // minimum                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    // @test
    // def minimum01(): Bool = HashSet.minimum((HashSet.empty(): HashSet[Int32])) == None

    // @test
    // def minimum02(): Bool = HashSet.minimum(HashSet.singleton(1)) == Some(1)

    // @test
    // def minimum03(): Bool = HashSet.minimum(HashSet.collect(List#{1, 2, 3, 0})) == Some(0)

    // @test
    // def minimum04(): Bool = HashSet.minimum(HashSet.range(5, 55)) == Some(5)

    /////////////////////////////////////////////////////////////////////////////
    // minimumBy                                                               //
    /////////////////////////////////////////////////////////////////////////////

    // @test
    // def minimumBy01(): Bool = HashSet.minimumBy((x, y) -> x <=> y, (HashSet.empty(): HashSet[Int32])) == None

    // @test
    // def minimumBy02(): Bool = HashSet.minimumBy((x, y) -> x <=> y, HashSet.singleton(1)) == Some(1)

    // @test
    // def minimumBy03(): Bool = HashSet.minimumBy((x, y) -> x <=> y, HashSet.collect(List#{1, 2, 3, 0})) == Some(0)

    // @test
    // def minimumBy04(): Bool = HashSet.minimumBy((x, y) -> x <=> y, HashSet.range(5, 55)) == Some(5)

    // @test
    // def minimumBy05(): Bool = region rc {
    //     let s = List.range(0, 2000) |> List.toSet;
    //     let a = MutList.empty(rc);
    //     discard HashSet.minimumBy((x, y) -> { let b = MutList.empty(rc); MutList.push!(y, b); MutList.append!(MutList.toList(b), a); x <=> y }, s);
    //     MutList.toList(a) == List.range(1, 2000) // The first / left most value is ignored
    // }


    /////////////////////////////////////////////////////////////////////////////
    // maximum                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    // @test
    // def maximum01(): Bool = HashSet.maximum((HashSet.empty(): HashSet[Int32])) == None

    // @test
    // def maximum02(): Bool = HashSet.maximum(HashSet.singleton(1)) == Some(1)

    // @test
    // def maximum03(): Bool = HashSet.maximum(HashSet.collect(List#{1, 2, 3, 0})) == Some(3)

    // @test
    // def maximum04(): Bool = HashSet.maximum(HashSet.range(5, 55)) == Some(54)

    /////////////////////////////////////////////////////////////////////////////
    // maximumBy                                                               //
    /////////////////////////////////////////////////////////////////////////////

    // @test
    // def maximumBy01(): Bool = HashSet.maximumBy((x, y) -> x <=> y, (HashSet.empty(): HashSet[Int32])) == None

    // @test
    // def maximumBy02(): Bool = HashSet.maximumBy((x, y) -> x <=> y, HashSet.singleton(1)) == Some(1)

    // @test
    // def maximumBy03(): Bool = HashSet.maximumBy((x, y) -> x <=> y, HashSet.collect(List#{1, 2, 3, 0})) == Some(3)

    // @test
    // def maximumBy04(): Bool = HashSet.maximumBy((x, y) -> x <=> y, HashSet.range(5, 55)) == Some(54)


    /////////////////////////////////////////////////////////////////////////////
    // toDelayList                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toDelayList01(): Bool =
        (HashSet.empty(): HashSet[Unit]) |> HashSet.toDelayList == DelayList.empty()

    @test
    def toDelayList02(): Bool =
        HashSet.range(-1000, 1000) |> HashSet.toDelayList == DelayList.range(-1000, 1000)

    @test
    def toDelayList03(): Bool = region local {
        HashSet.range(-1000, 1000) |> HashSet.toDelayList |> DelayList.iterator(local) |> HashSet.collectI == HashSet.range(-1000, 1000)
    }


    /////////////////////////////////////////////////////////////////////////////
    // iterator                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def iterator01(): Bool = region rc {
        (HashSet.empty(): HashSet[Int32]) |> HashSet.iterator(rc) |> HashSet.collectI == HashSet.empty()
    }

    @test
    def iterator02(): Bool = region rc {
        HashSet.collect(List#{1, 2, 3, 4}) |> HashSet.iterator(rc) |> HashSet.collectI == HashSet.collect(List#{1, 2, 3, 4})
    }

    @test
    def iterator03(): Bool = region rc {
        HashSet.range(0, 100) |> HashSet.iterator(rc) |> HashSet.collectI == HashSet.range(0, 100)
    }


    /////////////////////////////////////////////////////////////////////////////
    // join                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    // @test
    // def join01(): Bool =
    //     HashSet.join(",", (HashSet.empty(): HashSet[Int32])) == ""

    // @test
    // def join02(): Bool =
    //     HashSet.join(",", HashSet.singleton(1)) == "1"

    // @test
    // def join03(): Bool =
    //     HashSet.join(",", HashSet.collect(List#{1, 2, 3})) == "1,2,3"

    // @test
    // def join04(): Bool =
    //     HashSet.join(",", HashSet.collect(List#{"1", "2", "3"})) == "1,2,3"


    /////////////////////////////////////////////////////////////////////////////
    // joinWith                                                                //
    /////////////////////////////////////////////////////////////////////////////

    // @test
    // def joinWith01(): Bool =
    //     (Nil: List[Int32]) |> List.toSet |>
    //         HashSet.joinWith(x -> "${x + 1}", ",") == ""

    // @test
    // def joinWith02(): Bool =
    //     (1 :: Nil) |> List.toSet |>
    //         HashSet.joinWith(x -> "${x + 1}", ",") == "2"

    // @test
    // def joinWith03(): Bool =
    //     (1 :: 2 :: 3 :: Nil) |> List.toSet |>
    //         HashSet.joinWith(x -> "${x + 1}", ",") == "2,3,4"

    // @test
    // def joinWith04(): Bool =
    //     ("1" :: "2" :: "3" :: Nil) |> List.toSet |>
    //         HashSet.joinWith(x -> x + x, ",") == "11,22,33"

}

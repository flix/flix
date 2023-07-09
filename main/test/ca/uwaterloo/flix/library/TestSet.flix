/*
 * Copyright 2017 Liam Palmer
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
    Collectable.collect(Iterator.empty(rc): Iterator[Int32, rc, rc]) == Set.empty()
}

@test
def collect02(): Bool = region rc {
    Collectable.collect(Iterator.singleton(rc, 1)) == Set#{1}
}

@test
def collect03(): Bool = region rc {
    Collectable.collect(Iterator.range(rc, 1, 3)) == Set#{1, 2}
}

/////////////////////////////////////////////////////////////////////////////
// size                                                                    //
/////////////////////////////////////////////////////////////////////////////
@test
def size01(): Bool = Set.size(Set#{}) == 0

@test
def size02(): Bool = Set.size(Set#{1}) == 1

@test
def size03(): Bool = Set.size(Set#{1, 2}) == 2

@test
def size04(): Bool = Set.size(Set#{1, 2, 3}) == 3

@test
def size05(): Bool = Set.size(Set#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 14}) == 13

/////////////////////////////////////////////////////////////////////////////
// empty                                                                   //
/////////////////////////////////////////////////////////////////////////////
@test
def empty01(): Bool = Set.empty(): Set[Unit] == Set#{}

/////////////////////////////////////////////////////////////////////////////
// singleton                                                               //
/////////////////////////////////////////////////////////////////////////////
@test
def singleton01(): Bool = Set.singleton(1) == Set#{1}

@test
def singleton02(): Bool = Set.singleton(2) == Set#{2}

@test
def singleton03(): Bool = Set.singleton(true) == Set#{true}

@test
def singleton04(): Bool = Set.singleton(false) == Set#{false}

/////////////////////////////////////////////////////////////////////////////
// insert                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def insert01(): Bool = Set.insert(1, Set#{}) == Set#{1}

@test
def insert02(): Bool = Set.insert(1, Set#{1}) == Set#{1}

@test
def insert03(): Bool = Set.insert(1, Set#{2}) == Set#{1, 2}

@test
def insert04(): Bool = Set.insert(1, Set#{2, 3}) == Set#{1, 2, 3}

@test
def insert05(): Bool = Set.insert(1, Set#{1, 2, 3}) == Set#{1, 2, 3}

@test
def insert06(): Bool = Set.insert(1, Set#{2, 1, 3}) == Set#{2, 1, 3}

@test
def insert07(): Bool = Set.insert(1, Set#{2, 1, 3, 7, 8, 9, 10}) == Set#{2, 1, 3, 7, 8, 9, 10}

@test
def insert08(): Bool = Set.insert(11, Set#{2, 1, 3, 7, 8, 9, 10, 35}) == Set#{11, 2, 1, 3, 7, 8, 9, 10, 35}

/////////////////////////////////////////////////////////////////////////////
// remove                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def remove01(): Bool = Set.remove(1, Set#{}) == Set#{}

@test
def remove02(): Bool = Set.remove(1, Set#{1}) == Set#{}

@test
def remove03(): Bool = Set.remove(1, Set#{2}) == Set#{2}

@test
def remove04(): Bool = Set.remove(2, Set#{2, 3}) == Set#{3}

@test
def remove05(): Bool = Set.remove(3, Set#{2, 3}) == Set#{2}

@test
def remove06(): Bool = Set.remove(1, Set#{2, 3}) == Set#{2, 3}

@test
def remove07(): Bool = Set.remove(0, Set#{1, 2, 3, 4, 5, 6, 7}) == Set#{1, 2, 3, 4, 5, 6, 7}

@test
def remove08(): Bool = Set.remove(1, Set#{1, 2, 3, 4, 5, 6, 7}) == Set#{2, 3, 4, 5, 6, 7}

@test
def remove09(): Bool = Set.remove(2, Set#{1, 2, 3, 4, 5, 6, 7}) == Set#{1, 3, 4, 5, 6, 7}

@test
def remove10(): Bool = Set.remove(6, Set#{1, 2, 3, 4, 5, 6, 7}) == Set#{1, 2, 3, 4, 5, 7}

@test
def remove11(): Bool = Set.remove(7, Set#{1, 2, 3, 4, 5, 6, 7}) == Set#{1, 2, 3, 4, 5, 6}

@test
def remove12(): Bool = Set.remove(8, Set#{1, 2, 3, 4, 5, 6, 7}) == Set#{1, 2, 3, 4, 5, 6, 7}

/////////////////////////////////////////////////////////////////////////////
// isEmpty                                                                 //
/////////////////////////////////////////////////////////////////////////////
@test
def isEmpty01(): Bool = Set.isEmpty(Set#{}: Set[Unit]) == true

@test
def isEmpty02(): Bool = Set.isEmpty(Set#{1}) == false

@test
def isEmpty03(): Bool = Set.isEmpty(Set#{1, 2}) == false

@test
def isEmpty04(): Bool = Set.isEmpty(Set#{1, 2, 3}) == false

@test
def isEmpty05(): Bool = Set.isEmpty(Set#{1, 2, 3, 4, 5, 6, 7, 8}) == false

/////////////////////////////////////////////////////////////////////////////
// memberOf                                                                //
/////////////////////////////////////////////////////////////////////////////
@test
def memberOf01(): Bool = Set.memberOf(1, Set#{}) == false

@test
def memberOf02(): Bool = Set.memberOf(1, Set#{2}) == false

@test
def memberOf03(): Bool = Set.memberOf(1, Set#{1}) == true

@test
def memberOf04(): Bool = Set.memberOf(1, Set#{1, 2}) == true

@test
def memberOf05(): Bool = Set.memberOf(2, Set#{1, 2}) == true

@test
def memberOf06(): Bool = Set.memberOf(0, Set#{1, 2}) == false

@test
def memberOf07(): Bool = Set.memberOf(3, Set#{1, 2}) == false

@test
def memberOf08(): Bool = Set.memberOf(0, Set#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}) == false

@test
def memberOf09(): Bool = Set.memberOf(1, Set#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}) == true

@test
def memberOf10(): Bool = Set.memberOf(2, Set#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}) == true

@test
def memberOf11(): Bool = Set.memberOf(10, Set#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}) == true

@test
def memberOf12(): Bool = Set.memberOf(12, Set#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}) == false

/////////////////////////////////////////////////////////////////////////////
// find                                                                    //
/////////////////////////////////////////////////////////////////////////////
@test
def find01(): Bool = Set.find(i -> i > 2, Set#{}) == None

@test
def find02(): Bool = Set.find(i -> i > 2, Set#{1}) == None

@test
def find03(): Bool = Set.find(i -> i > 2, Set#{3}) == Some(3)

@test
def find04(): Bool = Set.find(i -> i > 2, Set#{2, 1}) == None

@test
def find05(): Bool = Set.find(i -> i > 2, Set#{-6, 6}) == Some(6)

@test
def find06(): Bool = Set.find(i -> i > 2, Set#{6, -6}) == Some(6)

@test
def find07(): Bool = Set.find(i -> i > 2, Set#{7, 6}) == Some(6)

/////////////////////////////////////////////////////////////////////////////
// findLeft                                                                //
/////////////////////////////////////////////////////////////////////////////
@test
def findLeft01(): Bool = Set.findLeft(i -> i > 2, Set#{}) == None

@test
def findLeft02(): Bool = Set.findLeft(i -> i > 2, Set#{1}) == None

@test
def findLeft03(): Bool = Set.findLeft(i -> i > 2, Set#{3}) == Some(3)

@test
def findLeft04(): Bool = Set.findLeft(i -> i > 2, Set#{2, 1}) == None

@test
def findLeft05(): Bool = Set.findLeft(i -> i > 2, Set#{-6, 6}) == Some(6)

@test
def findLeft06(): Bool = Set.findLeft(i -> i > 2, Set#{6, -6}) == Some(6)

@test
def findLeft07(): Bool = Set.findLeft(i -> i > 2, Set#{7, 6}) == Some(6)

/////////////////////////////////////////////////////////////////////////////
// findRight                                                               //
/////////////////////////////////////////////////////////////////////////////
@test
def findRight01(): Bool = Set.findRight(i -> i > 2, Set#{}) == None

@test
def findRight02(): Bool = Set.findRight(i -> i > 2, Set#{1}) == None

@test
def findRight03(): Bool = Set.findRight(i -> i > 2, Set#{3}) == Some(3)

@test
def findRight04(): Bool = Set.findRight(i -> i > 2, Set#{2, 1}) == None

@test
def findRight05(): Bool = Set.findRight(i -> i > 2, Set#{-6, 6}) == Some(6)

@test
def findRight06(): Bool = Set.findRight(i -> i > 2, Set#{6, -6}) == Some(6)

@test
def findRight07(): Bool = Set.findRight(i -> i > 2, Set#{7, 6}) == Some(7)

/////////////////////////////////////////////////////////////////////////////
// isSubsetOf                                                              //
/////////////////////////////////////////////////////////////////////////////
@test
def isSubsetOf01(): Bool = Set.isSubsetOf(Set#{}: Set[Unit], Set#{}) == true

@test
def isSubsetOf02(): Bool = Set.isSubsetOf(Set#{}, Set#{1}) == true

@test
def isSubsetOf03(): Bool = Set.isSubsetOf(Set#{1}, Set#{1}) == true

@test
def isSubsetOf04(): Bool = Set.isSubsetOf(Set#{1}, Set#{2}) == false

@test
def isSubsetOf05(): Bool = Set.isSubsetOf(Set#{}, Set#{1, 2}) == true

@test
def isSubsetOf06(): Bool = Set.isSubsetOf(Set#{1}, Set#{1, 2}) == true

@test
def isSubsetOf07(): Bool = Set.isSubsetOf(Set#{2}, Set#{1, 2}) == true

@test
def isSubsetOf08(): Bool = Set.isSubsetOf(Set#{3}, Set#{1, 2}) == false

@test
def isSubsetOf09(): Bool = Set.isSubsetOf(Set#{1, 2}, Set#{1, 2}) == true

@test
def isSubsetOf10(): Bool = Set.isSubsetOf(Set#{2, 1}, Set#{1, 2}) == true

@test
def isSubsetOf11(): Bool = Set.isSubsetOf(Set#{3, 1}, Set#{1, 2}) == false

@test
def isSubsetOf12(): Bool = Set.isSubsetOf(Set#{1, 2, 3}, Set#{1, 2}) == false

@test
def isSubsetOf13(): Bool = Set.isSubsetOf(Set#{10}, Set#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}) == true

@test
def isSubsetOf14(): Bool = Set.isSubsetOf(Set#{9, 1}, Set#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}) == true

@test
def isSubsetOf15(): Bool = Set.isSubsetOf(Set#{6, 5, 8}, Set#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}) == true

@test
def isSubsetOf16(): Bool = Set.isSubsetOf(Set#{10, 2, 3, 6, -1, 4}, Set#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}) == false

/////////////////////////////////////////////////////////////////////////////
// isProperSubsetOf                                                        //
/////////////////////////////////////////////////////////////////////////////
@test
def isProperSubsetOf01(): Bool = Set.isProperSubsetOf(Set#{}: Set[Unit], Set#{}) == false

@test
def isProperSubsetOf02(): Bool = Set.isProperSubsetOf(Set#{}, Set#{1}) == true

@test
def isProperSubsetOf03(): Bool = Set.isProperSubsetOf(Set#{1}, Set#{1}) == false

@test
def isProperSubsetOf04(): Bool = Set.isProperSubsetOf(Set#{1}, Set#{2}) == false

@test
def isProperSubsetOf05(): Bool = Set.isProperSubsetOf(Set#{}, Set#{1, 2}) == true

@test
def isProperSubsetOf06(): Bool = Set.isProperSubsetOf(Set#{1}, Set#{1, 2}) == true

@test
def isProperSubsetOf07(): Bool = Set.isProperSubsetOf(Set#{2}, Set#{1, 2}) == true

@test
def isProperSubsetOf08(): Bool = Set.isProperSubsetOf(Set#{3}, Set#{1, 2}) == false

@test
def isProperSubsetOf09(): Bool = Set.isProperSubsetOf(Set#{1, 2}, Set#{1, 2}) == false

@test
def isProperSubsetOf10(): Bool = Set.isProperSubsetOf(Set#{2, 1}, Set#{1, 2}) == false

@test
def isProperSubsetOf11(): Bool = Set.isProperSubsetOf(Set#{3, 1}, Set#{1, 2}) == false

@test
def isProperSubsetOf12(): Bool = Set.isProperSubsetOf(Set#{1, 2, 3}, Set#{1, 2}) == false

@test
def isProperSubsetOf13(): Bool = Set.isProperSubsetOf(Set#{10}, Set#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}) == true

@test
def isProperSubsetOf14(): Bool = Set.isProperSubsetOf(Set#{9, 1}, Set#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}) == true

@test
def isProperSubsetOf15(): Bool = Set.isProperSubsetOf(Set#{6, 5, 8}, Set#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}) == true

@test
def isProperSubsetOf16(): Bool = Set.isProperSubsetOf(Set#{10, 2, 3, 6, -1, 4}, Set#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}) == false

@test
def isProperSubsetOf17(): Bool = Set.isProperSubsetOf(Set#{10, 2, 3, 6, 1, 4, 5, 9, 8, 7}, Set#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}) == false

    /////////////////////////////////////////////////////////////////////////////
    // fold                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def fold01(): Bool = Set.fold(Set#{}: Set[String]) == ""

    @test
    def fold02(): Bool = Set.fold(Set#{}: Set[Unit]) == ()

    @test
    def fold03(): Bool = Set.fold(Set#{"a", "b", "c"}) == "abc"

    @test
    def fold04(): Bool = Set.fold(Set#{("a", "b"), ("c", "d")}) == ("ac", "bd")

/////////////////////////////////////////////////////////////////////////////
// foldLeft                                                                //
/////////////////////////////////////////////////////////////////////////////
@test
def foldLeft01(): Bool = Set.foldLeft((i, e) -> (i - e)*(e `Int32.rem` 2 + 1), 100, Set#{}) == 100

@test
def foldLeft02(): Bool = Set.foldLeft((i, e) -> (i - e)*(e `Int32.rem` 2 + 1), 100, Set#{1}) == 198

@test
def foldLeft03(): Bool = Set.foldLeft((i, e) -> (i - e)*(e `Int32.rem` 2 + 1), 100, Set#{2, 1}) == 196

@test
def foldLeft04(): Bool = Set.foldLeft((i, e) -> (i - e)*(e `Int32.rem` 2 + 1), 100, Set#{3, 2, 1}) == 386

/////////////////////////////////////////////////////////////////////////////
// foldRight                                                               //
/////////////////////////////////////////////////////////////////////////////
@test
def foldRight01(): Bool = Set.foldRight((e, acc) -> (acc - e) * (e `Int32.rem` 2 + 1), 100, Set#{}) == 100

@test
def foldRight02(): Bool = Set.foldRight((e, acc) -> (acc - e) * (e `Int32.rem` 2 + 1), 100, Set#{1}) == 198

@test
def foldRight03(): Bool = Set.foldRight((e, acc) -> (acc - e) * (e `Int32.rem` 2 + 1), 100, Set#{2, 1}) == 194

@test
def foldRight04(): Bool = Set.foldRight((e, acc) -> (acc - e) * (e `Int32.rem` 2 + 1), 100, Set#{3, 2, 1}) == 382

/////////////////////////////////////////////////////////////////////////////
// foldRightWithCont                                                       //
/////////////////////////////////////////////////////////////////////////////
@test
def foldRightWithCont01(): Bool = Set.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.rem` 2 + 1), 100, Set#{}) == 100

@test
def foldRightWithCont02(): Bool = Set.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.rem` 2 + 1), 100, Set#{1}) == 198

@test
def foldRightWithCont03(): Bool = Set.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.rem` 2 + 1), 100, Set#{2, 1}) == 194

@test
def foldRightWithCont04(): Bool = Set.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.rem` 2 + 1), 100, Set#{3, 2, 1}) == 382

/////////////////////////////////////////////////////////////////////////////
// foldMap                                                                 //
/////////////////////////////////////////////////////////////////////////////

@test
def foldMap01(): Bool = Set.foldMap(x -> 2 * x, Set#{}) == 0

@test
def foldMap02(): Bool = Set.foldMap(x -> 2 * x, Set#{1, 2}) == 6

@test
def foldMap03(): Bool = Set.foldMap(x -> if (x == "a") "b" else x, Set#{"a"}) == "b"

@test
def foldMap04(): Bool = Set.foldMap(x -> if (x == "c") "b" else x, Set#{"a", "b", "c"}) == "abb"

@test
def foldMap05(): Bool = Set.foldMap(Int32.toString, Set#{1, 2, 3}) == "123"

/////////////////////////////////////////////////////////////////////////////
// reduceLeft                                                              //
/////////////////////////////////////////////////////////////////////////////
@test
def reduceLeft01(): Bool = Set.reduceLeft((a, b) -> a-b, Set#{}: Set[Int32]) == None

@test
def reduceLeft02(): Bool = Set.reduceLeft((a, b) -> a-b, Set#{1}) == Some(1)

@test
def reduceLeft03(): Bool = Set.reduceLeft((a, b) -> a-b, Set#{2, 1}) == Some(-1)

@test
def reduceLeft04(): Bool = Set.reduceLeft((a, b) -> a-b, Set#{3, 2, 1}) == Some(-4)

@test
def reduceLeft05(): Bool = Set.reduceLeft((a, b) -> a-b, Set#{4, 3, 2, 1}) == Some(-8)

/////////////////////////////////////////////////////////////////////////////
// reduceRight                                                             //
/////////////////////////////////////////////////////////////////////////////
@test
def reduceRight01(): Bool = Set.reduceRight((a, b) -> a-b, Set#{}: Set[Int32]) == None

@test
def reduceRight02(): Bool = Set.reduceRight((a, b) -> a-b, Set#{1}) == Some(1)

@test
def reduceRight03(): Bool = Set.reduceRight((a, b) -> a-b, Set#{2, 1}) == Some(-1)

@test
def reduceRight04(): Bool = Set.reduceRight((a, b) -> a-b, Set#{3, 2, 1}) == Some(2)

@test
def reduceRight05(): Bool = Set.reduceRight((a, b) -> a-b, Set#{4, 3, 2, 1}) == Some(-2)

/////////////////////////////////////////////////////////////////////////////
// count                                                                   //
/////////////////////////////////////////////////////////////////////////////
@test
def count01(): Bool = Set.count(i -> i > 3, Set#{}) == 0

@test
def count02(): Bool = Set.count(i -> i > 3, Set#{1}) == 0

@test
def count03(): Bool = Set.count(i -> i > 3, Set#{4}) == 1

@test
def count04(): Bool = Set.count(i -> i > 3, Set#{2, 1}) == 0

@test
def count05(): Bool = Set.count(i -> i > 3, Set#{8, 1}) == 1

@test
def count06(): Bool = Set.count(i -> i > 3, Set#{1, 8}) == 1

@test
def count07(): Bool = Set.count(i -> i > 3, Set#{7, 6}) == 2

@test
def count08(): Bool = region rc {
    let range = List.range(0, 2000);
    let s = range |> List.toSet;
    let a = MutList.new(rc);
    discard Set.count(x -> { let b = MutList.new(rc); MutList.push!(x, b); MutList.append!(MutList.toList(b), a); true }, s);
    MutList.toList(a) == range
}


    /////////////////////////////////////////////////////////////////////////////
    // sum                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sum01(): Bool =
        Set.empty() |> Set.sum == 0

    @test
    def sum02(): Bool =
        Set#{1} |> Set.sum == 1

    @test
    def sum03(): Bool =
        Set#{1, 2, 3} |> Set.sum == 6

    @test
    def sum04(): Bool =
        Set#{1, 2, 3, -3} |> Set.sum == 3

    @test
    def sum05(): Bool =
        Set#{1, 2, -3, -4} |> Set.sum == -4

    @test
    def sum06(): Bool =
        Set#{10, -10} |> Set.sum == 0

    @test
    def sum07(): Bool =
        Set.range(1, 101) |> Set.sum == 5050


    /////////////////////////////////////////////////////////////////////////////
    // sumWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sumWith01(): Bool =
        Set.empty() |> Set.sumWith(x -> x + 1) == 0

    @test
    def sumWith02(): Bool =
        Set#{1} |> Set.sumWith(x -> x + 1) == 2

    @test
    def sumWith03(): Bool =
        Set#{1, 2, 3} |> Set.sumWith(x -> x + 1) == 9

    @test
    def sumWith04(): Bool =
        Set#{1, 2, 3, -3} |> Set.sumWith(x -> x + 1) == 7

    @test
    def sumWith05(): Bool =
        Set#{-1, -2, -3, -4} |> Set.sumWith(x -> x + 1) == -6

    @test
    def sumWith06(): Bool =
        Set#{10, -10} |> Set.sumWith(x -> x + 1) == 2

    @test
    def sumWith08(): Bool = region rc {
        let range = List.range(0, 2000);
        let m = range |> List.toSet;
        let a = MutList.new(rc);
        discard Set.sumWith(k -> { let b = MutList.new(rc); MutList.push!(k, b); MutList.append!(MutList.toList(b), a); 1 }, m);
        MutList.toList(a) == range
    }


/////////////////////////////////////////////////////////////////////////////
// flatten                                                                 //
/////////////////////////////////////////////////////////////////////////////
@test
def flatten01(): Bool = Set.flatten(Set#{}): Set[Unit] == Set#{}

@test
def flatten02(): Bool = Set.flatten(Set#{Set#{}}: Set[Set[Unit]]) == Set#{}

@test
def flatten03(): Bool = Set.flatten(Set#{Set#{1}}) == Set#{1}

@test
def flatten04(): Bool = Set.flatten(Set#{Set#{1, 2}}) == Set#{1, 2}

@test
def flatten05(): Bool = Set.flatten(Set#{Set#{}, Set#{}}): Set[Unit] == Set#{}

@test
def flatten06(): Bool = Set.flatten(Set#{Set#{1}, Set#{}}) == Set#{1}

@test
def flatten07(): Bool = Set.flatten(Set#{Set#{}, Set#{1}}) == Set#{1}

@test
def flatten08(): Bool = Set.flatten(Set#{Set#{1}, Set#{2}}) == Set#{1, 2}

@test
def flatten09(): Bool = Set.flatten(Set#{Set#{1}, Set#{1}}) == Set#{1}

@test
def flatten10(): Bool = Set.flatten(Set#{Set#{1, 2}, Set#{3, 4, 5}}) == Set#{1, 2, 3, 4, 5}

@test
def flatten11(): Bool = Set.flatten(Set#{Set#{1, 2}, Set#{3, 4, 3}}) == Set#{1, 2, 3, 4}

@test
def flatten12(): Bool = Set.flatten(Set#{Set#{1, 2}, Set#{3, 4, 1}}) == Set#{1, 2, 3, 4}

@test
def flatten13(): Bool = Set.flatten(Set#{Set#{1}, Set#{2, 3}, Set#{4}}) == Set#{1, 2, 3, 4}

@test
def flatten14(): Bool = Set.flatten(Set#{Set#{1}, Set#{2, 3}, Set#{1}}) == Set#{1, 2, 3}

/////////////////////////////////////////////////////////////////////////////
// exists                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def exists01(): Bool = Set.exists(x -> x `Int32.rem` 8 == 7, Set#{}) == false

@test
def exists02(): Bool = Set.exists(x -> x `Int32.rem` 8 == 7, Set#{5}) == false

@test
def exists03(): Bool = Set.exists(x -> x `Int32.rem` 8 == 7, Set#{7}) == true

@test
def exists04(): Bool = Set.exists(x -> x `Int32.rem` 8 == 7, Set#{15}) == true

@test
def exists05(): Bool = Set.exists(x -> x `Int32.rem` 8 == 7, Set#{1, 44}) == false

@test
def exists06(): Bool = Set.exists(x -> x `Int32.rem` 8 == 7, Set#{11, 71}) == true

@test
def exists07(): Bool = Set.exists(x -> x `Int32.rem` 8 == 7, Set#{71, 12}) == true

@test
def exists08(): Bool = Set.exists(x -> x `Int32.rem` 8 == 7, Set#{71, 79}) == true

@test
def exists09(): Bool = Set.exists(x -> x `Int32.rem` 8 == 7, Set#{11, -1, -14, -2, 84, 113}) == false

@test
def exists10(): Bool = Set.exists(x -> x `Int32.rem` 8 == 7, Set#{11, -1, 31, -14, -2, 84, 111}) == true

@test
def exists11(): Bool = Set.exists(x -> x `Int32.rem` 8 == 7, Set#{11, -1, -14, -2, 84, 111, 38}) == true

/////////////////////////////////////////////////////////////////////////////
// forAll                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def forAll01(): Bool = Set.forAll(x -> x `Int32.rem` 8 == 7, Set#{}) == true

@test
def forAll02(): Bool = Set.forAll(x -> x `Int32.rem` 8 == 7, Set#{5}) == false

@test
def forAll03(): Bool = Set.forAll(x -> x `Int32.rem` 8 == 7, Set#{7}) == true

@test
def forAll04(): Bool = Set.forAll(x -> x `Int32.rem` 8 == 7, Set#{15}) == true

@test
def forAll05(): Bool = Set.forAll(x -> x `Int32.rem` 8 == 7, Set#{1, 44}) == false

@test
def forAll06(): Bool = Set.forAll(x -> x `Int32.rem` 8 == 7, Set#{11, 71}) == false

@test
def forAll07(): Bool = Set.forAll(x -> x `Int32.rem` 8 == 7, Set#{71, 12}) == false

@test
def forAll08(): Bool = Set.forAll(x -> x `Int32.rem` 8 == 7, Set#{71, 79}) == true

@test
def forAll09(): Bool = Set.forAll(x -> x `Int32.rem` 8 == 7, Set#{7, 15, 23, 84, 111}) == false

@test
def forAll10(): Bool = Set.forAll(x -> x `Int32.rem` 8 == 7, Set#{7, 15, 23, 111, 3}) == false

@test
def forAll11(): Bool = Set.forAll(x -> x `Int32.rem` 8 == 7, Set#{7, 15, 23, 119, 111}) == true

/////////////////////////////////////////////////////////////////////////////
// union                                                                   //
/////////////////////////////////////////////////////////////////////////////
@test
def union01(): Bool = Set.union(Set#{}: Set[Unit], Set#{}) == Set#{}

@test
def union02(): Bool = Set.union(Set#{1}, Set#{}) == Set#{1}

@test
def union03(): Bool = Set.union(Set#{}, Set#{2}) == Set#{2}

@test
def union04(): Bool = Set.union(Set#{1}, Set#{1}) == Set#{1}

@test
def union05(): Bool = Set.union(Set#{1}, Set#{-1}) == Set#{1, -1}

@test
def union06(): Bool = Set.union(Set#{}, Set#{-1, 9}) == Set#{-1, 9}

@test
def union07(): Bool = Set.union(Set#{9}, Set#{-1, 9}) == Set#{-1, 9}

@test
def union08(): Bool = Set.union(Set#{4}, Set#{-1, 9}) == Set#{4, -1, 9}

@test
def union09(): Bool = Set.union(Set#{9, -1}, Set#{-1, 9}) == Set#{-1, 9}

@test
def union10(): Bool = Set.union(Set#{9, 5}, Set#{-1, 9}) == Set#{5, -1, 9}

@test
def union11(): Bool = Set.union(Set#{6, 5}, Set#{-1, 9}) == Set#{6, 5, -1, 9}

@test
def union12(): Bool = Set.union(Set#{6, -99}, Set#{6, 5, -1, 9, 43, 7, 8, -99}) == Set#{6, 5, -1, 9, 43, 7, 8, -99}

@test
def union13(): Bool = Set.union(Set#{6, -99, -1, 5, 22}, Set#{6, 5, -1, 9, 43, 7, 8, -99}) == Set#{22, 6, 5, -1, 9, 43, 7, 8, -99}

@test
def union14(): Bool = Set.union(Set#{-2, -3, -4}, Set#{6, 5, -1, 9, 43, 7, 8, -99}) == Set#{-2, -3, -4, 6, 5, -1, 9, 43, 7, 8, -99}

/////////////////////////////////////////////////////////////////////////////
// intersection                                                            //
/////////////////////////////////////////////////////////////////////////////
@test
def intersection01(): Bool = Set.intersection(Set#{}: Set[Unit], Set#{}) == Set#{}

@test
def intersection02(): Bool = Set.intersection(Set#{1}, Set#{}) == Set#{}

@test
def intersection03(): Bool = Set.intersection(Set#{}, Set#{2}) == Set#{}

@test
def intersection04(): Bool = Set.intersection(Set#{1}, Set#{2}) == Set#{}

@test
def intersection05(): Bool = Set.intersection(Set#{1}, Set#{1}) == Set#{1}

@test
def intersection06(): Bool = Set.intersection(Set#{}, Set#{1, 2}) == Set#{}

@test
def intersection07(): Bool = Set.intersection(Set#{1, 2}, Set#{}) == Set#{}

@test
def intersection08(): Bool = Set.intersection(Set#{2}, Set#{1, 2}) == Set#{2}

@test
def intersection09(): Bool = Set.intersection(Set#{1}, Set#{1, 2}) == Set#{1}

@test
def intersection10(): Bool = Set.intersection(Set#{2, 1}, Set#{1, 2}) == Set#{2, 1}

@test
def intersection11(): Bool = Set.intersection(Set#{1, 2}, Set#{1, 2}) == Set#{1, 2}

@test
def intersection12(): Bool = Set.intersection(Set#{3, 2}, Set#{1, 2}) == Set#{2}

@test
def intersection13(): Bool = Set.intersection(Set#{3, 55}, Set#{1, 2}) == Set#{}

@test
def intersection14(): Bool = Set.intersection(Set#{3, 55, 11, 87, 22, 34, -87, 23}, Set#{1, 2, 84, -87, 87, 3, 44}) == Set#{3, 87, -87}

@test
def intersection15(): Bool = Set.intersection(Set#{3, 55, 11, 87, 22, 34, -87, 23}, Set#{23, 1, 2, 84, 87, 3}) == Set#{3, 87, 23}

/////////////////////////////////////////////////////////////////////////////
// difference                                                              //
/////////////////////////////////////////////////////////////////////////////
@test
def difference01(): Bool = Set.difference(Set#{}: Set[Unit], Set#{}) == Set#{}

@test
def difference02(): Bool = Set.difference(Set#{}, Set#{2}) == Set#{}

@test
def difference03(): Bool = Set.difference(Set#{1}, Set#{}) == Set#{1}

@test
def difference04(): Bool = Set.difference(Set#{1}, Set#{2}) == Set#{1}

@test
def difference05(): Bool = Set.difference(Set#{1}, Set#{1}) == Set#{}

@test
def difference06(): Bool = Set.difference(Set#{1, 2}, Set#{}) == Set#{1, 2}

@test
def difference07(): Bool = Set.difference(Set#{1, 2}, Set#{1}) == Set#{2}

@test
def difference08(): Bool = Set.difference(Set#{1, 2}, Set#{2}) == Set#{1}

@test
def difference09(): Bool = Set.difference(Set#{1, 2}, Set#{8, 2, 4}) == Set#{1}

@test
def difference10(): Bool = Set.difference(Set#{1, 2}, Set#{3, 1, 2, 4}) == Set#{}

@test
def difference11(): Bool = Set.difference(Set#{1, 2}, Set#{3, 11, 21, 4}) == Set#{1, 2}

@test
def difference12(): Bool = Set.difference(Set#{1, 2, 87, 4, 5, 6, 86, 92, 111, -1}, Set#{-1, 92, 4, 5, 1, 2, 86}) == Set#{87, 6, 111}

@test
def difference13(): Bool = Set.difference(Set#{1, 2, 87, 4, 5, 6, 86, 92, 111, -1}, Set#{-1, 92, 4, 5, 1, 2, 86, 99, 6}) == Set#{87, 111}

/////////////////////////////////////////////////////////////////////////////
// subsets                                                                 //
/////////////////////////////////////////////////////////////////////////////
@test
def subsets01(): Bool = Set.subsets(Set#{}: Set[Unit]) == Set#{Set#{}}

@test
def subsets02(): Bool = Set.subsets(Set#{1}) == Set#{Set#{1}, Set#{}}

@test
def subsets03(): Bool = Set.subsets(Set#{1, 2}) == Set#{Set#{1, 2}, Set#{1}, Set#{2}, Set#{}}

@test
def subsets04(): Bool = Set.subsets(Set#{1, 2, 3}) == Set#{Set#{1, 2, 3}, Set#{1, 2}, Set#{1, 3}, Set#{1}, Set#{2, 3}, Set#{2}, Set#{3}, Set#{}}

/////////////////////////////////////////////////////////////////////////////
// filter                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def filter01(): Bool = Set.filter(x -> x `Int32.rem` 2 == 0, Set#{}) == Set#{}

@test
def filter02(): Bool = Set.filter(x -> x `Int32.rem` 2 == 0, Set#{1}) == Set#{}

@test
def filter03(): Bool = Set.filter(x -> x `Int32.rem` 2 == 0, Set#{2}) == Set#{2}

@test
def filter04(): Bool = Set.filter(x -> x `Int32.rem` 2 == 0, Set#{1, 3}) == Set#{}

@test
def filter05(): Bool = Set.filter(x -> x `Int32.rem` 2 == 0, Set#{8, 3}) == Set#{8}

@test
def filter06(): Bool = Set.filter(x -> x `Int32.rem` 2 == 0, Set#{-1, 32}) == Set#{32}

@test
def filter07(): Bool = Set.filter(x -> x `Int32.rem` 2 == 0, Set#{12, 34}) == Set#{12, 34}

@test
def filter08(): Bool = Set.filter(x -> x `Int32.rem` 2 == 0, Set#{-33, -1, 12, 1, 34, 88, 7, 77, 31}) == Set#{12, 34, 88}

@test
def filter09(): Bool = Set.filter(x -> x `Int32.rem` 2 == 0, Set#{-33, -1, 12, 1, 34, 88, 7, 77, 31, 7, -92, 841}) == Set#{12, 34, 88, -92}

/////////////////////////////////////////////////////////////////////////////
// map                                                                     //
/////////////////////////////////////////////////////////////////////////////
@test
def map01(): Bool = Set.map(x -> x `Int32.rem` 2 == 0, Set#{}) == Set#{}

@test
def map02(): Bool = Set.map(x -> x `Int32.rem` 2 == 0, Set#{1}) == Set#{false}

@test
def map03(): Bool = Set.map(x -> x `Int32.rem` 2 == 0, Set#{2}) == Set#{true}

@test
def map04(): Bool = Set.map(x -> x `Int32.rem` 2 == 0, Set#{1, -1}) == Set#{false}

@test
def map05(): Bool = Set.map(x -> x `Int32.rem` 2 == 0, Set#{1, -12}) == Set#{false, true}

@test
def map06(): Bool = Set.map(x -> x `Int32.rem` 2 == 0, Set#{16, -1}) == Set#{true, false}

@test
def map07(): Bool = Set.map(x -> x `Int32.rem` 2 == 0, Set#{12, -12}) == Set#{true}

@test
def map08(): Bool = Set.map(x -> x `Int32.rem` 2 == 0, Set#{12, -12}) == Set#{true}

@test
def map09(): Bool = Set.map(x -> x `Int32.rem` 2 == 0, Set#{12, -12, 1, 14}) == Set#{false, true}

@test
def map10(): Bool = Set.map(x -> x `Int32.rem` 2 == 0, Set#{12, -12, 1, 14, 7, 88, -91}) == Set#{true, false}

@test
def map11(): Bool = Set.map(x -> x `Int32.rem` 2 == 0, Set#{12, -12, 122, 14}) == Set#{true}

@test
def map12(): Bool = Set.map(x -> x `Int32.rem` 2 == 0, Set#{123, -123, 1223, 141}) == Set#{false}

@test
def map13(): Bool = Set.map(x -> x `Int32.rem` 9, Set#{11, 5, 16, 4}) == Set#{2, 5, 7, 4}

@test
def map14(): Bool = Set.map(x -> x `Int32.rem` 9, Set#{0, 5, 1, -9, -8}) == Set#{5, 1, 0, -8}

@test
def map15(): Bool = Set.map(x -> x `Int32.rem` 9, Set#{0, 5, 1, 10, 7, 19, 28, 2}) == Set#{0, 5, 7, 1, 2}

@test
def map16(): Bool = region rc {
    let range = List.range(0, 2000);
    let s = List.toSet(range);
    let a = MutList.new(rc);
    discard Set.map(x -> { let b = MutList.new(rc); MutList.push!(x, b); MutList.append!(MutList.toList(b), a); x }, s);
    MutList.toList(a) == range
}


/////////////////////////////////////////////////////////////////////////////
// flatMap                                                                 //
/////////////////////////////////////////////////////////////////////////////
@test
def flatMap01(): Bool = Set.flatMap(x -> if (x `Int32.rem` 2 == 0) Set#{} else Set#{x}, Set#{}) == Set#{}

@test
def flatMap02(): Bool = Set.flatMap(x -> if (x `Int32.rem` 2 == 0) Set#{} else Set#{x}, Set#{2}) == Set#{}

@test
def flatMap03(): Bool = Set.flatMap(x -> if (x `Int32.rem` 2 == 0) Set#{} else Set#{x}, Set#{1}) == Set#{1}

@test
def flatMap04(): Bool = Set.flatMap(x -> if (x `Int32.rem` 2 == 0) Set#{} else Set#{x}, Set#{2, 4}) == Set#{}

@test
def flatMap05(): Bool = Set.flatMap(x -> if (x `Int32.rem` 2 == 0) Set#{} else Set#{x}, Set#{2, 13}) == Set#{13}

@test
def flatMap06(): Bool = Set.flatMap(x -> if (x `Int32.rem` 2 == 0) Set#{} else Set#{x}, Set#{15, -8}) == Set#{15}

@test
def flatMap07(): Bool = Set.flatMap(x -> if (x `Int32.rem` 2 == 0) Set#{} else Set#{x}, Set#{1, 11}) == Set#{1, 11}

@test
def flatMap08(): Bool = Set.flatMap(x -> Set#{x, 2*x}, Set#{1, 4, 8, 2}) == Set#{1, 8, 16, 2, 4}

@test
def flatMap09(): Bool = Set.flatMap(x -> Set#{x, 3*x}, Set#{1, 8, 3, 2, 9, -5, -1}) == Set#{1, 8, 24, 3, 2, 6, 9, 27, -5, -15, -1, -3}

@test
def flatMap10(): Bool = Set.flatMap(x -> Set#{x, 2*x, 3*x}, Set#{1, 4, 3, 2}) == Set#{1, 8, 12, 3, 9, 2, 4, 6}

/////////////////////////////////////////////////////////////////////////////
// filterMap                                                               //
/////////////////////////////////////////////////////////////////////////////

@test
def filterMap01(): Bool =
    Set.filterMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Set.empty()) == Set.empty()

@test
def filterMap02(): Bool =
    Set.filterMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Set#{1}) == Set#{}

@test
def filterMap03(): Bool =
    Set.filterMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Set#{2}) == Set#{1}

@test
def filterMap04(): Bool =
    Set.filterMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Set#{1, 3}) == Set#{}

@test
def filterMap05(): Bool =
    Set.filterMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Set#{1, 4}) == Set#{2}

@test
def filterMap06(): Bool =
    Set.filterMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Set#{-1, 6}) == Set#{3}

@test
def filterMap07(): Bool =
    Set.filterMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Set#{6, 8}) == Set#{3, 4}

@test
def filterMap08(): Bool =
    let s1 = Set#{0, 1, 2, 3, 4, 5, 10};
    let s2 = Set#{0, 1, 2, 5};
    Set.filterMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, s1) == s2

/////////////////////////////////////////////////////////////////////////////
// replace                                                                 //
/////////////////////////////////////////////////////////////////////////////
@test
def replace01(): Bool = Set.replace(from = 3, to = 4, Set#{}) == Set#{}

@test
def replace02(): Bool = Set.replace(from = 3, to = 4, Set#{1}) == Set#{1}

@test
def replace03(): Bool = Set.replace(from = 3, to = 4, Set#{3}) == Set#{4}

@test
def replace04(): Bool = Set.replace(from = 3, to = 4, Set#{4}) == Set#{4}

@test
def replace05(): Bool = Set.replace(from = 3, to = 4, Set#{1, 2}) == Set#{1, 2}

@test
def replace06(): Bool = Set.replace(from = 3, to = 4, Set#{1, 3}) == Set#{1, 4}

@test
def replace07(): Bool = Set.replace(from = 3, to = 4, Set#{3, 2}) == Set#{4, 2}

@test
def replace08(): Bool = Set.replace(from = 3, to = 4, Set#{3, 4}) == Set#{4}

/////////////////////////////////////////////////////////////////////////////
// partition                                                               //
/////////////////////////////////////////////////////////////////////////////
@test
def partition01(): Bool = Set.partition(x -> x `Int32.rem` 2 == 0, Set#{}) == (Set#{}, Set#{})

@test
def partition02(): Bool = Set.partition(x -> x `Int32.rem` 2 == 0, Set#{1}) == (Set#{}, Set#{1})

@test
def partition03(): Bool = Set.partition(x -> x `Int32.rem` 2 == 0, Set#{2}) == (Set#{2}, Set#{})

@test
def partition04(): Bool = Set.partition(x -> x `Int32.rem` 2 == 0, Set#{1, 3}) == (Set#{}, Set#{1, 3})

@test
def partition05(): Bool = Set.partition(x -> x `Int32.rem` 2 == 0, Set#{1, 2}) == (Set#{2}, Set#{1})

@test
def partition06(): Bool = Set.partition(x -> x `Int32.rem` 2 == 0, Set#{2, 1}) == (Set#{2}, Set#{1})

@test
def partition07(): Bool = Set.partition(x -> x `Int32.rem` 2 == 0, Set#{2, -4}) == (Set#{2, -4}, Set#{})

@test
def partition08(): Bool = Set.partition(x -> x `Int32.rem` 2 == 0, Set#{2, -11, 89, -4, 11, -6, 84}) == (Set#{2, -4, -6, 84}, Set#{-11, 89, 11})

@test
def partition09(): Bool = Set.partition(x -> x `Int32.rem` 2 == 0, Set#{84, -6, 11, -4, 89, -11, 2}) == (Set#{84, -6, -4, 2}, Set#{11, 89, -11})

@test
def partition10(): Bool = Set.partition(x -> x `Int32.rem` 2 == 0, Set#{1, 2, 3, 4, 5, 6, 7, 8}) == (Set#{2, 4, 6, 8}, Set#{1, 3, 5, 7})

/////////////////////////////////////////////////////////////////////////////
// toList                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def toList01(): Bool = Set.toList(Set#{}: Set[Unit]) == Nil

@test
def toList02(): Bool = Set.toList(Set#{1}) == 1 :: Nil

@test
def toList03(): Bool = Set.toList(Set#{1, 2}) == 1 :: 2 :: Nil

@test
def toList04(): Bool = Set.toList(Set#{1, 2, 3}) == 1 :: 2 :: 3 :: Nil

/////////////////////////////////////////////////////////////////////////////
// toChain                                                                 //
/////////////////////////////////////////////////////////////////////////////

@test
def toChain01(): Bool =
    Set.toChain(Set#{}: Set[Int32]) == Chain.empty(): Chain[Int32]

@test
def toChain02(): Bool =
    Set.toChain(Set#{1}) == Chain.singleton(1)

@test
def toChain03(): Bool =
    Set.toChain(Set#{1, 2}) == List.toChain(1 :: 2 :: Nil)

@test
def toChain04(): Bool =
    Set.toChain(Set#{1, 2, 3}) == List.toChain(1 :: 2 :: 3 :: Nil)

/////////////////////////////////////////////////////////////////////////////
// toMap                                                                   //
/////////////////////////////////////////////////////////////////////////////
@test
def toMap01(): Bool = Set.toMap(Set#{}: Set[(Unit, Unit)]) == Map#{}

@test
def toMap02(): Bool = Set.toMap(Set#{(1, true)}) == Map#{1 => true}

@test
def toMap03(): Bool = Set.toMap(Set#{(1, true), (2, false)}) == Map#{1 => true, 2 => false}

@test
def toMap04(): Bool = Set.toMap(Set#{(1, true), (1, false)}) == Map#{1 => false}

@test
def toMap05(): Bool = Set.toMap(Set#{(1, true), (2, false), (3, true)}) == Map#{1 => true, 2 => false, 3 => true}


/////////////////////////////////////////////////////////////////////////////
// toMapWith                                                               //
/////////////////////////////////////////////////////////////////////////////

@test
def toMapWith02(): Bool =
    Set.toMapWith(x -> 0 * x, Set#{}) == Map#{}

@test
def toMapWith03(): Bool =
    Set.toMapWith(x -> 2 * x, Set#{1, 3, 6}) == Map#{1 => 2, 3 => 6, 6 => 12}

@test
def toMapWith04(): Bool =
    Set.toMapWith(x -> x + 10, Set#{1, 3, 6}) == Map#{1 => 11, 3 => 13, 6 => 16}

@test
def toMapWith05(): Bool =
    Set.toMapWith(x -> x + 10, Set#{7, 1, 3, 6}) == Map#{1 => 11, 3 => 13, 6 => 16, 7 => 17}

/////////////////////////////////////////////////////////////////////////////
// toMutDeque                                                              //
/////////////////////////////////////////////////////////////////////////////

@test
def toMutDeque01(): Bool = region rc {
    let s: Set[Int32] = Set#{};
    let d1 = Set.toMutDeque(rc, s);

    let d2 = MutDeque.new(rc);

    d1 `MutDeque.sameElements` d2
}

@test
def toMutDeque02(): Bool = region rc {
    let s = Set#{1};
    let d1 = Set.toMutDeque(rc, s);

    let d2 = MutDeque.new(rc);
    MutDeque.pushBack(1, d2);

    d1 `MutDeque.sameElements` d2
}

@test
def toMutDeque03(): Bool = region rc {
    let s = Set#{1, 3, 6};
    let d1 = Set.toMutDeque(rc, s);

    let d2 = MutDeque.new(rc);
    MutDeque.pushBack(3, d2);
    MutDeque.pushBack(6, d2);
    MutDeque.pushFront(1, d2);

    d1 `MutDeque.sameElements` d2
}

@test
def toMutDeque04(): Bool = region rc {
    let s = Set#{7, 1, 3, 6};
    let d1 = Set.toMutDeque(rc, s);

    let d2 = MutDeque.new(rc);
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
def eq01(): Bool = Set#{}: Set[Unit] == Set#{}

@test
def eq02(): Bool = Set#{1} != Set#{}

@test
def eq03(): Bool = Set#{} != Set#{1}

@test
def eq04(): Bool = Set#{1, 2} != Set#{}

@test
def eq05(): Bool = Set#{} != Set#{1, 2}

@test
def eq06(): Bool = Set#{1} != Set#{2}

@test
def eq07(): Bool = Set#{1} == Set#{1}

@test
def eq08(): Bool = Set#{1, 2, 3} != Set#{}

@test
def eq09(): Bool = Set#{1, 2} != Set#{1}

@test
def eq10(): Bool = Set#{1} != Set#{1, 2}

@test
def eq11(): Bool = Set#{} != Set#{1, 2, 3}

@test
def eq12(): Bool = Set#{1, 2} != Set#{1, 3}

@test
def eq13(): Bool = Set#{1, 2} == Set#{2, 1}

@test
def eq14(): Bool = Set#{1, 2} == Set#{1, 2}

@test
def eq15(): Bool = Set#{1, 2, 3} != Set#{1, 2, 4}

@test
def eq16(): Bool = Set#{1, 2, 3} == Set#{1, 2, 3}

@test
def eq17(): Bool = Set#{1, 2, 3} == Set#{2, 3, 1}

@test
def eq18(): Bool = Set#{1, 2, 3} == Set#{3, 1, 2}

@test
def eq19(): Bool = Set#{1, 2, 3} == Set#{2, 1, 3}

/////////////////////////////////////////////////////////////////////////////
// forEach                                                                 //
/////////////////////////////////////////////////////////////////////////////

@test
def forEach01(): Bool = region rc {
    let ri = ref 21 @ rc;
    Set.forEach(x -> ri := x, Set#{});
    21 == deref ri
}

@test
def forEach02(): Bool = region rc {
    let ri = ref 21 @ rc;
    Set.forEach(x -> ri := x, Set#{42});
    42 == deref ri
}

/////////////////////////////////////////////////////////////////////////////
// forEachWithIndex                                                        //
/////////////////////////////////////////////////////////////////////////////

@test
def forEachWithIndex01(): Bool = region rc {
    let ri = ref 21 @ rc;
    Set.forEachWithIndex((i, _) -> ri := i, Set#{});
    21 == deref ri
}

@test
def forEachWithIndex02(): Bool = region rc {
    let ri = ref 21 @ rc;
    Set.forEachWithIndex((i, _) -> ri := i, Set#{42});
    0 == deref ri
}

/////////////////////////////////////////////////////////////////////////////
// unfold                                                                  //
/////////////////////////////////////////////////////////////////////////////

@test
def unfold01(): Bool =
    Set.unfold(s -> if (true) None else Some(s + 48, s + 1), 0) == Set#{}

@test
def unfold02(): Bool =
    Set.unfold(s -> if (s > 0) None else Some(s + 48, s + 1), 0) == Set#{48}

@test
def unfold03(): Bool =
    Set.unfold(s -> if (s > 1) None else Some(s + 48, s + 1), 0) == Set#{48, 49}

@test
def unfold04(): Bool =
    Set.unfold(s -> if (s >= 10) None else Some(s + 48, s + 1), 0) == Set#{48, 49, 50, 51, 52, 53, 54, 55, 56, 57}

@test
def unfold05(): Bool =
    Set.unfold(s -> if (s >= 10) None else Some(s + 48, s + 1), 5) == Set#{53, 54, 55, 56, 57}

@test
def unfold06(): Bool =
    Set.unfold(s -> if (s >= 10) None else Some(s + 48, s + 2), 0) == Set#{48, 50, 52, 54, 56}

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
            x := deref x + 1;
            c
        };
    Set.unfoldWithIter(step) == Set#{}
}

@test
def unfoldWithIter02(): Bool = region rc {
    let x = ref 0 @ rc;
    let step = () ->
        if (deref x > 0)
            None
        else {
            let c = Some(deref x + 48);
            x := deref x + 1;
            c
        };
    Set.unfoldWithIter(step) == Set#{48}
}

@test
def unfoldWithIter03(): Bool = region rc {
    let x = ref 0 @ rc;
    let step = () ->
        if (deref x > 1)
            None
        else {
            let c = Some(deref x + 48);
            x := deref x + 1;
            c
        };
    Set.unfoldWithIter(step) == Set#{48, 49}
}

@test
def unfoldWithIter04(): Bool = region rc {
    let x = ref 0 @ rc;
    let step = () ->
        if (deref x >= 10)
            None
        else {
            let c = Some(deref x + 48);
            x := deref x + 1;
            c
        };
    Set.unfoldWithIter(step) == Set#{48, 49, 50, 51, 52, 53, 54, 55, 56, 57}
}

@test
def unfoldWithIter05(): Bool = region rc {
    let x = ref 5 @ rc;
    let step = () ->
        if (deref x >= 10)
            None
        else {
            let c = Some(deref x + 48);
            x := deref x + 1;
            c
        };
    Set.unfoldWithIter(step) == Set#{53, 54, 55, 56, 57}
}

@test
def unfoldWithIter06(): Bool = region rc {
    let x = ref 0 @ rc;
    let step = () ->
        if (deref x >= 10)
            None
        else {
            let c = Some(deref x + 48);
            x := deref x + 2;
            c
        };
    Set.unfoldWithIter(step) == Set#{48, 50, 52, 54, 56}
}

    /////////////////////////////////////////////////////////////////////////////
    // toString                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toString01(): Bool =
        toString(Set#{1}) == "Set#{1}"

    @test
    def toString02(): Bool =
        toString(Set#{1, 2, 3}) == "Set#{1, 2, 3}"

    @test
    def toString03(): Bool =
        toString(Set#{1, 2}) == "Set#{1, 2}"

    @test
    def toString04(): Bool =
        toString(Set#{97, 2, 3, 4, 0}) == "Set#{0, 2, 3, 4, 97}"

    @test
    def toString05(): Bool =
        toString(Set#{Set#{1, 2}, Set#{4, 6}}) == "Set#{Set#{1, 2}, Set#{4, 6}}"

    /////////////////////////////////////////////////////////////////////////////
    // hash                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def hash01(): Bool = hash(Set#{}: Set[Unit]) == hash(Set#{}: Set[Unit])

    @test
    def hash02(): Bool = hash(Set#{1, 2}) == hash(Set#{1, 2})

    @test
    def hash03(): Bool = hash(Set#{'a', 'b', 'c', 'd'}) == hash(Set#{'a', 'b', 'c', 'd'})

    @test
    def hash04(): Bool = hash(Set#{'a', 'b', 'c', 'e'}) != hash(Set#{'a', 'b', 'c', 'd'})

    @test
    def hash05(): Bool = hash(Set#{false}) != hash(Set#{true})

    /////////////////////////////////////////////////////////////////////////////
    // minimum                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def minimum01(): Bool = Set.minimum(Set#{}: Set[Int32]) == None

    @test
    def minimum02(): Bool = Set.minimum(Set#{1}) == Some(1)

    @test
    def minimum03(): Bool = Set.minimum(Set#{1, 2, 3, 0}) == Some(0)

    @test
    def minimum04(): Bool = Set.minimum(Set.range(5, 55)) == Some(5)

    /////////////////////////////////////////////////////////////////////////////
    // minimumBy                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def minimumBy01(): Bool = Set.minimumBy((x, y) -> x <=> y, Set#{}: Set[Int32]) == None

    @test
    def minimumBy02(): Bool = Set.minimumBy((x, y) -> x <=> y, Set#{1}) == Some(1)

    @test
    def minimumBy03(): Bool = Set.minimumBy((x, y) -> x <=> y, Set#{1, 2, 3, 0}) == Some(0)

    @test
    def minimumBy04(): Bool = Set.minimumBy((x, y) -> x <=> y, Set.range(5, 55)) == Some(5)

    @test
    def minimumBy05(): Bool = region rc {
        let s = List.range(0, 2000) |> List.toSet;
        let a = MutList.new(rc);
        discard Set.minimumBy((x, y) -> { let b = MutList.new(rc); MutList.push!(y, b); MutList.append!(MutList.toList(b), a); x <=> y }, s);
        MutList.toList(a) == List.range(1, 2000) // The first / left most value is ignored
    }


    /////////////////////////////////////////////////////////////////////////////
    // maximum                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def maximum01(): Bool = Set.maximum(Set#{}: Set[Int32]) == None

    @test
    def maximum02(): Bool = Set.maximum(Set#{1}) == Some(1)

    @test
    def maximum03(): Bool = Set.maximum(Set#{1, 2, 3, 0}) == Some(3)

    @test
    def maximum04(): Bool = Set.maximum(Set.range(5, 55)) == Some(54)

    /////////////////////////////////////////////////////////////////////////////
    // maximumBy                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def maximumBy01(): Bool = Set.maximumBy((x, y) -> x <=> y, Set#{}: Set[Int32]) == None

    @test
    def maximumBy02(): Bool = Set.maximumBy((x, y) -> x <=> y, Set#{1}) == Some(1)

    @test
    def maximumBy03(): Bool = Set.maximumBy((x, y) -> x <=> y, Set#{1, 2, 3, 0}) == Some(3)

    @test
    def maximumBy04(): Bool = Set.maximumBy((x, y) -> x <=> y, Set.range(5, 55)) == Some(54)


    /////////////////////////////////////////////////////////////////////////////
    // toDelayList                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toDelayList01(): Bool =
        Set.empty(): Set[Unit] |> Set.toDelayList == DelayList.empty()

    @test
    def toDelayList02(): Bool =
        Set.range(-1000, 1000) |> Set.toDelayList == DelayList.range(-1000, 1000)

    @test
    def toDelayList03(): Bool =
        Set.range(-1000, 1000) |> Set.toDelayList |> DelayList.toSet == Set.range(-1000, 1000)


    /////////////////////////////////////////////////////////////////////////////
    // iterator                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def iterator01(): Bool = region rc {
        Set.empty(): Set[Int32] |> Set.iterator(rc) |> Iterator.toSet == Set.empty()
    }

    @test
    def iterator02(): Bool = region rc {
        Set#{1, 2, 3, 4} |> Set.iterator(rc) |> Iterator.toSet == Set#{1, 2, 3, 4}
    }

    @test
    def iterator03(): Bool = region rc {
        Set.range(0, 100) |> Set.iterator(rc) |> Iterator.toSet == Set.range(0, 100)
    }


    /////////////////////////////////////////////////////////////////////////////
    // join                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def join01(): Bool =
        Set.join(",", Set.empty(): Set[Int32]) == ""

    @test
    def join02(): Bool =
        Set.join(",", Set#{1}) == "1"

    @test
    def join03(): Bool =
        Set.join(",", Set#{1, 2, 3}) == "1,2,3"

    @test
    def join04(): Bool =
        Set.join(",", Set#{"1", "2", "3"}) == "1,2,3"


    /////////////////////////////////////////////////////////////////////////////
    // joinWith                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def joinWith01(): Bool =
        (Nil: List[Int32]) |> List.toSet |>
            Set.joinWith(x -> "${x + 1}", ",") == ""

    @test
    def joinWith02(): Bool =
        (1 :: Nil) |> List.toSet |>
            Set.joinWith(x -> "${x + 1}", ",") == "2"

    @test
    def joinWith03(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List.toSet |>
            Set.joinWith(x -> "${x + 1}", ",") == "2,3,4"

    @test
    def joinWith04(): Bool =
        ("1" :: "2" :: "3" :: Nil) |> List.toSet |>
            Set.joinWith(x -> x + x, ",") == "11,22,33"

}

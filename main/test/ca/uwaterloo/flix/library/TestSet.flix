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
@Test
def collect01(): Bool = region rc {
    Collectable.collect(Iterator.empty(rc): Iterator[Int32, rc, rc]) == Set.empty()
}

@Test
def collect02(): Bool = region rc {
    Collectable.collect(Iterator.singleton(rc, 1)) == Set#{1}
}

@Test
def collect03(): Bool = region rc {
    Collectable.collect(Iterator.range(rc, 1, 3)) == Set#{1, 2}
}

/////////////////////////////////////////////////////////////////////////////
// size                                                                    //
/////////////////////////////////////////////////////////////////////////////
@Test
def size01(): Bool = Set.size(Set#{}) == 0

@Test
def size02(): Bool = Set.size(Set#{1}) == 1

@Test
def size03(): Bool = Set.size(Set#{1, 2}) == 2

@Test
def size04(): Bool = Set.size(Set#{1, 2, 3}) == 3

@Test
def size05(): Bool = Set.size(Set#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 14}) == 13

/////////////////////////////////////////////////////////////////////////////
// empty                                                                   //
/////////////////////////////////////////////////////////////////////////////
@Test
def empty01(): Bool = Set.empty(): Set[Unit] == Set#{}

/////////////////////////////////////////////////////////////////////////////
// singleton                                                               //
/////////////////////////////////////////////////////////////////////////////
@Test
def singleton01(): Bool = Set.singleton(1) == Set#{1}

@Test
def singleton02(): Bool = Set.singleton(2) == Set#{2}

@Test
def singleton03(): Bool = Set.singleton(true) == Set#{true}

@Test
def singleton04(): Bool = Set.singleton(false) == Set#{false}

/////////////////////////////////////////////////////////////////////////////
// insert                                                                  //
/////////////////////////////////////////////////////////////////////////////
@Test
def insert01(): Bool = Set.insert(1, Set#{}) == Set#{1}

@Test
def insert02(): Bool = Set.insert(1, Set#{1}) == Set#{1}

@Test
def insert03(): Bool = Set.insert(1, Set#{2}) == Set#{1, 2}

@Test
def insert04(): Bool = Set.insert(1, Set#{2, 3}) == Set#{1, 2, 3}

@Test
def insert05(): Bool = Set.insert(1, Set#{1, 2, 3}) == Set#{1, 2, 3}

@Test
def insert06(): Bool = Set.insert(1, Set#{2, 1, 3}) == Set#{2, 1, 3}

@Test
def insert07(): Bool = Set.insert(1, Set#{2, 1, 3, 7, 8, 9, 10}) == Set#{2, 1, 3, 7, 8, 9, 10}

@Test
def insert08(): Bool = Set.insert(11, Set#{2, 1, 3, 7, 8, 9, 10, 35}) == Set#{11, 2, 1, 3, 7, 8, 9, 10, 35}

/////////////////////////////////////////////////////////////////////////////
// remove                                                                  //
/////////////////////////////////////////////////////////////////////////////
@Test
def remove01(): Bool = Set.remove(1, Set#{}) == Set#{}

@Test
def remove02(): Bool = Set.remove(1, Set#{1}) == Set#{}

@Test
def remove03(): Bool = Set.remove(1, Set#{2}) == Set#{2}

@Test
def remove04(): Bool = Set.remove(2, Set#{2, 3}) == Set#{3}

@Test
def remove05(): Bool = Set.remove(3, Set#{2, 3}) == Set#{2}

@Test
def remove06(): Bool = Set.remove(1, Set#{2, 3}) == Set#{2, 3}

@Test
def remove07(): Bool = Set.remove(0, Set#{1, 2, 3, 4, 5, 6, 7}) == Set#{1, 2, 3, 4, 5, 6, 7}

@Test
def remove08(): Bool = Set.remove(1, Set#{1, 2, 3, 4, 5, 6, 7}) == Set#{2, 3, 4, 5, 6, 7}

@Test
def remove09(): Bool = Set.remove(2, Set#{1, 2, 3, 4, 5, 6, 7}) == Set#{1, 3, 4, 5, 6, 7}

@Test
def remove10(): Bool = Set.remove(6, Set#{1, 2, 3, 4, 5, 6, 7}) == Set#{1, 2, 3, 4, 5, 7}

@Test
def remove11(): Bool = Set.remove(7, Set#{1, 2, 3, 4, 5, 6, 7}) == Set#{1, 2, 3, 4, 5, 6}

@Test
def remove12(): Bool = Set.remove(8, Set#{1, 2, 3, 4, 5, 6, 7}) == Set#{1, 2, 3, 4, 5, 6, 7}

/////////////////////////////////////////////////////////////////////////////
// isEmpty                                                                 //
/////////////////////////////////////////////////////////////////////////////
@Test
def isEmpty01(): Bool = Set.isEmpty(Set#{}: Set[Unit]) == true

@Test
def isEmpty02(): Bool = Set.isEmpty(Set#{1}) == false

@Test
def isEmpty03(): Bool = Set.isEmpty(Set#{1, 2}) == false

@Test
def isEmpty04(): Bool = Set.isEmpty(Set#{1, 2, 3}) == false

@Test
def isEmpty05(): Bool = Set.isEmpty(Set#{1, 2, 3, 4, 5, 6, 7, 8}) == false

/////////////////////////////////////////////////////////////////////////////
// memberOf                                                                //
/////////////////////////////////////////////////////////////////////////////
@Test
def memberOf01(): Bool = Set.memberOf(1, Set#{}) == false

@Test
def memberOf02(): Bool = Set.memberOf(1, Set#{2}) == false

@Test
def memberOf03(): Bool = Set.memberOf(1, Set#{1}) == true

@Test
def memberOf04(): Bool = Set.memberOf(1, Set#{1, 2}) == true

@Test
def memberOf05(): Bool = Set.memberOf(2, Set#{1, 2}) == true

@Test
def memberOf06(): Bool = Set.memberOf(0, Set#{1, 2}) == false

@Test
def memberOf07(): Bool = Set.memberOf(3, Set#{1, 2}) == false

@Test
def memberOf08(): Bool = Set.memberOf(0, Set#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}) == false

@Test
def memberOf09(): Bool = Set.memberOf(1, Set#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}) == true

@Test
def memberOf10(): Bool = Set.memberOf(2, Set#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}) == true

@Test
def memberOf11(): Bool = Set.memberOf(10, Set#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}) == true

@Test
def memberOf12(): Bool = Set.memberOf(12, Set#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}) == false

/////////////////////////////////////////////////////////////////////////////
// find                                                                    //
/////////////////////////////////////////////////////////////////////////////
@Test
def find01(): Bool = Set.find(i -> i > 2, Set#{}) == None

@Test
def find02(): Bool = Set.find(i -> i > 2, Set#{1}) == None

@Test
def find03(): Bool = Set.find(i -> i > 2, Set#{3}) == Some(3)

@Test
def find04(): Bool = Set.find(i -> i > 2, Set#{2, 1}) == None

@Test
def find05(): Bool = Set.find(i -> i > 2, Set#{-6, 6}) == Some(6)

@Test
def find06(): Bool = Set.find(i -> i > 2, Set#{6, -6}) == Some(6)

@Test
def find07(): Bool = Set.find(i -> i > 2, Set#{7, 6}) == Some(6)

/////////////////////////////////////////////////////////////////////////////
// findLeft                                                                //
/////////////////////////////////////////////////////////////////////////////
@Test
def findLeft01(): Bool = Set.findLeft(i -> i > 2, Set#{}) == None

@Test
def findLeft02(): Bool = Set.findLeft(i -> i > 2, Set#{1}) == None

@Test
def findLeft03(): Bool = Set.findLeft(i -> i > 2, Set#{3}) == Some(3)

@Test
def findLeft04(): Bool = Set.findLeft(i -> i > 2, Set#{2, 1}) == None

@Test
def findLeft05(): Bool = Set.findLeft(i -> i > 2, Set#{-6, 6}) == Some(6)

@Test
def findLeft06(): Bool = Set.findLeft(i -> i > 2, Set#{6, -6}) == Some(6)

@Test
def findLeft07(): Bool = Set.findLeft(i -> i > 2, Set#{7, 6}) == Some(6)

/////////////////////////////////////////////////////////////////////////////
// findRight                                                               //
/////////////////////////////////////////////////////////////////////////////
@Test
def findRight01(): Bool = Set.findRight(i -> i > 2, Set#{}) == None

@Test
def findRight02(): Bool = Set.findRight(i -> i > 2, Set#{1}) == None

@Test
def findRight03(): Bool = Set.findRight(i -> i > 2, Set#{3}) == Some(3)

@Test
def findRight04(): Bool = Set.findRight(i -> i > 2, Set#{2, 1}) == None

@Test
def findRight05(): Bool = Set.findRight(i -> i > 2, Set#{-6, 6}) == Some(6)

@Test
def findRight06(): Bool = Set.findRight(i -> i > 2, Set#{6, -6}) == Some(6)

@Test
def findRight07(): Bool = Set.findRight(i -> i > 2, Set#{7, 6}) == Some(7)

/////////////////////////////////////////////////////////////////////////////
// isSubsetOf                                                              //
/////////////////////////////////////////////////////////////////////////////
@Test
def isSubsetOf01(): Bool = Set.isSubsetOf(Set#{}: Set[Unit], Set#{}) == true

@Test
def isSubsetOf02(): Bool = Set.isSubsetOf(Set#{}, Set#{1}) == true

@Test
def isSubsetOf03(): Bool = Set.isSubsetOf(Set#{1}, Set#{1}) == true

@Test
def isSubsetOf04(): Bool = Set.isSubsetOf(Set#{1}, Set#{2}) == false

@Test
def isSubsetOf05(): Bool = Set.isSubsetOf(Set#{}, Set#{1, 2}) == true

@Test
def isSubsetOf06(): Bool = Set.isSubsetOf(Set#{1}, Set#{1, 2}) == true

@Test
def isSubsetOf07(): Bool = Set.isSubsetOf(Set#{2}, Set#{1, 2}) == true

@Test
def isSubsetOf08(): Bool = Set.isSubsetOf(Set#{3}, Set#{1, 2}) == false

@Test
def isSubsetOf09(): Bool = Set.isSubsetOf(Set#{1, 2}, Set#{1, 2}) == true

@Test
def isSubsetOf10(): Bool = Set.isSubsetOf(Set#{2, 1}, Set#{1, 2}) == true

@Test
def isSubsetOf11(): Bool = Set.isSubsetOf(Set#{3, 1}, Set#{1, 2}) == false

@Test
def isSubsetOf12(): Bool = Set.isSubsetOf(Set#{1, 2, 3}, Set#{1, 2}) == false

@Test
def isSubsetOf13(): Bool = Set.isSubsetOf(Set#{10}, Set#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}) == true

@Test
def isSubsetOf14(): Bool = Set.isSubsetOf(Set#{9, 1}, Set#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}) == true

@Test
def isSubsetOf15(): Bool = Set.isSubsetOf(Set#{6, 5, 8}, Set#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}) == true

@Test
def isSubsetOf16(): Bool = Set.isSubsetOf(Set#{10, 2, 3, 6, -1, 4}, Set#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}) == false

/////////////////////////////////////////////////////////////////////////////
// isProperSubsetOf                                                        //
/////////////////////////////////////////////////////////////////////////////
@Test
def isProperSubsetOf01(): Bool = Set.isProperSubsetOf(Set#{}: Set[Unit], Set#{}) == false

@Test
def isProperSubsetOf02(): Bool = Set.isProperSubsetOf(Set#{}, Set#{1}) == true

@Test
def isProperSubsetOf03(): Bool = Set.isProperSubsetOf(Set#{1}, Set#{1}) == false

@Test
def isProperSubsetOf04(): Bool = Set.isProperSubsetOf(Set#{1}, Set#{2}) == false

@Test
def isProperSubsetOf05(): Bool = Set.isProperSubsetOf(Set#{}, Set#{1, 2}) == true

@Test
def isProperSubsetOf06(): Bool = Set.isProperSubsetOf(Set#{1}, Set#{1, 2}) == true

@Test
def isProperSubsetOf07(): Bool = Set.isProperSubsetOf(Set#{2}, Set#{1, 2}) == true

@Test
def isProperSubsetOf08(): Bool = Set.isProperSubsetOf(Set#{3}, Set#{1, 2}) == false

@Test
def isProperSubsetOf09(): Bool = Set.isProperSubsetOf(Set#{1, 2}, Set#{1, 2}) == false

@Test
def isProperSubsetOf10(): Bool = Set.isProperSubsetOf(Set#{2, 1}, Set#{1, 2}) == false

@Test
def isProperSubsetOf11(): Bool = Set.isProperSubsetOf(Set#{3, 1}, Set#{1, 2}) == false

@Test
def isProperSubsetOf12(): Bool = Set.isProperSubsetOf(Set#{1, 2, 3}, Set#{1, 2}) == false

@Test
def isProperSubsetOf13(): Bool = Set.isProperSubsetOf(Set#{10}, Set#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}) == true

@Test
def isProperSubsetOf14(): Bool = Set.isProperSubsetOf(Set#{9, 1}, Set#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}) == true

@Test
def isProperSubsetOf15(): Bool = Set.isProperSubsetOf(Set#{6, 5, 8}, Set#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}) == true

@Test
def isProperSubsetOf16(): Bool = Set.isProperSubsetOf(Set#{10, 2, 3, 6, -1, 4}, Set#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}) == false

@Test
def isProperSubsetOf17(): Bool = Set.isProperSubsetOf(Set#{10, 2, 3, 6, 1, 4, 5, 9, 8, 7}, Set#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}) == false

    /////////////////////////////////////////////////////////////////////////////
    // fold                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def fold01(): Bool = Set.fold(Set#{}: Set[String]) == ""

    @Test
    def fold02(): Bool = Set.fold(Set#{}: Set[Unit]) == ()

    @Test
    def fold03(): Bool = Set.fold(Set#{"a", "b", "c"}) == "abc"

    @Test
    def fold04(): Bool = Set.fold(Set#{("a", "b"), ("c", "d")}) == ("ac", "bd")

/////////////////////////////////////////////////////////////////////////////
// foldLeft                                                                //
/////////////////////////////////////////////////////////////////////////////
@Test
def foldLeft01(): Bool = Set.foldLeft((i, e) -> (i - e)*(e `Int32.rem` 2 + 1), 100, Set#{}) == 100

@Test
def foldLeft02(): Bool = Set.foldLeft((i, e) -> (i - e)*(e `Int32.rem` 2 + 1), 100, Set#{1}) == 198

@Test
def foldLeft03(): Bool = Set.foldLeft((i, e) -> (i - e)*(e `Int32.rem` 2 + 1), 100, Set#{2, 1}) == 196

@Test
def foldLeft04(): Bool = Set.foldLeft((i, e) -> (i - e)*(e `Int32.rem` 2 + 1), 100, Set#{3, 2, 1}) == 386

/////////////////////////////////////////////////////////////////////////////
// foldRight                                                               //
/////////////////////////////////////////////////////////////////////////////
@Test
def foldRight01(): Bool = Set.foldRight((e, acc) -> (acc - e) * (e `Int32.rem` 2 + 1), 100, Set#{}) == 100

@Test
def foldRight02(): Bool = Set.foldRight((e, acc) -> (acc - e) * (e `Int32.rem` 2 + 1), 100, Set#{1}) == 198

@Test
def foldRight03(): Bool = Set.foldRight((e, acc) -> (acc - e) * (e `Int32.rem` 2 + 1), 100, Set#{2, 1}) == 194

@Test
def foldRight04(): Bool = Set.foldRight((e, acc) -> (acc - e) * (e `Int32.rem` 2 + 1), 100, Set#{3, 2, 1}) == 382

/////////////////////////////////////////////////////////////////////////////
// foldRightWithCont                                                       //
/////////////////////////////////////////////////////////////////////////////
@Test
def foldRightWithCont01(): Bool = Set.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.rem` 2 + 1), 100, Set#{}) == 100

@Test
def foldRightWithCont02(): Bool = Set.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.rem` 2 + 1), 100, Set#{1}) == 198

@Test
def foldRightWithCont03(): Bool = Set.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.rem` 2 + 1), 100, Set#{2, 1}) == 194

@Test
def foldRightWithCont04(): Bool = Set.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.rem` 2 + 1), 100, Set#{3, 2, 1}) == 382

/////////////////////////////////////////////////////////////////////////////
// foldMap                                                                 //
/////////////////////////////////////////////////////////////////////////////

@Test
def foldMap01(): Bool = Set.foldMap(x -> 2 * x, Set#{}) == 0

@Test
def foldMap02(): Bool = Set.foldMap(x -> 2 * x, Set#{1, 2}) == 6

@Test
def foldMap03(): Bool = Set.foldMap(x -> if (x == "a") "b" else x, Set#{"a"}) == "b"

@Test
def foldMap04(): Bool = Set.foldMap(x -> if (x == "c") "b" else x, Set#{"a", "b", "c"}) == "abb"

@Test
def foldMap05(): Bool = Set.foldMap(Int32.toString, Set#{1, 2, 3}) == "123"

/////////////////////////////////////////////////////////////////////////////
// reduceLeft                                                              //
/////////////////////////////////////////////////////////////////////////////
@Test
def reduceLeft01(): Bool = Set.reduceLeft((a, b) -> a-b, Set#{}: Set[Int32]) == None

@Test
def reduceLeft02(): Bool = Set.reduceLeft((a, b) -> a-b, Set#{1}) == Some(1)

@Test
def reduceLeft03(): Bool = Set.reduceLeft((a, b) -> a-b, Set#{2, 1}) == Some(-1)

@Test
def reduceLeft04(): Bool = Set.reduceLeft((a, b) -> a-b, Set#{3, 2, 1}) == Some(-4)

@Test
def reduceLeft05(): Bool = Set.reduceLeft((a, b) -> a-b, Set#{4, 3, 2, 1}) == Some(-8)

/////////////////////////////////////////////////////////////////////////////
// reduceRight                                                             //
/////////////////////////////////////////////////////////////////////////////
@Test
def reduceRight01(): Bool = Set.reduceRight((a, b) -> a-b, Set#{}: Set[Int32]) == None

@Test
def reduceRight02(): Bool = Set.reduceRight((a, b) -> a-b, Set#{1}) == Some(1)

@Test
def reduceRight03(): Bool = Set.reduceRight((a, b) -> a-b, Set#{2, 1}) == Some(-1)

@Test
def reduceRight04(): Bool = Set.reduceRight((a, b) -> a-b, Set#{3, 2, 1}) == Some(2)

@Test
def reduceRight05(): Bool = Set.reduceRight((a, b) -> a-b, Set#{4, 3, 2, 1}) == Some(-2)

/////////////////////////////////////////////////////////////////////////////
// count                                                                   //
/////////////////////////////////////////////////////////////////////////////
@Test
def count01(): Bool = Set.count(i -> i > 3, Set#{}) == 0

@Test
def count02(): Bool = Set.count(i -> i > 3, Set#{1}) == 0

@Test
def count03(): Bool = Set.count(i -> i > 3, Set#{4}) == 1

@Test
def count04(): Bool = Set.count(i -> i > 3, Set#{2, 1}) == 0

@Test
def count05(): Bool = Set.count(i -> i > 3, Set#{8, 1}) == 1

@Test
def count06(): Bool = Set.count(i -> i > 3, Set#{1, 8}) == 1

@Test
def count07(): Bool = Set.count(i -> i > 3, Set#{7, 6}) == 2

@Test
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

    @Test
    def sum01(): Bool =
        Set.empty() |> Set.sum == 0

    @Test
    def sum02(): Bool =
        Set#{1} |> Set.sum == 1

    @Test
    def sum03(): Bool =
        Set#{1, 2, 3} |> Set.sum == 6

    @Test
    def sum04(): Bool =
        Set#{1, 2, 3, -3} |> Set.sum == 3

    @Test
    def sum05(): Bool =
        Set#{1, 2, -3, -4} |> Set.sum == -4

    @Test
    def sum06(): Bool =
        Set#{10, -10} |> Set.sum == 0

    @Test
    def sum07(): Bool =
        Set.range(1, 101) |> Set.sum == 5050


    /////////////////////////////////////////////////////////////////////////////
    // sumWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sumWith01(): Bool =
        Set.empty() |> Set.sumWith(x -> x + 1) == 0

    @Test
    def sumWith02(): Bool =
        Set#{1} |> Set.sumWith(x -> x + 1) == 2

    @Test
    def sumWith03(): Bool =
        Set#{1, 2, 3} |> Set.sumWith(x -> x + 1) == 9

    @Test
    def sumWith04(): Bool =
        Set#{1, 2, 3, -3} |> Set.sumWith(x -> x + 1) == 7

    @Test
    def sumWith05(): Bool =
        Set#{-1, -2, -3, -4} |> Set.sumWith(x -> x + 1) == -6

    @Test
    def sumWith06(): Bool =
        Set#{10, -10} |> Set.sumWith(x -> x + 1) == 2

    @Test
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
@Test
def flatten01(): Bool = Set.flatten(Set#{}): Set[Unit] == Set#{}

@Test
def flatten02(): Bool = Set.flatten(Set#{Set#{}}: Set[Set[Unit]]) == Set#{}

@Test
def flatten03(): Bool = Set.flatten(Set#{Set#{1}}) == Set#{1}

@Test
def flatten04(): Bool = Set.flatten(Set#{Set#{1, 2}}) == Set#{1, 2}

@Test
def flatten05(): Bool = Set.flatten(Set#{Set#{}, Set#{}}): Set[Unit] == Set#{}

@Test
def flatten06(): Bool = Set.flatten(Set#{Set#{1}, Set#{}}) == Set#{1}

@Test
def flatten07(): Bool = Set.flatten(Set#{Set#{}, Set#{1}}) == Set#{1}

@Test
def flatten08(): Bool = Set.flatten(Set#{Set#{1}, Set#{2}}) == Set#{1, 2}

@Test
def flatten09(): Bool = Set.flatten(Set#{Set#{1}, Set#{1}}) == Set#{1}

@Test
def flatten10(): Bool = Set.flatten(Set#{Set#{1, 2}, Set#{3, 4, 5}}) == Set#{1, 2, 3, 4, 5}

@Test
def flatten11(): Bool = Set.flatten(Set#{Set#{1, 2}, Set#{3, 4, 3}}) == Set#{1, 2, 3, 4}

@Test
def flatten12(): Bool = Set.flatten(Set#{Set#{1, 2}, Set#{3, 4, 1}}) == Set#{1, 2, 3, 4}

@Test
def flatten13(): Bool = Set.flatten(Set#{Set#{1}, Set#{2, 3}, Set#{4}}) == Set#{1, 2, 3, 4}

@Test
def flatten14(): Bool = Set.flatten(Set#{Set#{1}, Set#{2, 3}, Set#{1}}) == Set#{1, 2, 3}

/////////////////////////////////////////////////////////////////////////////
// exists                                                                  //
/////////////////////////////////////////////////////////////////////////////
@Test
def exists01(): Bool = Set.exists(x -> x `Int32.rem` 8 == 7, Set#{}) == false

@Test
def exists02(): Bool = Set.exists(x -> x `Int32.rem` 8 == 7, Set#{5}) == false

@Test
def exists03(): Bool = Set.exists(x -> x `Int32.rem` 8 == 7, Set#{7}) == true

@Test
def exists04(): Bool = Set.exists(x -> x `Int32.rem` 8 == 7, Set#{15}) == true

@Test
def exists05(): Bool = Set.exists(x -> x `Int32.rem` 8 == 7, Set#{1, 44}) == false

@Test
def exists06(): Bool = Set.exists(x -> x `Int32.rem` 8 == 7, Set#{11, 71}) == true

@Test
def exists07(): Bool = Set.exists(x -> x `Int32.rem` 8 == 7, Set#{71, 12}) == true

@Test
def exists08(): Bool = Set.exists(x -> x `Int32.rem` 8 == 7, Set#{71, 79}) == true

@Test
def exists09(): Bool = Set.exists(x -> x `Int32.rem` 8 == 7, Set#{11, -1, -14, -2, 84, 113}) == false

@Test
def exists10(): Bool = Set.exists(x -> x `Int32.rem` 8 == 7, Set#{11, -1, 31, -14, -2, 84, 111}) == true

@Test
def exists11(): Bool = Set.exists(x -> x `Int32.rem` 8 == 7, Set#{11, -1, -14, -2, 84, 111, 38}) == true

/////////////////////////////////////////////////////////////////////////////
// forAll                                                                  //
/////////////////////////////////////////////////////////////////////////////
@Test
def forAll01(): Bool = Set.forAll(x -> x `Int32.rem` 8 == 7, Set#{}) == true

@Test
def forAll02(): Bool = Set.forAll(x -> x `Int32.rem` 8 == 7, Set#{5}) == false

@Test
def forAll03(): Bool = Set.forAll(x -> x `Int32.rem` 8 == 7, Set#{7}) == true

@Test
def forAll04(): Bool = Set.forAll(x -> x `Int32.rem` 8 == 7, Set#{15}) == true

@Test
def forAll05(): Bool = Set.forAll(x -> x `Int32.rem` 8 == 7, Set#{1, 44}) == false

@Test
def forAll06(): Bool = Set.forAll(x -> x `Int32.rem` 8 == 7, Set#{11, 71}) == false

@Test
def forAll07(): Bool = Set.forAll(x -> x `Int32.rem` 8 == 7, Set#{71, 12}) == false

@Test
def forAll08(): Bool = Set.forAll(x -> x `Int32.rem` 8 == 7, Set#{71, 79}) == true

@Test
def forAll09(): Bool = Set.forAll(x -> x `Int32.rem` 8 == 7, Set#{7, 15, 23, 84, 111}) == false

@Test
def forAll10(): Bool = Set.forAll(x -> x `Int32.rem` 8 == 7, Set#{7, 15, 23, 111, 3}) == false

@Test
def forAll11(): Bool = Set.forAll(x -> x `Int32.rem` 8 == 7, Set#{7, 15, 23, 119, 111}) == true

/////////////////////////////////////////////////////////////////////////////
// union                                                                   //
/////////////////////////////////////////////////////////////////////////////
@Test
def union01(): Bool = Set.union(Set#{}: Set[Unit], Set#{}) == Set#{}

@Test
def union02(): Bool = Set.union(Set#{1}, Set#{}) == Set#{1}

@Test
def union03(): Bool = Set.union(Set#{}, Set#{2}) == Set#{2}

@Test
def union04(): Bool = Set.union(Set#{1}, Set#{1}) == Set#{1}

@Test
def union05(): Bool = Set.union(Set#{1}, Set#{-1}) == Set#{1, -1}

@Test
def union06(): Bool = Set.union(Set#{}, Set#{-1, 9}) == Set#{-1, 9}

@Test
def union07(): Bool = Set.union(Set#{9}, Set#{-1, 9}) == Set#{-1, 9}

@Test
def union08(): Bool = Set.union(Set#{4}, Set#{-1, 9}) == Set#{4, -1, 9}

@Test
def union09(): Bool = Set.union(Set#{9, -1}, Set#{-1, 9}) == Set#{-1, 9}

@Test
def union10(): Bool = Set.union(Set#{9, 5}, Set#{-1, 9}) == Set#{5, -1, 9}

@Test
def union11(): Bool = Set.union(Set#{6, 5}, Set#{-1, 9}) == Set#{6, 5, -1, 9}

@Test
def union12(): Bool = Set.union(Set#{6, -99}, Set#{6, 5, -1, 9, 43, 7, 8, -99}) == Set#{6, 5, -1, 9, 43, 7, 8, -99}

@Test
def union13(): Bool = Set.union(Set#{6, -99, -1, 5, 22}, Set#{6, 5, -1, 9, 43, 7, 8, -99}) == Set#{22, 6, 5, -1, 9, 43, 7, 8, -99}

@Test
def union14(): Bool = Set.union(Set#{-2, -3, -4}, Set#{6, 5, -1, 9, 43, 7, 8, -99}) == Set#{-2, -3, -4, 6, 5, -1, 9, 43, 7, 8, -99}

/////////////////////////////////////////////////////////////////////////////
// intersection                                                            //
/////////////////////////////////////////////////////////////////////////////
@Test
def intersection01(): Bool = Set.intersection(Set#{}: Set[Unit], Set#{}) == Set#{}

@Test
def intersection02(): Bool = Set.intersection(Set#{1}, Set#{}) == Set#{}

@Test
def intersection03(): Bool = Set.intersection(Set#{}, Set#{2}) == Set#{}

@Test
def intersection04(): Bool = Set.intersection(Set#{1}, Set#{2}) == Set#{}

@Test
def intersection05(): Bool = Set.intersection(Set#{1}, Set#{1}) == Set#{1}

@Test
def intersection06(): Bool = Set.intersection(Set#{}, Set#{1, 2}) == Set#{}

@Test
def intersection07(): Bool = Set.intersection(Set#{1, 2}, Set#{}) == Set#{}

@Test
def intersection08(): Bool = Set.intersection(Set#{2}, Set#{1, 2}) == Set#{2}

@Test
def intersection09(): Bool = Set.intersection(Set#{1}, Set#{1, 2}) == Set#{1}

@Test
def intersection10(): Bool = Set.intersection(Set#{2, 1}, Set#{1, 2}) == Set#{2, 1}

@Test
def intersection11(): Bool = Set.intersection(Set#{1, 2}, Set#{1, 2}) == Set#{1, 2}

@Test
def intersection12(): Bool = Set.intersection(Set#{3, 2}, Set#{1, 2}) == Set#{2}

@Test
def intersection13(): Bool = Set.intersection(Set#{3, 55}, Set#{1, 2}) == Set#{}

@Test
def intersection14(): Bool = Set.intersection(Set#{3, 55, 11, 87, 22, 34, -87, 23}, Set#{1, 2, 84, -87, 87, 3, 44}) == Set#{3, 87, -87}

@Test
def intersection15(): Bool = Set.intersection(Set#{3, 55, 11, 87, 22, 34, -87, 23}, Set#{23, 1, 2, 84, 87, 3}) == Set#{3, 87, 23}

/////////////////////////////////////////////////////////////////////////////
// difference                                                              //
/////////////////////////////////////////////////////////////////////////////
@Test
def difference01(): Bool = Set.difference(Set#{}: Set[Unit], Set#{}) == Set#{}

@Test
def difference02(): Bool = Set.difference(Set#{}, Set#{2}) == Set#{}

@Test
def difference03(): Bool = Set.difference(Set#{1}, Set#{}) == Set#{1}

@Test
def difference04(): Bool = Set.difference(Set#{1}, Set#{2}) == Set#{1}

@Test
def difference05(): Bool = Set.difference(Set#{1}, Set#{1}) == Set#{}

@Test
def difference06(): Bool = Set.difference(Set#{1, 2}, Set#{}) == Set#{1, 2}

@Test
def difference07(): Bool = Set.difference(Set#{1, 2}, Set#{1}) == Set#{2}

@Test
def difference08(): Bool = Set.difference(Set#{1, 2}, Set#{2}) == Set#{1}

@Test
def difference09(): Bool = Set.difference(Set#{1, 2}, Set#{8, 2, 4}) == Set#{1}

@Test
def difference10(): Bool = Set.difference(Set#{1, 2}, Set#{3, 1, 2, 4}) == Set#{}

@Test
def difference11(): Bool = Set.difference(Set#{1, 2}, Set#{3, 11, 21, 4}) == Set#{1, 2}

@Test
def difference12(): Bool = Set.difference(Set#{1, 2, 87, 4, 5, 6, 86, 92, 111, -1}, Set#{-1, 92, 4, 5, 1, 2, 86}) == Set#{87, 6, 111}

@Test
def difference13(): Bool = Set.difference(Set#{1, 2, 87, 4, 5, 6, 86, 92, 111, -1}, Set#{-1, 92, 4, 5, 1, 2, 86, 99, 6}) == Set#{87, 111}

/////////////////////////////////////////////////////////////////////////////
// subsets                                                                 //
/////////////////////////////////////////////////////////////////////////////
@Test
def subsets01(): Bool = Set.subsets(Set#{}: Set[Unit]) == Set#{Set#{}}

@Test
def subsets02(): Bool = Set.subsets(Set#{1}) == Set#{Set#{1}, Set#{}}

@Test
def subsets03(): Bool = Set.subsets(Set#{1, 2}) == Set#{Set#{1, 2}, Set#{1}, Set#{2}, Set#{}}

@Test
def subsets04(): Bool = Set.subsets(Set#{1, 2, 3}) == Set#{Set#{1, 2, 3}, Set#{1, 2}, Set#{1, 3}, Set#{1}, Set#{2, 3}, Set#{2}, Set#{3}, Set#{}}

/////////////////////////////////////////////////////////////////////////////
// filter                                                                  //
/////////////////////////////////////////////////////////////////////////////
@Test
def filter01(): Bool = Set.filter(x -> x `Int32.rem` 2 == 0, Set#{}) == Set#{}

@Test
def filter02(): Bool = Set.filter(x -> x `Int32.rem` 2 == 0, Set#{1}) == Set#{}

@Test
def filter03(): Bool = Set.filter(x -> x `Int32.rem` 2 == 0, Set#{2}) == Set#{2}

@Test
def filter04(): Bool = Set.filter(x -> x `Int32.rem` 2 == 0, Set#{1, 3}) == Set#{}

@Test
def filter05(): Bool = Set.filter(x -> x `Int32.rem` 2 == 0, Set#{8, 3}) == Set#{8}

@Test
def filter06(): Bool = Set.filter(x -> x `Int32.rem` 2 == 0, Set#{-1, 32}) == Set#{32}

@Test
def filter07(): Bool = Set.filter(x -> x `Int32.rem` 2 == 0, Set#{12, 34}) == Set#{12, 34}

@Test
def filter08(): Bool = Set.filter(x -> x `Int32.rem` 2 == 0, Set#{-33, -1, 12, 1, 34, 88, 7, 77, 31}) == Set#{12, 34, 88}

@Test
def filter09(): Bool = Set.filter(x -> x `Int32.rem` 2 == 0, Set#{-33, -1, 12, 1, 34, 88, 7, 77, 31, 7, -92, 841}) == Set#{12, 34, 88, -92}

/////////////////////////////////////////////////////////////////////////////
// map                                                                     //
/////////////////////////////////////////////////////////////////////////////
@Test
def map01(): Bool = Set.map(x -> x `Int32.rem` 2 == 0, Set#{}) == Set#{}

@Test
def map02(): Bool = Set.map(x -> x `Int32.rem` 2 == 0, Set#{1}) == Set#{false}

@Test
def map03(): Bool = Set.map(x -> x `Int32.rem` 2 == 0, Set#{2}) == Set#{true}

@Test
def map04(): Bool = Set.map(x -> x `Int32.rem` 2 == 0, Set#{1, -1}) == Set#{false}

@Test
def map05(): Bool = Set.map(x -> x `Int32.rem` 2 == 0, Set#{1, -12}) == Set#{false, true}

@Test
def map06(): Bool = Set.map(x -> x `Int32.rem` 2 == 0, Set#{16, -1}) == Set#{true, false}

@Test
def map07(): Bool = Set.map(x -> x `Int32.rem` 2 == 0, Set#{12, -12}) == Set#{true}

@Test
def map08(): Bool = Set.map(x -> x `Int32.rem` 2 == 0, Set#{12, -12}) == Set#{true}

@Test
def map09(): Bool = Set.map(x -> x `Int32.rem` 2 == 0, Set#{12, -12, 1, 14}) == Set#{false, true}

@Test
def map10(): Bool = Set.map(x -> x `Int32.rem` 2 == 0, Set#{12, -12, 1, 14, 7, 88, -91}) == Set#{true, false}

@Test
def map11(): Bool = Set.map(x -> x `Int32.rem` 2 == 0, Set#{12, -12, 122, 14}) == Set#{true}

@Test
def map12(): Bool = Set.map(x -> x `Int32.rem` 2 == 0, Set#{123, -123, 1223, 141}) == Set#{false}

@Test
def map13(): Bool = Set.map(x -> x `Int32.rem` 9, Set#{11, 5, 16, 4}) == Set#{2, 5, 7, 4}

@Test
def map14(): Bool = Set.map(x -> x `Int32.rem` 9, Set#{0, 5, 1, -9, -8}) == Set#{5, 1, 0, -8}

@Test
def map15(): Bool = Set.map(x -> x `Int32.rem` 9, Set#{0, 5, 1, 10, 7, 19, 28, 2}) == Set#{0, 5, 7, 1, 2}

@Test
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
@Test
def flatMap01(): Bool = Set.flatMap(x -> if (x `Int32.rem` 2 == 0) Set#{} else Set#{x}, Set#{}) == Set#{}

@Test
def flatMap02(): Bool = Set.flatMap(x -> if (x `Int32.rem` 2 == 0) Set#{} else Set#{x}, Set#{2}) == Set#{}

@Test
def flatMap03(): Bool = Set.flatMap(x -> if (x `Int32.rem` 2 == 0) Set#{} else Set#{x}, Set#{1}) == Set#{1}

@Test
def flatMap04(): Bool = Set.flatMap(x -> if (x `Int32.rem` 2 == 0) Set#{} else Set#{x}, Set#{2, 4}) == Set#{}

@Test
def flatMap05(): Bool = Set.flatMap(x -> if (x `Int32.rem` 2 == 0) Set#{} else Set#{x}, Set#{2, 13}) == Set#{13}

@Test
def flatMap06(): Bool = Set.flatMap(x -> if (x `Int32.rem` 2 == 0) Set#{} else Set#{x}, Set#{15, -8}) == Set#{15}

@Test
def flatMap07(): Bool = Set.flatMap(x -> if (x `Int32.rem` 2 == 0) Set#{} else Set#{x}, Set#{1, 11}) == Set#{1, 11}

@Test
def flatMap08(): Bool = Set.flatMap(x -> Set#{x, 2*x}, Set#{1, 4, 8, 2}) == Set#{1, 8, 16, 2, 4}

@Test
def flatMap09(): Bool = Set.flatMap(x -> Set#{x, 3*x}, Set#{1, 8, 3, 2, 9, -5, -1}) == Set#{1, 8, 24, 3, 2, 6, 9, 27, -5, -15, -1, -3}

@Test
def flatMap10(): Bool = Set.flatMap(x -> Set#{x, 2*x, 3*x}, Set#{1, 4, 3, 2}) == Set#{1, 8, 12, 3, 9, 2, 4, 6}

/////////////////////////////////////////////////////////////////////////////
// filterMap                                                               //
/////////////////////////////////////////////////////////////////////////////

@Test
def filterMap01(): Bool =
    Set.filterMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Set.empty()) == Set.empty()

@Test
def filterMap02(): Bool =
    Set.filterMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Set#{1}) == Set#{}

@Test
def filterMap03(): Bool =
    Set.filterMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Set#{2}) == Set#{1}

@Test
def filterMap04(): Bool =
    Set.filterMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Set#{1, 3}) == Set#{}

@Test
def filterMap05(): Bool =
    Set.filterMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Set#{1, 4}) == Set#{2}

@Test
def filterMap06(): Bool =
    Set.filterMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Set#{-1, 6}) == Set#{3}

@Test
def filterMap07(): Bool =
    Set.filterMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Set#{6, 8}) == Set#{3, 4}

@Test
def filterMap08(): Bool =
    let s1 = Set#{0, 1, 2, 3, 4, 5, 10};
    let s2 = Set#{0, 1, 2, 5};
    Set.filterMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, s1) == s2

/////////////////////////////////////////////////////////////////////////////
// replace                                                                 //
/////////////////////////////////////////////////////////////////////////////
@Test
def replace01(): Bool = Set.replace(from = 3, to = 4, Set#{}) == Set#{}

@Test
def replace02(): Bool = Set.replace(from = 3, to = 4, Set#{1}) == Set#{1}

@Test
def replace03(): Bool = Set.replace(from = 3, to = 4, Set#{3}) == Set#{4}

@Test
def replace04(): Bool = Set.replace(from = 3, to = 4, Set#{4}) == Set#{4}

@Test
def replace05(): Bool = Set.replace(from = 3, to = 4, Set#{1, 2}) == Set#{1, 2}

@Test
def replace06(): Bool = Set.replace(from = 3, to = 4, Set#{1, 3}) == Set#{1, 4}

@Test
def replace07(): Bool = Set.replace(from = 3, to = 4, Set#{3, 2}) == Set#{4, 2}

@Test
def replace08(): Bool = Set.replace(from = 3, to = 4, Set#{3, 4}) == Set#{4}

/////////////////////////////////////////////////////////////////////////////
// partition                                                               //
/////////////////////////////////////////////////////////////////////////////
@Test
def partition01(): Bool = Set.partition(x -> x `Int32.rem` 2 == 0, Set#{}) == (Set#{}, Set#{})

@Test
def partition02(): Bool = Set.partition(x -> x `Int32.rem` 2 == 0, Set#{1}) == (Set#{}, Set#{1})

@Test
def partition03(): Bool = Set.partition(x -> x `Int32.rem` 2 == 0, Set#{2}) == (Set#{2}, Set#{})

@Test
def partition04(): Bool = Set.partition(x -> x `Int32.rem` 2 == 0, Set#{1, 3}) == (Set#{}, Set#{1, 3})

@Test
def partition05(): Bool = Set.partition(x -> x `Int32.rem` 2 == 0, Set#{1, 2}) == (Set#{2}, Set#{1})

@Test
def partition06(): Bool = Set.partition(x -> x `Int32.rem` 2 == 0, Set#{2, 1}) == (Set#{2}, Set#{1})

@Test
def partition07(): Bool = Set.partition(x -> x `Int32.rem` 2 == 0, Set#{2, -4}) == (Set#{2, -4}, Set#{})

@Test
def partition08(): Bool = Set.partition(x -> x `Int32.rem` 2 == 0, Set#{2, -11, 89, -4, 11, -6, 84}) == (Set#{2, -4, -6, 84}, Set#{-11, 89, 11})

@Test
def partition09(): Bool = Set.partition(x -> x `Int32.rem` 2 == 0, Set#{84, -6, 11, -4, 89, -11, 2}) == (Set#{84, -6, -4, 2}, Set#{11, 89, -11})

@Test
def partition10(): Bool = Set.partition(x -> x `Int32.rem` 2 == 0, Set#{1, 2, 3, 4, 5, 6, 7, 8}) == (Set#{2, 4, 6, 8}, Set#{1, 3, 5, 7})

/////////////////////////////////////////////////////////////////////////////
// toList                                                                  //
/////////////////////////////////////////////////////////////////////////////
@Test
def toList01(): Bool = Set.toList(Set#{}: Set[Unit]) == Nil

@Test
def toList02(): Bool = Set.toList(Set#{1}) == 1 :: Nil

@Test
def toList03(): Bool = Set.toList(Set#{1, 2}) == 1 :: 2 :: Nil

@Test
def toList04(): Bool = Set.toList(Set#{1, 2, 3}) == 1 :: 2 :: 3 :: Nil

/////////////////////////////////////////////////////////////////////////////
// toChain                                                                 //
/////////////////////////////////////////////////////////////////////////////

@Test
def toChain01(): Bool =
    Set.toChain(Set#{}: Set[Int32]) == Chain.empty(): Chain[Int32]

@Test
def toChain02(): Bool =
    Set.toChain(Set#{1}) == Chain.singleton(1)

@Test
def toChain03(): Bool =
    Set.toChain(Set#{1, 2}) == List.toChain(1 :: 2 :: Nil)

@Test
def toChain04(): Bool =
    Set.toChain(Set#{1, 2, 3}) == List.toChain(1 :: 2 :: 3 :: Nil)

/////////////////////////////////////////////////////////////////////////////
// toMap                                                                   //
/////////////////////////////////////////////////////////////////////////////
@Test
def toMap01(): Bool = Set.toMap(Set#{}: Set[(Unit, Unit)]) == Map#{}

@Test
def toMap02(): Bool = Set.toMap(Set#{(1, true)}) == Map#{1 => true}

@Test
def toMap03(): Bool = Set.toMap(Set#{(1, true), (2, false)}) == Map#{1 => true, 2 => false}

@Test
def toMap04(): Bool = Set.toMap(Set#{(1, true), (1, false)}) == Map#{1 => false}

@Test
def toMap05(): Bool = Set.toMap(Set#{(1, true), (2, false), (3, true)}) == Map#{1 => true, 2 => false, 3 => true}


/////////////////////////////////////////////////////////////////////////////
// toMapWith                                                               //
/////////////////////////////////////////////////////////////////////////////

@Test
def toMapWith02(): Bool =
    Set.toMapWith(x -> 0 * x, Set#{}) == Map#{}

@Test
def toMapWith03(): Bool =
    Set.toMapWith(x -> 2 * x, Set#{1, 3, 6}) == Map#{1 => 2, 3 => 6, 6 => 12}

@Test
def toMapWith04(): Bool =
    Set.toMapWith(x -> x + 10, Set#{1, 3, 6}) == Map#{1 => 11, 3 => 13, 6 => 16}

@Test
def toMapWith05(): Bool =
    Set.toMapWith(x -> x + 10, Set#{7, 1, 3, 6}) == Map#{1 => 11, 3 => 13, 6 => 16, 7 => 17}

/////////////////////////////////////////////////////////////////////////////
// toMutDeque                                                              //
/////////////////////////////////////////////////////////////////////////////

@Test
def toMutDeque01(): Bool = region rc {
    let s: Set[Int32] = Set#{};
    let d1 = Set.toMutDeque(rc, s);

    let d2 = MutDeque.new(rc);

    d1 `MutDeque.sameElements` d2
}

@Test
def toMutDeque02(): Bool = region rc {
    let s = Set#{1};
    let d1 = Set.toMutDeque(rc, s);

    let d2 = MutDeque.new(rc);
    MutDeque.pushBack(1, d2);

    d1 `MutDeque.sameElements` d2
}

@Test
def toMutDeque03(): Bool = region rc {
    let s = Set#{1, 3, 6};
    let d1 = Set.toMutDeque(rc, s);

    let d2 = MutDeque.new(rc);
    MutDeque.pushBack(3, d2);
    MutDeque.pushBack(6, d2);
    MutDeque.pushFront(1, d2);

    d1 `MutDeque.sameElements` d2
}

@Test
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
@Test
def eq01(): Bool = Set#{}: Set[Unit] == Set#{}

@Test
def eq02(): Bool = Set#{1} != Set#{}

@Test
def eq03(): Bool = Set#{} != Set#{1}

@Test
def eq04(): Bool = Set#{1, 2} != Set#{}

@Test
def eq05(): Bool = Set#{} != Set#{1, 2}

@Test
def eq06(): Bool = Set#{1} != Set#{2}

@Test
def eq07(): Bool = Set#{1} == Set#{1}

@Test
def eq08(): Bool = Set#{1, 2, 3} != Set#{}

@Test
def eq09(): Bool = Set#{1, 2} != Set#{1}

@Test
def eq10(): Bool = Set#{1} != Set#{1, 2}

@Test
def eq11(): Bool = Set#{} != Set#{1, 2, 3}

@Test
def eq12(): Bool = Set#{1, 2} != Set#{1, 3}

@Test
def eq13(): Bool = Set#{1, 2} == Set#{2, 1}

@Test
def eq14(): Bool = Set#{1, 2} == Set#{1, 2}

@Test
def eq15(): Bool = Set#{1, 2, 3} != Set#{1, 2, 4}

@Test
def eq16(): Bool = Set#{1, 2, 3} == Set#{1, 2, 3}

@Test
def eq17(): Bool = Set#{1, 2, 3} == Set#{2, 3, 1}

@Test
def eq18(): Bool = Set#{1, 2, 3} == Set#{3, 1, 2}

@Test
def eq19(): Bool = Set#{1, 2, 3} == Set#{2, 1, 3}

/////////////////////////////////////////////////////////////////////////////
// forEach                                                                 //
/////////////////////////////////////////////////////////////////////////////

@Test
def forEach01(): Bool = region rc {
    let ri = ref 21 @ rc;
    Set.forEach(x -> ri := x, Set#{});
    21 == deref ri
}

@Test
def forEach02(): Bool = region rc {
    let ri = ref 21 @ rc;
    Set.forEach(x -> ri := x, Set#{42});
    42 == deref ri
}

/////////////////////////////////////////////////////////////////////////////
// forEachWithIndex                                                        //
/////////////////////////////////////////////////////////////////////////////

@Test
def forEachWithIndex01(): Bool = region rc {
    let ri = ref 21 @ rc;
    Set.forEachWithIndex((i, _) -> ri := i, Set#{});
    21 == deref ri
}

@Test
def forEachWithIndex02(): Bool = region rc {
    let ri = ref 21 @ rc;
    Set.forEachWithIndex((i, _) -> ri := i, Set#{42});
    0 == deref ri
}

/////////////////////////////////////////////////////////////////////////////
// unfold                                                                  //
/////////////////////////////////////////////////////////////////////////////

@Test
def unfold01(): Bool =
    Set.unfold(s -> if (true) None else Some(s + 48, s + 1), 0) == Set#{}

@Test
def unfold02(): Bool =
    Set.unfold(s -> if (s > 0) None else Some(s + 48, s + 1), 0) == Set#{48}

@Test
def unfold03(): Bool =
    Set.unfold(s -> if (s > 1) None else Some(s + 48, s + 1), 0) == Set#{48, 49}

@Test
def unfold04(): Bool =
    Set.unfold(s -> if (s >= 10) None else Some(s + 48, s + 1), 0) == Set#{48, 49, 50, 51, 52, 53, 54, 55, 56, 57}

@Test
def unfold05(): Bool =
    Set.unfold(s -> if (s >= 10) None else Some(s + 48, s + 1), 5) == Set#{53, 54, 55, 56, 57}

@Test
def unfold06(): Bool =
    Set.unfold(s -> if (s >= 10) None else Some(s + 48, s + 2), 0) == Set#{48, 50, 52, 54, 56}

/////////////////////////////////////////////////////////////////////////////
// unfoldWithIter                                                          //
/////////////////////////////////////////////////////////////////////////////

@Test
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

@Test
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

@Test
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

@Test
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

@Test
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

@Test
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

    @Test
    def toString01(): Bool =
        toString(Set#{1}) == "Set#{1}"

    @Test
    def toString02(): Bool =
        toString(Set#{1, 2, 3}) == "Set#{1, 2, 3}"

    @Test
    def toString03(): Bool =
        toString(Set#{1, 2}) == "Set#{1, 2}"

    @Test
    def toString04(): Bool =
        toString(Set#{97, 2, 3, 4, 0}) == "Set#{0, 2, 3, 4, 97}"

    @Test
    def toString05(): Bool =
        toString(Set#{Set#{1, 2}, Set#{4, 6}}) == "Set#{Set#{1, 2}, Set#{4, 6}}"

    /////////////////////////////////////////////////////////////////////////////
    // hash                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def hash01(): Bool = hash(Set#{}: Set[Unit]) == hash(Set#{}: Set[Unit])

    @Test
    def hash02(): Bool = hash(Set#{1, 2}) == hash(Set#{1, 2})

    @Test
    def hash03(): Bool = hash(Set#{'a', 'b', 'c', 'd'}) == hash(Set#{'a', 'b', 'c', 'd'})

    @Test
    def hash04(): Bool = hash(Set#{'a', 'b', 'c', 'e'}) != hash(Set#{'a', 'b', 'c', 'd'})

    @Test
    def hash05(): Bool = hash(Set#{false}) != hash(Set#{true})

    /////////////////////////////////////////////////////////////////////////////
    // minimum                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def minimum01(): Bool = Set.minimum(Set#{}: Set[Int32]) == None

    @Test
    def minimum02(): Bool = Set.minimum(Set#{1}) == Some(1)

    @Test
    def minimum03(): Bool = Set.minimum(Set#{1, 2, 3, 0}) == Some(0)

    @Test
    def minimum04(): Bool = Set.minimum(Set.range(5, 55)) == Some(5)

    /////////////////////////////////////////////////////////////////////////////
    // minimumBy                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def minimumBy01(): Bool = Set.minimumBy((x, y) -> x <=> y, Set#{}: Set[Int32]) == None

    @Test
    def minimumBy02(): Bool = Set.minimumBy((x, y) -> x <=> y, Set#{1}) == Some(1)

    @Test
    def minimumBy03(): Bool = Set.minimumBy((x, y) -> x <=> y, Set#{1, 2, 3, 0}) == Some(0)

    @Test
    def minimumBy04(): Bool = Set.minimumBy((x, y) -> x <=> y, Set.range(5, 55)) == Some(5)

    @Test
    def minimumBy05(): Bool = region rc {
        let s = List.range(0, 2000) |> List.toSet;
        let a = MutList.new(rc);
        discard Set.minimumBy((x, y) -> { let b = MutList.new(rc); MutList.push!(y, b); MutList.append!(MutList.toList(b), a); x <=> y }, s);
        MutList.toList(a) == List.range(1, 2000) // The first / left most value is ignored
    }


    /////////////////////////////////////////////////////////////////////////////
    // maximum                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def maximum01(): Bool = Set.maximum(Set#{}: Set[Int32]) == None

    @Test
    def maximum02(): Bool = Set.maximum(Set#{1}) == Some(1)

    @Test
    def maximum03(): Bool = Set.maximum(Set#{1, 2, 3, 0}) == Some(3)

    @Test
    def maximum04(): Bool = Set.maximum(Set.range(5, 55)) == Some(54)

    /////////////////////////////////////////////////////////////////////////////
    // maximumBy                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def maximumBy01(): Bool = Set.maximumBy((x, y) -> x <=> y, Set#{}: Set[Int32]) == None

    @Test
    def maximumBy02(): Bool = Set.maximumBy((x, y) -> x <=> y, Set#{1}) == Some(1)

    @Test
    def maximumBy03(): Bool = Set.maximumBy((x, y) -> x <=> y, Set#{1, 2, 3, 0}) == Some(3)

    @Test
    def maximumBy04(): Bool = Set.maximumBy((x, y) -> x <=> y, Set.range(5, 55)) == Some(54)


    /////////////////////////////////////////////////////////////////////////////
    // toDelayList                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toDelayList01(): Bool =
        Set.empty(): Set[Unit] |> Set.toDelayList == DelayList.empty()

    @Test
    def toDelayList02(): Bool =
        Set.range(-1000, 1000) |> Set.toDelayList == DelayList.range(-1000, 1000)

    @Test
    def toDelayList03(): Bool =
        Set.range(-1000, 1000) |> Set.toDelayList |> DelayList.toSet == Set.range(-1000, 1000)


    /////////////////////////////////////////////////////////////////////////////
    // iterator                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def iterator01(): Bool = region rc {
        Set.empty(): Set[Int32] |> Set.iterator(rc) |> Iterator.toSet == Set.empty()
    }

    @Test
    def iterator02(): Bool = region rc {
        Set#{1, 2, 3, 4} |> Set.iterator(rc) |> Iterator.toSet == Set#{1, 2, 3, 4}
    }

    @Test
    def iterator03(): Bool = region rc {
        Set.range(0, 100) |> Set.iterator(rc) |> Iterator.toSet == Set.range(0, 100)
    }


    /////////////////////////////////////////////////////////////////////////////
    // join                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def join01(): Bool =
        Set.join(",", Set.empty(): Set[Int32]) == ""

    @Test
    def join02(): Bool =
        Set.join(",", Set#{1}) == "1"

    @Test
    def join03(): Bool =
        Set.join(",", Set#{1, 2, 3}) == "1,2,3"

    @Test
    def join04(): Bool =
        Set.join(",", Set#{"1", "2", "3"}) == "1,2,3"


    /////////////////////////////////////////////////////////////////////////////
    // joinWith                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def joinWith01(): Bool =
        (Nil: List[Int32]) |> List.toSet |>
            Set.joinWith(x -> "${x + 1}", ",") == ""

    @Test
    def joinWith02(): Bool =
        (1 :: Nil) |> List.toSet |>
            Set.joinWith(x -> "${x + 1}", ",") == "2"

    @Test
    def joinWith03(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List.toSet |>
            Set.joinWith(x -> "${x + 1}", ",") == "2,3,4"

    @Test
    def joinWith04(): Bool =
        ("1" :: "2" :: "3" :: Nil) |> List.toSet |>
            Set.joinWith(x -> x + x, ",") == "11,22,33"

}

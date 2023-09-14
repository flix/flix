/*
 * Copyright 2022 Jakob Schneider Villumsen
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

mod TestUnorderedFoldable {

    // Most tests are copied over from the `Set` test suite
    // and optionally adapted to types with a `CommutativeMonoid` instance.

    /////////////////////////////////////////////////////////////////////////////
    // fold                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def fold01(): Bool =
        UnorderedFoldable.fold(Set#{}: Set[Unit]) == ()

    @Test
    def fold02(): Bool =
        UnorderedFoldable.fold(Set#{}: Set[Int32]) == 0

    @Test
    def fold03(): Bool =
        UnorderedFoldable.fold(Set#{1, 2, 3}) == 6

    @Test
    def fold04(): Bool =
        UnorderedFoldable.fold(Set#{(1, 2), (3, 4)}) == (4, 6)


    /////////////////////////////////////////////////////////////////////////////
    // isEmpty                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isEmpty01(): Bool =
        UnorderedFoldable.isEmpty(Set#{}: Set[Unit]) == true

    @Test
    def isEmpty02(): Bool =
        UnorderedFoldable.isEmpty(Set#{1}) == false

    @Test
    def isEmpty03(): Bool =
        UnorderedFoldable.isEmpty(Set#{1, 2}) == false

    @Test
    def isEmpty04(): Bool =
        UnorderedFoldable.isEmpty(Set#{1, 2, 3}) == false

    @Test
    def isEmpty05(): Bool =
        UnorderedFoldable.isEmpty(Set#{1, 2, 3, 4, 5, 6, 7, 8}) == false


    /////////////////////////////////////////////////////////////////////////////
    // exists                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def exists01(): Bool =
        UnorderedFoldable.exists(x -> x `Int32.rem` 8 == 7, Set#{}) == false

    @Test
    def exists02(): Bool =
        UnorderedFoldable.exists(x -> x `Int32.rem` 8 == 7, Set#{5}) == false

    @Test
    def exists03(): Bool =
        UnorderedFoldable.exists(x -> x `Int32.rem` 8 == 7, Set#{7}) == true

    @Test
    def exists04(): Bool =
        UnorderedFoldable.exists(x -> x `Int32.rem` 8 == 7, Set#{15}) == true

    @Test
    def exists05(): Bool =
        UnorderedFoldable.exists(x -> x `Int32.rem` 8 == 7, Set#{1, 44}) == false

    @Test
    def exists06(): Bool =
        UnorderedFoldable.exists(x -> x `Int32.rem` 8 == 7, Set#{11, 71}) == true

    @Test
    def exists07(): Bool =
        UnorderedFoldable.exists(x -> x `Int32.rem` 8 == 7, Set#{71, 12}) == true

    @Test
    def exists08(): Bool =
        UnorderedFoldable.exists(x -> x `Int32.rem` 8 == 7, Set#{71, 79}) == true

    @Test
    def exists09(): Bool =
        UnorderedFoldable.exists(x -> x `Int32.rem` 8 == 7, Set#{11, -1, -14, -2, 84, 113}) == false

    @Test
    def exists10(): Bool =
        UnorderedFoldable.exists(x -> x `Int32.rem` 8 == 7, Set#{11, -1, 31, -14, -2, 84, 111}) == true

    @Test
    def exists11(): Bool =
        UnorderedFoldable.exists(x -> x `Int32.rem` 8 == 7, Set#{11, -1, -14, -2, 84, 111, 38}) == true


    /////////////////////////////////////////////////////////////////////////////
    // forAll                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forAll01(): Bool =
        UnorderedFoldable.forAll(x -> x `Int32.rem` 8 == 7, Set#{}) == true

    @Test
    def forAll02(): Bool =
        UnorderedFoldable.forAll(x -> x `Int32.rem` 8 == 7, Set#{5}) == false

    @Test
    def forAll03(): Bool =
        UnorderedFoldable.forAll(x -> x `Int32.rem` 8 == 7, Set#{7}) == true

    @Test
    def forAll04(): Bool =
        UnorderedFoldable.forAll(x -> x `Int32.rem` 8 == 7, Set#{15}) == true

    @Test
    def forAll05(): Bool =
        UnorderedFoldable.forAll(x -> x `Int32.rem` 8 == 7, Set#{1, 44}) == false

    @Test
    def forAll06(): Bool =
        UnorderedFoldable.forAll(x -> x `Int32.rem` 8 == 7, Set#{11, 71}) == false

    @Test
    def forAll07(): Bool =
        UnorderedFoldable.forAll(x -> x `Int32.rem` 8 == 7, Set#{71, 12}) == false

    @Test
    def forAll08(): Bool =
        UnorderedFoldable.forAll(x -> x `Int32.rem` 8 == 7, Set#{71, 79}) == true

    @Test
    def forAll09(): Bool =
        UnorderedFoldable.forAll(x -> x `Int32.rem` 8 == 7, Set#{7, 15, 23, 84, 111}) == false

    @Test
    def forAll10(): Bool =
        UnorderedFoldable.forAll(x -> x `Int32.rem` 8 == 7, Set#{7, 15, 23, 111, 3}) == false

    @Test
    def forAll11(): Bool =
        UnorderedFoldable.forAll(x -> x `Int32.rem` 8 == 7, Set#{7, 15, 23, 119, 111}) == true


    /////////////////////////////////////////////////////////////////////////////
    // memberOf                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def memberOf01(): Bool =
        UnorderedFoldable.memberOf(1, Set#{}) == false

    @Test
    def memberOf02(): Bool =
        UnorderedFoldable.memberOf(1, Set#{2}) == false

    @Test
    def memberOf03(): Bool =
        UnorderedFoldable.memberOf(1, Set#{1}) == true

    @Test
    def memberOf04(): Bool =
        UnorderedFoldable.memberOf(1, Set#{1, 2}) == true

    @Test
    def memberOf05(): Bool =
        UnorderedFoldable.memberOf(2, Set#{1, 2}) == true

    @Test
    def memberOf06(): Bool =
        UnorderedFoldable.memberOf(0, Set#{1, 2}) == false

    @Test
    def memberOf07(): Bool =
        UnorderedFoldable.memberOf(3, Set#{1, 2}) == false

    @Test
    def memberOf08(): Bool =
        UnorderedFoldable.memberOf(0, Set#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}) == false

    @Test
    def memberOf09(): Bool =
        UnorderedFoldable.memberOf(1, Set#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}) == true

    @Test
    def memberOf10(): Bool =
        UnorderedFoldable.memberOf(2, Set#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}) == true

    @Test
    def memberOf11(): Bool =
        UnorderedFoldable.memberOf(10, Set#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}) == true

    @Test
    def memberOf12(): Bool =
        UnorderedFoldable.memberOf(12, Set#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}) == false


    /////////////////////////////////////////////////////////////////////////////
    // size                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def size01(): Bool =
        UnorderedFoldable.size(Set#{}) == 0

    @Test
    def size02(): Bool =
        UnorderedFoldable.size(Set#{1}) == 1

    @Test
    def size03(): Bool =
        UnorderedFoldable.size(Set#{1, 2}) == 2

    @Test
    def size04(): Bool =
        UnorderedFoldable.size(Set#{1, 2, 3}) == 3

    @Test
    def size05(): Bool =
        UnorderedFoldable.size(Set#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 14}) == 13


    /////////////////////////////////////////////////////////////////////////////
    // count                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def count01(): Bool =
        UnorderedFoldable.count(i -> i > 3, Set#{}) == 0

    @Test
    def count02(): Bool =
        UnorderedFoldable.count(i -> i > 3, Set#{1}) == 0

    @Test
    def count03(): Bool =
        UnorderedFoldable.count(i -> i > 3, Set#{4}) == 1

    @Test
    def count04(): Bool =
        UnorderedFoldable.count(i -> i > 3, Set#{2, 1}) == 0

    @Test
    def count05(): Bool =
        UnorderedFoldable.count(i -> i > 3, Set#{8, 1}) == 1

    @Test
    def count06(): Bool =
        UnorderedFoldable.count(i -> i > 3, Set#{1, 8}) == 1

    @Test
    def count07(): Bool =
        UnorderedFoldable.count(i -> i > 3, Set#{7, 6}) == 2

    @Test
    def count08(): Bool =
        UnorderedFoldable.count(i -> i > 3, Set.range(0, 100)) == 96

}

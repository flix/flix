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

    @test
    def fold01(): Bool =
        UnorderedFoldable.fold((Set#{}: Set[Unit])) == ()

    @test
    def fold02(): Bool =
        UnorderedFoldable.fold((Set#{}: Set[Int32])) == 0

    @test
    def fold03(): Bool =
        UnorderedFoldable.fold(Set#{1, 2, 3}) == 6

    @test
    def fold04(): Bool =
        UnorderedFoldable.fold(Set#{(1, 2), (3, 4)}) == (4, 6)


    /////////////////////////////////////////////////////////////////////////////
    // isEmpty                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def isEmpty01(): Bool =
        UnorderedFoldable.isEmpty((Set#{}: Set[Unit])) == true

    @test
    def isEmpty02(): Bool =
        UnorderedFoldable.isEmpty(Set#{1}) == false

    @test
    def isEmpty03(): Bool =
        UnorderedFoldable.isEmpty(Set#{1, 2}) == false

    @test
    def isEmpty04(): Bool =
        UnorderedFoldable.isEmpty(Set#{1, 2, 3}) == false

    @test
    def isEmpty05(): Bool =
        UnorderedFoldable.isEmpty(Set#{1, 2, 3, 4, 5, 6, 7, 8}) == false


    /////////////////////////////////////////////////////////////////////////////
    // nonEmpty                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def nonEmpty01(): Bool =
        UnorderedFoldable.nonEmpty((Set#{}: Set[Unit])) == false

    @test
    def nonEmpty02(): Bool =
        UnorderedFoldable.nonEmpty(Set#{1}) == true

    @test
    def nonEmpty03(): Bool =
        UnorderedFoldable.nonEmpty(Set#{1, 2}) == true

    @test
    def nonEmpty04(): Bool =
        UnorderedFoldable.nonEmpty(Set#{1, 2, 3}) == true

    @test
    def nonEmpty05(): Bool =
        UnorderedFoldable.nonEmpty(Set#{1, 2, 3, 4, 5, 6, 7, 8}) == true


    /////////////////////////////////////////////////////////////////////////////
    // exists                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def exists01(): Bool =
        UnorderedFoldable.exists(x -> x `Int32.remainder` 8 == 7, Set#{}) == false

    @test
    def exists02(): Bool =
        UnorderedFoldable.exists(x -> x `Int32.remainder` 8 == 7, Set#{5}) == false

    @test
    def exists03(): Bool =
        UnorderedFoldable.exists(x -> x `Int32.remainder` 8 == 7, Set#{7}) == true

    @test
    def exists04(): Bool =
        UnorderedFoldable.exists(x -> x `Int32.remainder` 8 == 7, Set#{15}) == true

    @test
    def exists05(): Bool =
        UnorderedFoldable.exists(x -> x `Int32.remainder` 8 == 7, Set#{1, 44}) == false

    @test
    def exists06(): Bool =
        UnorderedFoldable.exists(x -> x `Int32.remainder` 8 == 7, Set#{11, 71}) == true

    @test
    def exists07(): Bool =
        UnorderedFoldable.exists(x -> x `Int32.remainder` 8 == 7, Set#{71, 12}) == true

    @test
    def exists08(): Bool =
        UnorderedFoldable.exists(x -> x `Int32.remainder` 8 == 7, Set#{71, 79}) == true

    @test
    def exists09(): Bool =
        UnorderedFoldable.exists(x -> x `Int32.remainder` 8 == 7, Set#{11, -1, -14, -2, 84, 113}) == false

    @test
    def exists10(): Bool =
        UnorderedFoldable.exists(x -> x `Int32.remainder` 8 == 7, Set#{11, -1, 31, -14, -2, 84, 111}) == true

    @test
    def exists11(): Bool =
        UnorderedFoldable.exists(x -> x `Int32.remainder` 8 == 7, Set#{11, -1, -14, -2, 84, 111, 38}) == true


    /////////////////////////////////////////////////////////////////////////////
    // forAll                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def forAll01(): Bool =
        UnorderedFoldable.forAll(x -> x `Int32.remainder` 8 == 7, Set#{}) == true

    @test
    def forAll02(): Bool =
        UnorderedFoldable.forAll(x -> x `Int32.remainder` 8 == 7, Set#{5}) == false

    @test
    def forAll03(): Bool =
        UnorderedFoldable.forAll(x -> x `Int32.remainder` 8 == 7, Set#{7}) == true

    @test
    def forAll04(): Bool =
        UnorderedFoldable.forAll(x -> x `Int32.remainder` 8 == 7, Set#{15}) == true

    @test
    def forAll05(): Bool =
        UnorderedFoldable.forAll(x -> x `Int32.remainder` 8 == 7, Set#{1, 44}) == false

    @test
    def forAll06(): Bool =
        UnorderedFoldable.forAll(x -> x `Int32.remainder` 8 == 7, Set#{11, 71}) == false

    @test
    def forAll07(): Bool =
        UnorderedFoldable.forAll(x -> x `Int32.remainder` 8 == 7, Set#{71, 12}) == false

    @test
    def forAll08(): Bool =
        UnorderedFoldable.forAll(x -> x `Int32.remainder` 8 == 7, Set#{71, 79}) == true

    @test
    def forAll09(): Bool =
        UnorderedFoldable.forAll(x -> x `Int32.remainder` 8 == 7, Set#{7, 15, 23, 84, 111}) == false

    @test
    def forAll10(): Bool =
        UnorderedFoldable.forAll(x -> x `Int32.remainder` 8 == 7, Set#{7, 15, 23, 111, 3}) == false

    @test
    def forAll11(): Bool =
        UnorderedFoldable.forAll(x -> x `Int32.remainder` 8 == 7, Set#{7, 15, 23, 119, 111}) == true


    /////////////////////////////////////////////////////////////////////////////
    // memberOf                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def memberOf01(): Bool =
        UnorderedFoldable.memberOf(1, Set#{}) == false

    @test
    def memberOf02(): Bool =
        UnorderedFoldable.memberOf(1, Set#{2}) == false

    @test
    def memberOf03(): Bool =
        UnorderedFoldable.memberOf(1, Set#{1}) == true

    @test
    def memberOf04(): Bool =
        UnorderedFoldable.memberOf(1, Set#{1, 2}) == true

    @test
    def memberOf05(): Bool =
        UnorderedFoldable.memberOf(2, Set#{1, 2}) == true

    @test
    def memberOf06(): Bool =
        UnorderedFoldable.memberOf(0, Set#{1, 2}) == false

    @test
    def memberOf07(): Bool =
        UnorderedFoldable.memberOf(3, Set#{1, 2}) == false

    @test
    def memberOf08(): Bool =
        UnorderedFoldable.memberOf(0, Set#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}) == false

    @test
    def memberOf09(): Bool =
        UnorderedFoldable.memberOf(1, Set#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}) == true

    @test
    def memberOf10(): Bool =
        UnorderedFoldable.memberOf(2, Set#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}) == true

    @test
    def memberOf11(): Bool =
        UnorderedFoldable.memberOf(10, Set#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}) == true

    @test
    def memberOf12(): Bool =
        UnorderedFoldable.memberOf(12, Set#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}) == false


    /////////////////////////////////////////////////////////////////////////////
    // size                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def size01(): Bool =
        UnorderedFoldable.size(Set#{}) == 0

    @test
    def size02(): Bool =
        UnorderedFoldable.size(Set#{1}) == 1

    @test
    def size03(): Bool =
        UnorderedFoldable.size(Set#{1, 2}) == 2

    @test
    def size04(): Bool =
        UnorderedFoldable.size(Set#{1, 2, 3}) == 3

    @test
    def size05(): Bool =
        UnorderedFoldable.size(Set#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 14}) == 13


    /////////////////////////////////////////////////////////////////////////////
    // count                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def count01(): Bool =
        UnorderedFoldable.count(i -> i > 3, Set#{}) == 0

    @test
    def count02(): Bool =
        UnorderedFoldable.count(i -> i > 3, Set#{1}) == 0

    @test
    def count03(): Bool =
        UnorderedFoldable.count(i -> i > 3, Set#{4}) == 1

    @test
    def count04(): Bool =
        UnorderedFoldable.count(i -> i > 3, Set#{2, 1}) == 0

    @test
    def count05(): Bool =
        UnorderedFoldable.count(i -> i > 3, Set#{8, 1}) == 1

    @test
    def count06(): Bool =
        UnorderedFoldable.count(i -> i > 3, Set#{1, 8}) == 1

    @test
    def count07(): Bool =
        UnorderedFoldable.count(i -> i > 3, Set#{7, 6}) == 2

    @test
    def count08(): Bool =
        UnorderedFoldable.count(i -> i > 3, Set.range(0, 100)) == 96

}

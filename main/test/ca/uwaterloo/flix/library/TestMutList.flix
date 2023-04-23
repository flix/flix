/*
 * Copyright 2020 Esben Bjerre
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

mod TestMutList {
    use ToString.toString
    use MutList.MutList
    use MutList.sameElements

    ///
    /// Helper function to test capacity of internal array.
    ///
    def capacity(v: MutList[a, r]): Int32 \ Read(r) =
        let MutList(_, a, _) = v;
        Array.length(deref a)

    /////////////////////////////////////////////////////////////////////////////
    // toString                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toString01(): Bool = region r {
        let l: MutList[Int32, r] = MutList.new(r);
        MutList.toString(l) == "MutList#{}"
    }

    @test
    def toString02(): Bool = region r {
        let l: MutList[Int32, r] = MutList.new(r);
        MutList.push!(1, l);
        MutList.toString(l) == "MutList#{1}"
    }

    @test
    def toString03(): Bool = region r {
        let l: MutList[Int32, r] = MutList.new(r);
        MutList.push!(1, l);
        MutList.push!(2, l);
        MutList.push!(3, l);
        MutList.push!(4, l);
        MutList.push!(5, l);
        MutList.toString(l) == "MutList#{1, 2, 3, 4, 5}"
    }

    /////////////////////////////////////////////////////////////////////////////
    // sameElements                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sameElements01(): Bool = region r {
        let u: MutList[Int32, _] = MutList.new(r);
        let v: MutList[Int32, _] = MutList.new(r);
        u `sameElements` v
    }

    @test
    def sameElements02(): Bool = region r {
        let u = MutList.new(r);
        MutList.push!(0, u);
        let v = MutList.new(r);
        (u `sameElements` v) == false
    }

    @test
    def sameElements03(): Bool = region r {
        let u = MutList.new(r);
        MutList.push!(2, u);
        MutList.push!(3, u);
        let v = MutList.new(r);
        MutList.push!(2, v);
        MutList.push!(3, v);
        u `sameElements` v
    }

    @test
    def sameElements04(): Bool = region r {
        let u = MutList.new(r);
        MutList.push!(2, u);
        MutList.push!(3, u);
        let v = MutList.new(r);
        MutList.push!(2, v);
        MutList.push!(2, v);
        (u `sameElements` v) == false
    }

    @test
    def sameElements05(): Bool = region r {
        let u = MutList.new(r);
        MutList.push!(1, u);
        let v = MutList.new(r);
        (u `sameElements` v) == false
    }


    /////////////////////////////////////////////////////////////////////////////
    // Order.compare                                                           //
    /////////////////////////////////////////////////////////////////////////////

    // TODO


    /////////////////////////////////////////////////////////////////////////////
    // nth                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def nth01(): Bool = region r {
        MutList.nth(0, MutList.new(r): MutList[Int32, _]) == None
    }

    @test
    def nth02(): Bool = region r {
        let v = MutList.new(r);
        MutList.push!(42, v);
        MutList.nth(0, v) == Some(42)
    }

    @test
    def nth03(): Bool = region r {
        let v = MutList.new(r);
        MutList.push!(42, v);
        MutList.nth(1, v) == None
    }

    @test
    def nth04(): Bool = region r {
        MutList.nth(1, MutList.range(r, 0, 2)) == Some(1)
    }

    @test
    def nth05(): Bool = region r {
        MutList.nth(3, MutList.range(r, 0, 4)) == Some(3)
    }

    @test
    def nth06(): Bool = region r {
        MutList.nth(30, MutList.range(r, 0, 42)) == Some(30)
    }

    @test
    def nth07(): Bool = region r {
        MutList.nth(-30, MutList.range(r, 0, 42)) == None
    }

    @test
    def nth08(): Bool = region r {
        MutList.nth(50, MutList.range(r, 0, 42)) == None
    }


    /////////////////////////////////////////////////////////////////////////////
    // length                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def length01(): Bool = region r {
        MutList.length(MutList.new(r)) == 0
    }

    @test
    def length02(): Bool = region r {
        let v = MutList.new(r);
        MutList.push!(42, v);
        MutList.length(v) == 1
    }

    @test
    def length03(): Bool = region r {
        MutList.length(MutList.range(r, 0, 2)) == 2
    }

    @test
    def length04(): Bool = region r {
        MutList.length(MutList.range(r, 0, 4)) == 4
    }

    @test
    def length05(): Bool = region r {
        MutList.length(MutList.range(r, 0, 42)) == 42
    }


    /////////////////////////////////////////////////////////////////////////////
    // isEmpty                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def isEmpty01(): Bool = region r {
        MutList.isEmpty(MutList.new(r))
    }

    @test
    def isEmpty02(): Bool = region r {
        MutList.isEmpty(MutList.range(r, 0, 0))
    }

    @test
    def isEmpty03(): Bool = region r {
        MutList.isEmpty(MutList.range(r, 0, 1)) == false
    }

    @test
    def isEmpty04(): Bool = region r {
        MutList.isEmpty(MutList.range(r, 0, 2)) == false
    }

    @test
    def isEmpty05(): Bool = region r {
        MutList.isEmpty(MutList.range(r, 3, 4)) == false
    }


    /////////////////////////////////////////////////////////////////////////////
    // memberOf                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def memberOf01(): Bool = region r {
        MutList.memberOf(42, MutList.new(r)) == false
    }

    @test
    def memberOf02(): Bool = region r {
        let v = MutList.new(r);
        MutList.push!(2, v);
        MutList.memberOf(2, v)
    }

    @test
    def memberOf03(): Bool = region r {
        let v = MutList.new(r);
        MutList.push!(42, v);
        MutList.memberOf(2, v) == false
    }

    @test
    def memberOf04(): Bool = region r {
        MutList.memberOf(4, MutList.range(r, 0, 4)) == false
    }

    @test
    def memberOf05(): Bool = region r {
        MutList.memberOf(0, MutList.range(r, 0, 3))
    }

    @test
    def memberOf06(): Bool = region r {
        MutList.memberOf(2, MutList.range(r, 0, 3))
    }


    /////////////////////////////////////////////////////////////////////////////
    // count                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def count01(): Bool = region r {
        MutList.count(x -> x > 2, MutList.range(r, 0, 4)) == 1
    }

    @test
    def count02(): Bool = region r {
        MutList.count(x -> x >= 2, MutList.range(r, 0, 4)) == 2
    }

    @test
    def count03(): Bool = region r {
        MutList.count(x -> x `Int32.rem` 2 == 0, MutList.range(r, 0, 10)) == 5
    }

    @test
    def count04(): Bool = region r {
        MutList.count(x -> x == 11, MutList.range(r, 0, 21)) == 1
    }


    /////////////////////////////////////////////////////////////////////////////
    // exists                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def exists01(): Bool = region r {
        MutList.exists(x -> x > 9, MutList.range(r, 0, 9)) == false
    }

    @test
    def exists02(): Bool = region r {
        MutList.exists(x -> x < 0, MutList.range(r, 0, 9)) == false
    }

    @test
    def exists03(): Bool = region r {
        MutList.exists(x -> x == 8, MutList.range(r, 0, 9))
    }

    @test
    def exists04(): Bool = region r {
        MutList.exists(x -> x > 0, MutList.range(r, 0, 9))
    }

    @test
    def exists05(): Bool = region r {
        MutList.exists(x -> x == 0, MutList.new(r)) == false
    }


    /////////////////////////////////////////////////////////////////////////////
    // forAll                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def forAll01(): Bool = region r {
        MutList.forAll(x -> x > 0, MutList.range(r, 0, 9)) == false
    }

    @test
    def forAll02(): Bool = region r {
        MutList.forAll(x -> x >= 0, MutList.range(r, 0, 9))
    }

    @test
    def forAll03(): Bool = region r {
        MutList.forAll(x -> x >= 0 and x < 9, MutList.range(r, 0, 9))
    }

    @test
    def forAll05(): Bool = region r {
        MutList.forAll(x -> x > 0, MutList.new(r))
    }


    /////////////////////////////////////////////////////////////////////////////
    // head                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def head01(): Bool = region r {
        let v = MutList.new(r);
        MutList.push!(42, v);
        MutList.head(v) == Some(42)
    }

    @test
    def head02(): Bool = region r {
        MutList.head(MutList.new(r): MutList[Unit, _]) == None
    }

    @test
    def head03(): Bool = region r {
        let v = MutList.new(r);
        MutList.push!(101, v);
        MutList.push!(102, v);
        MutList.head(v) == Some(101)
    }


    /////////////////////////////////////////////////////////////////////////////
    // last                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def last01(): Bool = region r {
        MutList.last(MutList.new(r): MutList[Unit, _]) == None
    }

    @test
    def last02(): Bool = region r {
        MutList.last(MutList.range(r, 0, 8)) == Some(7)
    }

    @test
    def last03(): Bool = region r {
        let v = MutList.new(r);
        MutList.push!('A', v);
        MutList.push!('B', v);
        MutList.last(v) == Some('B')
    }


    /////////////////////////////////////////////////////////////////////////////
    // indexOfLeft                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def indexOfLeft01(): Bool = region r {
        MutList.indexOfLeft(2, MutList.range(r, 2, 3)) == Some(0)
    }

    @test
    def indexOfLeft02(): Bool = region r {
        MutList.indexOfLeft(10, MutList.range(r, 0, 10)) == None
    }

    @test
    def indexOfLeft03(): Bool = region r {
        let v = MutList.new(r);
        MutList.push!('A', v);
        MutList.push!('B', v);
        MutList.push!('C', v);
        MutList.indexOfLeft('B', v) == Some(1)
    }

    @test
    def indexOfLeft04(): Bool = region r {
        MutList.indexOfLeft(99, MutList.range(r, 0, 100)) == Some(99)
    }

    /////////////////////////////////////////////////////////////////////////////
    // indexOfRight                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def indexOfRight01(): Bool = region r {
        MutList.indexOfRight(2, MutList.range(r, 2, 3)) == Some(0)
    }

    @test
    def indexOfRight02(): Bool = region r {
        MutList.indexOfRight(10, MutList.range(r, 0, 10)) == None
    }

    @test
    def indexOfRight03(): Bool = region r {
        let v = MutList.new(r);
        MutList.push!('A', v);
        MutList.push!('B', v);
        MutList.push!('A', v);
        MutList.indexOfRight('A', v) == Some(2)
    }

    @test
    def indexOfRight04(): Bool = region r {
        let v1 = MutList.range(r, 0, 4);
        let v2 = MutList.range(r, 0, 4);
        MutList.append!(MutList.toList(v2), v1);
        MutList.indexOfRight(2, v1) == Some(6)
    }

    /////////////////////////////////////////////////////////////////////////////
    // findLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def findLeft01(): Bool = region r {
        MutList.findLeft(x -> x > 3, MutList.range(r, 3, 11)) == Some(4)
    }

    @test
    def findLeft02(): Bool = region r {
        MutList.findLeft(x -> x > 10, MutList.range(r, 3, 11)) == None
    }

    @test
    def findLeft03(): Bool = region r {
        MutList.findLeft(x -> x < 3, MutList.range(r, 3, 11)) == None
    }

    @test
    def findLeft04(): Bool = region r {
        MutList.findLeft(x -> x `Int32.rem` 2 == 1, MutList.range(r, 3, 11)) == Some(3)
    }

    /////////////////////////////////////////////////////////////////////////////
    // findRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def findRight01(): Bool = region r {
        MutList.findRight(x -> x > 0, MutList.range(r, 2, 5)) == Some(4)
    }

    @test
    def findRight02(): Bool = region r {
        MutList.findRight(x -> x == 10, MutList.range(r, 9, 10)) == None
    }

    @test
    def findRight03(): Bool = region r {
        MutList.findRight(x -> x `Int32.rem` 2 == 0, MutList.range(r, 3, 4)) == None
    }

    @test
    def findRight04(): Bool = region r {
        MutList.findRight(x -> x < 5, MutList.range(r, 3, 11)) == Some(4)
    }


    /////////////////////////////////////////////////////////////////////////////
    // scanLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def scanLeft01(): Bool = region r {
        let v = MutList.scanLeft(r, (x, y) -> x + y, 0, MutList.new(r));
        MutList.length(v) == 1 and MutList.head(v) == Some(0)
    }

    @test
    def scanLeft02(): Bool = region r {
        let v1 = MutList.range(r, 1, 6);
        let v2 = MutList.new(r);
        MutList.push!(0, v2);
        MutList.push!(1, v2);
        MutList.push!(3, v2);
        MutList.push!(6, v2);
        MutList.push!(10, v2);
        MutList.push!(15, v2);
        let v3 = MutList.scanLeft(r, (x, y) -> x + y, 0, v1);
        MutList.sameElements(v2, v3)
    }

    @test
    def scanLeft03(): Bool = region r {
        let v = MutList.scanLeft(r, (x, y) -> x + y, 7, MutList.range(r, 5, 13));
        MutList.last(v) == Some(75)
    }


    /////////////////////////////////////////////////////////////////////////////
    // scanRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def scanRight01(): Bool = region r {
        let v = MutList.scanRight(r, (x, y) -> x + y, 0, MutList.new(r));
        MutList.length(v) == 1 and MutList.head(v) == Some(0)
    }

    @test
    def scanRight02(): Bool = region r {
        let v1 = MutList.range(r, 1, 6);
        let v2 = MutList.new(r);
        MutList.push!(15, v2);
        MutList.push!(14, v2);
        MutList.push!(12, v2);
        MutList.push!(9, v2);
        MutList.push!(5, v2);
        MutList.push!(0, v2);
        let v3 = MutList.scanRight(r, (x, y) -> x + y, 0, v1);
        MutList.sameElements(v2, v3)
    }

    @test
    def scanRight03(): Bool = region r {
        let v = MutList.scanRight(r, (x, y) -> x + y, 7, MutList.range(r, 5, 13));
        MutList.last(v) == Some(7)
    }


    /////////////////////////////////////////////////////////////////////////////
    // map                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def map01(): Bool = region r {
        let v = MutList.map(r, x -> x * 2, MutList.range(r, 0, 6));
        MutList.sameElements(v, Array.toMutList(r, Array#{0, 2, 4, 6, 8, 10} @ r))
    }

    @test
    def map02(): Bool = region r {
        let v = MutList.map(r, identity, MutList.range(r, 0, 10));
        MutList.sameElements(v, MutList.range(r, 0, 10))
    }

    @test
    def map03(): Bool = region r {
        let v = MutList.map(r, identity, MutList.new(r): MutList[Unit, _]);
        MutList.sameElements(v, MutList.new(r))
    }

    @test
    def map04(): Bool = region r {
        let v = MutList.map(r, x -> x + 1, MutList.range(r, 0, 100));
        MutList.sameElements(v, MutList.range(r, 1, 101))
    }


    /////////////////////////////////////////////////////////////////////////////
    // transform!                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def transform!01(): Bool = region r {
        let v = MutList.range(r, 0, 6);
        MutList.transform!(x -> x * 2, v);
        MutList.sameElements(v, Array.toMutList(r, Array#{0, 2, 4, 6, 8, 10} @ r))
    }

    @test
    def transform!02(): Bool = region r {
        let v = MutList.range(r, 0, 10);
        MutList.transform!(identity, v);
        MutList.sameElements(v, MutList.range(r, 0, 10))
    }

    @test
    def transform!03(): Bool = region r {
        let v: MutList[Unit, _] = MutList.new(r);
        MutList.transform!(identity, v);
        MutList.sameElements(v, MutList.new(r))
    }

    @test
    def transform!04(): Bool = region r {
        let v = MutList.range(r, 0, 100);
        MutList.transform!(x -> x + 1, v);
        MutList.sameElements(v, MutList.range(r, 1, 101))
    }


    /////////////////////////////////////////////////////////////////////////////
    // mapWithIndex                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapWithIndex01(): Bool = region r {
        let v = MutList.mapWithIndex(r, (i, _) -> if (i < 3) 0 else 1, MutList.range(r, 0, 6));
        MutList.sameElements(v, Array.toMutList(r, Array#{0, 0, 0, 1, 1, 1} @ r))
    }

    @test
    def mapWithIndex02(): Bool = region r {
        let v = MutList.mapWithIndex(r, (i, x) -> if (x == 8 and i == 1) 42 else 0, MutList.range(r, 7, 10));
        MutList.sameElements(v, Array.toMutList(r, Array#{0, 42, 0} @ r))
    }

    @test
    def mapWithIndex03(): Bool = region r {
        let v = MutList.mapWithIndex(r, (i, _) -> if (i == 0) 42 else 0, MutList.new(r));
        MutList.sameElements(v, MutList.new(r))
    }


    /////////////////////////////////////////////////////////////////////////////
    // transformWithIndex!                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def transformWithIndex!01(): Bool = region r {
        let v = MutList.range(r, 0, 6);
        MutList.transformWithIndex!((i, _) -> if (i < 3) 0 else 1, v);
        MutList.sameElements(v, Array.toMutList(r, Array#{0, 0, 0, 1, 1, 1} @ r))
    }

    @test
    def transformWithIndex!02(): Bool = region r {
        let v = MutList.range(r, 7, 10);
        MutList.transformWithIndex!((i, x) -> if (x == 8 and i == 1) 42 else 0, v);
        MutList.sameElements(v, Array.toMutList(r, Array#{0, 42, 0} @ r))
    }

    @test
    def transformWithIndex!03(): Bool = region r {
        let v = MutList.new(r);
        MutList.transformWithIndex!((i, _) -> if (i == 0) 42 else 0, v);
        MutList.sameElements(v, MutList.new(r))
    }


    /////////////////////////////////////////////////////////////////////////////
    // foldLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldLeft01(): Bool = region r {
        MutList.foldLeft((acc, x) -> acc + x, 0, MutList.range(r, 1, 5)) == 10
    }

    @test
    def foldLeft02(): Bool = region r {
        let v1 = MutList.new(r);
        let v2 = MutList.range(r, 0, 10);
        MutList.foldLeft((_, x) -> MutList.push!(x, v1), (), v2);
        MutList.sameElements(v1, v2)
    }

    @test
    def foldLeft03(): Bool = region r {
        MutList.foldLeft((acc, x) -> x :: acc, Nil, MutList.range(r, 0, 6)) == 5 :: 4 :: 3 :: 2 :: 1 :: 0 :: Nil
    }

    @test
    def foldLeft04(): Bool = region r {
        MutList.foldLeft((acc, x) -> acc - x, 0, MutList.range(r, 0, 100)) == -4950
    }

    @test
    def foldLeft05(): Bool = region r {
        MutList.foldLeft((acc, x) -> acc + x, 42, MutList.new(r)) == 42
    }


    /////////////////////////////////////////////////////////////////////////////
    // foldRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldRight01(): Bool = region r {
        MutList.foldRight((x, acc) -> acc + x, 0, MutList.range(r, 1, 5)) == 10
    }

    @test
    def foldRight02(): Bool = region r {
        MutList.foldRight((x, acc) -> Int32.toString(x) + acc, "", MutList.range(r, 3, 8)) == "34567"
    }

    @test
    def foldRight03(): Bool = region r {
        MutList.foldRight((x, acc) -> x :: acc, Nil, MutList.range(r, 0, 6)) == 0 :: 1 :: 2 :: 3 :: 4 :: 5 :: Nil
    }

    @test
    def foldRight04(): Bool = region r {
        MutList.foldRight((x, acc) -> x - acc, 0, MutList.range(r, 0, 100)) == -50
    }

    @test
    def foldRight05(): Bool = region r {
        MutList.foldRight((x, acc) -> x + acc, 42, MutList.new(r)) == 42
    }

    /////////////////////////////////////////////////////////////////////////////
    // foldRightWithCont                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldRightWithCont01(): Bool = region r {
        MutList.foldRightWithCont((x, k) -> k() + x, 0, MutList.range(r, 1, 5)) == 10
    }

    @test
    def foldRightWithCont02(): Bool = region r {
        MutList.foldRightWithCont((x, k) -> Int32.toString(x) + k(), "", MutList.range(r, 3, 8)) == "34567"
    }

    @test
    def foldRightWithCont03(): Bool = region r {
        MutList.foldRightWithCont((x, k) -> x :: k(), Nil, MutList.range(r, 0, 6)) == 0 :: 1 :: 2 :: 3 :: 4 :: 5 :: Nil
    }

    @test
    def foldRightWithCont04(): Bool = region r {
        MutList.foldRightWithCont((x, k) -> x - k(), 0, MutList.range(r, 0, 100)) == -50
    }

    @test
    def foldRightWithCont05(): Bool = region r {
        MutList.foldRightWithCont((x, k) -> x + k(), 42, MutList.new(r)) == 42
    }


    /////////////////////////////////////////////////////////////////////////////
    // foldMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldMap01(): Bool = region r {
        MutList.foldMap(x -> 2 * x, List.toMutList(r, Nil: List[Int32])) == 0
    }

    @test
    def foldMap02(): Bool = region r {
        MutList.foldMap(x -> 2 * x, List.toMutList(r, 1 :: 2 :: Nil)) == 6
    }

    @test
    def foldMap03(): Bool = region r {
        MutList.foldMap(x -> if (x == "a") "b" else x, List.toMutList(r, "a" :: Nil)) == "b"
    }

    @test
    def foldMap04(): Bool = region r {
        MutList.foldMap(x -> if (x == "c") "b" else x, List.toMutList(r, "a" :: "b" :: "c":: Nil)) == "abb"
    }

    @test
    def foldMap05(): Bool = region r {
        MutList.foldMap(Int32.toString, List.toMutList(r, 1 :: 2 :: 3 :: Nil)) == "123"
    }


    /////////////////////////////////////////////////////////////////////////////
    // reduceLeft                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reduceLeft01(): Bool = region r {
        MutList.reduceLeft((x, y) -> x + y, MutList.new(r): MutList[Int32, _]) == None
    }

    @test
    def reduceLeft02(): Bool = region r {
        MutList.reduceLeft((x, y) -> x + y, MutList.range(r, 0, 5)) == Some(10)
    }

    @test
    def reduceLeft03(): Bool = region r {
        MutList.reduceLeft((x, y) -> x - y, MutList.range(r, 0, 101)) == Some(-5050)
    }

    @test
    def reduceLeft04(): Bool = region r {
        MutList.reduceLeft((_, y) -> if (y > 4) 1 else 0, MutList.range(r, 0, 6)) == Some(1)
    }


    /////////////////////////////////////////////////////////////////////////////
    // reduceRight                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reduceRight01(): Bool = region r {
        MutList.reduceRight((x, y) -> x + y, MutList.new(r): MutList[Int32, _]) == None
    }

    @test
    def reduceRight02(): Bool = region r {
        MutList.reduceRight((x, y) -> x + y, MutList.range(r, 0, 5)) == Some(10)
    }

    @test
    def reduceRight03(): Bool = region r {
        MutList.reduceRight((x, y) -> x - y, MutList.range(r, 0, 6)) == Some(-3)
    }

    @test
    def reduceRight04(): Bool = region r {
        MutList.reduceRight((x, _) -> if (x < 2) 1 else 0, MutList.range(r, 0, 6)) == Some(1)
    }


    /////////////////////////////////////////////////////////////////////////////
    // clear!                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def clear!01(): Bool = region r {
        let v = MutList.range(r, 0, 1);
        let len = MutList.length(v);
        MutList.clear!(v);
        len == 1 and MutList.isEmpty(v)
    }

    @test
    def clear!02(): Bool = region r {
        let v = MutList.new(r);
        let len = MutList.length(v);
        MutList.clear!(v);
        len == MutList.length(v)
    }

    @test
    def clear!03(): Bool = region r {
        let v = MutList.range(r, 0, 100);
        MutList.clear!(v);
        MutList.isEmpty(v)
    }


    /////////////////////////////////////////////////////////////////////////////
    // copy                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def copy01(): Bool = region r {
        let v1 = MutList.range(r, 0, 6);
        let v2 = MutList.copy(r, v1);
        v1 `sameElements` v2
    }

    @test
    def copy02(): Bool = region r {
        let v1 = MutList.range(r, 0, 100);
        let v2 = MutList.copy(r, v1);
        MutList.transform!(x -> x + 1, v2);
        MutList.last(v1) == Some(99) and MutList.last(v2) == Some(100)
    }

    @test
    def copy03(): Bool = region r {
        let v1 = MutList.range(r, 0, 6);
        let v2 = MutList.copy(r, v1);
        MutList.clear!(v1);
        MutList.length(v1) == 0 and MutList.length(v2) == 6
    }

    @test
    def copy04(): Bool = region r {
        let v1 = MutList.range(r, 0, 1000);
        let MutList(_, l1, n1) = v1;
        let MutList(_, l2, n2) = MutList.copy(r, v1);
        (deref n1 == deref n2) and (deref l1 `Array.sameElements` deref l2)
    }

    @test
    def copy05(): Bool = region r {
        let v1 = MutList.range(r, 0, 3);
        let MutList(_, l1, n1) = v1;
        let MutList(_, l2, n2) = MutList.copy(r, v1);
        (deref n1 == deref n2) and (deref l1 `Array.sameElements` deref l2) and
            (Array.length(deref l2) == 8)
    }


    /////////////////////////////////////////////////////////////////////////////
    // pop                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def pop01(): Bool = region r {
        let v = MutList.range(r, 0, 6);
        let len = MutList.length(v);
        let last = MutList.pop!(v);
        MutList.length(v) == len - 1 and last == Some(5)
    }

    @test
    def pop02(): Bool = region r {
        let v: MutList[Unit, _] = MutList.new(r);
        MutList.pop!(v) == None
    }

    @test
    def pop03(): Bool = region r {
        let v = MutList.range(r, 1, 4);
        let three = MutList.pop!(v);
        let two = MutList.pop!(v);
        let one = MutList.pop!(v);
        let none = MutList.pop!(v);
        three == Some(3) and two == Some(2) and one == Some(1) and none == None
    }


    /////////////////////////////////////////////////////////////////////////////
    // push                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def push01(): Bool = region r {
        let v = MutList.new(r);
        let len = MutList.length(v);
        MutList.push!(1, v);
        MutList.last(v) == Some(1) and MutList.length(v) == len + 1
    }

    @test
    def push02(): Bool = region r {
        let v = MutList.new(r);
        let len = MutList.length(v);
        List.forEach(x -> MutList.push!(x, v), List.range(0, 97));
        MutList.head(v) == Some(0) and MutList.last(v) == Some(96) and MutList.length(v) == len + 97
    }

    @test
    def push03(): Bool = region r {
        let v = MutList.new(r);
        MutList.push!(42, v);
        MutList.push!(31, v);
        MutList.push!(1019, v);
        MutList.length(v) == 3 and MutList.nth(0, v) == Some(42) and MutList.nth(1, v) == Some(31) and MutList.nth(2, v) == Some(1019)
    }

    @test
    def push04(): Bool = region r {
        let v = MutList.new(r);
        MutList.push!(0, v);
        MutList.push!(0, v);
        MutList.push!(0, v);
        MutList.push!(0, v);
        MutList.length(v) == 4 and MutList.nth(0, v) == Some(0) and MutList.nth(1, v) == Some(0) and MutList.nth(2, v) == Some(0) and MutList.nth(3, v) == Some(0)
    }


    /////////////////////////////////////////////////////////////////////////////
    // insert!                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def insert!01(): Bool = region r {
        let v = MutList.new(r);
        MutList.insert!(42, 0, v);
        MutList.length(v) == 1 and MutList.head(v) == Some(42)
    }

    @test
    def insert!02(): Bool = region r {
        let v = MutList.new(r);
        MutList.insert!(107, 0, v);
        MutList.insert!(7, 0, v);
        MutList.length(v) == 2 and MutList.head(v) == Some(7)
    }

    @test
    def insert!03(): Bool = region r {
        let v = MutList.new(r);
        List.forEach(x -> MutList.insert!(x, 0, v), List.range(0, 107));
        MutList.length(v) == 107 and MutList.head(v) == Some(106)
    }

    @test
    def insert!04(): Bool = region r {
        let v = MutList.new(r);
        MutList.insert!(42, 0, v);
        MutList.insert!(3, 1, v);
        MutList.insert!(5, 2, v);
        MutList.insert!(6, 0, v);
        MutList.length(v) == 4 and MutList.head(v) == Some(6) and MutList.last(v) == Some(5)
    }

    @test
    def insert!05(): Bool = region r {
        let v = MutList.new(r);
        List.forEach(x -> MutList.push!(x, v), List.range(97, 147));
        MutList.insert!(42, 0, v);
        MutList.insert!(49, 0, v);
        MutList.length(v) == 52 and MutList.head(v) == Some(49) and MutList.last(v) == Some(146)
    }

    @test
    def insert!06(): Bool = region r {
        let v = MutList.new(r);
        MutList.insert!(42, 0, v);
        MutList.insert!(42, 0, v);
        MutList.insert!(42, 0, v);
        MutList.insert!(42, 1, v);
        MutList.insert!(97, 0, v);
        MutList.insert!(98, 2, v);
        MutList.length(v) == 6 and MutList.head(v) == Some(97) and MutList.last(v) == Some(42)
    }

    @test
    def insert!07(): Bool = region r {
        let v = MutList.new(r);
        MutList.insert!(97, 0, v);
        MutList.insert!(98, 1, v);
        MutList.insert!(99, 2, v);
        MutList.insert!(107, 1, v);
        MutList.length(v) == 4 and MutList.nth(1, v) == Some(107)
    }

    @test
    def insert!08(): Bool = region r {
        let l1 = MutList.range(r, 0, 4);
        MutList.insert!(42, 3, l1);
        MutList.insert!(41, 2, l1);
        MutList.insert!(40, 1, l1);
        MutList.insert!(55, 0, l1);
        MutList.insert!(33, 1, l1);
        let l2 = MutList.new(r);
        let xs = 55 :: 33 :: 0 :: 40 :: 1 :: 41 :: 2 :: 42 :: 3 :: Nil;
        List.forEach(x -> MutList.push!(x, l2), xs);
        MutList.sameElements(l1, l2)
    }


    /////////////////////////////////////////////////////////////////////////////
    // remove!                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def remove!01(): Bool = region r {
        let v: MutList[Unit, _] = MutList.new(r);
        MutList.remove!(0, v);
        MutList.sameElements(v, MutList.new(r))
    }

    @test
    def remove!02(): Bool = region r {
        let v = MutList.range(r, 0, 4);
        MutList.remove!(0, v);
        MutList.sameElements(v, MutList.range(r, 1, 4))
    }

    @test
    def remove!03(): Bool = region r {
        let v = MutList.range(r, 0, 4);
        MutList.remove!(3, v);
        MutList.sameElements(v, MutList.range(r, 0, 3))
    }

    @test
    def remove!04(): Bool = region r {
        let v = MutList.range(r, 3, 7);
        MutList.remove!(0, v);
        MutList.remove!(0, v);
        MutList.remove!(0, v);
        MutList.sameElements(v, MutList.range(r, 6, 7))
    }

    @test
    def remove!05(): Bool = region r {
        let v = MutList.range(r, 9, 12);
        MutList.remove!(2, v);
        MutList.remove!(1, v);
        MutList.remove!(0, v);
        MutList.sameElements(v, MutList.new(r))
    }

    @test
    def remove!06(): Bool = region r {
        let v = MutList.range(r, 7, 13);
        MutList.remove!(0, v);
        MutList.remove!(0, v);
        MutList.remove!(0, v);
        MutList.remove!(0, v);
        MutList.remove!(0, v);
        MutList.remove!(0, v);
        MutList.sameElements(v, MutList.new(r))
    }

    @test
    def remove!07(): Bool = region r {
        let v = MutList.range(r, 0, 3);
        MutList.remove!(3, v);
        MutList.remove!(7, v);
        MutList.remove!(99, v);
        MutList.sameElements(v, MutList.range(r, 0, 3))
    }


    /////////////////////////////////////////////////////////////////////////////
    // pushAll!                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def pushAll!01(): Bool = region r {
        let v = MutList.range(r, 0, 4);
        let l = List.range(4, 8);
        MutList.pushAll!(l, v);
        MutList.sameElements(v, MutList.range(r, 0, 8))
    }

    @test
    def pushAll!02(): Bool = region r {
        let v = MutList.new(r);
        let l = List.range(1, 4);
        MutList.pushAll!(l, v);
        MutList.sameElements(v, MutList.range(r, 1, 4))
    }

    @test
    def pushAll!03(): Bool = region r {
        let v = MutList.range(r, 9, 24);
        let l = Nil: List[Int32];
        MutList.pushAll!(l, v);
        MutList.sameElements(v, MutList.range(r, 9, 24))
    }

    @test
    def pushAll!04(): Bool = region r {
        let v: MutList[Unit, _] = MutList.new(r);
        let l: List[Unit] = Nil;
        MutList.pushAll!(l, v);
        MutList.sameElements(v, MutList.new(r))
    }

    @test
    def pushAll!05(): Bool = region r {
        let v = MutList.range(r, 1, 3);
        let l1 = List.range(3, 5);
        let l2 = List.range(5, 99);
        MutList.pushAll!(l1, v);
        MutList.pushAll!(l2, v);
        MutList.sameElements(v, MutList.range(r, 1, 99))
    }


    /////////////////////////////////////////////////////////////////////////////
    // retain!                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def retain!01(): Bool = region r {
        let l1 = MutList.range(r, 0, 10);
        MutList.retain!(x -> x `Int32.rem` 2 == 0, l1);
        let l2 = MutList.new(r);
        let xs = 0 :: 2 :: 4 :: 6 :: 8 :: Nil;
        List.forEach(x -> MutList.push!(x, l2), xs);
        MutList.sameElements(l1, l2)
    }

    @test
    def retain!02(): Bool = region r {
        let l1 = MutList.range(r, 0, 10);
        MutList.retain!(x -> x `Int32.rem` 2 == 1, l1);
        let l2 = MutList.new(r);
        let xs = 1 :: 3 :: 5 :: 7 :: 9 :: Nil;
        List.forEach(x -> MutList.push!(x, l2), xs);
        MutList.sameElements(l1, l2)
    }

    @test
    def retain!03(): Bool = region r {
        let v = MutList.range(r, 0, 10);
        MutList.retain!(x -> x > 5, v);
        MutList.sameElements(v, MutList.range(r, 6, 10))
    }

    @test
    def retain!04(): Bool = region r {
        let v = MutList.range(r, 0, 10);
        MutList.retain!(x -> x > 10, v);
        MutList.sameElements(v, MutList.new(r))
    }


    /////////////////////////////////////////////////////////////////////////////
    // replace!                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def replace!01(): Bool = region r {
        let l1 = MutList.new(r);
        MutList.push!(1, l1);
        MutList.push!(2, l1);
        MutList.push!(2, l1);
        MutList.push!(1, l1);
        MutList.push!(1, l1);
        MutList.replace!(from = 1, to = 42, l1);
        let l2 = MutList.new(r);
        let xs = 42 :: 2 :: 2 :: 42 :: 42 :: Nil;
        List.forEach(x -> MutList.push!(x, l2), xs);
        MutList.sameElements(l1, l2)
    }

    @test
    def replace!02(): Bool = region r {
        let l1 = MutList.range(r, 4, 7);
        MutList.replace!(from = 4, to = 0, l1);
        let l2 = MutList.new(r);
        let xs = 0 :: 5 :: 6 :: Nil;
        List.forEach(x -> MutList.push!(x, l2), xs);
        MutList.sameElements(l1, l2)
    }

    @test
    def replace!03(): Bool = region r {
        let l1 = MutList.range(r, 4, 7);
        MutList.replace!(from = 5, to = 0, l1);
        let l2 = MutList.new(r);
        let xs = 4 :: 0 :: 6 :: Nil;
        List.forEach(x -> MutList.push!(x, l2), xs);
        MutList.sameElements(l1, l2)
    }

    @test
    def replace!04(): Bool = region r {
        let l1 = MutList.range(r, 4, 7);
        MutList.replace!(from = 6, to = 0, l1);
        let l2 = MutList.new(r);
        let xs = 4 :: 5 :: 0 :: Nil;
        List.forEach(x -> MutList.push!(x, l2), xs);
        MutList.sameElements(l1, l2)
    }

    @test
    def replace!05(): Bool = region r {
        let l1 = MutList.new(r);
        let xs1 = List.repeat(97, 'a');
        List.forEach(x -> MutList.push!(x, l1), xs1);
        MutList.replace!(from = 'a', to = 'z', l1);
        let l2 = MutList.new(r);
        let xs2 = List.repeat(97, 'z');
        List.forEach(x -> MutList.push!(x, l2), xs2);
        MutList.sameElements(l1, l2)
    }


    /////////////////////////////////////////////////////////////////////////////
    // reverse!                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reverse01!(): Bool = region r {
        let v: MutList[Unit, _] = MutList.new(r);
        MutList.reverse!(v);
        MutList.sameElements(v, MutList.new(r))
    }

    @test
    def reverse02!(): Bool = region r {
        let v = MutList.range(r, 0, 1);
        MutList.reverse!(v);
        MutList.sameElements(v, MutList.range(r, 0, 1))
    }

    @test
    def reverse03!(): Bool = region r {
        let l1 = MutList.range(r, 0, 5);
        MutList.reverse!(l1);
        let l2 = MutList.new(r);
        let xs = 4 :: 3 :: 2 :: 1 :: 0 :: Nil;
        List.forEach(x -> MutList.push!(x, l2), xs);
        MutList.sameElements(l1, l2)
    }

    @test
    def reverse04!(): Bool = region r {
        let l1 = MutList.range(r, 0, 6);
        MutList.reverse!(l1);
        let l2 = MutList.new(r);
        let xs = 5 :: 4 :: 3 :: 2 :: 1 :: 0 :: Nil;
        List.forEach(x -> MutList.push!(x, l2), xs);
        MutList.sameElements(l1, l2)
    }

    @test
    def reverse05!(): Bool = region r {
        let l = MutList.new(r);
        let xs = 7 :: 6 :: 5 :: 4 :: 3 :: 2 :: Nil;
        List.forEach(x -> MutList.push!(x, l), xs);
        MutList.reverse!(l);
        MutList.sameElements(l, MutList.range(r, 2, 8))
    }


    /////////////////////////////////////////////////////////////////////////////
    // shrink!                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def shrink!01(): Bool = region r {
        let v = MutList.new(r);
        MutList.push!(0, v);
        MutList.push!(1, v);
        MutList.push!(2, v);
        MutList.shrink!(v);
        MutList.length(v) == 3
    }

    @test
    def shrink!02(): Bool = region r {
        let v = MutList.new(r);
        MutList.shrink!(v);
        MutList.length(v) == 0
    }

    @test
    def shrink!03(): Bool = region r {
        let v = MutList.range(r, 0, 9);
        MutList.push!(9, v);
        MutList.shrink!(v);
        MutList.length(v) == 10
    }

    @test
    def shrink!04(): Bool = region r {
        let v = MutList.new(r);
        MutList.push!(4, v);
        MutList.append!(MutList.toList(MutList.range(r, 0, 4)), v);
        MutList.push!(5, v);
        MutList.shrink!(v);
        MutList.length(v) == 6
    }


    /////////////////////////////////////////////////////////////////////////////
    // truncate!                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def truncate!01(): Bool = region r {
        let v: MutList[Unit, _] = MutList.new(r);
        MutList.truncate!(0, v);
        MutList.sameElements(v, MutList.new(r))
    }

    @test
    def truncate!02(): Bool = region r {
        let v = MutList.range(r, 0, 10);
        MutList.truncate!(5, v);
        MutList.sameElements(v, MutList.range(r, 0, 5))
    }

    @test
    def truncate!03(): Bool = region r {
        let v = MutList.range(r, 0, 99);
        MutList.truncate!(-1, v);
        MutList.sameElements(v, MutList.new(r))
    }

    @test
    def truncate!04(): Bool = region r {
        let v = MutList.range(r, 7, 8);
        MutList.truncate!(2, v);
        MutList.sameElements(v, MutList.range(r, 7, 8))
    }

    @test
    def truncate!05(): Bool = region r {
        let v = MutList.range(r, 99, 102);
        MutList.truncate!(1, v);
        MutList.sameElements(v, MutList.range(r, 99, 100))
    }


    /////////////////////////////////////////////////////////////////////////////
    // reserve!                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reserve!01(): Bool = region r {
        let v = MutList.new(r);
        MutList.reserve!(4, v);
        capacity(v) - MutList.length(v) >= 4
    }

    @test
    def reserve!02(): Bool = region r {
        let v = MutList.range(r, 0, 5);
        MutList.reserve!(5, v);
        capacity(v) - MutList.length(v) >= 5
    }

    @test
    def reserve!03(): Bool = region r {
        let v = MutList.range(r, 0, 99);
        MutList.reserve!(1, v);
        capacity(v) - MutList.length(v) >= 1
    }

    @test
    def reserve!04(): Bool = region r {
        let v = MutList.range(r, 0, 100);
        MutList.reserve!(100, v);
        capacity(v) - MutList.length(v) >= 100
    }

    @test
    def reserve!05(): Bool = region r {
        let v = MutList.range(r, 0, 5);
        MutList.reserve!(MutList.length(v), v);
        capacity(v) - MutList.length(v) >= MutList.length(v)
    }

    @test
    def reserve!06(): Bool = region r {
        let n = 197;
        let v = MutList.range(r, 0, n);
        MutList.reserve!(1, v);
        MutList.reserve!(1, v);
        MutList.reserve!(1, v);
        MutList.reserve!(1, v);
        MutList.reserve!(1, v);
        capacity(v) - MutList.length(v) >= 1
    }


    /////////////////////////////////////////////////////////////////////////////
    // iterator                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def iterator01(): Bool = region r {
        let l = MutList.new(r): MutList[Int32, r];
        let iter = MutList.iterator(r, l);
        match Iterator.next(iter) {
            case None => true
            case _    => false
        }
    }

    @test
    def iterator02(): Bool = region r {
        let l = List.toMutList(r, 1 :: 2 :: 3 :: Nil);
        let iter = MutList.iterator(r, l);
        match Iterator.next(iter) {
            case Some(_) => true
            case _       => false
        }
    }

    @test
    def iterator03(): Bool = region r {
        let l = List.toMutList(r, 1 :: 2 :: 3 :: Nil);
        discard MutList.pop!(l);
        discard MutList.pop!(l);
        discard MutList.pop!(l);
        let iter = MutList.iterator(r, l);
        match Iterator.next(iter) {
            case None => true
            case _    => false
        }
    }

    @test
    def iterator04(): Bool = region r {
        let l = List.toMutList(r, 1 :: 2 :: 3 :: Nil);
        MutList.iterator(r, l) |> Iterator.toList == 1 :: 2 :: 3 :: Nil
    }

    @test
    def iterator05(): Bool = region r {
        let l = MutList.range(r, 0, 100);
        MutList.iterator(r, l) |> Iterator.toList == List.range(0, 100) and
        MutList.toList(l) == List.range(0, 100)
    }

    /////////////////////////////////////////////////////////////////////////////
    // forEach                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def forEach01(): Bool = region r {
        let v = MutList.new(r);
        let sb = StringBuilder.new(r);
        let f = x -> if (x > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        MutList.forEach(f, v);
        StringBuilder.toString(sb) == ""
    }

    @test
    def forEach02(): Bool = region r {
        let v = MutList.range(r, 0, 5);
        let sb = StringBuilder.new(r);
        let f = x -> if (x `Int32.rem` 2 == 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        MutList.forEach(f, v);
        StringBuilder.toString(sb) == "TFTFT"
    }

    @test
    def forEach03(): Bool = region r {
        let v = MutList.range(r, 0, 2);
        let sb = StringBuilder.new(r);
        let f = x -> if (x > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        MutList.forEach(f, v);
        StringBuilder.toString(sb) == "FT"
    }

    @test
    def forEach04(): Bool = region r {
        let v = MutList.range(r, 0, 2);
        let sb = StringBuilder.new(r);
        let f = x -> if (x < 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        MutList.forEach(f, v);
        StringBuilder.toString(sb) == "FF"
    }


    /////////////////////////////////////////////////////////////////////////////
    // forEachWithIndex                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def forEachWithIndex01(): Bool = region r {
        let v1 = MutList.range(r, 0, 100);
        let v2 = MutList.new(r);
        MutList.forEachWithIndex((i, x) -> if (x `Int32.rem` 2 == 0 and i < 10) MutList.push!(x, v2) else (), v1);
        let l = MutList.new(r);
        let xs = 0 :: 2 :: 4 :: 6 :: 8 :: Nil;
        List.forEach(x -> MutList.push!(x, l), xs);
        MutList.sameElements(v2, l)
    }

    @test
    def forEachWithIndex02(): Bool = region r {
        let v = MutList.range(r, 1, 4);
        let sb = StringBuilder.new(r);
        let f = (i, x) -> if (x `Int32.rem` 2 == 1 and i == 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        MutList.forEachWithIndex(f, v);
        StringBuilder.toString(sb) == "TFF"
    }

    @test
    def forEachWithIndex03(): Bool = region r {
        let v = MutList.range(r, 0, 5);
        let sb = StringBuilder.new(r);
        let f = (_, x) -> if (x `Int32.rem` 2 == 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        MutList.forEachWithIndex(f, v);
        StringBuilder.toString(sb) == "TFTFT"
    }


    /////////////////////////////////////////////////////////////////////////////
    // compress!                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def compress!01(): Bool = region r {
        // Pop 76 element such that the load factor is strictly lower than 1 / 4.
        let v = MutList.range(r, 0, 100);
        let c = capacity(v);
        let i = ref 0 @ r;
        let f = _ -> {
            if (deref i < 76) {
                discard MutList.pop!(v);
                i := deref i + 1
            }
            else ()
        };
        MutList.forEach(f, v);
        capacity(v) == c / 2 and
            MutList.length(v) == 100 - 75 - 1
    }

    @test
    def compress!02(): Bool = region r {
        let v = MutList.new(r);
        MutList.compress!(v);
        capacity(v) == capacity(MutList.new(r))
    }

    @test
    def compress!03(): Bool = region r {
        let v = MutList.new(r);
        let c = capacity(v);
        MutList.push!(42, v);
        MutList.compress!(v);
        capacity(v) <= c and
        MutList.length(v) == 1
    }

    @test
    def compress!04(): Bool = region r {
        let v = MutList.range(r, 0, 5);
        MutList.compress!(v);
        capacity(v) <= capacity(MutList.range(r, 0, 5))
    }


    /////////////////////////////////////////////////////////////////////////////
    // minimumBy                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def minimumBy01(): Bool = region r {
        MutList.minimumBy((x, y) -> x <=> y, MutList.new(r): MutList[Int32, _]) == None
    }

    @test
    def minimumBy02(): Bool = region r {
        MutList.minimumBy((x, y) -> x <=> y, MutList.range(r, 0, 5)) == Some(0)
    }

    @test
    def minimumBy03(): Bool = region r {
        MutList.minimumBy((x, y) -> x <=> y, MutList.range(r, 5, 10)) == Some(5)
    }

    @test
    def minimumBy04(): Bool = region r {
        let v = MutList.range(r, 0, 6);
        MutList.push!(-2, v);
        MutList.minimumBy((x, y) -> x <=> y, v) == Some(-2)
    }

    @test
    def minimumBy05(): Bool = region r {
        let v = MutList.range(r, 9, 19);
        let cmp = (x, y) -> if (x < y and x `Int32.rem` 2 == 0) Comparison.LessThan else Comparison.GreaterThan;
        MutList.minimumBy(cmp, v) == Some(10)
    }


    /////////////////////////////////////////////////////////////////////////////
    // maximumBy                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def maximumBy01(): Bool = region r {
        MutList.maximumBy((x, y) -> x <=> y, MutList.new(r): MutList[Int32, _]) == None
    }

    @test
    def maximumBy02(): Bool = region r {
        MutList.maximumBy((x, y) -> x <=> y, MutList.range(r, 0, 5)) == Some(4)
    }

    @test
    def maximumBy03(): Bool = region r {
        MutList.maximumBy((x, y) -> x <=> y, MutList.range(r, 5, 10)) == Some(9)
    }

    @test
    def maximumBy04(): Bool = region r {
        let v = MutList.range(r, 0, 6);
        MutList.push!(11, v);
        MutList.maximumBy((x, y) -> x <=> y, v) == Some(11)
    }

    @test
    def maximumBy05(): Bool = region r {
        let v = MutList.range(r, 9, 19);
        let cmp = (x, y) -> if (x > y and x `Int32.rem` 2 == 0) Comparison.GreaterThan else Comparison.LessThan;
        MutList.maximumBy(cmp, v) == Some(18)
    }

    /////////////////////////////////////////////////////////////////////////////
    // toArray                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toArray01(): Bool = region r {
        Array.sameElements(MutList.toArray(r, Array.toMutList(r, Array#{1, 2, 3} @ r)), Array#{1, 2, 3} @ r)
    }

    @test
    def toArray02(): Bool = region r {
        let v = Array.toMutList(r, Array#{1, 2, 3} @ r);
        let a = MutList.toArray(r, v);
        Array.put(42, 1, a);
        MutList.nth(1, v) == Some(2)
    }

    @test
    def toArray03(): Bool = region r {
        let v = Array.toMutList(r, Array#{1, 2, 3} @ r);
        discard MutList.pop!(v);
        discard MutList.pop!(v);
        Array.length(MutList.toArray(r, v)) == 1
    }

    /////////////////////////////////////////////////////////////////////////////
    // toVector                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toVector01(): Bool = region rc {
        MutList.toVector(MutList.new(rc) : MutList[Int32, _]) `Vector.equals` Vector.empty()
    }

    @test
    def toVector02(): Bool = region rc {
        let l = MutList.new(rc);
        MutList.push!(0, l);
        MutList.toVector(l) `Vector.equals` Vector.singleton(0)
    }

    @test
    def toVector03(): Bool = region rc {
        let l = MutList.new(rc);
        MutList.push!(1, l);
        MutList.push!(2, l);
        MutList.push!(3, l);
        MutList.toVector(l) `Vector.equals` List.toVector(1 :: 2 :: 3 :: Nil)
    }

    /////////////////////////////////////////////////////////////////////////////
    // toList                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toList01(): Bool = region r {
        MutList.new(r): MutList[Int32, _] |> MutList.toList == Nil: List[Int32]
    }

    @test
    def toList02(): Bool = region r {
        let ml = MutList.range(r, 0, 1000);
        ml |> MutList.toList == List.range(0, 1000) and
            not (MutList.isEmpty(ml))
    }

    @test
    def toList03(): Bool = region r {
        let ml = MutList.range(r, 0, 1000);
        ml |> MutList.toList == List.range(0, 1000) and
           ml `sameElements` MutList.range(r, 0, 1000)
    }


    /////////////////////////////////////////////////////////////////////////////
    // toChain                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toChain01(): Bool = region r {
        let l = MutList.new(r);
        let xs: List[Int32] = Nil;
        List.forEach(x -> MutList.push!(x, l), xs);
        MutList.toChain(l) == Chain.empty()
    }

    @test
    def toChain02(): Bool = region r {
        let l = MutList.new(r);
        let xs = 1 :: Nil;
        List.forEach(x -> MutList.push!(x, l), xs);
        MutList.toChain(l) == Chain.singleton(1)
    }

    @test
    def toChain03(): Bool = region r {
        let l = MutList.new(r);
        let xs = 1 :: 2 :: Nil;
        List.forEach(x -> MutList.push!(x, l), xs);
        MutList.toChain(l) == List.toChain(1 :: 2 :: Nil)
    }

    @test
    def toChain04(): Bool = region r {
        let l = MutList.new(r);
        let xs = 1 :: 2 :: 3 :: Nil;
        List.forEach(x -> MutList.push!(x, l), xs);
        MutList.toChain(l) == List.toChain(1 :: 2 :: 3 :: Nil)
    }


    /////////////////////////////////////////////////////////////////////////////
    // toMutDeque                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toMutDeque01(): Bool = region r {
        let l = MutList.new(r): MutList[Int32, _];
        let d = MutDeque.new(r): MutDeque[Int32, _];

        MutList.toMutDeque(r, l) `MutDeque.sameElements` d
    }

    @test
    def toMutDeque02(): Bool = region r {
        let xs = 1 :: Nil;

        let l = MutList.new(r);
        List.forEach(x -> MutList.push!(x, l), xs);

        let d = MutDeque.new(r);
        List.forEach(x -> MutDeque.pushBack(x, d), xs);

        MutList.toMutDeque(r, l) `MutDeque.sameElements` d
    }

    @test
    def toMutDeque03(): Bool = region r {
        let xs = 1 :: 2 :: Nil;

        let l = MutList.new(r);
        List.forEach(x -> MutList.push!(x, l), xs);

        let d = MutDeque.new(r);
        List.forEach(x -> MutDeque.pushBack(x, d), xs);

        MutList.toMutDeque(r, l) `MutDeque.sameElements` d
    }

    @test
    def toMutDeque04(): Bool = region r {
        let xs = 1 :: 2 :: 3 :: Nil;

        let l = MutList.new(r);
        List.forEach(x -> MutList.push!(x, l), xs);

        let d = MutDeque.new(r);
        List.forEach(x -> MutDeque.pushBack(x, d), xs);

        MutList.toMutDeque(r, l) `MutDeque.sameElements` d
    }


    /////////////////////////////////////////////////////////////////////////////
    // The following tests for sort functions correspond to those in TestArray //
    /////////////////////////////////////////////////////////////////////////////
    // sortWith                                                                //
    /////////////////////////////////////////////////////////////////////////////

    def cmp(x: Int32, y: Int32): Comparison =
        if (x < y) Comparison.LessThan
        else if (x == y) Comparison.EqualTo
        else Comparison.GreaterThan

    @test
    def sortWith01(): Bool = region r {
        let a = MutList.sortWith(r, cmp, Array.toMutList(r, (Array#{} @ r): Array[Int32, r]));
        a `sameElements` Array.toMutList(r, Array#{} @ r)
    }

    @test
    def sortWith02(): Bool = region r {
        let a = MutList.sortWith(r, cmp, Array.toMutList(r, Array#{0} @ r));
        a `sameElements` Array.toMutList(r, Array#{0} @ r)
    }

    @test
    def sortWith03(): Bool = region r {
        let a = MutList.sortWith(r, cmp, Array.toMutList(r, Array#{0, 1} @ r));
        a `sameElements` Array.toMutList(r, Array#{0,1} @ r)
    }

    @test
    def sortWith04(): Bool = region r {
        let a = MutList.sortWith(r, cmp, Array.toMutList(r, Array#{1,0} @ r));
        a `sameElements` Array.toMutList(r, Array#{0,1} @ r)
    }

    @test
    def sortWith05(): Bool = region r {
        let a = MutList.sortWith(r, cmp, Array.toMutList(r, Array#{1,1} @ r));
        a `sameElements` Array.toMutList(r, Array#{1,1} @ r)
    }

    @test
    def sortWith06(): Bool = region r {
        let a = MutList.sortWith(r, cmp, Array.toMutList(r, Array#{0,1,2,3,4,5} @ r));
        a `sameElements` Array.toMutList(r, Array#{0,1,2,3,4,5} @ r)
    }

    @test
    def sortWith07(): Bool = region r {
        let a = MutList.sortWith(r, cmp, Array.toMutList(r, Array#{5,4,3,2,1,0} @ r));
        a `sameElements` Array.toMutList(r, Array#{0,1,2,3,4,5} @ r)
    }

    @test
    def sortWith08(): Bool = region r {
        let a = MutList.sortWith(r, cmp, Array.toMutList(r, Array#{5,3,0,4,1,2} @ r));
        a `sameElements` Array.toMutList(r, Array#{0,1,2,3,4,5} @ r)
    }

    @test
    def sortWith09(): Bool = region r {
        let a = MutList.sortWith(r, cmp, Array.toMutList(r, Array#{2,3,0,4,1,2} @ r));
        a `sameElements` Array.toMutList(r, Array#{0,1,2,2,3,4} @ r)
    }

    @test
    def sortWith10(): Bool = region r {
        let a = MutList.sortWith(r, flip(cmp), Array.toMutList(r, Array#{0,1,2,3,4,5} @ r));
        a `sameElements` Array.toMutList(r, Array#{5,4,3,2,1,0} @ r)
    }

    @test
    def sortWith11(): Bool = region r {
        let a = MutList.sortWith(r, flip(cmp), Array.toMutList(r, Array#{5,4,3,2,1,0} @ r));
        a `sameElements` Array.toMutList(r, Array#{5,4,3,2,1,0} @ r)
    }

    @test
    def sortWith12(): Bool = region r {
        let a = MutList.sortWith(r, flip(cmp), Array.toMutList(r, Array#{5,3,0,4,1,2} @ r));
        a `sameElements` Array.toMutList(r, Array#{5,4,3,2,1,0} @ r)
    }


    @test
    def sortWith13(): Bool = region r {
        let a = MutList.sortWith(r, flip(cmp), Array.toMutList(r, Array#{2,3,0,4,1,2} @ r));
        a `sameElements` Array.toMutList(r, Array#{4,3,2,2,1,0} @ r)
    }

    /////////////////////////////////////////////////////////////////////////////
    // sort                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    def testSortVsSortWith(a: MutList[Int32, r]): Bool \ { Read(r) } = region r1 {
        MutList.sort(r1, a) `sameElements` MutList.sortWith(r1, cmp, a)
    }

    @test
    def sort01(): Bool = region r {
        testSortVsSortWith(Array.toMutList(r, (Array#{} @ r): Array[Int32, r]))
    }

    @test
    def sort02(): Bool = region r {
        testSortVsSortWith(Array.toMutList(r, Array#{0} @ r))
    }

    @test
    def sort03(): Bool = region r {
        testSortVsSortWith(Array.toMutList(r, Array#{0,1} @ r))
    }

    @test
    def sort04(): Bool = region r {
        testSortVsSortWith(Array.toMutList(r, Array#{1,0} @ r))
    }

    @test
    def sort05(): Bool = region r {
        testSortVsSortWith(Array.toMutList(r, Array#{1,1} @ r))
    }

    @test
    def sort06(): Bool = region r {
        testSortVsSortWith(Array.toMutList(r, Array#{0,1,2,3,4,5} @ r))
    }

    @test
    def sort07(): Bool = region r {
        testSortVsSortWith(Array.toMutList(r, Array#{5,4,3,2,1,0} @ r))
    }

    @test
    def sort08(): Bool = region r {
        testSortVsSortWith(Array.toMutList(r, Array#{5,3,0,4,1,2} @ r))
    }

    @test
    def sort09(): Bool = region r {
        testSortVsSortWith(Array.toMutList(r, Array#{2,3,0,4,1,2} @ r))
    }

    /////////////////////////////////////////////////////////////////////////////
    // sortWith!                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sortWith01!(): Bool = region r {
        let a = Array.toMutList(r, (Array#{} @ r): Array[Int32, r]);
        MutList.sortWith!(cmp, a);
        a `sameElements` Array.toMutList(r, (Array#{} @ r): Array[Int32, r])
    }

    @test
    def sortWith02!(): Bool = region r {
        let a = Array.toMutList(r, Array#{0} @ r);
        MutList.sortWith!(cmp, a);
        a `sameElements` Array.toMutList(r, Array#{0} @ r)
    }

    @test
    def sortWith03!(): Bool = region r {
        let a = Array.toMutList(r, Array#{0,1} @ r);
        MutList.sortWith!(cmp, a);
        a `sameElements` Array.toMutList(r, Array#{0,1} @ r)
    }

    @test
    def sortWith04!(): Bool = region r {
        let a = Array.toMutList(r, Array#{1,0} @ r);
        MutList.sortWith!(cmp, a);
        a `sameElements` Array.toMutList(r, Array#{0,1} @ r)
    }
    @test
    def sortWith05!(): Bool = region r {
        let a = Array.toMutList(r, Array#{1,1} @ r);
        MutList.sortWith!(cmp, a);
        a `sameElements` Array.toMutList(r, Array#{1,1} @ r)
    }

    @test
    def sortWith06!(): Bool = region r {
        let a = Array.toMutList(r, Array#{0,1,2,3,4,5} @ r);
        MutList.sortWith!(cmp, a);
        a `sameElements` Array.toMutList(r, Array#{0,1,2,3,4,5} @ r)
    }

    @test
    def sortWith07!(): Bool = region r {
        let a = Array.toMutList(r, Array#{5,4,3,2,1,0} @ r);
        MutList.sortWith!(cmp, a);
        a `sameElements` Array.toMutList(r, Array#{0,1,2,3,4,5} @ r)
    }

    @test
    def sortWith08!(): Bool = region r {
        let a = Array.toMutList(r, Array#{5,3,0,4,1,2} @ r);
        MutList.sortWith!(cmp, a);
        a `sameElements` Array.toMutList(r, Array#{0,1,2,3,4,5} @ r)
    }

    @test
    def sortWith09!(): Bool = region r {
        let a = Array.toMutList(r, Array#{2,3,0,4,1,2} @ r);
        MutList.sortWith!(cmp, a);
        a `sameElements` Array.toMutList(r, Array#{0,1,2,2,3,4} @ r)
    }

    @test
    def sortWith10!(): Bool = region r {
        let a = Array.toMutList(r, Array#{0,1,2,3,4,5} @ r);
        MutList.sortWith!(flip(cmp), a);
        a `sameElements` Array.toMutList(r, Array#{5,4,3,2,1,0} @ r)
    }

    @test
    def sortWith11!(): Bool = region r {
        let a = Array.toMutList(r, Array#{5,4,3,2,1,0} @ r);
        MutList.sortWith!(flip(cmp), a);
        a `sameElements` Array.toMutList(r, Array#{5,4,3,2,1,0} @ r)
    }

    @test
    def sortWith12!(): Bool = region r {
        let a = Array.toMutList(r, Array#{5,3,0,4,1,2} @ r);
        MutList.sortWith!(flip(cmp), a);
        a `sameElements` Array.toMutList(r, Array#{5,4,3,2,1,0} @ r)
    }

    @test
    def sortWith13!(): Bool = region r {
        let a = Array.toMutList(r, Array#{2,3,0,4,1,2} @ r);
        MutList.sortWith!(flip(cmp), a);
        a `sameElements` Array.toMutList(r, Array#{4,3,2,2,1,0} @ r)
    }

    /////////////////////////////////////////////////////////////////////////////
    // sort!                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    def testSort!VsSortWith!(a: MutList[Int32, r]): Bool \ { Read(r) } = region r1 {
        let b = MutList.copy(r1, a);
        let c = MutList.copy(r1, a);
        MutList.sort!(b);
        MutList.sortWith!(cmp, c);
        b `sameElements` c
    }

    @test
    def sort!01(): Bool = region r {
        testSort!VsSortWith!(Array.toMutList(r, (Array#{} @ r): Array[Int32, r]))
    }

    @test
    def sort!02(): Bool = region r {
        testSort!VsSortWith!(Array.toMutList(r, Array#{0} @ r))
    }

    @test
    def sort!03(): Bool = region r {
        testSort!VsSortWith!(Array.toMutList(r, Array#{0,1} @ r))
    }

    @test
    def sort!04(): Bool = region r {
        testSort!VsSortWith!(Array.toMutList(r, Array#{1,0} @ r))
    }

    @test
    def sort!05(): Bool = region r {
        testSort!VsSortWith!(Array.toMutList(r, Array#{1,1} @ r))
    }

    @test
    def sort!06(): Bool = region r {
        testSort!VsSortWith!(Array.toMutList(r, Array#{0,1,2,3,4,5} @ r))
    }

    @test
    def sort!07(): Bool = region r {
        testSort!VsSortWith!(Array.toMutList(r, Array#{5,4,3,2,1,0} @ r))
    }

    @test
    def sort!08(): Bool = region r {
        testSort!VsSortWith!(Array.toMutList(r, Array#{5,3,0,4,1,2} @ r))
    }

    @test
    def sort!09(): Bool = region r {
        testSort!VsSortWith!(Array.toMutList(r, Array#{2,3,0,4,1,2} @ r))
    }


    /////////////////////////////////////////////////////////////////////////////
    // sortBy                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    def testSortByVsSort(a: MutList[Int32, r]): Bool \ { Read(r) } = region r1 {
        (MutList.sortBy(r1, identity, a) `sameElements` MutList.sort(r1, a)) and
        (MutList.sortBy(r1, x -> 4 * x + 7, a) `sameElements` MutList.sort(r1, a)) and
        (MutList.sortBy(r1, x -> -x, a) `sameElements` MutList.sortWith(r1, flip(cmp),a))
    }

    @test
    def sortBy01(): Bool = region r {
        testSortByVsSort(Array.toMutList(r, (Array#{} @ r): Array[Int32, r]))
    }

    @test
    def sortBy02(): Bool = region r {
        testSortByVsSort(Array.toMutList(r, Array#{0} @ r))
    }

    @test
    def sortBy03(): Bool = region r {
        testSortByVsSort(Array.toMutList(r, Array#{0,1} @ r))
    }

    @test
    def sortBy04(): Bool = region r {
        testSortByVsSort(Array.toMutList(r, Array#{1,0} @ r))
    }

    @test
    def sortBy05(): Bool = region r {
        testSortByVsSort(Array.toMutList(r, Array#{1,1} @ r))
    }

    @test
    def sortBy06(): Bool = region r {
        testSortByVsSort(Array.toMutList(r, Array#{0,1,2,3,4,5} @ r))
    }

    @test
    def sortBy07(): Bool = region r {
        testSortByVsSort(Array.toMutList(r, Array#{5,4,3,2,1,0} @ r))
    }

    @test
    def sortBy08(): Bool = region r {
        testSortByVsSort(Array.toMutList(r, Array#{5,3,0,4,1,2} @ r))
    }

    @test
    def sortBy09(): Bool = region r {
        testSortByVsSort(Array.toMutList(r, Array#{2,3,0,4,1,2} @ r))
    }

    enum R {
        case R({i = Int32, s = String})
    }

    instance Eq[R] {
        pub def eq(a: R, b: R): Bool =
            let R.R(x) = a;
            let R.R(y) = b;
            x.i == y.i and x.s == y.s
    }

    @test
    def sortBy10(): Bool = region r {
        MutList.sortBy(r, rr -> let R.R(x) = rr; x.i, Array.toMutList(r, Array#{R.R({i = 2, s = "A"}), R.R({i = 1, s = "B"}), R.R({i = 3, s = "C"})} @ r))
        `sameElements` Array.toMutList(r, Array#{R.R({i = 1, s = "B"}), R.R({i = 2, s = "A"}), R.R({i = 3, s = "C"})} @ r)
    }


    /////////////////////////////////////////////////////////////////////////////
    // sortBy!                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    def testSortBy!VsSortBy(a: MutList[Int32, r]): Bool \ { Read(r) } = region r1 {
        let b = MutList.copy(r1, a);
        let c = MutList.copy(r1, a);
        MutList.sortBy!(identity, b);
        MutList.sortBy!(x -> 4 * x + 7, c);
        (b `sameElements` MutList.sortBy(r1, x -> 4 * x + 7, a)) and
        (c `sameElements` MutList.sortBy(r1, identity, a))
    }

    @test
    def sortBy!01(): Bool = region r {
        testSortBy!VsSortBy(Array.toMutList(r, (Array#{} @ r): Array[Int32, r]))
    }

    @test
    def sortBy!02(): Bool = region r {
        testSortBy!VsSortBy(Array.toMutList(r, Array#{0} @ r))
    }

    @test
    def sortBy!03(): Bool = region r {
        testSortBy!VsSortBy(Array.toMutList(r, Array#{0,1} @ r))
    }

    @test
    def sortBy!04(): Bool = region r {
        testSortBy!VsSortBy(Array.toMutList(r, Array#{1,0} @ r))
    }

    @test
    def sortBy!05(): Bool = region r {
        testSortBy!VsSortBy(Array.toMutList(r, Array#{1,1} @ r))
    }

    @test
    def sortBy!06(): Bool = region r {
        testSortBy!VsSortBy(Array.toMutList(r, Array#{0,1,2,3,4,5} @ r))
    }

    @test
    def sortBy!07(): Bool = region r {
        testSortBy!VsSortBy(Array.toMutList(r, Array#{5,4,3,2,1,0} @ r))
    }

    @test
    def sortBy!08(): Bool = region r {
        testSortBy!VsSortBy(Array.toMutList(r, Array#{5,3,0,4,1,2} @ r))
    }

    @test
    def sortBy!09(): Bool = region r {
        testSortBy!VsSortBy(Array.toMutList(r, Array#{2,3,0,4,1,2} @ r))
    }


    /////////////////////////////////////////////////////////////////////////////
    // sum                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sum01(): Bool = region r {
        MutList.new(r) |> MutList.sum == 0
    }

    @test
    def sum02(): Bool = region r {
        let l = MutList.new(r);
        let xs = 1 :: Nil;
        List.forEach(x -> MutList.push!(x, l), xs);
        MutList.sum(l) == 1
    }

    @test
    def sum03(): Bool = region r {
        let l = MutList.new(r);
        let xs = 1 :: 2 :: 3 :: Nil;
        List.forEach(x -> MutList.push!(x, l), xs);
        MutList.sum(l) == 6
    }

    @test
    def sum04(): Bool = region r {
        let l = MutList.new(r);
        let xs = 1 :: 2 :: 3 :: -3 :: Nil;
        List.forEach(x -> MutList.push!(x, l), xs);
        MutList.sum(l) == 3
    }

    @test
    def sum05(): Bool = region r {
        let l = MutList.new(r);
        let xs = -1 :: -2 :: -3 :: -4 :: Nil;
        List.forEach(x -> MutList.push!(x, l), xs);
        MutList.sum(l) == -10
    }

    @test
    def sum06(): Bool = region r {
        let l = MutList.new(r);
        let xs = 10 :: -10 :: Nil;
        List.forEach(x -> MutList.push!(x, l), xs);
        MutList.sum(l) == 0
    }

    @test
    def sum07(): Bool = region r {
        let l = MutList.new(r);
        let xs = List.range(1, 101);
        List.forEach(x -> MutList.push!(x, l), xs);
        MutList.sum(l) == 5050
    }


    /////////////////////////////////////////////////////////////////////////////
    // sumWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sumWith01(): Bool = region r {
        MutList.new(r) |> MutList.sumWith(x -> x + 1) == 0
    }

    @test
    def sumWith02(): Bool = region r {
        let l = MutList.new(r);
        let xs = 1 :: Nil;
        List.forEach(x -> MutList.push!(x, l), xs);
        MutList.sumWith(x -> x + 1, l) == 2
    }

    @test
    def sumWith03(): Bool = region r {
        let l = MutList.new(r);
        let xs = 1 :: 2 :: 3 :: Nil;
        List.forEach(x -> MutList.push!(x, l), xs);
        MutList.sumWith(x -> x + 1, l) == 9
    }

    @test
    def sumWith04(): Bool = region r {
        let l = MutList.new(r);
        let xs = 1 :: 2 :: 3 :: -3 :: Nil;
        List.forEach(x -> MutList.push!(x, l), xs);
        MutList.sumWith(x -> x + 1, l) == 7
    }

    @test
    def sumWith05(): Bool = region r {
        let l = MutList.new(r);
        let xs = -1 :: -2 :: -3 :: -4 :: Nil;
        List.forEach(x -> MutList.push!(x, l), xs);
        MutList.sumWith(x -> x + 1, l) == -6
    }

    @test
    def sumWith06(): Bool = region r {
        let l = MutList.new(r);
        let xs = 10 :: -10 :: Nil;
        List.forEach(x -> MutList.push!(x, l), xs);
        MutList.sumWith(x -> x + 1, l) == 2
    }


    /////////////////////////////////////////////////////////////////////////////
    // join                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def join01(): Bool = region r {
        (MutList.new(r): MutList[Int32, _]) |> MutList.join(",") == ""
    }

    @test
    def join02(): Bool = region r {
        let l = MutList.new(r);
        let xs = 1 :: Nil;
        List.forEach(x -> MutList.push!(x, l), xs);
        MutList.join(",", l) == "1"
    }

    @test
    def join03(): Bool = region r {
        let l = MutList.new(r);
        let xs = 1 :: 2 :: 3 :: Nil;
        List.forEach(x -> MutList.push!(x, l), xs);
        MutList.join(",", l) == "1,2,3"
    }

    @test
    def join04(): Bool = region r {
        let l = MutList.new(r);
        let xs = "1" :: "2" :: "3" :: Nil;
        List.forEach(x -> MutList.push!(x, l), xs);
        MutList.join(",", l) == "1,2,3"
    }


    /////////////////////////////////////////////////////////////////////////////
    // joinWith                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def joinWith01(): Bool = region r {
        (MutList.new(r): MutList[Int32, _]) |> MutList.joinWith(x -> "${x + 1}", ",") == ""
    }

    @test
    def joinWith02(): Bool = region r {
        let l = MutList.new(r);
        let xs = 1 :: Nil;
        List.forEach(x -> MutList.push!(x, l), xs);
        MutList.joinWith(x -> "${x + 1}", ",", l) == "2"
    }

    @test
    def joinWith03(): Bool = region r {
        let l = MutList.new(r);
        let xs = 1 :: 2 :: 3 :: Nil;
        List.forEach(x -> MutList.push!(x, l), xs);
        MutList.joinWith(x -> "${x + 1}", ",", l) == "2,3,4"
    }

    @test
    def joinWith04(): Bool = region r {
        let l = MutList.new(r);
        let xs = "1" :: "2" :: "3" :: Nil;
        List.forEach(x -> MutList.push!(x, l), xs);
        MutList.joinWith(x -> x + x, ",", l) == "11,22,33"
    }

    /////////////////////////////////////////////////////////////////////////////
    // shuffle                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def shuffle01(): Bool \ NonDet = region r {
        let l: List[Int32] = Nil;
        let v = MutList.shuffle(r, Random.new(), List.toMutList(r, l));
        MutList.sort!(v);

        MutList.length(v) == 0 and v `MutList.sameElements` List.toMutList(r, l)
    }

    @test
    def shuffle02(): Bool \ NonDet = region r {
        let l = 1 :: Nil;
        let v = MutList.shuffle(r, Random.new(), List.toMutList(r, l));
        MutList.sort!(v);

        MutList.length(v) == 1 and v `MutList.sameElements` List.toMutList(r, l)
    }

    @test
    def shuffle03(): Bool \ NonDet = region r {
        let l = 1 :: 2 :: 3 :: Nil;
        let v = MutList.shuffle(r, Random.new(), List.toMutList(r, l));
        MutList.sort!(v);

        MutList.length(v) == 3 and v `MutList.sameElements` List.toMutList(r, l)
    }

    @test
    def shuffle04(): Bool \ NonDet = region r {
        let l = 0 :: 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: 9 :: Nil;
        let v = MutList.shuffle(r, Random.new(), List.toMutList(r, l));
        MutList.sort!(v);

        MutList.length(v) == 10 and v `MutList.sameElements` List.toMutList(r, l)
    }

}

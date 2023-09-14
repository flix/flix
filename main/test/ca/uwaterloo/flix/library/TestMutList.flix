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
    def capacity(v: MutList[a, r]): Int32 \ r =
        let MutList(_, a, _) = v;
        Array.length(deref a)

    /////////////////////////////////////////////////////////////////////////////
    // toString                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toString01(): Bool = region rc {
        let l: MutList[Int32, rc] = MutList.new(rc);
        MutList.toString(l) == "MutList#{}"
    }

    @Test
    def toString02(): Bool = region rc {
        let l: MutList[Int32, rc] = MutList.new(rc);
        MutList.push!(1, l);
        MutList.toString(l) == "MutList#{1}"
    }

    @Test
    def toString03(): Bool = region rc {
        let l: MutList[Int32, rc] = MutList.new(rc);
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

    @Test
    def sameElements01(): Bool = region rc {
        let u: MutList[Int32, _] = MutList.new(rc);
        let v: MutList[Int32, _] = MutList.new(rc);
        u `sameElements` v
    }

    @Test
    def sameElements02(): Bool = region rc {
        let u = MutList.new(rc);
        MutList.push!(0, u);
        let v = MutList.new(rc);
        (u `sameElements` v) == false
    }

    @Test
    def sameElements03(): Bool = region rc {
        let u = MutList.new(rc);
        MutList.push!(2, u);
        MutList.push!(3, u);
        let v = MutList.new(rc);
        MutList.push!(2, v);
        MutList.push!(3, v);
        u `sameElements` v
    }

    @Test
    def sameElements04(): Bool = region rc {
        let u = MutList.new(rc);
        MutList.push!(2, u);
        MutList.push!(3, u);
        let v = MutList.new(rc);
        MutList.push!(2, v);
        MutList.push!(2, v);
        (u `sameElements` v) == false
    }

    @Test
    def sameElements05(): Bool = region rc {
        let u = MutList.new(rc);
        MutList.push!(1, u);
        let v = MutList.new(rc);
        (u `sameElements` v) == false
    }


    /////////////////////////////////////////////////////////////////////////////
    // Order.compare                                                           //
    /////////////////////////////////////////////////////////////////////////////

    // TODO


    /////////////////////////////////////////////////////////////////////////////
    // nth                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def nth01(): Bool = region rc {
        MutList.nth(0, MutList.new(rc): MutList[Int32, _]) == None
    }

    @Test
    def nth02(): Bool = region rc {
        let v = MutList.new(rc);
        MutList.push!(42, v);
        MutList.nth(0, v) == Some(42)
    }

    @Test
    def nth03(): Bool = region rc {
        let v = MutList.new(rc);
        MutList.push!(42, v);
        MutList.nth(1, v) == None
    }

    @Test
    def nth04(): Bool = region rc {
        MutList.nth(1, MutList.range(rc, 0, 2)) == Some(1)
    }

    @Test
    def nth05(): Bool = region rc {
        MutList.nth(3, MutList.range(rc, 0, 4)) == Some(3)
    }

    @Test
    def nth06(): Bool = region rc {
        MutList.nth(30, MutList.range(rc, 0, 42)) == Some(30)
    }

    @Test
    def nth07(): Bool = region rc {
        MutList.nth(-30, MutList.range(rc, 0, 42)) == None
    }

    @Test
    def nth08(): Bool = region rc {
        MutList.nth(50, MutList.range(rc, 0, 42)) == None
    }


    /////////////////////////////////////////////////////////////////////////////
    // length                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def length01(): Bool = region rc {
        MutList.length(MutList.new(rc)) == 0
    }

    @Test
    def length02(): Bool = region rc {
        let v = MutList.new(rc);
        MutList.push!(42, v);
        MutList.length(v) == 1
    }

    @Test
    def length03(): Bool = region rc {
        MutList.length(MutList.range(rc, 0, 2)) == 2
    }

    @Test
    def length04(): Bool = region rc {
        MutList.length(MutList.range(rc, 0, 4)) == 4
    }

    @Test
    def length05(): Bool = region rc {
        MutList.length(MutList.range(rc, 0, 42)) == 42
    }


    /////////////////////////////////////////////////////////////////////////////
    // isEmpty                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isEmpty01(): Bool = region rc {
        MutList.isEmpty(MutList.new(rc))
    }

    @Test
    def isEmpty02(): Bool = region rc {
        MutList.isEmpty(MutList.range(rc, 0, 0))
    }

    @Test
    def isEmpty03(): Bool = region rc {
        MutList.isEmpty(MutList.range(rc, 0, 1)) == false
    }

    @Test
    def isEmpty04(): Bool = region rc {
        MutList.isEmpty(MutList.range(rc, 0, 2)) == false
    }

    @Test
    def isEmpty05(): Bool = region rc {
        MutList.isEmpty(MutList.range(rc, 3, 4)) == false
    }


    /////////////////////////////////////////////////////////////////////////////
    // memberOf                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def memberOf01(): Bool = region rc {
        MutList.memberOf(42, MutList.new(rc)) == false
    }

    @Test
    def memberOf02(): Bool = region rc {
        let v = MutList.new(rc);
        MutList.push!(2, v);
        MutList.memberOf(2, v)
    }

    @Test
    def memberOf03(): Bool = region rc {
        let v = MutList.new(rc);
        MutList.push!(42, v);
        MutList.memberOf(2, v) == false
    }

    @Test
    def memberOf04(): Bool = region rc {
        MutList.memberOf(4, MutList.range(rc, 0, 4)) == false
    }

    @Test
    def memberOf05(): Bool = region rc {
        MutList.memberOf(0, MutList.range(rc, 0, 3))
    }

    @Test
    def memberOf06(): Bool = region rc {
        MutList.memberOf(2, MutList.range(rc, 0, 3))
    }


    /////////////////////////////////////////////////////////////////////////////
    // count                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def count01(): Bool = region rc {
        MutList.count(x -> x > 2, MutList.range(rc, 0, 4)) == 1
    }

    @Test
    def count02(): Bool = region rc {
        MutList.count(x -> x >= 2, MutList.range(rc, 0, 4)) == 2
    }

    @Test
    def count03(): Bool = region rc {
        MutList.count(x -> x `Int32.rem` 2 == 0, MutList.range(rc, 0, 10)) == 5
    }

    @Test
    def count04(): Bool = region rc {
        MutList.count(x -> x == 11, MutList.range(rc, 0, 21)) == 1
    }


    /////////////////////////////////////////////////////////////////////////////
    // exists                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def exists01(): Bool = region rc {
        MutList.exists(x -> x > 9, MutList.range(rc, 0, 9)) == false
    }

    @Test
    def exists02(): Bool = region rc {
        MutList.exists(x -> x < 0, MutList.range(rc, 0, 9)) == false
    }

    @Test
    def exists03(): Bool = region rc {
        MutList.exists(x -> x == 8, MutList.range(rc, 0, 9))
    }

    @Test
    def exists04(): Bool = region rc {
        MutList.exists(x -> x > 0, MutList.range(rc, 0, 9))
    }

    @Test
    def exists05(): Bool = region rc {
        MutList.exists(x -> x == 0, MutList.new(rc)) == false
    }


    /////////////////////////////////////////////////////////////////////////////
    // forAll                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forAll01(): Bool = region rc {
        MutList.forAll(x -> x > 0, MutList.range(rc, 0, 9)) == false
    }

    @Test
    def forAll02(): Bool = region rc {
        MutList.forAll(x -> x >= 0, MutList.range(rc, 0, 9))
    }

    @Test
    def forAll03(): Bool = region rc {
        MutList.forAll(x -> x >= 0 and x < 9, MutList.range(rc, 0, 9))
    }

    @Test
    def forAll05(): Bool = region rc {
        MutList.forAll(x -> x > 0, MutList.new(rc))
    }


    /////////////////////////////////////////////////////////////////////////////
    // head                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def head01(): Bool = region rc {
        let v = MutList.new(rc);
        MutList.push!(42, v);
        MutList.head(v) == Some(42)
    }

    @Test
    def head02(): Bool = region rc {
        MutList.head(MutList.new(rc): MutList[Unit, _]) == None
    }

    @Test
    def head03(): Bool = region rc {
        let v = MutList.new(rc);
        MutList.push!(101, v);
        MutList.push!(102, v);
        MutList.head(v) == Some(101)
    }


    /////////////////////////////////////////////////////////////////////////////
    // last                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def last01(): Bool = region rc {
        MutList.last(MutList.new(rc): MutList[Unit, _]) == None
    }

    @Test
    def last02(): Bool = region rc {
        MutList.last(MutList.range(rc, 0, 8)) == Some(7)
    }

    @Test
    def last03(): Bool = region rc {
        let v = MutList.new(rc);
        MutList.push!('A', v);
        MutList.push!('B', v);
        MutList.last(v) == Some('B')
    }


    /////////////////////////////////////////////////////////////////////////////
    // indexOfLeft                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def indexOfLeft01(): Bool = region rc {
        MutList.indexOfLeft(2, MutList.range(rc, 2, 3)) == Some(0)
    }

    @Test
    def indexOfLeft02(): Bool = region rc {
        MutList.indexOfLeft(10, MutList.range(rc, 0, 10)) == None
    }

    @Test
    def indexOfLeft03(): Bool = region rc {
        let v = MutList.new(rc);
        MutList.push!('A', v);
        MutList.push!('B', v);
        MutList.push!('C', v);
        MutList.indexOfLeft('B', v) == Some(1)
    }

    @Test
    def indexOfLeft04(): Bool = region rc {
        MutList.indexOfLeft(99, MutList.range(rc, 0, 100)) == Some(99)
    }

    /////////////////////////////////////////////////////////////////////////////
    // indexOfRight                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def indexOfRight01(): Bool = region rc {
        MutList.indexOfRight(2, MutList.range(rc, 2, 3)) == Some(0)
    }

    @Test
    def indexOfRight02(): Bool = region rc {
        MutList.indexOfRight(10, MutList.range(rc, 0, 10)) == None
    }

    @Test
    def indexOfRight03(): Bool = region rc {
        let v = MutList.new(rc);
        MutList.push!('A', v);
        MutList.push!('B', v);
        MutList.push!('A', v);
        MutList.indexOfRight('A', v) == Some(2)
    }

    @Test
    def indexOfRight04(): Bool = region rc {
        let v1 = MutList.range(rc, 0, 4);
        let v2 = MutList.range(rc, 0, 4);
        MutList.append!(MutList.toList(v2), v1);
        MutList.indexOfRight(2, v1) == Some(6)
    }

    /////////////////////////////////////////////////////////////////////////////
    // findLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def findLeft01(): Bool = region rc {
        MutList.findLeft(x -> x > 3, MutList.range(rc, 3, 11)) == Some(4)
    }

    @Test
    def findLeft02(): Bool = region rc {
        MutList.findLeft(x -> x > 10, MutList.range(rc, 3, 11)) == None
    }

    @Test
    def findLeft03(): Bool = region rc {
        MutList.findLeft(x -> x < 3, MutList.range(rc, 3, 11)) == None
    }

    @Test
    def findLeft04(): Bool = region rc {
        MutList.findLeft(x -> x `Int32.rem` 2 == 1, MutList.range(rc, 3, 11)) == Some(3)
    }

    /////////////////////////////////////////////////////////////////////////////
    // findRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def findRight01(): Bool = region rc {
        MutList.findRight(x -> x > 0, MutList.range(rc, 2, 5)) == Some(4)
    }

    @Test
    def findRight02(): Bool = region rc {
        MutList.findRight(x -> x == 10, MutList.range(rc, 9, 10)) == None
    }

    @Test
    def findRight03(): Bool = region rc {
        MutList.findRight(x -> x `Int32.rem` 2 == 0, MutList.range(rc, 3, 4)) == None
    }

    @Test
    def findRight04(): Bool = region rc {
        MutList.findRight(x -> x < 5, MutList.range(rc, 3, 11)) == Some(4)
    }


    /////////////////////////////////////////////////////////////////////////////
    // scanLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def scanLeft01(): Bool = region rc {
        let v = MutList.scanLeft(rc, (x, y) -> x + y, 0, MutList.new(rc));
        MutList.length(v) == 1 and MutList.head(v) == Some(0)
    }

    @Test
    def scanLeft02(): Bool = region rc {
        let v1 = MutList.range(rc, 1, 6);
        let v2 = MutList.new(rc);
        MutList.push!(0, v2);
        MutList.push!(1, v2);
        MutList.push!(3, v2);
        MutList.push!(6, v2);
        MutList.push!(10, v2);
        MutList.push!(15, v2);
        let v3 = MutList.scanLeft(rc, (x, y) -> x + y, 0, v1);
        MutList.sameElements(v2, v3)
    }

    @Test
    def scanLeft03(): Bool = region rc {
        let v = MutList.scanLeft(rc, (x, y) -> x + y, 7, MutList.range(rc, 5, 13));
        MutList.last(v) == Some(75)
    }


    /////////////////////////////////////////////////////////////////////////////
    // scanRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def scanRight01(): Bool = region rc {
        let v = MutList.scanRight(rc, (x, y) -> x + y, 0, MutList.new(rc));
        MutList.length(v) == 1 and MutList.head(v) == Some(0)
    }

    @Test
    def scanRight02(): Bool = region rc {
        let v1 = MutList.range(rc, 1, 6);
        let v2 = MutList.new(rc);
        MutList.push!(15, v2);
        MutList.push!(14, v2);
        MutList.push!(12, v2);
        MutList.push!(9, v2);
        MutList.push!(5, v2);
        MutList.push!(0, v2);
        let v3 = MutList.scanRight(rc, (x, y) -> x + y, 0, v1);
        MutList.sameElements(v2, v3)
    }

    @Test
    def scanRight03(): Bool = region rc {
        let v = MutList.scanRight(rc, (x, y) -> x + y, 7, MutList.range(rc, 5, 13));
        MutList.last(v) == Some(7)
    }


    /////////////////////////////////////////////////////////////////////////////
    // map                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def map01(): Bool = region rc {
        let v = MutList.map(rc, x -> x * 2, MutList.range(rc, 0, 6));
        MutList.sameElements(v, Array.toMutList(rc, Array#{0, 2, 4, 6, 8, 10} @ rc))
    }

    @Test
    def map02(): Bool = region rc {
        let v = MutList.map(rc, identity, MutList.range(rc, 0, 10));
        MutList.sameElements(v, MutList.range(rc, 0, 10))
    }

    @Test
    def map03(): Bool = region rc {
        let v = MutList.map(rc, identity, MutList.new(rc): MutList[Unit, _]);
        MutList.sameElements(v, MutList.new(rc))
    }

    @Test
    def map04(): Bool = region rc {
        let v = MutList.map(rc, x -> x + 1, MutList.range(rc, 0, 100));
        MutList.sameElements(v, MutList.range(rc, 1, 101))
    }


    /////////////////////////////////////////////////////////////////////////////
    // transform!                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def transform!01(): Bool = region rc {
        let v = MutList.range(rc, 0, 6);
        MutList.transform!(x -> x * 2, v);
        MutList.sameElements(v, Array.toMutList(rc, Array#{0, 2, 4, 6, 8, 10} @ rc))
    }

    @Test
    def transform!02(): Bool = region rc {
        let v = MutList.range(rc, 0, 10);
        MutList.transform!(identity, v);
        MutList.sameElements(v, MutList.range(rc, 0, 10))
    }

    @Test
    def transform!03(): Bool = region rc {
        let v: MutList[Unit, _] = MutList.new(rc);
        MutList.transform!(identity, v);
        MutList.sameElements(v, MutList.new(rc))
    }

    @Test
    def transform!04(): Bool = region rc {
        let v = MutList.range(rc, 0, 100);
        MutList.transform!(x -> x + 1, v);
        MutList.sameElements(v, MutList.range(rc, 1, 101))
    }


    /////////////////////////////////////////////////////////////////////////////
    // mapWithIndex                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def mapWithIndex01(): Bool = region rc {
        let v = MutList.mapWithIndex(rc, (i, _) -> if (i < 3) 0 else 1, MutList.range(rc, 0, 6));
        MutList.sameElements(v, Array.toMutList(rc, Array#{0, 0, 0, 1, 1, 1} @ rc))
    }

    @Test
    def mapWithIndex02(): Bool = region rc {
        let v = MutList.mapWithIndex(rc, (i, x) -> if (x == 8 and i == 1) 42 else 0, MutList.range(rc, 7, 10));
        MutList.sameElements(v, Array.toMutList(rc, Array#{0, 42, 0} @ rc))
    }

    @Test
    def mapWithIndex03(): Bool = region rc {
        let v = MutList.mapWithIndex(rc, (i, _) -> if (i == 0) 42 else 0, MutList.new(rc));
        MutList.sameElements(v, MutList.new(rc))
    }


    /////////////////////////////////////////////////////////////////////////////
    // transformWithIndex!                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def transformWithIndex!01(): Bool = region rc {
        let v = MutList.range(rc, 0, 6);
        MutList.transformWithIndex!((i, _) -> if (i < 3) 0 else 1, v);
        MutList.sameElements(v, Array.toMutList(rc, Array#{0, 0, 0, 1, 1, 1} @ rc))
    }

    @Test
    def transformWithIndex!02(): Bool = region rc {
        let v = MutList.range(rc, 7, 10);
        MutList.transformWithIndex!((i, x) -> if (x == 8 and i == 1) 42 else 0, v);
        MutList.sameElements(v, Array.toMutList(rc, Array#{0, 42, 0} @ rc))
    }

    @Test
    def transformWithIndex!03(): Bool = region rc {
        let v = MutList.new(rc);
        MutList.transformWithIndex!((i, _) -> if (i == 0) 42 else 0, v);
        MutList.sameElements(v, MutList.new(rc))
    }


    /////////////////////////////////////////////////////////////////////////////
    // foldLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldLeft01(): Bool = region rc {
        MutList.foldLeft((acc, x) -> acc + x, 0, MutList.range(rc, 1, 5)) == 10
    }

    @Test
    def foldLeft02(): Bool = region rc {
        let v1 = MutList.new(rc);
        let v2 = MutList.range(rc, 0, 10);
        MutList.foldLeft((_, x) -> MutList.push!(x, v1), (), v2);
        MutList.sameElements(v1, v2)
    }

    @Test
    def foldLeft03(): Bool = region rc {
        MutList.foldLeft((acc, x) -> x :: acc, Nil, MutList.range(rc, 0, 6)) == 5 :: 4 :: 3 :: 2 :: 1 :: 0 :: Nil
    }

    @Test
    def foldLeft04(): Bool = region rc {
        MutList.foldLeft((acc, x) -> acc - x, 0, MutList.range(rc, 0, 100)) == -4950
    }

    @Test
    def foldLeft05(): Bool = region rc {
        MutList.foldLeft((acc, x) -> acc + x, 42, MutList.new(rc)) == 42
    }


    /////////////////////////////////////////////////////////////////////////////
    // foldRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldRight01(): Bool = region rc {
        MutList.foldRight((x, acc) -> acc + x, 0, MutList.range(rc, 1, 5)) == 10
    }

    @Test
    def foldRight02(): Bool = region rc {
        MutList.foldRight((x, acc) -> Int32.toString(x) + acc, "", MutList.range(rc, 3, 8)) == "34567"
    }

    @Test
    def foldRight03(): Bool = region rc {
        MutList.foldRight((x, acc) -> x :: acc, Nil, MutList.range(rc, 0, 6)) == 0 :: 1 :: 2 :: 3 :: 4 :: 5 :: Nil
    }

    @Test
    def foldRight04(): Bool = region rc {
        MutList.foldRight((x, acc) -> x - acc, 0, MutList.range(rc, 0, 100)) == -50
    }

    @Test
    def foldRight05(): Bool = region rc {
        MutList.foldRight((x, acc) -> x + acc, 42, MutList.new(rc)) == 42
    }

    /////////////////////////////////////////////////////////////////////////////
    // foldRightWithCont                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldRightWithCont01(): Bool = region rc {
        MutList.foldRightWithCont((x, k) -> k() + x, 0, MutList.range(rc, 1, 5)) == 10
    }

    @Test
    def foldRightWithCont02(): Bool = region rc {
        MutList.foldRightWithCont((x, k) -> Int32.toString(x) + k(), "", MutList.range(rc, 3, 8)) == "34567"
    }

    @Test
    def foldRightWithCont03(): Bool = region rc {
        MutList.foldRightWithCont((x, k) -> x :: k(), Nil, MutList.range(rc, 0, 6)) == 0 :: 1 :: 2 :: 3 :: 4 :: 5 :: Nil
    }

    @Test
    def foldRightWithCont04(): Bool = region rc {
        MutList.foldRightWithCont((x, k) -> x - k(), 0, MutList.range(rc, 0, 100)) == -50
    }

    @Test
    def foldRightWithCont05(): Bool = region rc {
        MutList.foldRightWithCont((x, k) -> x + k(), 42, MutList.new(rc)) == 42
    }


    /////////////////////////////////////////////////////////////////////////////
    // foldMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldMap01(): Bool = region rc {
        MutList.foldMap(x -> 2 * x, List.toMutList(rc, Nil: List[Int32])) == 0
    }

    @Test
    def foldMap02(): Bool = region rc {
        MutList.foldMap(x -> 2 * x, List.toMutList(rc, 1 :: 2 :: Nil)) == 6
    }

    @Test
    def foldMap03(): Bool = region rc {
        MutList.foldMap(x -> if (x == "a") "b" else x, List.toMutList(rc, "a" :: Nil)) == "b"
    }

    @Test
    def foldMap04(): Bool = region rc {
        MutList.foldMap(x -> if (x == "c") "b" else x, List.toMutList(rc, "a" :: "b" :: "c":: Nil)) == "abb"
    }

    @Test
    def foldMap05(): Bool = region rc {
        MutList.foldMap(Int32.toString, List.toMutList(rc, 1 :: 2 :: 3 :: Nil)) == "123"
    }


    /////////////////////////////////////////////////////////////////////////////
    // reduceLeft                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def reduceLeft01(): Bool = region rc {
        MutList.reduceLeft((x, y) -> x + y, MutList.new(rc): MutList[Int32, _]) == None
    }

    @Test
    def reduceLeft02(): Bool = region rc {
        MutList.reduceLeft((x, y) -> x + y, MutList.range(rc, 0, 5)) == Some(10)
    }

    @Test
    def reduceLeft03(): Bool = region rc {
        MutList.reduceLeft((x, y) -> x - y, MutList.range(rc, 0, 101)) == Some(-5050)
    }

    @Test
    def reduceLeft04(): Bool = region rc {
        MutList.reduceLeft((_, y) -> if (y > 4) 1 else 0, MutList.range(rc, 0, 6)) == Some(1)
    }


    /////////////////////////////////////////////////////////////////////////////
    // reduceRight                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def reduceRight01(): Bool = region rc {
        MutList.reduceRight((x, y) -> x + y, MutList.new(rc): MutList[Int32, _]) == None
    }

    @Test
    def reduceRight02(): Bool = region rc {
        MutList.reduceRight((x, y) -> x + y, MutList.range(rc, 0, 5)) == Some(10)
    }

    @Test
    def reduceRight03(): Bool = region rc {
        MutList.reduceRight((x, y) -> x - y, MutList.range(rc, 0, 6)) == Some(-3)
    }

    @Test
    def reduceRight04(): Bool = region rc {
        MutList.reduceRight((x, _) -> if (x < 2) 1 else 0, MutList.range(rc, 0, 6)) == Some(1)
    }


    /////////////////////////////////////////////////////////////////////////////
    // clear!                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def clear!01(): Bool = region rc {
        let v = MutList.range(rc, 0, 1);
        let len = MutList.length(v);
        MutList.clear!(v);
        len == 1 and MutList.isEmpty(v)
    }

    @Test
    def clear!02(): Bool = region rc {
        let v = MutList.new(rc);
        let len = MutList.length(v);
        MutList.clear!(v);
        len == MutList.length(v)
    }

    @Test
    def clear!03(): Bool = region rc {
        let v = MutList.range(rc, 0, 100);
        MutList.clear!(v);
        MutList.isEmpty(v)
    }


    /////////////////////////////////////////////////////////////////////////////
    // copy                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def copy01(): Bool = region rc {
        let v1 = MutList.range(rc, 0, 6);
        let v2 = MutList.copy(rc, v1);
        v1 `sameElements` v2
    }

    @Test
    def copy02(): Bool = region rc {
        let v1 = MutList.range(rc, 0, 100);
        let v2 = MutList.copy(rc, v1);
        MutList.transform!(x -> x + 1, v2);
        MutList.last(v1) == Some(99) and MutList.last(v2) == Some(100)
    }

    @Test
    def copy03(): Bool = region rc {
        let v1 = MutList.range(rc, 0, 6);
        let v2 = MutList.copy(rc, v1);
        MutList.clear!(v1);
        MutList.length(v1) == 0 and MutList.length(v2) == 6
    }

    @Test
    def copy04(): Bool = region rc {
        let v1 = MutList.range(rc, 0, 1000);
        let MutList(_, l1, n1) = v1;
        let MutList(_, l2, n2) = MutList.copy(rc, v1);
        (deref n1 == deref n2) and (deref l1 `Array.sameElements` deref l2)
    }

    @Test
    def copy05(): Bool = region rc {
        let v1 = MutList.range(rc, 0, 3);
        let MutList(_, l1, n1) = v1;
        let MutList(_, l2, n2) = MutList.copy(rc, v1);
        (deref n1 == deref n2) and (deref l1 `Array.sameElements` deref l2) and
            (Array.length(deref l2) == 8)
    }


    /////////////////////////////////////////////////////////////////////////////
    // pop                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def pop01(): Bool = region rc {
        let v = MutList.range(rc, 0, 6);
        let len = MutList.length(v);
        let last = MutList.pop!(v);
        MutList.length(v) == len - 1 and last == Some(5)
    }

    @Test
    def pop02(): Bool = region rc {
        let v: MutList[Unit, _] = MutList.new(rc);
        MutList.pop!(v) == None
    }

    @Test
    def pop03(): Bool = region rc {
        let v = MutList.range(rc, 1, 4);
        let three = MutList.pop!(v);
        let two = MutList.pop!(v);
        let one = MutList.pop!(v);
        let none = MutList.pop!(v);
        three == Some(3) and two == Some(2) and one == Some(1) and none == None
    }


    /////////////////////////////////////////////////////////////////////////////
    // push                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def push01(): Bool = region rc {
        let v = MutList.new(rc);
        let len = MutList.length(v);
        MutList.push!(1, v);
        MutList.last(v) == Some(1) and MutList.length(v) == len + 1
    }

    @Test
    def push02(): Bool = region rc {
        let v = MutList.new(rc);
        let len = MutList.length(v);
        List.forEach(x -> MutList.push!(x, v), List.range(0, 97));
        MutList.head(v) == Some(0) and MutList.last(v) == Some(96) and MutList.length(v) == len + 97
    }

    @Test
    def push03(): Bool = region rc {
        let v = MutList.new(rc);
        MutList.push!(42, v);
        MutList.push!(31, v);
        MutList.push!(1019, v);
        MutList.length(v) == 3 and MutList.nth(0, v) == Some(42) and MutList.nth(1, v) == Some(31) and MutList.nth(2, v) == Some(1019)
    }

    @Test
    def push04(): Bool = region rc {
        let v = MutList.new(rc);
        MutList.push!(0, v);
        MutList.push!(0, v);
        MutList.push!(0, v);
        MutList.push!(0, v);
        MutList.length(v) == 4 and MutList.nth(0, v) == Some(0) and MutList.nth(1, v) == Some(0) and MutList.nth(2, v) == Some(0) and MutList.nth(3, v) == Some(0)
    }


    /////////////////////////////////////////////////////////////////////////////
    // insert!                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def insert!01(): Bool = region rc {
        let v = MutList.new(rc);
        MutList.insert!(42, 0, v);
        MutList.length(v) == 1 and MutList.head(v) == Some(42)
    }

    @Test
    def insert!02(): Bool = region rc {
        let v = MutList.new(rc);
        MutList.insert!(107, 0, v);
        MutList.insert!(7, 0, v);
        MutList.length(v) == 2 and MutList.head(v) == Some(7)
    }

    @Test
    def insert!03(): Bool = region rc {
        let v = MutList.new(rc);
        List.forEach(x -> MutList.insert!(x, 0, v), List.range(0, 107));
        MutList.length(v) == 107 and MutList.head(v) == Some(106)
    }

    @Test
    def insert!04(): Bool = region rc {
        let v = MutList.new(rc);
        MutList.insert!(42, 0, v);
        MutList.insert!(3, 1, v);
        MutList.insert!(5, 2, v);
        MutList.insert!(6, 0, v);
        MutList.length(v) == 4 and MutList.head(v) == Some(6) and MutList.last(v) == Some(5)
    }

    @Test
    def insert!05(): Bool = region rc {
        let v = MutList.new(rc);
        List.forEach(x -> MutList.push!(x, v), List.range(97, 147));
        MutList.insert!(42, 0, v);
        MutList.insert!(49, 0, v);
        MutList.length(v) == 52 and MutList.head(v) == Some(49) and MutList.last(v) == Some(146)
    }

    @Test
    def insert!06(): Bool = region rc {
        let v = MutList.new(rc);
        MutList.insert!(42, 0, v);
        MutList.insert!(42, 0, v);
        MutList.insert!(42, 0, v);
        MutList.insert!(42, 1, v);
        MutList.insert!(97, 0, v);
        MutList.insert!(98, 2, v);
        MutList.length(v) == 6 and MutList.head(v) == Some(97) and MutList.last(v) == Some(42)
    }

    @Test
    def insert!07(): Bool = region rc {
        let v = MutList.new(rc);
        MutList.insert!(97, 0, v);
        MutList.insert!(98, 1, v);
        MutList.insert!(99, 2, v);
        MutList.insert!(107, 1, v);
        MutList.length(v) == 4 and MutList.nth(1, v) == Some(107)
    }

    @Test
    def insert!08(): Bool = region rc {
        let l1 = MutList.range(rc, 0, 4);
        MutList.insert!(42, 3, l1);
        MutList.insert!(41, 2, l1);
        MutList.insert!(40, 1, l1);
        MutList.insert!(55, 0, l1);
        MutList.insert!(33, 1, l1);
        let l2 = MutList.new(rc);
        let xs = 55 :: 33 :: 0 :: 40 :: 1 :: 41 :: 2 :: 42 :: 3 :: Nil;
        List.forEach(x -> MutList.push!(x, l2), xs);
        MutList.sameElements(l1, l2)
    }


    /////////////////////////////////////////////////////////////////////////////
    // remove!                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def remove!01(): Bool = region rc {
        let v: MutList[Unit, _] = MutList.new(rc);
        MutList.remove!(0, v);
        MutList.sameElements(v, MutList.new(rc))
    }

    @Test
    def remove!02(): Bool = region rc {
        let v = MutList.range(rc, 0, 4);
        MutList.remove!(0, v);
        MutList.sameElements(v, MutList.range(rc, 1, 4))
    }

    @Test
    def remove!03(): Bool = region rc {
        let v = MutList.range(rc, 0, 4);
        MutList.remove!(3, v);
        MutList.sameElements(v, MutList.range(rc, 0, 3))
    }

    @Test
    def remove!04(): Bool = region rc {
        let v = MutList.range(rc, 3, 7);
        MutList.remove!(0, v);
        MutList.remove!(0, v);
        MutList.remove!(0, v);
        MutList.sameElements(v, MutList.range(rc, 6, 7))
    }

    @Test
    def remove!05(): Bool = region rc {
        let v = MutList.range(rc, 9, 12);
        MutList.remove!(2, v);
        MutList.remove!(1, v);
        MutList.remove!(0, v);
        MutList.sameElements(v, MutList.new(rc))
    }

    @Test
    def remove!06(): Bool = region rc {
        let v = MutList.range(rc, 7, 13);
        MutList.remove!(0, v);
        MutList.remove!(0, v);
        MutList.remove!(0, v);
        MutList.remove!(0, v);
        MutList.remove!(0, v);
        MutList.remove!(0, v);
        MutList.sameElements(v, MutList.new(rc))
    }

    @Test
    def remove!07(): Bool = region rc {
        let v = MutList.range(rc, 0, 3);
        MutList.remove!(3, v);
        MutList.remove!(7, v);
        MutList.remove!(99, v);
        MutList.sameElements(v, MutList.range(rc, 0, 3))
    }


    /////////////////////////////////////////////////////////////////////////////
    // pushAll!                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def pushAll!01(): Bool = region rc {
        let v = MutList.range(rc, 0, 4);
        let l = List.range(4, 8);
        MutList.pushAll!(l, v);
        MutList.sameElements(v, MutList.range(rc, 0, 8))
    }

    @Test
    def pushAll!02(): Bool = region rc {
        let v = MutList.new(rc);
        let l = List.range(1, 4);
        MutList.pushAll!(l, v);
        MutList.sameElements(v, MutList.range(rc, 1, 4))
    }

    @Test
    def pushAll!03(): Bool = region rc {
        let v = MutList.range(rc, 9, 24);
        let l = Nil: List[Int32];
        MutList.pushAll!(l, v);
        MutList.sameElements(v, MutList.range(rc, 9, 24))
    }

    @Test
    def pushAll!04(): Bool = region rc {
        let v: MutList[Unit, _] = MutList.new(rc);
        let l: List[Unit] = Nil;
        MutList.pushAll!(l, v);
        MutList.sameElements(v, MutList.new(rc))
    }

    @Test
    def pushAll!05(): Bool = region rc {
        let v = MutList.range(rc, 1, 3);
        let l1 = List.range(3, 5);
        let l2 = List.range(5, 99);
        MutList.pushAll!(l1, v);
        MutList.pushAll!(l2, v);
        MutList.sameElements(v, MutList.range(rc, 1, 99))
    }


    /////////////////////////////////////////////////////////////////////////////
    // retain!                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def retain!01(): Bool = region rc {
        let l1 = MutList.range(rc, 0, 10);
        MutList.retain!(x -> x `Int32.rem` 2 == 0, l1);
        let l2 = MutList.new(rc);
        let xs = 0 :: 2 :: 4 :: 6 :: 8 :: Nil;
        List.forEach(x -> MutList.push!(x, l2), xs);
        MutList.sameElements(l1, l2)
    }

    @Test
    def retain!02(): Bool = region rc {
        let l1 = MutList.range(rc, 0, 10);
        MutList.retain!(x -> x `Int32.rem` 2 == 1, l1);
        let l2 = MutList.new(rc);
        let xs = 1 :: 3 :: 5 :: 7 :: 9 :: Nil;
        List.forEach(x -> MutList.push!(x, l2), xs);
        MutList.sameElements(l1, l2)
    }

    @Test
    def retain!03(): Bool = region rc {
        let v = MutList.range(rc, 0, 10);
        MutList.retain!(x -> x > 5, v);
        MutList.sameElements(v, MutList.range(rc, 6, 10))
    }

    @Test
    def retain!04(): Bool = region rc {
        let v = MutList.range(rc, 0, 10);
        MutList.retain!(x -> x > 10, v);
        MutList.sameElements(v, MutList.new(rc))
    }


    /////////////////////////////////////////////////////////////////////////////
    // replace!                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def replace!01(): Bool = region rc {
        let l1 = MutList.new(rc);
        MutList.push!(1, l1);
        MutList.push!(2, l1);
        MutList.push!(2, l1);
        MutList.push!(1, l1);
        MutList.push!(1, l1);
        MutList.replace!(from = 1, to = 42, l1);
        let l2 = MutList.new(rc);
        let xs = 42 :: 2 :: 2 :: 42 :: 42 :: Nil;
        List.forEach(x -> MutList.push!(x, l2), xs);
        MutList.sameElements(l1, l2)
    }

    @Test
    def replace!02(): Bool = region rc {
        let l1 = MutList.range(rc, 4, 7);
        MutList.replace!(from = 4, to = 0, l1);
        let l2 = MutList.new(rc);
        let xs = 0 :: 5 :: 6 :: Nil;
        List.forEach(x -> MutList.push!(x, l2), xs);
        MutList.sameElements(l1, l2)
    }

    @Test
    def replace!03(): Bool = region rc {
        let l1 = MutList.range(rc, 4, 7);
        MutList.replace!(from = 5, to = 0, l1);
        let l2 = MutList.new(rc);
        let xs = 4 :: 0 :: 6 :: Nil;
        List.forEach(x -> MutList.push!(x, l2), xs);
        MutList.sameElements(l1, l2)
    }

    @Test
    def replace!04(): Bool = region rc {
        let l1 = MutList.range(rc, 4, 7);
        MutList.replace!(from = 6, to = 0, l1);
        let l2 = MutList.new(rc);
        let xs = 4 :: 5 :: 0 :: Nil;
        List.forEach(x -> MutList.push!(x, l2), xs);
        MutList.sameElements(l1, l2)
    }

    @Test
    def replace!05(): Bool = region rc {
        let l1 = MutList.new(rc);
        let xs1 = List.repeat(97, 'a');
        List.forEach(x -> MutList.push!(x, l1), xs1);
        MutList.replace!(from = 'a', to = 'z', l1);
        let l2 = MutList.new(rc);
        let xs2 = List.repeat(97, 'z');
        List.forEach(x -> MutList.push!(x, l2), xs2);
        MutList.sameElements(l1, l2)
    }


    /////////////////////////////////////////////////////////////////////////////
    // reverse!                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def reverse01!(): Bool = region rc {
        let v: MutList[Unit, _] = MutList.new(rc);
        MutList.reverse!(v);
        MutList.sameElements(v, MutList.new(rc))
    }

    @Test
    def reverse02!(): Bool = region rc {
        let v = MutList.range(rc, 0, 1);
        MutList.reverse!(v);
        MutList.sameElements(v, MutList.range(rc, 0, 1))
    }

    @Test
    def reverse03!(): Bool = region rc {
        let l1 = MutList.range(rc, 0, 5);
        MutList.reverse!(l1);
        let l2 = MutList.new(rc);
        let xs = 4 :: 3 :: 2 :: 1 :: 0 :: Nil;
        List.forEach(x -> MutList.push!(x, l2), xs);
        MutList.sameElements(l1, l2)
    }

    @Test
    def reverse04!(): Bool = region rc {
        let l1 = MutList.range(rc, 0, 6);
        MutList.reverse!(l1);
        let l2 = MutList.new(rc);
        let xs = 5 :: 4 :: 3 :: 2 :: 1 :: 0 :: Nil;
        List.forEach(x -> MutList.push!(x, l2), xs);
        MutList.sameElements(l1, l2)
    }

    @Test
    def reverse05!(): Bool = region rc {
        let l = MutList.new(rc);
        let xs = 7 :: 6 :: 5 :: 4 :: 3 :: 2 :: Nil;
        List.forEach(x -> MutList.push!(x, l), xs);
        MutList.reverse!(l);
        MutList.sameElements(l, MutList.range(rc, 2, 8))
    }


    /////////////////////////////////////////////////////////////////////////////
    // shrink!                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def shrink!01(): Bool = region rc {
        let v = MutList.new(rc);
        MutList.push!(0, v);
        MutList.push!(1, v);
        MutList.push!(2, v);
        MutList.shrink!(v);
        MutList.length(v) == 3
    }

    @Test
    def shrink!02(): Bool = region rc {
        let v = MutList.new(rc);
        MutList.shrink!(v);
        MutList.length(v) == 0
    }

    @Test
    def shrink!03(): Bool = region rc {
        let v = MutList.range(rc, 0, 9);
        MutList.push!(9, v);
        MutList.shrink!(v);
        MutList.length(v) == 10
    }

    @Test
    def shrink!04(): Bool = region rc {
        let v = MutList.new(rc);
        MutList.push!(4, v);
        MutList.append!(MutList.toList(MutList.range(rc, 0, 4)), v);
        MutList.push!(5, v);
        MutList.shrink!(v);
        MutList.length(v) == 6
    }


    /////////////////////////////////////////////////////////////////////////////
    // truncate!                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def truncate!01(): Bool = region rc {
        let v: MutList[Unit, _] = MutList.new(rc);
        MutList.truncate!(0, v);
        MutList.sameElements(v, MutList.new(rc))
    }

    @Test
    def truncate!02(): Bool = region rc {
        let v = MutList.range(rc, 0, 10);
        MutList.truncate!(5, v);
        MutList.sameElements(v, MutList.range(rc, 0, 5))
    }

    @Test
    def truncate!03(): Bool = region rc {
        let v = MutList.range(rc, 0, 99);
        MutList.truncate!(-1, v);
        MutList.sameElements(v, MutList.new(rc))
    }

    @Test
    def truncate!04(): Bool = region rc {
        let v = MutList.range(rc, 7, 8);
        MutList.truncate!(2, v);
        MutList.sameElements(v, MutList.range(rc, 7, 8))
    }

    @Test
    def truncate!05(): Bool = region rc {
        let v = MutList.range(rc, 99, 102);
        MutList.truncate!(1, v);
        MutList.sameElements(v, MutList.range(rc, 99, 100))
    }


    /////////////////////////////////////////////////////////////////////////////
    // reserve!                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def reserve!01(): Bool = region rc {
        let v = MutList.new(rc);
        MutList.reserve!(4, v);
        capacity(v) - MutList.length(v) >= 4
    }

    @Test
    def reserve!02(): Bool = region rc {
        let v = MutList.range(rc, 0, 5);
        MutList.reserve!(5, v);
        capacity(v) - MutList.length(v) >= 5
    }

    @Test
    def reserve!03(): Bool = region rc {
        let v = MutList.range(rc, 0, 99);
        MutList.reserve!(1, v);
        capacity(v) - MutList.length(v) >= 1
    }

    @Test
    def reserve!04(): Bool = region rc {
        let v = MutList.range(rc, 0, 100);
        MutList.reserve!(100, v);
        capacity(v) - MutList.length(v) >= 100
    }

    @Test
    def reserve!05(): Bool = region rc {
        let v = MutList.range(rc, 0, 5);
        MutList.reserve!(MutList.length(v), v);
        capacity(v) - MutList.length(v) >= MutList.length(v)
    }

    @Test
    def reserve!06(): Bool = region rc {
        let n = 197;
        let v = MutList.range(rc, 0, n);
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

    @Test
    def iterator01(): Bool = region rc {
        let l = MutList.new(rc): MutList[Int32, rc];
        let iter = MutList.iterator(rc, l);
        match Iterator.next(iter) {
            case None => true
            case _    => false
        }
    }

    @Test
    def iterator02(): Bool = region rc {
        let l = List.toMutList(rc, 1 :: 2 :: 3 :: Nil);
        let iter = MutList.iterator(rc, l);
        match Iterator.next(iter) {
            case Some(_) => true
            case _       => false
        }
    }

    @Test
    def iterator03(): Bool = region rc {
        let l = List.toMutList(rc, 1 :: 2 :: 3 :: Nil);
        discard MutList.pop!(l);
        discard MutList.pop!(l);
        discard MutList.pop!(l);
        let iter = MutList.iterator(rc, l);
        match Iterator.next(iter) {
            case None => true
            case _    => false
        }
    }

    @Test
    def iterator04(): Bool = region rc {
        let l = List.toMutList(rc, 1 :: 2 :: 3 :: Nil);
        MutList.iterator(rc, l) |> Iterator.toList == 1 :: 2 :: 3 :: Nil
    }

    @Test
    def iterator05(): Bool = region rc {
        let l = MutList.range(rc, 0, 100);
        MutList.iterator(rc, l) |> Iterator.toList == List.range(0, 100) and
        MutList.toList(l) == List.range(0, 100)
    }

    /////////////////////////////////////////////////////////////////////////////
    // forEach                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forEach01(): Bool = region rc {
        let v = MutList.new(rc);
        let sb = StringBuilder.new(rc);
        let f = x -> if (x > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        MutList.forEach(f, v);
        StringBuilder.toString(sb) == ""
    }

    @Test
    def forEach02(): Bool = region rc {
        let v = MutList.range(rc, 0, 5);
        let sb = StringBuilder.new(rc);
        let f = x -> if (x `Int32.rem` 2 == 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        MutList.forEach(f, v);
        StringBuilder.toString(sb) == "TFTFT"
    }

    @Test
    def forEach03(): Bool = region rc {
        let v = MutList.range(rc, 0, 2);
        let sb = StringBuilder.new(rc);
        let f = x -> if (x > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        MutList.forEach(f, v);
        StringBuilder.toString(sb) == "FT"
    }

    @Test
    def forEach04(): Bool = region rc {
        let v = MutList.range(rc, 0, 2);
        let sb = StringBuilder.new(rc);
        let f = x -> if (x < 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        MutList.forEach(f, v);
        StringBuilder.toString(sb) == "FF"
    }


    /////////////////////////////////////////////////////////////////////////////
    // forEachWithIndex                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forEachWithIndex01(): Bool = region rc {
        let v1 = MutList.range(rc, 0, 100);
        let v2 = MutList.new(rc);
        MutList.forEachWithIndex((i, x) -> if (x `Int32.rem` 2 == 0 and i < 10) MutList.push!(x, v2) else (), v1);
        let l = MutList.new(rc);
        let xs = 0 :: 2 :: 4 :: 6 :: 8 :: Nil;
        List.forEach(x -> MutList.push!(x, l), xs);
        MutList.sameElements(v2, l)
    }

    @Test
    def forEachWithIndex02(): Bool = region rc {
        let v = MutList.range(rc, 1, 4);
        let sb = StringBuilder.new(rc);
        let f = (i, x) -> if (x `Int32.rem` 2 == 1 and i == 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        MutList.forEachWithIndex(f, v);
        StringBuilder.toString(sb) == "TFF"
    }

    @Test
    def forEachWithIndex03(): Bool = region rc {
        let v = MutList.range(rc, 0, 5);
        let sb = StringBuilder.new(rc);
        let f = (_, x) -> if (x `Int32.rem` 2 == 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        MutList.forEachWithIndex(f, v);
        StringBuilder.toString(sb) == "TFTFT"
    }


    /////////////////////////////////////////////////////////////////////////////
    // compress!                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def compress!01(): Bool = region rc {
        // Pop 76 element such that the load factor is strictly lower than 1 / 4.
        let v = MutList.range(rc, 0, 100);
        let c = capacity(v);
        let i = ref 0 @ rc;
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

    @Test
    def compress!02(): Bool = region rc {
        let v = MutList.new(rc);
        MutList.compress!(v);
        capacity(v) == capacity(MutList.new(rc))
    }

    @Test
    def compress!03(): Bool = region rc {
        let v = MutList.new(rc);
        let c = capacity(v);
        MutList.push!(42, v);
        MutList.compress!(v);
        capacity(v) <= c and
        MutList.length(v) == 1
    }

    @Test
    def compress!04(): Bool = region rc {
        let v = MutList.range(rc, 0, 5);
        MutList.compress!(v);
        capacity(v) <= capacity(MutList.range(rc, 0, 5))
    }


    /////////////////////////////////////////////////////////////////////////////
    // minimumBy                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def minimumBy01(): Bool = region rc {
        MutList.minimumBy((x, y) -> x <=> y, MutList.new(rc): MutList[Int32, _]) == None
    }

    @Test
    def minimumBy02(): Bool = region rc {
        MutList.minimumBy((x, y) -> x <=> y, MutList.range(rc, 0, 5)) == Some(0)
    }

    @Test
    def minimumBy03(): Bool = region rc {
        MutList.minimumBy((x, y) -> x <=> y, MutList.range(rc, 5, 10)) == Some(5)
    }

    @Test
    def minimumBy04(): Bool = region rc {
        let v = MutList.range(rc, 0, 6);
        MutList.push!(-2, v);
        MutList.minimumBy((x, y) -> x <=> y, v) == Some(-2)
    }

    @Test
    def minimumBy05(): Bool = region rc {
        let v = MutList.range(rc, 9, 19);
        let cmp = (x, y) -> if (x < y and x `Int32.rem` 2 == 0) Comparison.LessThan else Comparison.GreaterThan;
        MutList.minimumBy(cmp, v) == Some(10)
    }


    /////////////////////////////////////////////////////////////////////////////
    // maximumBy                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def maximumBy01(): Bool = region rc {
        MutList.maximumBy((x, y) -> x <=> y, MutList.new(rc): MutList[Int32, _]) == None
    }

    @Test
    def maximumBy02(): Bool = region rc {
        MutList.maximumBy((x, y) -> x <=> y, MutList.range(rc, 0, 5)) == Some(4)
    }

    @Test
    def maximumBy03(): Bool = region rc {
        MutList.maximumBy((x, y) -> x <=> y, MutList.range(rc, 5, 10)) == Some(9)
    }

    @Test
    def maximumBy04(): Bool = region rc {
        let v = MutList.range(rc, 0, 6);
        MutList.push!(11, v);
        MutList.maximumBy((x, y) -> x <=> y, v) == Some(11)
    }

    @Test
    def maximumBy05(): Bool = region rc {
        let v = MutList.range(rc, 9, 19);
        let cmp = (x, y) -> if (x > y and x `Int32.rem` 2 == 0) Comparison.GreaterThan else Comparison.LessThan;
        MutList.maximumBy(cmp, v) == Some(18)
    }

    /////////////////////////////////////////////////////////////////////////////
    // toArray                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toArray01(): Bool = region rc {
        Array.sameElements(MutList.toArray(rc, Array.toMutList(rc, Array#{1, 2, 3} @ rc)), Array#{1, 2, 3} @ rc)
    }

    @Test
    def toArray02(): Bool = region rc {
        let v = Array.toMutList(rc, Array#{1, 2, 3} @ rc);
        let a = MutList.toArray(rc, v);
        Array.put(42, 1, a);
        MutList.nth(1, v) == Some(2)
    }

    @Test
    def toArray03(): Bool = region rc {
        let v = Array.toMutList(rc, Array#{1, 2, 3} @ rc);
        discard MutList.pop!(v);
        discard MutList.pop!(v);
        Array.length(MutList.toArray(rc, v)) == 1
    }

    /////////////////////////////////////////////////////////////////////////////
    // toVector                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toVector01(): Bool = region rc {
        MutList.toVector(MutList.new(rc) : MutList[Int32, _]) `Vector.equals` Vector.empty()
    }

    @Test
    def toVector02(): Bool = region rc {
        let l = MutList.new(rc);
        MutList.push!(0, l);
        MutList.toVector(l) `Vector.equals` Vector.singleton(0)
    }

    @Test
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

    @Test
    def toList01(): Bool = region rc {
        MutList.new(rc): MutList[Int32, _] |> MutList.toList == Nil: List[Int32]
    }

    @Test
    def toList02(): Bool = region rc {
        let ml = MutList.range(rc, 0, 1000);
        ml |> MutList.toList == List.range(0, 1000) and
            not (MutList.isEmpty(ml))
    }

    @Test
    def toList03(): Bool = region rc {
        let ml = MutList.range(rc, 0, 1000);
        ml |> MutList.toList == List.range(0, 1000) and
           ml `sameElements` MutList.range(rc, 0, 1000)
    }


    /////////////////////////////////////////////////////////////////////////////
    // toChain                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toChain01(): Bool = region rc {
        let l = MutList.new(rc);
        let xs: List[Int32] = Nil;
        List.forEach(x -> MutList.push!(x, l), xs);
        MutList.toChain(l) == Chain.empty()
    }

    @Test
    def toChain02(): Bool = region rc {
        let l = MutList.new(rc);
        let xs = 1 :: Nil;
        List.forEach(x -> MutList.push!(x, l), xs);
        MutList.toChain(l) == Chain.singleton(1)
    }

    @Test
    def toChain03(): Bool = region rc {
        let l = MutList.new(rc);
        let xs = 1 :: 2 :: Nil;
        List.forEach(x -> MutList.push!(x, l), xs);
        MutList.toChain(l) == List.toChain(1 :: 2 :: Nil)
    }

    @Test
    def toChain04(): Bool = region rc {
        let l = MutList.new(rc);
        let xs = 1 :: 2 :: 3 :: Nil;
        List.forEach(x -> MutList.push!(x, l), xs);
        MutList.toChain(l) == List.toChain(1 :: 2 :: 3 :: Nil)
    }


    /////////////////////////////////////////////////////////////////////////////
    // toMutDeque                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toMutDeque01(): Bool = region rc {
        let l = MutList.new(rc): MutList[Int32, _];
        let d = MutDeque.new(rc): MutDeque[Int32, _];

        MutList.toMutDeque(rc, l) `MutDeque.sameElements` d
    }

    @Test
    def toMutDeque02(): Bool = region rc {
        let xs = 1 :: Nil;

        let l = MutList.new(rc);
        List.forEach(x -> MutList.push!(x, l), xs);

        let d = MutDeque.new(rc);
        List.forEach(x -> MutDeque.pushBack(x, d), xs);

        MutList.toMutDeque(rc, l) `MutDeque.sameElements` d
    }

    @Test
    def toMutDeque03(): Bool = region rc {
        let xs = 1 :: 2 :: Nil;

        let l = MutList.new(rc);
        List.forEach(x -> MutList.push!(x, l), xs);

        let d = MutDeque.new(rc);
        List.forEach(x -> MutDeque.pushBack(x, d), xs);

        MutList.toMutDeque(rc, l) `MutDeque.sameElements` d
    }

    @Test
    def toMutDeque04(): Bool = region rc {
        let xs = 1 :: 2 :: 3 :: Nil;

        let l = MutList.new(rc);
        List.forEach(x -> MutList.push!(x, l), xs);

        let d = MutDeque.new(rc);
        List.forEach(x -> MutDeque.pushBack(x, d), xs);

        MutList.toMutDeque(rc, l) `MutDeque.sameElements` d
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

    @Test
    def sortWith01(): Bool = region rc {
        let a = MutList.sortWith(rc, cmp, Array.toMutList(rc, (Array#{} @ rc): Array[Int32, rc]));
        a `sameElements` Array.toMutList(rc, Array#{} @ rc)
    }

    @Test
    def sortWith02(): Bool = region rc {
        let a = MutList.sortWith(rc, cmp, Array.toMutList(rc, Array#{0} @ rc));
        a `sameElements` Array.toMutList(rc, Array#{0} @ rc)
    }

    @Test
    def sortWith03(): Bool = region rc {
        let a = MutList.sortWith(rc, cmp, Array.toMutList(rc, Array#{0, 1} @ rc));
        a `sameElements` Array.toMutList(rc, Array#{0,1} @ rc)
    }

    @Test
    def sortWith04(): Bool = region rc {
        let a = MutList.sortWith(rc, cmp, Array.toMutList(rc, Array#{1,0} @ rc));
        a `sameElements` Array.toMutList(rc, Array#{0,1} @ rc)
    }

    @Test
    def sortWith05(): Bool = region rc {
        let a = MutList.sortWith(rc, cmp, Array.toMutList(rc, Array#{1,1} @ rc));
        a `sameElements` Array.toMutList(rc, Array#{1,1} @ rc)
    }

    @Test
    def sortWith06(): Bool = region rc {
        let a = MutList.sortWith(rc, cmp, Array.toMutList(rc, Array#{0,1,2,3,4,5} @ rc));
        a `sameElements` Array.toMutList(rc, Array#{0,1,2,3,4,5} @ rc)
    }

    @Test
    def sortWith07(): Bool = region rc {
        let a = MutList.sortWith(rc, cmp, Array.toMutList(rc, Array#{5,4,3,2,1,0} @ rc));
        a `sameElements` Array.toMutList(rc, Array#{0,1,2,3,4,5} @ rc)
    }

    @Test
    def sortWith08(): Bool = region rc {
        let a = MutList.sortWith(rc, cmp, Array.toMutList(rc, Array#{5,3,0,4,1,2} @ rc));
        a `sameElements` Array.toMutList(rc, Array#{0,1,2,3,4,5} @ rc)
    }

    @Test
    def sortWith09(): Bool = region rc {
        let a = MutList.sortWith(rc, cmp, Array.toMutList(rc, Array#{2,3,0,4,1,2} @ rc));
        a `sameElements` Array.toMutList(rc, Array#{0,1,2,2,3,4} @ rc)
    }

    @Test
    def sortWith10(): Bool = region rc {
        let a = MutList.sortWith(rc, flip(cmp), Array.toMutList(rc, Array#{0,1,2,3,4,5} @ rc));
        a `sameElements` Array.toMutList(rc, Array#{5,4,3,2,1,0} @ rc)
    }

    @Test
    def sortWith11(): Bool = region rc {
        let a = MutList.sortWith(rc, flip(cmp), Array.toMutList(rc, Array#{5,4,3,2,1,0} @ rc));
        a `sameElements` Array.toMutList(rc, Array#{5,4,3,2,1,0} @ rc)
    }

    @Test
    def sortWith12(): Bool = region rc {
        let a = MutList.sortWith(rc, flip(cmp), Array.toMutList(rc, Array#{5,3,0,4,1,2} @ rc));
        a `sameElements` Array.toMutList(rc, Array#{5,4,3,2,1,0} @ rc)
    }


    @Test
    def sortWith13(): Bool = region rc {
        let a = MutList.sortWith(rc, flip(cmp), Array.toMutList(rc, Array#{2,3,0,4,1,2} @ rc));
        a `sameElements` Array.toMutList(rc, Array#{4,3,2,2,1,0} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // sort                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    def testSortVsSortWith(a: MutList[Int32, rc]): Bool \ rc = region rc1 {
        MutList.sort(rc1, a) `sameElements` MutList.sortWith(rc1, cmp, a)
    }

    @Test
    def sort01(): Bool = region rc {
        testSortVsSortWith(Array.toMutList(rc, (Array#{} @ rc): Array[Int32, rc]))
    }

    @Test
    def sort02(): Bool = region rc {
        testSortVsSortWith(Array.toMutList(rc, Array#{0} @ rc))
    }

    @Test
    def sort03(): Bool = region rc {
        testSortVsSortWith(Array.toMutList(rc, Array#{0,1} @ rc))
    }

    @Test
    def sort04(): Bool = region rc {
        testSortVsSortWith(Array.toMutList(rc, Array#{1,0} @ rc))
    }

    @Test
    def sort05(): Bool = region rc {
        testSortVsSortWith(Array.toMutList(rc, Array#{1,1} @ rc))
    }

    @Test
    def sort06(): Bool = region rc {
        testSortVsSortWith(Array.toMutList(rc, Array#{0,1,2,3,4,5} @ rc))
    }

    @Test
    def sort07(): Bool = region rc {
        testSortVsSortWith(Array.toMutList(rc, Array#{5,4,3,2,1,0} @ rc))
    }

    @Test
    def sort08(): Bool = region rc {
        testSortVsSortWith(Array.toMutList(rc, Array#{5,3,0,4,1,2} @ rc))
    }

    @Test
    def sort09(): Bool = region rc {
        testSortVsSortWith(Array.toMutList(rc, Array#{2,3,0,4,1,2} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // sortWith!                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sortWith01!(): Bool = region rc {
        let a = Array.toMutList(rc, (Array#{} @ rc): Array[Int32, rc]);
        MutList.sortWith!(cmp, a);
        a `sameElements` Array.toMutList(rc, (Array#{} @ rc): Array[Int32, rc])
    }

    @Test
    def sortWith02!(): Bool = region rc {
        let a = Array.toMutList(rc, Array#{0} @ rc);
        MutList.sortWith!(cmp, a);
        a `sameElements` Array.toMutList(rc, Array#{0} @ rc)
    }

    @Test
    def sortWith03!(): Bool = region rc {
        let a = Array.toMutList(rc, Array#{0,1} @ rc);
        MutList.sortWith!(cmp, a);
        a `sameElements` Array.toMutList(rc, Array#{0,1} @ rc)
    }

    @Test
    def sortWith04!(): Bool = region rc {
        let a = Array.toMutList(rc, Array#{1,0} @ rc);
        MutList.sortWith!(cmp, a);
        a `sameElements` Array.toMutList(rc, Array#{0,1} @ rc)
    }
    @Test
    def sortWith05!(): Bool = region rc {
        let a = Array.toMutList(rc, Array#{1,1} @ rc);
        MutList.sortWith!(cmp, a);
        a `sameElements` Array.toMutList(rc, Array#{1,1} @ rc)
    }

    @Test
    def sortWith06!(): Bool = region rc {
        let a = Array.toMutList(rc, Array#{0,1,2,3,4,5} @ rc);
        MutList.sortWith!(cmp, a);
        a `sameElements` Array.toMutList(rc, Array#{0,1,2,3,4,5} @ rc)
    }

    @Test
    def sortWith07!(): Bool = region rc {
        let a = Array.toMutList(rc, Array#{5,4,3,2,1,0} @ rc);
        MutList.sortWith!(cmp, a);
        a `sameElements` Array.toMutList(rc, Array#{0,1,2,3,4,5} @ rc)
    }

    @Test
    def sortWith08!(): Bool = region rc {
        let a = Array.toMutList(rc, Array#{5,3,0,4,1,2} @ rc);
        MutList.sortWith!(cmp, a);
        a `sameElements` Array.toMutList(rc, Array#{0,1,2,3,4,5} @ rc)
    }

    @Test
    def sortWith09!(): Bool = region rc {
        let a = Array.toMutList(rc, Array#{2,3,0,4,1,2} @ rc);
        MutList.sortWith!(cmp, a);
        a `sameElements` Array.toMutList(rc, Array#{0,1,2,2,3,4} @ rc)
    }

    @Test
    def sortWith10!(): Bool = region rc {
        let a = Array.toMutList(rc, Array#{0,1,2,3,4,5} @ rc);
        MutList.sortWith!(flip(cmp), a);
        a `sameElements` Array.toMutList(rc, Array#{5,4,3,2,1,0} @ rc)
    }

    @Test
    def sortWith11!(): Bool = region rc {
        let a = Array.toMutList(rc, Array#{5,4,3,2,1,0} @ rc);
        MutList.sortWith!(flip(cmp), a);
        a `sameElements` Array.toMutList(rc, Array#{5,4,3,2,1,0} @ rc)
    }

    @Test
    def sortWith12!(): Bool = region rc {
        let a = Array.toMutList(rc, Array#{5,3,0,4,1,2} @ rc);
        MutList.sortWith!(flip(cmp), a);
        a `sameElements` Array.toMutList(rc, Array#{5,4,3,2,1,0} @ rc)
    }

    @Test
    def sortWith13!(): Bool = region rc {
        let a = Array.toMutList(rc, Array#{2,3,0,4,1,2} @ rc);
        MutList.sortWith!(flip(cmp), a);
        a `sameElements` Array.toMutList(rc, Array#{4,3,2,2,1,0} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // sort!                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    def testSort!VsSortWith!(a: MutList[Int32, r]): Bool \ r = region rc1 {
        let b = MutList.copy(rc1, a);
        let c = MutList.copy(rc1, a);
        MutList.sort!(b);
        MutList.sortWith!(cmp, c);
        b `sameElements` c
    }

    @Test
    def sort!01(): Bool = region rc {
        testSort!VsSortWith!(Array.toMutList(rc, (Array#{} @ rc): Array[Int32, rc]))
    }

    @Test
    def sort!02(): Bool = region rc {
        testSort!VsSortWith!(Array.toMutList(rc, Array#{0} @ rc))
    }

    @Test
    def sort!03(): Bool = region rc {
        testSort!VsSortWith!(Array.toMutList(rc, Array#{0,1} @ rc))
    }

    @Test
    def sort!04(): Bool = region rc {
        testSort!VsSortWith!(Array.toMutList(rc, Array#{1,0} @ rc))
    }

    @Test
    def sort!05(): Bool = region rc {
        testSort!VsSortWith!(Array.toMutList(rc, Array#{1,1} @ rc))
    }

    @Test
    def sort!06(): Bool = region rc {
        testSort!VsSortWith!(Array.toMutList(rc, Array#{0,1,2,3,4,5} @ rc))
    }

    @Test
    def sort!07(): Bool = region rc {
        testSort!VsSortWith!(Array.toMutList(rc, Array#{5,4,3,2,1,0} @ rc))
    }

    @Test
    def sort!08(): Bool = region rc {
        testSort!VsSortWith!(Array.toMutList(rc, Array#{5,3,0,4,1,2} @ rc))
    }

    @Test
    def sort!09(): Bool = region rc {
        testSort!VsSortWith!(Array.toMutList(rc, Array#{2,3,0,4,1,2} @ rc))
    }


    /////////////////////////////////////////////////////////////////////////////
    // sortBy                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    def testSortByVsSort(a: MutList[Int32, r]): Bool \ r = region rc1 {
        (MutList.sortBy(rc1, identity, a) `sameElements` MutList.sort(rc1, a)) and
        (MutList.sortBy(rc1, x -> 4 * x + 7, a) `sameElements` MutList.sort(rc1, a)) and
        (MutList.sortBy(rc1, x -> -x, a) `sameElements` MutList.sortWith(rc1, flip(cmp),a))
    }

    @Test
    def sortBy01(): Bool = region rc {
        testSortByVsSort(Array.toMutList(rc, (Array#{} @ rc): Array[Int32, rc]))
    }

    @Test
    def sortBy02(): Bool = region rc {
        testSortByVsSort(Array.toMutList(rc, Array#{0} @ rc))
    }

    @Test
    def sortBy03(): Bool = region rc {
        testSortByVsSort(Array.toMutList(rc, Array#{0,1} @ rc))
    }

    @Test
    def sortBy04(): Bool = region rc {
        testSortByVsSort(Array.toMutList(rc, Array#{1,0} @ rc))
    }

    @Test
    def sortBy05(): Bool = region rc {
        testSortByVsSort(Array.toMutList(rc, Array#{1,1} @ rc))
    }

    @Test
    def sortBy06(): Bool = region rc {
        testSortByVsSort(Array.toMutList(rc, Array#{0,1,2,3,4,5} @ rc))
    }

    @Test
    def sortBy07(): Bool = region rc {
        testSortByVsSort(Array.toMutList(rc, Array#{5,4,3,2,1,0} @ rc))
    }

    @Test
    def sortBy08(): Bool = region rc {
        testSortByVsSort(Array.toMutList(rc, Array#{5,3,0,4,1,2} @ rc))
    }

    @Test
    def sortBy09(): Bool = region rc {
        testSortByVsSort(Array.toMutList(rc, Array#{2,3,0,4,1,2} @ rc))
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

    @Test
    def sortBy10(): Bool = region rc {
        MutList.sortBy(rc, r -> let R.R(x) = r; x.i, Array.toMutList(rc, Array#{R.R({i = 2, s = "A"}), R.R({i = 1, s = "B"}), R.R({i = 3, s = "C"})} @ rc))
        `sameElements` Array.toMutList(rc, Array#{R.R({i = 1, s = "B"}), R.R({i = 2, s = "A"}), R.R({i = 3, s = "C"})} @ rc)
    }


    /////////////////////////////////////////////////////////////////////////////
    // sortBy!                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    def testSortBy!VsSortBy(a: MutList[Int32, rc]): Bool \ rc = region rc1 {
        let b = MutList.copy(rc1, a);
        let c = MutList.copy(rc1, a);
        MutList.sortBy!(identity, b);
        MutList.sortBy!(x -> 4 * x + 7, c);
        (b `sameElements` MutList.sortBy(rc1, x -> 4 * x + 7, a)) and
        (c `sameElements` MutList.sortBy(rc1, identity, a))
    }

    @Test
    def sortBy!01(): Bool = region rc {
        testSortBy!VsSortBy(Array.toMutList(rc, (Array#{} @ rc): Array[Int32, rc]))
    }

    @Test
    def sortBy!02(): Bool = region rc {
        testSortBy!VsSortBy(Array.toMutList(rc, Array#{0} @ rc))
    }

    @Test
    def sortBy!03(): Bool = region rc {
        testSortBy!VsSortBy(Array.toMutList(rc, Array#{0,1} @ rc))
    }

    @Test
    def sortBy!04(): Bool = region rc {
        testSortBy!VsSortBy(Array.toMutList(rc, Array#{1,0} @ rc))
    }

    @Test
    def sortBy!05(): Bool = region rc {
        testSortBy!VsSortBy(Array.toMutList(rc, Array#{1,1} @ rc))
    }

    @Test
    def sortBy!06(): Bool = region rc {
        testSortBy!VsSortBy(Array.toMutList(rc, Array#{0,1,2,3,4,5} @ rc))
    }

    @Test
    def sortBy!07(): Bool = region rc {
        testSortBy!VsSortBy(Array.toMutList(rc, Array#{5,4,3,2,1,0} @ rc))
    }

    @Test
    def sortBy!08(): Bool = region rc {
        testSortBy!VsSortBy(Array.toMutList(rc, Array#{5,3,0,4,1,2} @ rc))
    }

    @Test
    def sortBy!09(): Bool = region rc {
        testSortBy!VsSortBy(Array.toMutList(rc, Array#{2,3,0,4,1,2} @ rc))
    }


    /////////////////////////////////////////////////////////////////////////////
    // sum                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sum01(): Bool = region rc {
        MutList.new(rc) |> MutList.sum == 0
    }

    @Test
    def sum02(): Bool = region rc {
        let l = MutList.new(rc);
        let xs = 1 :: Nil;
        List.forEach(x -> MutList.push!(x, l), xs);
        MutList.sum(l) == 1
    }

    @Test
    def sum03(): Bool = region rc {
        let l = MutList.new(rc);
        let xs = 1 :: 2 :: 3 :: Nil;
        List.forEach(x -> MutList.push!(x, l), xs);
        MutList.sum(l) == 6
    }

    @Test
    def sum04(): Bool = region rc {
        let l = MutList.new(rc);
        let xs = 1 :: 2 :: 3 :: -3 :: Nil;
        List.forEach(x -> MutList.push!(x, l), xs);
        MutList.sum(l) == 3
    }

    @Test
    def sum05(): Bool = region rc {
        let l = MutList.new(rc);
        let xs = -1 :: -2 :: -3 :: -4 :: Nil;
        List.forEach(x -> MutList.push!(x, l), xs);
        MutList.sum(l) == -10
    }

    @Test
    def sum06(): Bool = region rc {
        let l = MutList.new(rc);
        let xs = 10 :: -10 :: Nil;
        List.forEach(x -> MutList.push!(x, l), xs);
        MutList.sum(l) == 0
    }

    @Test
    def sum07(): Bool = region rc {
        let l = MutList.new(rc);
        let xs = List.range(1, 101);
        List.forEach(x -> MutList.push!(x, l), xs);
        MutList.sum(l) == 5050
    }


    /////////////////////////////////////////////////////////////////////////////
    // sumWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sumWith01(): Bool = region rc {
        MutList.new(rc) |> MutList.sumWith(x -> x + 1) == 0
    }

    @Test
    def sumWith02(): Bool = region rc {
        let l = MutList.new(rc);
        let xs = 1 :: Nil;
        List.forEach(x -> MutList.push!(x, l), xs);
        MutList.sumWith(x -> x + 1, l) == 2
    }

    @Test
    def sumWith03(): Bool = region rc {
        let l = MutList.new(rc);
        let xs = 1 :: 2 :: 3 :: Nil;
        List.forEach(x -> MutList.push!(x, l), xs);
        MutList.sumWith(x -> x + 1, l) == 9
    }

    @Test
    def sumWith04(): Bool = region rc {
        let l = MutList.new(rc);
        let xs = 1 :: 2 :: 3 :: -3 :: Nil;
        List.forEach(x -> MutList.push!(x, l), xs);
        MutList.sumWith(x -> x + 1, l) == 7
    }

    @Test
    def sumWith05(): Bool = region rc {
        let l = MutList.new(rc);
        let xs = -1 :: -2 :: -3 :: -4 :: Nil;
        List.forEach(x -> MutList.push!(x, l), xs);
        MutList.sumWith(x -> x + 1, l) == -6
    }

    @Test
    def sumWith06(): Bool = region rc {
        let l = MutList.new(rc);
        let xs = 10 :: -10 :: Nil;
        List.forEach(x -> MutList.push!(x, l), xs);
        MutList.sumWith(x -> x + 1, l) == 2
    }


    /////////////////////////////////////////////////////////////////////////////
    // join                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def join01(): Bool = region rc {
        (MutList.new(rc): MutList[Int32, _]) |> MutList.join(",") == ""
    }

    @Test
    def join02(): Bool = region rc {
        let l = MutList.new(rc);
        let xs = 1 :: Nil;
        List.forEach(x -> MutList.push!(x, l), xs);
        MutList.join(",", l) == "1"
    }

    @Test
    def join03(): Bool = region rc {
        let l = MutList.new(rc);
        let xs = 1 :: 2 :: 3 :: Nil;
        List.forEach(x -> MutList.push!(x, l), xs);
        MutList.join(",", l) == "1,2,3"
    }

    @Test
    def join04(): Bool = region rc {
        let l = MutList.new(rc);
        let xs = "1" :: "2" :: "3" :: Nil;
        List.forEach(x -> MutList.push!(x, l), xs);
        MutList.join(",", l) == "1,2,3"
    }


    /////////////////////////////////////////////////////////////////////////////
    // joinWith                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def joinWith01(): Bool = region rc {
        (MutList.new(rc): MutList[Int32, _]) |> MutList.joinWith(x -> "${x + 1}", ",") == ""
    }

    @Test
    def joinWith02(): Bool = region rc {
        let l = MutList.new(rc);
        let xs = 1 :: Nil;
        List.forEach(x -> MutList.push!(x, l), xs);
        MutList.joinWith(x -> "${x + 1}", ",", l) == "2"
    }

    @Test
    def joinWith03(): Bool = region rc {
        let l = MutList.new(rc);
        let xs = 1 :: 2 :: 3 :: Nil;
        List.forEach(x -> MutList.push!(x, l), xs);
        MutList.joinWith(x -> "${x + 1}", ",", l) == "2,3,4"
    }

    @Test
    def joinWith04(): Bool = region rc {
        let l = MutList.new(rc);
        let xs = "1" :: "2" :: "3" :: Nil;
        List.forEach(x -> MutList.push!(x, l), xs);
        MutList.joinWith(x -> x + x, ",", l) == "11,22,33"
    }

    /////////////////////////////////////////////////////////////////////////////
    // shuffle                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def shuffle01(): Bool \ NonDet = region rc {
        let l: List[Int32] = Nil;
        let v = MutList.shuffle(rc, Random.new(), List.toMutList(rc, l));
        MutList.sort!(v);

        MutList.length(v) == 0 and v `MutList.sameElements` List.toMutList(rc, l)
    }

    @Test
    def shuffle02(): Bool \ NonDet = region rc {
        let l = 1 :: Nil;
        let v = MutList.shuffle(rc, Random.new(), List.toMutList(rc, l));
        MutList.sort!(v);

        MutList.length(v) == 1 and v `MutList.sameElements` List.toMutList(rc, l)
    }

    @Test
    def shuffle03(): Bool \ NonDet = region rc {
        let l = 1 :: 2 :: 3 :: Nil;
        let v = MutList.shuffle(rc, Random.new(), List.toMutList(rc, l));
        MutList.sort!(v);

        MutList.length(v) == 3 and v `MutList.sameElements` List.toMutList(rc, l)
    }

    @Test
    def shuffle04(): Bool \ NonDet = region rc {
        let l = 0 :: 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: 9 :: Nil;
        let v = MutList.shuffle(rc, Random.new(), List.toMutList(rc, l));
        MutList.sort!(v);

        MutList.length(v) == 10 and v `MutList.sameElements` List.toMutList(rc, l)
    }

}

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

namespace TestMutList {

    use ToString.toString;

    /////////////////////////////////////////////////////////////////////////////
    // Eq.eq                                                                   //
    /////////////////////////////////////////////////////////////////////////////
    
    @test
    def eq01(): Bool & Impure =
        let u: MutList[Int32] = MutList.new();
        let v: MutList[Int32] = MutList.new();
        u == v

    @test
    def eq02(): Bool & Impure =
        let u = MutList.new();
        MutList.push!(0, u);
        let v = MutList.new();
        u != v

    @test
    def eq03(): Bool & Impure =
        let u = MutList.new();
        MutList.push!(2, u);
        MutList.push!(3, u);
        let v = MutList.new();
        MutList.push!(2, v);
        MutList.push!(3, v);
        u == v
        
    @test
    def eq04(): Bool & Impure =
        let u = MutList.new();
        MutList.push!(2, u);
        MutList.push!(3, u);
        let v = MutList.new();
        MutList.push!(2, v);
        MutList.push!(2, v);
        u != v

    /////////////////////////////////////////////////////////////////////////////
    // Order.compare                                                           //
    /////////////////////////////////////////////////////////////////////////////

    // TODO

    /////////////////////////////////////////////////////////////////////////////
    // length                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def length01(): Bool & Impure = MutList.length(MutList.new()) == 0

    @test
    def length02(): Bool & Impure =
        let v = MutList.new();
        MutList.push!(42, v);
        MutList.length(v) == 1

    @test
    def length03(): Bool & Impure = MutList.length(MutList.range(0, 2)) == 2

    @test
    def length04(): Bool & Impure = MutList.length(MutList.range(0, 4)) == 4

    @test
    def length05(): Bool & Impure = MutList.length(MutList.range(0, 42)) == 42

    /////////////////////////////////////////////////////////////////////////////
    // isEmpty                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def isEmpty01(): Bool & Impure = MutList.isEmpty(MutList.new())

    @test
    def isEmpty02(): Bool & Impure = MutList.isEmpty(MutList.range(0, 0))

    @test
    def isEmpty03(): Bool & Impure = MutList.isEmpty(MutList.range(0, 1)) == false

    @test
    def isEmpty04(): Bool & Impure = MutList.isEmpty(MutList.range(0, 2)) == false

    @test
    def isEmpty05(): Bool & Impure = MutList.isEmpty(MutList.range(3, 4)) == false

    /////////////////////////////////////////////////////////////////////////////
    // memberOf                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def memberOf01(): Bool & Impure = MutList.memberOf(42, MutList.new()) == false

    @test
    def memberOf02(): Bool & Impure =
        let v = MutList.new();
        MutList.push!(2, v);
        MutList.memberOf(2, v)

    @test
    def memberOf03(): Bool & Impure =
        let v = MutList.new();
        MutList.push!(42, v);
        MutList.memberOf(2, v) == false

    @test
    def memberOf04(): Bool & Impure = MutList.memberOf(4, MutList.range(0, 4)) == false

    @test
    def memberOf05(): Bool & Impure = MutList.memberOf(0, MutList.range(0, 3))

    @test
    def memberOf06(): Bool & Impure = MutList.memberOf(2, MutList.range(0, 3))

    /////////////////////////////////////////////////////////////////////////////
    // count                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def count01(): Bool & Impure = MutList.count(x -> x > 2, MutList.range(0, 4)) == 1

    @test
    def count02(): Bool & Impure = MutList.count(x -> x >= 2, MutList.range(0, 4)) == 2

    @test
    def count03(): Bool & Impure = MutList.count(x -> x % 2 == 0, MutList.range(0, 10)) == 5

    @test
    def count04(): Bool & Impure = MutList.count(x -> x == 11, MutList.range(0, 21)) == 1

    /////////////////////////////////////////////////////////////////////////////
    // exists                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def exists01(): Bool & Impure = MutList.exists(x -> x > 9, MutList.range(0, 9)) == false

    @test
    def exists02(): Bool & Impure = MutList.exists(x -> x < 0, MutList.range(0, 9)) == false

    @test
    def exists03(): Bool & Impure = MutList.exists(x -> x == 8, MutList.range(0, 9))

    @test
    def exists04(): Bool & Impure = MutList.exists(x -> x > 0, MutList.range(0, 9))

    @test
    def exists05(): Bool & Impure = MutList.exists(x -> x == 0, MutList.new()) == false

    /////////////////////////////////////////////////////////////////////////////
    // forall                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def forall01(): Bool & Impure = MutList.forall(x -> x > 0, MutList.range(0, 9)) == false

    @test
    def forall02(): Bool & Impure = MutList.forall(x -> x >= 0, MutList.range(0, 9))

    @test
    def forall03(): Bool & Impure = MutList.forall(x -> x >= 0 and x < 9, MutList.range(0, 9))

    @test
    def forall05(): Bool & Impure = MutList.forall(x -> x > 0, MutList.new())

    /////////////////////////////////////////////////////////////////////////////
    // head                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def head01(): Bool & Impure =
        let v = MutList.new();
        MutList.push!(42, v);
        MutList.head(v) == Some(42)

    @test
    def head02(): Bool & Impure = MutList.head(MutList.new(): MutList[Unit]) == None

    @test
    def head03(): Bool & Impure =
        let v = MutList.new();
        MutList.push!(101, v);
        MutList.push!(102, v);
        MutList.head(v) == Some(101)

    /////////////////////////////////////////////////////////////////////////////
    // last                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def last01(): Bool & Impure = MutList.last(MutList.new(): MutList[Unit]) == None

    @test
    def last02(): Bool & Impure = MutList.last(MutList.range(0, 8)) == Some(7)

    @test
    def last03(): Bool & Impure =
        let v = MutList.new();
        MutList.push!('A', v);
        MutList.push!('B', v);
        MutList.last(v) == Some('B')

    /////////////////////////////////////////////////////////////////////////////
    // indexOfLeft                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def indexOfLeft01(): Bool & Impure = MutList.indexOfLeft(2, MutList.range(2, 3)) == Some(0)

    @test
    def indexOfLeft02(): Bool & Impure = MutList.indexOfLeft(10, MutList.range(0, 10)) == None

    @test
    def indexOfLeft03(): Bool & Impure =
        let v = MutList.new();
        MutList.push!('A', v);
        MutList.push!('B', v);
        MutList.push!('C', v);
        MutList.indexOfLeft('B', v) == Some(1)

    @test
    def indexOfLeft04(): Bool & Impure = MutList.indexOfLeft(99, MutList.range(0, 100)) == Some(99)

    /////////////////////////////////////////////////////////////////////////////
    // indexOfRight                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def indexOfRight01(): Bool & Impure = MutList.indexOfRight(2, MutList.range(2, 3)) == Some(0)

    @test
    def indexOfRight02(): Bool & Impure = MutList.indexOfRight(10, MutList.range(0, 10)) == None

    @test
    def indexOfRight03(): Bool & Impure =
        let v = MutList.new();
        MutList.push!('A', v);
        MutList.push!('B', v);
        MutList.push!('A', v);
        MutList.indexOfRight('A', v) == Some(2)

    @test
    def indexOfRight04(): Bool & Impure =
        let v1 = MutList.range(0, 4);
        let v2 = MutList.range(0, 4);
        MutList.append!(v1, v2);
        MutList.indexOfRight(2, v1) == Some(6)

    /////////////////////////////////////////////////////////////////////////////
    // findLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def findLeft01(): Bool & Impure = MutList.findLeft(x -> x > 3, MutList.range(3, 11)) == Some(4)

    @test
    def findLeft02(): Bool & Impure = MutList.findLeft(x -> x > 10, MutList.range(3, 11)) == None

    @test
    def findLeft03(): Bool & Impure = MutList.findLeft(x -> x < 3, MutList.range(3, 11)) == None

    @test
    def findLeft04(): Bool & Impure = MutList.findLeft(x -> x % 2 == 1, MutList.range(3, 11)) == Some(3)

    /////////////////////////////////////////////////////////////////////////////
    // findRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def findRight01(): Bool & Impure = MutList.findRight(x -> x > 0, MutList.range(2, 5)) == Some(4)

    @test
    def findRight02(): Bool & Impure = MutList.findRight(x -> x == 10, MutList.range(9, 10)) == None

    @test
    def findRight03(): Bool & Impure = MutList.findRight(x -> x % 2 == 0, MutList.range(3, 4)) == None

    @test
    def findRight04(): Bool & Impure = MutList.findRight(x -> x < 5, MutList.range(3, 11)) == Some(4)

    /////////////////////////////////////////////////////////////////////////////
    // scanLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def scanLeft01(): Bool & Impure =
        let v = MutList.scanLeft((x, y) -> x + y, 0, MutList.new());
        MutList.length(v) == 1 and MutList.head(v) == Some(0)

    @test
    def scanLeft02(): Bool & Impure =
        let v1 = MutList.range(1, 6);
        let v2 = MutList.new();
        MutList.push!(0, v2);
        MutList.push!(1, v2);
        MutList.push!(3, v2);
        MutList.push!(6, v2);
        MutList.push!(10, v2);
        MutList.push!(15, v2);
        let v3 = MutList.scanLeft((x, y) -> x + y, 0, v1);
        MutList.sameElements(v2, v3)

    @test
    def scanLeft03(): Bool & Impure =
        let v = MutList.scanLeft((x, y) -> x + y, 7, MutList.range(5, 13));
        MutList.last(v) == Some(75)

    /////////////////////////////////////////////////////////////////////////////
    // scanRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def scanRight01(): Bool & Impure =
        let v = MutList.scanRight((x, y) -> x + y, 0, MutList.new());
        MutList.length(v) == 1 and MutList.head(v) == Some(0)

    @test
    def scanRight02(): Bool & Impure =
        let v1 = MutList.range(1, 6);
        let v2 = MutList.new();
        MutList.push!(15, v2);
        MutList.push!(14, v2);
        MutList.push!(12, v2);
        MutList.push!(9, v2);
        MutList.push!(5, v2);
        MutList.push!(0, v2);
        let v3 = MutList.scanRight((x, y) -> x + y, 0, v1);
        MutList.sameElements(v2, v3)

    @test
    def scanRight03(): Bool & Impure =
        let v = MutList.scanRight((x, y) -> x + y, 7, MutList.range(5, 13));
        MutList.last(v) == Some(7)

    /////////////////////////////////////////////////////////////////////////////
    // map                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def map01(): Bool & Impure =
        let v = MutList.map(x -> x * 2, MutList.range(0, 6));
        MutList.sameElements(v, Array.toMutList([0, 2, 4, 6, 8, 10]))

    @test
    def map02(): Bool & Impure =
        let v = MutList.map(identity, MutList.range(0, 10));
        MutList.sameElements(v, MutList.range(0, 10))

    @test
    def map03(): Bool & Impure =
        let v = MutList.map(identity, MutList.new(): MutList[Unit]);
        MutList.sameElements(v, MutList.new())

    @test
    def map04(): Bool & Impure =
        let v = MutList.map(x -> x + 1, MutList.range(0, 100));
        MutList.sameElements(v, MutList.range(1, 101))

    /////////////////////////////////////////////////////////////////////////////
    // transform!                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def transform!01(): Bool & Impure =
        let v = MutList.range(0, 6);
        MutList.transform!(x -> x * 2, v);
        MutList.sameElements(v, Array.toMutList([0, 2, 4, 6, 8, 10]))

    @test
    def transform!02(): Bool & Impure =
        let v = MutList.range(0, 10);
        MutList.transform!(identity, v);
        MutList.sameElements(v, MutList.range(0, 10))

    @test
    def transform!03(): Bool & Impure =
        let v: MutList[Unit] = MutList.new();
        MutList.transform!(identity, v);
        MutList.sameElements(v, MutList.new())

    @test
    def transform!04(): Bool & Impure =
        let v = MutList.range(0, 100);
        MutList.transform!(x -> x + 1, v);
        MutList.sameElements(v, MutList.range(1, 101))

    /////////////////////////////////////////////////////////////////////////////
    // mapWithIndex                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapWithIndex01(): Bool & Impure =
        let v = MutList.mapWithIndex((_, i) -> if (i < 3) 0 else 1, MutList.range(0, 6));
        MutList.sameElements(v, Array.toMutList([0, 0, 0, 1, 1, 1]))

    @test
    def mapWithIndex02(): Bool & Impure =
        let v = MutList.mapWithIndex((x, i) -> if (x == 8 and i == 1) 42 else 0, MutList.range(7, 10));
        MutList.sameElements(v, Array.toMutList([0, 42, 0]))

    @test
    def mapWithIndex03(): Bool & Impure =
        let v = MutList.mapWithIndex((_, i) -> if (i == 0) 42 else 0, MutList.new());
        MutList.sameElements(v, MutList.new())

    /////////////////////////////////////////////////////////////////////////////
    // transformWithIndex!                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def transformWithIndex!01(): Bool & Impure =
        let v = MutList.range(0, 6);
        MutList.transformWithIndex!((_, i) -> if (i < 3) 0 else 1, v);
        MutList.sameElements(v, Array.toMutList([0, 0, 0, 1, 1, 1]))

    @test
    def transformWithIndex!02(): Bool & Impure =
        let v = MutList.range(7, 10);
        MutList.transformWithIndex!((x, i) -> if (x == 8 and i == 1) 42 else 0, v);
        MutList.sameElements(v, Array.toMutList([0, 42, 0]))

    @test
    def transformWithIndex!03(): Bool & Impure =
        let v = MutList.new();
        MutList.transformWithIndex!((_, i) -> if (i == 0) 42 else 0, v);
        MutList.sameElements(v, MutList.new())

    /////////////////////////////////////////////////////////////////////////////
    // fold                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def fold01(): Bool & Impure =
        let v: MutList[Unit] = MutList.new();
        MutList.fold(v) == ()

    @test
    def fold02(): Bool & Impure =
        let v = MutList.new();
        MutList.push!("a", v);
        MutList.fold(v) == "a"

    @test
    def fold03(): Bool & Impure =
        let v = MutList.new();
        MutList.push!("a", v);
        MutList.push!("b", v);
        MutList.push!("c", v);
        MutList.fold(v) == "abc"

    @test
    def fold04(): Bool & Impure =
        let v = MutList.new();
        MutList.push!(("a", "d"), v);
        MutList.push!(("b", "e"), v);
        MutList.push!(("c", "f"), v);
        MutList.fold(v) == ("abc", "def")

    /////////////////////////////////////////////////////////////////////////////
    // foldLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldLeft01(): Bool & Impure = MutList.foldLeft((acc, x) -> acc + x, 0, MutList.range(1, 5)) == 10

    @test
    def foldLeft02(): Bool & Impure =
        let v1 = MutList.new();
        let v2 = MutList.range(0, 10);
        MutList.foldLeft((_, x) -> MutList.push!(x, v1), (), v2);
        MutList.sameElements(v1, v2)

    @test
    def foldLeft03(): Bool & Impure = MutList.foldLeft((acc, x) -> x :: acc, Nil, MutList.range(0, 6)) == 5 :: 4 :: 3 :: 2 :: 1 :: 0 :: Nil

    @test
    def foldLeft04(): Bool & Impure = MutList.foldLeft((acc, x) -> acc - x, 0, MutList.range(0, 100)) == -4950

    @test
    def foldLeft05(): Bool & Impure = MutList.foldRight((acc, x) -> acc + x, 42, MutList.new()) == 42

    /////////////////////////////////////////////////////////////////////////////
    // foldRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldRight01(): Bool & Impure = MutList.foldRight((x, acc) -> acc + x, 0, MutList.range(1, 5)) == 10

    @test
    def foldRight02(): Bool & Impure = MutList.foldRight((x, acc) -> Int32.toString(x) + acc, "", MutList.range(3, 8)) == "34567"

    @test
    def foldRight03(): Bool & Impure = MutList.foldRight((x, acc) -> x :: acc, Nil, MutList.range(0, 6)) == 0 :: 1 :: 2 :: 3 :: 4 :: 5 :: Nil

    @test
    def foldRight04(): Bool & Impure = MutList.foldRight((x, acc) -> x - acc, 0, MutList.range(0, 100)) == -50

    @test
    def foldRight05(): Bool & Impure = MutList.foldRight((x, acc) -> x + acc, 42, MutList.new()) == 42

    /////////////////////////////////////////////////////////////////////////////
    // reduceLeft                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reduceLeft01(): Bool & Impure = MutList.reduceLeft((x, y) -> x + y, MutList.new(): MutList[Int32]) == None

    @test
    def reduceLeft02(): Bool & Impure = MutList.reduceLeft((x, y) -> x + y, MutList.range(0, 5)) == Some(10)

    @test
    def reduceLeft03(): Bool & Impure = MutList.reduceLeft((x, y) -> x - y, MutList.range(0, 101)) == Some(-5050)

    @test
    def reduceLeft04(): Bool & Impure = MutList.reduceLeft((_, y) -> if (y > 4) 1 else 0, MutList.range(0, 6)) == Some(1)

    /////////////////////////////////////////////////////////////////////////////
    // reduceRight                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reduceRight01(): Bool & Impure = MutList.reduceRight((x, y) -> x + y, MutList.new(): MutList[Int32]) == None

    @test
    def reduceRight02(): Bool & Impure = MutList.reduceRight((x, y) -> x + y, MutList.range(0, 5)) == Some(10)

    @test
    def reduceRight03(): Bool & Impure = MutList.reduceRight((x, y) -> x - y, MutList.range(0, 6)) == Some(-3)

    @test
    def reduceRight04(): Bool & Impure = MutList.reduceRight((x, _) -> if (x < 2) 1 else 0, MutList.range(0, 6)) == Some(1)

    /////////////////////////////////////////////////////////////////////////////
    // clear!                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def clear!01(): Bool & Impure =
        let v = MutList.range(0, 1);
        let len = MutList.length(v);
        MutList.clear!(v);
        len == 1 and MutList.isEmpty(v)

    @test
    def clear!02(): Bool & Impure =
        let v = MutList.new();
        let len = MutList.length(v);
        MutList.clear!(v);
        len == MutList.length(v)

    @test
    def clear!03(): Bool & Impure =
        let v = MutList.range(0, 100);
        MutList.clear!(v);
        MutList.isEmpty(v)

    /////////////////////////////////////////////////////////////////////////////
    // copy                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def copy01(): Bool & Impure =
        let v1 = MutList.range(0, 6);
        let v2 = MutList.copy(v1);
        MutList.clear!(v1);
        MutList.length(v1) == 0 and MutList.length(v2) == 6

    @test
    def copy02(): Bool & Impure =
        let v1 = MutList.range(0, 100);
        let v2 = MutList.copy(v1);
        MutList.transform!(x -> x + 1, v2);
        MutList.last(v1) == Some(99) and MutList.last(v2) == Some(100)

    @test
    def copy03(): Bool & Impure =
        let v1 = MutList.range(0, 6);
        let v2 = MutList.copy(v1);
        MutList.clear!(v1);
        MutList.length(v1) == 0 and MutList.length(v2) == 6

    /////////////////////////////////////////////////////////////////////////////
    // pop                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def pop01(): Bool & Impure =
        let v = MutList.range(0, 6);
        let len = MutList.length(v);
        let last = MutList.pop!(v);
        MutList.length(v) == len - 1 and last == Some(5)

    @test
    def pop02(): Bool & Impure =
        let v: MutList[Unit] = MutList.new();
        MutList.pop!(v) == None

    @test
    def pop03(): Bool & Impure =
        let v = MutList.range(1, 4);
        let three = MutList.pop!(v);
        let two = MutList.pop!(v);
        let one = MutList.pop!(v);
        let none = MutList.pop!(v);
        three == Some(3) and two == Some(2) and one == Some(1) and none == None

    /////////////////////////////////////////////////////////////////////////////
    // push                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def push01(): Bool & Impure =
        let v = MutList.new();
        let len = MutList.length(v);
        MutList.push!(1, v);
        MutList.last(v) == Some(1) and MutList.length(v) == len + 1

    @test
    def push02(): Bool & Impure =
        let v = MutList.new();
        let len = MutList.length(v);
        List.foreach(x -> MutList.push!(x, v), List.range(0, 97));
        MutList.head(v) == Some(0) and MutList.last(v) == Some(96) and MutList.length(v) == len + 97

    @test
    def push03(): Bool & Impure =
        let v = MutList.new();
        MutList.push!(42, v);
        MutList.push!(31, v);
        MutList.push!(1019, v);
        MutList.length(v) == 3 and MutList.get(0, v) == 42 and MutList.get(1, v) == 31 and MutList.get(2, v) == 1019

    @test
    def push04(): Bool & Impure =
        let v = MutList.new();
        MutList.push!(0, v);
        MutList.push!(0, v);
        MutList.push!(0, v);
        MutList.push!(0, v);
        MutList.length(v) == 4 and MutList.get(0, v) == 0 and MutList.get(1, v) == 0 and MutList.get(2, v) == 0 and MutList.get(3, v) == 0

    /////////////////////////////////////////////////////////////////////////////
    // insert!                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def insert!01(): Bool & Impure =
        let v = MutList.new();
        MutList.insert!(42, 0, v);
        MutList.length(v) == 1 and MutList.head(v) == Some(42)

    @test
    def insert!02(): Bool & Impure =
        let v = MutList.new();
        MutList.insert!(107, 0, v);
        MutList.insert!(7, 0, v);
        MutList.length(v) == 2 and MutList.head(v) == Some(7)

    @test
    def insert!03(): Bool & Impure =
        let v = MutList.new();
        List.foreach(x -> MutList.insert!(x, 0, v), List.range(0, 107));
        MutList.length(v) == 107 and MutList.head(v) == Some(106)

    @test
    def insert!04(): Bool & Impure =
        let v = MutList.new();
        MutList.insert!(42, 0, v);
        MutList.insert!(3, 1, v);
        MutList.insert!(5, 2, v);
        MutList.insert!(6, 0, v);
        MutList.length(v) == 4 and MutList.head(v) == Some(6) and MutList.last(v) == Some(5)

    @test
    def insert!05(): Bool & Impure =
        let v = MutList.new();
        List.foreach(x -> MutList.push!(x, v), List.range(97, 147));
        MutList.insert!(42, 0, v);
        MutList.insert!(49, 0, v);
        MutList.length(v) == 52 and MutList.head(v) == Some(49) and MutList.last(v) == Some(146)

    @test
    def insert!06(): Bool & Impure =
        let v = MutList.new();
        MutList.insert!(42, 0, v);
        MutList.insert!(42, 0, v);
        MutList.insert!(42, 0, v);
        MutList.insert!(42, 1, v);
        MutList.insert!(97, 0, v);
        MutList.insert!(98, 2, v);
        MutList.length(v) == 6 and MutList.head(v) == Some(97) and MutList.last(v) == Some(42)

    @test
    def insert!07(): Bool & Impure =
        let v = MutList.new();
        MutList.insert!(97, 0, v);
        MutList.insert!(98, 1, v);
        MutList.insert!(99, 2, v);
        MutList.insert!(107, 1, v);
        MutList.length(v) == 4 and MutList.get(1, v) == 107

    @test
    def insert!08(): Bool & Impure =
        let v = MutList.range(0, 4);
        MutList.insert!(42, 3, v);
        MutList.insert!(41, 2, v);
        MutList.insert!(40, 1, v);
        MutList.insert!(55, 0, v);
        MutList.insert!(33, 1, v);
        MutList.sameElements(v, List.toMut(55 :: 33 :: 0 :: 40 :: 1 :: 41 :: 2 :: 42 :: 3 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // remove!                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def remove!01(): Bool & Impure =
        let v: MutList[Unit] = MutList.new();
        MutList.remove!(0, v);
        MutList.sameElements(v, MutList.new())

    @test
    def remove!02(): Bool & Impure =
        let v = MutList.range(0, 4);
        MutList.remove!(0, v);
        MutList.sameElements(v, MutList.range(1, 4))

    @test
    def remove!03(): Bool & Impure =
        let v = MutList.range(0, 4);
        MutList.remove!(3, v);
        MutList.sameElements(v, MutList.range(0, 3))

    @test
    def remove!04(): Bool & Impure =
        let v = MutList.range(3, 7);
        MutList.remove!(0, v);
        MutList.remove!(0, v);
        MutList.remove!(0, v);
        MutList.sameElements(v, MutList.range(6, 7))

    @test
    def remove!05(): Bool & Impure =
        let v = MutList.range(9, 12);
        MutList.remove!(2, v);
        MutList.remove!(1, v);
        MutList.remove!(0, v);
        MutList.sameElements(v, MutList.new())

    @test
    def remove!06(): Bool & Impure =
        let v = MutList.range(7, 13);
        MutList.remove!(0, v);
        MutList.remove!(0, v);
        MutList.remove!(0, v);
        MutList.remove!(0, v);
        MutList.remove!(0, v);
        MutList.remove!(0, v);
        MutList.sameElements(v, MutList.new())

    @test
    def remove!07(): Bool & Impure =
        let v = MutList.range(0, 3);
        MutList.remove!(3, v);
        MutList.remove!(7, v);
        MutList.remove!(99, v);
        MutList.sameElements(v, MutList.range(0, 3))

    /////////////////////////////////////////////////////////////////////////////
    // append!                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def append!01(): Bool & Impure =
        let v1 = MutList.range(0, 4);
        let v2 = MutList.range(4, 8);
        MutList.append!(v1, v2);
        MutList.sameElements(v1, MutList.range(0, 8))

    @test
    def append!02(): Bool & Impure =
        let v1 = MutList.new();
        let v2 = MutList.range(1, 4);
        MutList.append!(v1, v2);
        MutList.sameElements(v1, MutList.range(1, 4))

    @test
    def append!03(): Bool & Impure =
        let v1 = MutList.range(9, 24);
        let v2 = MutList.new();
        MutList.append!(v1, v2);
        MutList.sameElements(v1, MutList.range(9, 24))

    @test
    def append!04(): Bool & Impure =
        let v1: MutList[Unit] = MutList.new();
        let v2: MutList[Unit] = MutList.new();
        MutList.append!(v1, v2);
        MutList.sameElements(v1, MutList.new())

    @test
    def append!05(): Bool & Impure =
        let v1 = MutList.range(1, 3);
        let v2 = MutList.range(3, 5);
        let v3 = MutList.range(5, 99);
        MutList.append!(v1, v2);
        MutList.append!(v1, v3);
        MutList.sameElements(v1, MutList.range(1, 99))

    /////////////////////////////////////////////////////////////////////////////
    // retain!                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def retain!01(): Bool & Impure =
        let v = MutList.range(0, 10);
        MutList.retain!(x -> x % 2 == 0, v);
        MutList.sameElements(List.toMut(0 :: 2 :: 4 :: 6 :: 8 :: Nil), v)

    @test
    def retain!02(): Bool & Impure =
        let v = MutList.range(0, 10);
        MutList.retain!(x -> x % 2 == 1, v);
        MutList.sameElements(List.toMut(1 :: 3 :: 5 :: 7 :: 9 :: Nil), v)

    @test
    def retain!03(): Bool & Impure =
        let v = MutList.range(0, 10);
        MutList.retain!(x -> x > 5, v);
        MutList.sameElements(v, MutList.range(6, 10))

    @test
    def retain!04(): Bool & Impure =
        let v = MutList.range(0, 10);
        MutList.retain!(x -> x > 10, v);
        MutList.sameElements(v, MutList.new())

    /////////////////////////////////////////////////////////////////////////////
    // replace!                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def replace!01(): Bool & Impure =
        let v = MutList.new();
        MutList.push!(1, v);
        MutList.push!(2, v);
        MutList.push!(2, v);
        MutList.push!(1, v);
        MutList.push!(1, v);
        MutList.replace!(1, 42, v);
        MutList.sameElements(List.toMut(42 :: 2 :: 2 :: 42 :: 42 :: Nil), v)

    @test
    def replace!02(): Bool & Impure =
        let v = MutList.range(4, 7);
        MutList.replace!(4, 0, v);
        MutList.sameElements(List.toMut(0 :: 5 :: 6 :: Nil), v)

    @test
    def replace!03(): Bool & Impure =
        let v = MutList.range(4, 7);
        MutList.replace!(5, 0, v);
        MutList.sameElements(List.toMut(4 :: 0 :: 6 :: Nil), v)

    @test
    def replace!04(): Bool & Impure =
        let v = MutList.range(4, 7);
        MutList.replace!(6, 0, v);
        MutList.sameElements(List.toMut(4 :: 5 :: 0 :: Nil), v)

    @test
    def replace!05(): Bool & Impure =
        let v = List.toMut(List.repeat('a', 97));
        MutList.replace!('a', 'z', v);
        MutList.sameElements(List.toMut(List.repeat('z', 97)), v)

    /////////////////////////////////////////////////////////////////////////////
    // reverse!                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reverse01!(): Bool & Impure =
        let v: MutList[Unit] = MutList.new();
        MutList.reverse!(v);
        MutList.sameElements(v, MutList.new())

    @test
    def reverse02!(): Bool & Impure =
        let v = MutList.range(0, 1);
        MutList.reverse!(v);
        MutList.sameElements(v, MutList.range(0, 1))

    @test
    def reverse03!(): Bool & Impure =
        let v = MutList.range(0, 5);
        MutList.reverse!(v);
        MutList.sameElements(v, List.toMut(4 :: 3 :: 2 :: 1 :: 0 :: Nil))

    @test
    def reverse04!(): Bool & Impure =
        let v = MutList.range(0, 6);
        MutList.reverse!(v);
        MutList.sameElements(v, List.toMut(5 :: 4 :: 3 :: 2 :: 1 :: 0 :: Nil))

    @test
    def reverse05!(): Bool & Impure =
        let v = List.toMut(7 :: 6 :: 5 :: 4 :: 3 :: 2 :: Nil);
        MutList.reverse!(v);
        MutList.sameElements(v, MutList.range(2, 8))

    /////////////////////////////////////////////////////////////////////////////
    // shrinkTo!                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def shrinkTo!01(): Bool & Impure =
        let v = MutList.new();
        let c = MutList.capacity(v);
        MutList.shrinkTo!(0, v);
        c == MutList.capacity(v) and MutList.length(v) == 0

    @test
    def shrinkTo!02(): Bool & Impure =
        let v = MutList.range(0, 99);
        let c1 = MutList.capacity(v);
        MutList.shrinkTo!(90, v);
        let c2 = MutList.capacity(v);
        MutList.shrinkTo!(2, v);
        let c3 = MutList.capacity(v);
        MutList.shrinkTo!(1, v);
        let c4 = MutList.capacity(v);
        MutList.shrinkTo!(0, v);
        c1 == 99 and c2 == 90 and c3 == 8 and c4 == 8 and MutList.capacity(v) == 8 and MutList.length(v) == 8

    @test
    def shrinkTo!03(): Bool & Impure =
        let v = MutList.new();
        MutList.push!(3, v);
        MutList.push!(4, v);
        MutList.push!(5, v);
        MutList.push!(6, v);
        MutList.push!(7, v);
        MutList.shrinkTo!(4, v);
        let c1 = MutList.capacity(v);
        let m1 = MutList.shrinkTo!(2, v);
        let c2 = MutList.capacity(v);
        MutList.shrinkTo!(0, v);
        c1 == 8 and c2 == 8 and MutList.capacity(v) == 8 and MutList.length(m1) == 8 and MutList.length(v) == 5

    @test
    def shrinkTo!04(): Bool & Impure =
        let v = MutList.range(0, 10);
        MutList.shrinkTo!(11, v);
        let c = MutList.capacity(v);
        MutList.shrinkTo!(10, v);
        c == 10 and MutList.capacity(v) == 10 and MutList.length(v) == 10

    @test
    def shrinkTo!05(): Bool & Impure =
        let v = MutList.range(0, 10);
        MutList.shrinkTo!(1, v);
        MutList.capacity(v) == 8 and MutList.length(v) == 8

    @test
    def shrinkTo!06(): Bool & Impure =
        let v = MutList.range(0, 4);
        MutList.push!(42, v);
        MutList.insert!(99, 3, v);
        MutList.shrinkTo!(3, v);
        MutList.capacity(v) == 8 and MutList.length(v) == 6

    /////////////////////////////////////////////////////////////////////////////
    // shrink!                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def shrink!01(): Bool & Impure =
        let v = MutList.new();
        let c = MutList.capacity(v);
        MutList.push!(0, v);
        MutList.push!(1, v);
        MutList.push!(2, v);
        MutList.shrink!(v);
        c == 8 and MutList.capacity(v) == 8 and MutList.length(v) == 3

    @test
    def shrink!02(): Bool & Impure =
        let v = MutList.new();
        let c = MutList.capacity(v);
        MutList.shrink!(v);
        c == 8 and MutList.capacity(v) == 8 and MutList.length(v) == 0

    @test
    def shrink!03(): Bool & Impure =
        let v = MutList.range(0, 9);
        let c = MutList.capacity(v);
        MutList.push!(9, v);
        MutList.shrink!(v);
        c == 9 and MutList.capacity(v) == 10 and MutList.length(v) == 10

    @test
    def shrink!04(): Bool & Impure =
        let v = MutList.new();
        let c = MutList.capacity(v);
        MutList.push!(4, v);
        MutList.append!(v, MutList.range(0, 4));
        MutList.push!(5, v);
        MutList.shrink!(v);
        c == 8 and MutList.capacity(v) == 8 and MutList.length(v) == 6

    /////////////////////////////////////////////////////////////////////////////
    // truncate!                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def truncate!01(): Bool & Impure =
        let v: MutList[Unit] = MutList.new();
        MutList.truncate!(0, v);
        MutList.sameElements(v, MutList.new())

    @test
    def truncate!02(): Bool & Impure =
        let v = MutList.range(0, 10);
        MutList.truncate!(5, v);
        MutList.sameElements(v, MutList.range(0, 5))

    @test
    def truncate!03(): Bool & Impure =
        let v = MutList.range(0, 99);
        MutList.truncate!(-1, v);
        MutList.sameElements(v, MutList.new())

    @test
    def truncate!04(): Bool & Impure =
        let v = MutList.range(7, 8);
        MutList.truncate!(2, v);
        MutList.sameElements(v, MutList.range(7, 8))

    @test
    def truncate!05(): Bool & Impure =
        let v = MutList.range(99, 102);
        MutList.truncate!(1, v);
        MutList.sameElements(v, MutList.range(99, 100))

    /////////////////////////////////////////////////////////////////////////////
    // reserve!                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reserve!01(): Bool & Impure =
        let v = MutList.new();
        MutList.reserve!(4, v);
        MutList.capacity(v) == 4

    @test
    def reserve!02(): Bool & Impure =
        let v = MutList.range(0, 5);
        MutList.reserve!(5, v);
        MutList.capacity(v) == 10

    @test
    def reserve!03(): Bool & Impure =
        let v = MutList.range(0, 99);
        MutList.reserve!(1, v);
        MutList.capacity(v) == 100

    @test
    def reserve!04(): Bool & Impure =
        let v = MutList.range(0, 100);
        MutList.reserve!(100, v);
        MutList.capacity(v) == 200

    @test
    def reserve!05(): Bool & Impure =
        let n = 5;
        let v = MutList.range(0, n);
        MutList.reserve!(MutList.length(v), v);
        MutList.capacity(v) == 2 * n

    @test
    def reserve!06(): Bool & Impure =
        let n = 197;
        let v = MutList.range(0, n);
        MutList.reserve!(1, v);
        MutList.reserve!(1, v);
        MutList.reserve!(1, v);
        MutList.reserve!(1, v);
        MutList.reserve!(1, v);
        MutList.capacity(v) == n + 1

    /////////////////////////////////////////////////////////////////////////////
    // formatWith                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def formatWith01(): Bool & Impure =
        let v = MutList.range(0, 5);
        MutList.formatWith(Int32.toString, ", ", v) == "0, 1, 2, 3, 4"

    @test
    def formatWith02(): Bool & Impure =
        let v = MutList.range(1, 4);
        MutList.formatWith(Int32.toString, "+", v) == "1+2+3"

    @test
    def formatWith03(): Bool & Impure =
        let v = MutList.range(2, 3);
        MutList.formatWith(Int32.toString, "/", v) == "2"

    @test
    def formatWith04(): Bool & Impure =
        let v = MutList.range(6, 9);
        MutList.formatWith(Int32.toString, "/", v) == "6/7/8"

    /////////////////////////////////////////////////////////////////////////////
    // foreach                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foreach01(): Bool & Impure =
        let v = MutList.new();
        let sb = StringBuilder.new();
        let f = x -> if (x > 0) StringBuilder.appendChar!(sb, 'T') else StringBuilder.appendChar!(sb, 'F');
        MutList.foreach(f, v);
        StringBuilder.toString(sb) == ""

    @test
    def foreach02(): Bool & Impure =
        let v = MutList.range(0, 5);
        let sb = StringBuilder.new();
        let f = x -> if (x % 2 == 0) StringBuilder.appendChar!(sb, 'T') else StringBuilder.appendChar!(sb, 'F');
        MutList.foreach(f, v);
        StringBuilder.toString(sb) == "TFTFT"

    @test
    def foreach03(): Bool & Impure =
        let v = MutList.range(0, 2);
        let sb = StringBuilder.new();
        let f = x -> if (x > 0) StringBuilder.appendChar!(sb, 'T') else StringBuilder.appendChar!(sb, 'F');
        MutList.foreach(f, v);
        StringBuilder.toString(sb) == "FT"

    @test
    def foreach04(): Bool & Impure =
        let v = MutList.range(0, 2);
        let sb = StringBuilder.new();
        let f = x -> if (x < 0) StringBuilder.appendChar!(sb, 'T') else StringBuilder.appendChar!(sb, 'F');
        MutList.foreach(f, v);
        StringBuilder.toString(sb) == "FF"

    /////////////////////////////////////////////////////////////////////////////
    // foreachWithIndex                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foreachWithIndex01(): Bool & Impure =
        let v1 = MutList.range(0, 100);
        let v2 = MutList.new();
        MutList.foreachWithIndex((x, i) -> if (x % 2 == 0 and i < 10) MutList.push!(x, v2) else (), v1);
        MutList.sameElements(v2, List.toMut(0 :: 2 :: 4 :: 6 :: 8 :: Nil))

    @test
    def foreachWithIndex02(): Bool & Impure =
        let v = MutList.range(1, 4);
        let sb = StringBuilder.new();
        let f = (x, i) -> if (x % 2 == 1 and i == 0) StringBuilder.appendChar!(sb, 'T') else StringBuilder.appendChar!(sb, 'F');
        MutList.foreachWithIndex(f, v);
        StringBuilder.toString(sb) == "TFF"

    @test
    def foreachWithIndex03(): Bool & Impure =
        let v = MutList.range(0, 5);
        let sb = StringBuilder.new();
        let f = (x, _) -> if (x % 2 == 0) StringBuilder.appendChar!(sb, 'T') else StringBuilder.appendChar!(sb, 'F');
        MutList.foreachWithIndex(f, v);
        StringBuilder.toString(sb) == "TFTFT"

    /////////////////////////////////////////////////////////////////////////////
    // compress!                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def compress!01(): Bool & Impure =
        let v = MutList.range(0, 100);
        List.foreach(_ -> let _ = MutList.pop!(v); (), List.range(0, 75));
        let c = MutList.capacity(v);
        let _ = MutList.pop!(v);
        c == 100 and MutList.capacity(v) == 50

    @test
    def compress!02(): Bool & Impure =
        let v = MutList.new();
        MutList.compress!(v);
        MutList.capacity(v) == 0

    @test
    def compress!03(): Bool & Impure =
        let v = MutList.new();
        MutList.push!(42, v);
        MutList.compress!(v);
        MutList.capacity(v) == 8

    @test
    def compress!04(): Bool & Impure =
        let v = MutList.range(0, 5);
        MutList.compress!(v);
        MutList.capacity(v) == 8

    @test
    def compress!05(): Bool & Impure =
        let v = MutList.range(0, 8);
        MutList.push!(99, v);
        MutList.remove!(0, v);
        MutList.remove!(0, v);
        MutList.remove!(0, v);
        MutList.remove!(0, v);
        MutList.remove!(0, v);
        let c = MutList.capacity(v);
        MutList.remove!(0, v);
        c == 16 and MutList.capacity(v) == 8

    @test
    def compress!06(): Bool & Impure =
        let v = MutList.range(0, 8);
        MutList.push!(7, v);
        let _ = MutList.pop!(v);
        let _ = MutList.pop!(v);
        let _ = MutList.pop!(v);
        let _ = MutList.pop!(v);
        let _ = MutList.pop!(v);
        let c = MutList.capacity(v);
        let _ = MutList.pop!(v);
        c == 16 and MutList.capacity(v) == 8

    /////////////////////////////////////////////////////////////////////////////
    // capacity                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def capacity01(): Bool & Impure =
        let v = MutList.new();
        MutList.capacity(v) == 8

    @test
    def capacity02(): Bool & Impure =
        let v = MutList.range(0, 9);
        MutList.capacity(v) == 9

    @test
    def capacity03(): Bool & Impure =
        let v = MutList.new();
        MutList.push!(0, v);
        MutList.push!(1, v);
        MutList.push!(2, v);
        MutList.push!(3, v);
        MutList.capacity(v) == 8

    @test
    def capacity04(): Bool & Impure =
        let v = MutList.new();
        MutList.push!(42, v);
        MutList.insert!(41, 0, v);
        MutList.insert!(40, 0, v);
        MutList.insert!(39, 0, v);
        MutList.capacity(v) == 8

    @test
    def capacity05(): Bool & Impure =
        let v = MutList.new();
        MutList.push!(99, v);
        MutList.capacity(v) == 8

    @test
    def capacity06(): Bool & Impure =
        let v = MutList.new();
        MutList.insert!(42, 0, v);
        MutList.capacity(v) == 8

    @test
    def capacity07(): Bool & Impure =
        let v = MutList.new();
        MutList.insert!(5, 0, v);
        MutList.insert!(4, 0, v);
        MutList.insert!(3, 0, v);
        MutList.insert!(2, 0, v);
        MutList.insert!(1, 0, v);
        MutList.capacity(v) == 8

    /////////////////////////////////////////////////////////////////////////////
    // toString                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toString01(): Bool & Impure =
        let v = MutList.new();
        MutList.push!(1, v);
        toString(v) == "MutList#{1}"

    @test
    def toString02(): Bool & Impure =
        let v = MutList.new();
        MutList.push!(1, v);
        MutList.push!(97, v);
        toString(v) == "MutList#{1, 97}"

    @test
    def toString03(): Bool & Impure =
        let v = MutList.new();
        MutList.push!(1, v);
        MutList.push!(97, v);
        MutList.push!(0, v);
        toString(v) == "MutList#{1, 97, 0}"

    @test
    def toString04(): Bool & Impure =
        let v = MutList.new();
        MutList.push!('a', v);
        MutList.push!('b', v);
        MutList.push!('c', v);
        MutList.push!('d', v);
        toString(v) == "MutList#{a, b, c, d}"

    /////////////////////////////////////////////////////////////////////////////
    // minimumBy                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def minimumBy01(): Bool & Impure = MutList.minimumBy((x, y) -> x <=> y, MutList.new(): MutList[Int32]) == None

    @test
    def minimumBy02(): Bool & Impure = MutList.minimumBy((x, y) -> x <=> y, MutList.range(0, 5)) == Some(0)

    @test
    def minimumBy03(): Bool & Impure = MutList.minimumBy((x, y) -> x <=> y, MutList.range(5, 10)) == Some(5)

    @test
    def minimumBy04(): Bool & Impure =
        let v = MutList.range(0, 6);
        MutList.push!(-2, v);
        MutList.minimumBy((x, y) -> x <=> y, v) == Some(-2)

    @test
    def minimumBy05(): Bool & Impure =
        let v = MutList.range(9, 19);
        let cmp = (x, y) -> if (x < y and x % 2 == 0) LessThan else GreaterThan;
        MutList.minimumBy(cmp, v) == Some(10)

    /////////////////////////////////////////////////////////////////////////////
    // maximumBy                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def maximumBy01(): Bool & Impure = MutList.maximumBy((x, y) -> x <=> y, MutList.new(): MutList[Int32]) == None

    @test
    def maximumBy02(): Bool & Impure = MutList.maximumBy((x, y) -> x <=> y, MutList.range(0, 5)) == Some(4)

    @test
    def maximumBy03(): Bool & Impure = MutList.maximumBy((x, y) -> x <=> y, MutList.range(5, 10)) == Some(9)

    @test
    def maximumBy04(): Bool & Impure =
        let v = MutList.range(0, 6);
        MutList.push!(11, v);
        MutList.maximumBy((x, y) -> x <=> y, v) == Some(11)

    @test
    def maximumBy05(): Bool & Impure =
        let v = MutList.range(9, 19);
        let cmp = (x, y) -> if (x > y and x % 2 == 0) GreaterThan else LessThan;
        MutList.maximumBy(cmp, v) == Some(18)

    /////////////////////////////////////////////////////////////////////////////
    // toArray                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toArray01(): Bool & Impure =
        Array.sameElements(MutList.toArray(Array.toMutList([1, 2, 3])), [1, 2, 3])

    @test
    def toArray02(): Bool & Impure =
        let v = Array.toMutList([1, 2, 3]);
        let a = MutList.toArray(v);
        a[1] = 42;
        MutList.get(1, v) == 2

    @test
    def toArray03(): Bool & Impure =
        let v = Array.toMutList([1, 2, 3]);
        MutList.pop!(v);
        MutList.pop!(v);
        Array.length(MutList.toArray(v)) == 1

/////////////////////////////////////////////////////////////////////////////
// The following tests for sort functions correspond to those in TestArray //
/////////////////////////////////////////////////////////////////////////////
// sortWith                                                                //
/////////////////////////////////////////////////////////////////////////////

def cmp(x: Int32, y: Int32): Comparison =
    if (x < y) LessThan
    else if (x == y) EqualTo
    else GreaterThan

@test
def sortWith01(): Bool & Impure =
    let a = MutList.sortWith(cmp, Array.toMutList([]: Array[Int32]));
    a == Array.toMutList([]: Array[Int32])

@test
def sortWith02(): Bool & Impure =
    let a = MutList.sortWith(cmp, Array.toMutList([0]));
    a == Array.toMutList([0])

@test
def sortWith03(): Bool & Impure =
    let a = MutList.sortWith(cmp, Array.toMutList([0,1]));
    a == Array.toMutList([0,1])

@test
def sortWith04(): Bool & Impure =
    let a = MutList.sortWith(cmp, Array.toMutList([1,0]));
    a == Array.toMutList([0,1])

@test
def sortWith05(): Bool & Impure =
    let a = MutList.sortWith(cmp, Array.toMutList([1,1]));
    a == Array.toMutList([1,1])

@test
def sortWith06(): Bool & Impure =
    let a = MutList.sortWith(cmp, Array.toMutList([0,1,2,3,4,5]));
    a == Array.toMutList([0,1,2,3,4,5])

@test
def sortWith07(): Bool & Impure =
    let a = MutList.sortWith(cmp, Array.toMutList([5,4,3,2,1,0]));
    a == Array.toMutList([0,1,2,3,4,5])

@test
def sortWith08(): Bool & Impure =
    let a = MutList.sortWith(cmp, Array.toMutList([5,3,0,4,1,2]));
    a == Array.toMutList([0,1,2,3,4,5])

@test
def sortWith09(): Bool & Impure =
    let a = MutList.sortWith(cmp, Array.toMutList([2,3,0,4,1,2]));
    a == Array.toMutList([0,1,2,2,3,4])

@test
def sortWith10(): Bool & Impure =
    let a = MutList.sortWith(flip(cmp), Array.toMutList([0,1,2,3,4,5]));
    a == Array.toMutList([5,4,3,2,1,0])

@test
def sortWith11(): Bool & Impure =
    let a = MutList.sortWith(flip(cmp), Array.toMutList([5,4,3,2,1,0]));
    a == Array.toMutList([5,4,3,2,1,0])

@test
def sortWith12(): Bool & Impure =
    let a = MutList.sortWith(flip(cmp), Array.toMutList([5,3,0,4,1,2]));
    a == Array.toMutList([5,4,3,2,1,0])

@test
def sortWith13(): Bool & Impure =
    let a = MutList.sortWith(flip(cmp), Array.toMutList([2,3,0,4,1,2]));
    a == Array.toMutList([4,3,2,2,1,0])

/////////////////////////////////////////////////////////////////////////////
// sort                                                                    //
/////////////////////////////////////////////////////////////////////////////

def testSortVsSortWith(a: MutList[Int32]) : Bool & Impure =
    MutList.sort(a) == MutList.sortWith(cmp, a)

@test
def sort01(): Bool & Impure = testSortVsSortWith(Array.toMutList([]: Array[Int32]))

@test
def sort02(): Bool & Impure = testSortVsSortWith(Array.toMutList([0]))

@test
def sort03(): Bool & Impure = testSortVsSortWith(Array.toMutList([0,1]))

@test
def sort04(): Bool & Impure = testSortVsSortWith(Array.toMutList([1,0]))

@test
def sort05(): Bool & Impure = testSortVsSortWith(Array.toMutList([1,1]))

@test
def sort06(): Bool & Impure = testSortVsSortWith(Array.toMutList([0,1,2,3,4,5]))

@test
def sort07(): Bool & Impure = testSortVsSortWith(Array.toMutList([5,4,3,2,1,0]))

@test
def sort08(): Bool & Impure = testSortVsSortWith(Array.toMutList([5,3,0,4,1,2]))

@test
def sort09(): Bool & Impure = testSortVsSortWith(Array.toMutList([2,3,0,4,1,2]))

/////////////////////////////////////////////////////////////////////////////
// sortWith!                                                               //
/////////////////////////////////////////////////////////////////////////////

@test
def sortWith01!(): Bool & Impure =
    let a = Array.toMutList([]: Array[Int32]);
    MutList.sortWith(cmp, a);
    a == Array.toMutList([]: Array[Int32])

@test
def sortWith02!(): Bool & Impure =
    let a = Array.toMutList([0]);
    MutList.sortWith!(cmp, a);
    a == Array.toMutList([0])

@test
def sortWith03!(): Bool & Impure =
    let a = Array.toMutList([0,1]);
    MutList.sortWith!(cmp, a);
    a == Array.toMutList([0,1])

@test
def sortWith04!(): Bool & Impure =
    let a = Array.toMutList([1,0]);
    MutList.sortWith!(cmp, a);
    a == Array.toMutList([0,1])

@test
def sortWith05!(): Bool & Impure =
    let a = Array.toMutList([1,1]);
    MutList.sortWith!(cmp, a);
    a == Array.toMutList([1,1])

@test
def sortWith06!(): Bool & Impure =
    let a = Array.toMutList([0,1,2,3,4,5]);
    MutList.sortWith!(cmp, a);
    a == Array.toMutList([0,1,2,3,4,5])

@test
def sortWith07!(): Bool & Impure =
    let a = Array.toMutList([5,4,3,2,1,0]);
    MutList.sortWith!(cmp, a);
    a == Array.toMutList([0,1,2,3,4,5])

@test
def sortWith08!(): Bool & Impure =
    let a = Array.toMutList([5,3,0,4,1,2]);
    MutList.sortWith!(cmp, a);
    a == Array.toMutList([0,1,2,3,4,5])

@test
def sortWith09!(): Bool & Impure =
    let a = Array.toMutList([2,3,0,4,1,2]);
    MutList.sortWith!(cmp, a);
    a == Array.toMutList([0,1,2,2,3,4])

@test
def sortWith10!(): Bool & Impure =
    let a = Array.toMutList([0,1,2,3,4,5]);
    MutList.sortWith!(flip(cmp), a);
    a == Array.toMutList([5,4,3,2,1,0])

@test
def sortWith11!(): Bool & Impure =
    let a = Array.toMutList([5,4,3,2,1,0]);
    MutList.sortWith!(flip(cmp), a);
    a == Array.toMutList([5,4,3,2,1,0])

@test
def sortWith12!(): Bool & Impure =
    let a = Array.toMutList([5,3,0,4,1,2]);
    MutList.sortWith!(flip(cmp), a);
    a == Array.toMutList([5,4,3,2,1,0])

@test
def sortWith13!(): Bool & Impure =
    let a = Array.toMutList([2,3,0,4,1,2]);
    MutList.sortWith!(flip(cmp), a);
    a == Array.toMutList([4,3,2,2,1,0])

/////////////////////////////////////////////////////////////////////////////
// sort!                                                                   //
/////////////////////////////////////////////////////////////////////////////

def testSort!VsSortWith!(a: MutList[Int32]) : Bool & Impure =
    let b = MutList.copy(a);
    let c = MutList.copy(a);
    MutList.sort!(b);
    MutList.sortWith!(cmp, c);
    b == c

@test
def sort!01(): Bool & Impure = testSort!VsSortWith!(Array.toMutList([]: Array[Int32]))

@test
def sort!02(): Bool & Impure = testSort!VsSortWith!(Array.toMutList([0]))

@test
def sort!03(): Bool & Impure = testSort!VsSortWith!(Array.toMutList([0,1]))

@test
def sort!04(): Bool & Impure = testSort!VsSortWith!(Array.toMutList([1,0]))

@test
def sort!05(): Bool & Impure = testSort!VsSortWith!(Array.toMutList([1,1]))

@test
def sort!06(): Bool & Impure = testSort!VsSortWith!(Array.toMutList([0,1,2,3,4,5]))

@test
def sort!07(): Bool & Impure = testSort!VsSortWith!(Array.toMutList([5,4,3,2,1,0]))

@test
def sort!08(): Bool & Impure = testSort!VsSortWith!(Array.toMutList([5,3,0,4,1,2]))

@test
def sort!09(): Bool & Impure = testSort!VsSortWith!(Array.toMutList([2,3,0,4,1,2]))

/////////////////////////////////////////////////////////////////////////////
// sortBy                                                                  //
/////////////////////////////////////////////////////////////////////////////

def testSortByVsSort(a: MutList[Int32]) : Bool & Impure =
    (MutList.sortBy(identity, a) == MutList.sort(a)) and
    (MutList.sortBy(x -> 4*x+7, a) == MutList.sort(a)) and
    (MutList.sortBy(x -> -x, a) == MutList.sortWith(flip(cmp),a))

@test
def sortBy01(): Bool & Impure = testSortByVsSort(Array.toMutList([]: Array[Int32]))

@test
def sortBy02(): Bool & Impure = testSortByVsSort(Array.toMutList([0]))

@test
def sortBy03(): Bool & Impure = testSortByVsSort(Array.toMutList([0,1]))

@test
def sortBy04(): Bool & Impure = testSortByVsSort(Array.toMutList([1,0]))

@test
def sortBy05(): Bool & Impure = testSortByVsSort(Array.toMutList([1,1]))

@test
def sortBy06(): Bool & Impure = testSortByVsSort(Array.toMutList([0,1,2,3,4,5]))

@test
def sortBy07(): Bool & Impure = testSortByVsSort(Array.toMutList([5,4,3,2,1,0]))

@test
def sortBy08(): Bool & Impure = testSortByVsSort(Array.toMutList([5,3,0,4,1,2]))

@test
def sortBy09(): Bool & Impure = testSortByVsSort(Array.toMutList([2,3,0,4,1,2]))

enum R {
    case R({i: Int32, s: String})
}

instance Eq[R] {
    pub def eq(a: R, b: R): Bool =
        let R(x) = a;
        let R(y) = b;
        x.i == y.i and x.s == y.s
}

@test
def sortBy10(): Bool & Impure =
    MutList.sortBy(r -> let R(x) = r; x.i, Array.toMutList([R({i = 2, s = "A"}), R({i = 1, s = "B"}), R({i = 3, s = "C"})]))
    == Array.toMutList([R({i = 1, s = "B"}), R({i = 2, s = "A"}), R({i = 3, s = "C"})])

/////////////////////////////////////////////////////////////////////////////
// sortBy!                                                                 //
/////////////////////////////////////////////////////////////////////////////

def testSortBy!VsSortBy(a: MutList[Int32]) : Bool & Impure =
    let b = MutList.copy(a);
    let c = MutList.copy(a);
    MutList.sortBy!(identity, b);
    MutList.sortBy!(x -> 4*x+7, c);
    (b == MutList.sortBy(x -> 4*x+7, a)) and
    (c == MutList.sortBy(identity, a))

@test
def sortBy!01(): Bool & Impure = testSortBy!VsSortBy(Array.toMutList([]: Array[Int32]))

@test
def sortBy!02(): Bool & Impure = testSortBy!VsSortBy(Array.toMutList([0]))

@test
def sortBy!03(): Bool & Impure = testSortBy!VsSortBy(Array.toMutList([0,1]))

@test
def sortBy!04(): Bool & Impure = testSortBy!VsSortBy(Array.toMutList([1,0]))

@test
def sortBy!05(): Bool & Impure = testSortBy!VsSortBy(Array.toMutList([1,1]))

@test
def sortBy!06(): Bool & Impure = testSortBy!VsSortBy(Array.toMutList([0,1,2,3,4,5]))

@test
def sortBy!07(): Bool & Impure = testSortBy!VsSortBy(Array.toMutList([5,4,3,2,1,0]))

@test
def sortBy!08(): Bool & Impure = testSortBy!VsSortBy(Array.toMutList([5,3,0,4,1,2]))

@test
def sortBy!09(): Bool & Impure = testSortBy!VsSortBy(Array.toMutList([2,3,0,4,1,2]))
}

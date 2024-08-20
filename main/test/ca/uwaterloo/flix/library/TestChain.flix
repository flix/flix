/*
 * Copyright 2021 Stephen Tetley
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

mod TestChain {

    /////////////////////////////////////////////////////////////////////////////
    // Collectable.collect                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def collect01(): Bool = region rc {
        Collectable.collect((Iterator.empty(rc): Iterator[Int32, rc, rc])) == Chain.empty()
    }

    @test
    def collect02(): Bool = region rc {
        Collectable.collect(Iterator.singleton(rc, 1)) == List.toChain(1 :: Nil)
    }

    @test
    def collect03(): Bool = region rc {
        Collectable.collect(Iterator.range(rc, 1, 3)) == List.toChain(1 :: 2 :: Nil)
    }

    /////////////////////////////////////////////////////////////////////////////
    // empty                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def empty01(): Bool =
        let c: Chain[Int32] = Chain.empty();
        Chain.toList(c) == Nil

    /////////////////////////////////////////////////////////////////////////////
    // singleton                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def singleton01(): Bool =
        let c = Chain.singleton(1);
        Chain.toList(c) == 1 :: Nil

    @test
    def singleton02(): Bool =
        let c: Chain[Option[Int32]] = Chain.singleton(None);
        Chain.toList(c) == None :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // ap                                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def ap01(): Bool = Chain.ap(Chain.empty(), Chain.empty()) == (Chain.empty() : Chain[Int32])

    @test
    def ap02(): Bool = Chain.ap(Chain.singleton(x -> x+1), Chain.empty()) == Chain.empty()

    @test
    def ap03(): Bool = Chain.ap(Chain.empty(), Chain.singleton(5)) == (Chain.empty() : Chain[Int32])

    @test
    def ap04(): Bool = Chain.ap(Chain.singleton(x -> x+1), Chain.singleton(5)) == Chain.singleton(6)

    @test
    def ap05(): Bool = Chain.ap(Chain.singleton(x -> x+1), List.toChain(0 :: 5 :: Nil)) == List.toChain(1 :: 6 :: Nil)

    @test
    def ap06(): Bool = Chain.ap(List.toChain((x -> x+1) :: (x -> x*2) :: Nil), List.toChain(0 :: 4 :: Nil)) == List.toChain(1 :: 5 :: 0 :: 8 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // isEmpty                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def isEmpty01(): Bool = Chain.isEmpty(Chain.empty()) == true

    @test
    def isEmpty02(): Bool = Chain.isEmpty(Chain.singleton(1)) == false

    @test
    def isEmpty03(): Bool = Chain.isEmpty(List.toChain(1 :: 2 :: Nil)) == false

    /////////////////////////////////////////////////////////////////////////////
    // nonEmpty                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def nonEmpty01(): Bool = Chain.nonEmpty(Chain.empty()) == false

    @test
    def nonEmpty02(): Bool = Chain.nonEmpty(Chain.singleton(1)) == true

    @test
    def nonEmpty03(): Bool = Chain.nonEmpty(List.toChain(1 :: 2 :: Nil)) == true

    /////////////////////////////////////////////////////////////////////////////
    // cons                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def cons01(): Bool = Chain.cons(1, Chain.empty()) == Chain.singleton(1)

    @test
    def cons02(): Bool = Chain.cons(1, Chain.singleton(2)) == List.toChain(1 :: 2 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // snoc                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def snoc01(): Bool = Chain.snoc(Chain.empty(), 1) == Chain.singleton(1)

    @test
    def snoc02(): Bool = Chain.snoc(Chain.singleton(1), 2) == List.toChain(1 :: 2 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // head                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def head01(): Bool = Chain.head((Chain.empty(): Chain[Unit])) == None

    @test
    def head02(): Bool = Chain.head(Chain.singleton(1)) == Some(1)

    @test
    def head03(): Bool = Chain.head(List.toChain(2 :: 1 :: Nil)) == Some(2)

    @test
    def head04(): Bool = Chain.head(List.toChain(3 :: 2 :: 1 :: Nil)) == Some(3)

    /////////////////////////////////////////////////////////////////////////////
    // last                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def last01(): Bool = Chain.last((Chain.empty(): Chain[Unit])) == None

    @test
    def last02(): Bool = Chain.last(Chain.singleton(1)) == Some(1)

    @test
    def last03(): Bool = Chain.last(List.toChain(1 :: 2 :: Nil)) == Some(2)

    @test
    def last04(): Bool = Chain.last(List.toChain(1 :: 2 :: 3 :: Nil)) == Some(3)

    /////////////////////////////////////////////////////////////////////////////
    // init                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def init01(): Bool = Chain.init((Chain.empty(): Chain[Unit])) == None

    @test
    def init02(): Bool = Chain.init(Chain.singleton(0)) == Some(Chain.empty())

    @test
    def init03(): Bool = Chain.init(List.toChain(0 :: 1 :: Nil)) == Some(Chain.singleton(0))

    @test
    def init04(): Bool = Chain.init(List.toChain(2 :: 1 :: 0 :: Nil)) == Some(List.toChain(2 :: 1 :: Nil))

    @test
    def init05(): Bool = Chain.init(List.toChain('a' :: 'b' :: 'c' :: 'd' :: Nil)) == Some(List.toChain('a' :: 'b' :: 'c' :: Nil))

    @test
    def init06(): Bool = Chain.init(List.toChain("test1" :: "test2" :: "test3" :: Nil)) == Some(List.toChain("test1" :: "test2" :: Nil))

    @test
    def init07(): Bool = Chain.init(List.toChain(2 :: 1 :: 0 :: -1 :: Nil)) == Some(List.toChain(2 :: 1 :: 0 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // length                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def length01(): Bool = Chain.length(Chain.empty()) == 0

    @test
    def length02(): Bool = Chain.length(Chain.singleton(1)) == 1

    @test
    def length03(): Bool = Chain.length(List.toChain(1 :: 2 :: Nil)) == 2

    @test
    def length04(): Bool = Chain.length(List.toChain(1 :: 2 :: 3 :: Nil)) == 3

    /////////////////////////////////////////////////////////////////////////////
    // append                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def append01(): Bool = Chain.append((Chain.empty(): Chain[Unit]), (Chain.empty(): Chain[Unit])) == Chain.empty()

    @test
    def append02(): Bool = Chain.append(Chain.empty(), Chain.singleton(1)) == Chain.singleton(1)

    @test
    def append03(): Bool = Chain.append(Chain.empty(), List.toChain(1 :: 2 :: Nil)) == List.toChain(1 :: 2 :: Nil)

    @test
    def append04(): Bool = Chain.append(Chain.singleton(1), Chain.empty()) == Chain.singleton(1)

    @test
    def append05(): Bool = Chain.append(List.toChain(1 :: 2 :: Nil), Chain.empty()) == List.toChain(1 :: 2 :: Nil)

    @test
    def append06(): Bool = Chain.append(Chain.singleton(1), Chain.singleton(2)) == List.toChain(1 :: 2 :: Nil)

    @test
    def append07(): Bool = Chain.append(List.toChain(1 :: 2 :: Nil), Chain.singleton(3)) == List.toChain(1 :: 2 :: 3 :: Nil)

    @test
    def append08(): Bool = Chain.append(Chain.singleton(1), List.toChain(2 :: 3 :: Nil)) == List.toChain(1 :: 2 :: 3 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // viewLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def viewLeft01(): Bool = Chain.viewLeft((Chain.empty(): Chain[Int32])) == Chain.ViewLeft.NoneLeft

    @test
    def viewLeft02(): Bool = Chain.viewLeft(Chain.singleton(1)) == Chain.ViewLeft.SomeLeft(1, Chain.empty())

    @test
    def viewLeft03(): Bool = Chain.viewLeft(List.toChain(1 :: 2 :: Nil)) == Chain.ViewLeft.SomeLeft(1, Chain.singleton(2))

    @test
    def viewLeft04(): Bool = Chain.viewLeft(List.toChain(1 :: 2 :: 3 :: Nil)) == Chain.ViewLeft.SomeLeft(1, List.toChain(2 :: 3 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // viewRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def viewRight01(): Bool = Chain.viewRight((Chain.empty(): Chain[Int32])) == Chain.ViewRight.NoneRight

    @test
    def viewRight02(): Bool = Chain.viewRight(Chain.singleton(1)) == Chain.ViewRight.SomeRight(Chain.empty(), 1)

    @test
    def viewRight03(): Bool = Chain.viewRight(List.toChain(1 :: 2 :: Nil)) == Chain.ViewRight.SomeRight(Chain.singleton(1), 2)

    @test
    def viewRight04(): Bool = Chain.viewRight(List.toChain(1 :: 2 :: 3 :: Nil)) == Chain.ViewRight.SomeRight(List.toChain(1 :: 2 :: Nil), 3)

    /////////////////////////////////////////////////////////////////////////////
    // memberOf                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def memberOf01(): Bool = Chain.memberOf(0, Chain.empty()) == false

    @test
    def memberOf02(): Bool = Chain.memberOf(0, Chain.singleton(1)) == false

    @test
    def memberOf03(): Bool = Chain.memberOf(0, Chain.singleton(0)) == true

    @test
    def memberOf04(): Bool = Chain.memberOf(0, List.toChain(1 :: 2 :: Nil)) == false

    @test
    def memberOf05(): Bool = Chain.memberOf(1, List.toChain(1 :: 2 :: Nil)) == true

    @test
    def memberOf06(): Bool = Chain.memberOf(2, List.toChain(1 :: 2 :: Nil)) == true

    @test
    def memberOf07(): Bool = Chain.memberOf(3, List.toChain(1 :: 2 :: Nil)) == false

    /////////////////////////////////////////////////////////////////////////////
    // indexOf                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def indexOf01(): Bool = Chain.indexOf(0, Chain.empty()) == None

    @test
    def indexOf02(): Bool = Chain.indexOf(0, Chain.singleton(1)) == None

    @test
    def indexOf03(): Bool = Chain.indexOf(1, Chain.singleton(1)) == Some(0)

    @test
    def indexOf04(): Bool = Chain.indexOf(0, List.toChain(1 :: 2 :: Nil)) == None

    @test
    def indexOf05(): Bool = Chain.indexOf(1, List.toChain(1 :: 2 :: Nil)) == Some(0)

    @test
    def indexOf06(): Bool = Chain.indexOf(2, List.toChain(1 :: 2 :: Nil)) == Some(1)

    /////////////////////////////////////////////////////////////////////////////
    // find                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def find01(): Bool = Chain.find(i -> i > 2, Chain.empty()) == None

    @test
    def find02(): Bool = Chain.find(i -> i > 2, Chain.singleton(1)) == None

    @test
    def find03(): Bool = Chain.find(i -> i > 2, Chain.singleton(3)) == Some(3)

    @test
    def find04(): Bool = Chain.find(i -> i > 2, List.toChain(1 :: 2 :: Nil)) == None

    @test
    def find05(): Bool = Chain.find(i -> i > 2, List.toChain(6 :: -6 :: Nil)) == Some(6)

    @test
    def find06(): Bool = Chain.find(i -> i > 2, List.toChain(-6 :: 6 :: Nil)) == Some(6)

    @test
    def find07(): Bool = Chain.find(i -> i > 2, List.toChain(6 :: 7 :: Nil)) == Some(6)

    /////////////////////////////////////////////////////////////////////////////
    // findLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def findLeft01(): Bool = Chain.findLeft(i -> i > 2, Chain.empty()) == None

    @test
    def findLeft02(): Bool = Chain.findLeft(i -> i > 2, Chain.singleton(1)) == None

    @test
    def findLeft03(): Bool = Chain.findLeft(i -> i > 2, Chain.singleton(3)) == Some(3)

    @test
    def findLeft04(): Bool = Chain.findLeft(i -> i > 2, List.toChain(1 :: 2 :: Nil)) == None

    @test
    def findLeft05(): Bool = Chain.findLeft(i -> i > 2, List.toChain(6 :: -6 :: Nil)) == Some(6)

    @test
    def findLeft06(): Bool = Chain.findLeft(i -> i > 2, List.toChain(-6 :: 6 :: Nil)) == Some(6)

    @test
    def findLeft07(): Bool = Chain.findLeft(i -> i > 2, List.toChain(6 :: 7 :: Nil)) == Some(6)

    /////////////////////////////////////////////////////////////////////////////
    // findRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def findRight01(): Bool = Chain.findRight(i -> i > 2, Chain.empty()) == None

    @test
    def findRight02(): Bool = Chain.findRight(i -> i > 2, Chain.singleton(1)) == None

    @test
    def findRight03(): Bool = Chain.findRight(i -> i > 2, Chain.singleton(3)) == Some(3)

    @test
    def findRight04(): Bool = Chain.findRight(i -> i > 2, List.toChain(1 :: 2 :: Nil)) == None

    @test
    def findRight05(): Bool = Chain.findRight(i -> i > 2, List.toChain(6 :: -6 :: Nil)) == Some(6)

    @test
    def findRight06(): Bool = Chain.findRight(i -> i > 2, List.toChain(-6 :: 6 :: Nil)) == Some(6)

    @test
    def findRight07(): Bool = Chain.findRight(i -> i > 2, List.toChain(6 :: 7 :: Nil)) == Some(7)

    /////////////////////////////////////////////////////////////////////////////
    // range                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def range01(): Bool = Chain.range(1, 0) == Chain.empty()

    @test
    def range02(): Bool = Chain.range(1, 1) == Chain.empty()

    @test
    def range03(): Bool = Chain.range(1, 2) == Chain.singleton(1)

    @test
    def range04(): Bool = Chain.range(1, 3) == List.toChain(1 :: 2 :: Nil)

    @test
    def range05(): Bool = Chain.range(1, 4) == List.toChain(1 :: 2 :: 3 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // repeat                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def repeat01(): Bool = Chain.repeat(-1, 1) == Chain.empty()

    @test
    def repeat02(): Bool = Chain.repeat(0, 1) == Chain.empty()

    @test
    def repeat03(): Bool = Chain.repeat(1, 1) == Chain.singleton(1)

    @test
    def repeat04(): Bool = Chain.repeat(2, 1) == List.toChain(1 :: 1 :: Nil)

    @test
    def repeat05(): Bool = Chain.repeat(3, 1) == List.toChain(1 :: 1 :: 1 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // scan                                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def scan01(): Bool = Chain.scan((i, b) -> if (b) i+1 else i+2, 1, Chain.empty()) == Chain.singleton(1)

    @test
    def scan02(): Bool = Chain.scan((i, b) -> if (b) i+1 else i+2, 1, Chain.singleton(false)) == List.toChain(1 :: 3 :: Nil)

    @test
    def scan03(): Bool = Chain.scan((i, b) -> if (b) i+1 else i+2, 1, Chain.singleton(true)) == List.toChain(1 :: 2 :: Nil)

    @test
    def scan04(): Bool = Chain.scan((i, b) -> if (b) i+1 else i+2, 1, List.toChain(false :: false :: Nil)) == List.toChain(1 :: 3 :: 5 :: Nil)

    @test
    def scan05(): Bool = Chain.scan((i, b) -> if (b) i+1 else i+2, 1, List.toChain(false :: true :: Nil)) == List.toChain(1 :: 3 :: 4 :: Nil)

    @test
    def scan06(): Bool = Chain.scan((i, b) -> if (b) i+1 else i+2, 1, List.toChain(true :: false :: Nil)) == List.toChain(1 :: 2 :: 4 :: Nil)

    @test
    def scan07(): Bool = Chain.scan((i, b) -> if (b) i+1 else i+2, 1, List.toChain(true :: true :: Nil)) == List.toChain(1 :: 2 :: 3 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // scanLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def scanLeft01(): Bool = Chain.scanLeft((i, b) -> if (b) i+1 else i+2, 1, Chain.empty()) == Chain.singleton(1)

    @test
    def scanLeft02(): Bool = Chain.scanLeft((i, b) -> if (b) i+1 else i+2, 1, Chain.singleton(false)) == List.toChain(1 :: 3 :: Nil)

    @test
    def scanLeft03(): Bool = Chain.scanLeft((i, b) -> if (b) i+1 else i+2, 1, Chain.singleton(true)) == List.toChain(1 :: 2 :: Nil)

    @test
    def scanLeft04(): Bool = Chain.scanLeft((i, b) -> if (b) i+1 else i+2, 1, List.toChain(false :: false :: Nil)) == List.toChain(1 :: 3 :: 5 :: Nil)

    @test
    def scanLeft05(): Bool = Chain.scanLeft((i, b) -> if (b) i+1 else i+2, 1, List.toChain(false :: true :: Nil)) == List.toChain(1 :: 3 :: 4 :: Nil)

    @test
    def scanLeft06(): Bool = Chain.scanLeft((i, b) -> if (b) i+1 else i+2, 1, List.toChain(true :: false :: Nil)) == List.toChain(1 :: 2 :: 4 :: Nil)

    @test
    def scanLeft07(): Bool = Chain.scanLeft((i, b) -> if (b) i+1 else i+2, 1, List.toChain(true :: true :: Nil)) == List.toChain(1 :: 2 :: 3 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // scanRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def scanRight01(): Bool = Chain.scanRight((b, i) -> if (b) i+1 else i+2, 1, Chain.empty()) == Chain.singleton(1)

    @test
    def scanRight02(): Bool = Chain.scanRight((b, i) -> if (b) i+1 else i+2, 1, Chain.singleton(false)) == List.toChain(3 :: 1 :: Nil)

    @test
    def scanRight03(): Bool = Chain.scanRight((b, i) -> if (b) i+1 else i+2, 1, Chain.singleton(true)) == List.toChain(2 :: 1 :: Nil)

    @test
    def scanRight04(): Bool = Chain.scanRight((b, i) -> if (b) i+1 else i+2, 1, List.toChain(false :: false :: Nil)) == List.toChain(5 :: 3 :: 1 :: Nil)

    @test
    def scanRight05(): Bool = Chain.scanRight((b, i) -> if (b) i+1 else i+2, 1, List.toChain(false :: true :: Nil)) == List.toChain(4 :: 2 :: 1 :: Nil)

    @test
    def scanRight06(): Bool = Chain.scanRight((b, i) -> if (b) i+1 else i+2, 1, List.toChain(true :: false :: Nil)) == List.toChain(4 :: 3 :: 1 :: Nil)

    @test
    def scanRight07(): Bool = Chain.scanRight((b, i) -> if (b) i+1 else i+2, 1, List.toChain(true :: true :: Nil)) == List.toChain(3 :: 2 :: 1 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // map                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def map01(): Bool = Chain.map(i -> i > 2, Chain.empty()) == Chain.empty()

    @test
    def map02(): Bool = Chain.map(i -> i > 2, Chain.singleton(1)) == Chain.singleton(false)

    @test
    def map03(): Bool = Chain.map(i -> i > 2, Chain.singleton(3)) == Chain.singleton(true)

    @test
    def map04(): Bool = Chain.map(i -> i > 2, List.toChain(1 :: 2 :: Nil)) == List.toChain(false :: false :: Nil)

    @test
    def map05(): Bool = Chain.map(i -> i > 2, List.toChain(1 :: 8 :: Nil)) == List.toChain(false :: true :: Nil)

    @test
    def map06(): Bool = Chain.map(i -> i > 2, List.toChain(8 :: 1 :: Nil)) == List.toChain(true :: false :: Nil)

    @test
    def map07(): Bool = Chain.map(i -> i > 2, List.toChain(7 :: 8 :: Nil)) == List.toChain(true :: true :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // mapWithIndex                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapWithIndex01(): Bool = Chain.mapWithIndex((i, e) -> if (i < 1) e > 2 else e <= 2, Chain.empty()) == Chain.empty()

    @test
    def mapWithIndex02(): Bool = Chain.mapWithIndex((i, e) -> if (i < 1) e > 2 else e <= 2, Chain.singleton(1)) == Chain.singleton(false)

    @test
    def mapWithIndex03(): Bool = Chain.mapWithIndex((i, e) -> if (i < 1) e > 2 else e <= 2, Chain.singleton(3)) == Chain.singleton(true)

    @test
    def mapWithIndex04(): Bool = Chain.mapWithIndex((i, e) -> if (i < 1) e > 2 else e <= 2, List.toChain(1 :: 2 :: Nil)) == List.toChain(false :: true :: Nil)

    @test
    def mapWithIndex05(): Bool = Chain.mapWithIndex((i, e) -> if (i < 1) e > 2 else e <= 2, List.toChain(1 :: 8 :: Nil)) == List.toChain(false :: false :: Nil)

    @test
    def mapWithIndex06(): Bool = Chain.mapWithIndex((i, e) -> if (i < 1) e > 2 else e <= 2, List.toChain(8 :: 1 :: Nil)) == List.toChain(true :: true :: Nil)

    @test
    def mapWithIndex07(): Bool = Chain.mapWithIndex((i, e) -> if (i < 1) e > 2 else e <= 2, List.toChain(7 :: 8 :: Nil)) == List.toChain(true :: false :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // flatMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def flatMap01(): Bool = Chain.flatMap(i -> Chain.repeat(i, i), Chain.empty()) == Chain.empty()

    @test
    def flatMap02(): Bool = Chain.flatMap(i -> Chain.repeat(i, i), Chain.singleton(0)) == Chain.empty()

    @test
    def flatMap03(): Bool = Chain.flatMap(i -> Chain.repeat(i, i), Chain.singleton(1)) == Chain.singleton(1)

    @test
    def flatMap04(): Bool = Chain.flatMap(i -> Chain.repeat(i, i), Chain.singleton(2)) == List.toChain(2 :: 2 :: Nil)

    @test
    def flatMap05(): Bool = Chain.flatMap(i -> Chain.repeat(i, i), List.toChain(1 :: 2 :: Nil)) == List.toChain(1 :: 2 :: 2 :: Nil)

    @test
    def flatMap06(): Bool = Chain.flatMap(i -> Chain.repeat(i, i), List.toChain(2 :: 3 :: Nil)) == List.toChain(2 :: 2 :: 3 :: 3 :: 3 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // reverse                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reverse01(): Bool = Chain.reverse((Chain.empty(): Chain[Unit])) == Chain.empty()

    @test
    def reverse02(): Bool = Chain.reverse(Chain.singleton(1)) == Chain.singleton(1)

    @test
    def reverse03(): Bool = Chain.reverse(List.toChain(1 :: 2 :: Nil)) == List.toChain(2 :: 1 :: Nil)

    @test
    def reverse04(): Bool = Chain.reverse(List.toChain(1 :: 1 :: Nil)) == List.toChain(1 :: 1 :: Nil)

    @test
    def reverse05(): Bool = Chain.reverse(List.toChain(1 :: 2 :: 3 :: Nil)) == List.toChain(3 :: 2 :: 1 :: Nil)

    @test
    def reverse06(): Bool = Chain.reverse(List.toChain(1 :: 2 :: 3 :: 4 :: Nil)) == List.toChain(4 :: 3 :: 2 :: 1 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // intersperse                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def intersperse01(): Bool = Chain.intersperse(11, Chain.empty()) == Chain.empty()

    @test
    def intersperse02(): Bool = Chain.intersperse(11, Chain.singleton(1)) == Chain.singleton(1)

    @test
    def intersperse03(): Bool = Chain.intersperse(11, List.toChain(1 :: 2 :: Nil)) == List.toChain(1 :: 11 :: 2 :: Nil)

    @test
    def intersperse04(): Bool = Chain.intersperse(11, List.toChain(1 :: 2 :: 3 :: Nil)) == List.toChain(1 :: 11 :: 2 :: 11 :: 3 :: Nil)

    @test
    def intersperse05(): Bool = Chain.intersperse(11, List.toChain(1 :: 2 :: 3 :: 4 :: Nil)) == List.toChain(1 :: 11 :: 2 :: 11 :: 3 :: 11 :: 4 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // count                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def count01(): Bool = Chain.count(i -> i > 3, Chain.empty()) == 0

    @test
    def count02(): Bool = Chain.count(i -> i > 3, Chain.singleton(1)) == 0

    @test
    def count03(): Bool = Chain.count(i -> i > 3, Chain.singleton(4)) == 1

    @test
    def count04(): Bool = Chain.count(i -> i > 3, List.toChain(1 :: 2 :: Nil)) == 0

    @test
    def count05(): Bool = Chain.count(i -> i > 3, List.toChain(1 :: 8 :: Nil)) == 1

    @test
    def count06(): Bool = Chain.count(i -> i > 3, List.toChain(8 :: 1 :: Nil)) == 1

    @test
    def count07(): Bool = Chain.count(i -> i > 3, List.toChain(6 :: 7 :: Nil)) == 2


    /////////////////////////////////////////////////////////////////////////////
    // sum                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sum01(): Bool =
        List.toChain(Nil) |> Chain.sum == 0

    @test
    def sum02(): Bool =
        List.toChain(1 :: Nil) |> Chain.sum == 1

    @test
    def sum03(): Bool =
        List.toChain(1 :: 2 :: 3 :: Nil) |> Chain.sum == 6

    @test
    def sum04(): Bool =
        List.toChain(1 :: 2 :: 3 :: -3 :: Nil) |> Chain.sum == 3

    @test
    def sum05(): Bool =
        List.toChain(-1 :: -2 :: -3 :: -4 :: Nil) |> Chain.sum == -10

    @test
    def sum06(): Bool =
        List.toChain(10 :: -10 :: Nil) |> Chain.sum == 0

    @test
    def sum07(): Bool =
        List.range(1, 101) |> List.toChain |> Chain.sum == 5050


    /////////////////////////////////////////////////////////////////////////////
    // sumWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sumWith01(): Bool =
        List.toChain(Nil) |> Chain.sumWith(x -> x + 1) == 0

    @test
    def sumWith02(): Bool =
        List.toChain(1 :: Nil) |> Chain.sumWith(x -> x + 1) == 2

    @test
    def sumWith03(): Bool =
        List.toChain(1 :: 2 :: 3 :: Nil) |> Chain.sumWith(x -> x + 1) == 9

    @test
    def sumWith04(): Bool =
        List.toChain(1 :: 2 :: 3 :: -3 :: Nil) |> Chain.sumWith(x -> x + 1) == 7

    @test
    def sumWith05(): Bool =
        List.toChain(-1 :: -2 :: -3 :: -4 :: Nil) |> Chain.sumWith(x -> x + 1) == -6

    @test
    def sumWith06(): Bool =
        List.toChain(10 :: -10 :: Nil) |> Chain.sumWith(x -> x + 1) == 2


    /////////////////////////////////////////////////////////////////////////////
    // flatten                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def flatten01(): Bool = Chain.flatten((Chain.empty(): Chain[Chain[Unit]])) == Chain.empty()

    @test
    def flatten02(): Bool =( Chain.flatten(Chain.singleton(Chain.empty())): Chain[Unit]) == Chain.empty()

    @test
    def flatten03(): Bool = Chain.flatten(Chain.singleton(Chain.singleton(1))) == Chain.singleton(1)

    @test
    def flatten04(): Bool = Chain.flatten(Chain.singleton(List.toChain(1 :: 2 :: Nil))) == List.toChain(1 :: 2 :: Nil)

    @test
    def flatten05(): Bool = (Chain.flatten(List.toChain(Chain.empty() :: Chain.empty() :: Nil)): Chain[Unit]) == Chain.empty()

    @test
    def flatten06(): Bool = Chain.flatten(List.toChain(Chain.singleton(1) :: Chain.empty() :: Nil)) == Chain.singleton(1)

    @test
    def flatten07(): Bool = Chain.flatten(List.toChain(Chain.empty() :: Chain.singleton(1) :: Nil)) == Chain.singleton(1)

    @test
    def flatten08(): Bool = Chain.flatten(List.toChain(Chain.singleton(1) :: Chain.singleton(2) :: Nil)) == List.toChain(1 :: 2 :: Nil)

    @test
    def flatten09(): Bool = Chain.flatten(List.toChain(List.toChain(1 :: 2 :: Nil) :: List.toChain(3 :: 4 :: 5 :: Nil) :: Nil)) == List.toChain(1 :: 2 :: 3 :: 4 :: 5 :: Nil)

    @test
    def flatten10(): Bool = Chain.flatten(List.toChain(Chain.singleton(1) :: List.toChain(2 :: 3 :: Nil) :: Chain.singleton(4) :: Nil)) == List.toChain(1 :: 2 :: 3 :: 4 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // exists                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def exists01(): Bool = Chain.exists(i -> i > 3, Chain.empty()) == false

    @test
    def exists02(): Bool = Chain.exists(i -> i > 3, Chain.singleton(1)) == false

    @test
    def exists03(): Bool = Chain.exists(i -> i > 3, Chain.singleton(5)) == true

    @test
    def exists04(): Bool = Chain.exists(i -> i > 3, List.toChain(1 :: 2 :: Nil)) == false

    @test
    def exists05(): Bool = Chain.exists(i -> i > 3, List.toChain(1 :: 6 :: Nil)) == true

    @test
    def exists06(): Bool = Chain.exists(i -> i > 3, List.toChain(6 :: 1 :: Nil)) == true

    @test
    def exists07(): Bool = Chain.exists(i -> i > 3, List.toChain(16 :: 6 :: Nil)) == true

    @test
    def exists08(): Bool = Chain.exists(i -> i > 3, List.toChain(1 :: -9 :: 3 :: Nil)) == false

    @test
    def exists09(): Bool = Chain.exists(i -> i > 3, List.toChain(1 :: 9 :: 3 :: Nil)) == true

    /////////////////////////////////////////////////////////////////////////////
    // forAll                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def forAll01(): Bool = Chain.forAll(i -> i > 3, Chain.empty()) == true

    @test
    def forAll02(): Bool = Chain.forAll(i -> i > 3, Chain.singleton(1)) == false

    @test
    def forAll03(): Bool = Chain.forAll(i -> i > 3, Chain.singleton(5)) == true

    @test
    def forAll04(): Bool = Chain.forAll(i -> i > 3, List.toChain(1 :: 2 :: Nil)) == false

    @test
    def forAll05(): Bool = Chain.forAll(i -> i > 3, List.toChain(1 :: 6 :: Nil)) == false

    @test
    def forAll06(): Bool = Chain.forAll(i -> i > 3, List.toChain(6 :: 1 :: Nil)) == false

    @test
    def forAll07(): Bool = Chain.forAll(i -> i > 3, List.toChain(16 :: 6 :: Nil)) == true

    @test
    def forAll08(): Bool = Chain.forAll(i -> i > 3, List.toChain(1 :: -9 :: 3 :: Nil)) == false

    @test
    def forAll09(): Bool = Chain.forAll(i -> i > 3, List.toChain(1 :: 9 :: 3 :: Nil)) == false

    @test
    def forAll10(): Bool = Chain.forAll(i -> i > 3, List.toChain(11 :: 9 :: 31 :: Nil)) == true

    /////////////////////////////////////////////////////////////////////////////
    // filter                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filter01(): Bool = Chain.filter(i -> i > 3, Chain.empty()) == Chain.empty()

    @test
    def filter02(): Bool = Chain.filter(i -> i > 3, Chain.singleton(2)) == Chain.empty()

    @test
    def filter03(): Bool = Chain.filter(i -> i > 3, Chain.singleton(4)) == Chain.singleton(4)

    @test
    def filter04(): Bool = Chain.filter(i -> i > 3, List.toChain(1 :: 3 :: Nil)) == Chain.empty()

    @test
    def filter05(): Bool = Chain.filter(i -> i > 3, List.toChain(1 :: 8 :: Nil)) == Chain.singleton(8)

    @test
    def filter06(): Bool = Chain.filter(i -> i > 3, List.toChain(8 :: 1 :: Nil)) == Chain.singleton(8)

    @test
    def filter07(): Bool = Chain.filter(i -> i > 3, List.toChain(8 :: 9 :: Nil)) == List.toChain(8 :: 9 :: Nil)

    @test
    def filter08(): Bool = Chain.filter(i -> i > 3, List.toChain(1 :: 4 :: 11 :: 2 :: -22 :: 17 :: Nil)) == List.toChain(4 :: 11 :: 17 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // foldLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldLeft01(): Bool = Chain.foldLeft((i, e) -> (i - e)*(e `Int32.remainder` 2 + 1), 100, Chain.empty()) == 100

    @test
    def foldLeft02(): Bool = Chain.foldLeft((i, e) -> (i - e)*(e `Int32.remainder` 2 + 1), 100, Chain.singleton(1)) == 198

    @test
    def foldLeft03(): Bool = Chain.foldLeft((i, e) -> (i - e)*(e `Int32.remainder` 2 + 1), 100, List.toChain(1 :: 2 :: Nil)) == 196

    @test
    def foldLeft04(): Bool = Chain.foldLeft((i, e) -> (i - e)*(e `Int32.remainder` 2 + 1), 100, List.toChain(1 :: 2 :: 3 :: Nil)) == 386

    /////////////////////////////////////////////////////////////////////////////
    // foldRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldRight01(): Bool = Chain.foldRight((e, acc) -> (acc - e) * (e `Int32.remainder` 2 + 1), 100, Chain.empty()) == 100

    @test
    def foldRight02(): Bool = Chain.foldRight((e, acc) -> (acc - e) * (e `Int32.remainder` 2 + 1), 100, Chain.singleton(1)) == 198

    @test
    def foldRight03(): Bool = Chain.foldRight((e, acc) -> (acc - e) * (e `Int32.remainder` 2 + 1), 100, List.toChain(1 :: 2 :: Nil)) == 194

    @test
    def foldRight04(): Bool = Chain.foldRight((e, acc) -> (acc - e) * (e `Int32.remainder` 2 + 1), 100, List.toChain(1 :: 2 :: 3 :: Nil)) == 382

    /////////////////////////////////////////////////////////////////////////////
    // foldRightWithCont                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldRightWithCont01(): Bool = Chain.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.remainder` 2 + 1), 100, Chain.empty()) == 100

    @test
    def foldRightWithCont02(): Bool = Chain.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.remainder` 2 + 1), 100, Chain.singleton(1)) == 198

    @test
    def foldRightWithCont03(): Bool = Chain.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.remainder` 2 + 1), 100, List.toChain(1 :: 2 :: Nil)) == 194

    @test
    def foldRightWithCont04(): Bool = Chain.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.remainder` 2 + 1), 100, List.toChain(1 :: 2 :: 3 :: Nil)) == 382

    /////////////////////////////////////////////////////////////////////////////
    // foldMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldMap01(): Bool =
        Chain.foldMap(x -> 2 * x, List.toChain(Nil)) == 0

    @test
    def foldMap02(): Bool =
        Chain.foldMap(x -> 2 * x, List.toChain(1 :: 2 :: Nil)) == 6

    @test
    def foldMap03(): Bool =
        Chain.foldMap(x -> if(x == "a") "b" else x, List.toChain("a" :: Nil)) == "b"

    @test
    def foldMap04(): Bool =
        Chain.foldMap(x -> if(x == "c") "b" else x, List.toChain("a" :: "b" :: "c" :: Nil)) == "abb"

    @test
    def foldMap05(): Bool =
        Chain.foldMap(Int32.toString, List.toChain(1 :: 2 :: 3 :: Nil)) == "123"

    /////////////////////////////////////////////////////////////////////////////
    // dropLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def dropLeft01(): Bool = Chain.dropLeft(-1, (Chain.empty(): Chain[Unit])) == Chain.empty()

    @test
    def dropLeft02(): Bool = Chain.dropLeft(0, (Chain.empty(): Chain[Unit])) == Chain.empty()

    @test
    def dropLeft03(): Bool = Chain.dropLeft(1, (Chain.empty(): Chain[Unit])) == Chain.empty()

    @test
    def dropLeft04(): Bool = Chain.dropLeft(-1, Chain.singleton(1)) == Chain.singleton(1)

    @test
    def dropLeft05(): Bool = Chain.dropLeft(0, Chain.singleton(1)) == Chain.singleton(1)

    @test
    def dropLeft06(): Bool = Chain.dropLeft(1, Chain.singleton(1)) == Chain.empty()

    @test
    def dropLeft07(): Bool = Chain.dropLeft(2, Chain.singleton(1)) == Chain.empty()

    @test
    def dropLeft08(): Bool = Chain.dropLeft(0, List.toChain(1 :: 2 :: Nil)) == List.toChain(1 :: 2 :: Nil)

    @test
    def dropLeft09(): Bool = Chain.dropLeft(1, List.toChain(1 :: 2 :: Nil)) == Chain.singleton(2)

    @test
    def dropLeft10(): Bool = Chain.dropLeft(2, List.toChain(1 :: 2 :: Nil)) == Chain.empty()

    @test
    def dropLeft11(): Bool = Chain.dropLeft(2, List.toChain(1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil)) == List.toChain(3 :: 4 :: 5 :: 6 :: Nil)

    @test
    def dropLeft12(): Bool = Chain.dropLeft(4, List.toChain(1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil)) == List.toChain(5 :: 6 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // dropRight                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def dropRight01(): Bool = Chain.dropRight(-1, (Chain.empty(): Chain[Unit])) == Chain.empty()

    @test
    def dropRight02(): Bool = Chain.dropRight(0, (Chain.empty(): Chain[Unit])) == Chain.empty()

    @test
    def dropRight03(): Bool = Chain.dropRight(1, (Chain.empty(): Chain[Unit])) == Chain.empty()

    @test
    def dropRight04(): Bool = Chain.dropRight(-1, Chain.singleton(1)) == Chain.singleton(1)

    @test
    def dropRight05(): Bool = Chain.dropRight(0, Chain.singleton(1)) == Chain.singleton(1)

    @test
    def dropRight06(): Bool = Chain.dropRight(1, Chain.singleton(1)) == Chain.empty()

    @test
    def dropRight07(): Bool = Chain.dropRight(2, Chain.singleton(1)) == Chain.empty()

    @test
    def dropRight08(): Bool = Chain.dropRight(0, List.toChain(1 :: 2 :: Nil)) == List.toChain(1 :: 2 :: Nil)

    @test
    def dropRight09(): Bool = Chain.dropRight(1, List.toChain(1 :: 2 :: Nil)) == Chain.singleton(1)

    @test
    def dropRight10(): Bool = Chain.dropRight(2, List.toChain(1 :: 2 :: Nil)) == Chain.empty()

    @test
    def dropRight11(): Bool = Chain.dropRight(2, List.toChain(1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil)) == List.toChain(1 :: 2 :: 3 :: 4 :: Nil)

    @test
    def dropRight12(): Bool = Chain.dropRight(4, List.toChain(1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil)) == List.toChain(1 :: 2 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // dropWhileLeft                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def dropWhileLeft01(): Bool = Chain.dropWhileLeft(i -> i > 3, Chain.empty()) == Chain.empty()

    @test
    def dropWhileLeft02(): Bool = Chain.dropWhileLeft(i -> i > 3, Chain.singleton(1)) == Chain.singleton(1)

    @test
    def dropWhileLeft03(): Bool = Chain.dropWhileLeft(i -> i > 3, Chain.singleton(4)) == Chain.empty()

    @test
    def dropWhileLeft04(): Bool = Chain.dropWhileLeft(i -> i > 3, List.toChain(1 :: 2 :: Nil)) == List.toChain(1 :: 2 :: Nil)

    @test
    def dropWhileLeft05(): Bool = Chain.dropWhileLeft(i -> i > 3, List.toChain(1 :: 5 :: Nil)) == List.toChain(1 :: 5 :: Nil)

    @test
    def dropWhileLeft06(): Bool = Chain.dropWhileLeft(i -> i > 3, List.toChain(5 :: 1 :: Nil)) == Chain.singleton(1)

    @test
    def dropWhileLeft07(): Bool = Chain.dropWhileLeft(i -> i > 3, List.toChain(5 :: 8 :: Nil)) == Chain.empty()

    @test
    def dropWhileLeft08(): Bool = Chain.dropWhileLeft(i -> i > 3, List.toChain(4 :: 6 :: -3 :: 11 :: -5 :: 1 :: 2 :: 16 :: 7 :: 1 :: 7 :: Nil)) ==
                            List.toChain(-3 :: 11 :: -5 :: 1 :: 2 :: 16 :: 7 :: 1 :: 7 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // dropWhileRight                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def dropWhileRight01(): Bool = Chain.dropWhileRight(i -> i > 3, Chain.empty()) == Chain.empty()

    @test
    def dropWhileRight02(): Bool = Chain.dropWhileRight(i -> i > 3, Chain.singleton(1)) == Chain.singleton(1)

    @test
    def dropWhileRight03(): Bool = Chain.dropWhileRight(i -> i > 3, Chain.singleton(4)) == Chain.empty()

    @test
    def dropWhileRight04(): Bool = Chain.dropWhileRight(i -> i > 3, List.toChain(1 :: 2 :: Nil)) == List.toChain(1 :: 2 :: Nil)

    @test
    def dropWhileRight05(): Bool = Chain.dropWhileRight(i -> i > 3, List.toChain(1 :: 5 :: Nil)) == Chain.singleton(1)

    @test
    def dropWhileRight06(): Bool = Chain.dropWhileRight(i -> i > 3, List.toChain(5 :: 1 :: Nil)) == List.toChain(5 :: 1 :: Nil)

    @test
    def dropWhileRight07(): Bool = Chain.dropWhileRight(i -> i > 3, List.toChain(5 :: 8 :: Nil)) == Chain.empty()

    @test
    def dropWhileRight08(): Bool = Chain.dropWhileRight(i -> i > 3, List.toChain(4 :: 6 :: -3 :: 11 :: -5 :: 1 :: 2 :: 16 :: 7 :: 1 :: 7 :: Nil)) ==
                            List.toChain(4 :: 6 :: -3 :: 11 :: -5 :: 1 :: 2 :: 16 :: 7 :: 1 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // takeLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def takeLeft01(): Bool = Chain.takeLeft(-1, (Chain.empty(): Chain[Unit])) == Chain.empty()

    @test
    def takeLeft02(): Bool = Chain.takeLeft(0, (Chain.empty(): Chain[Unit])) == Chain.empty()

    @test
    def takeLeft03(): Bool = Chain.takeLeft(1, (Chain.empty(): Chain[Unit])) == Chain.empty()

    @test
    def takeLeft04(): Bool = Chain.takeLeft(-1, Chain.singleton(1)) == Chain.empty()

    @test
    def takeLeft05(): Bool = Chain.takeLeft(0, Chain.singleton(1)) == Chain.empty()

    @test
    def takeLeft06(): Bool = Chain.takeLeft(1, Chain.singleton(1)) == Chain.singleton(1)

    @test
    def takeLeft07(): Bool = Chain.takeLeft(2, Chain.singleton(1)) == Chain.singleton(1)

    @test
    def takeLeft08(): Bool = Chain.takeLeft(0, List.toChain(1 :: 2 :: Nil)) == Chain.empty()

    @test
    def takeLeft09(): Bool = Chain.takeLeft(1, List.toChain(1 :: 2 :: Nil)) == Chain.singleton(1)

    @test
    def takeLeft10(): Bool = Chain.takeLeft(2, List.toChain(1 :: 2 :: Nil)) == List.toChain(1 :: 2 :: Nil)

    @test
    def takeLeft11(): Bool = Chain.takeLeft(2, List.toChain(1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil)) == List.toChain(1 :: 2 :: Nil)

    @test
    def takeLeft12(): Bool = Chain.takeLeft(4, List.toChain(1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil)) == List.toChain(1 :: 2 :: 3 :: 4 :: Nil)


    /////////////////////////////////////////////////////////////////////////////
    // takeRight                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def takeRight01(): Bool = Chain.takeRight(-1, (Chain.empty(): Chain[Unit])) == Chain.empty()

    @test
    def takeRight02(): Bool = Chain.takeRight(0, (Chain.empty(): Chain[Unit])) == Chain.empty()

    @test
    def takeRight03(): Bool = Chain.takeRight(1, (Chain.empty(): Chain[Unit])) == Chain.empty()

    @test
    def takeRight04(): Bool = Chain.takeRight(-1, Chain.singleton(1)) == Chain.empty()

    @test
    def takeRight05(): Bool = Chain.takeRight(0, Chain.singleton(1)) == Chain.empty()

    @test
    def takeRight06(): Bool = Chain.takeRight(1, Chain.singleton(1)) == Chain.singleton(1)

    @test
    def takeRight07(): Bool = Chain.takeRight(2, Chain.singleton(1)) == Chain.singleton(1)

    @test
    def takeRight08(): Bool = Chain.takeRight(0, List.toChain(1 :: 2 :: Nil)) == Chain.empty()

    @test
    def takeRight09(): Bool = Chain.takeRight(1, List.toChain(1 :: 2 :: Nil)) == Chain.singleton(2)

    @test
    def takeRight10(): Bool = Chain.takeRight(2, List.toChain(1 :: 2 :: Nil)) == List.toChain(1 :: 2 :: Nil)

    @test
    def takeRight11(): Bool = Chain.takeRight(2, List.toChain(1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil)) == List.toChain(5 :: 6 :: Nil)

    @test
    def takeRight12(): Bool = Chain.takeRight(4, List.toChain(1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil)) == List.toChain(3 :: 4 :: 5 :: 6 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // takeWhileLeft                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def takeWhileLeft01(): Bool = Chain.takeWhileLeft(i -> i > 3, (Chain.empty(): Chain[Int32])) == Chain.empty()

    @test
    def takeWhileLeft02(): Bool = Chain.takeWhileLeft(i -> i > 3, Chain.singleton(1)) == Chain.empty()

    @test
    def takeWhileLeft03(): Bool = Chain.takeWhileLeft(i -> i > 3, Chain.singleton(4)) == Chain.singleton(4)

    @test
    def takeWhileLeft04(): Bool = Chain.takeWhileLeft(i -> i > 3, List.toChain(1 :: 2 :: Nil)) == Chain.empty()

    @test
    def takeWhileLeft05(): Bool = Chain.takeWhileLeft(i -> i > 3, List.toChain(1 :: 5 :: Nil)) == Chain.empty()

    @test
    def takeWhileLeft06(): Bool = Chain.takeWhileLeft(i -> i > 3, List.toChain(5 :: 1 :: Nil)) == Chain.singleton(5)

    @test
    def takeWhileLeft07(): Bool = Chain.takeWhileLeft(i -> i > 3, List.toChain(5 :: 8 :: Nil)) == List.toChain(5 :: 8 :: Nil)

    @test
    def takeWhileLeft08(): Bool = Chain.takeWhileLeft(i -> i > 3, List.toChain(4 :: 6 :: -3 :: 11 :: -5 :: 1 :: 2 :: 16 :: 7 :: 1 :: 7 :: Nil)) ==
                            List.toChain(4 :: 6 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // takeWhileRight                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def takeWhileRight01(): Bool = Chain.takeWhileRight(i -> i > 3, (Chain.empty(): Chain[Int32])) == Chain.empty()

    @test
    def takeWhileRight02(): Bool = Chain.takeWhileRight(i -> i > 3, Chain.singleton(1)) == Chain.empty()

    @test
    def takeWhileRight03(): Bool = Chain.takeWhileRight(i -> i > 3, Chain.singleton(4)) == Chain.singleton(4)

    @test
    def takeWhileRight04(): Bool = Chain.takeWhileRight(i -> i > 3, List.toChain(1 :: 2 :: Nil)) == Chain.empty()

    @test
    def takeWhileRight05(): Bool = Chain.takeWhileRight(i -> i > 3, List.toChain(1 :: 5 :: Nil)) == Chain.singleton(5)

    @test
    def takeWhileRight06(): Bool = Chain.takeWhileRight(i -> i > 3, List.toChain(5 :: 1 :: Nil)) == Chain.empty()

    @test
    def takeWhileRight07(): Bool = Chain.takeWhileRight(i -> i > 3, List.toChain(5 :: 8 :: Nil)) == List.toChain(5 :: 8 :: Nil)

    @test
    def takeWhileRight08(): Bool = Chain.takeWhileRight(i -> i > 3, List.toChain(4 :: 6 :: -3 :: 11 :: -5 :: 1 :: 2 :: 16 :: 7 :: 1 :: 7 :: Nil)) ==
                            Chain.singleton(7)

    /////////////////////////////////////////////////////////////////////////////
    // filterMap                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filterMap01(): Bool = Chain.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, Chain.empty()) == Chain.empty()

    @test
    def filterMap02(): Bool = Chain.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, Chain.singleton(1)) == Chain.empty()

    @test
    def filterMap03(): Bool = Chain.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, Chain.singleton(2)) == Chain.singleton(1)

    @test
    def filterMap04(): Bool = Chain.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, List.toChain(1 :: 3 :: Nil)) == Chain.empty()

    @test
    def filterMap05(): Bool = Chain.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, List.toChain(1 :: 4 :: Nil)) == Chain.singleton(2)

    @test
    def filterMap06(): Bool = Chain.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, List.toChain(6 :: -1 :: Nil)) == Chain.singleton(3)

    @test
    def filterMap07(): Bool = Chain.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, List.toChain(8 :: 6 :: Nil)) == List.toChain(4 :: 3 :: Nil)

    @test
    def filterMap08(): Bool = Chain.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, List.toChain(0 :: 1 :: 2 :: 3 :: 4 :: 5 :: 10 :: Nil)) ==
                            List.toChain(0 :: 1 :: 2 :: 5 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // findMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def findMap01(): Bool = Chain.findMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, Chain.empty()) == None

    @test
    def findMap02(): Bool = Chain.findMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, Chain.singleton(1)) == None

    @test
    def findMap03(): Bool = Chain.findMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, Chain.singleton(2)) == Some(1)

    @test
    def findMap04(): Bool = Chain.findMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, List.toChain(1 :: 3 :: Nil)) == None

    @test
    def findMap05(): Bool = Chain.findMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, List.toChain(1 :: 4 :: Nil)) == Some(2)

    @test
    def findMap06(): Bool = Chain.findMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, List.toChain(6 :: -1 :: Nil)) == Some(3)

    @test
    def findMap07(): Bool = Chain.findMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, List.toChain(8 :: 6 :: Nil)) == Some(4)

    @test
    def findMap08(): Bool = Chain.findMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, List.toChain(0 :: 1 :: 2 :: 3 :: 4 :: 5 :: 10 :: Nil)) == Some(0)

    /////////////////////////////////////////////////////////////////////////////
    // zip                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def zip01(): Bool = Chain.zip((Chain.empty(): Chain[Unit]), (Chain.empty(): Chain[Unit])) == Chain.empty()

    @test
    def zip02(): Bool = Chain.zip(Chain.singleton(1), (Chain.empty(): Chain[Unit])) == Chain.empty()

    @test
    def zip03(): Bool = Chain.zip((Chain.empty(): Chain[Unit]), Chain.singleton(2)) == Chain.empty()

    @test
    def zip04(): Bool = Chain.zip(Chain.singleton(1), Chain.singleton(2)) == Chain.singleton((1, 2))

    @test
    def zip05(): Bool = Chain.zip(List.toChain(1 :: 3 :: Nil), List.toChain(2 :: 4 :: Nil)) == List.toChain((1, 2) :: (3, 4) :: Nil)

    @test
    def zip06(): Bool = Chain.zip(List.toChain(1 :: 3 :: 5 :: Nil), List.toChain(2 :: 4 :: 6 :: Nil)) == List.toChain((1, 2) :: (3, 4) :: (5, 6) :: Nil)

    @test
    def zip07(): Bool = Chain.zip(List.toChain(1 :: 3 :: 5 :: 7 :: Nil), List.toChain(2 :: 4 :: 6 :: 8 :: Nil)) == List.toChain((1, 2) :: (3, 4) :: (5, 6) :: (7, 8) :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // zipWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def zipWith01(): Bool = Chain.zipWith((a, b) -> if (b) a+1 else a, Chain.empty(), Chain.empty()) == Chain.empty()

    @test
    def zipWith02(): Bool = Chain.zipWith((a, b) -> if (b) a+1 else a, Chain.singleton(1), Chain.empty()) == Chain.empty()

    @test
    def zipWith03(): Bool = Chain.zipWith((a, b) -> if (b) a+1 else a, Chain.empty(), Chain.singleton(true)) == Chain.empty()

    @test
    def zipWith04(): Bool = Chain.zipWith((a, b) -> if (b) a+1 else a, Chain.singleton(1), Chain.singleton(true)) == Chain.singleton(2)

    @test
    def zipWith05(): Bool = Chain.zipWith((a, b) -> if (b) a+1 else a, Chain.singleton(1), Chain.singleton(false)) == Chain.singleton(1)

    @test
    def zipWith06(): Bool = Chain.zipWith((a, b) -> if (b) a+1 else a, List.toChain(1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: Nil),
                          List.toChain(false :: true :: true :: false :: false :: true :: true :: true :: Nil)) ==
                          List.toChain(1 :: 3 :: 4 :: 4 :: 5 :: 7 :: 8 :: 9 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // zipWithA                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def zipWithA01(): Bool = Chain.zipWithA((a, b) -> {if (b) a+1 else a} |> Identity.Identity, Chain.empty(), Chain.empty()) == Identity.Identity(Chain.empty())

    @test
    def zipWithA02(): Bool = Chain.zipWithA((a, b) -> {if (b) a+1 else a} |> Identity.Identity, Chain.singleton(1), Chain.empty()) == Identity.Identity(Chain.empty())

    @test
    def zipWithA03(): Bool = Chain.zipWithA((a, b) -> {if (b) a+1 else a} |> Identity.Identity, Chain.empty(), Chain.singleton(true)) == Identity.Identity(Chain.empty())

    @test
    def zipWithA04(): Bool = Chain.zipWithA((a, b) -> {if (b) a+1 else a} |> Identity.Identity, Chain.singleton(1), Chain.singleton(true)) == Identity.Identity(Chain.singleton(2))

    @test
    def zipWithA05(): Bool = Chain.zipWithA((a, b) -> {if (b) a+1 else a} |> Identity.Identity, Chain.singleton(1), Chain.singleton(false)) == Identity.Identity(Chain.singleton(1))

    @test
    def zipWith06A(): Bool = Chain.zipWithA((a, b) -> {if (b) a+1 else a} |> Identity.Identity, List.toChain(1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: Nil),
                          List.toChain(false :: true :: true :: false :: false :: true :: true :: true :: Nil)) ==
                          Identity.Identity(List.toChain(1 :: 3 :: 4 :: 4 :: 5 :: 7 :: 8 :: 9 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // unzip                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def unzip01(): Bool = Chain.unzip((Chain.empty(): Chain[(Unit, Unit)])) == (Chain.empty(), Chain.empty())

    @test
    def unzip02(): Bool = Chain.unzip(Chain.singleton((1, true))) == (Chain.singleton(1), Chain.singleton(true))

    @test
    def unzip03(): Bool = Chain.unzip(List.toChain((1, true) :: (2, true) :: Nil)) == (List.toChain(1 :: 2 :: Nil), List.toChain(true :: true :: Nil))

    @test
    def unzip04(): Bool = Chain.unzip(List.toChain((1, true) :: (2, true) :: (3, false) :: Nil)) ==
                        (List.toChain(1 :: 2 :: 3 :: Nil), List.toChain(true :: true :: false :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // mapAccumLeft                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapAccumLeft01(): Bool = Chain.mapAccumLeft((s, x) -> (s+1, x+s), 0, Chain.empty()) == (0, Chain.empty())

    @test
    def mapAccumLeft02(): Bool = Chain.mapAccumLeft((s, x) -> (s+1, x+s), 0, Chain.singleton(1)) == (1, Chain.singleton(1))

    @test
    def mapAccumLeft03(): Bool = Chain.mapAccumLeft((s, x) -> (s+1, x+s), 0, List.toChain(1 :: 1 :: Nil)) == (2, List.toChain(1 :: 2 :: Nil))

    @test
    def mapAccumLeft04(): Bool = Chain.mapAccumLeft((s, x) -> (s+1, x+s), 0, List.toChain(1 :: 1 :: 1 :: Nil)) == (3, List.toChain(1 :: 2 :: 3 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // mapAccumRight                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapAccumRight01(): Bool = Chain.mapAccumRight((s, x) -> (s+1, x+s), 0, Chain.empty()) == (0, Chain.empty())

    @test
    def mapAccumRight02(): Bool = Chain.mapAccumRight((s, x) -> (s+1, x+s), 0, Chain.singleton(1)) == (1, Chain.singleton(1))

    @test
    def mapAccumRight03(): Bool = Chain.mapAccumRight((s, x) -> (s+1, x+s), 0, List.toChain(1 :: 1 :: Nil)) == (2, List.toChain(2 :: 1 :: Nil))

    @test
    def mapAccumRight04(): Bool = Chain.mapAccumRight((s, x) -> (s+1, x+s), 0, List.toChain(1 :: 1 :: 1 :: Nil)) == (3, List.toChain(3 :: 2 :: 1 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // forEach                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def forEach01(): Bool = region rc {
        let ri = Ref.fresh(rc, 21);
        Chain.forEach(x -> Ref.put(x, ri), Chain.empty());
        21 == deref ri
    }

    @test
    def forEach02(): Bool = region rc {
        let ri = Ref.fresh(rc, 21);
        Chain.forEach(x -> Ref.put(x, ri), List.toChain(1 :: 2 :: 3 :: Nil));
        3 == deref ri
    }

    /////////////////////////////////////////////////////////////////////////////
    // forEachWithIndex                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def forEachWithIndex01(): Bool = region rc {
        let ri = Ref.fresh(rc, 21);
        Chain.forEachWithIndex((i, _) -> Ref.put(i, ri), Chain.empty());
        21 == deref ri
    }

    @test
    def forEachWithIndex02(): Bool = region rc {
        let ri = Ref.fresh(rc, 21);
        Chain.forEachWithIndex((i, _) -> Ref.put(i, ri), List.toChain(1 :: 2 :: 3 :: Nil));
        2 == deref ri
    }

    /////////////////////////////////////////////////////////////////////////////
    // toList                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toList01(): Bool =
        Chain.toList((Chain.empty(): Chain[Int32])) == (Nil: List[Int32])

    @test
    def toList02(): Bool =
        Chain.toList(Chain.singleton(1)) == 1 :: Nil

    @test
    def toList03(): Bool =
        Chain.toList(List.toChain(1 :: 2 :: Nil)) == 1 :: 2 :: Nil

    @test
    def toList04(): Bool =
        Chain.toList(List.toChain(1 :: 2 :: 3 :: Nil)) == 1 :: 2 :: 3 :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // toMutList                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toMutList01(): Bool = region rc {
        Chain.toMutList(rc, (Chain.empty(): Chain[Int32])) `MutList.sameElements` (List.toMutList(rc, Nil): MutList[Int32, rc])
    }

    @test
    def toMutList02(): Bool = region rc {
        Chain.toMutList(rc, Chain.singleton(1)) `MutList.sameElements` List.toMutList(rc, 1 :: Nil)
    }

    @test
    def toMutList03(): Bool = region rc {
        Chain.toMutList(rc, List.toChain(1 :: 2 :: Nil)) `MutList.sameElements` List.toMutList(rc, 1 :: 2 :: Nil)
    }

    @test
    def toMutList04(): Bool = region rc {
        Chain.toMutList(rc, List.toChain(1 :: 2 :: 3 :: Nil)) `MutList.sameElements` List.toMutList(rc, 1 :: 2 :: 3 :: Nil)
    }

    /////////////////////////////////////////////////////////////////////////////
    // toMutDeque                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toMutDeque01(): Bool = region rc {
        let c = (Chain.empty(): Chain[Int32]);
        let d1 = Chain.toMutDeque(rc, c);

        let d2 = MutDeque.empty(rc);

        d1 `MutDeque.sameElements` d2
    }

    @test
    def toMutDeque02(): Bool = region rc {
        let c = Chain.singleton(1);
        let d1 = Chain.toMutDeque(rc, c);

        let d2 = MutDeque.empty(rc);
        MutDeque.pushBack(1, d2);

        d1 `MutDeque.sameElements` d2
    }

    @test
    def toMutDeque03(): Bool = region rc {
        let c = List.toChain(1 :: 2 :: Nil);
        let d1 = Chain.toMutDeque(rc, c);

        let d2 = MutDeque.empty(rc);
        MutDeque.pushFront(1, d2);
        MutDeque.pushBack(2, d2);

        d1 `MutDeque.sameElements` d2
    }

    @test
    def toMutDeque04(): Bool = region rc {
        let c = List.toChain('a' :: 'b' :: 'c' :: Nil);
        let d1 = Chain.toMutDeque(rc, c);

        let d2 = MutDeque.empty(rc);
        MutDeque.pushFront('b', d2);
        MutDeque.pushBack('c', d2);
        MutDeque.pushFront('a', d2);

        d1 `MutDeque.sameElements` d2
    }

    /////////////////////////////////////////////////////////////////////////////
    // toSet                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toSet01(): Bool =
        Chain.toSet((Chain.empty(): Chain[Int32])) == (Set#{}: Set[Int32])

    @test
    def toSet02(): Bool =
        Chain.toSet(Chain.singleton(1)) == Set#{1}

    @test
    def toSet03(): Bool =
        Chain.toSet(List.toChain(1 :: 2 :: Nil)) == Set#{1, 2}

    @test
    def toSet04(): Bool =
        Chain.toSet(List.toChain(1 :: 2 :: 3 :: Nil)) == Set#{1, 2, 3}

    /////////////////////////////////////////////////////////////////////////////
    // toMap                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toMap01(): Bool =
        Chain.toMap((Chain.empty(): Chain[(Int32, Int32)])) == (Map#{}: Map[Int32, Int32])

    @test
    def toMap02(): Bool =
        Chain.toMap(Chain.singleton((1, 1))) == Map#{1 => 1}

    @test
    def toMap03(): Bool =
        Chain.toMap(List.toChain((1, 1) :: (2, 2) :: Nil)) == Map#{1 => 1, 2 => 2}

    @test
    def toMap04(): Bool =
        Chain.toMap(List.toChain((1, 1) :: (2, 2) :: (3, 3) :: Nil)) == Map#{1 => 1, 2 => 2, 3 => 3}

    /////////////////////////////////////////////////////////////////////////////
    // toArray                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toArray01(): Bool = region rc {
        let c: Chain[Int32] = Chain.empty();
        Array.sameElements(Chain.toArray(rc, c), Array#{} @ rc)
    }

    @test
    def toArray02(): Bool = region rc {
        let a = Chain.toArray(rc, Chain.singleton(1));
        Array.sameElements(a, Array#{1} @ rc)
    }

    @test
    def toArray03(): Bool = region rc {
        let a = Chain.toArray(rc, List.toChain(1 :: 2 :: Nil));
        Array.sameElements(a, Array#{1, 2} @ rc)
    }

    @test
    def toArray04(): Bool = region rc {
        let a = Chain.toArray(rc, List.toChain(1 :: 2 :: 3 :: Nil));
        Array.sameElements(a, Array#{1, 2, 3} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // toVector                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toVector01(): Bool =
        let c: Chain[Int32] = Chain.empty();
        Chain.toVector(c) == Vector#{}

    @test
    def toVector02(): Bool =
        let c = Chain.singleton(1);
        Chain.toVector(c) == Vector#{1}

    @test
    def toVector03(): Bool =
        let c = List.toChain(1 :: 2 :: Nil);
        Chain.toVector(c) == Vector#{1, 2}

    @test
    def toVector04(): Bool =
        let c = List.toChain(1 :: 2 :: 3 :: Nil);
        Chain.toVector(c) == Vector#{1, 2, 3}

    /////////////////////////////////////////////////////////////////////////////
    // iterator                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def iterator01(): Bool = region rc {
        Chain.empty() |> Chain.iterator(rc) |> Iterator.toList == (Nil: List[Int32])
    }

    @test
    def iterator02(): Bool = region rc {
        List.toChain(1 :: 2 :: 3 :: Nil) |> Chain.iterator(rc) |> Iterator.toList == 1 :: 2 :: 3 :: Nil
    }

    @test
    def iterator03(): Bool = region rc {
        Chain.range(-100, 100) |> Chain.iterator(rc) |> Iterator.toList == List.range(-100, 100)
    }


    /////////////////////////////////////////////////////////////////////////////
    // enumerator                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def enumerator01(): Bool = region rc {
        Chain.empty() |> Chain.enumerator(rc) |> Iterator.toList == (Nil: List[(Int32, Int32)])
    }

    @test
    def enumerator02(): Bool = region rc {
        List.toChain(1 :: 2 :: 3 :: Nil) |> Chain.enumerator(rc) |> Iterator.toList == (0, 1) :: (1, 2) :: (2, 3) :: Nil
    }

    @test
    def enumerator03(): Bool = region rc {
        Chain.range(-100, 100) |> Chain.enumerator(rc) |> Iterator.toList == List.range(-100, 100) |> List.zipWithIndex
    }


    /////////////////////////////////////////////////////////////////////////////
    // toNel.Nel                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toNel01(): Bool = Chain.toNel((Chain.empty(): Chain[Int32])) == None

    @test
    def toNel02(): Bool = Chain.toNel(Chain.singleton(1)) == Some(Nel.Nel(1, Nil))

    @test
    def toNel03(): Bool =
        Chain.toNel(List.toChain(1 :: 2 :: Nil)) == Some(Nel.Nel(1, 2 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // toNec                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toNec01(): Bool = Chain.toNec((Chain.empty(): Chain[Int32])) == None

    @test
    def toNec02(): Bool = Chain.toNec(Chain.singleton(1)) == Some(Nec.singleton(1))

    @test
    def toNec03(): Bool =
        Chain.toNec(List.toChain(1 :: 2 :: Nil)) == Some(Nec.cons(1, Nec.singleton(2)))

    /////////////////////////////////////////////////////////////////////////////
    // equals                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def equals01(): Bool =
        (Chain.empty(): Chain[Int32]) `Chain.equals` (Chain.empty(): Chain[Int32])

    @test
    def equals02(): Bool =
        Chain.singleton(1) `Chain.equals` Chain.singleton(1)

    @test
    def equals03(): Bool =
        (Chain.empty() |> Chain.cons(1)) `Chain.equals` Chain.singleton(1)

    @test
    def equals04(): Bool =
        (Chain.empty() `Chain.snoc` 1) `Chain.equals` Chain.singleton(1)

    @test
    def equals05(): Bool =
        (Chain.empty() `Chain.snoc` 1) `Chain.equals` (Chain.empty() |> Chain.cons(1))

    /////////////////////////////////////////////////////////////////////////////
    // compare                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def compare01(): Bool =
        (Chain.empty(): Chain[Int32]) `Chain.compare` (Chain.empty(): Chain[Int32]) == Comparison.EqualTo

    @test
    def compare02(): Bool =
        Chain.singleton(1) `Chain.compare` Chain.empty() == Comparison.GreaterThan

    @test
    def compare03(): Bool =
        Chain.empty() `Chain.compare` Chain.singleton(1) == Comparison.LessThan

    @test
    def compare04(): Bool =
        Chain.singleton(1) `Chain.compare` Chain.singleton(1) == Comparison.EqualTo

    @test
    def compare05(): Bool =
        Chain.singleton(1) `Chain.compare` Chain.singleton(2) == Comparison.LessThan

    @test
    def compare06(): Bool =
        Chain.singleton(2) `Chain.compare` Chain.singleton(1) == Comparison.GreaterThan

    @test
    def compare07(): Bool =
        Chain.singleton(1) `Chain.compare` List.toChain(1 :: 2 :: Nil) == Comparison.LessThan

    @test
    def compare08(): Bool =
        List.toChain(1 :: 2 :: Nil) `Chain.compare` Chain.singleton(1) == Comparison.GreaterThan

    @test
    def compare09(): Bool =
        Chain.singleton(1) `Chain.compare` List.toChain(0 :: 0 :: Nil) == Comparison.GreaterThan

    @test
    def compare10(): Bool =
        List.toChain(0 :: 0 :: Nil)  `Chain.compare` Chain.singleton(1) == Comparison.LessThan

    @test
    def compare11(): Bool =
        List.toChain(1 :: 2 :: Nil) `Chain.compare` List.toChain(1 :: 1 :: Nil) == Comparison.GreaterThan

    @test
    def compare12(): Bool =
        List.toChain(1 :: 2 :: Nil) `Chain.compare` List.toChain(1 :: 3 :: Nil) == Comparison.LessThan

    @test
    def compare13(): Bool =
        List.toChain(1 :: 2 :: 3 :: 4 :: Nil) `Chain.compare` List.toChain(1 :: 2 :: 3 :: 4 :: Nil) == Comparison.EqualTo

    /////////////////////////////////////////////////////////////////////////////
    // sort                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    def testSortVsSortWith(c: Chain[Int32]) : Bool =
        Chain.sort(c) == Chain.sortWith(cmp, c)

    @test
    def sort01(): Bool = testSortVsSortWith((Chain.empty(): Chain[Int32]))

    @test
    def sort02(): Bool = testSortVsSortWith(Chain.singleton(0))

    @test
    def sort03(): Bool = testSortVsSortWith(List.toChain(0 :: 1 :: Nil))

    @test
    def sort04(): Bool = testSortVsSortWith(List.toChain(1 :: 0 :: Nil))

    @test
    def sort05(): Bool = testSortVsSortWith(List.toChain(1 :: 1 :: Nil))

    @test
    def sort06(): Bool = testSortVsSortWith(List.toChain(0 :: 1 :: 2 :: 3 :: 4 :: 5 :: Nil))

    @test
    def sort07(): Bool = testSortVsSortWith(List.toChain(5 :: 4 :: 3 :: 2 :: 1 :: 0 :: Nil))

    @test
    def sort08(): Bool = testSortVsSortWith(List.toChain(5 :: 3 :: 0 :: 4 :: 1 :: 2 :: Nil))

    @test
    def sort09(): Bool = testSortVsSortWith(List.toChain(2 :: 3 :: 0 :: 4 :: 1 :: 2 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // sortBy                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    def testSortByVsSort(a: Chain[Int32]) : Bool =
        (Chain.sortBy(identity, a) == Chain.sort(a)) and
        (Chain.sortBy(x -> 4*x+7, a) == Chain.sort(a)) and
        (Chain.sortBy(x -> -x, a) == Chain.sortWith(flip(cmp),a))

    @test
    def sortBy01(): Bool = testSortByVsSort((Chain.empty(): Chain[Int32]))

    @test
    def sortBy02(): Bool = testSortByVsSort(Chain.singleton(0))

    @test
    def sortBy03(): Bool = testSortByVsSort(List.toChain(0 :: 1 :: Nil))

    @test
    def sortBy04(): Bool = testSortByVsSort(List.toChain(1 :: 0 :: Nil))

    @test
    def sortBy05(): Bool = testSortByVsSort(List.toChain(1 :: 1 :: Nil))

    @test
    def sortBy06(): Bool = testSortByVsSort(List.toChain(0 :: 1 :: 2 :: 3 :: 4 :: 5 :: Nil))

    @test
    def sortBy07(): Bool = testSortByVsSort(List.toChain(5 :: 4 :: 3 :: 2 :: 1 :: 0 :: Nil))

    @test
    def sortBy08(): Bool = testSortByVsSort(List.toChain(5 :: 3 :: 0 :: 4 :: 1 :: 2 :: Nil))

    @test
    def sortBy09(): Bool = testSortByVsSort(List.toChain(2 :: 3 :: 0 :: 4 :: 1 :: 2 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // sortWith                                                                //
    /////////////////////////////////////////////////////////////////////////////

    def cmp(x: Int32, y: Int32): Comparison =
        if (x < y) Comparison.LessThan
        else if (x == y) Comparison.EqualTo
        else Comparison.GreaterThan

    @test
    def sortWith01(): Bool =
        Chain.sortWith(cmp, (Chain.empty(): Chain[Int32])) == Chain.empty()

    @test
    def sortWith02(): Bool =
        Chain.sortWith(cmp, Chain.singleton(0)) == Chain.singleton(0)

    @test
    def sortWith03(): Bool =
        Chain.sortWith(cmp, List.toChain(0 :: 1 :: Nil)) == List.toChain(0 :: 1 :: Nil)

    @test
    def sortWith04(): Bool =
        Chain.sortWith(cmp, List.toChain(1 :: 0 :: Nil)) == List.toChain(0 :: 1 :: Nil)

    @test
    def sortWith05(): Bool =
        Chain.sortWith(cmp, List.toChain(1 :: 1 :: Nil)) == List.toChain(1 :: 1 :: Nil)

    @test
    def sortWith06(): Bool =
        Chain.sortWith(cmp, List.toChain(0 :: 1 :: 2 :: 3 :: 4 :: 5 :: Nil)) == List.toChain(0 :: 1 :: 2 :: 3 :: 4 :: 5 :: Nil)

    @test
    def sortWith07(): Bool =
        Chain.sortWith(cmp, List.toChain(5 :: 4 :: 3 :: 2 :: 1 :: 0 :: Nil)) == List.toChain(0 :: 1 :: 2 :: 3 :: 4 :: 5 :: Nil)

    @test
    def sortWith08(): Bool =
        Chain.sortWith(cmp, List.toChain(5 :: 3 :: 0 :: 4 :: 1 :: 2 :: Nil)) == List.toChain(0 :: 1 :: 2 :: 3 :: 4 :: 5 :: Nil)

    @test
    def sortWith09(): Bool =
        Chain.sortWith(cmp, List.toChain(2 :: 3 :: 0 :: 4 :: 1 :: 2 :: Nil)) == List.toChain(0 :: 1 :: 2 :: 2 :: 3 :: 4 :: Nil)

    @test
    def sortWith10(): Bool =
        Chain.sortWith(flip(cmp), List.toChain(0 :: 1 :: 2 :: 3 :: 4 :: 5 :: Nil)) == List.toChain(5 :: 4 :: 3 :: 2 :: 1 :: 0 :: Nil)

    @test
    def sortWith11(): Bool =
        Chain.sortWith(flip(cmp), List.toChain(5 :: 4 :: 3 :: 2 :: 1 :: 0 :: Nil)) == List.toChain(5 :: 4 :: 3 :: 2 :: 1 :: 0 :: Nil)

    @test
    def sortWith12(): Bool =
        Chain.sortWith(flip(cmp), List.toChain(5 :: 3 :: 0 :: 4 :: 1 :: 2 :: Nil)) == List.toChain(5 :: 4 :: 3 :: 2 :: 1 :: 0 :: Nil)

    @test
    def sortWith13(): Bool =
        Chain.sortWith(flip(cmp), List.toChain(2 :: 3 :: 0 :: 4 :: 1 :: 2 :: Nil)) == List.toChain(4 :: 3 :: 2 :: 2 :: 1 :: 0 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // sequence                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sequence01(): Bool =
        let c: Chain[Identity[Int32]] = Chain.empty();
        Chain.sequence(c) == Identity.Identity(Chain.empty())

    @test
    def sequence02(): Bool =
        let c = Chain.singleton(Identity.Identity(1));
        Chain.sequence(c) == Identity.Identity(Chain.singleton(1))

    @test
    def sequence03(): Bool =
        let c = List.toChain(Identity.Identity(1) :: Identity.Identity(2) :: Nil);
        Chain.sequence(c) == Identity.Identity(List.toChain(1 :: 2 :: Nil))

    @test
    def sequence04(): Bool =
        let c = List.toChain(Identity.Identity(1) :: Identity.Identity(2) :: Identity.Identity(3) :: Nil);
        Chain.sequence(c) == Identity.Identity(List.toChain(1 :: 2 :: 3 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // traverse                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def traverse01(): Bool = region rc {
        let st = Ref.fresh(rc, 0);
        let c = Chain.empty();
        let ans = Chain.traverse(x -> {Ref.put(x, st); Identity.Identity(x)}, c);
        ans == Identity.Identity(Chain.empty()) and deref st == 0
    }

    @test
    def traverse02(): Bool = region rc {
        let st = Ref.fresh(rc, 0);
        let c = Chain.singleton(1);
        let ans = Chain.traverse(x -> {Ref.put(x, st); Identity.Identity(x)}, c);
        ans == Identity.Identity(Chain.singleton(1)) and deref st == 1
    }

    @test
    def traverse03(): Bool = region rc {
        let st = Ref.fresh(rc, 0);
        let c = List.toChain(1 :: 2 :: Nil);
        let ans = Chain.traverse(x -> {Ref.put(x, st); Identity.Identity(x)}, c);
        ans == Identity.Identity(List.toChain(1 :: 2 :: Nil)) and deref st == 2
    }

    @test
    def traverse04(): Bool = region rc {
        let st = Ref.fresh(rc, 0);
        let c = List.toChain(1 :: 2 :: 3 :: Nil);
        let ans = Chain.traverse(x -> {Ref.put(x, st); Identity.Identity(x)}, c);
        ans == Identity.Identity(List.toChain(1 :: 2 :: 3 :: Nil)) and deref st == 3
    }

    /////////////////////////////////////////////////////////////////////////////
    // join                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def join01(): Bool =
        (Nil: List[Int32]) |> List.toChain |> Chain.join(",") == ""

    @test
    def join02(): Bool =
        (1 :: Nil) |> List.toChain |> Chain.join(",") == "1"

    @test
    def join03(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List.toChain |> Chain.join(",") == "1,2,3"

    @test
    def join04(): Bool =
        ("1" :: "2" :: "3" :: Nil) |> List.toChain |> Chain.join(",") == "1,2,3"


    /////////////////////////////////////////////////////////////////////////////
    // joinWith                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def joinWith01(): Bool =
        (Nil: List[Int32]) |> List.toChain |>
            Chain.joinWith(x -> "${x + 1}", ",") == ""

    @test
    def joinWith02(): Bool =
        (1 :: Nil) |> List.toChain |>
            Chain.joinWith(x -> "${x + 1}", ",") == "2"

    @test
    def joinWith03(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List.toChain |>
            Chain.joinWith(x -> "${x + 1}", ",") == "2,3,4"

    @test
    def joinWith04(): Bool =
        ("1" :: "2" :: "3" :: Nil) |> List.toChain |>
            Chain.joinWith(x -> x + x, ",") == "11,22,33"

    /////////////////////////////////////////////////////////////////////////////
    // zipWithIndex                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def zipWithIndex01(): Bool = Chain.zipWithIndex((Chain.empty(): Chain[Int32])) == Chain.empty()

    @test
    def zipWithIndex02(): Bool = Chain.zipWithIndex(Chain.singleton(2)) == List.toChain((0, 2) :: Nil)

    @test
    def zipWithIndex03(): Bool = Chain.zipWithIndex(List.toChain(1 :: 8 :: Nil)) == List.toChain((0, 1) :: (1, 8) :: Nil)

    @test
    def zipWithIndex04(): Bool = Chain.zipWithIndex(List.toChain(8 :: 1 :: Nil)) == List.toChain((0, 8) :: (1, 1) :: Nil)

    @test
    def zipWithIndex05(): Bool = Chain.zipWithIndex(List.toChain(1 :: 4 :: 11 :: 2 :: -22 :: 17 :: Nil)) == List.toChain((0, 1) :: (1, 4) :: (2, 11) :: (3, 2) :: (4, -22) :: (5, 17) :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // shuffle                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def shuffle01(): Bool \ IO =
        let l: List[Int32] = Nil;
        let c = List.toChain(l) |> Chain.shuffle(Random.fresh());

        Chain.length(c) == 0 and Chain.toSet(c) == Set#{}

    @test
    def shuffle02(): Bool \ IO =
        let l = 1 :: Nil;
        let c = List.toChain(l) |> Chain.shuffle(Random.fresh());

        Chain.length(c) == 1 and Chain.toSet(c) == Set#{1}

    @test
    def shuffle03(): Bool \ IO =
        let l = 1 :: 2 :: 3 :: Nil;
        let c = List.toChain(l) |> Chain.shuffle(Random.fresh());

        Chain.length(c) == 3 and Chain.toSet(c) == Set#{1, 2, 3}

    @test
    def shuffle04(): Bool \ IO =
        let l = 0 :: 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: 9 :: Nil;
        let c = List.toChain(l) |> Chain.shuffle(Random.fresh());

        Chain.length(c) == 10 and Chain.toSet(c) == Set#{0, 1, 2, 3, 4, 5, 6, 7, 8, 9}

}

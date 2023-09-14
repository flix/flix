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

    @Test
    def collect01(): Bool = region rc {
        Collectable.collect(Iterator.empty(rc): Iterator[Int32, rc, rc]) == Chain.empty()
    }

    @Test
    def collect02(): Bool = region rc {
        Collectable.collect(Iterator.singleton(rc, 1)) == List.toChain(1 :: Nil)
    }

    @Test
    def collect03(): Bool = region rc {
        Collectable.collect(Iterator.range(rc, 1, 3)) == List.toChain(1 :: 2 :: Nil)
    }

    /////////////////////////////////////////////////////////////////////////////
    // empty                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def empty01(): Bool =
        let c: Chain[Int32] = Chain.empty();
        Chain.toList(c) == Nil

    /////////////////////////////////////////////////////////////////////////////
    // singleton                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def singleton01(): Bool =
        let c = Chain.singleton(1);
        Chain.toList(c) == 1 :: Nil

    @Test
    def singleton02(): Bool =
        let c: Chain[Option[Int32]] = Chain.singleton(None);
        Chain.toList(c) == None :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // ap                                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def ap01(): Bool = Chain.ap(Chain.empty(), Chain.empty()) == Chain.empty() : Chain[Int32]

    @Test
    def ap02(): Bool = Chain.ap(Chain.singleton(x -> x+1), Chain.empty()) == Chain.empty()

    @Test
    def ap03(): Bool = Chain.ap(Chain.empty(), Chain.singleton(5)) == Chain.empty() : Chain[Int32]

    @Test
    def ap04(): Bool = Chain.ap(Chain.singleton(x -> x+1), Chain.singleton(5)) == Chain.singleton(6)

    @Test
    def ap05(): Bool = Chain.ap(Chain.singleton(x -> x+1), List.toChain(0 :: 5 :: Nil)) == List.toChain(1 :: 6 :: Nil)

    @Test
    def ap06(): Bool = Chain.ap(List.toChain((x -> x+1) :: (x -> x*2) :: Nil), List.toChain(0 :: 4 :: Nil)) == List.toChain(1 :: 5 :: 0 :: 8 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // isEmpty                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isEmpty01(): Bool = Chain.isEmpty(Chain.empty()) == true

    @Test
    def isEmpty02(): Bool = Chain.isEmpty(Chain.singleton(1)) == false

    @Test
    def isEmpty03(): Bool = Chain.isEmpty(List.toChain(1 :: 2 :: Nil)) == false

    /////////////////////////////////////////////////////////////////////////////
    // cons                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def cons01(): Bool = Chain.cons(1, Chain.empty()) == Chain.singleton(1)

    @Test
    def cons02(): Bool = Chain.cons(1, Chain.singleton(2)) == List.toChain(1 :: 2 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // snoc                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def snoc01(): Bool = Chain.snoc(Chain.empty(), 1) == Chain.singleton(1)

    @Test
    def snoc02(): Bool = Chain.snoc(Chain.singleton(1), 2) == List.toChain(1 :: 2 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // head                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def head01(): Bool = Chain.head(Chain.empty(): Chain[Unit]) == None

    @Test
    def head02(): Bool = Chain.head(Chain.singleton(1)) == Some(1)

    @Test
    def head03(): Bool = Chain.head(List.toChain(2 :: 1 :: Nil)) == Some(2)

    @Test
    def head04(): Bool = Chain.head(List.toChain(3 :: 2 :: 1 :: Nil)) == Some(3)

    /////////////////////////////////////////////////////////////////////////////
    // last                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def last01(): Bool = Chain.last(Chain.empty(): Chain[Unit]) == None

    @Test
    def last02(): Bool = Chain.last(Chain.singleton(1)) == Some(1)

    @Test
    def last03(): Bool = Chain.last(List.toChain(1 :: 2 :: Nil)) == Some(2)

    @Test
    def last04(): Bool = Chain.last(List.toChain(1 :: 2 :: 3 :: Nil)) == Some(3)

    /////////////////////////////////////////////////////////////////////////////
    // init                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def init01(): Bool = Chain.init(Chain.empty(): Chain[Unit]) == None

    @Test
    def init02(): Bool = Chain.init(Chain.singleton(0)) == Some(Chain.empty())

    @Test
    def init03(): Bool = Chain.init(List.toChain(0 :: 1 :: Nil)) == Some(Chain.singleton(0))

    @Test
    def init04(): Bool = Chain.init(List.toChain(2 :: 1 :: 0 :: Nil)) == Some(List.toChain(2 :: 1 :: Nil))

    @Test
    def init05(): Bool = Chain.init(List.toChain('a' :: 'b' :: 'c' :: 'd' :: Nil)) == Some(List.toChain('a' :: 'b' :: 'c' :: Nil))

    @Test
    def init06(): Bool = Chain.init(List.toChain("test1" :: "test2" :: "test3" :: Nil)) == Some(List.toChain("test1" :: "test2" :: Nil))

    @Test
    def init07(): Bool = Chain.init(List.toChain(2 :: 1 :: 0 :: -1 :: Nil)) == Some(List.toChain(2 :: 1 :: 0 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // length                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def length01(): Bool = Chain.length(Chain.empty()) == 0

    @Test
    def length02(): Bool = Chain.length(Chain.singleton(1)) == 1

    @Test
    def length03(): Bool = Chain.length(List.toChain(1 :: 2 :: Nil)) == 2

    @Test
    def length04(): Bool = Chain.length(List.toChain(1 :: 2 :: 3 :: Nil)) == 3

    /////////////////////////////////////////////////////////////////////////////
    // append                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def append01(): Bool = Chain.append(Chain.empty(): Chain[Unit], Chain.empty(): Chain[Unit]) == Chain.empty()

    @Test
    def append02(): Bool = Chain.append(Chain.empty(), Chain.singleton(1)) == Chain.singleton(1)

    @Test
    def append03(): Bool = Chain.append(Chain.empty(), List.toChain(1 :: 2 :: Nil)) == List.toChain(1 :: 2 :: Nil)

    @Test
    def append04(): Bool = Chain.append(Chain.singleton(1), Chain.empty()) == Chain.singleton(1)

    @Test
    def append05(): Bool = Chain.append(List.toChain(1 :: 2 :: Nil), Chain.empty()) == List.toChain(1 :: 2 :: Nil)

    @Test
    def append06(): Bool = Chain.append(Chain.singleton(1), Chain.singleton(2)) == List.toChain(1 :: 2 :: Nil)

    @Test
    def append07(): Bool = Chain.append(List.toChain(1 :: 2 :: Nil), Chain.singleton(3)) == List.toChain(1 :: 2 :: 3 :: Nil)

    @Test
    def append08(): Bool = Chain.append(Chain.singleton(1), List.toChain(2 :: 3 :: Nil)) == List.toChain(1 :: 2 :: 3 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // viewLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def viewLeft01(): Bool = Chain.viewLeft(Chain.empty(): Chain[Int32]) == Chain.ViewLeft.NoneLeft

    @Test
    def viewLeft02(): Bool = Chain.viewLeft(Chain.singleton(1)) == Chain.ViewLeft.SomeLeft(1, Chain.empty())

    @Test
    def viewLeft03(): Bool = Chain.viewLeft(List.toChain(1 :: 2 :: Nil)) == Chain.ViewLeft.SomeLeft(1, Chain.singleton(2))

    @Test
    def viewLeft04(): Bool = Chain.viewLeft(List.toChain(1 :: 2 :: 3 :: Nil)) == Chain.ViewLeft.SomeLeft(1, List.toChain(2 :: 3 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // viewRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def viewRight01(): Bool = Chain.viewRight(Chain.empty(): Chain[Int32]) == Chain.ViewRight.NoneRight

    @Test
    def viewRight02(): Bool = Chain.viewRight(Chain.singleton(1)) == Chain.ViewRight.SomeRight(Chain.empty(), 1)

    @Test
    def viewRight03(): Bool = Chain.viewRight(List.toChain(1 :: 2 :: Nil)) == Chain.ViewRight.SomeRight(Chain.singleton(1), 2)

    @Test
    def viewRight04(): Bool = Chain.viewRight(List.toChain(1 :: 2 :: 3 :: Nil)) == Chain.ViewRight.SomeRight(List.toChain(1 :: 2 :: Nil), 3)

    /////////////////////////////////////////////////////////////////////////////
    // memberOf                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def memberOf01(): Bool = Chain.memberOf(0, Chain.empty()) == false

    @Test
    def memberOf02(): Bool = Chain.memberOf(0, Chain.singleton(1)) == false

    @Test
    def memberOf03(): Bool = Chain.memberOf(0, Chain.singleton(0)) == true

    @Test
    def memberOf04(): Bool = Chain.memberOf(0, List.toChain(1 :: 2 :: Nil)) == false

    @Test
    def memberOf05(): Bool = Chain.memberOf(1, List.toChain(1 :: 2 :: Nil)) == true

    @Test
    def memberOf06(): Bool = Chain.memberOf(2, List.toChain(1 :: 2 :: Nil)) == true

    @Test
    def memberOf07(): Bool = Chain.memberOf(3, List.toChain(1 :: 2 :: Nil)) == false

    /////////////////////////////////////////////////////////////////////////////
    // indexOf                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def indexOf01(): Bool = Chain.indexOf(0, Chain.empty()) == None

    @Test
    def indexOf02(): Bool = Chain.indexOf(0, Chain.singleton(1)) == None

    @Test
    def indexOf03(): Bool = Chain.indexOf(1, Chain.singleton(1)) == Some(0)

    @Test
    def indexOf04(): Bool = Chain.indexOf(0, List.toChain(1 :: 2 :: Nil)) == None

    @Test
    def indexOf05(): Bool = Chain.indexOf(1, List.toChain(1 :: 2 :: Nil)) == Some(0)

    @Test
    def indexOf06(): Bool = Chain.indexOf(2, List.toChain(1 :: 2 :: Nil)) == Some(1)

    /////////////////////////////////////////////////////////////////////////////
    // find                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def find01(): Bool = Chain.find(i -> i > 2, Chain.empty()) == None

    @Test
    def find02(): Bool = Chain.find(i -> i > 2, Chain.singleton(1)) == None

    @Test
    def find03(): Bool = Chain.find(i -> i > 2, Chain.singleton(3)) == Some(3)

    @Test
    def find04(): Bool = Chain.find(i -> i > 2, List.toChain(1 :: 2 :: Nil)) == None

    @Test
    def find05(): Bool = Chain.find(i -> i > 2, List.toChain(6 :: -6 :: Nil)) == Some(6)

    @Test
    def find06(): Bool = Chain.find(i -> i > 2, List.toChain(-6 :: 6 :: Nil)) == Some(6)

    @Test
    def find07(): Bool = Chain.find(i -> i > 2, List.toChain(6 :: 7 :: Nil)) == Some(6)

    /////////////////////////////////////////////////////////////////////////////
    // findLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def findLeft01(): Bool = Chain.findLeft(i -> i > 2, Chain.empty()) == None

    @Test
    def findLeft02(): Bool = Chain.findLeft(i -> i > 2, Chain.singleton(1)) == None

    @Test
    def findLeft03(): Bool = Chain.findLeft(i -> i > 2, Chain.singleton(3)) == Some(3)

    @Test
    def findLeft04(): Bool = Chain.findLeft(i -> i > 2, List.toChain(1 :: 2 :: Nil)) == None

    @Test
    def findLeft05(): Bool = Chain.findLeft(i -> i > 2, List.toChain(6 :: -6 :: Nil)) == Some(6)

    @Test
    def findLeft06(): Bool = Chain.findLeft(i -> i > 2, List.toChain(-6 :: 6 :: Nil)) == Some(6)

    @Test
    def findLeft07(): Bool = Chain.findLeft(i -> i > 2, List.toChain(6 :: 7 :: Nil)) == Some(6)

    /////////////////////////////////////////////////////////////////////////////
    // findRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def findRight01(): Bool = Chain.findRight(i -> i > 2, Chain.empty()) == None

    @Test
    def findRight02(): Bool = Chain.findRight(i -> i > 2, Chain.singleton(1)) == None

    @Test
    def findRight03(): Bool = Chain.findRight(i -> i > 2, Chain.singleton(3)) == Some(3)

    @Test
    def findRight04(): Bool = Chain.findRight(i -> i > 2, List.toChain(1 :: 2 :: Nil)) == None

    @Test
    def findRight05(): Bool = Chain.findRight(i -> i > 2, List.toChain(6 :: -6 :: Nil)) == Some(6)

    @Test
    def findRight06(): Bool = Chain.findRight(i -> i > 2, List.toChain(-6 :: 6 :: Nil)) == Some(6)

    @Test
    def findRight07(): Bool = Chain.findRight(i -> i > 2, List.toChain(6 :: 7 :: Nil)) == Some(7)

    /////////////////////////////////////////////////////////////////////////////
    // range                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def range01(): Bool = Chain.range(1, 0) == Chain.empty()

    @Test
    def range02(): Bool = Chain.range(1, 1) == Chain.empty()

    @Test
    def range03(): Bool = Chain.range(1, 2) == Chain.singleton(1)

    @Test
    def range04(): Bool = Chain.range(1, 3) == List.toChain(1 :: 2 :: Nil)

    @Test
    def range05(): Bool = Chain.range(1, 4) == List.toChain(1 :: 2 :: 3 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // repeat                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def repeat01(): Bool = Chain.repeat(-1, 1) == Chain.empty()

    @Test
    def repeat02(): Bool = Chain.repeat(0, 1) == Chain.empty()

    @Test
    def repeat03(): Bool = Chain.repeat(1, 1) == Chain.singleton(1)

    @Test
    def repeat04(): Bool = Chain.repeat(2, 1) == List.toChain(1 :: 1 :: Nil)

    @Test
    def repeat05(): Bool = Chain.repeat(3, 1) == List.toChain(1 :: 1 :: 1 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // scan                                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def scan01(): Bool = Chain.scan((i, b) -> if (b) i+1 else i+2, 1, Chain.empty()) == Chain.singleton(1)

    @Test
    def scan02(): Bool = Chain.scan((i, b) -> if (b) i+1 else i+2, 1, Chain.singleton(false)) == List.toChain(1 :: 3 :: Nil)

    @Test
    def scan03(): Bool = Chain.scan((i, b) -> if (b) i+1 else i+2, 1, Chain.singleton(true)) == List.toChain(1 :: 2 :: Nil)

    @Test
    def scan04(): Bool = Chain.scan((i, b) -> if (b) i+1 else i+2, 1, List.toChain(false :: false :: Nil)) == List.toChain(1 :: 3 :: 5 :: Nil)

    @Test
    def scan05(): Bool = Chain.scan((i, b) -> if (b) i+1 else i+2, 1, List.toChain(false :: true :: Nil)) == List.toChain(1 :: 3 :: 4 :: Nil)

    @Test
    def scan06(): Bool = Chain.scan((i, b) -> if (b) i+1 else i+2, 1, List.toChain(true :: false :: Nil)) == List.toChain(1 :: 2 :: 4 :: Nil)

    @Test
    def scan07(): Bool = Chain.scan((i, b) -> if (b) i+1 else i+2, 1, List.toChain(true :: true :: Nil)) == List.toChain(1 :: 2 :: 3 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // scanLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def scanLeft01(): Bool = Chain.scanLeft((i, b) -> if (b) i+1 else i+2, 1, Chain.empty()) == Chain.singleton(1)

    @Test
    def scanLeft02(): Bool = Chain.scanLeft((i, b) -> if (b) i+1 else i+2, 1, Chain.singleton(false)) == List.toChain(1 :: 3 :: Nil)

    @Test
    def scanLeft03(): Bool = Chain.scanLeft((i, b) -> if (b) i+1 else i+2, 1, Chain.singleton(true)) == List.toChain(1 :: 2 :: Nil)

    @Test
    def scanLeft04(): Bool = Chain.scanLeft((i, b) -> if (b) i+1 else i+2, 1, List.toChain(false :: false :: Nil)) == List.toChain(1 :: 3 :: 5 :: Nil)

    @Test
    def scanLeft05(): Bool = Chain.scanLeft((i, b) -> if (b) i+1 else i+2, 1, List.toChain(false :: true :: Nil)) == List.toChain(1 :: 3 :: 4 :: Nil)

    @Test
    def scanLeft06(): Bool = Chain.scanLeft((i, b) -> if (b) i+1 else i+2, 1, List.toChain(true :: false :: Nil)) == List.toChain(1 :: 2 :: 4 :: Nil)

    @Test
    def scanLeft07(): Bool = Chain.scanLeft((i, b) -> if (b) i+1 else i+2, 1, List.toChain(true :: true :: Nil)) == List.toChain(1 :: 2 :: 3 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // scanRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def scanRight01(): Bool = Chain.scanRight((b, i) -> if (b) i+1 else i+2, 1, Chain.empty()) == Chain.singleton(1)

    @Test
    def scanRight02(): Bool = Chain.scanRight((b, i) -> if (b) i+1 else i+2, 1, Chain.singleton(false)) == List.toChain(3 :: 1 :: Nil)

    @Test
    def scanRight03(): Bool = Chain.scanRight((b, i) -> if (b) i+1 else i+2, 1, Chain.singleton(true)) == List.toChain(2 :: 1 :: Nil)

    @Test
    def scanRight04(): Bool = Chain.scanRight((b, i) -> if (b) i+1 else i+2, 1, List.toChain(false :: false :: Nil)) == List.toChain(5 :: 3 :: 1 :: Nil)

    @Test
    def scanRight05(): Bool = Chain.scanRight((b, i) -> if (b) i+1 else i+2, 1, List.toChain(false :: true :: Nil)) == List.toChain(4 :: 2 :: 1 :: Nil)

    @Test
    def scanRight06(): Bool = Chain.scanRight((b, i) -> if (b) i+1 else i+2, 1, List.toChain(true :: false :: Nil)) == List.toChain(4 :: 3 :: 1 :: Nil)

    @Test
    def scanRight07(): Bool = Chain.scanRight((b, i) -> if (b) i+1 else i+2, 1, List.toChain(true :: true :: Nil)) == List.toChain(3 :: 2 :: 1 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // map                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def map01(): Bool = Chain.map(i -> i > 2, Chain.empty()) == Chain.empty()

    @Test
    def map02(): Bool = Chain.map(i -> i > 2, Chain.singleton(1)) == Chain.singleton(false)

    @Test
    def map03(): Bool = Chain.map(i -> i > 2, Chain.singleton(3)) == Chain.singleton(true)

    @Test
    def map04(): Bool = Chain.map(i -> i > 2, List.toChain(1 :: 2 :: Nil)) == List.toChain(false :: false :: Nil)

    @Test
    def map05(): Bool = Chain.map(i -> i > 2, List.toChain(1 :: 8 :: Nil)) == List.toChain(false :: true :: Nil)

    @Test
    def map06(): Bool = Chain.map(i -> i > 2, List.toChain(8 :: 1 :: Nil)) == List.toChain(true :: false :: Nil)

    @Test
    def map07(): Bool = Chain.map(i -> i > 2, List.toChain(7 :: 8 :: Nil)) == List.toChain(true :: true :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // mapWithIndex                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def mapWithIndex01(): Bool = Chain.mapWithIndex((i, e) -> if (i < 1) e > 2 else e <= 2, Chain.empty()) == Chain.empty()

    @Test
    def mapWithIndex02(): Bool = Chain.mapWithIndex((i, e) -> if (i < 1) e > 2 else e <= 2, Chain.singleton(1)) == Chain.singleton(false)

    @Test
    def mapWithIndex03(): Bool = Chain.mapWithIndex((i, e) -> if (i < 1) e > 2 else e <= 2, Chain.singleton(3)) == Chain.singleton(true)

    @Test
    def mapWithIndex04(): Bool = Chain.mapWithIndex((i, e) -> if (i < 1) e > 2 else e <= 2, List.toChain(1 :: 2 :: Nil)) == List.toChain(false :: true :: Nil)

    @Test
    def mapWithIndex05(): Bool = Chain.mapWithIndex((i, e) -> if (i < 1) e > 2 else e <= 2, List.toChain(1 :: 8 :: Nil)) == List.toChain(false :: false :: Nil)

    @Test
    def mapWithIndex06(): Bool = Chain.mapWithIndex((i, e) -> if (i < 1) e > 2 else e <= 2, List.toChain(8 :: 1 :: Nil)) == List.toChain(true :: true :: Nil)

    @Test
    def mapWithIndex07(): Bool = Chain.mapWithIndex((i, e) -> if (i < 1) e > 2 else e <= 2, List.toChain(7 :: 8 :: Nil)) == List.toChain(true :: false :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // flatMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def flatMap01(): Bool = Chain.flatMap(i -> Chain.repeat(i, i), Chain.empty()) == Chain.empty()

    @Test
    def flatMap02(): Bool = Chain.flatMap(i -> Chain.repeat(i, i), Chain.singleton(0)) == Chain.empty()

    @Test
    def flatMap03(): Bool = Chain.flatMap(i -> Chain.repeat(i, i), Chain.singleton(1)) == Chain.singleton(1)

    @Test
    def flatMap04(): Bool = Chain.flatMap(i -> Chain.repeat(i, i), Chain.singleton(2)) == List.toChain(2 :: 2 :: Nil)

    @Test
    def flatMap05(): Bool = Chain.flatMap(i -> Chain.repeat(i, i), List.toChain(1 :: 2 :: Nil)) == List.toChain(1 :: 2 :: 2 :: Nil)

    @Test
    def flatMap06(): Bool = Chain.flatMap(i -> Chain.repeat(i, i), List.toChain(2 :: 3 :: Nil)) == List.toChain(2 :: 2 :: 3 :: 3 :: 3 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // reverse                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def reverse01(): Bool = Chain.reverse(Chain.empty(): Chain[Unit]) == Chain.empty()

    @Test
    def reverse02(): Bool = Chain.reverse(Chain.singleton(1)) == Chain.singleton(1)

    @Test
    def reverse03(): Bool = Chain.reverse(List.toChain(1 :: 2 :: Nil)) == List.toChain(2 :: 1 :: Nil)

    @Test
    def reverse04(): Bool = Chain.reverse(List.toChain(1 :: 1 :: Nil)) == List.toChain(1 :: 1 :: Nil)

    @Test
    def reverse05(): Bool = Chain.reverse(List.toChain(1 :: 2 :: 3 :: Nil)) == List.toChain(3 :: 2 :: 1 :: Nil)

    @Test
    def reverse06(): Bool = Chain.reverse(List.toChain(1 :: 2 :: 3 :: 4 :: Nil)) == List.toChain(4 :: 3 :: 2 :: 1 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // intersperse                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def intersperse01(): Bool = Chain.intersperse(11, Chain.empty()) == Chain.empty()

    @Test
    def intersperse02(): Bool = Chain.intersperse(11, Chain.singleton(1)) == Chain.singleton(1)

    @Test
    def intersperse03(): Bool = Chain.intersperse(11, List.toChain(1 :: 2 :: Nil)) == List.toChain(1 :: 11 :: 2 :: Nil)

    @Test
    def intersperse04(): Bool = Chain.intersperse(11, List.toChain(1 :: 2 :: 3 :: Nil)) == List.toChain(1 :: 11 :: 2 :: 11 :: 3 :: Nil)

    @Test
    def intersperse05(): Bool = Chain.intersperse(11, List.toChain(1 :: 2 :: 3 :: 4 :: Nil)) == List.toChain(1 :: 11 :: 2 :: 11 :: 3 :: 11 :: 4 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // count                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def count01(): Bool = Chain.count(i -> i > 3, Chain.empty()) == 0

    @Test
    def count02(): Bool = Chain.count(i -> i > 3, Chain.singleton(1)) == 0

    @Test
    def count03(): Bool = Chain.count(i -> i > 3, Chain.singleton(4)) == 1

    @Test
    def count04(): Bool = Chain.count(i -> i > 3, List.toChain(1 :: 2 :: Nil)) == 0

    @Test
    def count05(): Bool = Chain.count(i -> i > 3, List.toChain(1 :: 8 :: Nil)) == 1

    @Test
    def count06(): Bool = Chain.count(i -> i > 3, List.toChain(8 :: 1 :: Nil)) == 1

    @Test
    def count07(): Bool = Chain.count(i -> i > 3, List.toChain(6 :: 7 :: Nil)) == 2


    /////////////////////////////////////////////////////////////////////////////
    // sum                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sum01(): Bool =
        List.toChain(Nil) |> Chain.sum == 0

    @Test
    def sum02(): Bool =
        List.toChain(1 :: Nil) |> Chain.sum == 1

    @Test
    def sum03(): Bool =
        List.toChain(1 :: 2 :: 3 :: Nil) |> Chain.sum == 6

    @Test
    def sum04(): Bool =
        List.toChain(1 :: 2 :: 3 :: -3 :: Nil) |> Chain.sum == 3

    @Test
    def sum05(): Bool =
        List.toChain(-1 :: -2 :: -3 :: -4 :: Nil) |> Chain.sum == -10

    @Test
    def sum06(): Bool =
        List.toChain(10 :: -10 :: Nil) |> Chain.sum == 0

    @Test
    def sum07(): Bool =
        List.range(1, 101) |> List.toChain |> Chain.sum == 5050


    /////////////////////////////////////////////////////////////////////////////
    // sumWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sumWith01(): Bool =
        List.toChain(Nil) |> Chain.sumWith(x -> x + 1) == 0

    @Test
    def sumWith02(): Bool =
        List.toChain(1 :: Nil) |> Chain.sumWith(x -> x + 1) == 2

    @Test
    def sumWith03(): Bool =
        List.toChain(1 :: 2 :: 3 :: Nil) |> Chain.sumWith(x -> x + 1) == 9

    @Test
    def sumWith04(): Bool =
        List.toChain(1 :: 2 :: 3 :: -3 :: Nil) |> Chain.sumWith(x -> x + 1) == 7

    @Test
    def sumWith05(): Bool =
        List.toChain(-1 :: -2 :: -3 :: -4 :: Nil) |> Chain.sumWith(x -> x + 1) == -6

    @Test
    def sumWith06(): Bool =
        List.toChain(10 :: -10 :: Nil) |> Chain.sumWith(x -> x + 1) == 2


    /////////////////////////////////////////////////////////////////////////////
    // flatten                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def flatten01(): Bool = Chain.flatten(Chain.empty(): Chain[Chain[Unit]]) == Chain.empty()

    @Test
    def flatten02(): Bool = Chain.flatten(Chain.singleton(Chain.empty())): Chain[Unit] == Chain.empty()

    @Test
    def flatten03(): Bool = Chain.flatten(Chain.singleton(Chain.singleton(1))) == Chain.singleton(1)

    @Test
    def flatten04(): Bool = Chain.flatten(Chain.singleton(List.toChain(1 :: 2 :: Nil))) == List.toChain(1 :: 2 :: Nil)

    @Test
    def flatten05(): Bool = Chain.flatten(List.toChain(Chain.empty() :: Chain.empty() :: Nil)): Chain[Unit] == Chain.empty()

    @Test
    def flatten06(): Bool = Chain.flatten(List.toChain(Chain.singleton(1) :: Chain.empty() :: Nil)) == Chain.singleton(1)

    @Test
    def flatten07(): Bool = Chain.flatten(List.toChain(Chain.empty() :: Chain.singleton(1) :: Nil)) == Chain.singleton(1)

    @Test
    def flatten08(): Bool = Chain.flatten(List.toChain(Chain.singleton(1) :: Chain.singleton(2) :: Nil)) == List.toChain(1 :: 2 :: Nil)

    @Test
    def flatten09(): Bool = Chain.flatten(List.toChain(List.toChain(1 :: 2 :: Nil) :: List.toChain(3 :: 4 :: 5 :: Nil) :: Nil)) == List.toChain(1 :: 2 :: 3 :: 4 :: 5 :: Nil)

    @Test
    def flatten10(): Bool = Chain.flatten(List.toChain(Chain.singleton(1) :: List.toChain(2 :: 3 :: Nil) :: Chain.singleton(4) :: Nil)) == List.toChain(1 :: 2 :: 3 :: 4 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // exists                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def exists01(): Bool = Chain.exists(i -> i > 3, Chain.empty()) == false

    @Test
    def exists02(): Bool = Chain.exists(i -> i > 3, Chain.singleton(1)) == false

    @Test
    def exists03(): Bool = Chain.exists(i -> i > 3, Chain.singleton(5)) == true

    @Test
    def exists04(): Bool = Chain.exists(i -> i > 3, List.toChain(1 :: 2 :: Nil)) == false

    @Test
    def exists05(): Bool = Chain.exists(i -> i > 3, List.toChain(1 :: 6 :: Nil)) == true

    @Test
    def exists06(): Bool = Chain.exists(i -> i > 3, List.toChain(6 :: 1 :: Nil)) == true

    @Test
    def exists07(): Bool = Chain.exists(i -> i > 3, List.toChain(16 :: 6 :: Nil)) == true

    @Test
    def exists08(): Bool = Chain.exists(i -> i > 3, List.toChain(1 :: -9 :: 3 :: Nil)) == false

    @Test
    def exists09(): Bool = Chain.exists(i -> i > 3, List.toChain(1 :: 9 :: 3 :: Nil)) == true

    /////////////////////////////////////////////////////////////////////////////
    // forAll                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forAll01(): Bool = Chain.forAll(i -> i > 3, Chain.empty()) == true

    @Test
    def forAll02(): Bool = Chain.forAll(i -> i > 3, Chain.singleton(1)) == false

    @Test
    def forAll03(): Bool = Chain.forAll(i -> i > 3, Chain.singleton(5)) == true

    @Test
    def forAll04(): Bool = Chain.forAll(i -> i > 3, List.toChain(1 :: 2 :: Nil)) == false

    @Test
    def forAll05(): Bool = Chain.forAll(i -> i > 3, List.toChain(1 :: 6 :: Nil)) == false

    @Test
    def forAll06(): Bool = Chain.forAll(i -> i > 3, List.toChain(6 :: 1 :: Nil)) == false

    @Test
    def forAll07(): Bool = Chain.forAll(i -> i > 3, List.toChain(16 :: 6 :: Nil)) == true

    @Test
    def forAll08(): Bool = Chain.forAll(i -> i > 3, List.toChain(1 :: -9 :: 3 :: Nil)) == false

    @Test
    def forAll09(): Bool = Chain.forAll(i -> i > 3, List.toChain(1 :: 9 :: 3 :: Nil)) == false

    @Test
    def forAll10(): Bool = Chain.forAll(i -> i > 3, List.toChain(11 :: 9 :: 31 :: Nil)) == true

    /////////////////////////////////////////////////////////////////////////////
    // filter                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def filter01(): Bool = Chain.filter(i -> i > 3, Chain.empty()) == Chain.empty()

    @Test
    def filter02(): Bool = Chain.filter(i -> i > 3, Chain.singleton(2)) == Chain.empty()

    @Test
    def filter03(): Bool = Chain.filter(i -> i > 3, Chain.singleton(4)) == Chain.singleton(4)

    @Test
    def filter04(): Bool = Chain.filter(i -> i > 3, List.toChain(1 :: 3 :: Nil)) == Chain.empty()

    @Test
    def filter05(): Bool = Chain.filter(i -> i > 3, List.toChain(1 :: 8 :: Nil)) == Chain.singleton(8)

    @Test
    def filter06(): Bool = Chain.filter(i -> i > 3, List.toChain(8 :: 1 :: Nil)) == Chain.singleton(8)

    @Test
    def filter07(): Bool = Chain.filter(i -> i > 3, List.toChain(8 :: 9 :: Nil)) == List.toChain(8 :: 9 :: Nil)

    @Test
    def filter08(): Bool = Chain.filter(i -> i > 3, List.toChain(1 :: 4 :: 11 :: 2 :: -22 :: 17 :: Nil)) == List.toChain(4 :: 11 :: 17 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // foldLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldLeft01(): Bool = Chain.foldLeft((i, e) -> (i - e)*(e `Int32.rem` 2 + 1), 100, Chain.empty()) == 100

    @Test
    def foldLeft02(): Bool = Chain.foldLeft((i, e) -> (i - e)*(e `Int32.rem` 2 + 1), 100, Chain.singleton(1)) == 198

    @Test
    def foldLeft03(): Bool = Chain.foldLeft((i, e) -> (i - e)*(e `Int32.rem` 2 + 1), 100, List.toChain(1 :: 2 :: Nil)) == 196

    @Test
    def foldLeft04(): Bool = Chain.foldLeft((i, e) -> (i - e)*(e `Int32.rem` 2 + 1), 100, List.toChain(1 :: 2 :: 3 :: Nil)) == 386

    /////////////////////////////////////////////////////////////////////////////
    // foldRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldRight01(): Bool = Chain.foldRight((e, acc) -> (acc - e) * (e `Int32.rem` 2 + 1), 100, Chain.empty()) == 100

    @Test
    def foldRight02(): Bool = Chain.foldRight((e, acc) -> (acc - e) * (e `Int32.rem` 2 + 1), 100, Chain.singleton(1)) == 198

    @Test
    def foldRight03(): Bool = Chain.foldRight((e, acc) -> (acc - e) * (e `Int32.rem` 2 + 1), 100, List.toChain(1 :: 2 :: Nil)) == 194

    @Test
    def foldRight04(): Bool = Chain.foldRight((e, acc) -> (acc - e) * (e `Int32.rem` 2 + 1), 100, List.toChain(1 :: 2 :: 3 :: Nil)) == 382

    /////////////////////////////////////////////////////////////////////////////
    // foldRightWithCont                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldRightWithCont01(): Bool = Chain.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.rem` 2 + 1), 100, Chain.empty()) == 100

    @Test
    def foldRightWithCont02(): Bool = Chain.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.rem` 2 + 1), 100, Chain.singleton(1)) == 198

    @Test
    def foldRightWithCont03(): Bool = Chain.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.rem` 2 + 1), 100, List.toChain(1 :: 2 :: Nil)) == 194

    @Test
    def foldRightWithCont04(): Bool = Chain.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.rem` 2 + 1), 100, List.toChain(1 :: 2 :: 3 :: Nil)) == 382

    /////////////////////////////////////////////////////////////////////////////
    // foldMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldMap01(): Bool =
        Chain.foldMap(x -> 2 * x, List.toChain(Nil)) == 0

    @Test
    def foldMap02(): Bool =
        Chain.foldMap(x -> 2 * x, List.toChain(1 :: 2 :: Nil)) == 6

    @Test
    def foldMap03(): Bool =
        Chain.foldMap(x -> if(x == "a") "b" else x, List.toChain("a" :: Nil)) == "b"

    @Test
    def foldMap04(): Bool =
        Chain.foldMap(x -> if(x == "c") "b" else x, List.toChain("a" :: "b" :: "c" :: Nil)) == "abb"

    @Test
    def foldMap05(): Bool =
        Chain.foldMap(Int32.toString, List.toChain(1 :: 2 :: 3 :: Nil)) == "123"

    /////////////////////////////////////////////////////////////////////////////
    // dropLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def dropLeft01(): Bool = Chain.dropLeft(-1, Chain.empty(): Chain[Unit]) == Chain.empty()

    @Test
    def dropLeft02(): Bool = Chain.dropLeft(0, Chain.empty(): Chain[Unit]) == Chain.empty()

    @Test
    def dropLeft03(): Bool = Chain.dropLeft(1, Chain.empty(): Chain[Unit]) == Chain.empty()

    @Test
    def dropLeft04(): Bool = Chain.dropLeft(-1, Chain.singleton(1)) == Chain.singleton(1)

    @Test
    def dropLeft05(): Bool = Chain.dropLeft(0, Chain.singleton(1)) == Chain.singleton(1)

    @Test
    def dropLeft06(): Bool = Chain.dropLeft(1, Chain.singleton(1)) == Chain.empty()

    @Test
    def dropLeft07(): Bool = Chain.dropLeft(2, Chain.singleton(1)) == Chain.empty()

    @Test
    def dropLeft08(): Bool = Chain.dropLeft(0, List.toChain(1 :: 2 :: Nil)) == List.toChain(1 :: 2 :: Nil)

    @Test
    def dropLeft09(): Bool = Chain.dropLeft(1, List.toChain(1 :: 2 :: Nil)) == Chain.singleton(2)

    @Test
    def dropLeft10(): Bool = Chain.dropLeft(2, List.toChain(1 :: 2 :: Nil)) == Chain.empty()

    @Test
    def dropLeft11(): Bool = Chain.dropLeft(2, List.toChain(1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil)) == List.toChain(3 :: 4 :: 5 :: 6 :: Nil)

    @Test
    def dropLeft12(): Bool = Chain.dropLeft(4, List.toChain(1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil)) == List.toChain(5 :: 6 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // dropRight                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def dropRight01(): Bool = Chain.dropRight(-1, Chain.empty(): Chain[Unit]) == Chain.empty()

    @Test
    def dropRight02(): Bool = Chain.dropRight(0, Chain.empty(): Chain[Unit]) == Chain.empty()

    @Test
    def dropRight03(): Bool = Chain.dropRight(1, Chain.empty(): Chain[Unit]) == Chain.empty()

    @Test
    def dropRight04(): Bool = Chain.dropRight(-1, Chain.singleton(1)) == Chain.singleton(1)

    @Test
    def dropRight05(): Bool = Chain.dropRight(0, Chain.singleton(1)) == Chain.singleton(1)

    @Test
    def dropRight06(): Bool = Chain.dropRight(1, Chain.singleton(1)) == Chain.empty()

    @Test
    def dropRight07(): Bool = Chain.dropRight(2, Chain.singleton(1)) == Chain.empty()

    @Test
    def dropRight08(): Bool = Chain.dropRight(0, List.toChain(1 :: 2 :: Nil)) == List.toChain(1 :: 2 :: Nil)

    @Test
    def dropRight09(): Bool = Chain.dropRight(1, List.toChain(1 :: 2 :: Nil)) == Chain.singleton(1)

    @Test
    def dropRight10(): Bool = Chain.dropRight(2, List.toChain(1 :: 2 :: Nil)) == Chain.empty()

    @Test
    def dropRight11(): Bool = Chain.dropRight(2, List.toChain(1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil)) == List.toChain(1 :: 2 :: 3 :: 4 :: Nil)

    @Test
    def dropRight12(): Bool = Chain.dropRight(4, List.toChain(1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil)) == List.toChain(1 :: 2 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // dropWhileLeft                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def dropWhileLeft01(): Bool = Chain.dropWhileLeft(i -> i > 3, Chain.empty()) == Chain.empty()

    @Test
    def dropWhileLeft02(): Bool = Chain.dropWhileLeft(i -> i > 3, Chain.singleton(1)) == Chain.singleton(1)

    @Test
    def dropWhileLeft03(): Bool = Chain.dropWhileLeft(i -> i > 3, Chain.singleton(4)) == Chain.empty()

    @Test
    def dropWhileLeft04(): Bool = Chain.dropWhileLeft(i -> i > 3, List.toChain(1 :: 2 :: Nil)) == List.toChain(1 :: 2 :: Nil)

    @Test
    def dropWhileLeft05(): Bool = Chain.dropWhileLeft(i -> i > 3, List.toChain(1 :: 5 :: Nil)) == List.toChain(1 :: 5 :: Nil)

    @Test
    def dropWhileLeft06(): Bool = Chain.dropWhileLeft(i -> i > 3, List.toChain(5 :: 1 :: Nil)) == Chain.singleton(1)

    @Test
    def dropWhileLeft07(): Bool = Chain.dropWhileLeft(i -> i > 3, List.toChain(5 :: 8 :: Nil)) == Chain.empty()

    @Test
    def dropWhileLeft08(): Bool = Chain.dropWhileLeft(i -> i > 3, List.toChain(4 :: 6 :: -3 :: 11 :: -5 :: 1 :: 2 :: 16 :: 7 :: 1 :: 7 :: Nil)) ==
                            List.toChain(-3 :: 11 :: -5 :: 1 :: 2 :: 16 :: 7 :: 1 :: 7 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // dropWhileRight                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def dropWhileRight01(): Bool = Chain.dropWhileRight(i -> i > 3, Chain.empty()) == Chain.empty()

    @Test
    def dropWhileRight02(): Bool = Chain.dropWhileRight(i -> i > 3, Chain.singleton(1)) == Chain.singleton(1)

    @Test
    def dropWhileRight03(): Bool = Chain.dropWhileRight(i -> i > 3, Chain.singleton(4)) == Chain.empty()

    @Test
    def dropWhileRight04(): Bool = Chain.dropWhileRight(i -> i > 3, List.toChain(1 :: 2 :: Nil)) == List.toChain(1 :: 2 :: Nil)

    @Test
    def dropWhileRight05(): Bool = Chain.dropWhileRight(i -> i > 3, List.toChain(1 :: 5 :: Nil)) == Chain.singleton(1)

    @Test
    def dropWhileRight06(): Bool = Chain.dropWhileRight(i -> i > 3, List.toChain(5 :: 1 :: Nil)) == List.toChain(5 :: 1 :: Nil)

    @Test
    def dropWhileRight07(): Bool = Chain.dropWhileRight(i -> i > 3, List.toChain(5 :: 8 :: Nil)) == Chain.empty()

    @Test
    def dropWhileRight08(): Bool = Chain.dropWhileRight(i -> i > 3, List.toChain(4 :: 6 :: -3 :: 11 :: -5 :: 1 :: 2 :: 16 :: 7 :: 1 :: 7 :: Nil)) ==
                            List.toChain(4 :: 6 :: -3 :: 11 :: -5 :: 1 :: 2 :: 16 :: 7 :: 1 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // takeLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def takeLeft01(): Bool = Chain.takeLeft(-1, Chain.empty(): Chain[Unit]) == Chain.empty()

    @Test
    def takeLeft02(): Bool = Chain.takeLeft(0, Chain.empty(): Chain[Unit]) == Chain.empty()

    @Test
    def takeLeft03(): Bool = Chain.takeLeft(1, Chain.empty(): Chain[Unit]) == Chain.empty()

    @Test
    def takeLeft04(): Bool = Chain.takeLeft(-1, Chain.singleton(1)) == Chain.empty()

    @Test
    def takeLeft05(): Bool = Chain.takeLeft(0, Chain.singleton(1)) == Chain.empty()

    @Test
    def takeLeft06(): Bool = Chain.takeLeft(1, Chain.singleton(1)) == Chain.singleton(1)

    @Test
    def takeLeft07(): Bool = Chain.takeLeft(2, Chain.singleton(1)) == Chain.singleton(1)

    @Test
    def takeLeft08(): Bool = Chain.takeLeft(0, List.toChain(1 :: 2 :: Nil)) == Chain.empty()

    @Test
    def takeLeft09(): Bool = Chain.takeLeft(1, List.toChain(1 :: 2 :: Nil)) == Chain.singleton(1)

    @Test
    def takeLeft10(): Bool = Chain.takeLeft(2, List.toChain(1 :: 2 :: Nil)) == List.toChain(1 :: 2 :: Nil)

    @Test
    def takeLeft11(): Bool = Chain.takeLeft(2, List.toChain(1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil)) == List.toChain(1 :: 2 :: Nil)

    @Test
    def takeLeft12(): Bool = Chain.takeLeft(4, List.toChain(1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil)) == List.toChain(1 :: 2 :: 3 :: 4 :: Nil)


    /////////////////////////////////////////////////////////////////////////////
    // takeRight                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def takeRight01(): Bool = Chain.takeRight(-1, Chain.empty(): Chain[Unit]) == Chain.empty()

    @Test
    def takeRight02(): Bool = Chain.takeRight(0, Chain.empty(): Chain[Unit]) == Chain.empty()

    @Test
    def takeRight03(): Bool = Chain.takeRight(1, Chain.empty(): Chain[Unit]) == Chain.empty()

    @Test
    def takeRight04(): Bool = Chain.takeRight(-1, Chain.singleton(1)) == Chain.empty()

    @Test
    def takeRight05(): Bool = Chain.takeRight(0, Chain.singleton(1)) == Chain.empty()

    @Test
    def takeRight06(): Bool = Chain.takeRight(1, Chain.singleton(1)) == Chain.singleton(1)

    @Test
    def takeRight07(): Bool = Chain.takeRight(2, Chain.singleton(1)) == Chain.singleton(1)

    @Test
    def takeRight08(): Bool = Chain.takeRight(0, List.toChain(1 :: 2 :: Nil)) == Chain.empty()

    @Test
    def takeRight09(): Bool = Chain.takeRight(1, List.toChain(1 :: 2 :: Nil)) == Chain.singleton(2)

    @Test
    def takeRight10(): Bool = Chain.takeRight(2, List.toChain(1 :: 2 :: Nil)) == List.toChain(1 :: 2 :: Nil)

    @Test
    def takeRight11(): Bool = Chain.takeRight(2, List.toChain(1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil)) == List.toChain(5 :: 6 :: Nil)

    @Test
    def takeRight12(): Bool = Chain.takeRight(4, List.toChain(1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil)) == List.toChain(3 :: 4 :: 5 :: 6 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // takeWhileLeft                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def takeWhileLeft01(): Bool = Chain.takeWhileLeft(i -> i > 3, Chain.empty(): Chain[Int32]) == Chain.empty()

    @Test
    def takeWhileLeft02(): Bool = Chain.takeWhileLeft(i -> i > 3, Chain.singleton(1)) == Chain.empty()

    @Test
    def takeWhileLeft03(): Bool = Chain.takeWhileLeft(i -> i > 3, Chain.singleton(4)) == Chain.singleton(4)

    @Test
    def takeWhileLeft04(): Bool = Chain.takeWhileLeft(i -> i > 3, List.toChain(1 :: 2 :: Nil)) == Chain.empty()

    @Test
    def takeWhileLeft05(): Bool = Chain.takeWhileLeft(i -> i > 3, List.toChain(1 :: 5 :: Nil)) == Chain.empty()

    @Test
    def takeWhileLeft06(): Bool = Chain.takeWhileLeft(i -> i > 3, List.toChain(5 :: 1 :: Nil)) == Chain.singleton(5)

    @Test
    def takeWhileLeft07(): Bool = Chain.takeWhileLeft(i -> i > 3, List.toChain(5 :: 8 :: Nil)) == List.toChain(5 :: 8 :: Nil)

    @Test
    def takeWhileLeft08(): Bool = Chain.takeWhileLeft(i -> i > 3, List.toChain(4 :: 6 :: -3 :: 11 :: -5 :: 1 :: 2 :: 16 :: 7 :: 1 :: 7 :: Nil)) ==
                            List.toChain(4 :: 6 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // takeWhileRight                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def takeWhileRight01(): Bool = Chain.takeWhileRight(i -> i > 3, Chain.empty(): Chain[Int32]) == Chain.empty()

    @Test
    def takeWhileRight02(): Bool = Chain.takeWhileRight(i -> i > 3, Chain.singleton(1)) == Chain.empty()

    @Test
    def takeWhileRight03(): Bool = Chain.takeWhileRight(i -> i > 3, Chain.singleton(4)) == Chain.singleton(4)

    @Test
    def takeWhileRight04(): Bool = Chain.takeWhileRight(i -> i > 3, List.toChain(1 :: 2 :: Nil)) == Chain.empty()

    @Test
    def takeWhileRight05(): Bool = Chain.takeWhileRight(i -> i > 3, List.toChain(1 :: 5 :: Nil)) == Chain.singleton(5)

    @Test
    def takeWhileRight06(): Bool = Chain.takeWhileRight(i -> i > 3, List.toChain(5 :: 1 :: Nil)) == Chain.empty()

    @Test
    def takeWhileRight07(): Bool = Chain.takeWhileRight(i -> i > 3, List.toChain(5 :: 8 :: Nil)) == List.toChain(5 :: 8 :: Nil)

    @Test
    def takeWhileRight08(): Bool = Chain.takeWhileRight(i -> i > 3, List.toChain(4 :: 6 :: -3 :: 11 :: -5 :: 1 :: 2 :: 16 :: 7 :: 1 :: 7 :: Nil)) ==
                            Chain.singleton(7)

    /////////////////////////////////////////////////////////////////////////////
    // filterMap                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def filterMap01(): Bool = Chain.filterMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Chain.empty()) == Chain.empty()

    @Test
    def filterMap02(): Bool = Chain.filterMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Chain.singleton(1)) == Chain.empty()

    @Test
    def filterMap03(): Bool = Chain.filterMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Chain.singleton(2)) == Chain.singleton(1)

    @Test
    def filterMap04(): Bool = Chain.filterMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, List.toChain(1 :: 3 :: Nil)) == Chain.empty()

    @Test
    def filterMap05(): Bool = Chain.filterMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, List.toChain(1 :: 4 :: Nil)) == Chain.singleton(2)

    @Test
    def filterMap06(): Bool = Chain.filterMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, List.toChain(6 :: -1 :: Nil)) == Chain.singleton(3)

    @Test
    def filterMap07(): Bool = Chain.filterMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, List.toChain(8 :: 6 :: Nil)) == List.toChain(4 :: 3 :: Nil)

    @Test
    def filterMap08(): Bool = Chain.filterMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, List.toChain(0 :: 1 :: 2 :: 3 :: 4 :: 5 :: 10 :: Nil)) ==
                            List.toChain(0 :: 1 :: 2 :: 5 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // findMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def findMap01(): Bool = Chain.findMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Chain.empty()) == None

    @Test
    def findMap02(): Bool = Chain.findMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Chain.singleton(1)) == None

    @Test
    def findMap03(): Bool = Chain.findMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Chain.singleton(2)) == Some(1)

    @Test
    def findMap04(): Bool = Chain.findMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, List.toChain(1 :: 3 :: Nil)) == None

    @Test
    def findMap05(): Bool = Chain.findMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, List.toChain(1 :: 4 :: Nil)) == Some(2)

    @Test
    def findMap06(): Bool = Chain.findMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, List.toChain(6 :: -1 :: Nil)) == Some(3)

    @Test
    def findMap07(): Bool = Chain.findMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, List.toChain(8 :: 6 :: Nil)) == Some(4)

    @Test
    def findMap08(): Bool = Chain.findMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, List.toChain(0 :: 1 :: 2 :: 3 :: 4 :: 5 :: 10 :: Nil)) == Some(0)

    /////////////////////////////////////////////////////////////////////////////
    // zip                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def zip01(): Bool = Chain.zip(Chain.empty(): Chain[Unit], Chain.empty(): Chain[Unit]) == Chain.empty()

    @Test
    def zip02(): Bool = Chain.zip(Chain.singleton(1), Chain.empty(): Chain[Unit]) == Chain.empty()

    @Test
    def zip03(): Bool = Chain.zip(Chain.empty(): Chain[Unit], Chain.singleton(2)) == Chain.empty()

    @Test
    def zip04(): Bool = Chain.zip(Chain.singleton(1), Chain.singleton(2)) == Chain.singleton((1, 2))

    @Test
    def zip05(): Bool = Chain.zip(List.toChain(1 :: 3 :: Nil), List.toChain(2 :: 4 :: Nil)) == List.toChain((1, 2) :: (3, 4) :: Nil)

    @Test
    def zip06(): Bool = Chain.zip(List.toChain(1 :: 3 :: 5 :: Nil), List.toChain(2 :: 4 :: 6 :: Nil)) == List.toChain((1, 2) :: (3, 4) :: (5, 6) :: Nil)

    @Test
    def zip07(): Bool = Chain.zip(List.toChain(1 :: 3 :: 5 :: 7 :: Nil), List.toChain(2 :: 4 :: 6 :: 8 :: Nil)) == List.toChain((1, 2) :: (3, 4) :: (5, 6) :: (7, 8) :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // zipWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def zipWith01(): Bool = Chain.zipWith((a, b) -> if (b) a+1 else a, Chain.empty(), Chain.empty()) == Chain.empty()

    @Test
    def zipWith02(): Bool = Chain.zipWith((a, b) -> if (b) a+1 else a, Chain.singleton(1), Chain.empty()) == Chain.empty()

    @Test
    def zipWith03(): Bool = Chain.zipWith((a, b) -> if (b) a+1 else a, Chain.empty(), Chain.singleton(true)) == Chain.empty()

    @Test
    def zipWith04(): Bool = Chain.zipWith((a, b) -> if (b) a+1 else a, Chain.singleton(1), Chain.singleton(true)) == Chain.singleton(2)

    @Test
    def zipWith05(): Bool = Chain.zipWith((a, b) -> if (b) a+1 else a, Chain.singleton(1), Chain.singleton(false)) == Chain.singleton(1)

    @Test
    def zipWith06(): Bool = Chain.zipWith((a, b) -> if (b) a+1 else a, List.toChain(1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: Nil),
                          List.toChain(false :: true :: true :: false :: false :: true :: true :: true :: Nil)) ==
                          List.toChain(1 :: 3 :: 4 :: 4 :: 5 :: 7 :: 8 :: 9 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // zipWithA                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def zipWithA01(): Bool = Chain.zipWithA((a, b) -> {if (b) a+1 else a} |> Identity.Identity, Chain.empty(), Chain.empty()) == Identity.Identity(Chain.empty())

    @Test
    def zipWithA02(): Bool = Chain.zipWithA((a, b) -> {if (b) a+1 else a} |> Identity.Identity, Chain.singleton(1), Chain.empty()) == Identity.Identity(Chain.empty())

    @Test
    def zipWithA03(): Bool = Chain.zipWithA((a, b) -> {if (b) a+1 else a} |> Identity.Identity, Chain.empty(), Chain.singleton(true)) == Identity.Identity(Chain.empty())

    @Test
    def zipWithA04(): Bool = Chain.zipWithA((a, b) -> {if (b) a+1 else a} |> Identity.Identity, Chain.singleton(1), Chain.singleton(true)) == Identity.Identity(Chain.singleton(2))

    @Test
    def zipWithA05(): Bool = Chain.zipWithA((a, b) -> {if (b) a+1 else a} |> Identity.Identity, Chain.singleton(1), Chain.singleton(false)) == Identity.Identity(Chain.singleton(1))

    @Test
    def zipWith06A(): Bool = Chain.zipWithA((a, b) -> {if (b) a+1 else a} |> Identity.Identity, List.toChain(1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: Nil),
                          List.toChain(false :: true :: true :: false :: false :: true :: true :: true :: Nil)) ==
                          Identity.Identity(List.toChain(1 :: 3 :: 4 :: 4 :: 5 :: 7 :: 8 :: 9 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // unzip                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def unzip01(): Bool = Chain.unzip(Chain.empty(): Chain[(Unit, Unit)]) == (Chain.empty(), Chain.empty())

    @Test
    def unzip02(): Bool = Chain.unzip(Chain.singleton((1, true))) == (Chain.singleton(1), Chain.singleton(true))

    @Test
    def unzip03(): Bool = Chain.unzip(List.toChain((1, true) :: (2, true) :: Nil)) == (List.toChain(1 :: 2 :: Nil), List.toChain(true :: true :: Nil))

    @Test
    def unzip04(): Bool = Chain.unzip(List.toChain((1, true) :: (2, true) :: (3, false) :: Nil)) ==
                        (List.toChain(1 :: 2 :: 3 :: Nil), List.toChain(true :: true :: false :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // mapAccumLeft                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def mapAccumLeft01(): Bool = Chain.mapAccumLeft((s, x) -> (s+1, x+s), 0, Chain.empty()) == (0, Chain.empty())

    @Test
    def mapAccumLeft02(): Bool = Chain.mapAccumLeft((s, x) -> (s+1, x+s), 0, Chain.singleton(1)) == (1, Chain.singleton(1))

    @Test
    def mapAccumLeft03(): Bool = Chain.mapAccumLeft((s, x) -> (s+1, x+s), 0, List.toChain(1 :: 1 :: Nil)) == (2, List.toChain(1 :: 2 :: Nil))

    @Test
    def mapAccumLeft04(): Bool = Chain.mapAccumLeft((s, x) -> (s+1, x+s), 0, List.toChain(1 :: 1 :: 1 :: Nil)) == (3, List.toChain(1 :: 2 :: 3 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // mapAccumRight                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def mapAccumRight01(): Bool = Chain.mapAccumRight((s, x) -> (s+1, x+s), 0, Chain.empty()) == (0, Chain.empty())

    @Test
    def mapAccumRight02(): Bool = Chain.mapAccumRight((s, x) -> (s+1, x+s), 0, Chain.singleton(1)) == (1, Chain.singleton(1))

    @Test
    def mapAccumRight03(): Bool = Chain.mapAccumRight((s, x) -> (s+1, x+s), 0, List.toChain(1 :: 1 :: Nil)) == (2, List.toChain(2 :: 1 :: Nil))

    @Test
    def mapAccumRight04(): Bool = Chain.mapAccumRight((s, x) -> (s+1, x+s), 0, List.toChain(1 :: 1 :: 1 :: Nil)) == (3, List.toChain(3 :: 2 :: 1 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // forEach                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forEach01(): Bool = region rc {
        let ri = ref 21 @ rc;
        Chain.forEach(x -> ri := x, Chain.empty());
        21 == deref ri
    }

    @Test
    def forEach02(): Bool = region rc {
        let ri = ref 21 @ rc;
        Chain.forEach(x -> ri := x, List.toChain(1 :: 2 :: 3 :: Nil));
        3 == deref ri
    }

    /////////////////////////////////////////////////////////////////////////////
    // forEachWithIndex                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forEachWithIndex01(): Bool = region rc {
        let ri = ref 21 @ rc;
        Chain.forEachWithIndex((i, _) -> ri := i, Chain.empty());
        21 == deref ri
    }

    @Test
    def forEachWithIndex02(): Bool = region rc {
        let ri = ref 21 @ rc;
        Chain.forEachWithIndex((i, _) -> ri := i, List.toChain(1 :: 2 :: 3 :: Nil));
        2 == deref ri
    }

    /////////////////////////////////////////////////////////////////////////////
    // toList                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toList01(): Bool =
        Chain.toList(Chain.empty(): Chain[Int32]) == Nil: List[Int32]

    @Test
    def toList02(): Bool =
        Chain.toList(Chain.singleton(1)) == 1 :: Nil

    @Test
    def toList03(): Bool =
        Chain.toList(List.toChain(1 :: 2 :: Nil)) == 1 :: 2 :: Nil

    @Test
    def toList04(): Bool =
        Chain.toList(List.toChain(1 :: 2 :: 3 :: Nil)) == 1 :: 2 :: 3 :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // toMutList                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toMutList01(): Bool = region rc {
        Chain.toMutList(rc, Chain.empty(): Chain[Int32]) `MutList.sameElements` List.toMutList(rc, Nil): MutList[Int32, rc]
    }

    @Test
    def toMutList02(): Bool = region rc {
        Chain.toMutList(rc, Chain.singleton(1)) `MutList.sameElements` List.toMutList(rc, 1 :: Nil)
    }

    @Test
    def toMutList03(): Bool = region rc {
        Chain.toMutList(rc, List.toChain(1 :: 2 :: Nil)) `MutList.sameElements` List.toMutList(rc, 1 :: 2 :: Nil)
    }

    @Test
    def toMutList04(): Bool = region rc {
        Chain.toMutList(rc, List.toChain(1 :: 2 :: 3 :: Nil)) `MutList.sameElements` List.toMutList(rc, 1 :: 2 :: 3 :: Nil)
    }

    /////////////////////////////////////////////////////////////////////////////
    // toMutDeque                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toMutDeque01(): Bool = region rc {
        let c = Chain.empty(): Chain[Int32];
        let d1 = Chain.toMutDeque(rc, c);

        let d2 = MutDeque.new(rc);

        d1 `MutDeque.sameElements` d2
    }

    @Test
    def toMutDeque02(): Bool = region rc {
        let c = Chain.singleton(1);
        let d1 = Chain.toMutDeque(rc, c);

        let d2 = MutDeque.new(rc);
        MutDeque.pushBack(1, d2);

        d1 `MutDeque.sameElements` d2
    }

    @Test
    def toMutDeque03(): Bool = region rc {
        let c = List.toChain(1 :: 2 :: Nil);
        let d1 = Chain.toMutDeque(rc, c);

        let d2 = MutDeque.new(rc);
        MutDeque.pushFront(1, d2);
        MutDeque.pushBack(2, d2);

        d1 `MutDeque.sameElements` d2
    }

    @Test
    def toMutDeque04(): Bool = region rc {
        let c = List.toChain('a' :: 'b' :: 'c' :: Nil);
        let d1 = Chain.toMutDeque(rc, c);

        let d2 = MutDeque.new(rc);
        MutDeque.pushFront('b', d2);
        MutDeque.pushBack('c', d2);
        MutDeque.pushFront('a', d2);

        d1 `MutDeque.sameElements` d2
    }

    /////////////////////////////////////////////////////////////////////////////
    // toSet                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toSet01(): Bool =
        Chain.toSet(Chain.empty(): Chain[Int32]) == Set#{}: Set[Int32]

    @Test
    def toSet02(): Bool =
        Chain.toSet(Chain.singleton(1)) == Set#{1}

    @Test
    def toSet03(): Bool =
        Chain.toSet(List.toChain(1 :: 2 :: Nil)) == Set#{1, 2}

    @Test
    def toSet04(): Bool =
        Chain.toSet(List.toChain(1 :: 2 :: 3 :: Nil)) == Set#{1, 2, 3}

    /////////////////////////////////////////////////////////////////////////////
    // toMap                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toMap01(): Bool =
        Chain.toMap(Chain.empty(): Chain[(Int32, Int32)]) == Map#{}: Map[Int32, Int32]

    @Test
    def toMap02(): Bool =
        Chain.toMap(Chain.singleton((1, 1))) == Map#{1 => 1}

    @Test
    def toMap03(): Bool =
        Chain.toMap(List.toChain((1, 1) :: (2, 2) :: Nil)) == Map#{1 => 1, 2 => 2}

    @Test
    def toMap04(): Bool =
        Chain.toMap(List.toChain((1, 1) :: (2, 2) :: (3, 3) :: Nil)) == Map#{1 => 1, 2 => 2, 3 => 3}

    /////////////////////////////////////////////////////////////////////////////
    // toArray                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toArray01(): Bool = region rc {
        let c: Chain[Int32] = Chain.empty();
        Array.sameElements(Chain.toArray(rc, c), Array#{} @ rc)
    }

    @Test
    def toArray02(): Bool = region rc {
        let a = Chain.toArray(rc, Chain.singleton(1));
        Array.sameElements(a, Array#{1} @ rc)
    }

    @Test
    def toArray03(): Bool = region rc {
        let a = Chain.toArray(rc, List.toChain(1 :: 2 :: Nil));
        Array.sameElements(a, Array#{1, 2} @ rc)
    }

    @Test
    def toArray04(): Bool = region rc {
        let a = Chain.toArray(rc, List.toChain(1 :: 2 :: 3 :: Nil));
        Array.sameElements(a, Array#{1, 2, 3} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // toVector                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toVector01(): Bool =
        let c: Chain[Int32] = Chain.empty();
        Chain.toVector(c) == Vector#{}

    @Test
    def toVector02(): Bool =
        let c = Chain.singleton(1);
        Chain.toVector(c) == Vector#{1}

    @Test
    def toVector03(): Bool =
        let c = List.toChain(1 :: 2 :: Nil);
        Chain.toVector(c) == Vector#{1, 2}

    @Test
    def toVector04(): Bool =
        let c = List.toChain(1 :: 2 :: 3 :: Nil);
        Chain.toVector(c) == Vector#{1, 2, 3}

    /////////////////////////////////////////////////////////////////////////////
    // iterator                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def iterator01(): Bool = region rc {
        Chain.empty() |> Chain.iterator(rc) |> Iterator.toList == Nil: List[Int32]
    }

    @Test
    def iterator02(): Bool = region rc {
        List.toChain(1 :: 2 :: 3 :: Nil) |> Chain.iterator(rc) |> Iterator.toList == 1 :: 2 :: 3 :: Nil
    }

    @Test
    def iterator03(): Bool = region rc {
        Chain.range(-100, 100) |> Chain.iterator(rc) |> Iterator.toList == List.range(-100, 100)
    }


    /////////////////////////////////////////////////////////////////////////////
    // enumerator                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def enumerator01(): Bool = region rc {
        Chain.empty() |> Chain.enumerator(rc) |> Iterator.toList == Nil: List[(Int32, Int32)]
    }

    @Test
    def enumerator02(): Bool = region rc {
        List.toChain(1 :: 2 :: 3 :: Nil) |> Chain.enumerator(rc) |> Iterator.toList == (0, 1) :: (1, 2) :: (2, 3) :: Nil
    }

    @Test
    def enumerator03(): Bool = region rc {
        Chain.range(-100, 100) |> Chain.enumerator(rc) |> Iterator.toList == List.range(-100, 100) |> List.zipWithIndex
    }


    /////////////////////////////////////////////////////////////////////////////
    // toNel.Nel                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toNel01(): Bool = Chain.toNel(Chain.empty(): Chain[Int32]) == None

    @Test
    def toNel02(): Bool = Chain.toNel(Chain.singleton(1)) == Some(Nel.Nel(1, Nil))

    @Test
    def toNel03(): Bool =
        Chain.toNel(List.toChain(1 :: 2 :: Nil)) == Some(Nel.Nel(1, 2 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // toNec                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toNec01(): Bool = Chain.toNec(Chain.empty(): Chain[Int32]) == None

    @Test
    def toNec02(): Bool = Chain.toNec(Chain.singleton(1)) == Some(Nec.singleton(1))

    @Test
    def toNec03(): Bool =
        Chain.toNec(List.toChain(1 :: 2 :: Nil)) == Some(Nec.cons(1, Nec.singleton(2)))

    /////////////////////////////////////////////////////////////////////////////
    // equals                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def equals01(): Bool =
        (Chain.empty(): Chain[Int32]) `Chain.equals` (Chain.empty(): Chain[Int32])

    @Test
    def equals02(): Bool =
        Chain.singleton(1) `Chain.equals` Chain.singleton(1)

    @Test
    def equals03(): Bool =
        (Chain.empty() |> Chain.cons(1)) `Chain.equals` Chain.singleton(1)

    @Test
    def equals04(): Bool =
        (Chain.empty() `Chain.snoc` 1) `Chain.equals` Chain.singleton(1)

    @Test
    def equals05(): Bool =
        (Chain.empty() `Chain.snoc` 1) `Chain.equals` (Chain.empty() |> Chain.cons(1))

    /////////////////////////////////////////////////////////////////////////////
    // compare                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def compare01(): Bool =
        (Chain.empty(): Chain[Int32]) `Chain.compare` (Chain.empty(): Chain[Int32]) == Comparison.EqualTo

    @Test
    def compare02(): Bool =
        Chain.singleton(1) `Chain.compare` Chain.empty() == Comparison.GreaterThan

    @Test
    def compare03(): Bool =
        Chain.empty() `Chain.compare` Chain.singleton(1) == Comparison.LessThan

    @Test
    def compare04(): Bool =
        Chain.singleton(1) `Chain.compare` Chain.singleton(1) == Comparison.EqualTo

    @Test
    def compare05(): Bool =
        Chain.singleton(1) `Chain.compare` Chain.singleton(2) == Comparison.LessThan

    @Test
    def compare06(): Bool =
        Chain.singleton(2) `Chain.compare` Chain.singleton(1) == Comparison.GreaterThan

    @Test
    def compare07(): Bool =
        Chain.singleton(1) `Chain.compare` List.toChain(1 :: 2 :: Nil) == Comparison.LessThan

    @Test
    def compare08(): Bool =
        List.toChain(1 :: 2 :: Nil) `Chain.compare` Chain.singleton(1) == Comparison.GreaterThan

    @Test
    def compare09(): Bool =
        Chain.singleton(1) `Chain.compare` List.toChain(0 :: 0 :: Nil) == Comparison.GreaterThan

    @Test
    def compare10(): Bool =
        List.toChain(0 :: 0 :: Nil)  `Chain.compare` Chain.singleton(1) == Comparison.LessThan

    @Test
    def compare11(): Bool =
        List.toChain(1 :: 2 :: Nil) `Chain.compare` List.toChain(1 :: 1 :: Nil) == Comparison.GreaterThan

    @Test
    def compare12(): Bool =
        List.toChain(1 :: 2 :: Nil) `Chain.compare` List.toChain(1 :: 3 :: Nil) == Comparison.LessThan

    @Test
    def compare13(): Bool =
        List.toChain(1 :: 2 :: 3 :: 4 :: Nil) `Chain.compare` List.toChain(1 :: 2 :: 3 :: 4 :: Nil) == Comparison.EqualTo

    /////////////////////////////////////////////////////////////////////////////
    // sort                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    def testSortVsSortWith(c: Chain[Int32]) : Bool =
        Chain.sort(c) == Chain.sortWith(cmp, c)

    @Test
    def sort01(): Bool = testSortVsSortWith(Chain.empty(): Chain[Int32])

    @Test
    def sort02(): Bool = testSortVsSortWith(Chain.singleton(0))

    @Test
    def sort03(): Bool = testSortVsSortWith(List.toChain(0 :: 1 :: Nil))

    @Test
    def sort04(): Bool = testSortVsSortWith(List.toChain(1 :: 0 :: Nil))

    @Test
    def sort05(): Bool = testSortVsSortWith(List.toChain(1 :: 1 :: Nil))

    @Test
    def sort06(): Bool = testSortVsSortWith(List.toChain(0 :: 1 :: 2 :: 3 :: 4 :: 5 :: Nil))

    @Test
    def sort07(): Bool = testSortVsSortWith(List.toChain(5 :: 4 :: 3 :: 2 :: 1 :: 0 :: Nil))

    @Test
    def sort08(): Bool = testSortVsSortWith(List.toChain(5 :: 3 :: 0 :: 4 :: 1 :: 2 :: Nil))

    @Test
    def sort09(): Bool = testSortVsSortWith(List.toChain(2 :: 3 :: 0 :: 4 :: 1 :: 2 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // sortBy                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    def testSortByVsSort(a: Chain[Int32]) : Bool =
        (Chain.sortBy(identity, a) == Chain.sort(a)) and
        (Chain.sortBy(x -> 4*x+7, a) == Chain.sort(a)) and
        (Chain.sortBy(x -> -x, a) == Chain.sortWith(flip(cmp),a))

    @Test
    def sortBy01(): Bool = testSortByVsSort(Chain.empty(): Chain[Int32])

    @Test
    def sortBy02(): Bool = testSortByVsSort(Chain.singleton(0))

    @Test
    def sortBy03(): Bool = testSortByVsSort(List.toChain(0 :: 1 :: Nil))

    @Test
    def sortBy04(): Bool = testSortByVsSort(List.toChain(1 :: 0 :: Nil))

    @Test
    def sortBy05(): Bool = testSortByVsSort(List.toChain(1 :: 1 :: Nil))

    @Test
    def sortBy06(): Bool = testSortByVsSort(List.toChain(0 :: 1 :: 2 :: 3 :: 4 :: 5 :: Nil))

    @Test
    def sortBy07(): Bool = testSortByVsSort(List.toChain(5 :: 4 :: 3 :: 2 :: 1 :: 0 :: Nil))

    @Test
    def sortBy08(): Bool = testSortByVsSort(List.toChain(5 :: 3 :: 0 :: 4 :: 1 :: 2 :: Nil))

    @Test
    def sortBy09(): Bool = testSortByVsSort(List.toChain(2 :: 3 :: 0 :: 4 :: 1 :: 2 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // sortWith                                                                //
    /////////////////////////////////////////////////////////////////////////////

    def cmp(x: Int32, y: Int32): Comparison =
        if (x < y) Comparison.LessThan
        else if (x == y) Comparison.EqualTo
        else Comparison.GreaterThan

    @Test
    def sortWith01(): Bool =
        Chain.sortWith(cmp, Chain.empty(): Chain[Int32]) == Chain.empty()

    @Test
    def sortWith02(): Bool =
        Chain.sortWith(cmp, Chain.singleton(0)) == Chain.singleton(0)

    @Test
    def sortWith03(): Bool =
        Chain.sortWith(cmp, List.toChain(0 :: 1 :: Nil)) == List.toChain(0 :: 1 :: Nil)

    @Test
    def sortWith04(): Bool =
        Chain.sortWith(cmp, List.toChain(1 :: 0 :: Nil)) == List.toChain(0 :: 1 :: Nil)

    @Test
    def sortWith05(): Bool =
        Chain.sortWith(cmp, List.toChain(1 :: 1 :: Nil)) == List.toChain(1 :: 1 :: Nil)

    @Test
    def sortWith06(): Bool =
        Chain.sortWith(cmp, List.toChain(0 :: 1 :: 2 :: 3 :: 4 :: 5 :: Nil)) == List.toChain(0 :: 1 :: 2 :: 3 :: 4 :: 5 :: Nil)

    @Test
    def sortWith07(): Bool =
        Chain.sortWith(cmp, List.toChain(5 :: 4 :: 3 :: 2 :: 1 :: 0 :: Nil)) == List.toChain(0 :: 1 :: 2 :: 3 :: 4 :: 5 :: Nil)

    @Test
    def sortWith08(): Bool =
        Chain.sortWith(cmp, List.toChain(5 :: 3 :: 0 :: 4 :: 1 :: 2 :: Nil)) == List.toChain(0 :: 1 :: 2 :: 3 :: 4 :: 5 :: Nil)

    @Test
    def sortWith09(): Bool =
        Chain.sortWith(cmp, List.toChain(2 :: 3 :: 0 :: 4 :: 1 :: 2 :: Nil)) == List.toChain(0 :: 1 :: 2 :: 2 :: 3 :: 4 :: Nil)

    @Test
    def sortWith10(): Bool =
        Chain.sortWith(flip(cmp), List.toChain(0 :: 1 :: 2 :: 3 :: 4 :: 5 :: Nil)) == List.toChain(5 :: 4 :: 3 :: 2 :: 1 :: 0 :: Nil)

    @Test
    def sortWith11(): Bool =
        Chain.sortWith(flip(cmp), List.toChain(5 :: 4 :: 3 :: 2 :: 1 :: 0 :: Nil)) == List.toChain(5 :: 4 :: 3 :: 2 :: 1 :: 0 :: Nil)

    @Test
    def sortWith12(): Bool =
        Chain.sortWith(flip(cmp), List.toChain(5 :: 3 :: 0 :: 4 :: 1 :: 2 :: Nil)) == List.toChain(5 :: 4 :: 3 :: 2 :: 1 :: 0 :: Nil)

    @Test
    def sortWith13(): Bool =
        Chain.sortWith(flip(cmp), List.toChain(2 :: 3 :: 0 :: 4 :: 1 :: 2 :: Nil)) == List.toChain(4 :: 3 :: 2 :: 2 :: 1 :: 0 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // sequence                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sequence01(): Bool =
        let c: Chain[Identity[Int32]] = Chain.empty();
        Chain.sequence(c) == Identity.Identity(Chain.empty())

    @Test
    def sequence02(): Bool =
        let c = Chain.singleton(Identity.Identity(1));
        Chain.sequence(c) == Identity.Identity(Chain.singleton(1))

    @Test
    def sequence03(): Bool =
        let c = List.toChain(Identity.Identity(1) :: Identity.Identity(2) :: Nil);
        Chain.sequence(c) == Identity.Identity(List.toChain(1 :: 2 :: Nil))

    @Test
    def sequence04(): Bool =
        let c = List.toChain(Identity.Identity(1) :: Identity.Identity(2) :: Identity.Identity(3) :: Nil);
        Chain.sequence(c) == Identity.Identity(List.toChain(1 :: 2 :: 3 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // traverse                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def traverse01(): Bool = region rc {
        let st = ref 0 @ rc;
        let c = Chain.empty();
        let ans = Chain.traverse(x -> {st := x; Identity.Identity(x)}, c);
        ans == Identity.Identity(Chain.empty()) and deref st == 0
    }

    @Test
    def traverse02(): Bool = region rc {
        let st = ref 0 @ rc;
        let c = Chain.singleton(1);
        let ans = Chain.traverse(x -> {st := x; Identity.Identity(x)}, c);
        ans == Identity.Identity(Chain.singleton(1)) and deref st == 1
    }

    @Test
    def traverse03(): Bool = region rc {
        let st = ref 0 @ rc;
        let c = List.toChain(1 :: 2 :: Nil);
        let ans = Chain.traverse(x -> {st := x; Identity.Identity(x)}, c);
        ans == Identity.Identity(List.toChain(1 :: 2 :: Nil)) and deref st == 2
    }

    @Test
    def traverse04(): Bool = region rc {
        let st = ref 0 @ rc;
        let c = List.toChain(1 :: 2 :: 3 :: Nil);
        let ans = Chain.traverse(x -> {st := x; Identity.Identity(x)}, c);
        ans == Identity.Identity(List.toChain(1 :: 2 :: 3 :: Nil)) and deref st == 3
    }

    /////////////////////////////////////////////////////////////////////////////
    // join                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def join01(): Bool =
        Nil: List[Int32] |> List.toChain |> Chain.join(",") == ""

    @Test
    def join02(): Bool =
        (1 :: Nil) |> List.toChain |> Chain.join(",") == "1"

    @Test
    def join03(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List.toChain |> Chain.join(",") == "1,2,3"

    @Test
    def join04(): Bool =
        ("1" :: "2" :: "3" :: Nil) |> List.toChain |> Chain.join(",") == "1,2,3"


    /////////////////////////////////////////////////////////////////////////////
    // joinWith                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def joinWith01(): Bool =
        (Nil: List[Int32]) |> List.toChain |>
            Chain.joinWith(x -> "${x + 1}", ",") == ""

    @Test
    def joinWith02(): Bool =
        (1 :: Nil) |> List.toChain |>
            Chain.joinWith(x -> "${x + 1}", ",") == "2"

    @Test
    def joinWith03(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List.toChain |>
            Chain.joinWith(x -> "${x + 1}", ",") == "2,3,4"

    @Test
    def joinWith04(): Bool =
        ("1" :: "2" :: "3" :: Nil) |> List.toChain |>
            Chain.joinWith(x -> x + x, ",") == "11,22,33"

    /////////////////////////////////////////////////////////////////////////////
    // zipWithIndex                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def zipWithIndex01(): Bool = Chain.zipWithIndex(Chain.empty(): Chain[Int32]) == Chain.empty()

    @Test
    def zipWithIndex02(): Bool = Chain.zipWithIndex(Chain.singleton(2)) == List.toChain((0, 2) :: Nil)

    @Test
    def zipWithIndex03(): Bool = Chain.zipWithIndex(List.toChain(1 :: 8 :: Nil)) == List.toChain((0, 1) :: (1, 8) :: Nil)

    @Test
    def zipWithIndex04(): Bool = Chain.zipWithIndex(List.toChain(8 :: 1 :: Nil)) == List.toChain((0, 8) :: (1, 1) :: Nil)

    @Test
    def zipWithIndex05(): Bool = Chain.zipWithIndex(List.toChain(1 :: 4 :: 11 :: 2 :: -22 :: 17 :: Nil)) == List.toChain((0, 1) :: (1, 4) :: (2, 11) :: (3, 2) :: (4, -22) :: (5, 17) :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // shuffle                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def shuffle01(): Bool \ NonDet =
        let l: List[Int32] = Nil;
        let c = List.toChain(l) |> Chain.shuffle(Random.new());

        Chain.length(c) == 0 and Chain.toSet(c) == Set#{}

    @Test
    def shuffle02(): Bool \ NonDet =
        let l = 1 :: Nil;
        let c = List.toChain(l) |> Chain.shuffle(Random.new());

        Chain.length(c) == 1 and Chain.toSet(c) == Set#{1}

    @Test
    def shuffle03(): Bool \ NonDet =
        let l = 1 :: 2 :: 3 :: Nil;
        let c = List.toChain(l) |> Chain.shuffle(Random.new());

        Chain.length(c) == 3 and Chain.toSet(c) == Set#{1, 2, 3}

    @Test
    def shuffle04(): Bool \ NonDet =
        let l = 0 :: 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: 9 :: Nil;
        let c = List.toChain(l) |> Chain.shuffle(Random.new());

        Chain.length(c) == 10 and Chain.toSet(c) == Set#{0, 1, 2, 3, 4, 5, 6, 7, 8, 9}

}

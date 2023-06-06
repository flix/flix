/*
 * Copyright 2023 Magnus Madsen, Stephen Tetley
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

mod TestVector {

    /////////////////////////////////////////////////////////////////////////////
    // compare                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def compare01(): Bool =
        (Vector#{}: Vector[Int32]) `Vector.compare` (Vector#{}: Vector[Int32]) == Comparison.EqualTo

    @test
    def compare02(): Bool =
        ((Vector#{1}) `Vector.compare` (Vector#{})) == Comparison.GreaterThan

    @test
    def compare03(): Bool =
        ((Vector#{}) `Vector.compare` (Vector#{1})) == Comparison.LessThan

    @test
    def compare04(): Bool =
        ((Vector#{1}) `Vector.compare` (Vector#{1})) == Comparison.EqualTo

    @test
    def compare05(): Bool =
        ((Vector#{1}) `Vector.compare` (Vector#{2})) == Comparison.LessThan

    @test
    def compare06(): Bool =
        ((Vector#{2}) `Vector.compare` (Vector#{1})) == Comparison.GreaterThan

    @test
    def compare07(): Bool =
        ((Vector#{1}) `Vector.compare` (Vector#{1, 2})) == Comparison.LessThan

    @test
    def compare08(): Bool =
        ((Vector#{1, 2}) `Vector.compare` (Vector#{1})) == Comparison.GreaterThan

    @test
    def compare09(): Bool =
        ((Vector#{1}) `Vector.compare` (Vector#{0, 0})) == Comparison.GreaterThan

    @test
    def compare10(): Bool =
        ((Vector#{0, 0}) `Vector.compare` (Vector#{1})) == Comparison.LessThan

    @test
    def compare11(): Bool =
        ((Vector#{1, 2}) `Vector.compare` (Vector#{1, 1})) == Comparison.GreaterThan

    @test
    def compare12(): Bool =
        ((Vector#{1, 2}) `Vector.compare` (Vector#{1, 3})) == Comparison.LessThan

    @test
    def compare13(): Bool =
        ((Vector#{1, 2, 3, 4}) `Vector.compare` (Vector#{1, 2, 3, 4})) == Comparison.EqualTo

    /////////////////////////////////////////////////////////////////////////////
    // toString                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toString01(): Bool =
        Vector.toString(Vector#{}: Vector[Int32]) == "Vector#{}"

    @test
    def toString02(): Bool =
        Vector.toString(Vector#{1}) == "Vector#{1}"

    @test
    def toString03(): Bool =
        Vector.toString(Vector#{1, 2, 3, 4}) == "Vector#{1, 2, 3, 4}"

    @test
    def toString04(): Bool =
        Vector.toString(Vector#{'a', 'b', 'c', 'd'}) == "Vector#{a, b, c, d}"

    /////////////////////////////////////////////////////////////////////////////
    // empty                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def empty01(): Bool =
        let v = Vector.empty();
        Vector.length(v) == 0

    /////////////////////////////////////////////////////////////////////////////
    // singleton                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def singleton01(): Bool =
        let v = Vector.singleton(0);
        Vector.nth(0, v) == Some(0)

    @test
    def singleton02(): Bool =
        let v = Vector.singleton('A');
        Vector.nth(0, v) == Some('A')

    /////////////////////////////////////////////////////////////////////////////
    // nth                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def nth01(): Bool =
        Vector.nth(0, Vector#{1, 2, 3}) == Some(1)

    @test
    def nth02(): Bool =
        Vector.nth(1, Vector#{1, 2, 3}) == Some(2)

    @test
    def nth03(): Bool =
        Vector.nth(2, Vector#{1, 2, 3}) == Some(3)

    @test
    def nth04(): Bool =
        Vector.nth(3, Vector#{1, 2, 3}) == None

    @test
    def nth05(): Bool =
        Vector.nth(-2, Vector#{1, 2, 3}) == None

    /////////////////////////////////////////////////////////////////////////////
    // isEmpty                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def isEmpty01(): Bool =
        Vector.isEmpty(Vector#{})

    @test
    def isEmpty02(): Bool =
        not Vector.isEmpty(Vector#{1})

    @test
    def isEmpty03(): Bool =
        not Vector.isEmpty(Vector#{1, 2, 3})

    /////////////////////////////////////////////////////////////////////////////
    // length                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def length01(): Bool =
        Vector.length(Vector#{}) == 0

    @test
    def length02(): Bool =
        Vector.length(Vector#{1}) == 1

    @test
    def length03(): Bool =
        Vector.length(Vector#{1, 2, 3}) == 3

    /////////////////////////////////////////////////////////////////////////////
    // slice                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def slice01(): Bool =
        Vector.slice(start = 0, end = 1, Vector#{1, 2, 3}) |> Vector.get(0) == 1

    @test
    def slice02(): Bool =
        Vector.slice(start = 1, end = 2, Vector#{1, 2, 3}) |> Vector.get(0) == 2

    @test
    def slice03(): Bool =
        Vector.slice(start = 2, end = 3, Vector#{1, 2, 3}) |> Vector.get(0) == 3

    /////////////////////////////////////////////////////////////////////////////
    // toList                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toList01(): Bool =
        Vector.toList(Vector#{}: Vector[Int32]) == Nil

    @test
    def toList02(): Bool =
        Vector.toList(Vector#{1}) == 1 :: Nil

    @test
    def toList03(): Bool =
        Vector.toList(Vector#{1, 2}) == 1 :: 2 :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // toNel                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toNel01(): Bool =
        Vector.toNel(Vector#{}: Vector[Unit]) == None

    @test
    def toNel02(): Bool =
        Vector.toNel(Vector#{1}) == Some(Nel.singleton(1))

    @test
    def toNel03(): Bool =
        Vector.toNel(Vector#{1, 2}) == List.toNel(1 :: 2 :: Nil)

    @test
    def toNel04(): Bool =
        Vector.toNel(Vector#{1, 2, 3}) == List.toNel(1 :: 2 :: 3 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // toMutDeque                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toMutDeque01(): Bool = region rc {
        let v = Vector#{}: Vector[Int32];
        let d = MutDeque.new(rc): MutDeque[Int32, _];
        Vector.toMutDeque(rc, v) `MutDeque.sameElements` d
    }

    @test
    def toMutDeque02(): Bool = region rc {
        let v = Vector#{1};
        let d = MutDeque.new(rc);
        MutDeque.pushBack(1, d);
        Vector.toMutDeque(rc, v) `MutDeque.sameElements` d
    }

    @test
    def toMutDeque03(): Bool = region rc {
        let v = Vector#{1, 2, 3};
        let d = MutDeque.new(rc);
        MutDeque.pushBack(1, d);
        MutDeque.pushBack(2, d);
        MutDeque.pushBack(3, d);
        Vector.toMutDeque(rc, v) `MutDeque.sameElements` d
    }

    @test
    def toMutDeque04(): Bool = region rc {
        let v = Vector#{1, 2, 3, 4, 5};
        let d = MutDeque.new(rc);
        MutDeque.pushBack(3, d);
        MutDeque.pushBack(4, d);
        MutDeque.pushBack(5, d);
        MutDeque.pushFront(2, d);
        MutDeque.pushFront(1, d);
        Vector.toMutDeque(rc, v) `MutDeque.sameElements` d
    }

    @test
    def toMutDeque05(): Bool = region rc {
        let v = Vector#{'a', 'b', 'c'};
        let d = MutDeque.new(rc);
        MutDeque.pushFront('c', d);
        MutDeque.pushFront('b', d);
        MutDeque.pushFront('a', d);
        Vector.toMutDeque(rc, v) `MutDeque.sameElements` d
    }

    /////////////////////////////////////////////////////////////////////////////
    // head                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def head01(): Bool =
        Vector.head(Vector#{}: Vector[Int32]) == None

    @test
    def head02(): Bool =
        Vector.head(Vector#{1}) == Some(1)

    @test
    def head03(): Bool =
        Vector.head(Vector#{2, 1}) == Some(2)

    @test
    def head04(): Bool =
        Vector.head(Vector#{3, 2, 1}) == Some(3)

    /////////////////////////////////////////////////////////////////////////////
    // last                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def last01(): Bool =
        Vector.last(Vector#{}): Option[Unit] == None

    @test
    def last02(): Bool =
        Vector.last(Vector#{1}) == Some(1)

    @test
    def last03(): Bool =
        Vector.last(Vector#{1, 2}) == Some(2)

    @test
    def last04(): Bool =
        Vector.last(Vector#{1, 2, 3}) == Some(3)

    /////////////////////////////////////////////////////////////////////////////
    // append                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def append01(): Bool =
        let v = Vector.append(Vector#{}: Vector[Int32], Vector#{}: Vector[Int32]);
        v == (Vector#{}: Vector[Int32])

    @test
    def append02(): Bool =
        let v = Vector.append(Vector#{}: Vector[Int32], Vector#{1});
        v == Vector#{1}

    @test
    def append03(): Bool =
        let v = Vector.append(Vector#{}: Vector[Int32], Vector#{1, 2});
        v == Vector#{1, 2}

    @test
    def append04(): Bool =
        let v = Vector.append(Vector#{1}, Vector#{}: Vector[Int32]);
        v == Vector#{1}

    @test
    def append05(): Bool =
        let v = Vector.append(Vector#{1, 2}, Vector#{}: Vector[Int32]);
        v == Vector#{1, 2}

    @test
    def append06(): Bool =
        let v = Vector.append(Vector#{1}, Vector#{2});
        v == Vector#{1, 2}

    @test
    def append07(): Bool =
        let v = Vector.append(Vector#{1, 2}, Vector#{3});
        v == Vector#{1, 2, 3}

    @test
    def append08(): Bool =
        let v = Vector.append(Vector#{1}, Vector#{2, 3});
        v == Vector#{1, 2, 3}

    /////////////////////////////////////////////////////////////////////////////
    // memberOf                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def memberOf01(): Bool =
        Vector.memberOf(0, Vector#{}: Vector[Int32]) == false

    @test
    def memberOf02(): Bool =
        Vector.memberOf(0, Vector#{1}) == false

    @test
    def memberOf03(): Bool =
        Vector.memberOf(1, Vector#{1}) == true

    @test
    def memberOf04(): Bool =
        Vector.memberOf(0, Vector#{1, 2}) == false

    @test
    def memberOf05(): Bool =
        Vector.memberOf(1, Vector#{1, 2}) == true

    @test
    def memberOf06(): Bool =
        Vector.memberOf(2, Vector#{1, 2}) == true

    @test
    def memberOf07(): Bool =
        Vector.memberOf(3, Vector#{1, 2}) == false

    /////////////////////////////////////////////////////////////////////////////
    // indexOf                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def indexOf01(): Bool =
        Vector.indexOf(0, Vector#{}: Vector[Int32]) == None

    @test
    def indexOf02(): Bool =
        Vector.indexOf(0, Vector#{1}) == None

    @test
    def indexOf03(): Bool =
        Vector.indexOf(1, Vector#{1}) == Some(0)

    @test
    def indexOf04(): Bool =
        Vector.indexOf(0, Vector#{1, 2}) == None

    @test
    def indexOf05(): Bool =
        Vector.indexOf(1, Vector#{1, 2}) == Some(0)

    @test
    def indexOf06(): Bool =
        Vector.indexOf(2, Vector#{1, 2}) == Some(1)

    /////////////////////////////////////////////////////////////////////////////
    // indexOfLeft                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def indexOfLeft01(): Bool =
        Vector.indexOfLeft(0, Vector#{}: Vector[Int32]) == None

    @test
    def indexOfLeft02(): Bool =
        Vector.indexOfLeft(0, Vector#{1}) == None

    @test
    def indexOfLeft03(): Bool =
        Vector.indexOfLeft(1, Vector#{1}) == Some(0)

    @test
    def indexOfLeft04(): Bool =
        Vector.indexOfLeft(0, Vector#{1, 2}) == None

    @test
    def indexOfLeft05(): Bool =
        Vector.indexOfLeft(1, Vector#{1, 2}) == Some(0)

    @test
    def indexOfLeft06(): Bool =
        Vector.indexOfLeft(2, Vector#{1, 2}) == Some(1)

    @test
    def indexOfLeft07(): Bool =
        Vector.indexOfLeft(1, Vector#{1, 1}) == Some(0)

    /////////////////////////////////////////////////////////////////////////////
    // indexOfRight                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def indexOfRight01(): Bool =
        Vector.indexOfRight(0, Vector#{}: Vector[Int32]) == None

    @test
    def indexOfRight02(): Bool =
        Vector.indexOfRight(0, Vector#{1}) == None

    @test
    def indexOfRight03(): Bool =
        Vector.indexOfRight(1, Vector#{1}) == Some(0)

    @test
    def indexOfRight04(): Bool =
        Vector.indexOfRight(0, Vector#{1, 2}) == None

    @test
    def indexOfRight05(): Bool =
        Vector.indexOfRight(1, Vector#{1, 2}) == Some(0)

    @test
    def indexOfRight06(): Bool =
        Vector.indexOfRight(2, Vector#{1, 2}) == Some(1)

    @test
    def indexOfRight07(): Bool =
        Vector.indexOfRight(1, Vector#{1, 1}) == Some(1)

    /////////////////////////////////////////////////////////////////////////////
    // indices                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def indices01(): Bool =
        let v = Vector.indices(0, Vector#{}: Vector[Int32]);
        v == Vector#{}

    @test
    def indices02(): Bool =
        let v = Vector.indices(0, Vector#{1});
        v == Vector#{}

    @test
    def indices03(): Bool =
        let v = Vector.indices(1, Vector#{1});
        v == Vector#{0}

    @test
    def indices04(): Bool =
        let v = Vector.indices(0, Vector#{1, 2});
        v == Vector#{}

    @test
    def indices05(): Bool =
        let v = Vector.indices(1, Vector#{1, 2});
        v == Vector#{0}

    @test
    def indices06(): Bool =
        let v = Vector.indices(2, Vector#{1, 2});
        v == Vector#{1}

    @test
    def indices07(): Bool =
        let v = Vector.indices(1, Vector#{1, 1});
        v == Vector#{0, 1}

    /////////////////////////////////////////////////////////////////////////////
    // find                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def find01(): Bool =
        Vector.find(i -> i > 2, Vector#{}) == None

    @test
    def find02(): Bool =
        Vector.find(i -> i > 2, Vector#{1}) == None

    @test
    def find03(): Bool =
        Vector.find(i -> i > 2, Vector#{3}) == Some(3)

    @test
    def find04(): Bool =
        Vector.find(i -> i > 2, Vector#{1, 2}) == None

    @test
    def find05(): Bool =
        Vector.find(i -> i > 2, Vector#{6, -6}) == Some(6)

    @test
    def find06(): Bool =
        Vector.find(i -> i > 2, Vector#{-6, 6}) == Some(6)

    @test
    def find07(): Bool =
        Vector.find(i -> i > 2, Vector#{6, 7}) == Some(6)

    /////////////////////////////////////////////////////////////////////////////
    // findLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def findLeft01(): Bool =
        Vector.findLeft(i -> i > 2, Vector#{}) == None

    @test
    def findLeft02(): Bool =
        Vector.findLeft(i -> i > 2, Vector#{1}) == None

    @test
    def findLeft03(): Bool =
        Vector.findLeft(i -> i > 2, Vector#{3}) == Some(3)

    @test
    def findLeft04(): Bool =
        Vector.findLeft(i -> i > 2, Vector#{1, 2}) == None

    @test
    def findLeft05(): Bool =
        Vector.findLeft(i -> i > 2, Vector#{6, -6}) == Some(6)

    @test
    def findLeft06(): Bool =
        Vector.findLeft(i -> i > 2, Vector#{-6, 6}) == Some(6)

    @test
    def findLeft07(): Bool =
        Vector.findLeft(i -> i > 2, Vector#{6, 7}) == Some(6)

    /////////////////////////////////////////////////////////////////////////////
    // findRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def findRight01(): Bool =
        Vector.findRight(i -> i > 2, Vector#{}) == None

    @test
    def findRight02(): Bool =
        Vector.findRight(i -> i > 2, Vector#{1}) == None

    @test
    def findRight03(): Bool =
        Vector.findRight(i -> i > 2, Vector#{3}) == Some(3)

    @test
    def findRight04(): Bool =
        Vector.findRight(i -> i > 2, Vector#{1, 2}) == None

    @test
    def findRight05(): Bool =
        Vector.findRight(i -> i > 2, Vector#{6, -6}) == Some(6)

    @test
    def findRight06(): Bool =
        Vector.findRight(i -> i > 2, Vector#{-6, 6}) == Some(6)

    @test
    def findRight07(): Bool =
        Vector.findRight(i -> i > 2, Vector#{6, 7}) == Some(7)

    /////////////////////////////////////////////////////////////////////////////
    // range                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def range01(): Bool =
        let v = Vector.range(1, 0);
        v == Vector#{}

    @test
    def range02(): Bool =
        let v = Vector.range(1, 1);
        v == Vector#{}

    @test
    def range03(): Bool =
        let v = Vector.range(1, 2);
        v == Vector#{1}

    @test
    def range04(): Bool =
        let v = Vector.range(1, 3);
        v == Vector#{1, 2}

    @test
    def range05(): Bool =
        let v = Vector.range(1, 4);
        v == Vector#{1, 2, 3}

    @test
    def range06(): Bool =
        let v = Vector.range(-1, 3);
        v == Vector#{-1, 0, 1, 2}

    /////////////////////////////////////////////////////////////////////////////
    // repeat                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def repeat01(): Bool =
        let v = Vector.repeat(-1, 1);
        v == Vector#{}

    @test
    def repeat02(): Bool =
        let v = Vector.repeat(0, 1);
        v == Vector#{}

    @test
    def repeat03(): Bool =
        let v = Vector.repeat(1, 1);
        v == Vector#{1}

    @test
    def repeat04(): Bool =
        let v = Vector.repeat(2, 1);
        v == Vector#{1, 1}

    @test
    def repeat05(): Bool =
        let v = Vector.repeat(3, 1);
        v == Vector#{1, 1, 1}

    /////////////////////////////////////////////////////////////////////////////
    // scan                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def scan01(): Bool =
        let v = Vector.scan((i, b) -> if (b) i + 1 else i + 2, 1, Vector#{});
        v == Vector#{1}

    @test
    def scan02(): Bool =
        let v = Vector.scan((i, b) -> if (b) i + 1 else i + 2, 1, Vector#{false});
        v == Vector#{1, 3}

    @test
    def scan03(): Bool =
        let v = Vector.scan((i, b) -> if (b) i + 1 else i + 2, 1, Vector#{true});
        v == Vector#{1, 2}

    @test
    def scan04(): Bool =
        let v = Vector.scan((i, b) -> if (b) i + 1 else i + 2, 1, Vector#{false, false});
        v == Vector#{1, 3, 5}

    @test
    def scan05(): Bool =
        let v = Vector.scan((i, b) -> if (b) i + 1 else i + 2, 1, Vector#{false, true});
        v == Vector#{1, 3, 4}

    @test
    def scan06(): Bool =
        let v = Vector.scan((i, b) -> if (b) i + 1 else i + 2, 1, Vector#{true, false});
        v == Vector#{1, 2, 4}

    @test
    def scan07(): Bool =
        let v = Vector.scan((i, b) -> if (b) i + 1 else i + 2, 1, Vector#{true, true});
        v == Vector#{1, 2, 3}

    /////////////////////////////////////////////////////////////////////////////
    // scanLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def scanLeft01(): Bool =
        let v = Vector.scanLeft((i, b) -> if (b) i + 1 else i + 2, 1, Vector#{}: Vector[Bool]);
        v == Vector#{1}

    @test
    def scanLeft02(): Bool =
        let v = Vector.scanLeft((i, b) -> if (b) i + 1 else i + 2, 1, Vector#{false});
        v == Vector#{1, 3}

    @test
    def scanLeft03(): Bool =
        let v = Vector.scanLeft((i, b) -> if (b) i + 1 else i + 2, 1, Vector#{true});
        v == Vector#{1, 2}

    @test
    def scanLeft04(): Bool =
        let v = Vector.scanLeft((i, b) -> if (b) i + 1 else i + 2, 1, Vector#{false, false});
        v == Vector#{1, 3, 5}

    @test
    def scanLeft05(): Bool =
        let v = Vector.scanLeft((i, b) -> if (b) i + 1 else i + 2, 1, Vector#{false, true});
        v == Vector#{1, 3, 4}

    @test
    def scanLeft06(): Bool =
        let v = Vector.scanLeft((i, b) -> if (b) i + 1 else i + 2, 1, Vector#{true, false});
        v == Vector#{1, 2, 4}

    @test
    def scanLeft07(): Bool =
        let v = Vector.scanLeft((i, b) -> if (b) i + 1 else i + 2, 1, Vector#{true, true});
        v == Vector#{1, 2, 3}

    /////////////////////////////////////////////////////////////////////////////
    // scanRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def scanRight01(): Bool =
        let v = Vector.scanRight((b, i) -> if (b) i + 1 else i + 2, 1, Vector#{});
        v == Vector#{1}

    @test
    def scanRight02(): Bool =
        let v = Vector.scanRight((b, i) -> if (b) i + 1 else i + 2, 1, Vector#{false});
        v == Vector#{3, 1}

    @test
    def scanRight03(): Bool =
        let v = Vector.scanRight((b, i) -> if (b) i + 1 else i + 2, 1, Vector#{true});
        v == Vector#{2, 1}

    @test
    def scanRight04(): Bool =
        let v = Vector.scanRight((b, i) -> if (b) i + 1 else i + 2, 1, Vector#{false, false});
        v == Vector#{5, 3, 1}

    @test
    def scanRight05(): Bool =
        let v = Vector.scanRight((b, i) -> if (b) i + 1 else i + 2, 1, Vector#{false, true});
        v == Vector#{4, 2, 1}

    @test
    def scanRight06(): Bool =
        let v = Vector.scanRight((b, i) -> if (b) i + 1 else i + 2, 1, Vector#{true, false});
        v == Vector#{4, 3, 1}

    @test
    def scanRight07(): Bool =
        let v = Vector.scanRight((b, i) -> if (b) i + 1 else i + 2, 1, Vector#{true, true});
        v == Vector#{3, 2, 1}

    /////////////////////////////////////////////////////////////////////////////
    // map                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def map01(): Bool =
        let v = Vector.map(i -> i > 2, Vector#{});
        v == Vector#{}

    @test
    def map02(): Bool =
        let v = Vector.map(i -> i > 2, Vector#{1});
        v == Vector#{false}

    @test
    def map03(): Bool =
        let v = Vector.map(i -> i > 2, Vector#{3});
        v == Vector#{true}

    @test
    def map04(): Bool =
        let v = Vector.map(i -> i > 2, Vector#{1, 2});
        v == Vector#{false, false}

    @test
    def map05(): Bool =
        let v = Vector.map(i -> i > 2, Vector#{1, 8});
        v == Vector#{false, true}

    @test
    def map06(): Bool =
        let v = Vector.map(i -> i > 2, Vector#{8, 1});
        v == Vector#{true, false}

    @test
    def map07(): Bool =
        let v = Vector.map(i -> i > 2, Vector#{7, 8});
        v == Vector#{true, true}

    /////////////////////////////////////////////////////////////////////////////
    // mapWithIndex                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapWithIndex01(): Bool =
        let v = Vector.mapWithIndex((i, e) -> if (i < 1) e > 2 else e <= 2, Vector#{});
        v == Vector#{}

    @test
    def mapWithIndex02(): Bool =
        let v = Vector.mapWithIndex((i, e) -> if (i < 1) e > 2 else e <= 2, Vector#{1});
        v == Vector#{false}

    @test
    def mapWithIndex03(): Bool =
        let v = Vector.mapWithIndex((i, e) -> if (i < 1) e > 2 else e <= 2, Vector#{3});
        v == Vector#{true}

    @test
    def mapWithIndex04(): Bool =
        let v = Vector.mapWithIndex((i, e) -> if (i < 1) e > 2 else e <= 2, Vector#{1, 2});
        v == Vector#{false, true}

    @test
    def mapWithIndex05(): Bool =
        let v = Vector.mapWithIndex((i, e) -> if (i < 1) e > 2 else e <= 2, Vector#{1, 8});
        v == Vector#{false, false}

    @test
    def mapWithIndex06(): Bool =
        let v = Vector.mapWithIndex((i, e) -> if (i < 1) e > 2 else e <= 2, Vector#{8, 1});
        v == Vector#{true, true}

    @test
    def mapWithIndex07(): Bool =
        let v = Vector.mapWithIndex((i, e) -> if (i < 1) e > 2 else e <= 2, Vector#{7, 8});
        v == Vector#{true, false}

    /////////////////////////////////////////////////////////////////////////////
    // ap                                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def ap01(): Bool =
        let v = Vector.ap((Vector#{} : Vector[Int32 -> Int32]), (Vector#{} : Vector[Int32]));
        v == Vector#{}

    @test
    def ap02(): Bool =
        let v = Vector.ap(Vector#{(x -> x+1)}, Vector#{});
        v == Vector#{}

    @test
    def ap03(): Bool =
        let v = Vector.ap((Vector#{} : Vector[Int32 -> Int32]), Vector#{5});
        v == Vector#{}

    @test
    def ap04(): Bool =
        let v = Vector.ap(Vector#{(x -> x+1)}, Vector#{5});
        v == Vector#{6}

    @test
    def ap05(): Bool =
        let v = Vector.ap(Vector#{(x -> x+1)}, Vector#{0, 5});
        v == Vector#{1, 6}

    @test
    def ap06(): Bool =
        let v = Vector.ap(Vector#{(x -> x+1), (x -> x*2)}, Vector#{0, 4});
        v == Vector#{1, 5, 0, 8}

    /////////////////////////////////////////////////////////////////////////////
    // sequence                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sequence01(): Bool =
        let v: Vector[Option[Int32]] = Vector#{};
        Vector.sequence(v) == Some(Vector#{})

    @test
    def sequence02(): Bool =
        let v: Vector[Option[Int32]] = Vector#{None};
        Vector.sequence(v) == None

    @test
    def sequence03(): Bool =
        let v = Vector#{Some(1)};
        Vector.sequence(v) == Some(Vector#{1})

    @test
    def sequence04(): Bool =
        let v: Vector[Option[Int32]] = Vector#{None, None};
        Vector.sequence(v) == None

    @test
    def sequence05(): Bool =
        let v = Vector#{Some(1), None};
        Vector.sequence(v) == None

    @test
    def sequence06(): Bool =
        let v = Vector#{None, Some(2)};
        Vector.sequence(v) == None

    @test
    def sequence07(): Bool =
        let v = Vector#{Some(1), Some(2)};
        Vector.sequence(v) == Some(Vector#{1, 2})

    /////////////////////////////////////////////////////////////////////////////
    // traverse                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def traverse01(): Bool =
        let v = Vector#{};
        Vector.traverse(x -> if (x > 2) None else Some(x), v) == Some(Vector#{})

    @test
    def traverse02(): Bool =
        let v = Vector#{1};
        Vector.traverse(x -> if (x > 2) None else Some(x), v) == Some(Vector#{1})

    @test
    def traverse03(): Bool =
        let v = Vector#{1, 2};
        Vector.traverse(x -> if (x > 2) None else Some(x), v) == Some(Vector#{1, 2})

    @test
    def traverse04(): Bool =
        let v = Vector#{1, 2, 3};
        Vector.traverse(x -> if (x > 2) None else Some(x), v) == None

    @test
    def traverse05(): Bool =
        let v = Vector#{3};
        Vector.traverse(x -> if (x > 2) None else Some(x), v) == None

    @test
    def traverse06(): Bool =
        let v = Vector#{1, 2, 1};
        Vector.traverse(x -> if (x > 2) None else Some(x), v) == Some(Vector#{1, 2, 1})

    /////////////////////////////////////////////////////////////////////////////
    // zipWithA                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def zipWithA01(): Bool =
        let v = Vector.zipWithA((x, y) -> if (x + y > 5) None else Some(x + y), Vector#{}, Vector#{});
        v == Some(Vector#{})
    @test
    def zipWithA02(): Bool =
        let v = Vector.zipWithA((x, y) -> if (x + y > 5) None else Some(x + y), Vector#{1}, Vector#{});
        v == Some(Vector#{})

    @test
    def zipWithA03(): Bool =
        let v = Vector.zipWithA((x, y) -> if (x + y > 5) None else Some(x + y), Vector#{1}, Vector#{1});
        v == Some(Vector#{2})

    @test
    def zipWithA04(): Bool =
        let v = Vector.zipWithA((x, y) -> if (x + y > 5) None else Some(x + y), Vector#{1, 2}, Vector#{1, 2});
        v == Some(Vector#{2, 4})

    @test
    def zipWithA05(): Bool =
        let v = Vector.zipWithA((x, y) -> if (x + y > 5) None else Some(x + y), Vector#{1, 2, 3}, Vector#{1, 2, 3});
        v == None

    @test
    def zipWithA06(): Bool =
        let v = Vector.zipWithA((x, y) -> if (x + y > 5) None else Some(x + y), Vector#{1, 2, 3}, Vector#{1, 1, 1});
        v == Some(Vector#{2, 3, 4})

    /////////////////////////////////////////////////////////////////////////////
    // flatMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def flatMap01(): Bool =
        let v = Vector.flatMap(i -> Vector.repeat(i, i), (Vector#{}: Vector[Int32]));
        v == Vector#{}

    @test
    def flatMap02(): Bool =
        let v = Vector.flatMap(i -> Vector.repeat(i, i), (Vector#{}: Vector[Int32]));
        v == Vector#{}

    @test
    def flatMap03(): Bool =
        let v = Vector.flatMap(i -> Vector.repeat(i, i), Vector#{1});
        v == Vector#{1}

    @test
    def flatMap04(): Bool =
        let v = Vector.flatMap(i -> Vector.repeat(i, i), Vector#{2});
        v == Vector#{2, 2}

    @test
    def flatMap05(): Bool =
        let v = Vector.flatMap(i -> Vector.repeat(i, i), Vector#{1, 2});
        v == Vector#{1, 2, 2}

    @test
    def flatMap06(): Bool =
        let v = Vector.flatMap(i -> Vector.repeat(i, i), Vector#{2, 3});
        v == Vector#{2, 2, 3, 3, 3}

    /////////////////////////////////////////////////////////////////////////////
    // reverse                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reverse01(): Bool =
        let v = Vector.reverse(Vector#{}: Vector[Int32]);
        v == Vector#{}

    @test
    def reverse02(): Bool =
        let v = Vector.reverse(Vector#{1});
        v == Vector#{1}

    @test
    def reverse03(): Bool =
        let v = Vector.reverse(Vector#{1, 2});
        v == Vector#{2, 1}

    @test
    def reverse04(): Bool =
        let v = Vector.reverse(Vector#{1, 1});
        v == Vector#{1, 1}

    @test
    def reverse05(): Bool =
        let v = Vector.reverse(Vector#{1, 2, 3});
        v == Vector#{3, 2, 1}

    @test
    def reverse06(): Bool =
        let v = Vector.reverse(Vector#{1, 2, 3, 4});
        v == Vector#{4, 3, 2, 1}

    /////////////////////////////////////////////////////////////////////////////
    // rotateLeft                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def rotateLeft01(): Bool =
        let v = Vector.rotateLeft(0, (Vector#{}: Vector[Int32]));
        v == Vector#{}

    @test
    def rotateLeft02(): Bool =
        let v = Vector.rotateLeft(1, (Vector#{}: Vector[Int32]));
        v == Vector#{}

    @test
    def rotateLeft03(): Bool =
        let v = Vector.rotateLeft(0, Vector#{1});
        v == Vector#{1}

    @test
    def rotateLeft04(): Bool =
        let v = Vector.rotateLeft(0, Vector#{1, 2});
        v == Vector#{1, 2}

    @test
    def rotateLeft05(): Bool =
        let v = Vector.rotateLeft(1, Vector#{1, 2});
        v == Vector#{2, 1}

    @test
    def rotateLeft06(): Bool =
        let v = Vector.rotateLeft(2, Vector#{1, 2});
        v == Vector#{1, 2}

    @test
    def rotateLeft07(): Bool =
        let v = Vector.rotateLeft(3, Vector#{1, 2});
        v == Vector#{2, 1}

    @test
    def rotateLeft08(): Bool =
        let v = Vector.rotateLeft(-1, Vector#{1, 2});
        v == Vector#{2, 1}

    @test
    def rotateLeft09(): Bool =
        let v = Vector.rotateLeft(0, Vector#{1, 2, 3});
        v == Vector#{1, 2, 3}

    @test
    def rotateLeft10(): Bool =
        let v = Vector.rotateLeft(1, Vector#{1, 2, 3});
        v == Vector#{2, 3, 1}

    @test
    def rotateLeft11(): Bool =
        let v = Vector.rotateLeft(2, Vector#{1, 2, 3});
        v == Vector#{3, 1, 2}

    /////////////////////////////////////////////////////////////////////////////
    // rotateRight                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def rotateRight01(): Bool =
        let v = Vector.rotateRight(0, (Vector#{}: Vector[Int32]));
        v == Vector#{}

    @test
    def rotateRight02(): Bool =
        let v = Vector.rotateRight(1, (Vector#{}: Vector[Int32]));
        v == Vector#{}

    @test
    def rotateRight03(): Bool =
        let v = Vector.rotateRight(0, Vector#{1});
        v == Vector#{1}

    @test
    def rotateRight04(): Bool =
        let v = Vector.rotateRight(0, Vector#{1, 2});
        v == Vector#{1, 2}

    @test
    def rotateRight05(): Bool =
        let v = Vector.rotateRight(1, Vector#{1, 2});
        v == Vector#{2, 1}

    @test
    def rotateRight06(): Bool =
        let v = Vector.rotateRight(2, Vector#{1, 2});
        v == Vector#{1, 2}

    @test
    def rotateRight07(): Bool =
        let v = Vector.rotateRight(3, Vector#{1, 2});
        v == Vector#{2, 1}

    @test
    def rotateRight08(): Bool =
        let v = Vector.rotateRight(-1, Vector#{1, 2});
        v == Vector#{2, 1}

    @test
    def rotateRight09(): Bool =
        let v = Vector.rotateRight(0, Vector#{1, 2, 3});
        v == Vector#{1, 2, 3}

    @test
    def rotateRight10(): Bool =
        let v = Vector.rotateRight(1, Vector#{1, 2, 3});
        v == Vector#{3, 1, 2}

    @test
    def rotateRight11(): Bool =
        let v = Vector.rotateRight(2, Vector#{1, 2, 3});
        v == Vector#{2, 3, 1}

    /////////////////////////////////////////////////////////////////////////////
    // update                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def update01(): Bool =
        let v = Vector.update(0, 2, (Vector#{}: Vector[Int32]));
        v == Vector#{}

    @test
    def update02(): Bool =
        let v = Vector.update(-1, 2, Vector#{1});
        v == Vector#{1}

    @test
    def update03(): Bool =
        let v = Vector.update(0, 2, Vector#{1});
        v == Vector#{2}

    @test
    def update04(): Bool =
        let v = Vector.update(1, 2, Vector#{1});
        v == Vector#{1}

    @test
    def update05(): Bool =
        let v = Vector.update(0, 5, Vector#{1, 2});
        v == Vector#{5, 2}

    @test
    def update06(): Bool =
        let v = Vector.update(1, 5, Vector#{1, 2});
        v == Vector#{1, 5}

    @test
    def update07(): Bool =
        let v = Vector.update(2, 5, Vector#{1, 2});
        v == Vector#{1, 2}

    /////////////////////////////////////////////////////////////////////////////
    // replace                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def replace01(): Bool =
        let v = Vector.replace(from = 3, to = 4, (Vector#{}: Vector[Int32]));
        v == Vector#{}

    @test
    def replace02(): Bool =
        let v = Vector.replace(from = 3, to = 4, Vector#{1});
        v == Vector#{1}

    @test
    def replace03(): Bool =
        let v = Vector.replace(from = 3, to = 4, Vector#{3});
        v == Vector#{4}

    @test
    def replace04(): Bool =
        let v = Vector.replace(from = 3, to = 4, Vector#{4});
        v == Vector#{4}

    @test
    def replace05(): Bool =
        let v = Vector.replace(from = 3, to = 4, Vector#{1, 2});
        v == Vector#{1, 2}

    @test
    def replace06(): Bool =
        let v = Vector.replace(from = 3, to = 4, Vector#{1, 3});
        v == Vector#{1, 4}

    @test
    def replace07(): Bool =
        let v = Vector.replace(from = 3, to = 4, Vector#{3, 4});
        v == Vector#{4, 4}

    @test
    def replace08(): Bool =
        let v = Vector.replace(from = 3, to = 4, Vector#{3, 3});
        v == Vector#{4, 4}

    /////////////////////////////////////////////////////////////////////////////
    // patch                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def patch01(): Bool =
        let v = Vector.patch(0, 0, (Vector#{}: Vector[Int32]), (Vector#{}: Vector[Int32]));
        v == Vector#{}

    @test
    def patch02(): Bool =
        let v = Vector.patch(0, 2, Vector#{1, 2}, Vector#{});
        v == Vector#{}

    @test
    def patch03(): Bool =
        let v = Vector.patch(0, 2, Vector#{}, Vector#{1, 2});
        v == Vector#{1, 2}

    @test
    def patch04(): Bool =
        let v = Vector.patch(-3, 3, Vector#{1, 2, 4}, Vector#{1, 2});
        v == Vector#{1, 2}

    @test
    def patch05(): Bool =
        let v = Vector.patch(2, 3, Vector#{1, 2, 4}, Vector#{1, 2});
        v == Vector#{1, 2}

    @test
    def patch06(): Bool =
        let v = Vector.patch(0, 0, Vector#{}, Vector#{1});
        v == Vector#{1}

    @test
    def patch07(): Bool =
        let v = Vector.patch(1, 0, Vector#{2}, Vector#{1});
        v == Vector#{1}

    @test
    def patch08(): Bool =
        let v = Vector.patch(0, 1, Vector#{2}, Vector#{1});
        v == Vector#{2}

    @test
    def patch09(): Bool =
        let v = Vector.patch(0, 2, Vector#{2, 4}, Vector#{1});
        v == Vector#{2}

    @test
    def patch10(): Bool =
        let v = Vector.patch(-1, 2, Vector#{2, 4}, Vector#{1});
        v == Vector#{4}

    @test
    def patch11(): Bool =
        let v = Vector.patch(-1, 2, Vector#{3, 4}, Vector#{1, 2});
        v == Vector#{4, 2}

    @test
    def patch12(): Bool =
        let v = Vector.patch(1, 2, Vector#{3, 4}, Vector#{1, 2});
        v == Vector#{1, 3}

    @test
    def patch13(): Bool =
        let v = Vector.patch(-2, 2, Vector#{3, 4}, Vector#{1, 2});
        v == Vector#{1, 2}

    @test
    def patch14(): Bool =
        let v = Vector.patch(2, 2, Vector#{3, 4}, Vector#{1, 2});
        v == Vector#{1, 2}

    @test
    def patch15(): Bool =
        let v = Vector.patch(1, 1, Vector#{3}, Vector#{1, 2});
        v == Vector#{1, 3}

    @test
    def patch16(): Bool =
        let v = Vector.patch(0, 2, Vector#{3, 4}, Vector#{1, 2});
        v == Vector#{3, 4}

    @test
    def patch17(): Bool =
        let v = Vector.patch(0, 1, Vector#{4}, Vector#{1, 2, 3});
        v == Vector#{4, 2, 3}

    @test
    def patch18(): Bool =
        let v = Vector.patch(1, 1, Vector#{4}, Vector#{1, 2, 3});
        v == Vector#{1, 4, 3}

    @test
    def patch19(): Bool =
        let v = Vector.patch(2, 1, Vector#{4}, Vector#{1, 2, 3});
        v == Vector#{1, 2, 4}

    @test
    def patch20(): Bool =
        let v = Vector.patch(0, 2, Vector#{4, 5}, Vector#{1, 2, 3});
        v == Vector#{4, 5, 3}

    @test
    def patch21(): Bool =
        let v = Vector.patch(1, 2, Vector#{4, 5}, Vector#{1, 2, 3});
        v == Vector#{1, 4, 5}

    @test
    def patch22(): Bool =
        let v = Vector.patch(0, 2, Vector#{4, 5, 6}, Vector#{1, 2, 3});
        v == Vector#{4, 5, 3}

    @test
    def patch23(): Bool =
        let v = Vector.patch(0, 3, Vector#{4, 5, 6}, Vector#{1, 2, 3});
        v == Vector#{4, 5, 6}

    @test
    def patch24(): Bool =
        let v = Vector.patch(2, 4, Vector#{14, 15, 16, 17}, Vector#{1, 2, 3, 4, 5, 6, 7});
        v == Vector#{1, 2, 14, 15, 16, 17, 7}

    @test
    def patch25(): Bool =
        let v = Vector.patch(-2, 4, Vector#{14, 15, 16, 17}, Vector#{1, 2, 3, 4, 5, 6, 7});
        v == Vector#{16, 17, 3, 4, 5, 6, 7}

    @test
    def patch26(): Bool =
        let v = Vector.patch(4, 5, Vector#{14, 15, 16, 17}, Vector#{1, 2, 3, 4, 5, 6, 7});
        v == Vector#{1, 2, 3, 4, 14, 15, 16}

    @test
    def patch27(): Bool =
        let v = Vector.patch(4, 2, Vector#{14, 15, 16, 17}, Vector#{1, 2, 3, 4, 5, 6, 7});
        v == Vector#{1, 2, 3, 4, 14, 15, 7}

    @test
    def patch28(): Bool =
        let v = Vector.patch(-1, 10, Vector#{-1, -2, -3, -4, -5, -6, -7, -8}, Vector#{1, 2, 3, 4, 5, 6, 7});
        v == Vector#{-2, -3, -4, -5, -6, -7, -8}

    /////////////////////////////////////////////////////////////////////////////
    // intersperse                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def intersperse01(): Bool =
        let v = Vector.intersperse(11, (Vector#{}: Vector[Int32]));
        v == Vector#{}

    @test
    def intersperse02(): Bool =
        let v = Vector.intersperse(11, Vector#{1});
        v == Vector#{1}

    @test
    def intersperse03(): Bool =
        let v = Vector.intersperse(11, Vector#{1, 2});
        v == Vector#{1, 11, 2}

    @test
    def intersperse04(): Bool =
        let v = Vector.intersperse(11, Vector#{1, 2, 3});
        v == Vector#{1, 11, 2, 11, 3}

    @test
    def intersperse05(): Bool =
        let v = Vector.intersperse(11, Vector#{1, 2, 3, 4});
        v == Vector#{1, 11, 2, 11, 3, 11, 4}

    /////////////////////////////////////////////////////////////////////////////
    // intercalate                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def intercalate01(): Bool =
        let v = Vector.intercalate(Vector#{}: Vector[Int32], Vector#{}: Vector[Vector[Int32]]);
        v == Vector#{}

    @test
    def intercalate02(): Bool =
        let v = Vector.intercalate(Vector#{}: Vector[Int32], Vector#{Vector#{1}});
        v == Vector#{1}

    @test
    def intercalate03(): Bool =
        let v = Vector.intercalate(Vector#{11, 12, 13}, Vector#{}: Vector[Vector[Int32]]);
        v == Vector#{}

    @test
    def intercalate04(): Bool =
        let v = Vector.intercalate(Vector#{}: Vector[Int32], Vector#{Vector#{1}, Vector#{2, 3}});
        v == Vector#{1, 2, 3}

    @test
    def intercalate05(): Bool =
        let v = Vector.intercalate(Vector#{11, 12, 13}, Vector#{Vector#{1}, Vector#{2, 3}});
        v == Vector#{1, 11, 12, 13, 2, 3}

    @test
    def intercalate06(): Bool =
        let v = Vector.intercalate(Vector#{}: Vector[Int32], Vector#{Vector#{1}, Vector#{2, 3}, Vector#{4}});
        v == Vector#{1, 2, 3, 4}

    @test
    def intercalate07(): Bool =
        let v = Vector.intercalate(Vector#{11, 12, 13}, Vector#{Vector#{1}, Vector#{2, 3}, Vector#{4}});
        v == Vector#{1, 11, 12, 13, 2, 3, 11, 12, 13, 4}

    /////////////////////////////////////////////////////////////////////////////
    // transpose                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def transpose01(): Bool =
        let v = Vector.transpose(Vector#{}: Vector[Vector[Int32]]);
        v == Vector#{}

    @test
    def transpose02(): Bool =
        let v: Vector[Vector[Int32]] = Vector.transpose(Vector#{Vector#{}});
        v == Vector#{Vector#{}}

    @test
    def transpose03(): Bool =
        let v: Vector[Vector[Int32]] = Vector.transpose(Vector#{Vector#{}, Vector#{}});
        v == Vector#{Vector#{}, Vector#{}}

    @test
    def transpose04(): Bool =
        let v: Vector[Vector[Int32]] = Vector.transpose(Vector#{Vector#{}, Vector#{}, Vector#{}});
        v == Vector#{Vector#{}, Vector#{}, Vector#{}}

    @test
    def transpose05(): Bool =
        let v = Vector.transpose(Vector#{Vector#{1}});
        v == Vector#{Vector#{1}}

    @test
    def transpose06(): Bool =
        let v = Vector.transpose(Vector#{Vector#{1, 2}});
        v == Vector#{Vector#{1}, Vector#{2}}

    @test
    def transpose07(): Bool =
        let v = Vector.transpose(Vector#{Vector#{1, 2, 3}});
        v == Vector#{Vector#{1}, Vector#{2}, Vector#{3}}

    @test
    def transpose08(): Bool =
        let v = Vector.transpose(Vector#{Vector#{1, 2, 3, 4}});
        v == Vector#{Vector#{1}, Vector#{2}, Vector#{3}, Vector#{4}}

    @test
    def transpose09(): Bool =
        let v = Vector.transpose(Vector#{Vector#{1}, Vector#{2}});
        v == Vector#{Vector#{1, 2}}

    @test
    def transpose10(): Bool =
        let v = Vector.transpose(Vector#{Vector#{1}, Vector#{2}, Vector#{3}});
        v == Vector#{Vector#{1, 2, 3}}

    @test
    def transpose11(): Bool =
        let v = Vector.transpose(Vector#{Vector#{1}, Vector#{2}, Vector#{3}, Vector#{4}});
        v == Vector#{Vector#{1, 2, 3, 4}}

    @test
    def transpose12(): Bool =
        let v = Vector.transpose(Vector#{Vector#{1, 2}, Vector#{3, 4}});
        v == Vector#{Vector#{1, 3}, Vector#{2, 4}}

    @test
    def transpose13(): Bool =
        let v = Vector.transpose(Vector#{Vector#{1, 2, 3}, Vector#{4, 5, 6}});
        v == Vector#{Vector#{1, 4}, Vector#{2, 5}, Vector#{3, 6}}

    @test
    def transpose14(): Bool =
        let v = Vector.transpose(Vector#{Vector#{1, 2, 3, 4}, Vector#{5, 6, 7, 8}});
        v == Vector#{Vector#{1, 5}, Vector#{2, 6}, Vector#{3, 7}, Vector#{4, 8}}

    @test
    def transpose15(): Bool =
        let v = Vector.transpose(Vector#{Vector#{1, 2, 3, 4, 5}, Vector#{6, 7, 8, 9, 10}});
        v == Vector#{Vector#{1, 6}, Vector#{2, 7}, Vector#{3, 8}, Vector#{4, 9}, Vector#{5, 10}}

    @test
    def transpose16(): Bool =
        let v = Vector.transpose(Vector#{Vector#{1, 2}, Vector#{3, 4}, Vector#{5, 6}});
        v == Vector#{Vector#{1, 3, 5}, Vector#{2, 4, 6}}

    @test
    def transpose17(): Bool =
        let v = Vector.transpose(Vector#{Vector#{1, 2}, Vector#{3, 4}, Vector#{5, 6}, Vector#{7, 8}});
        v == Vector#{Vector#{1, 3, 5, 7}, Vector#{2, 4, 6, 8}}

    @test
    def transpose18(): Bool =
        let v = Vector.transpose(Vector#{Vector#{1, 2}, Vector#{3, 4}, Vector#{5, 6}, Vector#{7, 8}, Vector#{9, 10}});
        v == Vector#{Vector#{1, 3, 5, 7, 9}, Vector#{2, 4, 6, 8, 10}}

    @test
    def transpose19(): Bool =
        let v = Vector.transpose(Vector#{Vector#{1, 2, 3}, Vector#{4, 5, 6}, Vector#{7, 8, 9}});
        v == Vector#{Vector#{1, 4, 7}, Vector#{2, 5, 8}, Vector#{3, 6, 9}}

    @test
    def transpose20(): Bool =
        let v = Vector.transpose(Vector#{Vector#{1, 2, 3}, Vector#{4, 5}, Vector#{7, 8, 9}});
        v == Vector#{Vector#{1, 2, 3}, Vector#{4, 5}, Vector#{7, 8, 9}}

    @test
    def transpose21(): Bool =
        let v = Vector.transpose(Vector#{Vector#{1, 2, 3}, Vector#{}, Vector#{7, 8, 9}});
        v == Vector#{Vector#{1, 2, 3}, Vector#{}, Vector#{7, 8, 9}}

    @test
    def transpose22(): Bool =
        let v = Vector.transpose(Vector#{Vector#{1, 2, 3}, Vector#{4, 5, 6}, Vector#{7, 8, 9, 10}});
        v == Vector#{Vector#{1, 2, 3}, Vector#{4, 5, 6}, Vector#{7, 8, 9, 10}}

    /////////////////////////////////////////////////////////////////////////////
    // isPrefixOf                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def isPrefixOf01(): Bool =
        Vector.isPrefixOf(Vector#{}: Vector[Int32], Vector#{}: Vector[Int32])

    @test
    def isPrefixOf02(): Bool =
        Vector.isPrefixOf(Vector#{1}, Vector#{}: Vector[Int32]) == false

    @test
    def isPrefixOf03(): Bool =
        Vector.isPrefixOf(Vector#{}: Vector[Int32], Vector#{1})

    @test
    def isPrefixOf04(): Bool =
        Vector.isPrefixOf(Vector#{1}, Vector#{1})

    @test
    def isPrefixOf05(): Bool =
        Vector.isPrefixOf(Vector#{2}, Vector#{1}) == false

    @test
    def isPrefixOf06(): Bool =
        Vector.isPrefixOf(Vector#{1, 2}, Vector#{1}) == false

    @test
    def isPrefixOf07(): Bool =
        Vector.isPrefixOf(Vector#{}: Vector[Int32], Vector#{1, 2})

    @test
    def isPrefixOf08(): Bool =
        Vector.isPrefixOf(Vector#{1}, Vector#{1, 2})

    @test
    def isPrefixOf09(): Bool =
        Vector.isPrefixOf(Vector#{2}, Vector#{1, 2}) == false

    @test
    def isPrefixOf10(): Bool =
        Vector.isPrefixOf(Vector#{1, 2}, Vector#{1, 2})

    @test
    def isPrefixOf11(): Bool =
        Vector.isPrefixOf(Vector#{1, 3}, Vector#{1, 2}) == false

    @test
    def isPrefixOf12(): Bool =
        Vector.isPrefixOf(Vector#{1, 2, 3}, Vector#{1, 2}) == false

    @test
    def isPrefixOf13(): Bool =
        Vector.isPrefixOf(Vector#{1, 2, 3}, Vector#{89, 11, 1, 2, 3, 4}) == false

    /////////////////////////////////////////////////////////////////////////////
    // isInfixOf                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def isInfixOf01(): Bool =
        Vector.isInfixOf(Vector#{}: Vector[Int32], Vector#{}: Vector[Int32])

    @test
    def isInfixOf02(): Bool =
        Vector.isInfixOf(Vector#{1}, Vector#{}: Vector[Int32]) == false

    @test
    def isInfixOf03(): Bool =
        Vector.isInfixOf(Vector#{}: Vector[Int32], Vector#{1})

    @test
    def isInfixOf04(): Bool =
        Vector.isInfixOf(Vector#{1}, Vector#{1})

    @test
    def isInfixOf05(): Bool =
        Vector.isInfixOf(Vector#{2}, Vector#{1}) == false

    @test
    def isInfixOf06(): Bool =
        Vector.isInfixOf(Vector#{1, 2}, Vector#{1}) == false

    @test
    def isInfixOf07(): Bool =
        Vector.isInfixOf(Vector#{}: Vector[Int32], Vector#{1, 2})

    @test
    def isInfixOf08(): Bool =
        Vector.isInfixOf(Vector#{1}, Vector#{1, 2})

    @test
    def isInfixOf09(): Bool =
        Vector.isInfixOf(Vector#{2}, Vector#{1, 2})

    @test
    def isInfixOf10(): Bool =
        Vector.isInfixOf(Vector#{1, 2}, Vector#{1, 2})

    @test
    def isInfixOf11(): Bool =
        Vector.isInfixOf(Vector#{1, 3}, Vector#{1, 2}) == false

    @test
    def isInfixOf12(): Bool =
        Vector.isInfixOf(Vector#{1, 2, 3}, Vector#{1, 2}) == false

    @test
    def isInfixOf13(): Bool =
        Vector.isInfixOf(Vector#{1, 2, 3}, Vector#{89, 11, 1, 2, 3, 4})

    /////////////////////////////////////////////////////////////////////////////
    // isSuffixOf                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def isSuffixOf01(): Bool =
        Vector.isSuffixOf(Vector#{}: Vector[Int32], Vector#{}: Vector[Int32])

    @test
    def isSuffixOf02(): Bool =
        Vector.isSuffixOf(Vector#{1}, Vector#{}: Vector[Int32]) == false

    @test
    def isSuffixOf03(): Bool =
        Vector.isSuffixOf(Vector#{}: Vector[Int32], Vector#{1})

    @test
    def isSuffixOf04(): Bool =
        Vector.isSuffixOf(Vector#{1}, Vector#{1})

    @test
    def isSuffixOf05(): Bool =
        Vector.isSuffixOf(Vector#{2}, Vector#{1}) == false

    @test
    def isSuffixOf06(): Bool =
        Vector.isSuffixOf(Vector#{1, 2}, Vector#{1}) == false

    @test
    def isSuffixOf07(): Bool =
        Vector.isSuffixOf(Vector#{}: Vector[Int32], Vector#{1, 2})

    @test
    def isSuffixOf08(): Bool =
        Vector.isSuffixOf(Vector#{1}, Vector#{1, 2}) == false

    @test
    def isSuffixOf09(): Bool =
        Vector.isSuffixOf(Vector#{2}, Vector#{1, 2})

    @test
    def isSuffixOf10(): Bool =
        Vector.isSuffixOf(Vector#{1, 2}, Vector#{1, 2})

    @test
    def isSuffixOf11(): Bool =
        Vector.isSuffixOf(Vector#{1, 3}, Vector#{1, 2}) == false

    @test
    def isSuffixOf12(): Bool =
        Vector.isSuffixOf(Vector#{1, 2, 3}, Vector#{1, 2}) == false

    @test
    def isSuffixOf13(): Bool =
        Vector.isSuffixOf(Vector#{1, 2, 3}, Vector#{89, 11, 1, 2, 3})

    /////////////////////////////////////////////////////////////////////////////
    // fold                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def fold01(): Bool =
        Vector.fold((Vector#{}): Vector[Unit]) == ()

    @test
    def fold02(): Bool =
        Vector.fold(Vector#{"a"}) == "a"

    @test
    def fold03(): Bool =
        Vector.fold(Vector#{"a", "b", "c"}) == "abc"

    @test
    def fold04(): Bool =
        Vector.fold(Vector#{("yes", "no"), ("no", "yes")}) == ("yesno", "noyes")

    /////////////////////////////////////////////////////////////////////////////
    // foldLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldLeft01(): Bool =
        Vector.foldLeft((i, e) -> (i - e) * (e `Int32.rem` 2 + 1), 100, Vector#{}) == 100

    @test
    def foldLeft02(): Bool =
        Vector.foldLeft((i, e) -> (i - e) * (e `Int32.rem` 2 + 1), 100, Vector#{1}) == 198

    @test
    def foldLeft03(): Bool =
        Vector.foldLeft((i, e) -> (i - e) * (e `Int32.rem` 2 + 1), 100, Vector#{1, 2}) == 196

    @test
    def foldLeft04(): Bool =
        Vector.foldLeft((i, e) -> (i - e) * (e `Int32.rem` 2 + 1), 100, Vector#{1, 2, 3}) == 386

    /////////////////////////////////////////////////////////////////////////////
    // foldRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldRight01(): Bool =
        Vector.foldRight((e, i) -> (i - e) * (e `Int32.rem` 2 + 1), 100, Vector#{}) == 100

    @test
    def foldRight02(): Bool =
        Vector.foldRight((e, i) -> (i - e) * (e `Int32.rem` 2 + 1), 100, Vector#{1}) == 198

    @test
    def foldRight03(): Bool =
        Vector.foldRight((e, i) -> (i - e) * (e `Int32.rem` 2 + 1), 100, Vector#{1, 2}) == 194

    @test
    def foldRight04(): Bool =
        Vector.foldRight((e, i) -> (i - e) * (e `Int32.rem` 2 + 1), 100, Vector#{1, 2, 3}) == 382

    /////////////////////////////////////////////////////////////////////////////
    // foldRightWithCont                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldRightWithCont01(): Bool =
        Vector.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.rem` 2 + 1), 100, Vector#{}) == 100

    @test
    def foldRightWithCont02(): Bool =
        Vector.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.rem` 2 + 1), 100, Vector#{1}) == 198

    @test
    def foldRightWithCont03(): Bool =
        Vector.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.rem` 2 + 1), 100, Vector#{1, 2}) == 194

    @test
    def foldRightWithCont04(): Bool =
        Vector.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.rem` 2 + 1), 100, Vector#{1, 2, 3}) == 382

    /////////////////////////////////////////////////////////////////////////////
    // foldMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldMap01(): Bool =
        Vector.foldMap(x -> 2 * x, Vector#{}) == 0

    @test
    def foldMap02(): Bool =
        Vector.foldMap(x -> 2 * x, Vector#{1, 2}) == 6

    @test
    def foldMap03(): Bool =
        Vector.foldMap(x -> if (x == "a") "b" else x, Vector#{"a"}) == "b"

    @test
    def foldMap04(): Bool =
        Vector.foldMap(x -> if (x == "c") "b" else x, Vector#{"a", "b", "c"}) == "abb"

    @test
    def foldMap05(): Bool =
        Vector.foldMap(Int32.toString, Vector#{1, 2, 3}) == "123"

    /////////////////////////////////////////////////////////////////////////////
    // reduceLeft                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reduceLeft01(): Bool =
        Vector.reduceLeft((a, b) -> a - b, Vector#{}: Vector[Int32]) == None

    @test
    def reduceLeft02(): Bool =
        Vector.reduceLeft((a, b) -> a - b, Vector#{1}) == Some(1)

    @test
    def reduceLeft03(): Bool =
        Vector.reduceLeft((a, b) -> a - b, Vector#{1, 2}) == Some(-1)

    @test
    def reduceLeft04(): Bool =
        Vector.reduceLeft((a, b) -> a - b, Vector#{1, 2, 3}) == Some(-4)

    @test
    def reduceLeft05(): Bool =
        Vector.reduceLeft((a, b) -> a - b, Vector#{1, 2, 3, 4}) == Some(-8)

    /////////////////////////////////////////////////////////////////////////////
    // reduceRight                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reduceRight01(): Bool =
        Vector.reduceRight((a, b) -> a - b, Vector#{}: Vector[Int32]) == None

    @test
    def reduceRight02(): Bool =
        Vector.reduceRight((a, b) -> a - b, Vector#{1}) == Some(1)

    @test
    def reduceRight03(): Bool =
        Vector.reduceRight((a, b) -> a - b, Vector#{1, 2}) == Some(-1)

    @test
    def reduceRight04(): Bool =
        Vector.reduceRight((a, b) -> a - b, Vector#{1, 2, 3}) == Some(2)

    @test
    def reduceRight05(): Bool =
        Vector.reduceRight((a, b) -> a - b, Vector#{1, 2, 3, 4}) == Some(-2)

    /////////////////////////////////////////////////////////////////////////////
    // count                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def count01(): Bool =
        Vector.count(i -> i > 3, Vector#{}) == 0

    @test
    def count02(): Bool =
        Vector.count(i -> i > 3, Vector#{1}) == 0

    @test
    def count03(): Bool =
        Vector.count(i -> i > 3, Vector#{4}) == 1

    @test
    def count04(): Bool =
        Vector.count(i -> i > 3, Vector#{1, 2}) == 0

    @test
    def count05(): Bool =
        Vector.count(i -> i > 3, Vector#{1, 8}) == 1

    @test
    def count06(): Bool =
        Vector.count(i -> i > 3, Vector#{8, 1}) == 1

    @test
    def count07(): Bool =
        Vector.count(i -> i > 3, Vector#{6, 7}) == 2

    /////////////////////////////////////////////////////////////////////////////
    // flatten                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def flatten01(): Bool =
        let v = Vector.flatten((Vector#{}: Vector[Vector[Int32]]));
        v == Vector#{}

    @test
    def flatten02(): Bool =
        let v = Vector.flatten((Vector#{Vector#{}}: Vector[Vector[Int32]]));
        v == Vector#{}

    @test
    def flatten03(): Bool =
        let v = Vector.flatten(Vector#{Vector#{1}});
        v == Vector#{1}

    @test
    def flatten04(): Bool =
        let v = Vector.flatten(Vector#{Vector#{1, 2}});
        v == Vector#{1, 2}

    @test
    def flatten05(): Bool =
        let v = Vector.flatten((Vector#{Vector#{}, Vector#{}}: Vector[Vector[Int32]]));
        v == Vector#{}

    @test
    def flatten06(): Bool =
        let v = Vector.flatten(Vector#{Vector#{1}, Vector#{}});
        v == Vector#{1}

    @test
    def flatten07(): Bool =
        let v = Vector.flatten(Vector#{Vector#{}, Vector#{1}});
        v == Vector#{1}

    @test
    def flatten08(): Bool =
        let v = Vector.flatten(Vector#{Vector#{1}, Vector#{2}});
        v == Vector#{1, 2}

    @test
    def flatten09(): Bool =
        let v = Vector.flatten(Vector#{Vector#{1, 2}, Vector#{3, 4, 5}});
        v == Vector#{1, 2, 3, 4, 5}

    @test
    def flatten10(): Bool =
        let v = Vector.flatten(Vector#{Vector#{1}, Vector#{2, 3}, Vector#{4}});
        v == Vector#{1, 2, 3, 4}

    /////////////////////////////////////////////////////////////////////////////
    // exists                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def exists01(): Bool =
        Vector.exists(i -> i > 3, Vector#{}: Vector[Int32]) == false

    @test
    def exists02(): Bool =
        Vector.exists(i -> i > 3, Vector#{1}) == false

    @test
    def exists03(): Bool =
        Vector.exists(i -> i > 3, Vector#{5}) == true

    @test
    def exists04(): Bool =
        Vector.exists(i -> i > 3, Vector#{1, 2}) == false

    @test
    def exists05(): Bool =
        Vector.exists(i -> i > 3, Vector#{1, 6}) == true

    @test
    def exists06(): Bool =
        Vector.exists(i -> i > 3, Vector#{6, 1}) == true

    @test
    def exists07(): Bool =
        Vector.exists(i -> i > 3, Vector#{16, 6}) == true

    @test
    def exists08(): Bool =
        Vector.exists(i -> i > 3, Vector#{1, -9, 3}) == false

    @test
    def exists09(): Bool =
        Vector.exists(i -> i > 3, Vector#{1, 9, 3}) == true

    /////////////////////////////////////////////////////////////////////////////
    // forAll                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def forAll01(): Bool =
        Vector.forAll(i -> i > 3, Vector#{}: Vector[Int32]) == true

    @test
    def forAll02(): Bool =
        Vector.forAll(i -> i > 3, Vector#{1}) == false

    @test
    def forAll03(): Bool =
        Vector.forAll(i -> i > 3, Vector#{5}) == true

    @test
    def forAll04(): Bool =
        Vector.forAll(i -> i > 3, Vector#{1, 2}) == false

    @test
    def forAll05(): Bool =
        Vector.forAll(i -> i > 3, Vector#{1, 6}) == false

    @test
    def forAll06(): Bool =
        Vector.forAll(i -> i > 3, Vector#{6, 1}) == false

    @test
    def forAll07(): Bool =
        Vector.forAll(i -> i > 3, Vector#{16, 6}) == true

    @test
    def forAll08(): Bool =
        Vector.forAll(i -> i > 3, Vector#{1, -9, 3}) == false

    @test
    def forAll09(): Bool =
        Vector.forAll(i -> i > 3, Vector#{1, 9, 3}) == false

    @test
    def forAll10(): Bool =
        Vector.forAll(i -> i > 3, Vector#{11, 9, 31}) == true

    /////////////////////////////////////////////////////////////////////////////
    // filter                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filter01(): Bool =
        let v = Vector.filter(i -> i > 3, (Vector#{}: Vector[Int32]));
        v == Vector#{}

    @test
    def filter02(): Bool =
        let v = Vector.filter(i -> i > 3, Vector#{2});
        v == Vector#{}

    @test
    def filter03(): Bool =
        let v = Vector.filter(i -> i > 3, Vector#{4});
        v == Vector#{4}

    @test
    def filter04(): Bool =
        let v = Vector.filter(i -> i > 3, Vector#{1, 3});
        v == Vector#{}

    @test
    def filter05(): Bool =
        let v = Vector.filter(i -> i > 3, Vector#{1, 8});
        v == Vector#{8}

    @test
    def filter06(): Bool =
        let v = Vector.filter(i -> i > 3, Vector#{8, 1});
        v == Vector#{8}

    @test
    def filter07(): Bool =
        let v = Vector.filter(i -> i > 3, Vector#{8, 9});
        v == Vector#{8, 9}

    @test
    def filter08(): Bool =
        let v = Vector.filter(i -> i > 3, Vector#{1, 4, 11, 2, -22, 17});
        v == Vector#{4, 11, 17}

    /////////////////////////////////////////////////////////////////////////////
    // partition                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def partition01(): Bool =
        let (a, b) = Vector.partition(i -> i > 3, Vector#{}: Vector[Int32]);
        (a == Vector#{}) and (b == Vector#{})

    @test
    def partition02(): Bool =
        let (a, b) = Vector.partition(i -> i > 3, Vector#{1});
        (a == Vector#{}) and (b == Vector#{1})

    @test
    def partition03(): Bool =
        let (a, b) = Vector.partition(i -> i > 3, Vector#{4});
        (a == Vector#{4}) and (b == Vector#{})

    @test
    def partition04(): Bool =
        let (a, b) = Vector.partition(i -> i > 3, Vector#{1, 2});
        (a ==Vector#{}) and (b == Vector#{1, 2})

    @test
    def partition05(): Bool =
        let (a, b) = Vector.partition(i -> i > 3, Vector#{1, 5});
        (a == Vector#{5}) and (b == Vector#{1})

    @test
    def partition06(): Bool =
        let (a, b) = Vector.partition(i -> i > 3, Vector#{5, 1});
        (a == Vector#{5}) and (b == Vector#{1})

    @test
    def partition07(): Bool =
        let (a, b) = Vector.partition(i -> i > 3, Vector#{5, 8});
        (a == Vector#{5, 8}) and (b == Vector#{}: Vector[Int32])

    @test
    def partition08(): Bool =
        let (a, b) = Vector.partition(i -> i > 3, Vector#{4, -3, -5, 1, 2, 16, 7, 1, 7});
        (a == Vector#{4, 16, 7, 7}) and (b == Vector#{-3, -5, 1, 2, 1})

    /////////////////////////////////////////////////////////////////////////////
    // span                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def span01(): Bool =
        let (a, b) = Vector.span(i -> i > 3, Vector#{}: Vector[Int32]);
        (a == Vector#{}) and (b == Vector#{})

    @test
    def span02(): Bool =
        let (a, b) = Vector.span(i -> i > 3, Vector#{1});
        (a == Vector#{}) and (b == Vector#{1})

    @test
    def span03(): Bool =
        let (a, b) = Vector.span(i -> i > 3, Vector#{4});
        (a == Vector#{4}) and (b == Vector#{})

    @test
    def span04(): Bool =
        let (a, b) = Vector.span(i -> i > 3, Vector#{1, 2});
        (a == Vector#{}) and (b == Vector#{1, 2})

    @test
    def span05(): Bool =
        let (a, b) = Vector.span(i -> i > 3, Vector#{1, 5});
        (a == Vector#{}) and (b == Vector#{1, 5})

    @test
    def span06(): Bool =
        let (a, b) = Vector.span(i -> i > 3, Vector#{5, 1});
        (a == Vector#{5}) and (b == Vector#{1})

    @test
    def span07(): Bool =
        let (a, b) = Vector.span(i -> i > 3, Vector#{5, 8});
        (a == Vector#{5, 8}) and (b == Vector#{})

    @test
    def span08(): Bool =
        let (a, b) = Vector.span(i -> i > 3, Vector#{4, 6, -3, 11, -5, 1, 2, 16, 7, 1, 7});
        (a == Vector#{4, 6}) and (b == Vector#{-3, 11, -5, 1, 2, 16, 7, 1, 7})

    /////////////////////////////////////////////////////////////////////////////
    // drop                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def drop01(): Bool =
        let v = Vector.drop(-1, Vector#{}: Vector[Int32]);
        v == Vector#{}

    @test
    def drop02(): Bool =
        let v = Vector.drop(0, Vector#{}: Vector[Int32]);
        v == Vector#{}

    @test
    def drop03(): Bool =
        let v = Vector.drop(1, Vector#{}: Vector[Int32]);
        v == Vector#{}

    @test
    def drop04(): Bool =
        let v = Vector.drop(-1, Vector#{1});
        v == Vector#{1}

    @test
    def drop05(): Bool =
        let v = Vector.drop(0, Vector#{1});
        v == Vector#{1}

    @test
    def drop06(): Bool =
        let v = Vector.drop(1, Vector#{1});
        v == Vector#{}

    @test
    def drop07(): Bool =
        let v = Vector.drop(2, Vector#{1});
        v == Vector#{}

    @test
    def drop08(): Bool =
        let v = Vector.drop(0, Vector#{1, 2});
        v == Vector#{1, 2}

    @test
    def drop09(): Bool =
        let v = Vector.drop(1, Vector#{1, 2});
        v == Vector#{2}

    @test
    def drop10(): Bool =
        let v = Vector.drop(2, Vector#{1, 2});
        v == Vector#{}

    @test
    def drop11(): Bool =
        let v = Vector.drop(2, Vector#{1, 2, 3, 4, 5, 6});
        v == Vector#{3, 4, 5, 6}

    @test
    def drop12(): Bool =
        let v = Vector.drop(4, Vector#{1, 2, 3, 4, 5, 6});
        v == Vector#{5, 6}

    /////////////////////////////////////////////////////////////////////////////
    // dropLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def dropLeft01(): Bool =
        let v = Vector.dropLeft(-1, Vector#{}: Vector[Int32]);
        v == Vector#{}

    @test
    def dropLeft02(): Bool =
        let v = Vector.dropLeft(0, Vector#{}: Vector[Int32]);
        v == Vector#{}

    @test
    def dropLeft03(): Bool =
        let v = Vector.dropLeft(1, Vector#{}: Vector[Int32]);
        v == Vector#{}

    @test
    def dropLeft04(): Bool =
        let v = Vector.dropLeft(-1, Vector#{1});
        v == Vector#{1}

    @test
    def dropLeft05(): Bool =
        let v = Vector.dropLeft(0, Vector#{1});
        v == Vector#{1}

    @test
    def dropLeft06(): Bool =
        let v = Vector.dropLeft(1, Vector#{1});
        v == Vector#{}

    @test
    def dropLeft07(): Bool =
        let v = Vector.dropLeft(2, Vector#{1});
        v == Vector#{}

    @test
    def dropLeft08(): Bool =
        let v = Vector.dropLeft(0, Vector#{1, 2});
        v == Vector#{1, 2}

    @test
    def dropLeft09(): Bool =
        let v = Vector.dropLeft(1, Vector#{1, 2});
        v == Vector#{2}

    @test
    def dropLeft10(): Bool =
        let v = Vector.dropLeft(2, Vector#{1, 2});
        v == Vector#{}

    @test
    def dropLeft11(): Bool =
        let v = Vector.dropLeft(2, Vector#{1, 2, 3, 4, 5, 6});
        v == Vector#{3, 4, 5, 6}

    @test
    def dropLeft12(): Bool =
        let v = Vector.dropLeft(4, Vector#{1, 2, 3, 4, 5, 6});
        v == Vector#{5, 6}

    /////////////////////////////////////////////////////////////////////////////
    // dropRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def dropRight01(): Bool =
        let v = Vector.dropRight(-1, Vector#{}: Vector[Int32]);
        v == Vector#{}

    @test
    def dropRight02(): Bool =
        let v = Vector.dropRight(0, Vector#{}: Vector[Int32]);
        v == Vector#{}

    @test
    def dropRight03(): Bool =
        let v = Vector.dropRight(1, Vector#{}: Vector[Int32]);
        v == Vector#{}

    @test
    def dropRight04(): Bool =
        let v = Vector.dropRight(-1, Vector#{1});
        v == Vector#{1}

    @test
    def dropRight05(): Bool =
        let v = Vector.dropRight(0, Vector#{1});
        v == Vector#{1}

    @test
    def dropRight06(): Bool =
        let v = Vector.dropRight(1, Vector#{1});
        v == Vector#{}

    @test
    def dropRight07(): Bool =
        let v = Vector.dropRight(2, Vector#{1});
        v == Vector#{}

    @test
    def dropRight08(): Bool =
        let v = Vector.dropRight(0, Vector#{1, 2});
        v == Vector#{1, 2}

    @test
    def dropRight09(): Bool =
        let v = Vector.dropRight(1, Vector#{1, 2});
        v == Vector#{1}

    @test
    def dropRight10(): Bool =
        let v = Vector.dropRight(2, Vector#{1, 2});
        v == Vector#{}

    @test
    def dropRight11(): Bool =
        let v = Vector.dropRight(2, Vector#{1, 2, 3, 4, 5, 6});
        v == Vector#{1, 2, 3, 4}

    @test
    def dropRight12(): Bool =
        let v = Vector.dropRight(4, Vector#{1, 2, 3, 4, 5, 6});
        v == Vector#{1, 2}

    /////////////////////////////////////////////////////////////////////////////
    // dropWhile                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def dropWhile01(): Bool =
        let v = Vector.dropWhile(i -> i > 3, Vector#{}: Vector[Int32]);
        v == Vector#{}

    @test
    def dropWhile02(): Bool =
        let v = Vector.dropWhile(i -> i > 3, Vector#{1});
        v == Vector#{1}

    @test
    def dropWhile03(): Bool =
        let v = Vector.dropWhile(i -> i > 3, Vector#{4});
        v == Vector#{}

    @test
    def dropWhile04(): Bool =
        let v = Vector.dropWhile(i -> i > 3, Vector#{1, 2});
        v == Vector#{1, 2}

    @test
    def dropWhile05(): Bool =
        let v = Vector.dropWhile(i -> i > 3, Vector#{1, 5});
        v == Vector#{1, 5}

    @test
    def dropWhile06(): Bool =
        let v = Vector.dropWhile(i -> i > 3, Vector#{5, 1});
        v == Vector#{1}

    @test
    def dropWhile07(): Bool =
        let v = Vector.dropWhile(i -> i > 3, Vector#{5, 8});
        v == Vector#{}

    @test
    def dropWhile08(): Bool =
        let v = Vector.dropWhile(i -> i > 3, Vector#{4, 6, -3, 11, -5, 1, 2, 16, 7, 1, 7});
        v == Vector#{-3, 11, -5, 1, 2, 16, 7, 1, 7}

    /////////////////////////////////////////////////////////////////////////////
    // dropWhileLeft                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def dropWhileLeft01(): Bool =
        let v = Vector.dropWhileLeft(i -> i > 3, Vector#{}: Vector[Int32]);
        v == Vector#{}

    @test
    def dropWhileLeft02(): Bool =
        let v = Vector.dropWhileLeft(i -> i > 3, Vector#{1});
        v == Vector#{1}

    @test
    def dropWhileLeft03(): Bool =
        let v = Vector.dropWhileLeft(i -> i > 3, Vector#{4});
        v == Vector#{}

    @test
    def dropWhileLeft04(): Bool =
        let v = Vector.dropWhileLeft(i -> i > 3, Vector#{1, 2});
        v == Vector#{1, 2}

    @test
    def dropWhileLeft05(): Bool =
        let v = Vector.dropWhileLeft(i -> i > 3, Vector#{1, 5});
        v == Vector#{1, 5}

    @test
    def dropWhileLeft06(): Bool =
        let v = Vector.dropWhileLeft(i -> i > 3, Vector#{5, 1});
        v == Vector#{1}

    @test
    def dropWhileLeft07(): Bool =
        let v = Vector.dropWhileLeft(i -> i > 3, Vector#{5, 8});
        v == Vector#{}

    @test
    def dropWhileLeft08(): Bool =
        let v = Vector.dropWhileLeft(i -> i > 3, Vector#{4, 6, -3, 11, -5, 1, 2, 16, 7, 1, 7});
        v == Vector#{-3, 11, -5, 1, 2, 16, 7, 1, 7}

    /////////////////////////////////////////////////////////////////////////////
    // dropWhileRight                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def dropWhileRight01(): Bool =
        let v = Vector.dropWhileRight(i -> i > 3, Vector#{}: Vector[Int32]);
        v == Vector#{}

    @test
    def dropWhileRight02(): Bool =
        let v = Vector.dropWhileRight(i -> i > 3, Vector#{1});
        v == Vector#{1}

    @test
    def dropWhileRight03(): Bool =
        let v = Vector.dropWhileRight(i -> i > 3, Vector#{4});
        v == Vector#{}

    @test
    def dropWhileRight04(): Bool =
        let v = Vector.dropWhileRight(i -> i > 3, Vector#{1, 2});
        v == Vector#{1, 2}

    @test
    def dropWhileRight05(): Bool =
        let v = Vector.dropWhileRight(i -> i > 3, Vector#{1, 5});
        v == Vector#{1}

    @test
    def dropWhileRight06(): Bool =
        let v = Vector.dropWhileRight(i -> i > 3, Vector#{5, 1});
        v == Vector#{5, 1}

    @test
    def dropWhileRight07(): Bool =
        let v = Vector.dropWhileRight(i -> i > 3, Vector#{5, 8});
        v == Vector#{}

    @test
    def dropWhileRight08(): Bool =
        let v = Vector.dropWhileRight(i -> i > 3, Vector#{4, 6, -3, 11, -5, 1, 2, 16, 7, 1, 7});
        v == Vector#{4, 6, -3, 11, -5, 1, 2, 16, 7, 1}

    /////////////////////////////////////////////////////////////////////////////
    // take                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def take01(): Bool =
        let v = Vector.take(-1, Vector#{}: Vector[Int32]);
        v == Vector#{}

    @test
    def take02(): Bool =
        let v = Vector.take(0, Vector#{}: Vector[Int32]);
        v == Vector#{}

    @test
    def take03(): Bool =
        let v = Vector.take(1, Vector#{}: Vector[Int32]);
        v == Vector#{}

    @test
    def take04(): Bool =
        let v = Vector.take(-1, Vector#{1});
        v == Vector#{}

    @test
    def take05(): Bool =
        let v = Vector.take(0, Vector#{1});
        v == Vector#{}

    @test
    def take06(): Bool =
        let v = Vector.take(1, Vector#{1});
        v == Vector#{1}

    @test
    def take07(): Bool =
        let v = Vector.take(2, Vector#{1});
        v == Vector#{1}

    @test
    def take08(): Bool =
        let v = Vector.take(0, Vector#{1, 2});
        v == Vector#{}

    @test
    def take09(): Bool =
        let v = Vector.take(1, Vector#{1, 2});
        v == Vector#{1}

    @test
    def take10(): Bool =
        let v = Vector.take(2, Vector#{1, 2});
        v == Vector#{1, 2}

    @test
    def take11(): Bool =
        let v = Vector.take(2, Vector#{1, 2, 3, 4, 5, 6});
        v == Vector#{1, 2}

    @test
    def take12(): Bool =
        let v = Vector.take(4, Vector#{1, 2, 3, 4, 5, 6});
        v == Vector#{1, 2, 3, 4}

    /////////////////////////////////////////////////////////////////////////////
    // takeLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def takeLeft01(): Bool =
        let v = Vector.takeLeft(-1, Vector#{}: Vector[Int32]);
        v == Vector#{}

    @test
    def takeLeft02(): Bool =
        let v = Vector.takeLeft(0, Vector#{}: Vector[Int32]);
        v == Vector#{}

    @test
    def takeLeft03(): Bool =
        let v = Vector.takeLeft(1, Vector#{}: Vector[Int32]);
        v == Vector#{}

    @test
    def takeLeft04(): Bool =
        let v = Vector.takeLeft(-1, Vector#{1});
        v == Vector#{}

    @test
    def takeLeft05(): Bool =
        let v = Vector.takeLeft(0, Vector#{1});
        v == Vector#{}

    @test
    def takeLeft06(): Bool =
        let v = Vector.takeLeft(1, Vector#{1});
        v == Vector#{1}

    @test
    def takeLeft07(): Bool =
        let v = Vector.takeLeft(2, Vector#{1});
        v == Vector#{1}

    @test
    def takeLeft08(): Bool =
        let v = Vector.takeLeft(0, Vector#{1, 2});
        v == Vector#{}

    @test
    def takeLeft09(): Bool =
        let v = Vector.takeLeft(1, Vector#{1, 2});
        v == Vector#{1}

    @test
    def takeLeft10(): Bool =
        let v = Vector.takeLeft(2, Vector#{1, 2});
        v == Vector#{1, 2}

    @test
    def takeLeft11(): Bool =
        let v = Vector.takeLeft(2, Vector#{1, 2, 3, 4, 5, 6});
        v == Vector#{1, 2}

    @test
    def takeLeft12(): Bool =
        let v = Vector.takeLeft(4, Vector#{1, 2, 3, 4, 5, 6});
        v == Vector#{1, 2, 3, 4}

    /////////////////////////////////////////////////////////////////////////////
    // takeRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def takeRight01(): Bool =
        let v = Vector.takeRight(-1, Vector#{}: Vector[Int32]);
        v == Vector#{}

    @test
    def takeRight02(): Bool =
        let v = Vector.takeRight(0, Vector#{}: Vector[Int32]);
        v == Vector#{}

    @test
    def takeRight03(): Bool =
        let v = Vector.takeRight(1, Vector#{}: Vector[Int32]);
        v == Vector#{}

    @test
    def takeRight04(): Bool =
        let v = Vector.takeRight(-1, Vector#{1});
        v == Vector#{}

    @test
    def takeRight05(): Bool =
        let v = Vector.takeRight(0, Vector#{1});
        v == Vector#{}

    @test
    def takeRight06(): Bool =
        let v = Vector.takeRight(1, Vector#{1});
        v == Vector#{1}

    @test
    def takeRight07(): Bool =
        let v = Vector.takeRight(2, Vector#{1});
        v == Vector#{1}

    @test
    def takeRight08(): Bool =
        let v = Vector.takeRight(0, Vector#{1, 2});
        v == Vector#{}

    @test
    def takeRight09(): Bool =
        let v = Vector.takeRight(1, Vector#{1, 2});
        v == Vector#{2}

    @test
    def takeRight10(): Bool =
        let v = Vector.takeRight(2, Vector#{1, 2});
        v == Vector#{1, 2}

    @test
    def takeRight11(): Bool =
        let v = Vector.takeRight(2, Vector#{1, 2, 3, 4, 5, 6});
        v == Vector#{5, 6}

    @test
    def takeRight12(): Bool =
        let v = Vector.takeRight(4, Vector#{1, 2, 3, 4, 5, 6});
        v == Vector#{3, 4, 5, 6}

    /////////////////////////////////////////////////////////////////////////////
    // takeWhile                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def takeWhile01(): Bool =
        let v = Vector.takeWhile(i -> i > 3, Vector#{}: Vector[Int32]);
        v == Vector#{}

    @test
    def takeWhile02(): Bool =
        let v = Vector.takeWhile(i -> i > 3, Vector#{1});
        v == Vector#{}

    @test
    def takeWhile03(): Bool =
        let v = Vector.takeWhile(i -> i > 3, Vector#{4});
        v == Vector#{4}

    @test
    def takeWhile04(): Bool =
        let v = Vector.takeWhile(i -> i > 3, Vector#{1, 2});
        v == Vector#{}

    @test
    def takeWhile05(): Bool =
        let v = Vector.takeWhile(i -> i > 3, Vector#{1, 5});
        v == Vector#{}

    @test
    def takeWhile06(): Bool =
        let v = Vector.takeWhile(i -> i > 3, Vector#{5, 1});
        v == Vector#{5}

    @test
    def takeWhile07(): Bool =
        let v = Vector.takeWhile(i -> i > 3, Vector#{5, 8});
        v == Vector#{5, 8}

    @test
    def takeWhile08(): Bool =
        let v = Vector.takeWhile(i -> i > 3, Vector#{4, 6, -3, 11, -5, 1, 2, 16, 7, 1, 7});
        v == Vector#{4, 6}

    /////////////////////////////////////////////////////////////////////////////
    // takeWhileLeft                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def takeWhileLeft01(): Bool =
        let v = Vector.takeWhileLeft(i -> i > 3, Vector#{}: Vector[Int32]);
        v == Vector#{}

    @test
    def takeWhileLeft02(): Bool =
        let v = Vector.takeWhileLeft(i -> i > 3, Vector#{1});
        v == Vector#{}

    @test
    def takeWhileLeft03(): Bool =
        let v = Vector.takeWhileLeft(i -> i > 3, Vector#{4});
        v == Vector#{4}

    @test
    def takeWhileLeft04(): Bool =
        let v = Vector.takeWhileLeft(i -> i > 3, Vector#{1, 2});
        v == Vector#{}

    @test
    def takeWhileLeft05(): Bool =
        let v = Vector.takeWhileLeft(i -> i > 3, Vector#{1, 5});
        v == Vector#{}

    @test
    def takeWhileLeft06(): Bool =
        let v = Vector.takeWhileLeft(i -> i > 3, Vector#{5, 1});
        v == Vector#{5}

    @test
    def takeWhileLeft07(): Bool =
        let v = Vector.takeWhileLeft(i -> i > 3, Vector#{5, 8});
        v == Vector#{5, 8}

    @test
    def takeWhileLeft08(): Bool =
        let v = Vector.takeWhileLeft(i -> i > 3, Vector#{4, 6, -3, 11, -5, 1, 2, 16, 7, 1, 7});
        v == Vector#{4, 6}

    /////////////////////////////////////////////////////////////////////////////
    // takeWhileRight                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def takeWhileRight01(): Bool =
        let v = Vector.takeWhileRight(i -> i > 3, Vector#{}: Vector[Int32]);
        v == Vector#{}

    @test
    def takeWhileRight02(): Bool =
        let v = Vector.takeWhileRight(i -> i > 3, Vector#{1});
        v == Vector#{}

    @test
    def takeWhileRight03(): Bool =
        let v = Vector.takeWhileRight(i -> i > 3, Vector#{4});
        v == Vector#{4}

    @test
    def takeWhileRight04(): Bool =
        let v = Vector.takeWhileRight(i -> i > 3, Vector#{1, 2});
        v == Vector#{}

    @test
    def takeWhileRight05(): Bool =
        let v = Vector.takeWhileRight(i -> i > 3, Vector#{1, 5});
        v == Vector#{5}

    @test
    def takeWhileRight06(): Bool =
        let v = Vector.takeWhileRight(i -> i > 3, Vector#{5, 1});
        v == Vector#{}

    @test
    def takeWhileRight07(): Bool =
        let v = Vector.takeWhileRight(i -> i > 3, Vector#{5, 8});
        v == Vector#{5, 8}

    @test
    def takeWhileRight08(): Bool =
        let v = Vector.takeWhileRight(i -> i > 3, Vector#{4, 6, -3, 11, -5, 1, 2, 16, 7, 1, 7});
        v == Vector#{7}

    /////////////////////////////////////////////////////////////////////////////
    // splitAt                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def splitAt01(): Bool =
        let v = Vector#{}: Vector[Int32];
        Vector.splitAt(0, v) == (Vector#{}, Vector#{})

    @test
    def splitAt02(): Bool =
        let v = Vector#{1, 5, 19, 3};
        Vector.splitAt(3, v) == (Vector#{1, 5, 19}, Vector#{3})

    @test
    def splitAt03(): Bool =
        let v = Vector#{1, 5, 19, 3};
        Vector.splitAt(-3, v) == (Vector#{}, v)

    @test
    def splitAt04(): Bool =
        let v = Vector#{1, 5, 19, 3};
        Vector.splitAt(5, v) == (v, Vector#{})

    /////////////////////////////////////////////////////////////////////////////
    // groupBy                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def groupBy01(): Bool =
        let v = Vector.groupBy((a, b) -> a > 3 or b > 8, Vector#{}: Vector[Int32]);
        v == Vector#{}

    @test
    def groupBy02(): Bool =
        let v = Vector.groupBy((a, b) -> a > 3 or b > 8, Vector#{1});
        v == Vector#{Vector#{1}}

    @test
    def groupBy03(): Bool =
        let v = Vector.groupBy((a, b) -> a > 3 or b > 8, Vector#{1, 4});
        v == Vector#{Vector#{1}, Vector#{4}}

    @test
    def groupBy04(): Bool =
        let v = Vector.groupBy((a, b) -> a > 3 or b > 8, Vector#{1, 9});
        v == Vector#{Vector#{1, 9}}

    @test
    def groupBy05(): Bool =
        let v = Vector.groupBy((a, b) -> a > 3 or b > 8, Vector#{1, 4, 7, 6, 9, 2, 4, 4, 8, 16});
        v == Vector#{Vector#{1, 9, 16}, Vector#{4, 7, 6, 4, 4, 8}, Vector#{2}}

    @test
    def groupBy06(): Bool =
        let v = Vector.groupBy((a, b) -> a > -6 or a*b >= 0, Vector#{-1, -11, 4, -11, 0, 8, 2, 1, -3, -24});
        v == Vector#{Vector#{-1, -11, -11, 0, -3, -24}, Vector#{4, 8, 2, 1}}

    @test
    def groupBy07(): Bool =
        let v = Vector.groupBy((a, b) -> a < 0 or (a > 10 or (b > 10 or a == b)), Vector#{-5, 6, 11, 8, 8, -11, -1, 0, 4, -1});
        v == Vector#{Vector#{-5, 11, -11, -1, -1}, Vector#{6}, Vector#{8, 8}, Vector#{0}, Vector#{4}}

    /////////////////////////////////////////////////////////////////////////////
    // zip                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def zip01(): Bool =
        let v = Vector.zip(Vector#{}: Vector[Int32], Vector#{}: Vector[Int32]);
        v == Vector#{}

    @test
    def zip02(): Bool =
        let v = Vector.zip(Vector#{1}, Vector#{}: Vector[Int32]);
        v == Vector#{}

    @test
    def zip03(): Bool =
        let v = Vector.zip(Vector#{}: Vector[Int32], Vector#{2});
        v == Vector#{}

    @test
    def zip04(): Bool =
        let v = Vector.zip(Vector#{1}, Vector#{2});
        v == Vector#{(1, 2)}

    @test
    def zip05(): Bool =
        let v = Vector.zip(Vector#{1, 3}, Vector#{2, 4});
        v == Vector#{(1, 2), (3, 4)}

    @test
    def zip06(): Bool =
        let v = Vector.zip(Vector#{1, 3, 5}, Vector#{2, 4, 6});
        v == Vector#{(1, 2), (3, 4), (5, 6)}

    @test
    def zip07(): Bool =
        let v = Vector.zip(Vector#{1, 3, 5, 7}, Vector#{2, 4, 6, 8});
        v == Vector#{(1, 2), (3, 4), (5, 6), (7, 8)}

    /////////////////////////////////////////////////////////////////////////////
    // zipWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def zipWith01(): Bool =
        let v = Vector.zipWith((a, b) -> if (b) a + 1 else a, Vector#{}: Vector[Int32], Vector#{}: Vector[Bool]);
        v == Vector#{}

    @test
    def zipWith02(): Bool =
        let v = Vector.zipWith((a, b) -> if (b) a + 1 else a, Vector#{1}, Vector#{}: Vector[Bool]);
        v == Vector#{}

    @test
    def zipWith03(): Bool =
        let v = Vector.zipWith((a, b) -> if (b) a + 1 else a, Vector#{}: Vector[Int32], Vector#{true});
        v == Vector#{}

    @test
    def zipWith04(): Bool =
        let v = Vector.zipWith((a, b) -> if (b) a + 1 else a, Vector#{1}, Vector#{true});
        v == Vector#{2}

    @test
    def zipWith05(): Bool =
        let v = Vector.zipWith((a, b) -> if (b) a + 1 else a, Vector#{1}, Vector#{false});
        v == Vector#{1}

    @test
    def zipWith06(): Bool =
        let v = Vector.zipWith((a, b) -> if (b) a + 1 else a,
                        Vector#{1, 2, 3, 4, 5, 6, 7, 8},
                        Vector#{false, true, true, false, false, true, true, true});
        v == Vector#{1, 3, 4, 4, 5, 7, 8, 9}

    /////////////////////////////////////////////////////////////////////////////
    // unzip                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def unzip01(): Bool =
        let (a, b) = Vector.unzip((Vector#{}): Vector[(Unit, Unit)]);
        (a == Vector#{}) and (b == Vector#{})

    @test
    def unzip02(): Bool =
        let (a, b) = Vector.unzip(Vector#{(1, true)});
        (a == Vector#{1}) and (b == Vector#{true})

    @test
    def unzip03(): Bool =
        let (a,b) = Vector.unzip(Vector#{(1, true), (2, true)});
        (a == Vector#{1, 2}) and (b == Vector#{true, true})

    @test
    def unzip04(): Bool =
        let (a,b) = Vector.unzip(Vector#{(1, true), (2, true), (3, false)});
        (a == Vector#{1, 2, 3}) and (b == Vector#{true, true, false})

    @test
    def unzip05(): Bool =
        fst(Vector.unzip(Vector#{(1, "1"), (2, "2"), (3, "3")})) == Vector#{1, 2, 3}


    /////////////////////////////////////////////////////////////////////////////
    // fold2                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def fold201(): Bool =
        Vector.fold2((c, a, b) -> if (b) a + c else a * c, 4, Vector#{}: Vector[Int32], Vector#{}: Vector[Bool]) == 4

    @test
    def fold202(): Bool =
        Vector.fold2((c, a, b) -> if (b) a + c else a * c, 4, Vector#{1}, Vector#{}) == 4

    @test
    def fold203(): Bool =
        Vector.fold2((c, a, b) -> if (b) a + c else a * c, 4, Vector#{}, Vector#{true}) == 4

    @test
    def fold204(): Bool =
        Vector.fold2((c, a, b) -> if (b) a + c else a * c, 4, Vector#{2}, Vector#{true}) == 6

    @test
    def fold205(): Bool =
        Vector.fold2((c, a, b) -> if (b) a + c else a * c, 4, Vector#{2}, Vector#{false}) == 8

    @test
    def fold206(): Bool =
        Vector.fold2((c, a, b) -> if (b) a + c else a * c, 4, Vector#{3, 2}, Vector#{true, true}) == 9

    @test
    def fold207(): Bool =
        Vector.fold2((c, a, b) -> if (b) a + c else a * c, 4, Vector#{3, 2}, Vector#{true, false}) == 14

    @test
    def fold208(): Bool =
        Vector.fold2((c, a, b) -> if (b) a + c else a * c, 4, Vector#{3, 2}, Vector#{false, true}) == 14

    @test
    def fold209(): Bool =
        Vector.fold2((c, a, b) -> if (b) a + c else a * c, 4, Vector#{3, 2}, Vector#{false, false}) == 24

    @test
    def fold210(): Bool =
        Vector.fold2((c, a, b) -> if (b) a + c else a * c, 4, Vector#{6, -4, 3, 2},
            Vector#{true, false, false, true}) == -118

    /////////////////////////////////////////////////////////////////////////////
    // foldLeft2                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldLeft201(): Bool =
        Vector.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, Vector#{}: Vector[Int32], Vector#{}: Vector[Bool]) == 4

    @test
    def foldLeft202(): Bool =
        Vector.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, Vector#{1}, Vector#{}: Vector[Bool]) == 4

    @test
    def foldLeft203(): Bool =
        Vector.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, Vector#{}: Vector[Int32], Vector#{true}) == 4

    @test
    def foldLeft204(): Bool =
        Vector.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, Vector#{2}, Vector#{true}) == 6

    @test
    def foldLeft205(): Bool =
        Vector.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, Vector#{2}, Vector#{false}) == 8

    @test
    def foldLeft206(): Bool =
        Vector.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, Vector#{3, 2}, Vector#{true, true}) == 9

    @test
    def foldLeft207(): Bool =
        Vector.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, Vector#{3, 2}, Vector#{true, false}) == 14

    @test
    def foldLeft208(): Bool =
        Vector.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, Vector#{3, 2}, Vector#{false, true}) == 14

    @test
    def foldLeft209(): Bool =
        Vector.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, Vector#{3, 2}, Vector#{false, false}) == 24

    @test
    def foldLeft210(): Bool =
        Vector.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, Vector#{6, -4, 3, 2},
            Vector#{true, false, false, true}) == -118

    /////////////////////////////////////////////////////////////////////////////
    // foldRight2                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldRight201(): Bool =
        Vector.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, Vector#{}: Vector[Int32], Vector#{}: Vector[Bool]) == 4

    @test
    def foldRight202(): Bool =
        Vector.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, Vector#{1}, Vector#{}: Vector[Bool]) == 4

    @test
    def foldRight203(): Bool =
        Vector.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, Vector#{}: Vector[Int32], Vector#{true}) == 4

    @test
    def foldRight204(): Bool =
        Vector.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, Vector#{2}, Vector#{true}) == 6

    @test
    def foldRight205(): Bool =
        Vector.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, Vector#{2}, Vector#{false}) == 8

    @test
    def foldRight206(): Bool =
        Vector.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, Vector#{3, 2}, Vector#{true, true}) == 9

    @test
    def foldRight207(): Bool =
        Vector.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, Vector#{3, 2}, Vector#{true, false}) == 11

    @test
    def foldRight208(): Bool =
        Vector.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, Vector#{3, 2}, Vector#{false, true}) == 18

    @test
    def foldRight209(): Bool =
        Vector.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, Vector#{3, 2}, Vector#{false, false}) == 24

    @test
    def foldRight210(): Bool =
        Vector.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, Vector#{6, -4, 3, 2},
            Vector#{true, false, false, true}) == -66

    /////////////////////////////////////////////////////////////////////////////
    // filterMap                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filterMap01(): Bool =
        let v = Vector.filterMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Vector#{}: Vector[Int32]);
        v == Vector#{}

    @test
    def filterMap02(): Bool =
        let v = Vector.filterMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Vector#{1});
        v == Vector#{}

    @test
    def filterMap03(): Bool =
        let v = Vector.filterMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Vector#{2});
        v == Vector#{1}

    @test
    def filterMap04(): Bool =
        let v = Vector.filterMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Vector#{1, 3});
        v == Vector#{}

    @test
    def filterMap05(): Bool =
        let v = Vector.filterMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Vector#{1, 4});
        v == Vector#{2}

    @test
    def filterMap06(): Bool =
        let v = Vector.filterMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Vector#{6, -1});
        v == Vector#{3}

    @test
    def filterMap07(): Bool =
        let v = Vector.filterMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Vector#{8, 6});
        v == Vector#{4, 3}

    @test
    def filterMap08(): Bool =
        let v = Vector.filterMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Vector#{0, 1, 2, 3, 4, 5, 10});
        v == Vector#{0, 1, 2, 5}

    /////////////////////////////////////////////////////////////////////////////
    // findMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def findMap01(): Bool =
        Vector.findMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Vector#{}: Vector[Int32]) == None

    @test
    def findMap02(): Bool =
        Vector.findMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Vector#{1}) == None

    @test
    def findMap03(): Bool =
        Vector.findMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Vector#{2}) == Some(1)

    @test
    def findMap04(): Bool =
        Vector.findMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Vector#{1, 3}) == None

    @test
    def findMap05(): Bool =
        Vector.findMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Vector#{1, 4}) == Some(2)

    @test
    def findMap06(): Bool =
        Vector.findMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Vector#{6, -1}) == Some(3)

    @test
    def findMap07(): Bool =
        Vector.findMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Vector#{8, 6}) == Some(4)

    @test
    def findMap08(): Bool =
        Vector.findMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Vector#{0, 1, 2, 3, 4, 5, 10}) == Some(0)

    /////////////////////////////////////////////////////////////////////////////
    // toSet                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toSet01(): Bool =
        Vector.toSet(Vector#{}: Vector[Int32]) == Set#{}

    @test
    def toSet02(): Bool =
        Vector.toSet(Vector#{1}) == Set#{1}

    @test
    def toSet03(): Bool =
        Vector.toSet(Vector#{1, 2}) == Set#{1, 2}

    @test
    def toSet04(): Bool =
        Vector.toSet(Vector#{1, 1}) == Set#{1}

    @test
    def toSet05(): Bool =
        Vector.toSet(Vector#{1, 2, 3}) == Set#{1, 2, 3}

    @test
    def toSet06(): Bool =
        Vector.toSet(Vector#{1, 2, 1}) == Set#{1, 2}

    @test
    def toSet07(): Bool =
        Vector.toSet(Vector#{1, 1, 2}) == Set#{1, 2}

    @test
    def toSet08(): Bool =
        Vector.toSet(Vector#{2, 1, 2, 3, 4, 5, 6, 3, 3}) == Set#{1, 2, 3, 4, 5, 6}

    /////////////////////////////////////////////////////////////////////////////
    // toMap                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toMap01(): Bool =
        Vector.toMap((Vector#{}): Vector[(Int32, Bool)]) == Map#{}

    @test
    def toMap02(): Bool =
        Vector.toMap(Vector#{(1, true)}) == Map#{1 => true}

    @test
    def toMap03(): Bool =
        Vector.toMap(Vector#{(1, true), (2, false)}) == Map#{1 => true, 2 => false}

    @test
    def toMap04(): Bool =
        Vector.toMap(Vector#{(1, true), (1, false)}) == Map#{1 => true}

    /////////////////////////////////////////////////////////////////////////////
    // findIndexOf                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def findIndexOf01(): Bool =
        Vector.findIndexOf(i -> i > 2, Vector#{}: Vector[Int32]) == None

    @test
    def findIndexOf02(): Bool =
        Vector.findIndexOf(i -> i > 2, Vector#{1}) == None

    @test
    def findIndexOf03(): Bool =
        Vector.findIndexOf(i -> i > 2, Vector#{3}) == Some(0)

    @test
    def findIndexOf04(): Bool =
        Vector.findIndexOf(i -> i > 2, Vector#{1, 2}) == None

    @test
    def findIndexOf05(): Bool =
        Vector.findIndexOf(i -> i > 2, Vector#{6, -6}) == Some(0)

    @test
    def findIndexOf06(): Bool =
        Vector.findIndexOf(i -> i > 2, Vector#{-6, 6}) == Some(1)

    @test
    def findIndexOf07(): Bool =
        Vector.findIndexOf(i -> i > 2, Vector#{6, 7}) == Some(0)

    /////////////////////////////////////////////////////////////////////////////
    // findIndexOfLeft                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def findIndexOfLeft01(): Bool =
        Vector.findIndexOfLeft(i -> i > 2, Vector#{}: Vector[Int32]) == None

    @test
    def findIndexOfLeft02(): Bool =
        Vector.findIndexOfLeft(i -> i > 2, Vector#{1}) == None

    @test
    def findIndexOfLeft03(): Bool =
        Vector.findIndexOfLeft(i -> i > 2, Vector#{3}) == Some(0)

    @test
    def findIndexOfLeft04(): Bool =
        Vector.findIndexOfLeft(i -> i > 2, Vector#{1, 2}) == None

    @test
    def findIndexOfLeft05(): Bool =
        Vector.findIndexOfLeft(i -> i > 2, Vector#{6, -6}) == Some(0)

    @test
    def findIndexOfLeft06(): Bool =
        Vector.findIndexOfLeft(i -> i > 2, Vector#{-6, 6}) == Some(1)

    @test
    def findIndexOfLeft07(): Bool =
        Vector.findIndexOfLeft(i -> i > 2, Vector#{6, 7}) == Some(0)

    /////////////////////////////////////////////////////////////////////////////
    // findIndexOfRight                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def findIndexOfRight01(): Bool =
        Vector.findIndexOfRight(i -> i > 2, Vector#{}: Vector[Int32]) == None

    @test
    def findIndexOfRight02(): Bool =
        Vector.findIndexOfRight(i -> i > 2, Vector#{1}) == None

    @test
    def findIndexOfRight03(): Bool =
        Vector.findIndexOfRight(i -> i > 2, Vector#{3}) == Some(0)

    @test
    def findIndexOfRight04(): Bool =
        Vector.findIndexOfRight(i -> i > 2, Vector#{1, 2}) == None

    @test
    def findIndexOfRight05(): Bool =
        Vector.findIndexOfRight(i -> i > 2, Vector#{6, -6}) == Some(0)

    @test
    def findIndexOfRight06(): Bool =
        Vector.findIndexOfRight(i -> i > 2, Vector#{-6, 6}) == Some(1)

    @test
    def findIndexOfRight07(): Bool =
        Vector.findIndexOfRight(i -> i > 2, Vector#{6, 7}) == Some(1)

    /////////////////////////////////////////////////////////////////////////////
    // findIndices                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def findIndices01(): Bool =
        let v = Vector.findIndices(i -> i > 2, (Vector#{}: Vector[Int32]));
        v == Vector#{}

    @test
    def findIndices02(): Bool =
        let v = Vector.findIndices(i -> i > 2, Vector#{1});
        v == Vector#{}

    @test
    def findIndices03(): Bool =
        let v = Vector.findIndices(i -> i > 2, Vector#{3});
        v == Vector#{0}

    @test
    def findIndices04(): Bool =
        let v = Vector.findIndices(i -> i > 2, Vector#{1, 2});
        v == Vector#{}

    @test
    def findIndices05(): Bool =
        let v = Vector.findIndices(i -> i > 2, Vector#{6, -6});
        v == Vector#{0}

    @test
    def findIndices06(): Bool =
        let v = Vector.findIndices(i -> i > 2, Vector#{-6, 6});
        v == Vector#{1}

    @test
    def findIndices07(): Bool =
        let v = Vector.findIndices(i -> i > 2, Vector#{6, 7});
        v == Vector#{0, 1}

    /////////////////////////////////////////////////////////////////////////////
    // init                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def init01(): Bool =
        let v = Vector.init(x -> x, 0);
        v == Vector#{}: Vector[Int32]

    @test
    def init02(): Bool =
        let v = Vector.init(x -> x, -1);
        v == Vector#{}

    @test
    def init03(): Bool =
        let v = Vector.init(x -> x, 1);
        v == Vector#{0}

    @test
    def init04(): Bool =
        let v = Vector.init(x -> x, 2);
        v == Vector#{0, 1}

    @test
    def init05(): Bool =
        let v = Vector.init(x -> x, 50);
        Vector.length(v) == 50

    /////////////////////////////////////////////////////////////////////////////
    // equals                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def equals01(): Bool =
        let a = Vector#{}: Vector[Int32];
        let b = Vector#{}: Vector[Int32];
        Vector.equals(a, b) == true

    @test
    def equals02(): Bool =
        let a = (Vector#{1});
        let b = Vector#{}: Vector[Int32];
        Vector.equals(a, b) == false

    @test
    def equals03(): Bool =
        let a = Vector#{}: Vector[Int32];
        let b = (Vector#{1});
        Vector.equals(a, b) == false

    @test
    def equals04(): Bool =
        let a = (Vector#{1});
        let b = (Vector#{1});
        Vector.equals(a, b) == true

    @test
    def equals05(): Bool =
        let a = (Vector#{1});
        let b = Vector#{}: Vector[Int32];
        Vector.equals(a, b) == false

    @test
    def equals06(): Bool =
        let a = (Vector#{1, 2});
        let b = (Vector#{1, 2});
        Vector.equals(a, b) == true

    @test
    def equals07(): Bool =
        let a = (Vector#{1, 2});
        let b = (Vector#{2, 1});
        Vector.equals(a, b) == false

    /////////////////////////////////////////////////////////////////////////////
    // forEach                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def forEach01(): Bool = region rc {
        let v = Vector#{}: Vector[Int32];
        let sb = StringBuilder.new(rc);
        let fn = x -> if (x > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        Vector.forEach(fn, v);
        StringBuilder.toString(sb) == ""
    }

    @test
    def forEach02(): Bool = region rc {
        let v = Vector#{0};
        let sb = StringBuilder.new(rc);
        let fn = x -> if (x > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        Vector.forEach(fn, v);
        StringBuilder.toString(sb) == "F"
    }

    @test
    def forEach03(): Bool = region rc {
        let v = Vector#{1};
        let sb = StringBuilder.new(rc);
        let fn = x -> if (x > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        Vector.forEach(fn, v);
        StringBuilder.toString(sb) == "T"
    }

    @test
    def forEach04(): Bool = region rc {
        let v = Vector#{0, 1};
        let sb = StringBuilder.new(rc);
        let fn = x -> if (x > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        Vector.forEach(fn, v);
        StringBuilder.toString(sb) == "FT"
    }

    /////////////////////////////////////////////////////////////////////////////
    // forEachWithIndex                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def forEachWithIndex01(): Bool = region rc {
        let v = (Vector#{}): Vector[Float32];
        let sb = StringBuilder.new(rc);
        let fn = (ix, _) -> if (ix > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        Vector.forEachWithIndex(fn, v);
        StringBuilder.toString(sb) == ""
    }

    @test
    def forEachWithIndex02(): Bool = region rc {
        let v = Vector#{0.0f32};
        let sb = StringBuilder.new(rc);
        let fn = (ix, _) -> if (ix > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        Vector.forEachWithIndex(fn, v);
        StringBuilder.toString(sb) == "F"
    }

    @test
    def forEachWithIndex03(): Bool = region rc {
        let v = Vector#{0.0f32, 0.1f32};
        let sb = StringBuilder.new(rc);
        let fn = (ix, _) -> if (ix > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        Vector.forEachWithIndex(fn, v);
        StringBuilder.toString(sb) == "FT"
    }

    @test
    def forEachWithIndex04(): Bool = region rc {
        let v = Vector#{0.0f32, 0.1f32, 0.2f32};
        let sb = StringBuilder.new(rc);
        let fn = (ix, _) -> if (ix > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        Vector.forEachWithIndex(fn, v);
        StringBuilder.toString(sb) == "FTT"
    }

    @test
    def forEachWithIndex05(): Bool = region rc {
        let v = Vector#{0.0f32, 0.1f32, 0.2f32, 0.3f32};
        let sb = StringBuilder.new(rc);
        let fn = (ix, _) -> if (ix > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        Vector.forEachWithIndex(fn, v);
        StringBuilder.toString(sb) == "FTTT"
    }

    /////////////////////////////////////////////////////////////////////////////
    // updateSequence                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def updateSequence01(): Bool =
         let v = Vector.updateSequence(0, Vector#{}: Vector[Int32], Vector#{}: Vector[Int32]);
         v == Vector#{}

    @test
    def updateSequence02(): Bool =
        let v = Vector.updateSequence(0, Vector#{1, 2}, Vector#{});
        v == Vector#{}

    @test
    def updateSequence03(): Bool =
        let v = Vector.updateSequence(0, Vector#{}, Vector#{1, 2});
        v == Vector#{1, 2}

    @test
    def updateSequence04(): Bool =
        let v = Vector.updateSequence(-3, Vector#{1, 2, 4}, Vector#{1, 2});
        v == Vector#{1, 2}

    @test
    def updateSequence05(): Bool =
        let v = Vector.updateSequence(2, Vector#{1, 2, 4}, Vector#{1, 2});
        v == Vector#{1, 2}

    @test
    def updateSequence06(): Bool =
        let v = Vector.updateSequence(0, Vector#{}, Vector#{1});
        v == Vector#{1}

    @test
    def updateSequence07(): Bool =
        let v = Vector.updateSequence(1, Vector#{2}, Vector#{1});
        v == Vector#{1}

    @test
    def updateSequence08(): Bool =
        let v = Vector.updateSequence(0, Vector#{2}, Vector#{1});
        v == Vector#{2}

    @test
    def updateSequence09(): Bool =
        let v = Vector.updateSequence(0, Vector#{2, 4}, Vector#{1});
        v == Vector#{2}

    @test
    def updateSequence10(): Bool =
        let v = Vector.updateSequence(-1, Vector#{2, 4}, Vector#{1});
        v == Vector#{4}

    @test
    def updateSequence11(): Bool =
        let v = Vector.updateSequence(-1, Vector#{3, 4}, Vector#{1, 2});
        v == Vector#{4, 2}

    @test
    def updateSequence12(): Bool =
        let v = Vector.updateSequence(1, Vector#{3, 4}, Vector#{1, 2});
        v == Vector#{1, 3}

    @test
    def updateSequence13(): Bool =
        let v = Vector.updateSequence(-2, Vector#{3, 4}, Vector#{1, 2});
        v == Vector#{1, 2}

    @test
    def updateSequence14(): Bool =
        let v = Vector.updateSequence(2, Vector#{3, 4}, Vector#{1, 2});
        v == Vector#{1, 2}

    @test
    def updateSequence15(): Bool =
        let v = Vector.updateSequence(1, Vector#{3}, Vector#{1, 2});
        v == Vector#{1, 3}

    @test
    def updateSequence16(): Bool =
        let v = Vector.updateSequence(0, Vector#{3, 4}, Vector#{1, 2});
        v == Vector#{3, 4}

    @test
    def updateSequence17(): Bool =
        let v = Vector.updateSequence(0, Vector#{4}, Vector#{1, 2, 3});
        v == Vector#{4, 2, 3}

    @test
    def updateSequence18(): Bool =
        let v = Vector.updateSequence(1, Vector#{4}, Vector#{1, 2, 3});
        v == Vector#{1, 4, 3}

    @test
    def updateSequence19(): Bool =
        let v = Vector.updateSequence(2, Vector#{4}, Vector#{1, 2, 3});
        v == Vector#{1, 2, 4}

    @test
    def updateSequence20(): Bool =
        let v = Vector.updateSequence(0, Vector#{4, 5}, Vector#{1, 2, 3});
        v == Vector#{4, 5, 3}

    @test
    def updateSequence21(): Bool =
        let v = Vector.updateSequence(1, Vector#{4, 5}, Vector#{1, 2, 3});
        v == Vector#{1, 4, 5}

    @test
    def updateSequence22(): Bool =
        let v = Vector.updateSequence(-1, Vector#{4, 5, 6}, Vector#{1, 2, 3});
        v == Vector#{5, 6, 3}

    @test
    def updateSequence23(): Bool =
        let v = Vector.updateSequence(0, Vector#{4, 5, 6}, Vector#{1, 2, 3});
        v == Vector#{4, 5, 6}

    @test
    def updateSequence24(): Bool =
        let v = Vector.updateSequence(2, Vector#{14, 15, 16, 17}, Vector#{1, 2, 3, 4, 5, 6, 7});
        v == Vector#{1, 2, 14, 15, 16, 17, 7}

    @test
    def updateSequence25(): Bool =
        let v = Vector.updateSequence(-2, Vector#{14, 15, 16, 17}, Vector#{1, 2, 3, 4, 5, 6, 7});
        v == Vector#{16, 17, 3, 4, 5, 6, 7}

    @test
    def updateSequence26(): Bool =
        let v = Vector.updateSequence(4, Vector#{14, 15, 16, 17}, Vector#{1, 2, 3, 4, 5, 6, 7});
        v == Vector#{1, 2, 3, 4, 14, 15, 16}

    @test
    def updateSequence27(): Bool =
        let v = Vector.updateSequence(4, Vector#{14, 15}, Vector#{1, 2, 3, 4, 5, 6, 7});
        v == Vector#{1, 2, 3, 4, 14, 15, 7}

    @test
    def updateSequence28(): Bool =
        let v = Vector.updateSequence(-1, Vector#{-1, -2, -3, -4, -5, -6, -7, -8}, Vector#{1, 2, 3, 4, 5, 6, 7});
        v == Vector#{-2, -3, -4, -5, -6, -7, -8}

    /////////////////////////////////////////////////////////////////////////////
    // sortWith                                                                //
    /////////////////////////////////////////////////////////////////////////////

    def cmp(x: Int32, y: Int32): Comparison =
        if (x < y) Comparison.LessThan
        else if (x == y) Comparison.EqualTo
        else Comparison.GreaterThan

    @test
    def sortWith01(): Bool =
        let v = Vector.sortWith(cmp, Vector#{}: Vector[Int32]);
        v == Vector#{}

    @test
    def sortWith02(): Bool =
        let v = Vector.sortWith(cmp, Vector#{0});
        v == Vector#{0}

    @test
    def sortWith03(): Bool =
        let v = Vector.sortWith(cmp, Vector#{0, 1});
        v == Vector#{0, 1}

    @test
    def sortWith04(): Bool =
        let v = Vector.sortWith(cmp, Vector#{1, 0});
        v == Vector#{0, 1}

    @test
    def sortWith05(): Bool =
        let v = Vector.sortWith(cmp, Vector#{1, 1});
        v == Vector#{1, 1}

    @test
    def sortWith06(): Bool =
        let v = Vector.sortWith(cmp, Vector#{0, 1, 2, 3, 4, 5});
        v == Vector#{0, 1, 2, 3, 4, 5}

    @test
    def sortWith07(): Bool =
        let v = Vector.sortWith(cmp, Vector#{5, 4, 3, 2, 1, 0});
        v == Vector#{0, 1, 2, 3, 4, 5}

    @test
    def sortWith08(): Bool =
        let v = Vector.sortWith(cmp, Vector#{5, 3, 0, 4, 1, 2});
        v == Vector#{0, 1, 2, 3, 4, 5}

    @test
    def sortWith09(): Bool =
        let v = Vector.sortWith(cmp, Vector#{2, 3, 0, 4, 1, 2});
        v == Vector#{0, 1, 2, 2, 3, 4}

    @test
    def sortWith10(): Bool =
        let v = Vector.sortWith(flip(cmp), Vector#{0, 1, 2, 3, 4, 5});
        v == Vector#{5, 4, 3, 2, 1, 0}

    @test
    def sortWith11(): Bool =
        let v = Vector.sortWith(flip(cmp), Vector#{5, 4, 3, 2, 1, 0});
        v == Vector#{5, 4, 3, 2, 1, 0}

    @test
    def sortWith12(): Bool =
        let v = Vector.sortWith(flip(cmp), Vector#{5, 3, 0, 4, 1, 2});
        v == Vector#{5, 4, 3, 2, 1, 0}

    @test
    def sortWith13(): Bool =
        let v = Vector.sortWith(flip(cmp), Vector#{2, 3, 0, 4, 1, 2});
        v == Vector#{4, 3, 2, 2, 1, 0}

    /////////////////////////////////////////////////////////////////////////////
    // sort                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    def testSortVsSortWith(a: Vector[Int32]): Bool =
        Vector.sort(a) == Vector.sortWith(cmp, a)


    @test
    def sort01(): Bool =
        testSortVsSortWith(Vector#{}: Vector[Int32])

    @test
    def sort02(): Bool =
        testSortVsSortWith(Vector#{0})

    @test
    def sort03(): Bool =
        testSortVsSortWith(Vector#{0, 1})

    @test
    def sort04(): Bool =
        testSortVsSortWith(Vector#{1, 0})

    @test
    def sort05(): Bool =
        testSortVsSortWith(Vector#{1, 1})

    @test
    def sort06(): Bool =
        testSortVsSortWith(Vector#{0, 1, 2, 3, 4, 5})

    @test
    def sort07(): Bool =
        testSortVsSortWith(Vector#{5, 4, 3, 2, 1, 0})

    @test
    def sort08(): Bool =
        testSortVsSortWith(Vector#{5, 3, 0, 4, 1, 2})

    @test
    def sort09(): Bool =
        testSortVsSortWith(Vector#{2, 3, 0, 4, 1, 2})

    /////////////////////////////////////////////////////////////////////////////
    // sortBy                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    def testSortByVsSort(a: Vector[Int32]): Bool =
        (Vector.sortBy(identity, a) == Vector.sort(a)) and
        (Vector.sortBy(x -> 4 * x + 7, a) == Vector.sort(a)) and
        (Vector.sortBy(x -> -x, a) == Vector.sortWith(flip(cmp), a))

    @test
    def sortBy01(): Bool =
        testSortByVsSort(Vector#{}: Vector[Int32])

    @test
    def sortBy02(): Bool =
        testSortByVsSort(Vector#{0})

    @test
    def sortBy03(): Bool =
        testSortByVsSort(Vector#{0, 1})

    @test
    def sortBy04(): Bool =
        testSortByVsSort(Vector#{1, 0})

    @test
    def sortBy05(): Bool =
        testSortByVsSort(Vector#{1, 1})

    @test
    def sortBy06(): Bool =
        testSortByVsSort(Vector#{0, 1, 2, 3, 4, 5})

    @test
    def sortBy07(): Bool =
        testSortByVsSort(Vector#{5, 4, 3, 2, 1, 0})

    @test
    def sortBy08(): Bool =
        testSortByVsSort(Vector#{5, 3, 0, 4, 1, 2})

    @test
    def sortBy09(): Bool =
        testSortByVsSort(Vector#{2, 3, 0, 4, 1, 2})

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
    def sortBy10(): Bool =
        let geti = r -> {let R.R(x) = r; x.i};
        Vector.sortBy(geti, Vector#{R.R({i = 2, s = "A"}), R.R({i = 1, s = "B"}), R.R({i = 3, s = "C"})})
            == Vector#{R.R({i = 1, s = "B"}), R.R({i = 2, s = "A"}), R.R({i = 3, s = "C"})}

    /////////////////////////////////////////////////////////////////////////////
    // minimumBy                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def minimumBy01(): Bool =
        Vector.minimumBy((x, y) -> x <=> y, Vector#{}: Vector[Int32]) == None

    @test
    def minimumBy02(): Bool =
        Vector.minimumBy((x, y) -> x <=> y, Vector.range(0, 5)) == Some(0)

    @test
    def minimumBy03(): Bool =
        Vector.minimumBy((x, y) -> x <=> y, Vector.range(4, 10)) == Some(4)

    @test
    def minimumBy04(): Bool =
        Vector.minimumBy((x, y) -> x <=> y, Vector.range(10, 20)) == Some(10)

    /////////////////////////////////////////////////////////////////////////////
    // maximumBy                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def maximumBy01(): Bool =
        Vector.maximumBy((x, y) -> x <=> y, Vector#{}: Vector[Int32]) == None

    @test
    def maximumBy02(): Bool =
        Vector.maximumBy((x, y) -> x <=> y, Vector.range(0, 5)) == Some(4)

    @test
    def maximumBy03(): Bool =
        Vector.maximumBy((x, y) -> x <=> y, Vector.range(4, 10)) == Some(9)

    @test
    def maximumBy04(): Bool =
        Vector.maximumBy((x, y) -> x <=> y, Vector.range(10, 20)) == Some(19)

    /////////////////////////////////////////////////////////////////////////////
    // sum                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sum01(): Bool =
        Vector.sum(Vector#{}) == 0

    @test
    def sum02(): Bool =
        Vector.sum(Vector#{1}) == 1

    @test
    def sum03(): Bool =
        Vector.sum(Vector#{1, 2, 3}) == 6

    @test
    def sum04(): Bool =
        Vector.sum(Vector#{1, 2, 3, -3}) == 3

    @test
    def sum05(): Bool =
        Vector.sum(Vector#{-1, -2, -3, -4}) == -10

    @test
    def sum06(): Bool =
        Vector.sum(Vector#{10, -10}) == 0

    @test
    def sum07(): Bool =
        Vector.range(1, 101) |> Vector.sum == 5050

    /////////////////////////////////////////////////////////////////////////////
    // sumWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sumWith01(): Bool =
        Vector.sumWith(x -> x + 1, Vector#{}) == 0

    @test
    def sumWith02(): Bool =
        Vector.sumWith(x -> x + 1, Vector#{1}) == 2

    @test
    def sumWith03(): Bool =
        Vector.sumWith(x -> x + 1, Vector#{1, 2, 3}) == 9

    @test
    def sumWith04(): Bool =
        Vector.sumWith(x -> x + 1, Vector#{1, 2, 3, -3}) == 7

    @test
    def sumWith05(): Bool =
        Vector.sumWith(x -> x + 1, Vector#{-1, -2, -3, -4}) == -6

    @test
    def sumWith06(): Bool =
        Vector.sumWith(x -> x + 1, Vector#{10, -10}) == 2


    /////////////////////////////////////////////////////////////////////////////
    // toDelayList                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toDelayList01(): Bool =
        (Vector#{}): Vector[Unit] |> Vector.toDelayList == DelayList.empty()

    @test
    def toDelayList02(): Bool =
        (Vector.empty(): Vector[String]) |> Vector.toDelayList == DelayList.empty()

    @test
    def toDelayList03(): Bool =
        Vector.range(-1000, 1000) |> Vector.toDelayList == DelayList.range(-1000, 1000)

    @test
    def toDelayList04(): Bool =
        Vector.range(-1000, 1000) |> Vector.toDelayList |> DelayList.toVector == Vector.range(-1000, 1000)

    /////////////////////////////////////////////////////////////////////////////
    // toChain                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toChain01(): Bool =
        Vector.toChain((Vector#{}): Vector[Unit]) == Chain.empty(): Chain[Unit]

    @test
    def toChain02(): Bool =
        Vector.toChain(Vector#{1}) == Chain.singleton(1)

    @test
    def toChain03(): Bool =
        Vector.toChain(Vector#{1, 2}) == List.toChain(1 :: 2 :: Nil)

    @test
    def toChain04(): Bool =
        Vector.toChain(Vector#{1, 2, 3}) == List.toChain(1 :: 2 :: 3 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // toNec                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toNec01(): Bool =
        Vector.toNec((Vector#{}): Vector[Unit]) == None

    @test
    def toNec02(): Bool =
        Vector.toNec(Vector#{1}) == Some(Nec.singleton(1))

    @test
    def toNec03(): Bool =
        Vector.toNec(Vector#{1, 2}) == List.toNec(1 :: 2 :: Nil)

    @test
    def toNec04(): Bool =
        Vector.toNec(Vector#{1, 2, 3}) == List.toNec(1 :: 2 :: 3 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // iterator                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def iterator01(): Bool = region rc {
        Vector#{}: Vector[Int32] |> Vector.iterator(rc) |> Iterator.toList == Nil
    }

    @test
    def iterator02(): Bool = region rc {
        (Vector#{1, 2, 3}) |> Vector.iterator(rc) |> Iterator.toList == 1 :: 2 :: 3 :: Nil
    }

    @test
    def iterator03(): Bool = region rc {
        (Vector#{1, 2, 3}) |> Vector.iterator(rc) |> Iterator.toVector == Vector#{1, 2, 3}
    }

    @test
    def iterator04(): Bool = region rc {
        Vector.range(-100, 100) |> Vector.iterator(rc) |> Iterator.toVector == Vector.range(-100, 100)
    }

    /////////////////////////////////////////////////////////////////////////////
    // join                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def join01(): Bool =
        Vector.join(",", Vector#{}: Vector[Int32]) == ""

    @test
    def join02(): Bool =
        Vector.join(",", Vector#{1}) == "1"

    @test
    def join03(): Bool =
        Vector.join(",", Vector#{1, 2, 3}) == "1,2,3"

    @test
    def join04(): Bool =
        Vector.join(",", Vector#{"1", "2", "3"}) == "1,2,3"

    /////////////////////////////////////////////////////////////////////////////
    // joinWith                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def joinWith01(): Bool =
        Vector.joinWith(x -> "${x + 1}", ",", Vector#{}) == ""

    @test
    def joinWith02(): Bool =
        Vector.joinWith(x -> "${x + 1}", ",", Vector#{1}) == "2"

    @test
    def joinWith03(): Bool =
        Vector.joinWith(x -> "${x + 1}", ",", Vector#{1, 2, 3}) == "2,3,4"

    @test
    def joinWith04(): Bool =
        Vector.joinWith(x -> x + x, ",", Vector#{"1", "2", "3"}) == "11,22,33"

    @test
    def joinWith05(): Bool =
        let v: Vector[Int32] = Vector#{};
        let s = Vector.joinWith(Int32.toString, ",", v);
        s == ""

    @test
    def joinWith06(): Bool =
        let v = Vector#{1};
        let s = Vector.joinWith(Int32.toString, ",", v);
        s == "1"

    @test
    def joinWith07(): Bool =
        let v = Vector#{1, 2};
        let s = Vector.joinWith(Int32.toString, ",", v);
        s == "1,2"

    @test
    def joinWith08(): Bool =
        let v = Vector#{1, 2, 3};
        let s = Vector.joinWith(Int32.toString, ",", v);
        s == "1,2,3"

    @test
    def joinWith09(): Bool =
        let v = Vector#{1, 2, 3};
        let s = Vector.joinWith(Int32.toString, "", v);
        s == "123"

    @test
    def joinWith10(): Bool =
        let v = Vector#{1, 2, 3};
        let s = Vector.joinWith(Int32.toString, "..", v);
        s == "1..2..3"


    /////////////////////////////////////////////////////////////////////////////
    // toMutList                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toMutList01(): Bool = region rc {
        MutList.sameElements(Vector.toMutList(rc, Vector#{}: Vector[Int32]), MutList.new(rc))
    }

    @test
    def toMutList02(): Bool = region rc {
        let l = MutList.new(rc);
        MutList.push!(1, l);
        MutList.sameElements(Vector.toMutList(rc, Vector#{1}), l)
    }

    @test
    def toMutList03(): Bool = region rc {
        let l = MutList.new(rc);
        MutList.push!(1, l);
        MutList.push!(2, l);
        MutList.push!(3, l);
        MutList.sameElements(Vector.toMutList(rc, Vector#{1, 2, 3}), l)
    }

    @test
    def toMutList04(): Bool = region rc {
        MutList.sameElements((Vector.range(0, 100) |> Vector.toMutList(rc)), MutList.range(rc, 0, 100))
    }

    @test
    def toMutList05(): Bool = region rc {
        let l = Vector.toMutList(rc, Vector#{1, 2, 3});
        MutList.push!(4, l);
        Vector.equals(MutList.toVector(l), Vector#{1, 2, 3, 4})
    }

    @test
    def toMutList06(): Bool = region rc {
        let l = Vector.toMutList(rc, Vector#{1, 2, 3});
        MutList.push!(4, l);
        discard MutList.pop!(l);
        discard MutList.pop!(l);
        discard MutList.pop!(l);
        discard MutList.pop!(l);
        Vector.equals(MutList.toVector(l), Vector#{})
    }

    /////////////////////////////////////////////////////////////////////////////
    // shuffle                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def shuffle01(): Bool \ NonDet =
        let v = Vector#{}: Vector[Int32];
        let v1 = Vector.shuffle(Random.new(), v);
        Vector.length(v1) == 0 and Vector.toSet(v1) == Set#{}

    @test
    def shuffle02(): Bool \ NonDet =
        let v = Vector#{0};
        let v1 = Vector.shuffle(Random.new(), v);
        Vector.length(v1) == 1 and Vector.toSet(v1) == Set#{0}

    @test
    def shuffle03(): Bool \ NonDet =
        let v = Vector#{0, 1, 2, 3};
        let v1 = Vector.shuffle(Random.new(), v);
        Vector.length(v1) == 4 and Vector.toSet(v1) == Set#{0, 1, 2, 3}

    @test
    def shuffle04(): Bool \ NonDet =
        let v = Vector#{0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
        let v1 = Vector.shuffle(Random.new(), v);
        Vector.length(v1) == 10 and Vector.toSet(v1) == Set#{0, 1, 2, 3, 4, 5, 6, 7, 8, 9}

}

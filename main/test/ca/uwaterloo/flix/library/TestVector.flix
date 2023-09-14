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

    @Test
    def compare01(): Bool =
        (Vector#{}: Vector[Int32]) `Vector.compare` (Vector#{}: Vector[Int32]) == Comparison.EqualTo

    @Test
    def compare02(): Bool =
        ((Vector#{1}) `Vector.compare` (Vector#{})) == Comparison.GreaterThan

    @Test
    def compare03(): Bool =
        ((Vector#{}) `Vector.compare` (Vector#{1})) == Comparison.LessThan

    @Test
    def compare04(): Bool =
        ((Vector#{1}) `Vector.compare` (Vector#{1})) == Comparison.EqualTo

    @Test
    def compare05(): Bool =
        ((Vector#{1}) `Vector.compare` (Vector#{2})) == Comparison.LessThan

    @Test
    def compare06(): Bool =
        ((Vector#{2}) `Vector.compare` (Vector#{1})) == Comparison.GreaterThan

    @Test
    def compare07(): Bool =
        ((Vector#{1}) `Vector.compare` (Vector#{1, 2})) == Comparison.LessThan

    @Test
    def compare08(): Bool =
        ((Vector#{1, 2}) `Vector.compare` (Vector#{1})) == Comparison.GreaterThan

    @Test
    def compare09(): Bool =
        ((Vector#{1}) `Vector.compare` (Vector#{0, 0})) == Comparison.GreaterThan

    @Test
    def compare10(): Bool =
        ((Vector#{0, 0}) `Vector.compare` (Vector#{1})) == Comparison.LessThan

    @Test
    def compare11(): Bool =
        ((Vector#{1, 2}) `Vector.compare` (Vector#{1, 1})) == Comparison.GreaterThan

    @Test
    def compare12(): Bool =
        ((Vector#{1, 2}) `Vector.compare` (Vector#{1, 3})) == Comparison.LessThan

    @Test
    def compare13(): Bool =
        ((Vector#{1, 2, 3, 4}) `Vector.compare` (Vector#{1, 2, 3, 4})) == Comparison.EqualTo

    /////////////////////////////////////////////////////////////////////////////
    // toString                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toString01(): Bool =
        Vector.toString(Vector#{}: Vector[Int32]) == "Vector#{}"

    @Test
    def toString02(): Bool =
        Vector.toString(Vector#{1}) == "Vector#{1}"

    @Test
    def toString03(): Bool =
        Vector.toString(Vector#{1, 2, 3, 4}) == "Vector#{1, 2, 3, 4}"

    @Test
    def toString04(): Bool =
        Vector.toString(Vector#{'a', 'b', 'c', 'd'}) == "Vector#{a, b, c, d}"

    /////////////////////////////////////////////////////////////////////////////
    // empty                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def empty01(): Bool =
        let v = Vector.empty();
        Vector.length(v) == 0

    /////////////////////////////////////////////////////////////////////////////
    // singleton                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def singleton01(): Bool =
        let v = Vector.singleton(0);
        Vector.nth(0, v) == Some(0)

    @Test
    def singleton02(): Bool =
        let v = Vector.singleton('A');
        Vector.nth(0, v) == Some('A')

    /////////////////////////////////////////////////////////////////////////////
    // nth                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def nth01(): Bool =
        Vector.nth(0, Vector#{1, 2, 3}) == Some(1)

    @Test
    def nth02(): Bool =
        Vector.nth(1, Vector#{1, 2, 3}) == Some(2)

    @Test
    def nth03(): Bool =
        Vector.nth(2, Vector#{1, 2, 3}) == Some(3)

    @Test
    def nth04(): Bool =
        Vector.nth(3, Vector#{1, 2, 3}) == None

    @Test
    def nth05(): Bool =
        Vector.nth(-2, Vector#{1, 2, 3}) == None

    /////////////////////////////////////////////////////////////////////////////
    // isEmpty                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isEmpty01(): Bool =
        Vector.isEmpty(Vector#{})

    @Test
    def isEmpty02(): Bool =
        not Vector.isEmpty(Vector#{1})

    @Test
    def isEmpty03(): Bool =
        not Vector.isEmpty(Vector#{1, 2, 3})

    /////////////////////////////////////////////////////////////////////////////
    // length                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def length01(): Bool =
        Vector.length(Vector#{}) == 0

    @Test
    def length02(): Bool =
        Vector.length(Vector#{1}) == 1

    @Test
    def length03(): Bool =
        Vector.length(Vector#{1, 2, 3}) == 3

    /////////////////////////////////////////////////////////////////////////////
    // slice                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def slice01(): Bool =
        Vector.slice(start = 0, end = 1, Vector#{1, 2, 3}) |> Vector.get(0) == 1

    @Test
    def slice02(): Bool =
        Vector.slice(start = 1, end = 2, Vector#{1, 2, 3}) |> Vector.get(0) == 2

    @Test
    def slice03(): Bool =
        Vector.slice(start = 2, end = 3, Vector#{1, 2, 3}) |> Vector.get(0) == 3

    /////////////////////////////////////////////////////////////////////////////
    // toList                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toList01(): Bool =
        Vector.toList(Vector#{}: Vector[Int32]) == Nil

    @Test
    def toList02(): Bool =
        Vector.toList(Vector#{1}) == 1 :: Nil

    @Test
    def toList03(): Bool =
        Vector.toList(Vector#{1, 2}) == 1 :: 2 :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // toNel                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toNel01(): Bool =
        Vector.toNel(Vector#{}: Vector[Unit]) == None

    @Test
    def toNel02(): Bool =
        Vector.toNel(Vector#{1}) == Some(Nel.singleton(1))

    @Test
    def toNel03(): Bool =
        Vector.toNel(Vector#{1, 2}) == List.toNel(1 :: 2 :: Nil)

    @Test
    def toNel04(): Bool =
        Vector.toNel(Vector#{1, 2, 3}) == List.toNel(1 :: 2 :: 3 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // toMutDeque                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toMutDeque01(): Bool = region rc {
        let v = Vector#{}: Vector[Int32];
        let d = MutDeque.new(rc): MutDeque[Int32, _];
        Vector.toMutDeque(rc, v) `MutDeque.sameElements` d
    }

    @Test
    def toMutDeque02(): Bool = region rc {
        let v = Vector#{1};
        let d = MutDeque.new(rc);
        MutDeque.pushBack(1, d);
        Vector.toMutDeque(rc, v) `MutDeque.sameElements` d
    }

    @Test
    def toMutDeque03(): Bool = region rc {
        let v = Vector#{1, 2, 3};
        let d = MutDeque.new(rc);
        MutDeque.pushBack(1, d);
        MutDeque.pushBack(2, d);
        MutDeque.pushBack(3, d);
        Vector.toMutDeque(rc, v) `MutDeque.sameElements` d
    }

    @Test
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

    @Test
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

    @Test
    def head01(): Bool =
        Vector.head(Vector#{}: Vector[Int32]) == None

    @Test
    def head02(): Bool =
        Vector.head(Vector#{1}) == Some(1)

    @Test
    def head03(): Bool =
        Vector.head(Vector#{2, 1}) == Some(2)

    @Test
    def head04(): Bool =
        Vector.head(Vector#{3, 2, 1}) == Some(3)

    /////////////////////////////////////////////////////////////////////////////
    // last                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def last01(): Bool =
        Vector.last(Vector#{}): Option[Unit] == None

    @Test
    def last02(): Bool =
        Vector.last(Vector#{1}) == Some(1)

    @Test
    def last03(): Bool =
        Vector.last(Vector#{1, 2}) == Some(2)

    @Test
    def last04(): Bool =
        Vector.last(Vector#{1, 2, 3}) == Some(3)

    /////////////////////////////////////////////////////////////////////////////
    // append                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def append01(): Bool =
        let v = Vector.append(Vector#{}: Vector[Int32], Vector#{}: Vector[Int32]);
        v == (Vector#{}: Vector[Int32])

    @Test
    def append02(): Bool =
        let v = Vector.append(Vector#{}: Vector[Int32], Vector#{1});
        v == Vector#{1}

    @Test
    def append03(): Bool =
        let v = Vector.append(Vector#{}: Vector[Int32], Vector#{1, 2});
        v == Vector#{1, 2}

    @Test
    def append04(): Bool =
        let v = Vector.append(Vector#{1}, Vector#{}: Vector[Int32]);
        v == Vector#{1}

    @Test
    def append05(): Bool =
        let v = Vector.append(Vector#{1, 2}, Vector#{}: Vector[Int32]);
        v == Vector#{1, 2}

    @Test
    def append06(): Bool =
        let v = Vector.append(Vector#{1}, Vector#{2});
        v == Vector#{1, 2}

    @Test
    def append07(): Bool =
        let v = Vector.append(Vector#{1, 2}, Vector#{3});
        v == Vector#{1, 2, 3}

    @Test
    def append08(): Bool =
        let v = Vector.append(Vector#{1}, Vector#{2, 3});
        v == Vector#{1, 2, 3}

    /////////////////////////////////////////////////////////////////////////////
    // memberOf                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def memberOf01(): Bool =
        Vector.memberOf(0, Vector#{}: Vector[Int32]) == false

    @Test
    def memberOf02(): Bool =
        Vector.memberOf(0, Vector#{1}) == false

    @Test
    def memberOf03(): Bool =
        Vector.memberOf(1, Vector#{1}) == true

    @Test
    def memberOf04(): Bool =
        Vector.memberOf(0, Vector#{1, 2}) == false

    @Test
    def memberOf05(): Bool =
        Vector.memberOf(1, Vector#{1, 2}) == true

    @Test
    def memberOf06(): Bool =
        Vector.memberOf(2, Vector#{1, 2}) == true

    @Test
    def memberOf07(): Bool =
        Vector.memberOf(3, Vector#{1, 2}) == false

    /////////////////////////////////////////////////////////////////////////////
    // indexOf                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def indexOf01(): Bool =
        Vector.indexOf(0, Vector#{}: Vector[Int32]) == None

    @Test
    def indexOf02(): Bool =
        Vector.indexOf(0, Vector#{1}) == None

    @Test
    def indexOf03(): Bool =
        Vector.indexOf(1, Vector#{1}) == Some(0)

    @Test
    def indexOf04(): Bool =
        Vector.indexOf(0, Vector#{1, 2}) == None

    @Test
    def indexOf05(): Bool =
        Vector.indexOf(1, Vector#{1, 2}) == Some(0)

    @Test
    def indexOf06(): Bool =
        Vector.indexOf(2, Vector#{1, 2}) == Some(1)

    /////////////////////////////////////////////////////////////////////////////
    // indexOfLeft                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def indexOfLeft01(): Bool =
        Vector.indexOfLeft(0, Vector#{}: Vector[Int32]) == None

    @Test
    def indexOfLeft02(): Bool =
        Vector.indexOfLeft(0, Vector#{1}) == None

    @Test
    def indexOfLeft03(): Bool =
        Vector.indexOfLeft(1, Vector#{1}) == Some(0)

    @Test
    def indexOfLeft04(): Bool =
        Vector.indexOfLeft(0, Vector#{1, 2}) == None

    @Test
    def indexOfLeft05(): Bool =
        Vector.indexOfLeft(1, Vector#{1, 2}) == Some(0)

    @Test
    def indexOfLeft06(): Bool =
        Vector.indexOfLeft(2, Vector#{1, 2}) == Some(1)

    @Test
    def indexOfLeft07(): Bool =
        Vector.indexOfLeft(1, Vector#{1, 1}) == Some(0)

    /////////////////////////////////////////////////////////////////////////////
    // indexOfRight                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def indexOfRight01(): Bool =
        Vector.indexOfRight(0, Vector#{}: Vector[Int32]) == None

    @Test
    def indexOfRight02(): Bool =
        Vector.indexOfRight(0, Vector#{1}) == None

    @Test
    def indexOfRight03(): Bool =
        Vector.indexOfRight(1, Vector#{1}) == Some(0)

    @Test
    def indexOfRight04(): Bool =
        Vector.indexOfRight(0, Vector#{1, 2}) == None

    @Test
    def indexOfRight05(): Bool =
        Vector.indexOfRight(1, Vector#{1, 2}) == Some(0)

    @Test
    def indexOfRight06(): Bool =
        Vector.indexOfRight(2, Vector#{1, 2}) == Some(1)

    @Test
    def indexOfRight07(): Bool =
        Vector.indexOfRight(1, Vector#{1, 1}) == Some(1)

    /////////////////////////////////////////////////////////////////////////////
    // indices                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def indices01(): Bool =
        let v = Vector.indices(0, Vector#{}: Vector[Int32]);
        v == Vector#{}

    @Test
    def indices02(): Bool =
        let v = Vector.indices(0, Vector#{1});
        v == Vector#{}

    @Test
    def indices03(): Bool =
        let v = Vector.indices(1, Vector#{1});
        v == Vector#{0}

    @Test
    def indices04(): Bool =
        let v = Vector.indices(0, Vector#{1, 2});
        v == Vector#{}

    @Test
    def indices05(): Bool =
        let v = Vector.indices(1, Vector#{1, 2});
        v == Vector#{0}

    @Test
    def indices06(): Bool =
        let v = Vector.indices(2, Vector#{1, 2});
        v == Vector#{1}

    @Test
    def indices07(): Bool =
        let v = Vector.indices(1, Vector#{1, 1});
        v == Vector#{0, 1}

    /////////////////////////////////////////////////////////////////////////////
    // find                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def find01(): Bool =
        Vector.find(i -> i > 2, Vector#{}) == None

    @Test
    def find02(): Bool =
        Vector.find(i -> i > 2, Vector#{1}) == None

    @Test
    def find03(): Bool =
        Vector.find(i -> i > 2, Vector#{3}) == Some(3)

    @Test
    def find04(): Bool =
        Vector.find(i -> i > 2, Vector#{1, 2}) == None

    @Test
    def find05(): Bool =
        Vector.find(i -> i > 2, Vector#{6, -6}) == Some(6)

    @Test
    def find06(): Bool =
        Vector.find(i -> i > 2, Vector#{-6, 6}) == Some(6)

    @Test
    def find07(): Bool =
        Vector.find(i -> i > 2, Vector#{6, 7}) == Some(6)

    /////////////////////////////////////////////////////////////////////////////
    // findLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def findLeft01(): Bool =
        Vector.findLeft(i -> i > 2, Vector#{}) == None

    @Test
    def findLeft02(): Bool =
        Vector.findLeft(i -> i > 2, Vector#{1}) == None

    @Test
    def findLeft03(): Bool =
        Vector.findLeft(i -> i > 2, Vector#{3}) == Some(3)

    @Test
    def findLeft04(): Bool =
        Vector.findLeft(i -> i > 2, Vector#{1, 2}) == None

    @Test
    def findLeft05(): Bool =
        Vector.findLeft(i -> i > 2, Vector#{6, -6}) == Some(6)

    @Test
    def findLeft06(): Bool =
        Vector.findLeft(i -> i > 2, Vector#{-6, 6}) == Some(6)

    @Test
    def findLeft07(): Bool =
        Vector.findLeft(i -> i > 2, Vector#{6, 7}) == Some(6)

    /////////////////////////////////////////////////////////////////////////////
    // findRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def findRight01(): Bool =
        Vector.findRight(i -> i > 2, Vector#{}) == None

    @Test
    def findRight02(): Bool =
        Vector.findRight(i -> i > 2, Vector#{1}) == None

    @Test
    def findRight03(): Bool =
        Vector.findRight(i -> i > 2, Vector#{3}) == Some(3)

    @Test
    def findRight04(): Bool =
        Vector.findRight(i -> i > 2, Vector#{1, 2}) == None

    @Test
    def findRight05(): Bool =
        Vector.findRight(i -> i > 2, Vector#{6, -6}) == Some(6)

    @Test
    def findRight06(): Bool =
        Vector.findRight(i -> i > 2, Vector#{-6, 6}) == Some(6)

    @Test
    def findRight07(): Bool =
        Vector.findRight(i -> i > 2, Vector#{6, 7}) == Some(7)

    /////////////////////////////////////////////////////////////////////////////
    // range                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def range01(): Bool =
        let v = Vector.range(1, 0);
        v == Vector#{}

    @Test
    def range02(): Bool =
        let v = Vector.range(1, 1);
        v == Vector#{}

    @Test
    def range03(): Bool =
        let v = Vector.range(1, 2);
        v == Vector#{1}

    @Test
    def range04(): Bool =
        let v = Vector.range(1, 3);
        v == Vector#{1, 2}

    @Test
    def range05(): Bool =
        let v = Vector.range(1, 4);
        v == Vector#{1, 2, 3}

    @Test
    def range06(): Bool =
        let v = Vector.range(-1, 3);
        v == Vector#{-1, 0, 1, 2}

    /////////////////////////////////////////////////////////////////////////////
    // repeat                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def repeat01(): Bool =
        let v = Vector.repeat(-1, 1);
        v == Vector#{}

    @Test
    def repeat02(): Bool =
        let v = Vector.repeat(0, 1);
        v == Vector#{}

    @Test
    def repeat03(): Bool =
        let v = Vector.repeat(1, 1);
        v == Vector#{1}

    @Test
    def repeat04(): Bool =
        let v = Vector.repeat(2, 1);
        v == Vector#{1, 1}

    @Test
    def repeat05(): Bool =
        let v = Vector.repeat(3, 1);
        v == Vector#{1, 1, 1}

    /////////////////////////////////////////////////////////////////////////////
    // scan                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def scan01(): Bool =
        let v = Vector.scan((i, b) -> if (b) i + 1 else i + 2, 1, Vector#{});
        v == Vector#{1}

    @Test
    def scan02(): Bool =
        let v = Vector.scan((i, b) -> if (b) i + 1 else i + 2, 1, Vector#{false});
        v == Vector#{1, 3}

    @Test
    def scan03(): Bool =
        let v = Vector.scan((i, b) -> if (b) i + 1 else i + 2, 1, Vector#{true});
        v == Vector#{1, 2}

    @Test
    def scan04(): Bool =
        let v = Vector.scan((i, b) -> if (b) i + 1 else i + 2, 1, Vector#{false, false});
        v == Vector#{1, 3, 5}

    @Test
    def scan05(): Bool =
        let v = Vector.scan((i, b) -> if (b) i + 1 else i + 2, 1, Vector#{false, true});
        v == Vector#{1, 3, 4}

    @Test
    def scan06(): Bool =
        let v = Vector.scan((i, b) -> if (b) i + 1 else i + 2, 1, Vector#{true, false});
        v == Vector#{1, 2, 4}

    @Test
    def scan07(): Bool =
        let v = Vector.scan((i, b) -> if (b) i + 1 else i + 2, 1, Vector#{true, true});
        v == Vector#{1, 2, 3}

    /////////////////////////////////////////////////////////////////////////////
    // scanLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def scanLeft01(): Bool =
        let v = Vector.scanLeft((i, b) -> if (b) i + 1 else i + 2, 1, Vector#{}: Vector[Bool]);
        v == Vector#{1}

    @Test
    def scanLeft02(): Bool =
        let v = Vector.scanLeft((i, b) -> if (b) i + 1 else i + 2, 1, Vector#{false});
        v == Vector#{1, 3}

    @Test
    def scanLeft03(): Bool =
        let v = Vector.scanLeft((i, b) -> if (b) i + 1 else i + 2, 1, Vector#{true});
        v == Vector#{1, 2}

    @Test
    def scanLeft04(): Bool =
        let v = Vector.scanLeft((i, b) -> if (b) i + 1 else i + 2, 1, Vector#{false, false});
        v == Vector#{1, 3, 5}

    @Test
    def scanLeft05(): Bool =
        let v = Vector.scanLeft((i, b) -> if (b) i + 1 else i + 2, 1, Vector#{false, true});
        v == Vector#{1, 3, 4}

    @Test
    def scanLeft06(): Bool =
        let v = Vector.scanLeft((i, b) -> if (b) i + 1 else i + 2, 1, Vector#{true, false});
        v == Vector#{1, 2, 4}

    @Test
    def scanLeft07(): Bool =
        let v = Vector.scanLeft((i, b) -> if (b) i + 1 else i + 2, 1, Vector#{true, true});
        v == Vector#{1, 2, 3}

    /////////////////////////////////////////////////////////////////////////////
    // scanRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def scanRight01(): Bool =
        let v = Vector.scanRight((b, i) -> if (b) i + 1 else i + 2, 1, Vector#{});
        v == Vector#{1}

    @Test
    def scanRight02(): Bool =
        let v = Vector.scanRight((b, i) -> if (b) i + 1 else i + 2, 1, Vector#{false});
        v == Vector#{3, 1}

    @Test
    def scanRight03(): Bool =
        let v = Vector.scanRight((b, i) -> if (b) i + 1 else i + 2, 1, Vector#{true});
        v == Vector#{2, 1}

    @Test
    def scanRight04(): Bool =
        let v = Vector.scanRight((b, i) -> if (b) i + 1 else i + 2, 1, Vector#{false, false});
        v == Vector#{5, 3, 1}

    @Test
    def scanRight05(): Bool =
        let v = Vector.scanRight((b, i) -> if (b) i + 1 else i + 2, 1, Vector#{false, true});
        v == Vector#{4, 2, 1}

    @Test
    def scanRight06(): Bool =
        let v = Vector.scanRight((b, i) -> if (b) i + 1 else i + 2, 1, Vector#{true, false});
        v == Vector#{4, 3, 1}

    @Test
    def scanRight07(): Bool =
        let v = Vector.scanRight((b, i) -> if (b) i + 1 else i + 2, 1, Vector#{true, true});
        v == Vector#{3, 2, 1}

    /////////////////////////////////////////////////////////////////////////////
    // map                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def map01(): Bool =
        let v = Vector.map(i -> i > 2, Vector#{});
        v == Vector#{}

    @Test
    def map02(): Bool =
        let v = Vector.map(i -> i > 2, Vector#{1});
        v == Vector#{false}

    @Test
    def map03(): Bool =
        let v = Vector.map(i -> i > 2, Vector#{3});
        v == Vector#{true}

    @Test
    def map04(): Bool =
        let v = Vector.map(i -> i > 2, Vector#{1, 2});
        v == Vector#{false, false}

    @Test
    def map05(): Bool =
        let v = Vector.map(i -> i > 2, Vector#{1, 8});
        v == Vector#{false, true}

    @Test
    def map06(): Bool =
        let v = Vector.map(i -> i > 2, Vector#{8, 1});
        v == Vector#{true, false}

    @Test
    def map07(): Bool =
        let v = Vector.map(i -> i > 2, Vector#{7, 8});
        v == Vector#{true, true}

    /////////////////////////////////////////////////////////////////////////////
    // mapWithIndex                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def mapWithIndex01(): Bool =
        let v = Vector.mapWithIndex((i, e) -> if (i < 1) e > 2 else e <= 2, Vector#{});
        v == Vector#{}

    @Test
    def mapWithIndex02(): Bool =
        let v = Vector.mapWithIndex((i, e) -> if (i < 1) e > 2 else e <= 2, Vector#{1});
        v == Vector#{false}

    @Test
    def mapWithIndex03(): Bool =
        let v = Vector.mapWithIndex((i, e) -> if (i < 1) e > 2 else e <= 2, Vector#{3});
        v == Vector#{true}

    @Test
    def mapWithIndex04(): Bool =
        let v = Vector.mapWithIndex((i, e) -> if (i < 1) e > 2 else e <= 2, Vector#{1, 2});
        v == Vector#{false, true}

    @Test
    def mapWithIndex05(): Bool =
        let v = Vector.mapWithIndex((i, e) -> if (i < 1) e > 2 else e <= 2, Vector#{1, 8});
        v == Vector#{false, false}

    @Test
    def mapWithIndex06(): Bool =
        let v = Vector.mapWithIndex((i, e) -> if (i < 1) e > 2 else e <= 2, Vector#{8, 1});
        v == Vector#{true, true}

    @Test
    def mapWithIndex07(): Bool =
        let v = Vector.mapWithIndex((i, e) -> if (i < 1) e > 2 else e <= 2, Vector#{7, 8});
        v == Vector#{true, false}

    /////////////////////////////////////////////////////////////////////////////
    // ap                                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def ap01(): Bool =
        let v = Vector.ap((Vector#{} : Vector[Int32 -> Int32]), (Vector#{} : Vector[Int32]));
        v == Vector#{}

    @Test
    def ap02(): Bool =
        let v = Vector.ap(Vector#{(x -> x+1)}, Vector#{});
        v == Vector#{}

    @Test
    def ap03(): Bool =
        let v = Vector.ap((Vector#{} : Vector[Int32 -> Int32]), Vector#{5});
        v == Vector#{}

    @Test
    def ap04(): Bool =
        let v = Vector.ap(Vector#{(x -> x+1)}, Vector#{5});
        v == Vector#{6}

    @Test
    def ap05(): Bool =
        let v = Vector.ap(Vector#{(x -> x+1)}, Vector#{0, 5});
        v == Vector#{1, 6}

    @Test
    def ap06(): Bool =
        let v = Vector.ap(Vector#{(x -> x+1), (x -> x*2)}, Vector#{0, 4});
        v == Vector#{1, 5, 0, 8}

    /////////////////////////////////////////////////////////////////////////////
    // sequence                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sequence01(): Bool =
        let v: Vector[Option[Int32]] = Vector#{};
        Vector.sequence(v) == Some(Vector#{})

    @Test
    def sequence02(): Bool =
        let v: Vector[Option[Int32]] = Vector#{None};
        Vector.sequence(v) == None

    @Test
    def sequence03(): Bool =
        let v = Vector#{Some(1)};
        Vector.sequence(v) == Some(Vector#{1})

    @Test
    def sequence04(): Bool =
        let v: Vector[Option[Int32]] = Vector#{None, None};
        Vector.sequence(v) == None

    @Test
    def sequence05(): Bool =
        let v = Vector#{Some(1), None};
        Vector.sequence(v) == None

    @Test
    def sequence06(): Bool =
        let v = Vector#{None, Some(2)};
        Vector.sequence(v) == None

    @Test
    def sequence07(): Bool =
        let v = Vector#{Some(1), Some(2)};
        Vector.sequence(v) == Some(Vector#{1, 2})

    /////////////////////////////////////////////////////////////////////////////
    // traverse                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def traverse01(): Bool =
        let v = Vector#{};
        Vector.traverse(x -> if (x > 2) None else Some(x), v) == Some(Vector#{})

    @Test
    def traverse02(): Bool =
        let v = Vector#{1};
        Vector.traverse(x -> if (x > 2) None else Some(x), v) == Some(Vector#{1})

    @Test
    def traverse03(): Bool =
        let v = Vector#{1, 2};
        Vector.traverse(x -> if (x > 2) None else Some(x), v) == Some(Vector#{1, 2})

    @Test
    def traverse04(): Bool =
        let v = Vector#{1, 2, 3};
        Vector.traverse(x -> if (x > 2) None else Some(x), v) == None

    @Test
    def traverse05(): Bool =
        let v = Vector#{3};
        Vector.traverse(x -> if (x > 2) None else Some(x), v) == None

    @Test
    def traverse06(): Bool =
        let v = Vector#{1, 2, 1};
        Vector.traverse(x -> if (x > 2) None else Some(x), v) == Some(Vector#{1, 2, 1})

    /////////////////////////////////////////////////////////////////////////////
    // zipWithA                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def zipWithA01(): Bool =
        let v = Vector.zipWithA((x, y) -> if (x + y > 5) None else Some(x + y), Vector#{}, Vector#{});
        v == Some(Vector#{})
    @Test
    def zipWithA02(): Bool =
        let v = Vector.zipWithA((x, y) -> if (x + y > 5) None else Some(x + y), Vector#{1}, Vector#{});
        v == Some(Vector#{})

    @Test
    def zipWithA03(): Bool =
        let v = Vector.zipWithA((x, y) -> if (x + y > 5) None else Some(x + y), Vector#{1}, Vector#{1});
        v == Some(Vector#{2})

    @Test
    def zipWithA04(): Bool =
        let v = Vector.zipWithA((x, y) -> if (x + y > 5) None else Some(x + y), Vector#{1, 2}, Vector#{1, 2});
        v == Some(Vector#{2, 4})

    @Test
    def zipWithA05(): Bool =
        let v = Vector.zipWithA((x, y) -> if (x + y > 5) None else Some(x + y), Vector#{1, 2, 3}, Vector#{1, 2, 3});
        v == None

    @Test
    def zipWithA06(): Bool =
        let v = Vector.zipWithA((x, y) -> if (x + y > 5) None else Some(x + y), Vector#{1, 2, 3}, Vector#{1, 1, 1});
        v == Some(Vector#{2, 3, 4})

    /////////////////////////////////////////////////////////////////////////////
    // flatMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def flatMap01(): Bool =
        let v = Vector.flatMap(i -> Vector.repeat(i, i), (Vector#{}: Vector[Int32]));
        v == Vector#{}

    @Test
    def flatMap02(): Bool =
        let v = Vector.flatMap(i -> Vector.repeat(i, i), (Vector#{}: Vector[Int32]));
        v == Vector#{}

    @Test
    def flatMap03(): Bool =
        let v = Vector.flatMap(i -> Vector.repeat(i, i), Vector#{1});
        v == Vector#{1}

    @Test
    def flatMap04(): Bool =
        let v = Vector.flatMap(i -> Vector.repeat(i, i), Vector#{2});
        v == Vector#{2, 2}

    @Test
    def flatMap05(): Bool =
        let v = Vector.flatMap(i -> Vector.repeat(i, i), Vector#{1, 2});
        v == Vector#{1, 2, 2}

    @Test
    def flatMap06(): Bool =
        let v = Vector.flatMap(i -> Vector.repeat(i, i), Vector#{2, 3});
        v == Vector#{2, 2, 3, 3, 3}

    /////////////////////////////////////////////////////////////////////////////
    // reverse                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def reverse01(): Bool =
        let v = Vector.reverse(Vector#{}: Vector[Int32]);
        v == Vector#{}

    @Test
    def reverse02(): Bool =
        let v = Vector.reverse(Vector#{1});
        v == Vector#{1}

    @Test
    def reverse03(): Bool =
        let v = Vector.reverse(Vector#{1, 2});
        v == Vector#{2, 1}

    @Test
    def reverse04(): Bool =
        let v = Vector.reverse(Vector#{1, 1});
        v == Vector#{1, 1}

    @Test
    def reverse05(): Bool =
        let v = Vector.reverse(Vector#{1, 2, 3});
        v == Vector#{3, 2, 1}

    @Test
    def reverse06(): Bool =
        let v = Vector.reverse(Vector#{1, 2, 3, 4});
        v == Vector#{4, 3, 2, 1}

    /////////////////////////////////////////////////////////////////////////////
    // rotateLeft                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def rotateLeft01(): Bool =
        let v = Vector.rotateLeft(0, (Vector#{}: Vector[Int32]));
        v == Vector#{}

    @Test
    def rotateLeft02(): Bool =
        let v = Vector.rotateLeft(1, (Vector#{}: Vector[Int32]));
        v == Vector#{}

    @Test
    def rotateLeft03(): Bool =
        let v = Vector.rotateLeft(0, Vector#{1});
        v == Vector#{1}

    @Test
    def rotateLeft04(): Bool =
        let v = Vector.rotateLeft(0, Vector#{1, 2});
        v == Vector#{1, 2}

    @Test
    def rotateLeft05(): Bool =
        let v = Vector.rotateLeft(1, Vector#{1, 2});
        v == Vector#{2, 1}

    @Test
    def rotateLeft06(): Bool =
        let v = Vector.rotateLeft(2, Vector#{1, 2});
        v == Vector#{1, 2}

    @Test
    def rotateLeft07(): Bool =
        let v = Vector.rotateLeft(3, Vector#{1, 2});
        v == Vector#{2, 1}

    @Test
    def rotateLeft08(): Bool =
        let v = Vector.rotateLeft(-1, Vector#{1, 2});
        v == Vector#{2, 1}

    @Test
    def rotateLeft09(): Bool =
        let v = Vector.rotateLeft(0, Vector#{1, 2, 3});
        v == Vector#{1, 2, 3}

    @Test
    def rotateLeft10(): Bool =
        let v = Vector.rotateLeft(1, Vector#{1, 2, 3});
        v == Vector#{2, 3, 1}

    @Test
    def rotateLeft11(): Bool =
        let v = Vector.rotateLeft(2, Vector#{1, 2, 3});
        v == Vector#{3, 1, 2}

    /////////////////////////////////////////////////////////////////////////////
    // rotateRight                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def rotateRight01(): Bool =
        let v = Vector.rotateRight(0, (Vector#{}: Vector[Int32]));
        v == Vector#{}

    @Test
    def rotateRight02(): Bool =
        let v = Vector.rotateRight(1, (Vector#{}: Vector[Int32]));
        v == Vector#{}

    @Test
    def rotateRight03(): Bool =
        let v = Vector.rotateRight(0, Vector#{1});
        v == Vector#{1}

    @Test
    def rotateRight04(): Bool =
        let v = Vector.rotateRight(0, Vector#{1, 2});
        v == Vector#{1, 2}

    @Test
    def rotateRight05(): Bool =
        let v = Vector.rotateRight(1, Vector#{1, 2});
        v == Vector#{2, 1}

    @Test
    def rotateRight06(): Bool =
        let v = Vector.rotateRight(2, Vector#{1, 2});
        v == Vector#{1, 2}

    @Test
    def rotateRight07(): Bool =
        let v = Vector.rotateRight(3, Vector#{1, 2});
        v == Vector#{2, 1}

    @Test
    def rotateRight08(): Bool =
        let v = Vector.rotateRight(-1, Vector#{1, 2});
        v == Vector#{2, 1}

    @Test
    def rotateRight09(): Bool =
        let v = Vector.rotateRight(0, Vector#{1, 2, 3});
        v == Vector#{1, 2, 3}

    @Test
    def rotateRight10(): Bool =
        let v = Vector.rotateRight(1, Vector#{1, 2, 3});
        v == Vector#{3, 1, 2}

    @Test
    def rotateRight11(): Bool =
        let v = Vector.rotateRight(2, Vector#{1, 2, 3});
        v == Vector#{2, 3, 1}

    /////////////////////////////////////////////////////////////////////////////
    // update                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def update01(): Bool =
        let v = Vector.update(0, 2, (Vector#{}: Vector[Int32]));
        v == Vector#{}

    @Test
    def update02(): Bool =
        let v = Vector.update(-1, 2, Vector#{1});
        v == Vector#{1}

    @Test
    def update03(): Bool =
        let v = Vector.update(0, 2, Vector#{1});
        v == Vector#{2}

    @Test
    def update04(): Bool =
        let v = Vector.update(1, 2, Vector#{1});
        v == Vector#{1}

    @Test
    def update05(): Bool =
        let v = Vector.update(0, 5, Vector#{1, 2});
        v == Vector#{5, 2}

    @Test
    def update06(): Bool =
        let v = Vector.update(1, 5, Vector#{1, 2});
        v == Vector#{1, 5}

    @Test
    def update07(): Bool =
        let v = Vector.update(2, 5, Vector#{1, 2});
        v == Vector#{1, 2}

    /////////////////////////////////////////////////////////////////////////////
    // replace                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def replace01(): Bool =
        let v = Vector.replace(from = 3, to = 4, (Vector#{}: Vector[Int32]));
        v == Vector#{}

    @Test
    def replace02(): Bool =
        let v = Vector.replace(from = 3, to = 4, Vector#{1});
        v == Vector#{1}

    @Test
    def replace03(): Bool =
        let v = Vector.replace(from = 3, to = 4, Vector#{3});
        v == Vector#{4}

    @Test
    def replace04(): Bool =
        let v = Vector.replace(from = 3, to = 4, Vector#{4});
        v == Vector#{4}

    @Test
    def replace05(): Bool =
        let v = Vector.replace(from = 3, to = 4, Vector#{1, 2});
        v == Vector#{1, 2}

    @Test
    def replace06(): Bool =
        let v = Vector.replace(from = 3, to = 4, Vector#{1, 3});
        v == Vector#{1, 4}

    @Test
    def replace07(): Bool =
        let v = Vector.replace(from = 3, to = 4, Vector#{3, 4});
        v == Vector#{4, 4}

    @Test
    def replace08(): Bool =
        let v = Vector.replace(from = 3, to = 4, Vector#{3, 3});
        v == Vector#{4, 4}

    /////////////////////////////////////////////////////////////////////////////
    // patch                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def patch01(): Bool =
        let v = Vector.patch(0, 0, (Vector#{}: Vector[Int32]), (Vector#{}: Vector[Int32]));
        v == Vector#{}

    @Test
    def patch02(): Bool =
        let v = Vector.patch(0, 2, Vector#{1, 2}, Vector#{});
        v == Vector#{}

    @Test
    def patch03(): Bool =
        let v = Vector.patch(0, 2, Vector#{}, Vector#{1, 2});
        v == Vector#{1, 2}

    @Test
    def patch04(): Bool =
        let v = Vector.patch(-3, 3, Vector#{1, 2, 4}, Vector#{1, 2});
        v == Vector#{1, 2}

    @Test
    def patch05(): Bool =
        let v = Vector.patch(2, 3, Vector#{1, 2, 4}, Vector#{1, 2});
        v == Vector#{1, 2}

    @Test
    def patch06(): Bool =
        let v = Vector.patch(0, 0, Vector#{}, Vector#{1});
        v == Vector#{1}

    @Test
    def patch07(): Bool =
        let v = Vector.patch(1, 0, Vector#{2}, Vector#{1});
        v == Vector#{1}

    @Test
    def patch08(): Bool =
        let v = Vector.patch(0, 1, Vector#{2}, Vector#{1});
        v == Vector#{2}

    @Test
    def patch09(): Bool =
        let v = Vector.patch(0, 2, Vector#{2, 4}, Vector#{1});
        v == Vector#{2}

    @Test
    def patch10(): Bool =
        let v = Vector.patch(-1, 2, Vector#{2, 4}, Vector#{1});
        v == Vector#{4}

    @Test
    def patch11(): Bool =
        let v = Vector.patch(-1, 2, Vector#{3, 4}, Vector#{1, 2});
        v == Vector#{4, 2}

    @Test
    def patch12(): Bool =
        let v = Vector.patch(1, 2, Vector#{3, 4}, Vector#{1, 2});
        v == Vector#{1, 3}

    @Test
    def patch13(): Bool =
        let v = Vector.patch(-2, 2, Vector#{3, 4}, Vector#{1, 2});
        v == Vector#{1, 2}

    @Test
    def patch14(): Bool =
        let v = Vector.patch(2, 2, Vector#{3, 4}, Vector#{1, 2});
        v == Vector#{1, 2}

    @Test
    def patch15(): Bool =
        let v = Vector.patch(1, 1, Vector#{3}, Vector#{1, 2});
        v == Vector#{1, 3}

    @Test
    def patch16(): Bool =
        let v = Vector.patch(0, 2, Vector#{3, 4}, Vector#{1, 2});
        v == Vector#{3, 4}

    @Test
    def patch17(): Bool =
        let v = Vector.patch(0, 1, Vector#{4}, Vector#{1, 2, 3});
        v == Vector#{4, 2, 3}

    @Test
    def patch18(): Bool =
        let v = Vector.patch(1, 1, Vector#{4}, Vector#{1, 2, 3});
        v == Vector#{1, 4, 3}

    @Test
    def patch19(): Bool =
        let v = Vector.patch(2, 1, Vector#{4}, Vector#{1, 2, 3});
        v == Vector#{1, 2, 4}

    @Test
    def patch20(): Bool =
        let v = Vector.patch(0, 2, Vector#{4, 5}, Vector#{1, 2, 3});
        v == Vector#{4, 5, 3}

    @Test
    def patch21(): Bool =
        let v = Vector.patch(1, 2, Vector#{4, 5}, Vector#{1, 2, 3});
        v == Vector#{1, 4, 5}

    @Test
    def patch22(): Bool =
        let v = Vector.patch(0, 2, Vector#{4, 5, 6}, Vector#{1, 2, 3});
        v == Vector#{4, 5, 3}

    @Test
    def patch23(): Bool =
        let v = Vector.patch(0, 3, Vector#{4, 5, 6}, Vector#{1, 2, 3});
        v == Vector#{4, 5, 6}

    @Test
    def patch24(): Bool =
        let v = Vector.patch(2, 4, Vector#{14, 15, 16, 17}, Vector#{1, 2, 3, 4, 5, 6, 7});
        v == Vector#{1, 2, 14, 15, 16, 17, 7}

    @Test
    def patch25(): Bool =
        let v = Vector.patch(-2, 4, Vector#{14, 15, 16, 17}, Vector#{1, 2, 3, 4, 5, 6, 7});
        v == Vector#{16, 17, 3, 4, 5, 6, 7}

    @Test
    def patch26(): Bool =
        let v = Vector.patch(4, 5, Vector#{14, 15, 16, 17}, Vector#{1, 2, 3, 4, 5, 6, 7});
        v == Vector#{1, 2, 3, 4, 14, 15, 16}

    @Test
    def patch27(): Bool =
        let v = Vector.patch(4, 2, Vector#{14, 15, 16, 17}, Vector#{1, 2, 3, 4, 5, 6, 7});
        v == Vector#{1, 2, 3, 4, 14, 15, 7}

    @Test
    def patch28(): Bool =
        let v = Vector.patch(-1, 10, Vector#{-1, -2, -3, -4, -5, -6, -7, -8}, Vector#{1, 2, 3, 4, 5, 6, 7});
        v == Vector#{-2, -3, -4, -5, -6, -7, -8}

    /////////////////////////////////////////////////////////////////////////////
    // intersperse                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def intersperse01(): Bool =
        let v = Vector.intersperse(11, (Vector#{}: Vector[Int32]));
        v == Vector#{}

    @Test
    def intersperse02(): Bool =
        let v = Vector.intersperse(11, Vector#{1});
        v == Vector#{1}

    @Test
    def intersperse03(): Bool =
        let v = Vector.intersperse(11, Vector#{1, 2});
        v == Vector#{1, 11, 2}

    @Test
    def intersperse04(): Bool =
        let v = Vector.intersperse(11, Vector#{1, 2, 3});
        v == Vector#{1, 11, 2, 11, 3}

    @Test
    def intersperse05(): Bool =
        let v = Vector.intersperse(11, Vector#{1, 2, 3, 4});
        v == Vector#{1, 11, 2, 11, 3, 11, 4}

    /////////////////////////////////////////////////////////////////////////////
    // intercalate                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def intercalate01(): Bool =
        let v = Vector.intercalate(Vector#{}: Vector[Int32], Vector#{}: Vector[Vector[Int32]]);
        v == Vector#{}

    @Test
    def intercalate02(): Bool =
        let v = Vector.intercalate(Vector#{}: Vector[Int32], Vector#{Vector#{1}});
        v == Vector#{1}

    @Test
    def intercalate03(): Bool =
        let v = Vector.intercalate(Vector#{11, 12, 13}, Vector#{}: Vector[Vector[Int32]]);
        v == Vector#{}

    @Test
    def intercalate04(): Bool =
        let v = Vector.intercalate(Vector#{}: Vector[Int32], Vector#{Vector#{1}, Vector#{2, 3}});
        v == Vector#{1, 2, 3}

    @Test
    def intercalate05(): Bool =
        let v = Vector.intercalate(Vector#{11, 12, 13}, Vector#{Vector#{1}, Vector#{2, 3}});
        v == Vector#{1, 11, 12, 13, 2, 3}

    @Test
    def intercalate06(): Bool =
        let v = Vector.intercalate(Vector#{}: Vector[Int32], Vector#{Vector#{1}, Vector#{2, 3}, Vector#{4}});
        v == Vector#{1, 2, 3, 4}

    @Test
    def intercalate07(): Bool =
        let v = Vector.intercalate(Vector#{11, 12, 13}, Vector#{Vector#{1}, Vector#{2, 3}, Vector#{4}});
        v == Vector#{1, 11, 12, 13, 2, 3, 11, 12, 13, 4}

    /////////////////////////////////////////////////////////////////////////////
    // transpose                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def transpose01(): Bool =
        let v = Vector.transpose(Vector#{}: Vector[Vector[Int32]]);
        v == Vector#{}

    @Test
    def transpose02(): Bool =
        let v: Vector[Vector[Int32]] = Vector.transpose(Vector#{Vector#{}});
        v == Vector#{Vector#{}}

    @Test
    def transpose03(): Bool =
        let v: Vector[Vector[Int32]] = Vector.transpose(Vector#{Vector#{}, Vector#{}});
        v == Vector#{Vector#{}, Vector#{}}

    @Test
    def transpose04(): Bool =
        let v: Vector[Vector[Int32]] = Vector.transpose(Vector#{Vector#{}, Vector#{}, Vector#{}});
        v == Vector#{Vector#{}, Vector#{}, Vector#{}}

    @Test
    def transpose05(): Bool =
        let v = Vector.transpose(Vector#{Vector#{1}});
        v == Vector#{Vector#{1}}

    @Test
    def transpose06(): Bool =
        let v = Vector.transpose(Vector#{Vector#{1, 2}});
        v == Vector#{Vector#{1}, Vector#{2}}

    @Test
    def transpose07(): Bool =
        let v = Vector.transpose(Vector#{Vector#{1, 2, 3}});
        v == Vector#{Vector#{1}, Vector#{2}, Vector#{3}}

    @Test
    def transpose08(): Bool =
        let v = Vector.transpose(Vector#{Vector#{1, 2, 3, 4}});
        v == Vector#{Vector#{1}, Vector#{2}, Vector#{3}, Vector#{4}}

    @Test
    def transpose09(): Bool =
        let v = Vector.transpose(Vector#{Vector#{1}, Vector#{2}});
        v == Vector#{Vector#{1, 2}}

    @Test
    def transpose10(): Bool =
        let v = Vector.transpose(Vector#{Vector#{1}, Vector#{2}, Vector#{3}});
        v == Vector#{Vector#{1, 2, 3}}

    @Test
    def transpose11(): Bool =
        let v = Vector.transpose(Vector#{Vector#{1}, Vector#{2}, Vector#{3}, Vector#{4}});
        v == Vector#{Vector#{1, 2, 3, 4}}

    @Test
    def transpose12(): Bool =
        let v = Vector.transpose(Vector#{Vector#{1, 2}, Vector#{3, 4}});
        v == Vector#{Vector#{1, 3}, Vector#{2, 4}}

    @Test
    def transpose13(): Bool =
        let v = Vector.transpose(Vector#{Vector#{1, 2, 3}, Vector#{4, 5, 6}});
        v == Vector#{Vector#{1, 4}, Vector#{2, 5}, Vector#{3, 6}}

    @Test
    def transpose14(): Bool =
        let v = Vector.transpose(Vector#{Vector#{1, 2, 3, 4}, Vector#{5, 6, 7, 8}});
        v == Vector#{Vector#{1, 5}, Vector#{2, 6}, Vector#{3, 7}, Vector#{4, 8}}

    @Test
    def transpose15(): Bool =
        let v = Vector.transpose(Vector#{Vector#{1, 2, 3, 4, 5}, Vector#{6, 7, 8, 9, 10}});
        v == Vector#{Vector#{1, 6}, Vector#{2, 7}, Vector#{3, 8}, Vector#{4, 9}, Vector#{5, 10}}

    @Test
    def transpose16(): Bool =
        let v = Vector.transpose(Vector#{Vector#{1, 2}, Vector#{3, 4}, Vector#{5, 6}});
        v == Vector#{Vector#{1, 3, 5}, Vector#{2, 4, 6}}

    @Test
    def transpose17(): Bool =
        let v = Vector.transpose(Vector#{Vector#{1, 2}, Vector#{3, 4}, Vector#{5, 6}, Vector#{7, 8}});
        v == Vector#{Vector#{1, 3, 5, 7}, Vector#{2, 4, 6, 8}}

    @Test
    def transpose18(): Bool =
        let v = Vector.transpose(Vector#{Vector#{1, 2}, Vector#{3, 4}, Vector#{5, 6}, Vector#{7, 8}, Vector#{9, 10}});
        v == Vector#{Vector#{1, 3, 5, 7, 9}, Vector#{2, 4, 6, 8, 10}}

    @Test
    def transpose19(): Bool =
        let v = Vector.transpose(Vector#{Vector#{1, 2, 3}, Vector#{4, 5, 6}, Vector#{7, 8, 9}});
        v == Vector#{Vector#{1, 4, 7}, Vector#{2, 5, 8}, Vector#{3, 6, 9}}

    @Test
    def transpose20(): Bool =
        let v = Vector.transpose(Vector#{Vector#{1, 2, 3}, Vector#{4, 5}, Vector#{7, 8, 9}});
        v == Vector#{Vector#{1, 2, 3}, Vector#{4, 5}, Vector#{7, 8, 9}}

    @Test
    def transpose21(): Bool =
        let v = Vector.transpose(Vector#{Vector#{1, 2, 3}, Vector#{}, Vector#{7, 8, 9}});
        v == Vector#{Vector#{1, 2, 3}, Vector#{}, Vector#{7, 8, 9}}

    @Test
    def transpose22(): Bool =
        let v = Vector.transpose(Vector#{Vector#{1, 2, 3}, Vector#{4, 5, 6}, Vector#{7, 8, 9, 10}});
        v == Vector#{Vector#{1, 2, 3}, Vector#{4, 5, 6}, Vector#{7, 8, 9, 10}}

    /////////////////////////////////////////////////////////////////////////////
    // isPrefixOf                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isPrefixOf01(): Bool =
        Vector.isPrefixOf(Vector#{}: Vector[Int32], Vector#{}: Vector[Int32])

    @Test
    def isPrefixOf02(): Bool =
        Vector.isPrefixOf(Vector#{1}, Vector#{}: Vector[Int32]) == false

    @Test
    def isPrefixOf03(): Bool =
        Vector.isPrefixOf(Vector#{}: Vector[Int32], Vector#{1})

    @Test
    def isPrefixOf04(): Bool =
        Vector.isPrefixOf(Vector#{1}, Vector#{1})

    @Test
    def isPrefixOf05(): Bool =
        Vector.isPrefixOf(Vector#{2}, Vector#{1}) == false

    @Test
    def isPrefixOf06(): Bool =
        Vector.isPrefixOf(Vector#{1, 2}, Vector#{1}) == false

    @Test
    def isPrefixOf07(): Bool =
        Vector.isPrefixOf(Vector#{}: Vector[Int32], Vector#{1, 2})

    @Test
    def isPrefixOf08(): Bool =
        Vector.isPrefixOf(Vector#{1}, Vector#{1, 2})

    @Test
    def isPrefixOf09(): Bool =
        Vector.isPrefixOf(Vector#{2}, Vector#{1, 2}) == false

    @Test
    def isPrefixOf10(): Bool =
        Vector.isPrefixOf(Vector#{1, 2}, Vector#{1, 2})

    @Test
    def isPrefixOf11(): Bool =
        Vector.isPrefixOf(Vector#{1, 3}, Vector#{1, 2}) == false

    @Test
    def isPrefixOf12(): Bool =
        Vector.isPrefixOf(Vector#{1, 2, 3}, Vector#{1, 2}) == false

    @Test
    def isPrefixOf13(): Bool =
        Vector.isPrefixOf(Vector#{1, 2, 3}, Vector#{89, 11, 1, 2, 3, 4}) == false

    /////////////////////////////////////////////////////////////////////////////
    // isInfixOf                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isInfixOf01(): Bool =
        Vector.isInfixOf(Vector#{}: Vector[Int32], Vector#{}: Vector[Int32])

    @Test
    def isInfixOf02(): Bool =
        Vector.isInfixOf(Vector#{1}, Vector#{}: Vector[Int32]) == false

    @Test
    def isInfixOf03(): Bool =
        Vector.isInfixOf(Vector#{}: Vector[Int32], Vector#{1})

    @Test
    def isInfixOf04(): Bool =
        Vector.isInfixOf(Vector#{1}, Vector#{1})

    @Test
    def isInfixOf05(): Bool =
        Vector.isInfixOf(Vector#{2}, Vector#{1}) == false

    @Test
    def isInfixOf06(): Bool =
        Vector.isInfixOf(Vector#{1, 2}, Vector#{1}) == false

    @Test
    def isInfixOf07(): Bool =
        Vector.isInfixOf(Vector#{}: Vector[Int32], Vector#{1, 2})

    @Test
    def isInfixOf08(): Bool =
        Vector.isInfixOf(Vector#{1}, Vector#{1, 2})

    @Test
    def isInfixOf09(): Bool =
        Vector.isInfixOf(Vector#{2}, Vector#{1, 2})

    @Test
    def isInfixOf10(): Bool =
        Vector.isInfixOf(Vector#{1, 2}, Vector#{1, 2})

    @Test
    def isInfixOf11(): Bool =
        Vector.isInfixOf(Vector#{1, 3}, Vector#{1, 2}) == false

    @Test
    def isInfixOf12(): Bool =
        Vector.isInfixOf(Vector#{1, 2, 3}, Vector#{1, 2}) == false

    @Test
    def isInfixOf13(): Bool =
        Vector.isInfixOf(Vector#{1, 2, 3}, Vector#{89, 11, 1, 2, 3, 4})

    /////////////////////////////////////////////////////////////////////////////
    // isSuffixOf                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isSuffixOf01(): Bool =
        Vector.isSuffixOf(Vector#{}: Vector[Int32], Vector#{}: Vector[Int32])

    @Test
    def isSuffixOf02(): Bool =
        Vector.isSuffixOf(Vector#{1}, Vector#{}: Vector[Int32]) == false

    @Test
    def isSuffixOf03(): Bool =
        Vector.isSuffixOf(Vector#{}: Vector[Int32], Vector#{1})

    @Test
    def isSuffixOf04(): Bool =
        Vector.isSuffixOf(Vector#{1}, Vector#{1})

    @Test
    def isSuffixOf05(): Bool =
        Vector.isSuffixOf(Vector#{2}, Vector#{1}) == false

    @Test
    def isSuffixOf06(): Bool =
        Vector.isSuffixOf(Vector#{1, 2}, Vector#{1}) == false

    @Test
    def isSuffixOf07(): Bool =
        Vector.isSuffixOf(Vector#{}: Vector[Int32], Vector#{1, 2})

    @Test
    def isSuffixOf08(): Bool =
        Vector.isSuffixOf(Vector#{1}, Vector#{1, 2}) == false

    @Test
    def isSuffixOf09(): Bool =
        Vector.isSuffixOf(Vector#{2}, Vector#{1, 2})

    @Test
    def isSuffixOf10(): Bool =
        Vector.isSuffixOf(Vector#{1, 2}, Vector#{1, 2})

    @Test
    def isSuffixOf11(): Bool =
        Vector.isSuffixOf(Vector#{1, 3}, Vector#{1, 2}) == false

    @Test
    def isSuffixOf12(): Bool =
        Vector.isSuffixOf(Vector#{1, 2, 3}, Vector#{1, 2}) == false

    @Test
    def isSuffixOf13(): Bool =
        Vector.isSuffixOf(Vector#{1, 2, 3}, Vector#{89, 11, 1, 2, 3})

    /////////////////////////////////////////////////////////////////////////////
    // fold                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def fold01(): Bool =
        Vector.fold((Vector#{}): Vector[Unit]) == ()

    @Test
    def fold02(): Bool =
        Vector.fold(Vector#{"a"}) == "a"

    @Test
    def fold03(): Bool =
        Vector.fold(Vector#{"a", "b", "c"}) == "abc"

    @Test
    def fold04(): Bool =
        Vector.fold(Vector#{("yes", "no"), ("no", "yes")}) == ("yesno", "noyes")

    /////////////////////////////////////////////////////////////////////////////
    // foldLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldLeft01(): Bool =
        Vector.foldLeft((i, e) -> (i - e) * (e `Int32.rem` 2 + 1), 100, Vector#{}) == 100

    @Test
    def foldLeft02(): Bool =
        Vector.foldLeft((i, e) -> (i - e) * (e `Int32.rem` 2 + 1), 100, Vector#{1}) == 198

    @Test
    def foldLeft03(): Bool =
        Vector.foldLeft((i, e) -> (i - e) * (e `Int32.rem` 2 + 1), 100, Vector#{1, 2}) == 196

    @Test
    def foldLeft04(): Bool =
        Vector.foldLeft((i, e) -> (i - e) * (e `Int32.rem` 2 + 1), 100, Vector#{1, 2, 3}) == 386

    /////////////////////////////////////////////////////////////////////////////
    // foldRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldRight01(): Bool =
        Vector.foldRight((e, i) -> (i - e) * (e `Int32.rem` 2 + 1), 100, Vector#{}) == 100

    @Test
    def foldRight02(): Bool =
        Vector.foldRight((e, i) -> (i - e) * (e `Int32.rem` 2 + 1), 100, Vector#{1}) == 198

    @Test
    def foldRight03(): Bool =
        Vector.foldRight((e, i) -> (i - e) * (e `Int32.rem` 2 + 1), 100, Vector#{1, 2}) == 194

    @Test
    def foldRight04(): Bool =
        Vector.foldRight((e, i) -> (i - e) * (e `Int32.rem` 2 + 1), 100, Vector#{1, 2, 3}) == 382

    /////////////////////////////////////////////////////////////////////////////
    // foldRightWithCont                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldRightWithCont01(): Bool =
        Vector.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.rem` 2 + 1), 100, Vector#{}) == 100

    @Test
    def foldRightWithCont02(): Bool =
        Vector.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.rem` 2 + 1), 100, Vector#{1}) == 198

    @Test
    def foldRightWithCont03(): Bool =
        Vector.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.rem` 2 + 1), 100, Vector#{1, 2}) == 194

    @Test
    def foldRightWithCont04(): Bool =
        Vector.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.rem` 2 + 1), 100, Vector#{1, 2, 3}) == 382

    /////////////////////////////////////////////////////////////////////////////
    // foldMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldMap01(): Bool =
        Vector.foldMap(x -> 2 * x, Vector#{}) == 0

    @Test
    def foldMap02(): Bool =
        Vector.foldMap(x -> 2 * x, Vector#{1, 2}) == 6

    @Test
    def foldMap03(): Bool =
        Vector.foldMap(x -> if (x == "a") "b" else x, Vector#{"a"}) == "b"

    @Test
    def foldMap04(): Bool =
        Vector.foldMap(x -> if (x == "c") "b" else x, Vector#{"a", "b", "c"}) == "abb"

    @Test
    def foldMap05(): Bool =
        Vector.foldMap(Int32.toString, Vector#{1, 2, 3}) == "123"

    /////////////////////////////////////////////////////////////////////////////
    // reduceLeft                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def reduceLeft01(): Bool =
        Vector.reduceLeft((a, b) -> a - b, Vector#{}: Vector[Int32]) == None

    @Test
    def reduceLeft02(): Bool =
        Vector.reduceLeft((a, b) -> a - b, Vector#{1}) == Some(1)

    @Test
    def reduceLeft03(): Bool =
        Vector.reduceLeft((a, b) -> a - b, Vector#{1, 2}) == Some(-1)

    @Test
    def reduceLeft04(): Bool =
        Vector.reduceLeft((a, b) -> a - b, Vector#{1, 2, 3}) == Some(-4)

    @Test
    def reduceLeft05(): Bool =
        Vector.reduceLeft((a, b) -> a - b, Vector#{1, 2, 3, 4}) == Some(-8)

    /////////////////////////////////////////////////////////////////////////////
    // reduceRight                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def reduceRight01(): Bool =
        Vector.reduceRight((a, b) -> a - b, Vector#{}: Vector[Int32]) == None

    @Test
    def reduceRight02(): Bool =
        Vector.reduceRight((a, b) -> a - b, Vector#{1}) == Some(1)

    @Test
    def reduceRight03(): Bool =
        Vector.reduceRight((a, b) -> a - b, Vector#{1, 2}) == Some(-1)

    @Test
    def reduceRight04(): Bool =
        Vector.reduceRight((a, b) -> a - b, Vector#{1, 2, 3}) == Some(2)

    @Test
    def reduceRight05(): Bool =
        Vector.reduceRight((a, b) -> a - b, Vector#{1, 2, 3, 4}) == Some(-2)

    /////////////////////////////////////////////////////////////////////////////
    // count                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def count01(): Bool =
        Vector.count(i -> i > 3, Vector#{}) == 0

    @Test
    def count02(): Bool =
        Vector.count(i -> i > 3, Vector#{1}) == 0

    @Test
    def count03(): Bool =
        Vector.count(i -> i > 3, Vector#{4}) == 1

    @Test
    def count04(): Bool =
        Vector.count(i -> i > 3, Vector#{1, 2}) == 0

    @Test
    def count05(): Bool =
        Vector.count(i -> i > 3, Vector#{1, 8}) == 1

    @Test
    def count06(): Bool =
        Vector.count(i -> i > 3, Vector#{8, 1}) == 1

    @Test
    def count07(): Bool =
        Vector.count(i -> i > 3, Vector#{6, 7}) == 2

    /////////////////////////////////////////////////////////////////////////////
    // flatten                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def flatten01(): Bool =
        let v = Vector.flatten((Vector#{}: Vector[Vector[Int32]]));
        v == Vector#{}

    @Test
    def flatten02(): Bool =
        let v = Vector.flatten((Vector#{Vector#{}}: Vector[Vector[Int32]]));
        v == Vector#{}

    @Test
    def flatten03(): Bool =
        let v = Vector.flatten(Vector#{Vector#{1}});
        v == Vector#{1}

    @Test
    def flatten04(): Bool =
        let v = Vector.flatten(Vector#{Vector#{1, 2}});
        v == Vector#{1, 2}

    @Test
    def flatten05(): Bool =
        let v = Vector.flatten((Vector#{Vector#{}, Vector#{}}: Vector[Vector[Int32]]));
        v == Vector#{}

    @Test
    def flatten06(): Bool =
        let v = Vector.flatten(Vector#{Vector#{1}, Vector#{}});
        v == Vector#{1}

    @Test
    def flatten07(): Bool =
        let v = Vector.flatten(Vector#{Vector#{}, Vector#{1}});
        v == Vector#{1}

    @Test
    def flatten08(): Bool =
        let v = Vector.flatten(Vector#{Vector#{1}, Vector#{2}});
        v == Vector#{1, 2}

    @Test
    def flatten09(): Bool =
        let v = Vector.flatten(Vector#{Vector#{1, 2}, Vector#{3, 4, 5}});
        v == Vector#{1, 2, 3, 4, 5}

    @Test
    def flatten10(): Bool =
        let v = Vector.flatten(Vector#{Vector#{1}, Vector#{2, 3}, Vector#{4}});
        v == Vector#{1, 2, 3, 4}

    /////////////////////////////////////////////////////////////////////////////
    // exists                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def exists01(): Bool =
        Vector.exists(i -> i > 3, Vector#{}: Vector[Int32]) == false

    @Test
    def exists02(): Bool =
        Vector.exists(i -> i > 3, Vector#{1}) == false

    @Test
    def exists03(): Bool =
        Vector.exists(i -> i > 3, Vector#{5}) == true

    @Test
    def exists04(): Bool =
        Vector.exists(i -> i > 3, Vector#{1, 2}) == false

    @Test
    def exists05(): Bool =
        Vector.exists(i -> i > 3, Vector#{1, 6}) == true

    @Test
    def exists06(): Bool =
        Vector.exists(i -> i > 3, Vector#{6, 1}) == true

    @Test
    def exists07(): Bool =
        Vector.exists(i -> i > 3, Vector#{16, 6}) == true

    @Test
    def exists08(): Bool =
        Vector.exists(i -> i > 3, Vector#{1, -9, 3}) == false

    @Test
    def exists09(): Bool =
        Vector.exists(i -> i > 3, Vector#{1, 9, 3}) == true

    /////////////////////////////////////////////////////////////////////////////
    // forAll                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forAll01(): Bool =
        Vector.forAll(i -> i > 3, Vector#{}: Vector[Int32]) == true

    @Test
    def forAll02(): Bool =
        Vector.forAll(i -> i > 3, Vector#{1}) == false

    @Test
    def forAll03(): Bool =
        Vector.forAll(i -> i > 3, Vector#{5}) == true

    @Test
    def forAll04(): Bool =
        Vector.forAll(i -> i > 3, Vector#{1, 2}) == false

    @Test
    def forAll05(): Bool =
        Vector.forAll(i -> i > 3, Vector#{1, 6}) == false

    @Test
    def forAll06(): Bool =
        Vector.forAll(i -> i > 3, Vector#{6, 1}) == false

    @Test
    def forAll07(): Bool =
        Vector.forAll(i -> i > 3, Vector#{16, 6}) == true

    @Test
    def forAll08(): Bool =
        Vector.forAll(i -> i > 3, Vector#{1, -9, 3}) == false

    @Test
    def forAll09(): Bool =
        Vector.forAll(i -> i > 3, Vector#{1, 9, 3}) == false

    @Test
    def forAll10(): Bool =
        Vector.forAll(i -> i > 3, Vector#{11, 9, 31}) == true

    /////////////////////////////////////////////////////////////////////////////
    // filter                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def filter01(): Bool =
        let v = Vector.filter(i -> i > 3, (Vector#{}: Vector[Int32]));
        v == Vector#{}

    @Test
    def filter02(): Bool =
        let v = Vector.filter(i -> i > 3, Vector#{2});
        v == Vector#{}

    @Test
    def filter03(): Bool =
        let v = Vector.filter(i -> i > 3, Vector#{4});
        v == Vector#{4}

    @Test
    def filter04(): Bool =
        let v = Vector.filter(i -> i > 3, Vector#{1, 3});
        v == Vector#{}

    @Test
    def filter05(): Bool =
        let v = Vector.filter(i -> i > 3, Vector#{1, 8});
        v == Vector#{8}

    @Test
    def filter06(): Bool =
        let v = Vector.filter(i -> i > 3, Vector#{8, 1});
        v == Vector#{8}

    @Test
    def filter07(): Bool =
        let v = Vector.filter(i -> i > 3, Vector#{8, 9});
        v == Vector#{8, 9}

    @Test
    def filter08(): Bool =
        let v = Vector.filter(i -> i > 3, Vector#{1, 4, 11, 2, -22, 17});
        v == Vector#{4, 11, 17}

    /////////////////////////////////////////////////////////////////////////////
    // partition                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def partition01(): Bool =
        let (a, b) = Vector.partition(i -> i > 3, Vector#{}: Vector[Int32]);
        (a == Vector#{}) and (b == Vector#{})

    @Test
    def partition02(): Bool =
        let (a, b) = Vector.partition(i -> i > 3, Vector#{1});
        (a == Vector#{}) and (b == Vector#{1})

    @Test
    def partition03(): Bool =
        let (a, b) = Vector.partition(i -> i > 3, Vector#{4});
        (a == Vector#{4}) and (b == Vector#{})

    @Test
    def partition04(): Bool =
        let (a, b) = Vector.partition(i -> i > 3, Vector#{1, 2});
        (a ==Vector#{}) and (b == Vector#{1, 2})

    @Test
    def partition05(): Bool =
        let (a, b) = Vector.partition(i -> i > 3, Vector#{1, 5});
        (a == Vector#{5}) and (b == Vector#{1})

    @Test
    def partition06(): Bool =
        let (a, b) = Vector.partition(i -> i > 3, Vector#{5, 1});
        (a == Vector#{5}) and (b == Vector#{1})

    @Test
    def partition07(): Bool =
        let (a, b) = Vector.partition(i -> i > 3, Vector#{5, 8});
        (a == Vector#{5, 8}) and (b == Vector#{}: Vector[Int32])

    @Test
    def partition08(): Bool =
        let (a, b) = Vector.partition(i -> i > 3, Vector#{4, -3, -5, 1, 2, 16, 7, 1, 7});
        (a == Vector#{4, 16, 7, 7}) and (b == Vector#{-3, -5, 1, 2, 1})

    /////////////////////////////////////////////////////////////////////////////
    // span                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def span01(): Bool =
        let (a, b) = Vector.span(i -> i > 3, Vector#{}: Vector[Int32]);
        (a == Vector#{}) and (b == Vector#{})

    @Test
    def span02(): Bool =
        let (a, b) = Vector.span(i -> i > 3, Vector#{1});
        (a == Vector#{}) and (b == Vector#{1})

    @Test
    def span03(): Bool =
        let (a, b) = Vector.span(i -> i > 3, Vector#{4});
        (a == Vector#{4}) and (b == Vector#{})

    @Test
    def span04(): Bool =
        let (a, b) = Vector.span(i -> i > 3, Vector#{1, 2});
        (a == Vector#{}) and (b == Vector#{1, 2})

    @Test
    def span05(): Bool =
        let (a, b) = Vector.span(i -> i > 3, Vector#{1, 5});
        (a == Vector#{}) and (b == Vector#{1, 5})

    @Test
    def span06(): Bool =
        let (a, b) = Vector.span(i -> i > 3, Vector#{5, 1});
        (a == Vector#{5}) and (b == Vector#{1})

    @Test
    def span07(): Bool =
        let (a, b) = Vector.span(i -> i > 3, Vector#{5, 8});
        (a == Vector#{5, 8}) and (b == Vector#{})

    @Test
    def span08(): Bool =
        let (a, b) = Vector.span(i -> i > 3, Vector#{4, 6, -3, 11, -5, 1, 2, 16, 7, 1, 7});
        (a == Vector#{4, 6}) and (b == Vector#{-3, 11, -5, 1, 2, 16, 7, 1, 7})

    /////////////////////////////////////////////////////////////////////////////
    // drop                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def drop01(): Bool =
        let v = Vector.drop(-1, Vector#{}: Vector[Int32]);
        v == Vector#{}

    @Test
    def drop02(): Bool =
        let v = Vector.drop(0, Vector#{}: Vector[Int32]);
        v == Vector#{}

    @Test
    def drop03(): Bool =
        let v = Vector.drop(1, Vector#{}: Vector[Int32]);
        v == Vector#{}

    @Test
    def drop04(): Bool =
        let v = Vector.drop(-1, Vector#{1});
        v == Vector#{1}

    @Test
    def drop05(): Bool =
        let v = Vector.drop(0, Vector#{1});
        v == Vector#{1}

    @Test
    def drop06(): Bool =
        let v = Vector.drop(1, Vector#{1});
        v == Vector#{}

    @Test
    def drop07(): Bool =
        let v = Vector.drop(2, Vector#{1});
        v == Vector#{}

    @Test
    def drop08(): Bool =
        let v = Vector.drop(0, Vector#{1, 2});
        v == Vector#{1, 2}

    @Test
    def drop09(): Bool =
        let v = Vector.drop(1, Vector#{1, 2});
        v == Vector#{2}

    @Test
    def drop10(): Bool =
        let v = Vector.drop(2, Vector#{1, 2});
        v == Vector#{}

    @Test
    def drop11(): Bool =
        let v = Vector.drop(2, Vector#{1, 2, 3, 4, 5, 6});
        v == Vector#{3, 4, 5, 6}

    @Test
    def drop12(): Bool =
        let v = Vector.drop(4, Vector#{1, 2, 3, 4, 5, 6});
        v == Vector#{5, 6}

    /////////////////////////////////////////////////////////////////////////////
    // dropLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def dropLeft01(): Bool =
        let v = Vector.dropLeft(-1, Vector#{}: Vector[Int32]);
        v == Vector#{}

    @Test
    def dropLeft02(): Bool =
        let v = Vector.dropLeft(0, Vector#{}: Vector[Int32]);
        v == Vector#{}

    @Test
    def dropLeft03(): Bool =
        let v = Vector.dropLeft(1, Vector#{}: Vector[Int32]);
        v == Vector#{}

    @Test
    def dropLeft04(): Bool =
        let v = Vector.dropLeft(-1, Vector#{1});
        v == Vector#{1}

    @Test
    def dropLeft05(): Bool =
        let v = Vector.dropLeft(0, Vector#{1});
        v == Vector#{1}

    @Test
    def dropLeft06(): Bool =
        let v = Vector.dropLeft(1, Vector#{1});
        v == Vector#{}

    @Test
    def dropLeft07(): Bool =
        let v = Vector.dropLeft(2, Vector#{1});
        v == Vector#{}

    @Test
    def dropLeft08(): Bool =
        let v = Vector.dropLeft(0, Vector#{1, 2});
        v == Vector#{1, 2}

    @Test
    def dropLeft09(): Bool =
        let v = Vector.dropLeft(1, Vector#{1, 2});
        v == Vector#{2}

    @Test
    def dropLeft10(): Bool =
        let v = Vector.dropLeft(2, Vector#{1, 2});
        v == Vector#{}

    @Test
    def dropLeft11(): Bool =
        let v = Vector.dropLeft(2, Vector#{1, 2, 3, 4, 5, 6});
        v == Vector#{3, 4, 5, 6}

    @Test
    def dropLeft12(): Bool =
        let v = Vector.dropLeft(4, Vector#{1, 2, 3, 4, 5, 6});
        v == Vector#{5, 6}

    /////////////////////////////////////////////////////////////////////////////
    // dropRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def dropRight01(): Bool =
        let v = Vector.dropRight(-1, Vector#{}: Vector[Int32]);
        v == Vector#{}

    @Test
    def dropRight02(): Bool =
        let v = Vector.dropRight(0, Vector#{}: Vector[Int32]);
        v == Vector#{}

    @Test
    def dropRight03(): Bool =
        let v = Vector.dropRight(1, Vector#{}: Vector[Int32]);
        v == Vector#{}

    @Test
    def dropRight04(): Bool =
        let v = Vector.dropRight(-1, Vector#{1});
        v == Vector#{1}

    @Test
    def dropRight05(): Bool =
        let v = Vector.dropRight(0, Vector#{1});
        v == Vector#{1}

    @Test
    def dropRight06(): Bool =
        let v = Vector.dropRight(1, Vector#{1});
        v == Vector#{}

    @Test
    def dropRight07(): Bool =
        let v = Vector.dropRight(2, Vector#{1});
        v == Vector#{}

    @Test
    def dropRight08(): Bool =
        let v = Vector.dropRight(0, Vector#{1, 2});
        v == Vector#{1, 2}

    @Test
    def dropRight09(): Bool =
        let v = Vector.dropRight(1, Vector#{1, 2});
        v == Vector#{1}

    @Test
    def dropRight10(): Bool =
        let v = Vector.dropRight(2, Vector#{1, 2});
        v == Vector#{}

    @Test
    def dropRight11(): Bool =
        let v = Vector.dropRight(2, Vector#{1, 2, 3, 4, 5, 6});
        v == Vector#{1, 2, 3, 4}

    @Test
    def dropRight12(): Bool =
        let v = Vector.dropRight(4, Vector#{1, 2, 3, 4, 5, 6});
        v == Vector#{1, 2}

    /////////////////////////////////////////////////////////////////////////////
    // dropWhile                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def dropWhile01(): Bool =
        let v = Vector.dropWhile(i -> i > 3, Vector#{}: Vector[Int32]);
        v == Vector#{}

    @Test
    def dropWhile02(): Bool =
        let v = Vector.dropWhile(i -> i > 3, Vector#{1});
        v == Vector#{1}

    @Test
    def dropWhile03(): Bool =
        let v = Vector.dropWhile(i -> i > 3, Vector#{4});
        v == Vector#{}

    @Test
    def dropWhile04(): Bool =
        let v = Vector.dropWhile(i -> i > 3, Vector#{1, 2});
        v == Vector#{1, 2}

    @Test
    def dropWhile05(): Bool =
        let v = Vector.dropWhile(i -> i > 3, Vector#{1, 5});
        v == Vector#{1, 5}

    @Test
    def dropWhile06(): Bool =
        let v = Vector.dropWhile(i -> i > 3, Vector#{5, 1});
        v == Vector#{1}

    @Test
    def dropWhile07(): Bool =
        let v = Vector.dropWhile(i -> i > 3, Vector#{5, 8});
        v == Vector#{}

    @Test
    def dropWhile08(): Bool =
        let v = Vector.dropWhile(i -> i > 3, Vector#{4, 6, -3, 11, -5, 1, 2, 16, 7, 1, 7});
        v == Vector#{-3, 11, -5, 1, 2, 16, 7, 1, 7}

    /////////////////////////////////////////////////////////////////////////////
    // dropWhileLeft                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def dropWhileLeft01(): Bool =
        let v = Vector.dropWhileLeft(i -> i > 3, Vector#{}: Vector[Int32]);
        v == Vector#{}

    @Test
    def dropWhileLeft02(): Bool =
        let v = Vector.dropWhileLeft(i -> i > 3, Vector#{1});
        v == Vector#{1}

    @Test
    def dropWhileLeft03(): Bool =
        let v = Vector.dropWhileLeft(i -> i > 3, Vector#{4});
        v == Vector#{}

    @Test
    def dropWhileLeft04(): Bool =
        let v = Vector.dropWhileLeft(i -> i > 3, Vector#{1, 2});
        v == Vector#{1, 2}

    @Test
    def dropWhileLeft05(): Bool =
        let v = Vector.dropWhileLeft(i -> i > 3, Vector#{1, 5});
        v == Vector#{1, 5}

    @Test
    def dropWhileLeft06(): Bool =
        let v = Vector.dropWhileLeft(i -> i > 3, Vector#{5, 1});
        v == Vector#{1}

    @Test
    def dropWhileLeft07(): Bool =
        let v = Vector.dropWhileLeft(i -> i > 3, Vector#{5, 8});
        v == Vector#{}

    @Test
    def dropWhileLeft08(): Bool =
        let v = Vector.dropWhileLeft(i -> i > 3, Vector#{4, 6, -3, 11, -5, 1, 2, 16, 7, 1, 7});
        v == Vector#{-3, 11, -5, 1, 2, 16, 7, 1, 7}

    /////////////////////////////////////////////////////////////////////////////
    // dropWhileRight                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def dropWhileRight01(): Bool =
        let v = Vector.dropWhileRight(i -> i > 3, Vector#{}: Vector[Int32]);
        v == Vector#{}

    @Test
    def dropWhileRight02(): Bool =
        let v = Vector.dropWhileRight(i -> i > 3, Vector#{1});
        v == Vector#{1}

    @Test
    def dropWhileRight03(): Bool =
        let v = Vector.dropWhileRight(i -> i > 3, Vector#{4});
        v == Vector#{}

    @Test
    def dropWhileRight04(): Bool =
        let v = Vector.dropWhileRight(i -> i > 3, Vector#{1, 2});
        v == Vector#{1, 2}

    @Test
    def dropWhileRight05(): Bool =
        let v = Vector.dropWhileRight(i -> i > 3, Vector#{1, 5});
        v == Vector#{1}

    @Test
    def dropWhileRight06(): Bool =
        let v = Vector.dropWhileRight(i -> i > 3, Vector#{5, 1});
        v == Vector#{5, 1}

    @Test
    def dropWhileRight07(): Bool =
        let v = Vector.dropWhileRight(i -> i > 3, Vector#{5, 8});
        v == Vector#{}

    @Test
    def dropWhileRight08(): Bool =
        let v = Vector.dropWhileRight(i -> i > 3, Vector#{4, 6, -3, 11, -5, 1, 2, 16, 7, 1, 7});
        v == Vector#{4, 6, -3, 11, -5, 1, 2, 16, 7, 1}

    /////////////////////////////////////////////////////////////////////////////
    // take                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def take01(): Bool =
        let v = Vector.take(-1, Vector#{}: Vector[Int32]);
        v == Vector#{}

    @Test
    def take02(): Bool =
        let v = Vector.take(0, Vector#{}: Vector[Int32]);
        v == Vector#{}

    @Test
    def take03(): Bool =
        let v = Vector.take(1, Vector#{}: Vector[Int32]);
        v == Vector#{}

    @Test
    def take04(): Bool =
        let v = Vector.take(-1, Vector#{1});
        v == Vector#{}

    @Test
    def take05(): Bool =
        let v = Vector.take(0, Vector#{1});
        v == Vector#{}

    @Test
    def take06(): Bool =
        let v = Vector.take(1, Vector#{1});
        v == Vector#{1}

    @Test
    def take07(): Bool =
        let v = Vector.take(2, Vector#{1});
        v == Vector#{1}

    @Test
    def take08(): Bool =
        let v = Vector.take(0, Vector#{1, 2});
        v == Vector#{}

    @Test
    def take09(): Bool =
        let v = Vector.take(1, Vector#{1, 2});
        v == Vector#{1}

    @Test
    def take10(): Bool =
        let v = Vector.take(2, Vector#{1, 2});
        v == Vector#{1, 2}

    @Test
    def take11(): Bool =
        let v = Vector.take(2, Vector#{1, 2, 3, 4, 5, 6});
        v == Vector#{1, 2}

    @Test
    def take12(): Bool =
        let v = Vector.take(4, Vector#{1, 2, 3, 4, 5, 6});
        v == Vector#{1, 2, 3, 4}

    /////////////////////////////////////////////////////////////////////////////
    // takeLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def takeLeft01(): Bool =
        let v = Vector.takeLeft(-1, Vector#{}: Vector[Int32]);
        v == Vector#{}

    @Test
    def takeLeft02(): Bool =
        let v = Vector.takeLeft(0, Vector#{}: Vector[Int32]);
        v == Vector#{}

    @Test
    def takeLeft03(): Bool =
        let v = Vector.takeLeft(1, Vector#{}: Vector[Int32]);
        v == Vector#{}

    @Test
    def takeLeft04(): Bool =
        let v = Vector.takeLeft(-1, Vector#{1});
        v == Vector#{}

    @Test
    def takeLeft05(): Bool =
        let v = Vector.takeLeft(0, Vector#{1});
        v == Vector#{}

    @Test
    def takeLeft06(): Bool =
        let v = Vector.takeLeft(1, Vector#{1});
        v == Vector#{1}

    @Test
    def takeLeft07(): Bool =
        let v = Vector.takeLeft(2, Vector#{1});
        v == Vector#{1}

    @Test
    def takeLeft08(): Bool =
        let v = Vector.takeLeft(0, Vector#{1, 2});
        v == Vector#{}

    @Test
    def takeLeft09(): Bool =
        let v = Vector.takeLeft(1, Vector#{1, 2});
        v == Vector#{1}

    @Test
    def takeLeft10(): Bool =
        let v = Vector.takeLeft(2, Vector#{1, 2});
        v == Vector#{1, 2}

    @Test
    def takeLeft11(): Bool =
        let v = Vector.takeLeft(2, Vector#{1, 2, 3, 4, 5, 6});
        v == Vector#{1, 2}

    @Test
    def takeLeft12(): Bool =
        let v = Vector.takeLeft(4, Vector#{1, 2, 3, 4, 5, 6});
        v == Vector#{1, 2, 3, 4}

    /////////////////////////////////////////////////////////////////////////////
    // takeRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def takeRight01(): Bool =
        let v = Vector.takeRight(-1, Vector#{}: Vector[Int32]);
        v == Vector#{}

    @Test
    def takeRight02(): Bool =
        let v = Vector.takeRight(0, Vector#{}: Vector[Int32]);
        v == Vector#{}

    @Test
    def takeRight03(): Bool =
        let v = Vector.takeRight(1, Vector#{}: Vector[Int32]);
        v == Vector#{}

    @Test
    def takeRight04(): Bool =
        let v = Vector.takeRight(-1, Vector#{1});
        v == Vector#{}

    @Test
    def takeRight05(): Bool =
        let v = Vector.takeRight(0, Vector#{1});
        v == Vector#{}

    @Test
    def takeRight06(): Bool =
        let v = Vector.takeRight(1, Vector#{1});
        v == Vector#{1}

    @Test
    def takeRight07(): Bool =
        let v = Vector.takeRight(2, Vector#{1});
        v == Vector#{1}

    @Test
    def takeRight08(): Bool =
        let v = Vector.takeRight(0, Vector#{1, 2});
        v == Vector#{}

    @Test
    def takeRight09(): Bool =
        let v = Vector.takeRight(1, Vector#{1, 2});
        v == Vector#{2}

    @Test
    def takeRight10(): Bool =
        let v = Vector.takeRight(2, Vector#{1, 2});
        v == Vector#{1, 2}

    @Test
    def takeRight11(): Bool =
        let v = Vector.takeRight(2, Vector#{1, 2, 3, 4, 5, 6});
        v == Vector#{5, 6}

    @Test
    def takeRight12(): Bool =
        let v = Vector.takeRight(4, Vector#{1, 2, 3, 4, 5, 6});
        v == Vector#{3, 4, 5, 6}

    /////////////////////////////////////////////////////////////////////////////
    // takeWhile                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def takeWhile01(): Bool =
        let v = Vector.takeWhile(i -> i > 3, Vector#{}: Vector[Int32]);
        v == Vector#{}

    @Test
    def takeWhile02(): Bool =
        let v = Vector.takeWhile(i -> i > 3, Vector#{1});
        v == Vector#{}

    @Test
    def takeWhile03(): Bool =
        let v = Vector.takeWhile(i -> i > 3, Vector#{4});
        v == Vector#{4}

    @Test
    def takeWhile04(): Bool =
        let v = Vector.takeWhile(i -> i > 3, Vector#{1, 2});
        v == Vector#{}

    @Test
    def takeWhile05(): Bool =
        let v = Vector.takeWhile(i -> i > 3, Vector#{1, 5});
        v == Vector#{}

    @Test
    def takeWhile06(): Bool =
        let v = Vector.takeWhile(i -> i > 3, Vector#{5, 1});
        v == Vector#{5}

    @Test
    def takeWhile07(): Bool =
        let v = Vector.takeWhile(i -> i > 3, Vector#{5, 8});
        v == Vector#{5, 8}

    @Test
    def takeWhile08(): Bool =
        let v = Vector.takeWhile(i -> i > 3, Vector#{4, 6, -3, 11, -5, 1, 2, 16, 7, 1, 7});
        v == Vector#{4, 6}

    /////////////////////////////////////////////////////////////////////////////
    // takeWhileLeft                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def takeWhileLeft01(): Bool =
        let v = Vector.takeWhileLeft(i -> i > 3, Vector#{}: Vector[Int32]);
        v == Vector#{}

    @Test
    def takeWhileLeft02(): Bool =
        let v = Vector.takeWhileLeft(i -> i > 3, Vector#{1});
        v == Vector#{}

    @Test
    def takeWhileLeft03(): Bool =
        let v = Vector.takeWhileLeft(i -> i > 3, Vector#{4});
        v == Vector#{4}

    @Test
    def takeWhileLeft04(): Bool =
        let v = Vector.takeWhileLeft(i -> i > 3, Vector#{1, 2});
        v == Vector#{}

    @Test
    def takeWhileLeft05(): Bool =
        let v = Vector.takeWhileLeft(i -> i > 3, Vector#{1, 5});
        v == Vector#{}

    @Test
    def takeWhileLeft06(): Bool =
        let v = Vector.takeWhileLeft(i -> i > 3, Vector#{5, 1});
        v == Vector#{5}

    @Test
    def takeWhileLeft07(): Bool =
        let v = Vector.takeWhileLeft(i -> i > 3, Vector#{5, 8});
        v == Vector#{5, 8}

    @Test
    def takeWhileLeft08(): Bool =
        let v = Vector.takeWhileLeft(i -> i > 3, Vector#{4, 6, -3, 11, -5, 1, 2, 16, 7, 1, 7});
        v == Vector#{4, 6}

    /////////////////////////////////////////////////////////////////////////////
    // takeWhileRight                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def takeWhileRight01(): Bool =
        let v = Vector.takeWhileRight(i -> i > 3, Vector#{}: Vector[Int32]);
        v == Vector#{}

    @Test
    def takeWhileRight02(): Bool =
        let v = Vector.takeWhileRight(i -> i > 3, Vector#{1});
        v == Vector#{}

    @Test
    def takeWhileRight03(): Bool =
        let v = Vector.takeWhileRight(i -> i > 3, Vector#{4});
        v == Vector#{4}

    @Test
    def takeWhileRight04(): Bool =
        let v = Vector.takeWhileRight(i -> i > 3, Vector#{1, 2});
        v == Vector#{}

    @Test
    def takeWhileRight05(): Bool =
        let v = Vector.takeWhileRight(i -> i > 3, Vector#{1, 5});
        v == Vector#{5}

    @Test
    def takeWhileRight06(): Bool =
        let v = Vector.takeWhileRight(i -> i > 3, Vector#{5, 1});
        v == Vector#{}

    @Test
    def takeWhileRight07(): Bool =
        let v = Vector.takeWhileRight(i -> i > 3, Vector#{5, 8});
        v == Vector#{5, 8}

    @Test
    def takeWhileRight08(): Bool =
        let v = Vector.takeWhileRight(i -> i > 3, Vector#{4, 6, -3, 11, -5, 1, 2, 16, 7, 1, 7});
        v == Vector#{7}

    /////////////////////////////////////////////////////////////////////////////
    // splitAt                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def splitAt01(): Bool =
        let v = Vector#{}: Vector[Int32];
        Vector.splitAt(0, v) == (Vector#{}, Vector#{})

    @Test
    def splitAt02(): Bool =
        let v = Vector#{1, 5, 19, 3};
        Vector.splitAt(3, v) == (Vector#{1, 5, 19}, Vector#{3})

    @Test
    def splitAt03(): Bool =
        let v = Vector#{1, 5, 19, 3};
        Vector.splitAt(-3, v) == (Vector#{}, v)

    @Test
    def splitAt04(): Bool =
        let v = Vector#{1, 5, 19, 3};
        Vector.splitAt(5, v) == (v, Vector#{})

    /////////////////////////////////////////////////////////////////////////////
    // groupBy                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def groupBy01(): Bool =
        let v = Vector.groupBy((a, b) -> a > 3 or b > 8, Vector#{}: Vector[Int32]);
        v == Vector#{}

    @Test
    def groupBy02(): Bool =
        let v = Vector.groupBy((a, b) -> a > 3 or b > 8, Vector#{1});
        v == Vector#{Vector#{1}}

    @Test
    def groupBy03(): Bool =
        let v = Vector.groupBy((a, b) -> a > 3 or b > 8, Vector#{1, 4});
        v == Vector#{Vector#{1}, Vector#{4}}

    @Test
    def groupBy04(): Bool =
        let v = Vector.groupBy((a, b) -> a > 3 or b > 8, Vector#{1, 9});
        v == Vector#{Vector#{1, 9}}

    @Test
    def groupBy05(): Bool =
        let v = Vector.groupBy((a, b) -> a > 3 or b > 8, Vector#{1, 4, 7, 6, 9, 2, 4, 4, 8, 16});
        v == Vector#{Vector#{1, 9, 16}, Vector#{4, 7, 6, 4, 4, 8}, Vector#{2}}

    @Test
    def groupBy06(): Bool =
        let v = Vector.groupBy((a, b) -> a > -6 or a*b >= 0, Vector#{-1, -11, 4, -11, 0, 8, 2, 1, -3, -24});
        v == Vector#{Vector#{-1, -11, -11, 0, -3, -24}, Vector#{4, 8, 2, 1}}

    @Test
    def groupBy07(): Bool =
        let v = Vector.groupBy((a, b) -> a < 0 or (a > 10 or (b > 10 or a == b)), Vector#{-5, 6, 11, 8, 8, -11, -1, 0, 4, -1});
        v == Vector#{Vector#{-5, 11, -11, -1, -1}, Vector#{6}, Vector#{8, 8}, Vector#{0}, Vector#{4}}

    /////////////////////////////////////////////////////////////////////////////
    // zip                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def zip01(): Bool =
        let v = Vector.zip(Vector#{}: Vector[Int32], Vector#{}: Vector[Int32]);
        v == Vector#{}

    @Test
    def zip02(): Bool =
        let v = Vector.zip(Vector#{1}, Vector#{}: Vector[Int32]);
        v == Vector#{}

    @Test
    def zip03(): Bool =
        let v = Vector.zip(Vector#{}: Vector[Int32], Vector#{2});
        v == Vector#{}

    @Test
    def zip04(): Bool =
        let v = Vector.zip(Vector#{1}, Vector#{2});
        v == Vector#{(1, 2)}

    @Test
    def zip05(): Bool =
        let v = Vector.zip(Vector#{1, 3}, Vector#{2, 4});
        v == Vector#{(1, 2), (3, 4)}

    @Test
    def zip06(): Bool =
        let v = Vector.zip(Vector#{1, 3, 5}, Vector#{2, 4, 6});
        v == Vector#{(1, 2), (3, 4), (5, 6)}

    @Test
    def zip07(): Bool =
        let v = Vector.zip(Vector#{1, 3, 5, 7}, Vector#{2, 4, 6, 8});
        v == Vector#{(1, 2), (3, 4), (5, 6), (7, 8)}

    /////////////////////////////////////////////////////////////////////////////
    // zipWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def zipWith01(): Bool =
        let v = Vector.zipWith((a, b) -> if (b) a + 1 else a, Vector#{}: Vector[Int32], Vector#{}: Vector[Bool]);
        v == Vector#{}

    @Test
    def zipWith02(): Bool =
        let v = Vector.zipWith((a, b) -> if (b) a + 1 else a, Vector#{1}, Vector#{}: Vector[Bool]);
        v == Vector#{}

    @Test
    def zipWith03(): Bool =
        let v = Vector.zipWith((a, b) -> if (b) a + 1 else a, Vector#{}: Vector[Int32], Vector#{true});
        v == Vector#{}

    @Test
    def zipWith04(): Bool =
        let v = Vector.zipWith((a, b) -> if (b) a + 1 else a, Vector#{1}, Vector#{true});
        v == Vector#{2}

    @Test
    def zipWith05(): Bool =
        let v = Vector.zipWith((a, b) -> if (b) a + 1 else a, Vector#{1}, Vector#{false});
        v == Vector#{1}

    @Test
    def zipWith06(): Bool =
        let v = Vector.zipWith((a, b) -> if (b) a + 1 else a,
                        Vector#{1, 2, 3, 4, 5, 6, 7, 8},
                        Vector#{false, true, true, false, false, true, true, true});
        v == Vector#{1, 3, 4, 4, 5, 7, 8, 9}

    /////////////////////////////////////////////////////////////////////////////
    // unzip                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def unzip01(): Bool =
        let (a, b) = Vector.unzip((Vector#{}): Vector[(Unit, Unit)]);
        (a == Vector#{}) and (b == Vector#{})

    @Test
    def unzip02(): Bool =
        let (a, b) = Vector.unzip(Vector#{(1, true)});
        (a == Vector#{1}) and (b == Vector#{true})

    @Test
    def unzip03(): Bool =
        let (a,b) = Vector.unzip(Vector#{(1, true), (2, true)});
        (a == Vector#{1, 2}) and (b == Vector#{true, true})

    @Test
    def unzip04(): Bool =
        let (a,b) = Vector.unzip(Vector#{(1, true), (2, true), (3, false)});
        (a == Vector#{1, 2, 3}) and (b == Vector#{true, true, false})

    @Test
    def unzip05(): Bool =
        fst(Vector.unzip(Vector#{(1, "1"), (2, "2"), (3, "3")})) == Vector#{1, 2, 3}


    /////////////////////////////////////////////////////////////////////////////
    // fold2                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def fold201(): Bool =
        Vector.fold2((c, a, b) -> if (b) a + c else a * c, 4, Vector#{}: Vector[Int32], Vector#{}: Vector[Bool]) == 4

    @Test
    def fold202(): Bool =
        Vector.fold2((c, a, b) -> if (b) a + c else a * c, 4, Vector#{1}, Vector#{}) == 4

    @Test
    def fold203(): Bool =
        Vector.fold2((c, a, b) -> if (b) a + c else a * c, 4, Vector#{}, Vector#{true}) == 4

    @Test
    def fold204(): Bool =
        Vector.fold2((c, a, b) -> if (b) a + c else a * c, 4, Vector#{2}, Vector#{true}) == 6

    @Test
    def fold205(): Bool =
        Vector.fold2((c, a, b) -> if (b) a + c else a * c, 4, Vector#{2}, Vector#{false}) == 8

    @Test
    def fold206(): Bool =
        Vector.fold2((c, a, b) -> if (b) a + c else a * c, 4, Vector#{3, 2}, Vector#{true, true}) == 9

    @Test
    def fold207(): Bool =
        Vector.fold2((c, a, b) -> if (b) a + c else a * c, 4, Vector#{3, 2}, Vector#{true, false}) == 14

    @Test
    def fold208(): Bool =
        Vector.fold2((c, a, b) -> if (b) a + c else a * c, 4, Vector#{3, 2}, Vector#{false, true}) == 14

    @Test
    def fold209(): Bool =
        Vector.fold2((c, a, b) -> if (b) a + c else a * c, 4, Vector#{3, 2}, Vector#{false, false}) == 24

    @Test
    def fold210(): Bool =
        Vector.fold2((c, a, b) -> if (b) a + c else a * c, 4, Vector#{6, -4, 3, 2},
            Vector#{true, false, false, true}) == -118

    /////////////////////////////////////////////////////////////////////////////
    // foldLeft2                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldLeft201(): Bool =
        Vector.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, Vector#{}: Vector[Int32], Vector#{}: Vector[Bool]) == 4

    @Test
    def foldLeft202(): Bool =
        Vector.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, Vector#{1}, Vector#{}: Vector[Bool]) == 4

    @Test
    def foldLeft203(): Bool =
        Vector.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, Vector#{}: Vector[Int32], Vector#{true}) == 4

    @Test
    def foldLeft204(): Bool =
        Vector.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, Vector#{2}, Vector#{true}) == 6

    @Test
    def foldLeft205(): Bool =
        Vector.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, Vector#{2}, Vector#{false}) == 8

    @Test
    def foldLeft206(): Bool =
        Vector.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, Vector#{3, 2}, Vector#{true, true}) == 9

    @Test
    def foldLeft207(): Bool =
        Vector.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, Vector#{3, 2}, Vector#{true, false}) == 14

    @Test
    def foldLeft208(): Bool =
        Vector.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, Vector#{3, 2}, Vector#{false, true}) == 14

    @Test
    def foldLeft209(): Bool =
        Vector.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, Vector#{3, 2}, Vector#{false, false}) == 24

    @Test
    def foldLeft210(): Bool =
        Vector.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, Vector#{6, -4, 3, 2},
            Vector#{true, false, false, true}) == -118

    /////////////////////////////////////////////////////////////////////////////
    // foldRight2                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldRight201(): Bool =
        Vector.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, Vector#{}: Vector[Int32], Vector#{}: Vector[Bool]) == 4

    @Test
    def foldRight202(): Bool =
        Vector.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, Vector#{1}, Vector#{}: Vector[Bool]) == 4

    @Test
    def foldRight203(): Bool =
        Vector.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, Vector#{}: Vector[Int32], Vector#{true}) == 4

    @Test
    def foldRight204(): Bool =
        Vector.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, Vector#{2}, Vector#{true}) == 6

    @Test
    def foldRight205(): Bool =
        Vector.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, Vector#{2}, Vector#{false}) == 8

    @Test
    def foldRight206(): Bool =
        Vector.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, Vector#{3, 2}, Vector#{true, true}) == 9

    @Test
    def foldRight207(): Bool =
        Vector.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, Vector#{3, 2}, Vector#{true, false}) == 11

    @Test
    def foldRight208(): Bool =
        Vector.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, Vector#{3, 2}, Vector#{false, true}) == 18

    @Test
    def foldRight209(): Bool =
        Vector.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, Vector#{3, 2}, Vector#{false, false}) == 24

    @Test
    def foldRight210(): Bool =
        Vector.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, Vector#{6, -4, 3, 2},
            Vector#{true, false, false, true}) == -66

    /////////////////////////////////////////////////////////////////////////////
    // filterMap                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def filterMap01(): Bool =
        let v = Vector.filterMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Vector#{}: Vector[Int32]);
        v == Vector#{}

    @Test
    def filterMap02(): Bool =
        let v = Vector.filterMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Vector#{1});
        v == Vector#{}

    @Test
    def filterMap03(): Bool =
        let v = Vector.filterMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Vector#{2});
        v == Vector#{1}

    @Test
    def filterMap04(): Bool =
        let v = Vector.filterMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Vector#{1, 3});
        v == Vector#{}

    @Test
    def filterMap05(): Bool =
        let v = Vector.filterMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Vector#{1, 4});
        v == Vector#{2}

    @Test
    def filterMap06(): Bool =
        let v = Vector.filterMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Vector#{6, -1});
        v == Vector#{3}

    @Test
    def filterMap07(): Bool =
        let v = Vector.filterMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Vector#{8, 6});
        v == Vector#{4, 3}

    @Test
    def filterMap08(): Bool =
        let v = Vector.filterMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Vector#{0, 1, 2, 3, 4, 5, 10});
        v == Vector#{0, 1, 2, 5}

    /////////////////////////////////////////////////////////////////////////////
    // findMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def findMap01(): Bool =
        Vector.findMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Vector#{}: Vector[Int32]) == None

    @Test
    def findMap02(): Bool =
        Vector.findMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Vector#{1}) == None

    @Test
    def findMap03(): Bool =
        Vector.findMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Vector#{2}) == Some(1)

    @Test
    def findMap04(): Bool =
        Vector.findMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Vector#{1, 3}) == None

    @Test
    def findMap05(): Bool =
        Vector.findMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Vector#{1, 4}) == Some(2)

    @Test
    def findMap06(): Bool =
        Vector.findMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Vector#{6, -1}) == Some(3)

    @Test
    def findMap07(): Bool =
        Vector.findMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Vector#{8, 6}) == Some(4)

    @Test
    def findMap08(): Bool =
        Vector.findMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Vector#{0, 1, 2, 3, 4, 5, 10}) == Some(0)

    /////////////////////////////////////////////////////////////////////////////
    // toSet                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toSet01(): Bool =
        Vector.toSet(Vector#{}: Vector[Int32]) == Set#{}

    @Test
    def toSet02(): Bool =
        Vector.toSet(Vector#{1}) == Set#{1}

    @Test
    def toSet03(): Bool =
        Vector.toSet(Vector#{1, 2}) == Set#{1, 2}

    @Test
    def toSet04(): Bool =
        Vector.toSet(Vector#{1, 1}) == Set#{1}

    @Test
    def toSet05(): Bool =
        Vector.toSet(Vector#{1, 2, 3}) == Set#{1, 2, 3}

    @Test
    def toSet06(): Bool =
        Vector.toSet(Vector#{1, 2, 1}) == Set#{1, 2}

    @Test
    def toSet07(): Bool =
        Vector.toSet(Vector#{1, 1, 2}) == Set#{1, 2}

    @Test
    def toSet08(): Bool =
        Vector.toSet(Vector#{2, 1, 2, 3, 4, 5, 6, 3, 3}) == Set#{1, 2, 3, 4, 5, 6}

    /////////////////////////////////////////////////////////////////////////////
    // toMap                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toMap01(): Bool =
        Vector.toMap((Vector#{}): Vector[(Int32, Bool)]) == Map#{}

    @Test
    def toMap02(): Bool =
        Vector.toMap(Vector#{(1, true)}) == Map#{1 => true}

    @Test
    def toMap03(): Bool =
        Vector.toMap(Vector#{(1, true), (2, false)}) == Map#{1 => true, 2 => false}

    @Test
    def toMap04(): Bool =
        Vector.toMap(Vector#{(1, true), (1, false)}) == Map#{1 => true}

    /////////////////////////////////////////////////////////////////////////////
    // findIndexOf                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def findIndexOf01(): Bool =
        Vector.findIndexOf(i -> i > 2, Vector#{}: Vector[Int32]) == None

    @Test
    def findIndexOf02(): Bool =
        Vector.findIndexOf(i -> i > 2, Vector#{1}) == None

    @Test
    def findIndexOf03(): Bool =
        Vector.findIndexOf(i -> i > 2, Vector#{3}) == Some(0)

    @Test
    def findIndexOf04(): Bool =
        Vector.findIndexOf(i -> i > 2, Vector#{1, 2}) == None

    @Test
    def findIndexOf05(): Bool =
        Vector.findIndexOf(i -> i > 2, Vector#{6, -6}) == Some(0)

    @Test
    def findIndexOf06(): Bool =
        Vector.findIndexOf(i -> i > 2, Vector#{-6, 6}) == Some(1)

    @Test
    def findIndexOf07(): Bool =
        Vector.findIndexOf(i -> i > 2, Vector#{6, 7}) == Some(0)

    /////////////////////////////////////////////////////////////////////////////
    // findIndexOfLeft                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def findIndexOfLeft01(): Bool =
        Vector.findIndexOfLeft(i -> i > 2, Vector#{}: Vector[Int32]) == None

    @Test
    def findIndexOfLeft02(): Bool =
        Vector.findIndexOfLeft(i -> i > 2, Vector#{1}) == None

    @Test
    def findIndexOfLeft03(): Bool =
        Vector.findIndexOfLeft(i -> i > 2, Vector#{3}) == Some(0)

    @Test
    def findIndexOfLeft04(): Bool =
        Vector.findIndexOfLeft(i -> i > 2, Vector#{1, 2}) == None

    @Test
    def findIndexOfLeft05(): Bool =
        Vector.findIndexOfLeft(i -> i > 2, Vector#{6, -6}) == Some(0)

    @Test
    def findIndexOfLeft06(): Bool =
        Vector.findIndexOfLeft(i -> i > 2, Vector#{-6, 6}) == Some(1)

    @Test
    def findIndexOfLeft07(): Bool =
        Vector.findIndexOfLeft(i -> i > 2, Vector#{6, 7}) == Some(0)

    /////////////////////////////////////////////////////////////////////////////
    // findIndexOfRight                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def findIndexOfRight01(): Bool =
        Vector.findIndexOfRight(i -> i > 2, Vector#{}: Vector[Int32]) == None

    @Test
    def findIndexOfRight02(): Bool =
        Vector.findIndexOfRight(i -> i > 2, Vector#{1}) == None

    @Test
    def findIndexOfRight03(): Bool =
        Vector.findIndexOfRight(i -> i > 2, Vector#{3}) == Some(0)

    @Test
    def findIndexOfRight04(): Bool =
        Vector.findIndexOfRight(i -> i > 2, Vector#{1, 2}) == None

    @Test
    def findIndexOfRight05(): Bool =
        Vector.findIndexOfRight(i -> i > 2, Vector#{6, -6}) == Some(0)

    @Test
    def findIndexOfRight06(): Bool =
        Vector.findIndexOfRight(i -> i > 2, Vector#{-6, 6}) == Some(1)

    @Test
    def findIndexOfRight07(): Bool =
        Vector.findIndexOfRight(i -> i > 2, Vector#{6, 7}) == Some(1)

    /////////////////////////////////////////////////////////////////////////////
    // findIndices                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def findIndices01(): Bool =
        let v = Vector.findIndices(i -> i > 2, (Vector#{}: Vector[Int32]));
        v == Vector#{}

    @Test
    def findIndices02(): Bool =
        let v = Vector.findIndices(i -> i > 2, Vector#{1});
        v == Vector#{}

    @Test
    def findIndices03(): Bool =
        let v = Vector.findIndices(i -> i > 2, Vector#{3});
        v == Vector#{0}

    @Test
    def findIndices04(): Bool =
        let v = Vector.findIndices(i -> i > 2, Vector#{1, 2});
        v == Vector#{}

    @Test
    def findIndices05(): Bool =
        let v = Vector.findIndices(i -> i > 2, Vector#{6, -6});
        v == Vector#{0}

    @Test
    def findIndices06(): Bool =
        let v = Vector.findIndices(i -> i > 2, Vector#{-6, 6});
        v == Vector#{1}

    @Test
    def findIndices07(): Bool =
        let v = Vector.findIndices(i -> i > 2, Vector#{6, 7});
        v == Vector#{0, 1}

    /////////////////////////////////////////////////////////////////////////////
    // init                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def init01(): Bool =
        let v = Vector.init(x -> x, 0);
        v == Vector#{}: Vector[Int32]

    @Test
    def init02(): Bool =
        let v = Vector.init(x -> x, -1);
        v == Vector#{}

    @Test
    def init03(): Bool =
        let v = Vector.init(x -> x, 1);
        v == Vector#{0}

    @Test
    def init04(): Bool =
        let v = Vector.init(x -> x, 2);
        v == Vector#{0, 1}

    @Test
    def init05(): Bool =
        let v = Vector.init(x -> x, 50);
        Vector.length(v) == 50

    /////////////////////////////////////////////////////////////////////////////
    // equals                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def equals01(): Bool =
        let a = Vector#{}: Vector[Int32];
        let b = Vector#{}: Vector[Int32];
        Vector.equals(a, b) == true

    @Test
    def equals02(): Bool =
        let a = (Vector#{1});
        let b = Vector#{}: Vector[Int32];
        Vector.equals(a, b) == false

    @Test
    def equals03(): Bool =
        let a = Vector#{}: Vector[Int32];
        let b = (Vector#{1});
        Vector.equals(a, b) == false

    @Test
    def equals04(): Bool =
        let a = (Vector#{1});
        let b = (Vector#{1});
        Vector.equals(a, b) == true

    @Test
    def equals05(): Bool =
        let a = (Vector#{1});
        let b = Vector#{}: Vector[Int32];
        Vector.equals(a, b) == false

    @Test
    def equals06(): Bool =
        let a = (Vector#{1, 2});
        let b = (Vector#{1, 2});
        Vector.equals(a, b) == true

    @Test
    def equals07(): Bool =
        let a = (Vector#{1, 2});
        let b = (Vector#{2, 1});
        Vector.equals(a, b) == false

    /////////////////////////////////////////////////////////////////////////////
    // forEach                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forEach01(): Bool = region rc {
        let v = Vector#{}: Vector[Int32];
        let sb = StringBuilder.new(rc);
        let fn = x -> if (x > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        Vector.forEach(fn, v);
        StringBuilder.toString(sb) == ""
    }

    @Test
    def forEach02(): Bool = region rc {
        let v = Vector#{0};
        let sb = StringBuilder.new(rc);
        let fn = x -> if (x > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        Vector.forEach(fn, v);
        StringBuilder.toString(sb) == "F"
    }

    @Test
    def forEach03(): Bool = region rc {
        let v = Vector#{1};
        let sb = StringBuilder.new(rc);
        let fn = x -> if (x > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        Vector.forEach(fn, v);
        StringBuilder.toString(sb) == "T"
    }

    @Test
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

    @Test
    def forEachWithIndex01(): Bool = region rc {
        let v = (Vector#{}): Vector[Float32];
        let sb = StringBuilder.new(rc);
        let fn = (ix, _) -> if (ix > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        Vector.forEachWithIndex(fn, v);
        StringBuilder.toString(sb) == ""
    }

    @Test
    def forEachWithIndex02(): Bool = region rc {
        let v = Vector#{0.0f32};
        let sb = StringBuilder.new(rc);
        let fn = (ix, _) -> if (ix > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        Vector.forEachWithIndex(fn, v);
        StringBuilder.toString(sb) == "F"
    }

    @Test
    def forEachWithIndex03(): Bool = region rc {
        let v = Vector#{0.0f32, 0.1f32};
        let sb = StringBuilder.new(rc);
        let fn = (ix, _) -> if (ix > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        Vector.forEachWithIndex(fn, v);
        StringBuilder.toString(sb) == "FT"
    }

    @Test
    def forEachWithIndex04(): Bool = region rc {
        let v = Vector#{0.0f32, 0.1f32, 0.2f32};
        let sb = StringBuilder.new(rc);
        let fn = (ix, _) -> if (ix > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        Vector.forEachWithIndex(fn, v);
        StringBuilder.toString(sb) == "FTT"
    }

    @Test
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

    @Test
    def updateSequence01(): Bool =
         let v = Vector.updateSequence(0, Vector#{}: Vector[Int32], Vector#{}: Vector[Int32]);
         v == Vector#{}

    @Test
    def updateSequence02(): Bool =
        let v = Vector.updateSequence(0, Vector#{1, 2}, Vector#{});
        v == Vector#{}

    @Test
    def updateSequence03(): Bool =
        let v = Vector.updateSequence(0, Vector#{}, Vector#{1, 2});
        v == Vector#{1, 2}

    @Test
    def updateSequence04(): Bool =
        let v = Vector.updateSequence(-3, Vector#{1, 2, 4}, Vector#{1, 2});
        v == Vector#{1, 2}

    @Test
    def updateSequence05(): Bool =
        let v = Vector.updateSequence(2, Vector#{1, 2, 4}, Vector#{1, 2});
        v == Vector#{1, 2}

    @Test
    def updateSequence06(): Bool =
        let v = Vector.updateSequence(0, Vector#{}, Vector#{1});
        v == Vector#{1}

    @Test
    def updateSequence07(): Bool =
        let v = Vector.updateSequence(1, Vector#{2}, Vector#{1});
        v == Vector#{1}

    @Test
    def updateSequence08(): Bool =
        let v = Vector.updateSequence(0, Vector#{2}, Vector#{1});
        v == Vector#{2}

    @Test
    def updateSequence09(): Bool =
        let v = Vector.updateSequence(0, Vector#{2, 4}, Vector#{1});
        v == Vector#{2}

    @Test
    def updateSequence10(): Bool =
        let v = Vector.updateSequence(-1, Vector#{2, 4}, Vector#{1});
        v == Vector#{4}

    @Test
    def updateSequence11(): Bool =
        let v = Vector.updateSequence(-1, Vector#{3, 4}, Vector#{1, 2});
        v == Vector#{4, 2}

    @Test
    def updateSequence12(): Bool =
        let v = Vector.updateSequence(1, Vector#{3, 4}, Vector#{1, 2});
        v == Vector#{1, 3}

    @Test
    def updateSequence13(): Bool =
        let v = Vector.updateSequence(-2, Vector#{3, 4}, Vector#{1, 2});
        v == Vector#{1, 2}

    @Test
    def updateSequence14(): Bool =
        let v = Vector.updateSequence(2, Vector#{3, 4}, Vector#{1, 2});
        v == Vector#{1, 2}

    @Test
    def updateSequence15(): Bool =
        let v = Vector.updateSequence(1, Vector#{3}, Vector#{1, 2});
        v == Vector#{1, 3}

    @Test
    def updateSequence16(): Bool =
        let v = Vector.updateSequence(0, Vector#{3, 4}, Vector#{1, 2});
        v == Vector#{3, 4}

    @Test
    def updateSequence17(): Bool =
        let v = Vector.updateSequence(0, Vector#{4}, Vector#{1, 2, 3});
        v == Vector#{4, 2, 3}

    @Test
    def updateSequence18(): Bool =
        let v = Vector.updateSequence(1, Vector#{4}, Vector#{1, 2, 3});
        v == Vector#{1, 4, 3}

    @Test
    def updateSequence19(): Bool =
        let v = Vector.updateSequence(2, Vector#{4}, Vector#{1, 2, 3});
        v == Vector#{1, 2, 4}

    @Test
    def updateSequence20(): Bool =
        let v = Vector.updateSequence(0, Vector#{4, 5}, Vector#{1, 2, 3});
        v == Vector#{4, 5, 3}

    @Test
    def updateSequence21(): Bool =
        let v = Vector.updateSequence(1, Vector#{4, 5}, Vector#{1, 2, 3});
        v == Vector#{1, 4, 5}

    @Test
    def updateSequence22(): Bool =
        let v = Vector.updateSequence(-1, Vector#{4, 5, 6}, Vector#{1, 2, 3});
        v == Vector#{5, 6, 3}

    @Test
    def updateSequence23(): Bool =
        let v = Vector.updateSequence(0, Vector#{4, 5, 6}, Vector#{1, 2, 3});
        v == Vector#{4, 5, 6}

    @Test
    def updateSequence24(): Bool =
        let v = Vector.updateSequence(2, Vector#{14, 15, 16, 17}, Vector#{1, 2, 3, 4, 5, 6, 7});
        v == Vector#{1, 2, 14, 15, 16, 17, 7}

    @Test
    def updateSequence25(): Bool =
        let v = Vector.updateSequence(-2, Vector#{14, 15, 16, 17}, Vector#{1, 2, 3, 4, 5, 6, 7});
        v == Vector#{16, 17, 3, 4, 5, 6, 7}

    @Test
    def updateSequence26(): Bool =
        let v = Vector.updateSequence(4, Vector#{14, 15, 16, 17}, Vector#{1, 2, 3, 4, 5, 6, 7});
        v == Vector#{1, 2, 3, 4, 14, 15, 16}

    @Test
    def updateSequence27(): Bool =
        let v = Vector.updateSequence(4, Vector#{14, 15}, Vector#{1, 2, 3, 4, 5, 6, 7});
        v == Vector#{1, 2, 3, 4, 14, 15, 7}

    @Test
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

    @Test
    def sortWith01(): Bool =
        let v = Vector.sortWith(cmp, Vector#{}: Vector[Int32]);
        v == Vector#{}

    @Test
    def sortWith02(): Bool =
        let v = Vector.sortWith(cmp, Vector#{0});
        v == Vector#{0}

    @Test
    def sortWith03(): Bool =
        let v = Vector.sortWith(cmp, Vector#{0, 1});
        v == Vector#{0, 1}

    @Test
    def sortWith04(): Bool =
        let v = Vector.sortWith(cmp, Vector#{1, 0});
        v == Vector#{0, 1}

    @Test
    def sortWith05(): Bool =
        let v = Vector.sortWith(cmp, Vector#{1, 1});
        v == Vector#{1, 1}

    @Test
    def sortWith06(): Bool =
        let v = Vector.sortWith(cmp, Vector#{0, 1, 2, 3, 4, 5});
        v == Vector#{0, 1, 2, 3, 4, 5}

    @Test
    def sortWith07(): Bool =
        let v = Vector.sortWith(cmp, Vector#{5, 4, 3, 2, 1, 0});
        v == Vector#{0, 1, 2, 3, 4, 5}

    @Test
    def sortWith08(): Bool =
        let v = Vector.sortWith(cmp, Vector#{5, 3, 0, 4, 1, 2});
        v == Vector#{0, 1, 2, 3, 4, 5}

    @Test
    def sortWith09(): Bool =
        let v = Vector.sortWith(cmp, Vector#{2, 3, 0, 4, 1, 2});
        v == Vector#{0, 1, 2, 2, 3, 4}

    @Test
    def sortWith10(): Bool =
        let v = Vector.sortWith(flip(cmp), Vector#{0, 1, 2, 3, 4, 5});
        v == Vector#{5, 4, 3, 2, 1, 0}

    @Test
    def sortWith11(): Bool =
        let v = Vector.sortWith(flip(cmp), Vector#{5, 4, 3, 2, 1, 0});
        v == Vector#{5, 4, 3, 2, 1, 0}

    @Test
    def sortWith12(): Bool =
        let v = Vector.sortWith(flip(cmp), Vector#{5, 3, 0, 4, 1, 2});
        v == Vector#{5, 4, 3, 2, 1, 0}

    @Test
    def sortWith13(): Bool =
        let v = Vector.sortWith(flip(cmp), Vector#{2, 3, 0, 4, 1, 2});
        v == Vector#{4, 3, 2, 2, 1, 0}

    /////////////////////////////////////////////////////////////////////////////
    // sort                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    def testSortVsSortWith(a: Vector[Int32]): Bool =
        Vector.sort(a) == Vector.sortWith(cmp, a)


    @Test
    def sort01(): Bool =
        testSortVsSortWith(Vector#{}: Vector[Int32])

    @Test
    def sort02(): Bool =
        testSortVsSortWith(Vector#{0})

    @Test
    def sort03(): Bool =
        testSortVsSortWith(Vector#{0, 1})

    @Test
    def sort04(): Bool =
        testSortVsSortWith(Vector#{1, 0})

    @Test
    def sort05(): Bool =
        testSortVsSortWith(Vector#{1, 1})

    @Test
    def sort06(): Bool =
        testSortVsSortWith(Vector#{0, 1, 2, 3, 4, 5})

    @Test
    def sort07(): Bool =
        testSortVsSortWith(Vector#{5, 4, 3, 2, 1, 0})

    @Test
    def sort08(): Bool =
        testSortVsSortWith(Vector#{5, 3, 0, 4, 1, 2})

    @Test
    def sort09(): Bool =
        testSortVsSortWith(Vector#{2, 3, 0, 4, 1, 2})

    /////////////////////////////////////////////////////////////////////////////
    // sortBy                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    def testSortByVsSort(a: Vector[Int32]): Bool =
        (Vector.sortBy(identity, a) == Vector.sort(a)) and
        (Vector.sortBy(x -> 4 * x + 7, a) == Vector.sort(a)) and
        (Vector.sortBy(x -> -x, a) == Vector.sortWith(flip(cmp), a))

    @Test
    def sortBy01(): Bool =
        testSortByVsSort(Vector#{}: Vector[Int32])

    @Test
    def sortBy02(): Bool =
        testSortByVsSort(Vector#{0})

    @Test
    def sortBy03(): Bool =
        testSortByVsSort(Vector#{0, 1})

    @Test
    def sortBy04(): Bool =
        testSortByVsSort(Vector#{1, 0})

    @Test
    def sortBy05(): Bool =
        testSortByVsSort(Vector#{1, 1})

    @Test
    def sortBy06(): Bool =
        testSortByVsSort(Vector#{0, 1, 2, 3, 4, 5})

    @Test
    def sortBy07(): Bool =
        testSortByVsSort(Vector#{5, 4, 3, 2, 1, 0})

    @Test
    def sortBy08(): Bool =
        testSortByVsSort(Vector#{5, 3, 0, 4, 1, 2})

    @Test
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

    @Test
    def sortBy10(): Bool =
        let geti = r -> {let R.R(x) = r; x.i};
        Vector.sortBy(geti, Vector#{R.R({i = 2, s = "A"}), R.R({i = 1, s = "B"}), R.R({i = 3, s = "C"})})
            == Vector#{R.R({i = 1, s = "B"}), R.R({i = 2, s = "A"}), R.R({i = 3, s = "C"})}

    /////////////////////////////////////////////////////////////////////////////
    // minimumBy                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def minimumBy01(): Bool =
        Vector.minimumBy((x, y) -> x <=> y, Vector#{}: Vector[Int32]) == None

    @Test
    def minimumBy02(): Bool =
        Vector.minimumBy((x, y) -> x <=> y, Vector.range(0, 5)) == Some(0)

    @Test
    def minimumBy03(): Bool =
        Vector.minimumBy((x, y) -> x <=> y, Vector.range(4, 10)) == Some(4)

    @Test
    def minimumBy04(): Bool =
        Vector.minimumBy((x, y) -> x <=> y, Vector.range(10, 20)) == Some(10)

    /////////////////////////////////////////////////////////////////////////////
    // maximumBy                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def maximumBy01(): Bool =
        Vector.maximumBy((x, y) -> x <=> y, Vector#{}: Vector[Int32]) == None

    @Test
    def maximumBy02(): Bool =
        Vector.maximumBy((x, y) -> x <=> y, Vector.range(0, 5)) == Some(4)

    @Test
    def maximumBy03(): Bool =
        Vector.maximumBy((x, y) -> x <=> y, Vector.range(4, 10)) == Some(9)

    @Test
    def maximumBy04(): Bool =
        Vector.maximumBy((x, y) -> x <=> y, Vector.range(10, 20)) == Some(19)

    /////////////////////////////////////////////////////////////////////////////
    // sum                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sum01(): Bool =
        Vector.sum(Vector#{}) == 0

    @Test
    def sum02(): Bool =
        Vector.sum(Vector#{1}) == 1

    @Test
    def sum03(): Bool =
        Vector.sum(Vector#{1, 2, 3}) == 6

    @Test
    def sum04(): Bool =
        Vector.sum(Vector#{1, 2, 3, -3}) == 3

    @Test
    def sum05(): Bool =
        Vector.sum(Vector#{-1, -2, -3, -4}) == -10

    @Test
    def sum06(): Bool =
        Vector.sum(Vector#{10, -10}) == 0

    @Test
    def sum07(): Bool =
        Vector.range(1, 101) |> Vector.sum == 5050

    /////////////////////////////////////////////////////////////////////////////
    // sumWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sumWith01(): Bool =
        Vector.sumWith(x -> x + 1, Vector#{}) == 0

    @Test
    def sumWith02(): Bool =
        Vector.sumWith(x -> x + 1, Vector#{1}) == 2

    @Test
    def sumWith03(): Bool =
        Vector.sumWith(x -> x + 1, Vector#{1, 2, 3}) == 9

    @Test
    def sumWith04(): Bool =
        Vector.sumWith(x -> x + 1, Vector#{1, 2, 3, -3}) == 7

    @Test
    def sumWith05(): Bool =
        Vector.sumWith(x -> x + 1, Vector#{-1, -2, -3, -4}) == -6

    @Test
    def sumWith06(): Bool =
        Vector.sumWith(x -> x + 1, Vector#{10, -10}) == 2


    /////////////////////////////////////////////////////////////////////////////
    // toDelayList                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toDelayList01(): Bool =
        (Vector#{}): Vector[Unit] |> Vector.toDelayList == DelayList.empty()

    @Test
    def toDelayList02(): Bool =
        (Vector.empty(): Vector[String]) |> Vector.toDelayList == DelayList.empty()

    @Test
    def toDelayList03(): Bool =
        Vector.range(-1000, 1000) |> Vector.toDelayList == DelayList.range(-1000, 1000)

    @Test
    def toDelayList04(): Bool =
        Vector.range(-1000, 1000) |> Vector.toDelayList |> DelayList.toVector == Vector.range(-1000, 1000)

    /////////////////////////////////////////////////////////////////////////////
    // toChain                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toChain01(): Bool =
        Vector.toChain((Vector#{}): Vector[Unit]) == Chain.empty(): Chain[Unit]

    @Test
    def toChain02(): Bool =
        Vector.toChain(Vector#{1}) == Chain.singleton(1)

    @Test
    def toChain03(): Bool =
        Vector.toChain(Vector#{1, 2}) == List.toChain(1 :: 2 :: Nil)

    @Test
    def toChain04(): Bool =
        Vector.toChain(Vector#{1, 2, 3}) == List.toChain(1 :: 2 :: 3 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // toNec                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toNec01(): Bool =
        Vector.toNec((Vector#{}): Vector[Unit]) == None

    @Test
    def toNec02(): Bool =
        Vector.toNec(Vector#{1}) == Some(Nec.singleton(1))

    @Test
    def toNec03(): Bool =
        Vector.toNec(Vector#{1, 2}) == List.toNec(1 :: 2 :: Nil)

    @Test
    def toNec04(): Bool =
        Vector.toNec(Vector#{1, 2, 3}) == List.toNec(1 :: 2 :: 3 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // iterator                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def iterator01(): Bool = region rc {
        Vector#{}: Vector[Int32] |> Vector.iterator(rc) |> Iterator.toList == Nil
    }

    @Test
    def iterator02(): Bool = region rc {
        (Vector#{1, 2, 3}) |> Vector.iterator(rc) |> Iterator.toList == 1 :: 2 :: 3 :: Nil
    }

    @Test
    def iterator03(): Bool = region rc {
        (Vector#{1, 2, 3}) |> Vector.iterator(rc) |> Iterator.toVector == Vector#{1, 2, 3}
    }

    @Test
    def iterator04(): Bool = region rc {
        Vector.range(-100, 100) |> Vector.iterator(rc) |> Iterator.toVector == Vector.range(-100, 100)
    }

    /////////////////////////////////////////////////////////////////////////////
    // join                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def join01(): Bool =
        Vector.join(",", Vector#{}: Vector[Int32]) == ""

    @Test
    def join02(): Bool =
        Vector.join(",", Vector#{1}) == "1"

    @Test
    def join03(): Bool =
        Vector.join(",", Vector#{1, 2, 3}) == "1,2,3"

    @Test
    def join04(): Bool =
        Vector.join(",", Vector#{"1", "2", "3"}) == "1,2,3"

    /////////////////////////////////////////////////////////////////////////////
    // joinWith                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def joinWith01(): Bool =
        Vector.joinWith(x -> "${x + 1}", ",", Vector#{}) == ""

    @Test
    def joinWith02(): Bool =
        Vector.joinWith(x -> "${x + 1}", ",", Vector#{1}) == "2"

    @Test
    def joinWith03(): Bool =
        Vector.joinWith(x -> "${x + 1}", ",", Vector#{1, 2, 3}) == "2,3,4"

    @Test
    def joinWith04(): Bool =
        Vector.joinWith(x -> x + x, ",", Vector#{"1", "2", "3"}) == "11,22,33"

    @Test
    def joinWith05(): Bool =
        let v: Vector[Int32] = Vector#{};
        let s = Vector.joinWith(Int32.toString, ",", v);
        s == ""

    @Test
    def joinWith06(): Bool =
        let v = Vector#{1};
        let s = Vector.joinWith(Int32.toString, ",", v);
        s == "1"

    @Test
    def joinWith07(): Bool =
        let v = Vector#{1, 2};
        let s = Vector.joinWith(Int32.toString, ",", v);
        s == "1,2"

    @Test
    def joinWith08(): Bool =
        let v = Vector#{1, 2, 3};
        let s = Vector.joinWith(Int32.toString, ",", v);
        s == "1,2,3"

    @Test
    def joinWith09(): Bool =
        let v = Vector#{1, 2, 3};
        let s = Vector.joinWith(Int32.toString, "", v);
        s == "123"

    @Test
    def joinWith10(): Bool =
        let v = Vector#{1, 2, 3};
        let s = Vector.joinWith(Int32.toString, "..", v);
        s == "1..2..3"


    /////////////////////////////////////////////////////////////////////////////
    // toMutList                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toMutList01(): Bool = region rc {
        MutList.sameElements(Vector.toMutList(rc, Vector#{}: Vector[Int32]), MutList.new(rc))
    }

    @Test
    def toMutList02(): Bool = region rc {
        let l = MutList.new(rc);
        MutList.push!(1, l);
        MutList.sameElements(Vector.toMutList(rc, Vector#{1}), l)
    }

    @Test
    def toMutList03(): Bool = region rc {
        let l = MutList.new(rc);
        MutList.push!(1, l);
        MutList.push!(2, l);
        MutList.push!(3, l);
        MutList.sameElements(Vector.toMutList(rc, Vector#{1, 2, 3}), l)
    }

    @Test
    def toMutList04(): Bool = region rc {
        MutList.sameElements((Vector.range(0, 100) |> Vector.toMutList(rc)), MutList.range(rc, 0, 100))
    }

    @Test
    def toMutList05(): Bool = region rc {
        let l = Vector.toMutList(rc, Vector#{1, 2, 3});
        MutList.push!(4, l);
        Vector.equals(MutList.toVector(l), Vector#{1, 2, 3, 4})
    }

    @Test
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

    @Test
    def shuffle01(): Bool \ NonDet =
        let v = Vector#{}: Vector[Int32];
        let v1 = Vector.shuffle(Random.new(), v);
        Vector.length(v1) == 0 and Vector.toSet(v1) == Set#{}

    @Test
    def shuffle02(): Bool \ NonDet =
        let v = Vector#{0};
        let v1 = Vector.shuffle(Random.new(), v);
        Vector.length(v1) == 1 and Vector.toSet(v1) == Set#{0}

    @Test
    def shuffle03(): Bool \ NonDet =
        let v = Vector#{0, 1, 2, 3};
        let v1 = Vector.shuffle(Random.new(), v);
        Vector.length(v1) == 4 and Vector.toSet(v1) == Set#{0, 1, 2, 3}

    @Test
    def shuffle04(): Bool \ NonDet =
        let v = Vector#{0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
        let v1 = Vector.shuffle(Random.new(), v);
        Vector.length(v1) == 10 and Vector.toSet(v1) == Set#{0, 1, 2, 3, 4, 5, 6, 7, 8, 9}

}

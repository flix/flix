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

namespace TestVector {

    use Vector.sameElements

    def vectorOf2(x: a, y: a): Vector[a] =
        List.toVector(x :: y :: Nil)

    def vectorOf3(w: a, x: a, y: a): Vector[a] =
        List.toVector(w :: x :: y :: Nil)

    def vectorOf4(v: a, w: a, x: a, y: a): Vector[a] =
        List.toVector(v :: w :: x :: y :: Nil)

    def vectorOf5(u: a, v: a, w: a, x: a, y: a): Vector[a] =
        List.toVector(u :: v :: w :: x :: y :: Nil)

    def vectorOf7(s: a, t: a, u: a, v: a, w: a, x: a, y: a): Vector[a] =
        List.toVector(s :: t :: u :: v :: w :: x :: y :: Nil)

    def vectorOf8(r: a, s: a, t: a, u: a, v: a, w: a, x: a, y: a): Vector[a] =
        List.toVector(r :: s :: t :: u :: v :: w :: x :: y :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // toString                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toString01(): Bool =
        Vector.toString(Vector.empty(): Vector[Int32]) == "Vector#{}"

    @test
    def toString02(): Bool =
        Vector.toString(List.toVector(1 :: Nil)) == "Vector#{1}"

    @test
    def toString03(): Bool =
        Vector.toString(List.toVector(1 :: 2 :: 3 :: 4 :: Nil)) == "Vector#{1, 2, 3, 4}"

    @test
    def toString04(): Bool =
        Vector.toString(List.toVector('a' :: 'b' :: 'c' :: 'd' :: Nil)) == "Vector#{a, b, c, d}"

    /////////////////////////////////////////////////////////////////////////////
    // nth                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testNth01(): Bool =
        Vector.nth(0, vectorOf3(1, 2, 3)) == Some(1)

    @test
    def testNth02(): Bool =
        Vector.nth(1, vectorOf3(1, 2, 3)) == Some(2)

    @test
    def testNth03(): Bool =
        Vector.nth(2, vectorOf3(1, 2, 3)) == Some(3)

    @test
    def testNth04(): Bool =
        Vector.nth(3, vectorOf3(1, 2, 3)) == None

    @test
    def testNth05(): Bool =
        Vector.nth(-2, vectorOf3(1, 2, 3)) == None

    /////////////////////////////////////////////////////////////////////////////
    // isEmpty                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testIsEmpty01(): Bool =
        Vector.isEmpty(Vector.empty())

    @test
    def testIsEmpty02(): Bool =
        not Vector.isEmpty(Vector.singleton(1))

    @test
    def testIsEmpty03(): Bool =
        not Vector.isEmpty(vectorOf3(1, 2, 3))

    /////////////////////////////////////////////////////////////////////////////
    // length                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testLength01(): Bool =
        Vector.length(Vector.empty()) == 0

    @test
    def testLength02(): Bool =
        Vector.length(Vector.singleton(1)) == 1

    @test
    def testLength03(): Bool =
        Vector.length(vectorOf3(1, 2, 3)) == 3


    /////////////////////////////////////////////////////////////////////////////
    // slice                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testSlice01(): Bool =
        Vector.slice(start = 0, end = 1, vectorOf3(1, 2, 3)) |> Vector.get(0) == 1

    @test
    def testSlice02(): Bool =
        Vector.slice(start = 1, end = 2, vectorOf3(1, 2, 3)) |> Vector.get(0) == 2

    @test
    def testSlice03(): Bool =
        Vector.slice(start = 2, end = 3, vectorOf3(1, 2, 3)) |> Vector.get(0) == 3

    /////////////////////////////////////////////////////////////////////////////
    // toList                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testToList01(): Bool =
        Vector.toList(Vector.empty(): Vector[Int32]) == Nil

    @test
    def testToList02(): Bool =
        Vector.toList(Vector.singleton(1)) == 1 :: Nil

    @test
    def testToList03(): Bool =
        Vector.toList(vectorOf2(1, 2)) == 1 :: 2 :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // toNel                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toNel01(): Bool =
        Vector.toNel(Vector.empty(): Vector[Unit]) == None

    @test
    def toNel02(): Bool =
        Vector.toNel(Vector.singleton(1)) == Some(Nel.singleton(1))

    @test
    def toNel03(): Bool =
        Vector.toNel(vectorOf2(1, 2)) == List.toNel(1 :: 2 :: Nil)

    @test
    def toNel04(): Bool =
        Vector.toNel(vectorOf3(1, 2, 3)) == List.toNel(1 :: 2 :: 3 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // toMutDeque                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toMutDeque01(): Bool = region rc {
        let v = Vector.empty(): Vector[Int32];
        let d = new MutDeque(rc): MutDeque[Int32, _];
        Vector.toMutDeque(rc, v) `MutDeque.sameElements` d
    }

    @test
    def toMutDeque02(): Bool = region rc {
        let v = Vector.singleton(1);
        let d = new MutDeque(rc);
        MutDeque.pushBack(1, d);
        Vector.toMutDeque(rc, v) `MutDeque.sameElements` d
    }

    @test
    def toMutDeque03(): Bool = region rc {
        let v = vectorOf3(1, 2, 3);
        let d = new MutDeque(rc);
        MutDeque.pushBack(1, d);
        MutDeque.pushBack(2, d);
        MutDeque.pushBack(3, d);
        Vector.toMutDeque(rc, v) `MutDeque.sameElements` d
    }

    @test
    def toMutDeque04(): Bool = region rc {
        let v = vectorOf5(1, 2, 3, 4, 5);
        let d = new MutDeque(rc);
        MutDeque.pushBack(3, d);
        MutDeque.pushBack(4, d);
        MutDeque.pushBack(5, d);
        MutDeque.pushFront(2, d);
        MutDeque.pushFront(1, d);
        Vector.toMutDeque(rc, v) `MutDeque.sameElements` d
    }

    @test
    def toMutDeque05(): Bool = region rc {
        let v = vectorOf3('a', 'b', 'c');
        let d = new MutDeque(rc);
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
        Vector.head(Vector.empty(): Vector[Int32]) == None

    @test
    def head02(): Bool =
        Vector.head(Vector.singleton(1)) == Some(1)

    @test
    def head03(): Bool =
        Vector.head(vectorOf2(2, 1)) == Some(2)

    @test
    def head04(): Bool =
        Vector.head(vectorOf3(3, 2, 1)) == Some(3)

    /////////////////////////////////////////////////////////////////////////////
    // last                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def last01(): Bool =
        Vector.last(Vector.empty()): Option[Unit] == None

    @test
    def last02(): Bool =
        Vector.last(Vector.singleton(1)) == Some(1)

    @test
    def last03(): Bool =
        Vector.last(vectorOf2(1, 2)) == Some(2)

    @test
    def last04(): Bool =
        Vector.last(vectorOf3(1, 2, 3)) == Some(3)

    /////////////////////////////////////////////////////////////////////////////
    // append                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def append01(): Bool =
        let v = Vector.append(Vector.empty(): Vector[Int32], Vector.empty(): Vector[Int32]);
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def append02(): Bool =
        let v = Vector.append(Vector.empty(): Vector[Int32], Vector.singleton(1));
        Vector.sameElements(v, Vector.singleton(1))

    @test
    def append03(): Bool =
        let v = Vector.append(Vector.empty(): Vector[Int32], vectorOf2(1, 2));
        Vector.sameElements(v, vectorOf2(1, 2))

    @test
    def append04(): Bool =
        let v = Vector.append(Vector.singleton(1), Vector.empty(): Vector[Int32]);
        Vector.sameElements(v, Vector.singleton(1))

    @test
    def append05(): Bool =
        let v = Vector.append(vectorOf2(1, 2), Vector.empty(): Vector[Int32]);
        Vector.sameElements(v, vectorOf2(1, 2))

    @test
    def append06(): Bool =
        let v = Vector.append(Vector.singleton(1), Vector.singleton(2));
        Vector.sameElements(v, vectorOf2(1, 2))

    @test
    def append07(): Bool =
        let v = Vector.append(vectorOf2(1, 2), Vector.singleton(3));
        Vector.sameElements(v, vectorOf3(1, 2, 3))

    @test
    def append08(): Bool =
        let v = Vector.append(Vector.singleton(1), vectorOf2(2, 3));
        Vector.sameElements(v, vectorOf3(1, 2, 3))

    /////////////////////////////////////////////////////////////////////////////
    // memberOf                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def memberOf01(): Bool =
        Vector.memberOf(0, Vector.empty(): Vector[Int32]) == false

    @test
    def memberOf02(): Bool =
        Vector.memberOf(0, Vector.singleton(1)) == false

    @test
    def memberOf03(): Bool =
        Vector.memberOf(1, Vector.singleton(1)) == true

    @test
    def memberOf04(): Bool =
        Vector.memberOf(0, vectorOf2(1, 2)) == false

    @test
    def memberOf05(): Bool =
        Vector.memberOf(1, vectorOf2(1, 2)) == true

    @test
    def memberOf06(): Bool =
        Vector.memberOf(2, vectorOf2(1, 2)) == true

    @test
    def memberOf07(): Bool =
        Vector.memberOf(3, vectorOf2(1, 2)) == false

    /////////////////////////////////////////////////////////////////////////////
    // indexOf                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def indexOf01(): Bool =
        Vector.indexOf(0, Vector.empty(): Vector[Int32]) == None

    @test
    def indexOf02(): Bool =
        Vector.indexOf(0, Vector.singleton(1)) == None

    @test
    def indexOf03(): Bool =
        Vector.indexOf(1, Vector.singleton(1)) == Some(0)

    @test
    def indexOf04(): Bool =
        Vector.indexOf(0, vectorOf2(1, 2)) == None

    @test
    def indexOf05(): Bool =
        Vector.indexOf(1, vectorOf2(1, 2)) == Some(0)

    @test
    def indexOf06(): Bool =
        Vector.indexOf(2, vectorOf2(1, 2)) == Some(1)

    /////////////////////////////////////////////////////////////////////////////
    // indexOfLeft                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def indexOfLeft01(): Bool =
        Vector.indexOfLeft(0, Vector.empty(): Vector[Int32]) == None

    @test
    def indexOfLeft02(): Bool =
        Vector.indexOfLeft(0, Vector.singleton(1)) == None

    @test
    def indexOfLeft03(): Bool =
        Vector.indexOfLeft(1, Vector.singleton(1)) == Some(0)

    @test
    def indexOfLeft04(): Bool =
        Vector.indexOfLeft(0, vectorOf2(1, 2)) == None

    @test
    def indexOfLeft05(): Bool =
        Vector.indexOfLeft(1, vectorOf2(1, 2)) == Some(0)

    @test
    def indexOfLeft06(): Bool =
        Vector.indexOfLeft(2, vectorOf2(1, 2)) == Some(1)

    @test
    def indexOfLeft07(): Bool =
        Vector.indexOfLeft(1, vectorOf2(1, 1)) == Some(0)

    /////////////////////////////////////////////////////////////////////////////
    // indexOfRight                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def indexOfRight01(): Bool =
        Vector.indexOfRight(0, Vector.empty(): Vector[Int32]) == None

    @test
    def indexOfRight02(): Bool =
        Vector.indexOfRight(0, Vector.singleton(1)) == None

    @test
    def indexOfRight03(): Bool =
        Vector.indexOfRight(1, Vector.singleton(1)) == Some(0)

    @test
    def indexOfRight04(): Bool =
        Vector.indexOfRight(0, vectorOf2(1, 2)) == None

    @test
    def indexOfRight05(): Bool =
        Vector.indexOfRight(1, vectorOf2(1, 2)) == Some(0)

    @test
    def indexOfRight06(): Bool =
        Vector.indexOfRight(2, vectorOf2(1, 2)) == Some(1)

    @test
    def indexOfRight07(): Bool =
        Vector.indexOfRight(1, vectorOf2(1, 1)) == Some(1)

    /////////////////////////////////////////////////////////////////////////////
    // indices                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def indices01(): Bool =
        let v = Vector.indices(0, Vector.empty(): Vector[Int32]);
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def indices02(): Bool =
        let v = Vector.indices(0, Vector.singleton(1));
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def indices03(): Bool =
        let v = Vector.indices(1, Vector.singleton(1));
        Vector.sameElements(v, Vector.singleton(0): Vector[Int32])

    @test
    def indices04(): Bool =
        let v = Vector.indices(0, vectorOf2(1, 2));
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def indices05(): Bool =
        let v = Vector.indices(1, vectorOf2(1, 2));
        Vector.sameElements(v, Vector.singleton(0): Vector[Int32])

    @test
    def indices06(): Bool =
        let v = Vector.indices(2, vectorOf2(1, 2));
        Vector.sameElements(v, Vector.singleton(1))

    @test
    def indices07(): Bool =
        let v = Vector.indices(1, vectorOf2(1, 1));
        Vector.sameElements(v, vectorOf2(0, 1))

    /////////////////////////////////////////////////////////////////////////////
    // find                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def find01(): Bool =
        Vector.find(i -> i > 2, Vector.empty()) == None

    @test
    def find02(): Bool =
        Vector.find(i -> i > 2, Vector.singleton(1)) == None

    @test
    def find03(): Bool =
        Vector.find(i -> i > 2, Vector.singleton(3)) == Some(3)

    @test
    def find04(): Bool =
        Vector.find(i -> i > 2, vectorOf2(1, 2)) == None

    @test
    def find05(): Bool =
        Vector.find(i -> i > 2, vectorOf2(6, -6)) == Some(6)

    @test
    def find06(): Bool =
        Vector.find(i -> i > 2, vectorOf2(-6, 6)) == Some(6)

    @test
    def find07(): Bool =
        Vector.find(i -> i > 2, vectorOf2(6, 7)) == Some(6)

    /////////////////////////////////////////////////////////////////////////////
    // findLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def findLeft01(): Bool =
        Vector.findLeft(i -> i > 2, Vector.empty()) == None

    @test
    def findLeft02(): Bool =
        Vector.findLeft(i -> i > 2, Vector.singleton(1)) == None

    @test
    def findLeft03(): Bool =
        Vector.findLeft(i -> i > 2, Vector.singleton(3)) == Some(3)

    @test
    def findLeft04(): Bool =
        Vector.findLeft(i -> i > 2, vectorOf2(1, 2)) == None

    @test
    def findLeft05(): Bool =
        Vector.findLeft(i -> i > 2, vectorOf2(6, -6)) == Some(6)

    @test
    def findLeft06(): Bool =
        Vector.findLeft(i -> i > 2, vectorOf2(-6, 6)) == Some(6)

    @test
    def findLeft07(): Bool =
        Vector.findLeft(i -> i > 2, vectorOf2(6, 7)) == Some(6)

    /////////////////////////////////////////////////////////////////////////////
    // findRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def findRight01(): Bool =
        Vector.findRight(i -> i > 2, Vector.empty()) == None

    @test
    def findRight02(): Bool =
        Vector.findRight(i -> i > 2, Vector.singleton(1)) == None

    @test
    def findRight03(): Bool =
        Vector.findRight(i -> i > 2, Vector.singleton(3)) == Some(3)

    @test
    def findRight04(): Bool =
        Vector.findRight(i -> i > 2, vectorOf2(1, 2)) == None

    @test
    def findRight05(): Bool =
        Vector.findRight(i -> i > 2, vectorOf2(6, -6)) == Some(6)

    @test
    def findRight06(): Bool =
        Vector.findRight(i -> i > 2, vectorOf2(-6, 6)) == Some(6)

    @test
    def findRight07(): Bool =
        Vector.findRight(i -> i > 2, vectorOf2(6, 7)) == Some(7)

    /////////////////////////////////////////////////////////////////////////////
    // range                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def range01(): Bool =
        let v = Vector.range(1, 0);
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def range02(): Bool =
        let v = Vector.range(1, 1);
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def range03(): Bool =
        let v = Vector.range(1, 2);
        Vector.sameElements(v, Vector.singleton(1))

    @test
    def range04(): Bool =
        let v = Vector.range(1, 3);
        Vector.sameElements(v, vectorOf2(1, 2))

    @test
    def range05(): Bool =
        let v = Vector.range(1, 4);
        Vector.sameElements(v, vectorOf3(1, 2, 3))

    @test
    def range06(): Bool =
        let v = Vector.range(-1, 3);
        Vector.sameElements(v, vectorOf4(-1, 0, 1, 2))

    /////////////////////////////////////////////////////////////////////////////
    // repeat                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def repeat01(): Bool =
        let v = Vector.repeat(-1, 1);
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def repeat02(): Bool =
        let v = Vector.repeat(0, 1);
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def repeat03(): Bool =
        let v = Vector.repeat(1, 1);
        Vector.sameElements(v, Vector.singleton(1))

    @test
    def repeat04(): Bool =
        let v = Vector.repeat(2, 1);
        Vector.sameElements(v, vectorOf2(1,1))

    @test
    def repeat05(): Bool =
        let v = Vector.repeat(3, 1);
        Vector.sameElements(v, vectorOf3(1, 1, 1))

    /////////////////////////////////////////////////////////////////////////////
    // scan                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def scan01(): Bool =
        let v = Vector.scan((i, b) -> if (b) i + 1 else i + 2, 1, Vector.empty());
        Vector.sameElements(v, Vector.singleton(1))

    @test
    def scan02(): Bool =
        let v = Vector.scan((i, b) -> if (b) i + 1 else i + 2, 1, Vector.singleton(false));
        Vector.sameElements(v, vectorOf2(1, 3))

    @test
    def scan03(): Bool =
        let v = Vector.scan((i, b) -> if (b) i + 1 else i + 2, 1, Vector.singleton(true));
        Vector.sameElements(v, vectorOf2(1, 2))

    @test
    def scan04(): Bool =
        let v = Vector.scan((i, b) -> if (b) i + 1 else i + 2, 1, vectorOf2(false, false));
        Vector.sameElements(v, vectorOf3(1, 3, 5))

    @test
    def scan05(): Bool =
        let v = Vector.scan((i, b) -> if (b) i + 1 else i + 2, 1, vectorOf2(false, true));
        Vector.sameElements(v, vectorOf3(1, 3, 4))

    @test
    def scan06(): Bool =
        let v = Vector.scan((i, b) -> if (b) i + 1 else i + 2, 1, vectorOf2(true, false));
        Vector.sameElements(v, vectorOf3(1, 2, 4))

    @test
    def scan07(): Bool =
        let v = Vector.scan((i, b) -> if (b) i + 1 else i + 2, 1, vectorOf2(true, true));
        Vector.sameElements(v, vectorOf3(1, 2, 3))

    /////////////////////////////////////////////////////////////////////////////
    // scanLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def scanLeft01(): Bool =
        let v = Vector.scanLeft((i, b) -> if (b) i + 1 else i + 2, 1, Vector.empty(): Vector[Bool]);
        Vector.sameElements(v, Vector.singleton(1))

    @test
    def scanLeft02(): Bool =
        let v = Vector.scanLeft((i, b) -> if (b) i + 1 else i + 2, 1, Vector.singleton(false));
        Vector.sameElements(v, vectorOf2(1, 3))

    @test
    def scanLeft03(): Bool =
        let v = Vector.scanLeft((i, b) -> if (b) i + 1 else i + 2, 1, Vector.singleton(true));
        Vector.sameElements(v, vectorOf2(1, 2))

    @test
    def scanLeft04(): Bool =
        let v = Vector.scanLeft((i, b) -> if (b) i + 1 else i + 2, 1, vectorOf2(false, false));
        Vector.sameElements(v, vectorOf3(1, 3, 5))

    @test
    def scanLeft05(): Bool =
        let v = Vector.scanLeft((i, b) -> if (b) i + 1 else i + 2, 1, vectorOf2(false, true));
        Vector.sameElements(v, vectorOf3(1, 3, 4))

    @test
    def scanLeft06(): Bool =
        let v = Vector.scanLeft((i, b) -> if (b) i + 1 else i + 2, 1, vectorOf2(true, false));
        Vector.sameElements(v, vectorOf3(1, 2, 4))

    @test
    def scanLeft07(): Bool =
        let v = Vector.scanLeft((i, b) -> if (b) i + 1 else i + 2, 1, vectorOf2(true, true));
        Vector.sameElements(v, vectorOf3(1, 2, 3))

    /////////////////////////////////////////////////////////////////////////////
    // scanRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def scanRight01(): Bool =
        let v = Vector.scanRight((b, i) -> if (b) i + 1 else i + 2, 1, Vector.empty());
        Vector.sameElements(v, Vector.singleton(1))

    @test
    def scanRight02(): Bool =
        let v = Vector.scanRight((b, i) -> if (b) i + 1 else i + 2, 1, Vector.singleton(false));
        Vector.sameElements(v, vectorOf2(3, 1))

    @test
    def scanRight03(): Bool =
        let v = Vector.scanRight((b, i) -> if (b) i + 1 else i + 2, 1, Vector.singleton(true));
        Vector.sameElements(v, vectorOf2(2, 1))

    @test
    def scanRight04(): Bool =
        let v = Vector.scanRight((b, i) -> if (b) i + 1 else i + 2, 1, vectorOf2(false, false));
        Vector.sameElements(v, vectorOf3(5, 3, 1))

    @test
    def scanRight05(): Bool =
        let v = Vector.scanRight((b, i) -> if (b) i + 1 else i + 2, 1, vectorOf2(false, true));
        Vector.sameElements(v, vectorOf3(4, 2, 1))

    @test
    def scanRight06(): Bool =
        let v = Vector.scanRight((b, i) -> if (b) i + 1 else i + 2, 1, vectorOf2(true, false));
        Vector.sameElements(v, vectorOf3(4, 3, 1))

    @test
    def scanRight07(): Bool =
        let v = Vector.scanRight((b, i) -> if (b) i + 1 else i + 2, 1, vectorOf2(true, true));
        Vector.sameElements(v, vectorOf3(3, 2, 1))

    /////////////////////////////////////////////////////////////////////////////
    // map                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def map01(): Bool =
        let v = Vector.map(i -> i > 2, Vector.empty());
        Vector.sameElements(v, Vector.empty(): Vector[Bool])

    @test
    def map02(): Bool =
        let v = Vector.map(i -> i > 2, Vector.singleton(1));
        Vector.sameElements(v, Vector.singleton(false))

    @test
    def map03(): Bool =
        let v = Vector.map(i -> i > 2, Vector.singleton(3));
        Vector.sameElements(v, Vector.singleton(true))

    @test
    def map04(): Bool =
        let v = Vector.map(i -> i > 2, vectorOf2(1, 2));
        Vector.sameElements(v, vectorOf2(false, false))

    @test
    def map05(): Bool =
        let v = Vector.map(i -> i > 2, vectorOf2(1, 8));
        Vector.sameElements(v, vectorOf2(false, true))

    @test
    def map06(): Bool =
        let v = Vector.map(i -> i > 2, vectorOf2(8, 1));
        Vector.sameElements(v, vectorOf2(true, false))

    @test
    def map07(): Bool =
        let v = Vector.map(i -> i > 2, vectorOf2(7, 8));
        Vector.sameElements(v, vectorOf2(true, true))

    /////////////////////////////////////////////////////////////////////////////
    // mapWithIndex                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapWithIndex01(): Bool =
        let v = Vector.mapWithIndex((i, e) -> if (i < 1) e > 2 else e <= 2, Vector.empty());
        Vector.sameElements(v, Vector.empty(): Vector[Bool])

    @test
    def mapWithIndex02(): Bool =
        let v = Vector.mapWithIndex((i, e) -> if (i < 1) e > 2 else e <= 2, Vector.singleton(1));
        Vector.sameElements(v, Vector.singleton(false))

    @test
    def mapWithIndex03(): Bool =
        let v = Vector.mapWithIndex((i, e) -> if (i < 1) e > 2 else e <= 2, Vector.singleton(3));
        Vector.sameElements(v, Vector.singleton(true))

    @test
    def mapWithIndex04(): Bool =
        let v = Vector.mapWithIndex((i, e) -> if (i < 1) e > 2 else e <= 2, vectorOf2(1, 2));
        Vector.sameElements(v, vectorOf2(false, true))

    @test
    def mapWithIndex05(): Bool =
        let v = Vector.mapWithIndex((i, e) -> if (i < 1) e > 2 else e <= 2, vectorOf2(1, 8));
        Vector.sameElements(v, vectorOf2(false, false))

    @test
    def mapWithIndex06(): Bool =
        let v = Vector.mapWithIndex((i, e) -> if (i < 1) e > 2 else e <= 2, vectorOf2(8, 1));
        Vector.sameElements(v, vectorOf2(true, true))

    @test
    def mapWithIndex07(): Bool =
        let v = Vector.mapWithIndex((i, e) -> if (i < 1) e > 2 else e <= 2, vectorOf2(7, 8));
        Vector.sameElements(v, vectorOf2(true, false))

    /////////////////////////////////////////////////////////////////////////////
    // flatMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    // depends on flatten

    @test
    def flatMap01(): Bool =
        let v = Vector.flatMap(i -> Vector.repeat(i, i), Vector.empty());
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def flatMap02(): Bool =
        let v = Vector.flatMap(i -> Vector.repeat(i, i), Vector.empty());
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def flatMap03(): Bool =
        let v = Vector.flatMap(i -> Vector.repeat(i, i), Vector.singleton(1));
        Vector.sameElements(v, Vector.singleton(1))

    @test
    def flatMap04(): Bool =
        let v = Vector.flatMap(i -> Vector.repeat(i, i), Vector.singleton(2));
        Vector.sameElements(v, vectorOf2(2, 2))

    @test
    def flatMap05(): Bool =
        let v = Vector.flatMap(i -> Vector.repeat(i, i), vectorOf2(1, 2));
        Vector.sameElements(v, vectorOf3(1, 2, 2))

    @test
    def flatMap06(): Bool =
        let v = Vector.flatMap(i -> Vector.repeat(i, i), vectorOf2(2, 3));
        Vector.sameElements(v, vectorOf5(2, 2, 3, 3, 3))

    /////////////////////////////////////////////////////////////////////////////
    // reverse                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reverse01(): Bool =
        let v = Vector.reverse(Vector.empty(): Vector[Int32]);
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def reverse02(): Bool =
        let v = Vector.reverse(Vector.singleton(1));
        Vector.sameElements(v, Vector.singleton(1))

    @test
    def reverse03(): Bool =
        let v = Vector.reverse(vectorOf2(1, 2));
        Vector.sameElements(v, vectorOf2(2, 1))

    @test
    def reverse04(): Bool =
        let v = Vector.reverse(vectorOf2(1,1));
        Vector.sameElements(v, vectorOf2(1,1))

    @test
    def reverse05(): Bool =
        let v = Vector.reverse(vectorOf3(1, 2, 3));
        Vector.sameElements(v, vectorOf3(3, 2, 1))

    @test
    def reverse06(): Bool =
        let v = Vector.reverse(vectorOf4(1, 2, 3, 4));
        Vector.sameElements(v, vectorOf4(4, 3, 2, 1))

    /////////////////////////////////////////////////////////////////////////////
    // rotateLeft                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def rotateLeft01(): Bool =
        let v = Vector.rotateLeft(0, Vector.empty(): Vector[Int32]);
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def rotateLeft02(): Bool =
        let v = Vector.rotateLeft(1, Vector.empty(): Vector[Int32]);
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def rotateLeft03(): Bool =
        let v = Vector.rotateLeft(0, Vector.singleton(1));
        Vector.sameElements(v, Vector.singleton(1))

    @test
    def rotateLeft04(): Bool =
        let v = Vector.rotateLeft(0, vectorOf2(1, 2));
        Vector.sameElements(v, vectorOf2(1, 2))

    @test
    def rotateLeft05(): Bool =
        let v = Vector.rotateLeft(1, vectorOf2(1, 2));
        Vector.sameElements(v, vectorOf2(2, 1))

    @test
    def rotateLeft06(): Bool =
        let v = Vector.rotateLeft(2, vectorOf2(1, 2));
        Vector.sameElements(v, vectorOf2(1, 2))

    @test
    def rotateLeft07(): Bool =
        let v = Vector.rotateLeft(3, vectorOf2(1, 2));
        Vector.sameElements(v, vectorOf2(2, 1))

    @test
    def rotateLeft08(): Bool =
        let v = Vector.rotateLeft(-1, vectorOf2(1, 2));
        Vector.sameElements(v, vectorOf2(2, 1))

    @test
    def rotateLeft09(): Bool =
        let v = Vector.rotateLeft(0, vectorOf3(1, 2, 3));
        Vector.sameElements(v, vectorOf3(1, 2, 3))

    @test
    def rotateLeft10(): Bool =
        let v = Vector.rotateLeft(1, vectorOf3(1, 2, 3));
        Vector.sameElements(v, vectorOf3(2, 3, 1))

    @test
    def rotateLeft11(): Bool =
        let v = Vector.rotateLeft(2, vectorOf3(1, 2, 3));
        Vector.sameElements(v, vectorOf3(3, 1, 2))

    /////////////////////////////////////////////////////////////////////////////
    // rotateRight                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def rotateRight01(): Bool =
        let v = Vector.rotateRight(0, Vector.empty(): Vector[Int32]);
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def rotateRight02(): Bool =
        let v = Vector.rotateRight(1, Vector.empty(): Vector[Int32]);
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def rotateRight03(): Bool =
        let v = Vector.rotateRight(0, Vector.singleton(1));
        Vector.sameElements(v, Vector.singleton(1))

    @test
    def rotateRight04(): Bool =
        let v = Vector.rotateRight(0, vectorOf2(1, 2));
        Vector.sameElements(v, vectorOf2(1, 2))

    @test
    def rotateRight05(): Bool =
        let v = Vector.rotateRight(1, vectorOf2(1, 2));
        Vector.sameElements(v, vectorOf2(2, 1))

    @test
    def rotateRight06(): Bool =
        let v = Vector.rotateRight(2, vectorOf2(1, 2));
        Vector.sameElements(v, vectorOf2(1, 2))

    @test
    def rotateRight07(): Bool =
        let v = Vector.rotateRight(3, vectorOf2(1, 2));
        Vector.sameElements(v, vectorOf2(2, 1))

    @test
    def rotateRight08(): Bool =
        let v = Vector.rotateRight(-1, vectorOf2(1, 2));
        Vector.sameElements(v, vectorOf2(2, 1))

    @test
    def rotateRight09(): Bool =
        let v = Vector.rotateRight(0, vectorOf3(1, 2, 3));
        Vector.sameElements(v, vectorOf3(1, 2, 3))

    @test
    def rotateRight10(): Bool =
        let v = Vector.rotateRight(1, vectorOf3(1, 2, 3));
        Vector.sameElements(v, vectorOf3(3, 1, 2))

    @test
    def rotateRight11(): Bool =
        let v = Vector.rotateRight(2, vectorOf3(1, 2, 3));
        Vector.sameElements(v, vectorOf3(2, 3, 1))

    /////////////////////////////////////////////////////////////////////////////
    // update                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def update01(): Bool =
        let v = Vector.update(0, 2, Vector.empty(): Vector[Int32]);
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def update02(): Bool =
        let v = Vector.update(-1, 2, Vector.singleton(1));
        Vector.sameElements(v, Vector.singleton(1))

    @test
    def update03(): Bool =
        let v = Vector.update(0, 2, Vector.singleton(1));
        Vector.sameElements(v, Vector.singleton(2))

    @test
    def update04(): Bool =
        let v = Vector.update(1, 2, Vector.singleton(1));
        Vector.sameElements(v, Vector.singleton(1))

    @test
    def update05(): Bool =
        let v = Vector.update(0, 5, vectorOf2(1, 2));
        Vector.sameElements(v, vectorOf2(5, 2))

    @test
    def update06(): Bool =
        let v = Vector.update(1, 5, vectorOf2(1, 2));
        Vector.sameElements(v, vectorOf2(1, 5))

    @test
    def update07(): Bool =
        let v = Vector.update(2, 5, vectorOf2(1, 2));
        Vector.sameElements(v, vectorOf2(1, 2))

    /////////////////////////////////////////////////////////////////////////////
    // replace                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def replace01(): Bool =
        let v = Vector.replace(from = 3, to = 4, Vector.empty());
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def replace02(): Bool =
        let v = Vector.replace(from = 3, to = 4, Vector.singleton(1));
        Vector.sameElements(v, Vector.singleton(1))

    @test
    def replace03(): Bool =
        let v = Vector.replace(from = 3, to = 4, Vector.singleton(3));
        Vector.sameElements(v, Vector.singleton(4))

    @test
    def replace04(): Bool =
        let v = Vector.replace(from = 3, to = 4, Vector.singleton(4));
        Vector.sameElements(v, Vector.singleton(4))

    @test
    def replace05(): Bool =
        let v = Vector.replace(from = 3, to = 4, vectorOf2(1, 2));
        Vector.sameElements(v, vectorOf2(1, 2))

    @test
    def replace06(): Bool =
        let v = Vector.replace(from = 3, to = 4, vectorOf2(1, 3));
        Vector.sameElements(v, vectorOf2(1, 4))

    @test
    def replace07(): Bool =
        let v = Vector.replace(from = 3, to = 4, vectorOf2(3, 4));
        Vector.sameElements(v, vectorOf2(4, 4))

    @test
    def replace08(): Bool =
        let v = Vector.replace(from = 3, to = 4, vectorOf2(3, 3));
        Vector.sameElements(v, vectorOf2(4, 4))

    /////////////////////////////////////////////////////////////////////////////
    // patch                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def patch01(): Bool =
        let v = Vector.patch(0, 0, Vector.empty(): Vector[Int32], Vector.empty(): Vector[Int32]);
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def patch02(): Bool =
        let v = Vector.patch(0, 2, vectorOf2(1, 2), Vector.empty());
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def patch03(): Bool =
        let v = Vector.patch(0, 2, Vector.empty(), vectorOf2(1, 2));
        Vector.sameElements(v, vectorOf2(1, 2))

    @test
    def patch04(): Bool =
        let v = Vector.patch(-3, 3, vectorOf3(1, 2, 4), vectorOf2(1, 2));
        Vector.sameElements(v, vectorOf2(1, 2))

    @test
    def patch05(): Bool =
        let v = Vector.patch(2, 3, vectorOf3(1, 2, 4), vectorOf2(1, 2));
        Vector.sameElements(v, vectorOf2(1, 2))

    @test
    def patch06(): Bool =
        let v = Vector.patch(0, 0, Vector.empty(), Vector.singleton(1));
        Vector.sameElements(v, Vector.singleton(1))

    @test
    def patch07(): Bool =
        let v = Vector.patch(1, 0, Vector.singleton(2), Vector.singleton(1));
        Vector.sameElements(v, Vector.singleton(1))

    @test
    def patch08(): Bool =
        let v = Vector.patch(0, 1, Vector.singleton(2), Vector.singleton(1));
        Vector.sameElements(v, Vector.singleton(2))

    @test
    def patch09(): Bool =
        let v = Vector.patch(0, 2, vectorOf2(2, 4), Vector.singleton(1));
        Vector.sameElements(v, Vector.singleton(2))

    @test
    def patch10(): Bool =
        let v = Vector.patch(-1, 2, vectorOf2(2, 4), Vector.singleton(1));
        Vector.sameElements(v, Vector.singleton(4))

    @test
    def patch11(): Bool =
        let v = Vector.patch(-1, 2, vectorOf2(3, 4), vectorOf2(1, 2));
        Vector.sameElements(v, vectorOf2(4, 2))

    @test
    def patch12(): Bool =
        let v = Vector.patch(1, 2, vectorOf2(3, 4), vectorOf2(1, 2));
        Vector.sameElements(v, vectorOf2(1, 3))

    @test
    def patch13(): Bool =
        let v = Vector.patch(-2, 2, vectorOf2(3, 4), vectorOf2(1, 2));
        Vector.sameElements(v, vectorOf2(1, 2))

    @test
    def patch14(): Bool =
        let v = Vector.patch(2, 2, vectorOf2(3, 4), vectorOf2(1, 2));
        Vector.sameElements(v, vectorOf2(1, 2))

    @test
    def patch15(): Bool =
        let v = Vector.patch(1, 1, Vector.singleton(3), vectorOf2(1, 2));
        Vector.sameElements(v, vectorOf2(1, 3))

    @test
    def patch16(): Bool =
        let v = Vector.patch(0, 2, vectorOf2(3, 4), vectorOf2(1, 2));
        Vector.sameElements(v, vectorOf2(3, 4))

    @test
    def patch17(): Bool =
        let v = Vector.patch(0, 1, Vector.singleton(4), vectorOf3(1, 2, 3));
        Vector.sameElements(v, vectorOf3(4, 2, 3))

    @test
    def patch18(): Bool =
        let v = Vector.patch(1, 1, Vector.singleton(4), vectorOf3(1, 2, 3));
        Vector.sameElements(v, vectorOf3(1, 4, 3))

    @test
    def patch19(): Bool =
        let v = Vector.patch(2, 1, Vector.singleton(4), vectorOf3(1, 2, 3));
        Vector.sameElements(v, vectorOf3(1, 2, 4))

    @test
    def patch20(): Bool =
        let v = Vector.patch(0, 2, vectorOf2(4, 5), vectorOf3(1, 2, 3));
        Vector.sameElements(v, vectorOf3(4, 5, 3))

    @test
    def patch21(): Bool =
        let v = Vector.patch(1, 2, vectorOf2(4, 5), vectorOf3(1, 2, 3));
        Vector.sameElements(v, vectorOf3(1, 4, 5))

    @test
    def patch22(): Bool =
        let v = Vector.patch(0, 2, vectorOf3(4, 5, 6), vectorOf3(1, 2, 3));
        Vector.sameElements(v, vectorOf3(4, 5, 3))

    @test
    def patch23(): Bool =
        let v = Vector.patch(0, 3, vectorOf3(4, 5, 6), vectorOf3(1, 2, 3));
        Vector.sameElements(v, vectorOf3(4, 5, 6))

    @test
    def patch24(): Bool =
        let v = Vector.patch(2, 4, vectorOf4(14, 15, 16, 17), vectorOf7(1, 2, 3, 4, 5, 6, 7));
        Vector.sameElements(v, vectorOf7(1, 2, 14, 15, 16, 17, 7))

    @test
    def patch25(): Bool =
        let v = Vector.patch(-2, 4, vectorOf4(14, 15, 16, 17), vectorOf7(1, 2, 3, 4, 5, 6, 7));
        Vector.sameElements(v, vectorOf7(16, 17, 3, 4, 5, 6, 7))

    @test
    def patch26(): Bool =
        let v = Vector.patch(4, 5, vectorOf4(14, 15, 16, 17), vectorOf7(1, 2, 3, 4, 5, 6, 7));
        Vector.sameElements(v, vectorOf7(1, 2, 3, 4, 14, 15, 16))

    @test
    def patch27(): Bool =
        let v = Vector.patch(4, 2, vectorOf4(14, 15, 16, 17), vectorOf7(1, 2, 3, 4, 5, 6, 7));
        Vector.sameElements(v, vectorOf7(1, 2, 3, 4, 14, 15, 7))

    @test
    def patch28(): Bool =
        let v = Vector.patch(-1, 10, vectorOf8(-1, -2, -3, -4, -5, -6, -7, -8), vectorOf7(1, 2, 3, 4, 5, 6, 7));
        Vector.sameElements(v, vectorOf7(-2, -3, -4, -5, -6, -7, -8))

    /////////////////////////////////////////////////////////////////////////////
    // intersperse                                                             //
    /////////////////////////////////////////////////////////////////////////////
/*
    @test
    def intersperse01(): Bool =
        let v = Vector.intersperse(11, Vector.empty());
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def intersperse02(): Bool =
        let v = Vector.intersperse(11, Vector.singleton(1));
        Vector.sameElements(v, Vector.singleton(1))

    @test
    def intersperse03(): Bool =
        let v = Vector.intersperse(11, vectorOf2(1, 2));
        Vector.sameElements(v, vectorOf3(1, 11, 2))

    @test
    def intersperse04(): Bool =
        let v = Vector.intersperse(11, vectorOf3(1, 2, 3));
        Vector.sameElements(v, vectorOf5(1, 11, 2, 11, 3))

    @test
    def intersperse05(): Bool =
        let v = Vector.intersperse(11, vectorOf4(1, 2, 3, 4));
        Vector.sameElements(v, vectorOf7(1, 11, 2, 11, 3, 11, 4))

    /////////////////////////////////////////////////////////////////////////////
    // intercalate                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def intercalate01(): Bool =
        let v = Vector.intercalate(Vector.empty(): Vector[Int32], (Vector.empty()): Vector[Vector[Int32]]);
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def intercalate02(): Bool =
        let v = Vector.intercalate(Vector.empty(): Vector[Int32], Vector.singleton(Vector.singleton(1)));
        Vector.sameElements(v, Vector.singleton(1))

    @test
    def intercalate03(): Bool =
        let v = Vector.intercalate(vectorOf3(11, 12, 13), (Vector.empty()): Vector[Vector[Int32]]);
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def intercalate04(): Bool =
        let v = Vector.intercalate(Vector.empty(): Vector[Int32], vectorOf2(Vector.singleton(1), vectorOf2(2, 3)));
        Vector.sameElements(v, vectorOf3(1, 2, 3))

    @test
    def intercalate05(): Bool =
        let v = Vector.intercalate(vectorOf3(11, 12, 13), Array#{Vector.singleton(1), vectorOf2(2, 3)} @ r);
        Vector.sameElements(v, Array#{1,11,12,13,2,3} @ r)

    @test
    def intercalate06(): Bool =
        let v = Vector.intercalate(Vector.empty(): Vector[Int32], Array#{Vector.singleton(1), vectorOf2(2, 3), Vector.singleton(4)} @ r);
        Vector.sameElements(v, Array#{1,2,3,4} @ r)

    @test
    def intercalate07(): Bool =
        let v = Vector.intercalate(vectorOf3(11, 12, 13), Array#{Vector.singleton(1), vectorOf2(2, 3), Vector.singleton(4)} @ r);
        Vector.sameElements(v, Array#{1,11,12,13,2,3,11,12,13,4} @ r)

    /////////////////////////////////////////////////////////////////////////////
    // transpose                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def transpose01(): Bool =
        let v = Vector.transpose(Vector.empty());
        let b = Vector.toList(a) |> List.map(Vector.toList);
        b: List[List[Unit]] == Nil

    @test
    def transpose02(): Bool =
        let a: Array[Vector[Int32], _] = Vector.transpose(Array#{Vector.empty()} @ r);
        let b = Vector.toList(a) |> List.map(Vector.toList);
        b == Nil :: Nil

    @test
    def transpose03(): Bool =
        let a: Array[Vector[Int32], _] = Vector.transpose(Array#{Vector.empty(), Vector.empty()} @ r);
        let b = Vector.toList(a) |> List.map(Vector.toList);
        b == Nil :: Nil :: Nil

    @test
    def transpose04(): Bool =
        let a: Array[Vector[Int32], _] = Vector.transpose(Array#{Vector.empty(), Vector.empty(), Vector.empty()} @ r);
        let b = Vector.toList(a) |> List.map(Vector.toList);
        b == Nil :: Nil :: Nil :: Nil

    @test
    def transpose05(): Bool =
        let v = Vector.transpose(Array#{ Vector.singleton(1) } @ r);
        let b = Vector.toList(a) |> List.map(Vector.toList);
        b == (1 :: Nil) :: Nil

    @test
    def transpose06(): Bool =
        let v = Vector.transpose(Array#{vectorOf2(1, 2)} @ r);
        let b = Vector.toList(a) |> List.map(Vector.toList);
        b == (1 :: Nil) :: (2 :: Nil) :: Nil

    @test
    def transpose07(): Bool =
        let v = Vector.transpose(Array#{vectorOf3(1, 2, 3)} @ r);
        let b = Vector.toList(a) |> List.map(Vector.toList);
        b == (1 :: Nil) :: (2 :: Nil) :: (3 :: Nil) :: Nil

    @test
    def transpose08(): Bool =
        let v = Vector.transpose(Array#{Array#{1, 2, 3, 4} @ r} @ r);
        let b = Vector.toList(a) |> List.map(Vector.toList);
        b == (1 :: Nil) :: (2 :: Nil) :: (3 :: Nil) :: (4 :: Nil) :: Nil

    @test
    def transpose09(): Bool =
        let v = Vector.transpose(Array#{Vector.singleton(1), Vector.singleton(2)} @ r);
        let b = Vector.toList(a) |> List.map(Vector.toList);
        b == (1 :: 2 :: Nil) :: Nil

    @test
    def transpose10(): Bool =
        let v = Vector.transpose(Array#{Vector.singleton(1), Vector.singleton(2), Vector.singleton(3)} @ r);
        let b = Vector.toList(a) |> List.map(Vector.toList);
        b == (1 :: 2 :: 3 :: Nil) :: Nil

    @test
    def transpose11(): Bool =
        let v = Vector.transpose(Array#{Vector.singleton(1), Vector.singleton(2), Vector.singleton(3), Vector.singleton(4)} @ r);
        let b = Vector.toList(a) |> List.map(Vector.toList);
        b == (1 :: 2 :: 3 :: 4 :: Nil) :: Nil

    @test
    def transpose12(): Bool =
        let v = Vector.transpose(Array#{vectorOf2(1, 2), Array#{3, 4} @ r} @ r);
        let b = Vector.toList(a) |> List.map(Vector.toList);
        b == (1 :: 3 :: Nil) :: (2 :: 4 :: Nil) :: Nil

    @test
    def transpose13(): Bool =
        let v = Vector.transpose(Array#{vectorOf3(1, 2, 3), Array#{4, 5, 6} @ r} @ r);
        let b = Vector.toList(a) |> List.map(Vector.toList);
        b == (1 :: 4 :: Nil) :: (2 :: 5 :: Nil) :: (3 :: 6 :: Nil) :: Nil

    @test
    def transpose14(): Bool =
        let v = Vector.transpose(Array#{Array#{1, 2, 3, 4} @ r, Array#{5, 6, 7, 8} @ r} @ r);
        let b = Vector.toList(a) |> List.map(Vector.toList);
        b == (1 :: 5 :: Nil) :: (2 :: 6 :: Nil) :: (3 :: 7 :: Nil) :: (4 :: 8 :: Nil) :: Nil

    @test
    def transpose15(): Bool =
        let v = Vector.transpose(Array#{Array#{1, 2, 3, 4, 5} @ r, Array#{6, 7, 8, 9, 10} @ r} @ r);
        let b = Vector.toList(a) |> List.map(Vector.toList);
        b == (1 :: 6 :: Nil) :: (2 :: 7 :: Nil) :: (3 :: 8 :: Nil) :: (4 :: 9 :: Nil) :: (5 :: 10 :: Nil) :: Nil

    @test
    def transpose16(): Bool =
        let v = Vector.transpose(Array#{vectorOf2(1, 2), Array#{3, 4} @ r, Array#{5, 6} @ r} @ r);
        let b = Vector.toList(a) |> List.map(Vector.toList);
        b == (1 :: 3 :: 5 :: Nil) :: (2 :: 4 :: 6 :: Nil) :: Nil

    @test
    def transpose17(): Bool =
        let v = Vector.transpose(Array#{vectorOf2(1, 2), Array#{3, 4} @ r, Array#{5, 6} @ r, Array#{7,8} @ r} @ r);
        let b = Vector.toList(a) |> List.map(Vector.toList);
        b == (1 :: 3 :: 5 :: 7 :: Nil) :: (2 :: 4 :: 6 :: 8 :: Nil) :: Nil

    @test
    def transpose18(): Bool =
        let v = Vector.transpose(Array#{vectorOf2(1, 2), Array#{3, 4} @ r, Array#{5, 6} @ r, Array#{7, 8} @ r, Array#{9, 10} @ r} @ r);
        let b = Vector.toList(a) |> List.map(Vector.toList);
        b == (1 :: 3 :: 5 :: 7 :: 9 :: Nil) :: (2 :: 4 :: 6 :: 8 :: 10 :: Nil) :: Nil

    @test
    def transpose19(): Bool =
        let v = Vector.transpose(Array#{vectorOf3(1, 2, 3), vectorOf3(4, 5, 6) Array#{7, 8, 9} @ r} @ r);
        let b = Vector.toList(a) |> List.map(Vector.toList);
        b == (1 :: 4 :: 7 :: Nil) :: (2 :: 5 :: 8 :: Nil) :: (3 :: 6 :: 9 :: Nil) :: Nil

    @test
    def transpose20(): Bool =
        let v = Vector.transpose(Array#{vectorOf3(1, 2, 3), Array#{4, 5} @ r, Array#{7, 8, 9} @ r} @ r);
        let b = Vector.toList(a) |> List.map(Vector.toList);
        b == (1 :: 2 :: 3 :: Nil) :: (4 :: 5 :: Nil) :: (7 :: 8 :: 9 :: Nil) :: Nil

    @test
    def transpose21(): Bool =
        let v = Vector.transpose(Array#{vectorOf3(1, 2, 3), Vector.empty(), Array#{7, 8, 9} @ r} @ r);
        let b = Vector.toList(a) |> List.map(Vector.toList);
        b == (1 :: 2 :: 3 :: Nil) :: Nil :: (7 :: 8 :: 9 :: Nil) :: Nil

    @test
    def transpose22(): Bool =
        let v = Vector.transpose(Array#{vectorOf3(1, 2, 3), vectorOf3(4, 5, 6) Array#{7, 8, 9, 10} @ r} @ r);
        let b = Vector.toList(a) |> List.map(Vector.toList);
        b == (1 :: 2 :: 3 :: Nil) :: (4 :: 5 :: 6 :: Nil) :: (7 :: 8 :: 9 :: 10 :: Nil) :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // isPrefixOf                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def isPrefixOf01(): Bool =
        Vector.isPrefixOf(Vector.empty(): Vector[Int32], Vector.empty(): Vector[Int32])

    @test
    def isPrefixOf02(): Bool =
        Vector.isPrefixOf(Vector.singleton(1), Vector.empty(): Vector[Int32]) == false

    @test
    def isPrefixOf03(): Bool =
        Vector.isPrefixOf(Vector.empty(): Vector[Int32], Vector.singleton(1))

    @test
    def isPrefixOf04(): Bool =
        Vector.isPrefixOf(Vector.singleton(1), Vector.singleton(1))

    @test
    def isPrefixOf05(): Bool =
        Vector.isPrefixOf(Vector.singleton(2), Vector.singleton(1)) == false

    @test
    def isPrefixOf06(): Bool =
        Vector.isPrefixOf(vectorOf2(1, 2), Vector.singleton(1)) == false

    @test
    def isPrefixOf07(): Bool =
        Vector.isPrefixOf(Vector.empty(): Vector[Int32], vectorOf2(1, 2))

    @test
    def isPrefixOf08(): Bool =
        Vector.isPrefixOf(Vector.singleton(1), vectorOf2(1, 2))

    @test
    def isPrefixOf09(): Bool =
        Vector.isPrefixOf(Vector.singleton(2), vectorOf2(1, 2)) == false

    @test
    def isPrefixOf10(): Bool =
        Vector.isPrefixOf(vectorOf2(1, 2), vectorOf2(1, 2))

    @test
    def isPrefixOf11(): Bool =
        Vector.isPrefixOf(vectorOf2(1, 3), vectorOf2(1, 2)) == false

    @test
    def isPrefixOf12(): Bool =
        Vector.isPrefixOf(vectorOf3(1, 2, 3), vectorOf2(1, 2)) == false

    @test
    def isPrefixOf13(): Bool =
        Vector.isPrefixOf(vectorOf3(1, 2, 3), Array#{89,11,1,2,3,4} @ r) == false

    /////////////////////////////////////////////////////////////////////////////
    // isInfixOf                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def isInfixOf01(): Bool =
        Vector.isInfixOf(Vector.empty(): Vector[Int32], Vector.empty(): Vector[Int32])

    @test
    def isInfixOf02(): Bool =
        Vector.isInfixOf(Vector.singleton(1), Vector.empty(): Vector[Int32]) == false

    @test
    def isInfixOf03(): Bool =
        Vector.isInfixOf(Vector.empty(): Vector[Int32], Vector.singleton(1))

    @test
    def isInfixOf04(): Bool =
        Vector.isInfixOf(Vector.singleton(1), Vector.singleton(1))

    @test
    def isInfixOf05(): Bool =
        Vector.isInfixOf(Vector.singleton(2), Vector.singleton(1)) == false

    @test
    def isInfixOf06(): Bool =
        Vector.isInfixOf(vectorOf2(1, 2), Vector.singleton(1)) == false

    @test
    def isInfixOf07(): Bool =
        Vector.isInfixOf(Vector.empty(): Vector[Int32], vectorOf2(1, 2))

    @test
    def isInfixOf08(): Bool =
        Vector.isInfixOf(Vector.singleton(1), vectorOf2(1, 2))

    @test
    def isInfixOf09(): Bool =
        Vector.isInfixOf(Vector.singleton(2), vectorOf2(1, 2))

    @test
    def isInfixOf10(): Bool =
        Vector.isInfixOf(vectorOf2(1, 2), vectorOf2(1, 2))

    @test
    def isInfixOf11(): Bool =
        Vector.isInfixOf(vectorOf2(1, 3), vectorOf2(1, 2)) == false

    @test
    def isInfixOf12(): Bool =
        Vector.isInfixOf(vectorOf3(1, 2, 3), vectorOf2(1, 2)) == false

    @test
    def isInfixOf13(): Bool =
        Vector.isInfixOf(vectorOf3(1, 2, 3), Array#{89,11,1,2,3,4} @ r)

    /////////////////////////////////////////////////////////////////////////////
    // isSuffixOf                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def isSuffixOf01(): Bool =
        Vector.isSuffixOf(Vector.empty(): Vector[Int32], Vector.empty(): Vector[Int32])

    @test
    def isSuffixOf02(): Bool =
        Vector.isSuffixOf(Vector.singleton(1), Vector.empty(): Vector[Int32]) == false

    @test
    def isSuffixOf03(): Bool =
        Vector.isSuffixOf(Vector.empty(): Vector[Int32], Vector.singleton(1))

    @test
    def isSuffixOf04(): Bool =
        Vector.isSuffixOf(Vector.singleton(1), Vector.singleton(1))

    @test
    def isSuffixOf05(): Bool =
        Vector.isSuffixOf(Vector.singleton(2), Vector.singleton(1)) == false

    @test
    def isSuffixOf06(): Bool =
        Vector.isSuffixOf(vectorOf2(1, 2), Vector.singleton(1)) == false

    @test
    def isSuffixOf07(): Bool =
        Vector.isSuffixOf(Vector.empty(): Vector[Int32], vectorOf2(1, 2))

    @test
    def isSuffixOf08(): Bool =
        Vector.isSuffixOf(Vector.singleton(1), vectorOf2(1, 2)) == false

    @test
    def isSuffixOf09(): Bool =
        Vector.isSuffixOf(Vector.singleton(2), vectorOf2(1, 2))

    @test
    def isSuffixOf10(): Bool =
        Vector.isSuffixOf(vectorOf2(1, 2), vectorOf2(1, 2))

    @test
    def isSuffixOf11(): Bool =
        Vector.isSuffixOf(vectorOf2(1, 3), vectorOf2(1, 2)) == false

    @test
    def isSuffixOf12(): Bool =
        Vector.isSuffixOf(vectorOf3(1, 2, 3), vectorOf2(1, 2)) == false

    @test
    def isSuffixOf13(): Bool =
        Vector.isSuffixOf(vectorOf3(1, 2, 3), Array#{89,11,1,2,3} @ r)

    /////////////////////////////////////////////////////////////////////////////
    // eq                                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testSameElements01(): Bool =
        (Vector.empty(): Vector[Int32]) `sameElements` (Vector.empty(): Vector[Int32])

    @test
    def testSameElements02(): Bool =
        not ((Vector.singleton(1)) `sameElements` Vector.empty())

    @test
    def testSameElements03(): Bool =
        not ((Vector.empty()) `sameElements` Vector.singleton(1))

    @test
    def testSameElements04(): Bool =
        (Vector.singleton(1)) `sameElements` Vector.singleton(1)

    @test
    def testSameElements05(): Bool =
        not ((Vector.singleton(1)) `sameElements` Vector.singleton(2))

    @test
    def testSameElements06(): Bool =
        not ((vectorOf2(1, 2)) `sameElements` Vector.singleton(1))

    @test
    def testSameElements07(): Bool =
        not ((Vector.singleton(1)) `sameElements` vectorOf2(1, 2))

    @test
    def testSameElements08(): Bool =
        (Array#{1,2,3,4} @ r) `sameElements` Array#{1,2,3,4} @ r

    /////////////////////////////////////////////////////////////////////////////
    // compare                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testCompare01(): Bool =
        (Vector.empty(): Vector[Int32]) `Vector.compare` (Vector.empty(): Vector[Int32]) == Comparison.EqualTo

    @test
    def testCompare02(): Bool =
        ((Vector.singleton(1)) `Vector.compare` (Vector.empty())) == Comparison.GreaterThan

    @test
    def testCompare03(): Bool =
        ((Vector.empty()) `Vector.compare` (Vector.singleton(1))) == Comparison.LessThan

    @test
    def testCompare04(): Bool =
        ((Vector.singleton(1)) `Vector.compare` (Vector.singleton(1))) == Comparison.EqualTo

    @test
    def testCompare05(): Bool =
        ((Vector.singleton(1)) `Vector.compare` (Vector.singleton(2))) == Comparison.LessThan

    @test
    def testCompare06(): Bool =
        ((Vector.singleton(2)) `Vector.compare` (Vector.singleton(1))) == Comparison.GreaterThan

    @test
    def testCompare07(): Bool =
        ((Vector.singleton(1)) `Vector.compare` (vectorOf2(1, 2))) == Comparison.LessThan

    @test
    def testCompare08(): Bool =
        ((vectorOf2(1, 2)) `Vector.compare` (Vector.singleton(1))) == Comparison.GreaterThan

    @test
    def testCompare09(): Bool =
        ((Vector.singleton(1)) `Vector.compare` (Array#{0,0} @ r)) == Comparison.GreaterThan

    @test
    def testCompare10(): Bool =
        ((Array#{0,0} @ r) `Vector.compare` (Vector.singleton(1))) == Comparison.LessThan

    @test
    def testCompare11(): Bool =
        ((vectorOf2(1, 2)) `Vector.compare` (vectorOf2(1,1))) == Comparison.GreaterThan

    @test
    def testCompare12(): Bool =
        ((vectorOf2(1, 2)) `Vector.compare` (vectorOf2(1, 3))) == Comparison.LessThan

    @test
    def testCompare13(): Bool =
        ((Array#{1,2,3,4} @ r) `Vector.compare` (Array#{1,2,3,4} @ r)) == Comparison.EqualTo

    /////////////////////////////////////////////////////////////////////////////
    // fold                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def fold01(): Bool =
        Vector.fold((Vector.empty()): Array[Unit, _]) == ()

    @test
    def fold02(): Bool =
        Vector.fold(Array#{"a"} @ r) == "a"

    @test
    def fold03(): Bool =
        Vector.fold(Array#{"a", "b", "c"} @ r) == "abc"

    @test
    def fold04(): Bool =
        Vector.fold(Array#{("yes", "no"), ("no", "yes")} @ r) == ("yesno", "noyes")

    /////////////////////////////////////////////////////////////////////////////
    // foldLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldLeft01(): Bool =
        Vector.foldLeft((i, e) -> (i - e) * (e `Int32.rem` 2 + 1), 100, Vector.empty()) == 100

    @test
    def foldLeft02(): Bool =
        Vector.foldLeft((i, e) -> (i - e) * (e `Int32.rem` 2 + 1), 100, Vector.singleton(1)) == 198

    @test
    def foldLeft03(): Bool =
        Vector.foldLeft((i, e) -> (i - e) * (e `Int32.rem` 2 + 1), 100, vectorOf2(1, 2)) == 196

    @test
    def foldLeft04(): Bool =
        Vector.foldLeft((i, e) -> (i - e) * (e `Int32.rem` 2 + 1), 100, vectorOf3(1, 2, 3)) == 386

    /////////////////////////////////////////////////////////////////////////////
    // foldRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldRight01(): Bool =
        Vector.foldRight((e, i) -> (i - e) * (e `Int32.rem` 2 + 1), 100, Vector.empty()) == 100

    @test
    def foldRight02(): Bool =
        Vector.foldRight((e, i) -> (i - e) * (e `Int32.rem` 2 + 1), 100, Vector.singleton(1)) == 198

    @test
    def foldRight03(): Bool =
        Vector.foldRight((e, i) -> (i - e) * (e `Int32.rem` 2 + 1), 100, vectorOf2(1, 2)) == 194

    @test
    def foldRight04(): Bool =
        Vector.foldRight((e, i) -> (i - e) * (e `Int32.rem` 2 + 1), 100, vectorOf3(1, 2, 3)) == 382

    /////////////////////////////////////////////////////////////////////////////
    // foldRightWithCont                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldRightWithCont01(): Bool =
        Vector.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.rem` 2 + 1), 100, Vector.empty()) == 100

    @test
    def foldRightWithCont02(): Bool =
        Vector.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.rem` 2 + 1), 100, Vector.singleton(1)) == 198

    @test
    def foldRightWithCont03(): Bool =
        Vector.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.rem` 2 + 1), 100, vectorOf2(1, 2)) == 194

    @test
    def foldRightWithCont04(): Bool =
        Vector.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.rem` 2 + 1), 100, vectorOf3(1, 2, 3)) == 382

    /////////////////////////////////////////////////////////////////////////////
    // foldMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldMap01(): Bool =
        Vector.foldMap(x -> 2 * x, Vector.empty()) == 0

    @test
    def foldMap02(): Bool =
        Vector.foldMap(x -> 2 * x, vectorOf2(1, 2)) == 6

    @test
    def foldMap03(): Bool =
        Vector.foldMap(x -> if (x == "a") "b" else x, Array#{"a"} @ r) == "b"

    @test
    def foldMap04(): Bool =
        Vector.foldMap(x -> if (x == "c") "b" else x, Array#{"a", "b", "c"} @ r) == "abb"

    @test
    def foldMap05(): Bool =
        Vector.foldMap(Int32.toString, vectorOf3(1, 2, 3)) == "123"

    /////////////////////////////////////////////////////////////////////////////
    // reduceLeft                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reduceLeft01(): Bool =
        Vector.reduceLeft((a, b) -> a - b, Vector.empty(): Vector[Int32]) == None

    @test
    def reduceLeft02(): Bool =
        Vector.reduceLeft((a, b) -> a - b, Vector.singleton(1)) == Some(1)

    @test
    def reduceLeft03(): Bool =
        Vector.reduceLeft((a, b) -> a - b, vectorOf2(1, 2)) == Some(-1)

    @test
    def reduceLeft04(): Bool =
        Vector.reduceLeft((a, b) -> a - b, vectorOf3(1, 2, 3)) == Some(-4)

    @test
    def reduceLeft05(): Bool =
        Vector.reduceLeft((a, b) -> a - b, Array#{1,2,3,4} @ r) == Some(-8)

    /////////////////////////////////////////////////////////////////////////////
    // reduceRight                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reduceRight01(): Bool =
        Vector.reduceRight((a, b) -> a - b, Vector.empty(): Vector[Int32]) == None

    @test
    def reduceRight02(): Bool =
        Vector.reduceRight((a, b) -> a - b, Vector.singleton(1)) == Some(1)

    @test
    def reduceRight03(): Bool =
        Vector.reduceRight((a, b) -> a - b, vectorOf2(1, 2)) == Some(-1)

    @test
    def reduceRight04(): Bool =
        Vector.reduceRight((a, b) -> a - b, vectorOf3(1, 2, 3)) == Some(2)

    @test
    def reduceRight05(): Bool =
        Vector.reduceRight((a, b) -> a - b, Array#{1,2,3,4} @ r) == Some(-2)

    /////////////////////////////////////////////////////////////////////////////
    // count                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def count01(): Bool =
        Vector.count(i -> i > 3, Vector.empty()) == 0

    @test
    def count02(): Bool =
        Vector.count(i -> i > 3, Vector.singleton(1)) == 0

    @test
    def count03(): Bool =
        Vector.count(i -> i > 3, Vector.singleton(4)) == 1

    @test
    def count04(): Bool =
        Vector.count(i -> i > 3, vectorOf2(1, 2)) == 0

    @test
    def count05(): Bool =
        Vector.count(i -> i > 3, Array#{1,8} @ r) == 1

    @test
    def count06(): Bool =
        Vector.count(i -> i > 3, Array#{8,1} @ r) == 1

    @test
    def count07(): Bool =
        Vector.count(i -> i > 3, vectorOf2(6, 7)) == 2


    /////////////////////////////////////////////////////////////////////////////
    // product                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def product01(): Bool =
        Vector.product(Vector.empty()) == 1

    @test
    def product02(): Bool =
        Vector.product(Vector.singleton(1)) == 1

    @test
    def product03(): Bool =
        Vector.product(vectorOf3(1, 2, 3)) == 6

    @test
    def product04(): Bool =
        Vector.product(Array#{1, 2, 3, -3} @ r) == -18

    @test
    def product05(): Bool =
        Vector.product(Array#{-1, -2, -3, -4} @ r) == 24

    @test
    def product06(): Bool =
        Vector.product(Array#{10, -10} @ r) == -100


    /////////////////////////////////////////////////////////////////////////////
    // productWith                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def productWith01(): Bool =
        Vector.productWith(x -> x + 1, Vector.empty()) == 1

    @test
    def productWith02(): Bool =
        Vector.productWith(x -> x + 1, Vector.singleton(1)) == 2

    @test
    def productWith03(): Bool =
        Vector.productWith(x -> x + 1, vectorOf3(1, 2, 3)) == 24

    @test
    def productWith04(): Bool =
        Vector.productWith(x -> x + 1, Array#{1, 2, 3, -3} @ r) == -48

    @test
    def productWith05(): Bool =
        Vector.productWith(x -> x + 1, Array#{-2, -3, -4, -5} @ r) == 24

    @test
    def productWith06(): Bool =
        Vector.productWith(x -> x + 1, Array#{10, -10} @ r) == -99

    /////////////////////////////////////////////////////////////////////////////
    // flatten                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def flatten01(): Bool =
        let v = Vector.flatten((Vector.empty()): Array[Vector[Int32], r]);
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def flatten02(): Bool =
        let v = Vector.flatten((Array#{Vector.empty()} @ r): Array[Vector[Int32], r]);
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def flatten03(): Bool =
        let v = Vector.flatten(Array#{Vector.singleton(1)} @ r);
        Vector.sameElements(v, Vector.singleton(1))

    @test
    def flatten04(): Bool =
        let a= Vector.flatten(Array#{vectorOf2(1, 2)} @ r);
        Vector.sameElements(v, vectorOf2(1, 2))

    @test
    def flatten05(): Bool =
        let v = Vector.flatten((Array#{Vector.empty(),Vector.empty()} @ r): Array[Vector[Int32], r]);
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def flatten06(): Bool =
        let v = Vector.flatten(Array#{Vector.singleton(1),Vector.empty()} @ r);
        Vector.sameElements(v, Vector.singleton(1))

    @test
    def flatten07(): Bool =
        let v = Vector.flatten(Array#{Vector.empty(),Vector.singleton(1)} @ r);
        Vector.sameElements(v, Vector.singleton(1))

    @test
    def flatten08(): Bool =
        let v = Vector.flatten(Array#{Vector.singleton(1),Vector.singleton(2)} @ r);
        Vector.sameElements(v, vectorOf2(1, 2))

    @test
    def flatten09(): Bool =
        let v = Vector.flatten(Array#{vectorOf2(1, 2),Array#{3,4,5} @ r} @ r);
        Vector.sameElements(v, Array#{1,2,3,4,5} @ r)

    @test
    def flatten10(): Bool =
        let v = Vector.flatten(Array#{Vector.singleton(1),vectorOf2(2, 3),Vector.singleton(4)} @ r);
        Vector.sameElements(v, Array#{1,2,3,4} @ r)

    /////////////////////////////////////////////////////////////////////////////
    // exists                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def exists01(): Bool =
        Vector.exists(i -> i > 3, Vector.empty(): Vector[Int32]) == false

    @test
    def exists02(): Bool =
        Vector.exists(i -> i > 3, Vector.singleton(1)) == false

    @test
    def exists03(): Bool =
        Vector.exists(i -> i > 3, Vector.singleton(5)) == true

    @test
    def exists04(): Bool =
        Vector.exists(i -> i > 3, vectorOf2(1, 2)) == false

    @test
    def exists05(): Bool =
        Vector.exists(i -> i > 3, Array#{1,6} @ r) == true

    @test
    def exists06(): Bool =
        Vector.exists(i -> i > 3, Array#{6,1} @ r) == true

    @test
    def exists07(): Bool =
        Vector.exists(i -> i > 3, Array#{16,6} @ r) == true

    @test
    def exists08(): Bool =
        Vector.exists(i -> i > 3, Array#{1,-9,3} @ r) == false

    @test
    def exists09(): Bool =
        Vector.exists(i -> i > 3, Array#{1,9,3} @ r) == true

    /////////////////////////////////////////////////////////////////////////////
    // forAll                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def forAll01(): Bool =
        Vector.forAll(i -> i > 3, Vector.empty(): Vector[Int32]) == true

    @test
    def forAll02(): Bool =
        Vector.forAll(i -> i > 3, Vector.singleton(1)) == false

    @test
    def forAll03(): Bool =
        Vector.forAll(i -> i > 3, Vector.singleton(5)) == true

    @test
    def forAll04(): Bool =
        Vector.forAll(i -> i > 3, vectorOf2(1, 2)) == false

    @test
    def forAll05(): Bool =
        Vector.forAll(i -> i > 3, Array#{1,6} @ r) == false

    @test
    def forAll06(): Bool =
        Vector.forAll(i -> i > 3, Array#{6,1} @ r) == false

    @test
    def forAll07(): Bool =
        Vector.forAll(i -> i > 3, Array#{16,6} @ r) == true

    @test
    def forAll08(): Bool =
        Vector.forAll(i -> i > 3, Array#{1,-9,3} @ r) == false

    @test
    def forAll09(): Bool =
        Vector.forAll(i -> i > 3, Array#{1,9,3} @ r) == false

    @test
    def forAll10(): Bool =
        Vector.forAll(i -> i > 3, Array#{11,9,31} @ r) == true

    /////////////////////////////////////////////////////////////////////////////
    // filter                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filter01(): Bool =
        let v = Vector.filter(i -> i > 3, Vector.empty(): Vector[Int32]);
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def filter02(): Bool =
        let v = Vector.filter(i -> i > 3, Vector.singleton(2));
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def filter03(): Bool =
        let v = Vector.filter(i -> i > 3, Vector.singleton(4));
        Vector.sameElements(v, Vector.singleton(4))

    @test
    def filter04(): Bool =
        let v = Vector.filter(i -> i > 3, vectorOf2(1, 3));
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def filter05(): Bool =
        let v = Vector.filter(i -> i > 3, Array#{1,8} @ r);
        Vector.sameElements(v, Array#{8} @ r)

    @test
    def filter06(): Bool =
        let v = Vector.filter(i -> i > 3, Array#{8,1} @ r);
        Vector.sameElements(v, Array#{8} @ r)

    @test
    def filter07(): Bool =
        let v = Vector.filter(i -> i > 3, Array#{8,9} @ r);
        Vector.sameElements(v, Array#{8,9} @ r)

    @test
    def filter08(): Bool =
        let v = Vector.filter(i -> i > 3, Array#{1,4,11,2,-22,17} @ r);
        Vector.sameElements(v, Array#{4,11,17} @ r)

    /////////////////////////////////////////////////////////////////////////////
    // partition                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def partition01(): Bool =
        let (a,b) = Vector.partition(r, i -> i > 3, Vector.empty(): Vector[Int32]);
        Vector.sameElements(v, Vector.empty(): Vector[Int32]) and Vector.sameElements(b, Vector.empty(): Vector[Int32])

    @test
    def partition02(): Bool =
        let (a,b) = Vector.partition(r, i -> i > 3, Vector.singleton(1));
        Vector.sameElements(v, Vector.empty(): Vector[Int32]) and Vector.sameElements(b, Vector.singleton(1))

    @test
    def partition03(): Bool =
        let (a,b) = Vector.partition(r, i -> i > 3, Vector.singleton(4));
        Vector.sameElements(v, Vector.singleton(4)) and Vector.sameElements(b, Vector.empty(): Vector[Int32])

    @test
    def partition04(): Bool =
        let (a,b) = Vector.partition(r, i -> i > 3, vectorOf2(1, 2));
        Vector.sameElements(v, Vector.empty(): Vector[Int32]) and Vector.sameElements(b, vectorOf2(1, 2))

    @test
    def partition05(): Bool =
        let (a,b) = Vector.partition(r, i -> i > 3, Array#{1,5} @ r);
        Vector.sameElements(v, Vector.singleton(5)) and Vector.sameElements(b, Vector.singleton(1))

    @test
    def partition06(): Bool =
        let (a,b) = Vector.partition(r, i -> i > 3, Array#{5,1} @ r);
        Vector.sameElements(v, Vector.singleton(5)) and Vector.sameElements(b, Vector.singleton(1))

    @test
    def partition07(): Bool =
        let (a,b) = Vector.partition(r, i -> i > 3, Array#{5,8} @ r);
        Vector.sameElements(v, Array#{5,8} @ r) and Vector.sameElements(b, Vector.empty(): Vector[Int32])

    @test
    def partition08(): Bool =
        let (a,b) = Vector.partition(r, i -> i > 3, Array#{4, -3, -5, 1, 2, 16, 7, 1, 7} @ r);
        Vector.sameElements(v, Array#{4, 16, 7, 7} @ r) and Vector.sameElements(b, Array#{-3, -5, 1, 2, 1} @ r)

    /////////////////////////////////////////////////////////////////////////////
    // span                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def span01(): Bool =
        let (a,b) = Vector.span(r, i -> i > 3, Vector.empty(): Vector[Int32]);
        Vector.sameElements(v, Vector.empty(): Vector[Int32]) and Vector.sameElements(b, Vector.empty(): Vector[Int32])

    @test
    def span02(): Bool =
        let (a,b) = Vector.span(r, i -> i > 3, Vector.singleton(1));
        Vector.sameElements(v,Vector.empty(): Vector[Int32]) and Vector.sameElements(b, Vector.singleton(1))

    @test
    def span03(): Bool =
        let (a,b) = Vector.span(r, i -> i > 3, Vector.singleton(4));
        Vector.sameElements(v, Vector.singleton(4)) and Vector.sameElements(b, Vector.empty(): Vector[Int32])

    @test
    def span04(): Bool =
        let (a,b) = Vector.span(r, i -> i > 3, vectorOf2(1, 2));
        Vector.sameElements(v, Vector.empty(): Vector[Int32]) and Vector.sameElements(b, vectorOf2(1, 2))

    @test
    def span05(): Bool =
        let (a,b) = Vector.span(r, i -> i > 3, Array#{1,5} @ r);
        Vector.sameElements(v, Vector.empty(): Vector[Int32]) and Vector.sameElements(b, Array#{1,5} @ r)

    @test
    def span06(): Bool =
        let (a,b) = Vector.span(r, i -> i > 3, Array#{5,1} @ r);
        Vector.sameElements(v, Vector.singleton(5)) and Vector.sameElements(b, Vector.singleton(1))

    @test
    def span07(): Bool =
        let (a,b) = Vector.span(r, i -> i > 3, Array#{5,8} @ r);
        Vector.sameElements(v, Array#{5,8} @ r) and Vector.sameElements(b, Vector.empty(): Vector[Int32])

    @test
    def span08(): Bool =
        let (a,b) = Vector.span(r, i -> i > 3, Array#{4, 6, -3, 11, -5, 1, 2, 16, 7, 1, 7} @ r);
        Vector.sameElements(v, Array#{4, 6} @ r) and Vector.sameElements(b, Array#{-3, 11, -5, 1, 2, 16, 7, 1, 7} @ r)

    /////////////////////////////////////////////////////////////////////////////
    // drop                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def drop01(): Bool =
        let v = Vector.drop(-1, Vector.empty(): Vector[Int32]);
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def drop02(): Bool =
        let v = Vector.drop(0, Vector.empty(): Vector[Int32]);
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def drop03(): Bool =
        let v = Vector.drop(1, Vector.empty(): Vector[Int32]);
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def drop04(): Bool =
        let v = Vector.drop(-1, Vector.singleton(1));
        Vector.sameElements(v, Vector.singleton(1))

    @test
    def drop05(): Bool =
        let v = Vector.drop(0, Vector.singleton(1));
        Vector.sameElements(v, Vector.singleton(1))

    @test
    def drop06(): Bool =
        let v = Vector.drop(1, Vector.singleton(1));
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def drop07(): Bool =
        let v = Vector.drop(2, Vector.singleton(1));
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def drop08(): Bool =
        let v = Vector.drop(0, vectorOf2(1, 2));
        Vector.sameElements(v, vectorOf2(1, 2))

    @test
    def drop09(): Bool =
        let v = Vector.drop(1, vectorOf2(1, 2));
        Vector.sameElements(v, Vector.singleton(2))

    @test
    def drop10(): Bool =
        let v = Vector.drop(2, vectorOf2(1, 2));
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def drop11(): Bool =
        let v = Vector.drop(2, Array#{1,2,3,4,5,6} @ r);
        Vector.sameElements(v, Array#{3,4,5,6} @ r)

    @test
    def drop12(): Bool =
        let v = Vector.drop(4, Array#{1,2,3,4,5,6} @ r);
        Vector.sameElements(v, Array#{5,6} @ r)

    /////////////////////////////////////////////////////////////////////////////
    // dropLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def dropLeft01(): Bool =
        let v = Vector.dropLeft(-1, Vector.empty(): Vector[Int32]);
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def dropLeft02(): Bool =
        let v = Vector.dropLeft(0, Vector.empty(): Vector[Int32]);
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def dropLeft03(): Bool =
        let v = Vector.dropLeft(1, Vector.empty(): Vector[Int32]);
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def dropLeft04(): Bool =
        let v = Vector.dropLeft(-1, Vector.singleton(1));
        Vector.sameElements(v, Vector.singleton(1))

    @test
    def dropLeft05(): Bool =
        let v = Vector.dropLeft(0, Vector.singleton(1));
        Vector.sameElements(v, Vector.singleton(1))

    @test
    def dropLeft06(): Bool =
        let v = Vector.dropLeft(1, Vector.singleton(1));
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def dropLeft07(): Bool =
        let v = Vector.dropLeft(2, Vector.singleton(1));
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def dropLeft08(): Bool =
        let v = Vector.dropLeft(0, vectorOf2(1, 2));
        Vector.sameElements(v, vectorOf2(1, 2))

    @test
    def dropLeft09(): Bool =
        let v = Vector.dropLeft(1, vectorOf2(1, 2));
        Vector.sameElements(v, Vector.singleton(2))

    @test
    def dropLeft10(): Bool =
        let v = Vector.dropLeft(2, vectorOf2(1, 2));
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def dropLeft11(): Bool =
        let v = Vector.dropLeft(2, Array#{1,2,3,4,5,6} @ r);
        Vector.sameElements(v, Array#{3,4,5,6} @ r)

    @test
    def dropLeft12(): Bool =
        let v = Vector.dropLeft(4, Array#{1,2,3,4,5,6} @ r);
        Vector.sameElements(v, Array#{5,6} @ r)

    /////////////////////////////////////////////////////////////////////////////
    // dropRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def dropRight01(): Bool =
        let v = Vector.dropRight(-1, Vector.empty(): Vector[Int32]);
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def dropRight02(): Bool =
        let v = Vector.dropRight(0, Vector.empty(): Vector[Int32]);
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def dropRight03(): Bool =
        let v = Vector.dropRight(1, Vector.empty(): Vector[Int32]);
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def dropRight04(): Bool =
        let v = Vector.dropRight(-1, Vector.singleton(1));
        Vector.sameElements(v, Vector.singleton(1))

    @test
    def dropRight05(): Bool =
        let v = Vector.dropRight(0, Vector.singleton(1));
        Vector.sameElements(v, Vector.singleton(1))

    @test
    def dropRight06(): Bool =
        let v = Vector.dropRight(1, Vector.singleton(1));
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def dropRight07(): Bool =
        let v = Vector.dropRight(2, Vector.singleton(1));
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def dropRight08(): Bool =
        let v = Vector.dropRight(0, vectorOf2(1, 2));
        Vector.sameElements(v, vectorOf2(1, 2))

    @test
    def dropRight09(): Bool =
        let v = Vector.dropRight(1, vectorOf2(1, 2));
        Vector.sameElements(v, Vector.singleton(1))

    @test
    def dropRight10(): Bool =
        let v = Vector.dropRight(2, vectorOf2(1, 2));
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def dropRight11(): Bool =
        let v = Vector.dropRight(2, Array#{1,2,3,4,5,6} @ r);
        Vector.sameElements(v, Array#{1,2,3,4} @ r)

    @test
    def dropRight12(): Bool =
        let v = Vector.dropRight(4, Array#{1,2,3,4,5,6} @ r);
        Vector.sameElements(v, vectorOf2(1, 2))

    /////////////////////////////////////////////////////////////////////////////
    // dropWhile                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def dropWhile01(): Bool =
        let v = Vector.dropWhile(i -> i > 3, Vector.empty());
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def dropWhile02(): Bool =
        let v = Vector.dropWhile(i -> i > 3, Vector.singleton(1));
        Vector.sameElements(v, Vector.singleton(1))

    @test
    def dropWhile03(): Bool =
        let v = Vector.dropWhile(i -> i > 3, Vector.singleton(4));
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def dropWhile04(): Bool =
        let v = Vector.dropWhile(i -> i > 3, vectorOf2(1, 2));
        Vector.sameElements(v, vectorOf2(1, 2))

    @test
    def dropWhile05(): Bool =
        let v = Vector.dropWhile(i -> i > 3, Array#{1,5} @ r);
        Vector.sameElements(v, Array#{1,5} @ r)

    @test
    def dropWhile06(): Bool =
        let v = Vector.dropWhile(i -> i > 3, Array#{5,1} @ r);
        Vector.sameElements(v, Vector.singleton(1))

    @test
    def dropWhile07(): Bool =
        let v = Vector.dropWhile(i -> i > 3, Array#{5,8} @ r);
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def dropWhile08(): Bool =
        let v = Vector.dropWhile(i -> i > 3, Array#{4, 6, -3, 11, -5, 1, 2, 16, 7, 1, 7} @ r);
        Vector.sameElements(v, Array#{-3, 11, -5, 1, 2, 16, 7, 1, 7} @ r)

    /////////////////////////////////////////////////////////////////////////////
    // dropWhileLeft                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def dropWhileLeft01(): Bool =
        let v = Vector.dropWhileLeft(i -> i > 3, Vector.empty());
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def dropWhileLeft02(): Bool =
        let v = Vector.dropWhileLeft(i -> i > 3, Vector.singleton(1));
        Vector.sameElements(v, Vector.singleton(1))

    @test
    def dropWhileLeft03(): Bool =
        let v = Vector.dropWhileLeft(i -> i > 3, Vector.singleton(4));
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def dropWhileLeft04(): Bool =
        let v = Vector.dropWhileLeft(i -> i > 3, vectorOf2(1, 2));
        Vector.sameElements(v, vectorOf2(1, 2))

    @test
    def dropWhileLeft05(): Bool =
        let v = Vector.dropWhileLeft(i -> i > 3, Array#{1,5} @ r);
        Vector.sameElements(v, Array#{1,5} @ r)

    @test
    def dropWhileLeft06(): Bool =
        let v = Vector.dropWhileLeft(i -> i > 3, Array#{5,1} @ r);
        Vector.sameElements(v, Vector.singleton(1))

    @test
    def dropWhileLeft07(): Bool =
        let v = Vector.dropWhileLeft(i -> i > 3, Array#{5,8} @ r);
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def dropWhileLeft08(): Bool =
        let v = Vector.dropWhileLeft(i -> i > 3, Array#{4, 6, -3, 11, -5, 1, 2, 16, 7, 1, 7} @ r);
        Vector.sameElements(v, Array#{-3, 11, -5, 1, 2, 16, 7, 1, 7} @ r)

    /////////////////////////////////////////////////////////////////////////////
    // dropWhileRight                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def dropWhileRight01(): Bool =
        let v = Vector.dropWhileRight(i -> i > 3, Vector.empty());
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def dropWhileRight02(): Bool =
        let v = Vector.dropWhileRight(i -> i > 3, Vector.singleton(1));
        Vector.sameElements(v, Vector.singleton(1))

    @test
    def dropWhileRight03(): Bool =
        let v = Vector.dropWhileRight(i -> i > 3, Vector.singleton(4));
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def dropWhileRight04(): Bool =
        let v = Vector.dropWhileRight(i -> i > 3, vectorOf2(1, 2));
        Vector.sameElements(v, vectorOf2(1, 2))

    @test
    def dropWhileRight05(): Bool =
        let v = Vector.dropWhileRight(i -> i > 3, Array#{1,5} @ r);
        Vector.sameElements(v, Vector.singleton(1))

    @test
    def dropWhileRight06(): Bool =
        let v = Vector.dropWhileRight(i -> i > 3, Array#{5,1} @ r);
        Vector.sameElements(v, Array#{5,1} @ r)

    @test
    def dropWhileRight07(): Bool =
        let v = Vector.dropWhileRight(i -> i > 3, Array#{5,8} @ r);
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def dropWhileRight08(): Bool =
        let v = Vector.dropWhileRight(i -> i > 3, Array#{4, 6, -3, 11, -5, 1, 2, 16, 7, 1, 7} @ r);
        Vector.sameElements(v, Array#{4, 6, -3, 11, -5, 1, 2, 16, 7, 1} @ r)

    /////////////////////////////////////////////////////////////////////////////
    // take                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def take01(): Bool =
        let v = Vector.take(-1, Vector.empty(): Vector[Int32]);
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def take02(): Bool =
        let v = Vector.take(0, Vector.empty(): Vector[Int32]);
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def take03(): Bool =
        let v = Vector.take(1, Vector.empty(): Vector[Int32]);
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def take04(): Bool =
        let v = Vector.take(-1, Vector.singleton(1));
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def take05(): Bool =
        let v = Vector.take(0, Vector.singleton(1));
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def take06(): Bool =
        let v = Vector.take(1, Vector.singleton(1));
        Vector.sameElements(v, Vector.singleton(1))

    @test
    def take07(): Bool =
        let v = Vector.take(2, Vector.singleton(1));
        Vector.sameElements(v, Vector.singleton(1))

    @test
    def take08(): Bool =
        let v = Vector.take(0, vectorOf2(1, 2));
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def take09(): Bool =
        let v = Vector.take(1, vectorOf2(1, 2));
        Vector.sameElements(v, Vector.singleton(1))

    @test
    def take10(): Bool =
        let v = Vector.take(2, vectorOf2(1, 2));
        Vector.sameElements(v, vectorOf2(1, 2))

    @test
    def take11(): Bool =
        let v = Vector.take(2, Array#{1,2,3,4,5,6} @ r);
        Vector.sameElements(v, vectorOf2(1, 2))

    @test
    def take12(): Bool =
        let v = Vector.take(4, Array#{1,2,3,4,5,6} @ r);
        Vector.sameElements(v, Array#{1,2,3,4} @ r)

    /////////////////////////////////////////////////////////////////////////////
    // takeLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def takeLeft01(): Bool =
        let v = Vector.takeLeft(-1, Vector.empty(): Vector[Int32]);
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def takeLeft02(): Bool =
        let v = Vector.takeLeft(0, Vector.empty(): Vector[Int32]);
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def takeLeft03(): Bool =
        let v = Vector.takeLeft(1, Vector.empty(): Vector[Int32]);
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def takeLeft04(): Bool =
        let v = Vector.takeLeft(-1, Vector.singleton(1));
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def takeLeft05(): Bool =
        let v = Vector.takeLeft(0, Vector.singleton(1));
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def takeLeft06(): Bool =
        let v = Vector.takeLeft(1, Vector.singleton(1));
        Vector.sameElements(v, Vector.singleton(1))

    @test
    def takeLeft07(): Bool =
        let v = Vector.takeLeft(2, Vector.singleton(1));
        Vector.sameElements(v, Vector.singleton(1))

    @test
    def takeLeft08(): Bool =
        let v = Vector.takeLeft(0, vectorOf2(1, 2));
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def takeLeft09(): Bool =
        let v = Vector.takeLeft(1, vectorOf2(1, 2));
        Vector.sameElements(v, Vector.singleton(1))

    @test
    def takeLeft10(): Bool =
        let v = Vector.takeLeft(2, vectorOf2(1, 2));
        Vector.sameElements(v, vectorOf2(1, 2))

    @test
    def takeLeft11(): Bool =
        let v = Vector.takeLeft(2, Array#{1,2,3,4,5,6} @ r);
        Vector.sameElements(v, vectorOf2(1, 2))

    @test
    def takeLeft12(): Bool =
        let v = Vector.takeLeft(4, Array#{1,2,3,4,5,6} @ r);
        Vector.sameElements(v, Array#{1,2,3,4} @ r)

    /////////////////////////////////////////////////////////////////////////////
    // takeRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def takeRight01(): Bool =
        let v = Vector.takeRight(-1, Vector.empty(): Vector[Int32]);
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def takeRight02(): Bool =
        let v = Vector.takeRight(0, Vector.empty(): Vector[Int32]);
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def takeRight03(): Bool =
        let v = Vector.takeRight(1, Vector.empty(): Vector[Int32]);
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def takeRight04(): Bool =
        let v = Vector.takeRight(-1, Vector.singleton(1));
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def takeRight05(): Bool =
        let v = Vector.takeRight(0, Vector.singleton(1));
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def takeRight06(): Bool =
        let v = Vector.takeRight(1, Vector.singleton(1));
        Vector.sameElements(v, Vector.singleton(1))

    @test
    def takeRight07(): Bool =
        let v = Vector.takeRight(2, Vector.singleton(1));
        Vector.sameElements(v, Vector.singleton(1))

    @test
    def takeRight08(): Bool =
        let v = Vector.takeRight(0, vectorOf2(1, 2));
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def takeRight09(): Bool =
        let v = Vector.takeRight(1, vectorOf2(1, 2));
        Vector.sameElements(v, Vector.singleton(2))

    @test
    def takeRight10(): Bool =
        let v = Vector.takeRight(2, vectorOf2(1, 2));
        Vector.sameElements(v, vectorOf2(1, 2))

    @test
    def takeRight11(): Bool =
        let v = Vector.takeRight(2, Array#{1,2,3,4,5,6} @ r);
        Vector.sameElements(v, vectorOf2(5, 6))

    @test
    def takeRight12(): Bool =
        let v = Vector.takeRight(4, Array#{1,2,3,4,5,6} @ r);
        Vector.sameElements(v, Array#{3,4,5,6} @ r)

    /////////////////////////////////////////////////////////////////////////////
    // takeWhile                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def takeWhile01(): Bool =
        let v = Vector.takeWhile(i -> i > 3, Vector.empty(): Vector[Int32]);
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def takeWhile02(): Bool =
        let v = Vector.takeWhile(i -> i > 3, Vector.singleton(1));
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def takeWhile03(): Bool =
        let v = Vector.takeWhile(i -> i > 3, Vector.singleton(4));
        Vector.sameElements(v, Vector.singleton(4))

    @test
    def takeWhile04(): Bool =
        let v = Vector.takeWhile(i -> i > 3, vectorOf2(1, 2));
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def takeWhile05(): Bool =
        let v = Vector.takeWhile(i -> i > 3, Array#{1,5} @ r);
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def takeWhile06(): Bool =
        let v = Vector.takeWhile(i -> i > 3, Array#{5,1} @ r);
        Vector.sameElements(v, Vector.singleton(5))

    @test
    def takeWhile07(): Bool =
        let v = Vector.takeWhile(i -> i > 3, Array#{5,8} @ r);
        Vector.sameElements(v, Array#{5,8} @ r)

    @test
    def takeWhile08(): Bool =
        let v = Vector.takeWhile(i -> i > 3, Array#{4, 6, -3, 11, -5, 1, 2, 16, 7, 1, 7} @ r);
        Vector.sameElements(v, Array#{4, 6} @ r)

    /////////////////////////////////////////////////////////////////////////////
    // takeWhileLeft                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def takeWhileLeft01(): Bool =
        let v = Vector.takeWhileLeft(i -> i > 3, Vector.empty(): Vector[Int32]);
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def takeWhileLeft02(): Bool =
        let v = Vector.takeWhileLeft(i -> i > 3, Vector.singleton(1));
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def takeWhileLeft03(): Bool =
        let v = Vector.takeWhileLeft(i -> i > 3, Vector.singleton(4));
        Vector.sameElements(v, Vector.singleton(4))

    @test
    def takeWhileLeft04(): Bool =
        let v = Vector.takeWhileLeft(i -> i > 3, vectorOf2(1, 2));
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def takeWhileLeft05(): Bool =
        let v = Vector.takeWhileLeft(i -> i > 3, Array#{1,5} @ r);
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def takeWhileLeft06(): Bool =
        let v = Vector.takeWhileLeft(i -> i > 3, Array#{5,1} @ r);
        Vector.sameElements(v, Vector.singleton(5))

    @test
    def takeWhileLeft07(): Bool =
        let v = Vector.takeWhileLeft(i -> i > 3, Array#{5,8} @ r);
        Vector.sameElements(v, Array#{5,8} @ r)

    @test
    def takeWhileLeft08(): Bool =
        let v = Vector.takeWhileLeft(i -> i > 3, Array#{4, 6, -3, 11, -5, 1, 2, 16, 7, 1, 7} @ r);
        Vector.sameElements(v, Array#{4, 6} @ r)

    /////////////////////////////////////////////////////////////////////////////
    // takeWhileRight                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def takeWhileRight01(): Bool =
        let v = Vector.takeWhileRight(i -> i > 3, Vector.empty(): Vector[Int32]);
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def takeWhileRight02(): Bool =
        let v = Vector.takeWhileRight(i -> i > 3, Vector.singleton(1));
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def takeWhileRight03(): Bool =
        let v = Vector.takeWhileRight(i -> i > 3, Vector.singleton(4));
        Vector.sameElements(v, Vector.singleton(4))

    @test
    def takeWhileRight04(): Bool =
        let v = Vector.takeWhileRight(i -> i > 3, vectorOf2(1, 2));
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def takeWhileRight05(): Bool =
        let v = Vector.takeWhileRight(i -> i > 3, Array#{1,5} @ r);
        Vector.sameElements(v, Vector.singleton(5))

    @test
    def takeWhileRight06(): Bool =
        let v = Vector.takeWhileRight(i -> i > 3, Array#{5,1} @ r);
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def takeWhileRight07(): Bool =
        let v = Vector.takeWhileRight(i -> i > 3, Array#{5,8} @ r);
        Vector.sameElements(v, Array#{5,8} @ r)

    @test
    def takeWhileRight08(): Bool =
        let v = Vector.takeWhileRight(i -> i > 3, Array#{4, 6, -3, 11, -5, 1, 2, 16, 7, 1, 7} @ r);
        Vector.sameElements(v, Array#{7} @ r)

    /////////////////////////////////////////////////////////////////////////////
    // groupBy                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def groupBy01(): Bool =
        let arr = Vector.groupBy((a, b) -> a > 3 or b > 8, Vector.empty(): Vector[Int32]);
        let xs = Vector.toList(arr) |> List.map(Vector.toList);
        xs == Nil

    @test
    def groupBy02(): Bool =
        let arr = Vector.groupBy((a, b) -> a > 3 or b > 8, Vector.singleton(1));
        let xs = Vector.toList(arr) |> List.map(Vector.toList);
        xs == (1 :: Nil) :: Nil

    @test
    def groupBy03(): Bool =
        let arr = Vector.groupBy((a, b) -> a > 3 or b > 8, vectorOf2(1, 4));
        let xs = Vector.toList(arr) |> List.map(Vector.toList);
        xs == (1 :: Nil) :: (4 :: Nil) :: Nil

    @test
    def groupBy04(): Bool =
        let arr = Vector.groupBy((a, b) -> a > 3 or b > 8, vectorOf2(1, 9));
        let xs = Vector.toList(arr) |> List.map(Vector.toList);
        xs == (1 :: 9 :: Nil) :: Nil

    @test
    def groupBy05(): Bool =
        let arr = Vector.groupBy((a, b) -> a > 3 or b > 8, Array#{1, 4, 7, 6, 9, 2, 4, 4, 8, 16} @ r);
        let xs = Vector.toList(arr) |> List.map(Vector.toList);
        xs == (1 :: 9 :: 16 :: Nil) :: (4 :: 7 :: 6 :: 4 :: 4 :: 8 :: Nil) :: (2 :: Nil) :: Nil

    @test
    def groupBy06(): Bool =
        let arr = Vector.groupBy((a, b) -> a > -6 or a*b >= 0, Array#{-1, -11, 4, -11, 0, 8, 2, 1, -3, -24} @ r);
        let xs = Vector.toList(arr) |> List.map(Vector.toList);
        xs == (-1 :: -11 :: -11 :: 0 :: -3 :: -24 :: Nil) :: (4 :: 8 :: 2 :: 1 :: Nil) :: Nil

    @test
    def groupBy07(): Bool =
        let arr = Vector.groupBy((a, b) -> a < 0 or (a > 10 or (b > 10 or a == b)), Array#{-5, 6, 11, 8, 8, -11, -1, 0, 4, -1} @ r);
        let xs = Vector.toList(arr) |> List.map(Vector.toList);
        xs == (-5 :: 11 :: -11 :: -1 :: -1 :: Nil) :: (6 :: Nil) :: (8 :: 8 :: Nil) :: (0 :: Nil) :: (4 :: Nil) :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // zip                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def zip01(): Bool =
        let v = Vector.zip(Vector.empty(): Vector[Int32], Vector.empty(): Vector[Int32]);
        Vector.sameElements(v, (Vector.empty()): Array[(Int32, Int32), _])

    @test
    def zip02(): Bool =
        let v = Vector.zip(Vector.singleton(1), Vector.empty(): Vector[Int32]);
        Vector.sameElements(v, (Vector.empty()): Array[(Int32, Int32), _])

    @test
    def zip03(): Bool =
        let v = Vector.zip(Vector.empty(): Vector[Int32], Vector.singleton(2));
        Vector.sameElements(v, (Vector.empty()): Array[(Int32, Int32), _])

    @test
    def zip04(): Bool =
        let v = Vector.zip(Vector.singleton(1), Vector.singleton(2));
        Vector.sameElements(v, Array#{(1, 2)} @ r)

    @test
    def zip05(): Bool =
        let v = Vector.zip(vectorOf2(1, 3), vectorOf2(2, 4) @ r);
        Vector.sameElements(v, Array#{(1, 2), (3, 4)} @ r)

    @test
    def zip06(): Bool =
        let v = Vector.zip(vectorOf3(1, 3, 5), Array#{2,4,6} @ r);
        Vector.sameElements(v, Array#{(1, 2), (3, 4), (5, 6)} @ r)

    @test
    def zip07(): Bool =
        let v = Vector.zip(Array#{1,3,5,7} @ r, Array#{2,4,6,8} @ r);
        Vector.sameElements(v, Array#{(1, 2), (3, 4), (5, 6), (7, 8)} @ r)

    /////////////////////////////////////////////////////////////////////////////
    // zipWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def zipWith01(): Bool =
        let arr = Vector.zipWith((a, b) -> if (b) a + 1 else a, Vector.empty(): Vector[Int32], Vector.empty(): Vector[Bool]);
        Vector.sameElements(vrr, Vector.empty(): Vector[Int32])

    @test
    def zipWith02(): Bool =
        let arr = Vector.zipWith((a, b) -> if (b) a + 1 else a, Vector.singleton(1), Vector.empty(): Vector[Bool]);
        Vector.sameElements(vrr, Vector.empty(): Vector[Int32])

    @test
    def zipWith03(): Bool =
        let arr = Vector.zipWith((a, b) -> if (b) a + 1 else a, Vector.empty(): Vector[Int32], Vector.singleton(true));
        Vector.sameElements(vrr, Vector.empty(): Vector[Int32])

    @test
    def zipWith04(): Bool =
        let arr = Vector.zipWith((a, b) -> if (b) a + 1 else a, Vector.singleton(1), Vector.singleton(true));
        Vector.sameElements(vrr, Vector.singleton(2))

    @test
    def zipWith05(): Bool =
        let arr = Vector.zipWith((a, b) -> if (b) a + 1 else a, Vector.singleton(1), Vector.singleton(false));
        Vector.sameElements(vrr, Vector.singleton(1))

    @test
    def zipWith06(): Bool =
        let arr = Vector.zipWith((a, b) -> if (b) a + 1 else a,
                        Array#{1, 2, 3, 4, 5, 6, 7, 8} @ r,
                        Array#{false, true, true, false, false, true, true, true} @ r);
        Vector.sameElements(vrr, Array#{1, 3, 4, 4, 5, 7, 8, 9} @ r)

    /////////////////////////////////////////////////////////////////////////////
    // unzip                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def unzip01(): Bool =
        let (a,b) = Vector.unzip(r, (Vector.empty()): Array[(Unit, Unit), _]);
        Vector.sameElements(v, Vector.empty()) and Vector.sameElements(b, Vector.empty())

    @test
    def unzip02(): Bool =
        let (a,b) = Vector.unzip(r, Array#{(1, true)} @ r);
        Vector.sameElements(v, Vector.singleton(1)) and Vector.sameElements(b, Vector.singleton(true))

    @test
    def unzip03(): Bool =
        let (a,b) = Vector.unzip(r, Array#{(1, true), (2, true)} @ r);
        Vector.sameElements(v, vectorOf2(1, 2))  and Vector.sameElements(b, vectorOf2(true, true))

    @test
    def unzip04(): Bool =
        let (a,b) = Vector.unzip(r, Array#{(1, true), (2, true), (3, false)} @ r);
        Vector.sameElements(v, vectorOf3(1, 2, 3)) and Vector.sameElements(b, Array#{true,true,false} @ r)

    @test
    def unzip05(): Bool =
        unzip05Aux(r1, Array#{(1, "1"), (2, "2"), (3, "3")} @ r2) `Vector.sameElements` (vectorOf3(1, 2, 3))

    def unzip05Aux(r1: Region[r1], a: Array[(a, b), r2]): Array[a, r1] \ { Write(r1), Read(r2) } =
        region r3 {
            // snd is local, fst is for caller
            Vector.unzip(r1, r3, a) |> fst
        }

    /////////////////////////////////////////////////////////////////////////////
    // fold2                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def fold201(): Bool =
        Vector.fold2((c, a, b) -> if (b) a + c else a * c, 4, Vector.empty(): Vector[Int32], Vector.empty(): Vector[Bool]) == 4

    @test
    def fold202(): Bool =
        Vector.fold2((c, a, b) -> if (b) a + c else a * c, 4, Vector.singleton(1), Vector.empty()) == 4

    @test
    def fold203(): Bool =
        Vector.fold2((c, a, b) -> if (b) a + c else a * c, 4, Vector.empty(), Vector.singleton(true)) == 4

    @test
    def fold204(): Bool =
        Vector.fold2((c, a, b) -> if (b) a + c else a * c, 4, Vector.singleton(2), Vector.singleton(true)) == 6

    @test
    def fold205(): Bool =
        Vector.fold2((c, a, b) -> if (b) a + c else a * c, 4, Vector.singleton(2), Vector.singleton(false)) == 8

    @test
    def fold206(): Bool =
        Vector.fold2((c, a, b) -> if (b) a + c else a * c, 4, Array#{3,2} @ r, vectorOf2(true, true)) == 9

    @test
    def fold207(): Bool =
        Vector.fold2((c, a, b) -> if (b) a + c else a * c, 4, Array#{3,2} @ r, vectorOf2(true, false)) == 14

    @test
    def fold208(): Bool =
        Vector.fold2((c, a, b) -> if (b) a + c else a * c, 4, Array#{3,2} @ r, vectorOf2(false, true)) == 14

    @test
    def fold209(): Bool =
        Vector.fold2((c, a, b) -> if (b) a + c else a * c, 4, Array#{3,2} @ r, vectorOf2(false, false)) == 24

    @test
    def fold210(): Bool =
        Vector.fold2((c, a, b) -> if (b) a + c else a * c, 4, Array#{6,-4,3,2} @ r,
            Array#{true,false,false,true} @ r) == -118

    /////////////////////////////////////////////////////////////////////////////
    // foldLeft2                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldLeft201(): Bool =
        Vector.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, Vector.empty(): Vector[Int32], Vector.empty(): Vector[Bool]) == 4

    @test
    def foldLeft202(): Bool =
        Vector.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, Vector.singleton(1), Vector.empty(): Vector[Bool]) == 4

    @test
    def foldLeft203(): Bool =
        Vector.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, Vector.empty(): Vector[Int32], Vector.singleton(true)) == 4

    @test
    def foldLeft204(): Bool =
        Vector.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, Vector.singleton(2), Vector.singleton(true)) == 6

    @test
    def foldLeft205(): Bool =
        Vector.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, Vector.singleton(2), Vector.singleton(false)) == 8

    @test
    def foldLeft206(): Bool =
        Vector.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, Array#{3,2} @ r, vectorOf2(true, true)) == 9

    @test
    def foldLeft207(): Bool =
        Vector.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, Array#{3,2} @ r, vectorOf2(true, false)) == 14

    @test
    def foldLeft208(): Bool =
        Vector.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, Array#{3,2} @ r, vectorOf2(false, true)) == 14

    @test
    def foldLeft209(): Bool =
        Vector.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, Array#{3,2} @ r, vectorOf2(false, false)) == 24

    @test
    def foldLeft210(): Bool =
        Vector.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, Array#{6,-4,3,2} @ r,
            Array#{true,false,false,true} @ r) == -118

    /////////////////////////////////////////////////////////////////////////////
    // foldRight2                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldRight201(): Bool =
        Vector.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, Vector.empty(): Vector[Int32], Vector.empty(): Vector[Bool]) == 4

    @test
    def foldRight202(): Bool =
        Vector.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, Vector.singleton(1), Vector.empty(): Vector[Bool]) == 4

    @test
    def foldRight203(): Bool =
        Vector.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, Vector.empty(): Vector[Int32], Vector.singleton(true)) == 4

    @test
    def foldRight204(): Bool =
        Vector.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, Vector.singleton(2), Vector.singleton(true)) == 6

    @test
    def foldRight205(): Bool =
        Vector.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, Vector.singleton(2), Vector.singleton(false)) == 8

    @test
    def foldRight206(): Bool =
        Vector.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, Array#{3,2} @ r, vectorOf2(true, true)) == 9

    @test
    def foldRight207(): Bool =
        Vector.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, Array#{3,2} @ r, vectorOf2(true, false)) == 11

    @test
    def foldRight208(): Bool =
        Vector.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, Array#{3,2} @ r, vectorOf2(false, true)) == 18

    @test
    def foldRight209(): Bool =
        Vector.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, Array#{3,2} @ r, vectorOf2(false, false)) == 24

    @test
    def foldRight210(): Bool =
        Vector.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, Array#{6,-4,3,2} @ r,
            Array#{true,false,false,true} @ r) == -66

    /////////////////////////////////////////////////////////////////////////////
    // filterMap                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filterMap01(): Bool =
        let v = Vector.filterMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Vector.empty(): Vector[Int32]);
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def filterMap02(): Bool =
        let v = Vector.filterMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Vector.singleton(1));
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def filterMap03(): Bool =
        let v = Vector.filterMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Vector.singleton(2));
        Vector.sameElements(v, Vector.singleton(1))

    @test
    def filterMap04(): Bool =
        let v = Vector.filterMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, vectorOf2(1, 3));
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def filterMap05(): Bool =
        let v = Vector.filterMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, vectorOf2(1, 4));
        Vector.sameElements(v, Vector.singleton(2))

    @test
    def filterMap06(): Bool =
        let v = Vector.filterMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Array#{6,-1} @ r);
        Vector.sameElements(v, Vector.singleton(3))

    @test
    def filterMap07(): Bool =
        let v = Vector.filterMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Array#{8,6} @ r);
        Vector.sameElements(v, Array#{4,3} @ r)

    @test
    def filterMap08(): Bool =
        let v = Vector.filterMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Array#{0,1,2,3,4,5,10} @ r);
        Vector.sameElements(v, Array#{0,1,2,5} @ r)

    /////////////////////////////////////////////////////////////////////////////
    // findMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def findMap01(): Bool =
        Vector.findMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Vector.empty(): Vector[Int32]) == None

    @test
    def findMap02(): Bool =
        Vector.findMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Vector.singleton(1)) == None

    @test
    def findMap03(): Bool =
        Vector.findMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Vector.singleton(2)) == Some(1)

    @test
    def findMap04(): Bool =
        Vector.findMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, vectorOf2(1, 3)) == None

    @test
    def findMap05(): Bool =
        Vector.findMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, vectorOf2(1, 4)) == Some(2)

    @test
    def findMap06(): Bool =
        Vector.findMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Array#{6,-1} @ r) == Some(3)

    @test
    def findMap07(): Bool =
        Vector.findMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Array#{8,6} @ r) == Some(4)

    @test
    def findMap08(): Bool =
        Vector.findMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Array#{0,1,2,3,4,5,10} @ r) == Some(0)

    /////////////////////////////////////////////////////////////////////////////
    // toSet                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toSet01(): Bool =
        Vector.toSet(Vector.empty(): Vector[Int32]) == Set#{}

    @test
    def toSet02(): Bool =
        Vector.toSet(Vector.singleton(1)) == Set#{1}

    @test
    def toSet03(): Bool =
        Vector.toSet(vectorOf2(1, 2)) == Set#{1, 2}

    @test
    def toSet04(): Bool =
        Vector.toSet(vectorOf2(1,1)) == Set#{1}

    @test
    def toSet05(): Bool =
        Vector.toSet(vectorOf3(1, 2, 3)) == Set#{1, 2, 3}

    @test
    def toSet06(): Bool =
        Vector.toSet(Array#{1,2,1} @ r) == Set#{1, 2}

    @test
    def toSet07(): Bool =
        Vector.toSet(Array#{1,1,2} @ r) == Set#{1, 2}

    @test
    def toSet08(): Bool =
        Vector.toSet(Array#{2,1,2,3,4,5,6,3,3} @ r) == Set#{1, 2, 3, 4, 5, 6}

    /////////////////////////////////////////////////////////////////////////////
    // toMap                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toMap01(): Bool =
        Vector.toMap((Vector.empty()): Array[(Int32, Bool), _]) == Map#{}

    @test
    def toMap02(): Bool =
        Vector.toMap(Array#{(1, true)} @ r) == Map#{1 => true}

    @test
    def toMap03(): Bool =
        Vector.toMap(Array#{(1, true), (2, false)} @ r) == Map#{1 => true, 2 => false}

    @test
    def toMap04(): Bool =
        Vector.toMap(Array#{(1, true), (1, false)} @ r) == Map#{1 => true}

    /////////////////////////////////////////////////////////////////////////////
    // findIndexOf                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def findIndexOf01(): Bool =
        Vector.findIndexOf(i -> i > 2, Vector.empty(): Vector[Int32]) == None

    @test
    def findIndexOf02(): Bool =
        Vector.findIndexOf(i -> i > 2, Vector.singleton(1)) == None

    @test
    def findIndexOf03(): Bool =
        Vector.findIndexOf(i -> i > 2, Vector.singleton(3)) == Some(0)

    @test
    def findIndexOf04(): Bool =
        Vector.findIndexOf(i -> i > 2, vectorOf2(1, 2)) == None

    @test
    def findIndexOf05(): Bool =
        Vector.findIndexOf(i -> i > 2, vectorOf2(6, -6)) == Some(0)

    @test
    def findIndexOf06(): Bool =
        Vector.findIndexOf(i -> i > 2, vectorOf2(-6, 6)) == Some(1)

    @test
    def findIndexOf07(): Bool =
        Vector.findIndexOf(i -> i > 2, vectorOf2(6, 7)) == Some(0)

    /////////////////////////////////////////////////////////////////////////////
    // findIndexOfLeft                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def findIndexOfLeft01(): Bool =
        Vector.findIndexOfLeft(i -> i > 2, Vector.empty(): Vector[Int32]) == None

    @test
    def findIndexOfLeft02(): Bool =
        Vector.findIndexOfLeft(i -> i > 2, Vector.singleton(1)) == None

    @test
    def findIndexOfLeft03(): Bool =
        Vector.findIndexOfLeft(i -> i > 2, Vector.singleton(3)) == Some(0)

    @test
    def findIndexOfLeft04(): Bool =
        Vector.findIndexOfLeft(i -> i > 2, vectorOf2(1, 2)) == None

    @test
    def findIndexOfLeft05(): Bool =
        Vector.findIndexOfLeft(i -> i > 2, vectorOf2(6, -6)) == Some(0)

    @test
    def findIndexOfLeft06(): Bool =
        Vector.findIndexOfLeft(i -> i > 2, vectorOf2(-6, 6)) == Some(1)

    @test
    def findIndexOfLeft07(): Bool =
        Vector.findIndexOfLeft(i -> i > 2, vectorOf2(6, 7)) == Some(0)

    /////////////////////////////////////////////////////////////////////////////
    // findIndexOfRight                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def findIndexOfRight01(): Bool =
        Vector.findIndexOfRight(i -> i > 2, Vector.empty(): Vector[Int32]) == None

    @test
    def findIndexOfRight02(): Bool =
        Vector.findIndexOfRight(i -> i > 2, Vector.singleton(1)) == None

    @test
    def findIndexOfRight03(): Bool =
        Vector.findIndexOfRight(i -> i > 2, Vector.singleton(3)) == Some(0)

    @test
    def findIndexOfRight04(): Bool =
        Vector.findIndexOfRight(i -> i > 2, vectorOf2(1, 2)) == None

    @test
    def findIndexOfRight05(): Bool =
        Vector.findIndexOfRight(i -> i > 2, vectorOf2(6, -6)) == Some(0)

    @test
    def findIndexOfRight06(): Bool =
        Vector.findIndexOfRight(i -> i > 2, vectorOf2(-6, 6)) == Some(1)

    @test
    def findIndexOfRight07(): Bool =
        Vector.findIndexOfRight(i -> i > 2, vectorOf2(6, 7)) == Some(1)

    /////////////////////////////////////////////////////////////////////////////
    // findIndices                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def findIndices01(): Bool =
        let v = Vector.findIndices(i -> i > 2, Vector.empty(): Vector[Int32]);
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def findIndices02(): Bool =
        let v = Vector.findIndices(i -> i > 2, Vector.singleton(1));
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def findIndices03(): Bool =
        let v = Vector.findIndices(i -> i > 2, Vector.singleton(3));
        Vector.sameElements(v, Vector.empty())

    @test
    def findIndices04(): Bool =
        let v = Vector.findIndices(i -> i > 2, vectorOf2(1, 2));
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def findIndices05(): Bool =
        let v = Vector.findIndices(i -> i > 2, vectorOf2(6, -6));
        Vector.sameElements(v, Vector.empty())

    @test
    def findIndices06(): Bool =
        let v = Vector.findIndices(i -> i > 2, vectorOf2(-6, 6));
        Vector.sameElements(v, Vector.singleton(1))

    @test
    def findIndices07(): Bool =
        let v = Vector.findIndices(i -> i > 2, vectorOf2(6, 7));
        Vector.sameElements(v, Array#{0,1} @ r)

    /////////////////////////////////////////////////////////////////////////////
    // init                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def init01(): Bool =
        let v = Vector.init(x -> x, 0);
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def init02(): Bool =
        let v = Vector.init(x -> x, -1);
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def init03(): Bool =
        let v = Vector.init(x -> x, 1);
        Vector.sameElements(v, Vector.empty())

    @test
    def init04(): Bool =
        let v = Vector.init(x -> x, 2);
        Vector.sameElements(v, Array#{0,1} @ r)

    @test
    def init05(): Bool =
        let v = Vector.init(x -> x, 50);
        Vector.length(a) == 50

    /////////////////////////////////////////////////////////////////////////////
    // sameElements                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sameElements01(): Bool =
        let a = Vector.empty(): Vector[Int32];
        let b = Vector.empty(): Vector[Int32];
        Vector.sameElements(v,b) == true

    @test
    def sameElements02(): Bool =
        let a = (Vector.singleton(1)): Vector[Int32];
        let b = Vector.empty(): Vector[Int32];
        Vector.sameElements(v,b) == false

    @test
    def sameElements03(): Bool =
        let a = Vector.empty(): Vector[Int32];
        let b = (Vector.singleton(1)): Vector[Int32];
        Vector.sameElements(v,b) == false

    @test
    def sameElements04(): Bool =
        let a = (Vector.singleton(1)): Vector[Int32];
        let b = (Vector.singleton(1)): Vector[Int32];
        Vector.sameElements(v,b) == true

    @test
    def sameElements05(): Bool =
        let a = (Vector.singleton(1)): Vector[Int32];
        let b = Vector.empty(): Vector[Int32];
        Vector.sameElements(v,b) == false

    @test
    def sameElements06(): Bool =
        let a = (vectorOf2(1, 2)): Vector[Int32];
        let b = (vectorOf2(1, 2)): Vector[Int32];
        Vector.sameElements(v,b) == true

    @test
    def sameElements07(): Bool =
        let a = (vectorOf2(1, 2)): Vector[Int32];
        let b = (vectorOf2(2, 1)): Vector[Int32];
        Vector.sameElements(v,b) == false

    /////////////////////////////////////////////////////////////////////////////
    // forEach                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def forEach01(): Bool =
        let a = Vector.empty(): Vector[Int32];
        let sb = new StringBuilder(r);
        let fn = x -> if (x > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        Vector.forEach(fn, a);
        StringBuilder.toString(sb) == ""

    @test
    def forEach02(): Bool =
        let v = Vector.empty();
        let sb = new StringBuilder(r);
        let fn = x -> if (x > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        Vector.forEach(fn, a);
        StringBuilder.toString(sb) == "F"

    @test
    def forEach03(): Bool =
        let v = Vector.singleton(1);
        let sb = new StringBuilder(r);
        let fn = x -> if (x > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        Vector.forEach(fn, a);
        StringBuilder.toString(sb) == "T"

    @test
    def forEach04(): Bool =
        let a = Array#{0,1} @ r;
        let sb = new StringBuilder(r);
        let fn = x -> if (x > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        Vector.forEach(fn, a);
        StringBuilder.toString(sb) == "FT"

    /////////////////////////////////////////////////////////////////////////////
    // forEachWithIndex                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def forEachWithIndex01(): Bool =
        let a = (Vector.empty()): Array[Float32, _];
        let sb = new StringBuilder(r);
        let fn = (ix, _) -> if (ix > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        Vector.forEachWithIndex(fn, a);
        StringBuilder.toString(sb) == ""

    @test
    def forEachWithIndex02(): Bool =
        let a = Array#{0.0} @ r;
        let sb = new StringBuilder(r);
        let fn = (ix, _) -> if (ix > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        Vector.forEachWithIndex(fn, a);
        StringBuilder.toString(sb) == "F"

    @test
    def forEachWithIndex03(): Bool =
        let a = Array#{0.0, 0.1} @ r;
        let sb = new StringBuilder(r);
        let fn = (ix, _) -> if (ix > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        Vector.forEachWithIndex(fn, a);
        StringBuilder.toString(sb) == "FT"

    @test
    def forEachWithIndex04(): Bool =
        let a = Array#{0.0, 0.1, 0.2} @ r;
        let sb = new StringBuilder(r);
        let fn = (ix, _) -> if (ix > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        Vector.forEachWithIndex(fn, a);
        StringBuilder.toString(sb) == "FTT"

    @test
    def forEachWithIndex05(): Bool =
        let a = Array#{0.0, 0.1, 0.2, 0.3} @ r;
        let sb = new StringBuilder(r);
        let fn = (ix, _) -> if (ix > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        Vector.forEachWithIndex(fn, a);
        StringBuilder.toString(sb) == "FTTT"

    /////////////////////////////////////////////////////////////////////////////
    // updateSequence                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def updateSequence01(): Bool =
         let v = Vector.updateSequence(0, Vector.empty(): Vector[Int32], Vector.empty(): Vector[Int32]);
         Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def updateSequence02(): Bool =
        let v = Vector.updateSequence(0, vectorOf2(1, 2), Vector.empty());
        Vector.sameElements(v, Vector.empty(): Vector[Int32])

    @test
    def updateSequence03(): Bool =
        let v = Vector.updateSequence(0, Vector.empty(), vectorOf2(1, 2));
        Vector.sameElements(v, vectorOf2(1, 2))

    @test
    def updateSequence04(): Bool =
        let v = Vector.updateSequence(-3, vectorOf3(1, 2, 4), vectorOf2(1, 2));
        Vector.sameElements(v, vectorOf2(1, 2))

    @test
    def updateSequence05(): Bool =
        let v = Vector.updateSequence(2, vectorOf3(1, 2, 4), vectorOf2(1, 2));
        Vector.sameElements(v, vectorOf2(1, 2))

    @test
    def updateSequence06(): Bool =
        let v = Vector.updateSequence(0, Vector.empty(), Vector.singleton(1));
        Vector.sameElements(v, Vector.singleton(1))

    @test
    def updateSequence07(): Bool =
        let v = Vector.updateSequence(1, Vector.singleton(2), Vector.singleton(1));
        Vector.sameElements(v, Vector.singleton(1))

    @test
    def updateSequence08(): Bool =
        let v = Vector.updateSequence(0, Vector.singleton(2), Vector.singleton(1));
        Vector.sameElements(v, Vector.singleton(2))

    @test
    def updateSequence09(): Bool =
        let v = Vector.updateSequence(0, vectorOf2(2, 4) @ r, Vector.singleton(1));
        Vector.sameElements(v, Vector.singleton(2))

    @test
    def updateSequence10(): Bool =
        let v = Vector.updateSequence(-1, vectorOf2(2, 4) @ r, Vector.singleton(1));
        Vector.sameElements(v, Vector.singleton(4))

    @test
    def updateSequence11(): Bool =
        let v = Vector.updateSequence(-1, vectorOf2(3, 4), vectorOf2(1, 2));
        Vector.sameElements(v, vectorOf2(4, 2) @ r)

    @test
    def updateSequence12(): Bool =
        let v = Vector.updateSequence(1, vectorOf2(3, 4), vectorOf2(1, 2));
        Vector.sameElements(v, vectorOf2(1, 3))

    @test
    def updateSequence13(): Bool =
        let v = Vector.updateSequence(-2, vectorOf2(3, 4), vectorOf2(1, 2));
        Vector.sameElements(v, vectorOf2(1, 2))

    @test
    def updateSequence14(): Bool =
        let v = Vector.updateSequence(2, vectorOf2(3, 4), vectorOf2(1, 2));
        Vector.sameElements(v, vectorOf2(1, 2))

    @test
    def updateSequence15(): Bool =
        let v = Vector.updateSequence(1, Vector.singleton(3), vectorOf2(1, 2));
        Vector.sameElements(v, vectorOf2(1, 3))

    @test
    def updateSequence16(): Bool =
        let v = Vector.updateSequence(0, vectorOf2(3, 4), vectorOf2(1, 2));
        Vector.sameElements(v, vectorOf2(3, 4))

    @test
    def updateSequence17(): Bool =
        let v = Vector.updateSequence(0, Vector.singleton(4), vectorOf3(1, 2, 3));
        Vector.sameElements(v, Array#{4, 2, 3} @ r)

    @test
    def updateSequence18(): Bool =
        let v = Vector.updateSequence(1, Vector.singleton(4), vectorOf3(1, 2, 3));
        Vector.sameElements(v, Array#{1, 4, 3} @ r)

    @test
    def updateSequence19(): Bool =
        let v = Vector.updateSequence(2, Vector.singleton(4), vectorOf3(1, 2, 3));
        Vector.sameElements(v, Array#{1, 2, 4} @ r)

    @test
    def updateSequence20(): Bool =
        let v = Vector.updateSequence(0, Array#{4, 5} @ r, vectorOf3(1, 2, 3));
        Vector.sameElements(v, Array#{4, 5, 3} @ r)

    @test
    def updateSequence21(): Bool =
        let v = Vector.updateSequence(1, Array#{4, 5} @ r, vectorOf3(1, 2, 3));
        Vector.sameElements(v, Array#{1, 4, 5} @ r)

    @test
    def updateSequence22(): Bool =
        let v = Vector.updateSequence(-1, vectorOf3(4, 5, 6) vectorOf3(1, 2, 3));
        Vector.sameElements(v, Array#{5, 6, 3} @ r)

    @test
    def updateSequence23(): Bool =
        let v = Vector.updateSequence(0, vectorOf3(4, 5, 6) vectorOf3(1, 2, 3));
        Vector.sameElements(v, Array#{4, 5, 6} @ r)

    @test
    def updateSequence24(): Bool =
        let v = Vector.updateSequence(2, Array#{14, 15, 16, 17} @ r, Array#{1, 2, 3, 4, 5, 6, 7} @ r);
        Vector.sameElements(v, Array#{1, 2, 14, 15, 16, 17, 7} @ r)

    @test
    def updateSequence25(): Bool =
        let v = Vector.updateSequence(-2, Array#{14, 15, 16, 17} @ r, Array#{1, 2, 3, 4, 5, 6, 7} @ r);
        Vector.sameElements(v, Array#{16, 17, 3, 4, 5, 6, 7} @ r)

    @test
    def updateSequence26(): Bool =
        let v = Vector.updateSequence(4, Array#{14, 15, 16, 17} @ r, Array#{1, 2, 3, 4, 5, 6, 7} @ r);
        Vector.sameElements(v, Array#{1, 2, 3, 4, 14, 15, 16} @ r)

    @test
    def updateSequence27(): Bool =
        let v = Vector.updateSequence(4, Array#{14, 15} @ r, Array#{1, 2, 3, 4, 5, 6, 7} @ r);
        Vector.sameElements(v, Array#{1, 2, 3, 4, 14, 15, 7} @ r)

    @test
    def updateSequence28(): Bool =
        let v = Vector.updateSequence(-1, Array#{-1, -2, -3, -4, -5, -6, -7, -8} @ r, Array#{1, 2, 3, 4, 5, 6, 7} @ r);
        Vector.sameElements(v, Array#{-2, -3, -4, -5, -6, -7, -8} @ r)

    /////////////////////////////////////////////////////////////////////////////
    // sortWith                                                                //
    /////////////////////////////////////////////////////////////////////////////

    def cmp(x: Int32, y: Int32): Comparison =
        if (x < y) Comparison.LessThan
        else if (x == y) Comparison.EqualTo
        else Comparison.GreaterThan

    @test
    def sortWith01(): Bool =
        let v = Vector.sortWith(cmp, Vector.empty(): Vector[Int32]);
        a `sameElements` Vector.empty(): Vector[Int32]

    @test
    def sortWith02(): Bool =
        let v = Vector.sortWith(cmp, Vector.empty());
        a `sameElements` Vector.empty()

    @test
    def sortWith03(): Bool =
        let v = Vector.sortWith(cmp, Array#{0,1} @ r);
        a `sameElements` Array#{0,1} @ r

    @test
    def sortWith04(): Bool =
        let v = Vector.sortWith(cmp, Array#{1,0} @ r);
        a `sameElements` Array#{0,1} @ r

    @test
    def sortWith05(): Bool =
        let v = Vector.sortWith(cmp, vectorOf2(1,1));
        a `sameElements` vectorOf2(1,1)

    @test
    def sortWith06(): Bool =
        let v = Vector.sortWith(cmp, Array#{0,1,2,3,4,5} @ r);
        a `sameElements` Array#{0,1,2,3,4,5} @ r

    @test
    def sortWith07(): Bool =
        let v = Vector.sortWith(cmp, Array#{5,4,3,2,1,0} @ r);
        a `sameElements` Array#{0,1,2,3,4,5} @ r

    @test
    def sortWith08(): Bool =
        let v = Vector.sortWith(cmp, Array#{5,3,0,4,1,2} @ r);
        a `sameElements` Array#{0,1,2,3,4,5} @ r

    @test
    def sortWith09(): Bool =
        let v = Vector.sortWith(cmp, Array#{2,3,0,4,1,2} @ r);
        a `sameElements` Array#{0,1,2,2,3,4} @ r

    @test
    def sortWith10(): Bool =
        let v = Vector.sortWith(flip(cmp), Array#{0,1,2,3,4,5} @ r);
        a `sameElements` Array#{5,4,3,2,1,0} @ r

    @test
    def sortWith11(): Bool =
        let v = Vector.sortWith(flip(cmp), Array#{5,4,3,2,1,0} @ r);
        a `sameElements` Array#{5,4,3,2,1,0} @ r

    @test
    def sortWith12(): Bool =
        let v = Vector.sortWith(flip(cmp), Array#{5,3,0,4,1,2} @ r);
        a `sameElements` Array#{5,4,3,2,1,0} @ r

    @test
    def sortWith13(): Bool =
        let v = Vector.sortWith(flip(cmp), Array#{2,3,0,4,1,2} @ r);
        a `sameElements` Array#{4,3,2,2,1,0} @ r

    /////////////////////////////////////////////////////////////////////////////
    // sort                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    def testSortVsSortWith(a: Vector[Int32]): Bool =
        Vector.sort(a) `sameElements` Vector.sortWith(cmp, a)


    @test
    def sort01(): Bool =
        testSortVsSortWith(Vector.empty(): Vector[Int32])

    @test
    def sort02(): Bool =
        testSortVsSortWith(Vector.empty())

    @test
    def sort03(): Bool =
        testSortVsSortWith(Array#{0,1} @ r)

    @test
    def sort04(): Bool =
        testSortVsSortWith(Array#{1,0} @ r)

    @test
    def sort05(): Bool =
        testSortVsSortWith(vectorOf2(1,1))

    @test
    def sort06(): Bool =
        testSortVsSortWith(Array#{0,1,2,3,4,5} @ r)

    @test
    def sort07(): Bool =
        testSortVsSortWith(Array#{5,4,3,2,1,0} @ r)

    @test
    def sort08(): Bool =
        testSortVsSortWith(Array#{5,3,0,4,1,2} @ r)

    @test
    def sort09(): Bool =
        testSortVsSortWith(Array#{2,3,0,4,1,2} @ r)

    /////////////////////////////////////////////////////////////////////////////
    // sortBy                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    def testSortByVsSort(a: Vector[Int32]): Bool =
        (Vector.sortBy(identity, a) `sameElements` Vector.sort(a)) and
        (Vector.sortBy(x -> 4 * x + 7, a) `sameElements` Vector.sort(a)) and
        (Vector.sortBy(x -> -x, a) `sameElements` Vector.sortWith(flip(cmp), a))

    @test
    def sortBy01(): Bool =
        testSortByVsSort(Vector.empty(): Vector[Int32])

    @test
    def sortBy02(): Bool =
        testSortByVsSort(Vector.empty())

    @test
    def sortBy03(): Bool =
        testSortByVsSort(Array#{0,1} @ r)

    @test
    def sortBy04(): Bool =
        testSortByVsSort(Array#{1,0} @ r)

    @test
    def sortBy05(): Bool =
        testSortByVsSort(vectorOf2(1,1))

    @test
    def sortBy06(): Bool =
        testSortByVsSort(Array#{0,1,2,3,4,5} @ r)

    @test
    def sortBy07(): Bool =
        testSortByVsSort(Array#{5,4,3,2,1,0} @ r)

    @test
    def sortBy08(): Bool =
        testSortByVsSort(Array#{5,3,0,4,1,2} @ r)

    @test
    def sortBy09(): Bool =
        testSortByVsSort(Array#{2,3,0,4,1,2} @ r)

    enum R {
        case R({i = Int32, s = String})
    }

    instance Eq[R] {
        pub def eq(a: R, b: R): Bool =
            let R.R(x) = a;
            let R.R(y) = b;
            x.i == y.i and x.s == y.s

    @test
    def sortBy10(): Bool = region reg {
        Vector.sortBy(reg, r -> let R.R(x) = r; x.i, Array#{R.R({i = 2, s = "A"}), R.R({i = 1, s = "B"}), R.R({i = 3, s = "C"})} @ reg)
        `sameElements` Array#{R.R({i = 1, s = "B"}), R.R({i = 2, s = "A"}), R.R({i = 3, s = "C"})} @

    /////////////////////////////////////////////////////////////////////////////
    // minimumBy                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def minimumBy01(): Bool =
        Vector.minimumBy((x, y) -> x <=> y, Vector.empty(): Vector[Int32]) == None

    @test
    def minimumBy02(): Bool =
        Vector.minimumBy((x, y) -> x <=> y, Vector.range(0, 5)) == Some(0)

    @test
    def minimumBy03(): Bool =
        Vector.minimumBy((x, y) -> x <=> y, Vector.range(4, 10)) == Some(4)

    @test
    def minimumBy04(): Bool =
        let v = Vector.range(10, 20);
        a[5] = -22;
        Vector.minimumBy((x, y) -> x <=> y, a) == Some(-22)

    /////////////////////////////////////////////////////////////////////////////
    // maximumBy                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def maximumBy01(): Bool =
        Vector.maximumBy((x, y) -> x <=> y, Vector.empty(): Vector[Int32]) == None

    @test
    def maximumBy02(): Bool =
        Vector.maximumBy((x, y) -> x <=> y, Vector.range(0, 5)) == Some(4)

    @test
    def maximumBy03(): Bool =
        Vector.maximumBy((x, y) -> x <=> y, Vector.range(4, 10)) == Some(9)

    @test
    def maximumBy04(): Bool =
        let v = Vector.range(10, 20);
        a[5] = 42;
        Vector.maximumBy((x, y) -> x <=> y, a) == Some(42)

    /////////////////////////////////////////////////////////////////////////////
    // sum                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sum01(): Bool =
        Vector.sum(Vector.empty()) == 0

    @test
    def sum02(): Bool =
        Vector.sum(Vector.singleton(1)) == 1

    @test
    def sum03(): Bool =
        Vector.sum(vectorOf3(1, 2, 3)) == 6

    @test
    def sum04(): Bool =
        Vector.sum(Array#{1, 2, 3, -3} @ r) == 3

    @test
    def sum05(): Bool =
        Vector.sum(Array#{-1, -2, -3, -4} @ r) == -10

    @test
    def sum06(): Bool =
        Vector.sum(Array#{10, -10} @ r) == 0

    @test
    def sum07(): Bool =
        Vector.range(1, 101) |> Vector.sum == 5050

    /////////////////////////////////////////////////////////////////////////////
    // sumWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sumWith01(): Bool =
        Vector.sumWith(x -> x + 1, Vector.empty()) == 0

    @test
    def sumWith02(): Bool =
        Vector.sumWith(x -> x + 1, Vector.singleton(1)) == 2

    @test
    def sumWith03(): Bool =
        Vector.sumWith(x -> x + 1, vectorOf3(1, 2, 3)) == 9

    @test
    def sumWith04(): Bool =
        Vector.sumWith(x -> x + 1, Array#{1, 2, 3, -3} @ r) == 7

    @test
    def sumWith05(): Bool =
        Vector.sumWith(x -> x + 1, Array#{-1, -2, -3, -4} @ r) == -6

    @test
    def sumWith06(): Bool =
        Vector.sumWith(x -> x + 1, Array#{10, -10} @ r) == 2


    /////////////////////////////////////////////////////////////////////////////
    // toDelayList                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toDelayList01(): Bool =
        (Vector.empty()): Array[Unit, _] |> Vector.toDelayList == DelayList.empty()

    @test
    def toDelayList02(): Bool =
        (Vector.new(0): Array[String, r]) |> Vector.toDelayList == DelayList.empty()

    @test
    def toDelayList03(): Bool =
        Vector.range(-1000, 1000) |> Vector.toDelayList == DelayList.range(-1000, 1000)

    @test
    def toDelayList04(): Bool =
        Vector.range(-1000, 1000) |> Vector.toDelayList |> DelayList.toArray(r) `sameElements` Vector.range(-1000, 1000)

    /////////////////////////////////////////////////////////////////////////////
    // toChain                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toChain01(): Bool =
        Vector.toChain((Vector.empty()): Array[Unit, _]) == Chain.empty(): Chain[Unit]

    @test
    def toChain02(): Bool =
        Vector.toChain(Vector.singleton(1)) == Chain.singleton(1)

    @test
    def toChain03(): Bool =
        Vector.toChain(vectorOf2(1, 2)) == List.toChain(1 :: 2 :: Nil)

    @test
    def toChain04(): Bool =
        Vector.toChain(vectorOf3(1, 2, 3)) == List.toChain(1 :: 2 :: 3 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // toNec                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toNec01(): Bool =
        Vector.toNec((Vector.empty()): Array[Unit, _]) == None

    @test
    def toNec02(): Bool =
        Vector.toNec(Vector.singleton(1)) == Some(Nec.singleton(1))

    @test
    def toNec03(): Bool =
        Vector.toNec(vectorOf2(1, 2)) == List.toNec(1 :: 2 :: Nil)

    @test
    def toNec04(): Bool =
        Vector.toNec(vectorOf3(1, 2, 3)) == List.toNec(1 :: 2 :: 3 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // iterator                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def iterator01(): Bool =
        Vector.empty(): Vector[Int32] |> Vector.iterator(r) |> Iterator.toList == Nil

    @test
    def iterator02(): Bool =
        (vectorOf3(1, 2, 3)) |> Vector.iterator(r) |> Iterator.toList == 1 :: 2 :: 3 :: Nil

    @test
    def iterator03(): Bool =
        (vectorOf3(1, 2, 3)) |> Vector.iterator(r) |> Iterator.toArray(r) `sameElements` vectorOf3(1, 2, 3)

    @test
    def iterator04(): Bool =
        Vector.range(-100, 100) |> Vector.iterator(r) |> Iterator.toArray(r) `sameElements` Vector.range(-100, 100)


    /////////////////////////////////////////////////////////////////////////////
    // join                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def join01(): Bool =
        Vector.join(",", Vector.empty(): Vector[Int32]) == ""

    @test
    def join02(): Bool =
        Vector.join(",", Vector.singleton(1)) == "1"

    @test
    def join03(): Bool =
        Vector.join(",", vectorOf3(1, 2, 3)) == "1,2,3"

    @test
    def join04(): Bool =
        Vector.join(",", vectorOf3("1", "2", "3")) == "1,2,3"

    /////////////////////////////////////////////////////////////////////////////
    // joinWith                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def joinWith01(): Bool =
        Vector.joinWith(x -> "${x + 1}", ",", Vector.empty()) == ""

    @test
    def joinWith02(): Bool =
        Vector.joinWith(x -> "${x + 1}", ",", Vector.singleton(1)) == "2"

    @test
    def joinWith03(): Bool =
        Vector.joinWith(x -> "${x + 1}", ",", vectorOf3(1, 2, 3)) == "2,3,4"

    @test
    def joinWith04(): Bool =
        Vector.joinWith(x -> x + x, ",", Array#{"1", "2", "3"} @ r) == "11,22,33"

    @test
    def joinWith05(): Bool =
        let a: Vector[Int32] = Vector.empty();
        let s = Vector.joinWith(Int32.toString, ",", a);
        s == ""

    @test
    def joinWith06(): Bool =
        let v = Vector.singleton(1);
        let s = Vector.joinWith(Int32.toString, ",", a);
        s == "1"

    @test
    def joinWith07(): Bool =
        let v = VectorOf2(1, 2);
        let s = Vector.joinWith(Int32.toString, ",", a);
        s == "1,2"

    @test
    def joinWith08(): Bool =
        let v = VectorOf3(1, 2, 3);
        let s = Vector.joinWith(Int32.toString, ",", a);
        s == "1,2,3"

    @test
    def joinWith09(): Bool =
        let v = VectorOf3(1, 2, 3);
        let s = Vector.joinWith(Int32.toString, "", a);
        s == "123"

    @test
    def joinWith10(): Bool =
        let v = VectorOf3(1, 2, 3);
        let s = Vector.joinWith(Int32.toString, "..", a);
        s == "1..2..3"


    /////////////////////////////////////////////////////////////////////////////
    // toMutList                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toMutList01(): Bool = region rc {
        MutList.sameElements(Vector.toMutList(Vector.empty(): Vector[Int32]), new MutList(rc))
    }

    @test
    def toMutList02(): Bool = region rc {
        let v = new MutList(rc);
        MutList.push!(1, v);
        MutList.sameElements(Vector.toMutList(Vector.singleton(1)), v)

    @test
    def toMutList03(): Bool = region rc {
        let v = new MutList(rc);
        MutList.push!(1, v);
        MutList.push!(2, v);
        MutList.push!(3, v);
        MutList.sameElements(Vector.toMutList(vectorOf3(1, 2, 3)), v)

    @test
    def toMutList04(): Bool = region rc {
        MutList.sameElements((Vector.range(0, 100) |> Vector.toMutList(rc)), MutList.range(0, 100))
    }

    @test
    def toMutList05(): Bool = region rc {
        let v = Vector.toMutList(vectorOf3(1, 2, 3));
        MutList.push!(4, v);
        Vector.sameElements(MutList.toArray(v), Array#{1, 2, 3, 4} @ r)
    }

    @test
    def toMutList06(): Bool = region rc {
        let v = Vector.toMutList(vectorOf3(1, 2, 3));
        MutList.push!(4, v);
        discard MutList.pop!(v);
        discard MutList.pop!(v);
        discard MutList.pop!(v);
        discard MutList.pop!(v);
        Vector.sameElements(MutList.toArray(v), Vector.empty())
    }

    /////////////////////////////////////////////////////////////////////////////
    // shuffle                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def shuffle01(): Bool \ NonDet = region r {
        let a = Vector.empty(): Vector[Int32];
        Vector.shuffle(Random.new(), a);
        Vector.length(a) == 0 and Vector.toSet(a) == Set#{}

    @test
    def shuffle02(): Bool \ NonDet = region r {
        let a = (Vector.empty());
        Vector.shuffle(Random.new(), a);
        Vector.length(a) == 1 and Vector.toSet(a) == Set#{0}

    @test
    def shuffle03(): Bool \ NonDet = region r {
        let a = (Array#{0, 1, 2, 3} @ r);
        Vector.shuffle(Random.new(), a);
        Vector.length(a) == 4 and Vector.toSet(a) == Set#{0, 1, 2, 3}

    @test
    def shuffle04(): Bool \ NonDet = region r {
        let a = (Array#{0, 1, 2, 3, 4, 5, 6, 7, 8, 9} @ r);
        Vector.shuffle(Random.new(), a);
        Vector.length(a) == 10 and Vector.toSet(a) == Set#{0, 1, 2, 3, 4, 5, 6, 7, 8, 9}
*/
}

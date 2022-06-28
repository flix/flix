/*
 * Copyright 2019 Magnus Madsen, Stephen Tetley
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

namespace TestArray {

    use Array.sameElements;

    /////////////////////////////////////////////////////////////////////////////
    // toString                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toString01(): Bool = region r {
        Array.toString(([] @ r): Array[Int32, _]) == "[]"
    }

    @test
    def toString02(): Bool = region r {
        Array.toString(([1] @ r)) == "[1]"
    }

    @test
    def toString03(): Bool = region r {
        Array.toString(([1, 2, 3, 4] @ r)) == "[1, 2, 3, 4]"
    }

    @test
    def toString04(): Bool = region r {
        Array.toString((['a', 'b', 'c', 'd'] @ r)) == "[a, b, c, d]"
    }

    /////////////////////////////////////////////////////////////////////////////
    // new                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testNew01(): Bool = region r {
        Array.new(42, 10, r)[0] == 42
    }

    @test
    def testNew02(): Bool = region r {
        Array.new(42, 10, r)[5] == 42
    }

    @test
    def testNew03(): Bool = region r {
        Array.new(42, 10, r)[9] == 42
    }

    /////////////////////////////////////////////////////////////////////////////
    // get                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testGet01(): Bool = region r {
        Array.get(0, [1, 2, 3] @ r) == 1
    }

    @test
    def testGet02(): Bool = region r {
        Array.get(1, [1, 2, 3] @ r) == 2
    }

    @test
    def testGet03(): Bool = region r {
        Array.get(2, [1, 2, 3] @ r) == 3
    }

    /////////////////////////////////////////////////////////////////////////////
    // put                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testPut01(): Bool = region r {
        let a = ([1] @ r) !> Array.put(42, 0);
        a[0] == 42
    }

    @test
    def testPut02(): Bool = region r {
        let a = ([1, 2] @ r) !> Array.put(42, 1);
        a[1] == 42
    }

    @test
    def testPut03(): Bool = region r {
        let a = ([1, 2, 3] @ r) !> Array.put(42, 2);
        a[2] == 42
    }

    /////////////////////////////////////////////////////////////////////////////
    // isEmpty                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testIsEmpty01(): Bool = region r {
        Array.isEmpty([])
    }

    @test
    def testIsEmpty02(): Bool = region r {
        not Array.isEmpty([1])
    }

    @test
    def testIsEmpty03(): Bool = region r {
        not Array.isEmpty([1, 2, 3])
    }

    /////////////////////////////////////////////////////////////////////////////
    // length                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testLength01(): Bool = region r {
        Array.length([]) == 0
    }

    @test
    def testLength02(): Bool = region r {
        Array.length([1]) == 1
    }

    @test
    def testLength03(): Bool = region r {
        Array.length([1, 2, 3]) == 3
    }

    /////////////////////////////////////////////////////////////////////////////
    // slice                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testSlice01(): Bool = region r {
        Array.slice(0, 1, [1, 2, 3])[0] == 1
    }

    @test
    def testSlice02(): Bool = region r {
        Array.slice(1, 2, [1, 2, 3])[0] == 2
    }

    @test
    def testSlice03(): Bool = region r {
        Array.slice(2, 3, [1, 2, 3])[0] == 3
    }

    /////////////////////////////////////////////////////////////////////////////
    // toList                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testToList01(): Bool = region r {
        Array.toList([]: Array[Unit, _]) == Nil
    }

    @test
    def testToList02(): Bool = region r {
        Array.toList([1]) == 1 :: Nil
    }

    @test
    def testToList03(): Bool = region r {
        Array.toList([1, 2]) == 1 :: 2 :: Nil
    }

    /////////////////////////////////////////////////////////////////////////////
    // toMutDeque                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toMutDeque01(): Bool = region r {
        let a = ([] @ r): Array[Int32, _];
        let d = new MutDeque(r): MutDeque[Int32, _];

        Array.toMutDeque(a, r) `MutDeque.sameElements` d
    }

    @test
    def toMutDeque02(): Bool = region r {
        let a = ([1] @ r);

        let d = new MutDeque(r);
        MutDeque.pushBack(1, d);

        Array.toMutDeque(a, r) `MutDeque.sameElements` d
    }

    @test
    def toMutDeque03(): Bool = region r {
        let a = ([1, 2, 3] @ r);

        let d = new MutDeque(r);
        MutDeque.pushBack(1, d);
        MutDeque.pushBack(2, d);
        MutDeque.pushBack(3, d);

        Array.toMutDeque(a, r) `MutDeque.sameElements` d
    }

    @test
    def toMutDeque04(): Bool = region r {
        let a = ([1, 2, 3, 4, 5] @ r);

        let d = new MutDeque(r);
        MutDeque.pushBack(3, d);
        MutDeque.pushBack(4, d);
        MutDeque.pushBack(5, d);
        MutDeque.pushFront(2, d);
        MutDeque.pushFront(1, d);

        Array.toMutDeque(a, r) `MutDeque.sameElements` d
    }

    @test
    def toMutDeque05(): Bool = region r {
        let a = (['a', 'b', 'c'] @ r);

        let d = new MutDeque(r);
        MutDeque.pushFront('c', d);
        MutDeque.pushFront('b', d);
        MutDeque.pushFront('a', d);

        Array.toMutDeque(a, r) `MutDeque.sameElements` d
    }

    /////////////////////////////////////////////////////////////////////////////
    // head                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def head01(): Bool = region r {
        Array.head([]: Array[Int32, _]) == None
    }

    @test
    def head02(): Bool = region r {
        Array.head([1]) == Some(1)
    }

    @test
    def head03(): Bool = region r {
        Array.head([2,1]) == Some(2)
    }

    @test
    def head04(): Bool = region r {
        Array.head([3,2,1]) == Some(3)
    }

    /////////////////////////////////////////////////////////////////////////////
    // last                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def last01(): Bool = region r {
        Array.last([]): Option[Unit] == None
    }

    @test
    def last02(): Bool = region r {
        Array.last([1]) == Some(1)
    }

    @test
    def last03(): Bool = region r {
        Array.last([1,2]) == Some(2)
    }

    @test
    def last04(): Bool = region r {
        Array.last([1,2,3]) == Some(3)
    }

    /////////////////////////////////////////////////////////////////////////////
    // append                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def append01(): Bool = region r {
        let a = Array.append([]: Array[Int32, _], []: Array[Int32, _]);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def append02(): Bool = region r {
        let a = Array.append([]: Array[Int32, _], [1]);
        Array.sameElements(a, [1])
    }

    @test
    def append03(): Bool = region r {
        let a = Array.append([]: Array[Int32, _], [1,2]);
        Array.sameElements(a, [1,2])
    }

    @test
    def append04(): Bool = region r {
        let a = Array.append([1], []: Array[Int32, _]);
        Array.sameElements(a, [1])
    }

    @test
    def append05(): Bool = region r {
        let a = Array.append([1,2], []: Array[Int32, _]);
        Array.sameElements(a, [1,2])
    }

    @test
    def append06(): Bool = region r {
        let a = Array.append([1], [2]);
        Array.sameElements(a, [1,2])
    }

    @test
    def append07(): Bool = region r {
        let a = Array.append([1,2], [3]);
        Array.sameElements(a, [1,2,3])
    }

    @test
    def append08(): Bool = region r {
        let a = Array.append([1], [2,3]);
        Array.sameElements(a, [1,2,3])
    }

    /////////////////////////////////////////////////////////////////////////////
    // memberOf                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def memberOf01(): Bool = region r {
        Array.memberOf(0, []: Array[Int32, _]) == false
    }

    @test
    def memberOf02(): Bool = region r {
        Array.memberOf(0, [1]) == false
    }

    @test
    def memberOf03(): Bool = region r {
        Array.memberOf(0, [0]) == true
    }

    @test
    def memberOf04(): Bool = region r {
        Array.memberOf(0, [1,2]) == false
    }

    @test
    def memberOf05(): Bool = region r {
        Array.memberOf(1, [1,2]) == true
    }

    @test
    def memberOf06(): Bool = region r {
        Array.memberOf(2, [1,2]) == true
    }

    @test
    def memberOf07(): Bool = region r {
        Array.memberOf(3, [1,2]) == false
    }

    /////////////////////////////////////////////////////////////////////////////
    // indexOf                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def indexOf01(): Bool = region r {
        Array.indexOf(0, []: Array[Int32, _]) == None
    }

    @test
    def indexOf02(): Bool = region r {
        Array.indexOf(0, [1]) == None
    }

    @test
    def indexOf03(): Bool = region r {
        Array.indexOf(1, [1]) == Some(0)
    }

    @test
    def indexOf04(): Bool = region r {
        Array.indexOf(0, [1,2]) == None
    }

    @test
    def indexOf05(): Bool = region r {
        Array.indexOf(1, [1,2]) == Some(0)
    }

    @test
    def indexOf06(): Bool = region r {
        Array.indexOf(2, [1,2]) == Some(1)
    }

    /////////////////////////////////////////////////////////////////////////////
    // indexOfLeft                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def indexOfLeft01(): Bool = region r {
        Array.indexOfLeft(0, []: Array[Int32, _]) == None
    }

    @test
    def indexOfLeft02(): Bool = region r {
        Array.indexOfLeft(0, [1]) == None
    }

    @test
    def indexOfLeft03(): Bool = region r {
        Array.indexOfLeft(1, [1]) == Some(0)
    }

    @test
    def indexOfLeft04(): Bool = region r {
        Array.indexOfLeft(0, [1,2]) == None
    }

    @test
    def indexOfLeft05(): Bool = region r {
        Array.indexOfLeft(1, [1,2]) == Some(0)
    }

    @test
    def indexOfLeft06(): Bool = region r {
        Array.indexOfLeft(2, [1,2]) == Some(1)
    }

    @test
    def indexOfLeft07(): Bool = region r {
        Array.indexOfLeft(1, [1,1]) == Some(0)
    }

    /////////////////////////////////////////////////////////////////////////////
    // indexOfRight                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def indexOfRight01(): Bool = region r {
        Array.indexOfRight(0, []: Array[Int32, _]) == None
    }

    @test
    def indexOfRight02(): Bool = region r {
        Array.indexOfRight(0, [1]) == None
    }

    @test
    def indexOfRight03(): Bool = region r {
        Array.indexOfRight(1, [1]) == Some(0)
    }

    @test
    def indexOfRight04(): Bool = region r {
        Array.indexOfRight(0, [1,2]) == None
    }

    @test
    def indexOfRight05(): Bool = region r {
        Array.indexOfRight(1, [1,2]) == Some(0)
    }

    @test
    def indexOfRight06(): Bool = region r {
        Array.indexOfRight(2, [1,2]) == Some(1)
    }

    @test
    def indexOfRight07(): Bool = region r {
        Array.indexOfRight(1, [1,1]) == Some(1)
    }

    /////////////////////////////////////////////////////////////////////////////
    // indices                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def indices01(): Bool = region r {
        let a = Array.indices(0, []: Array[Int32, r]);
        Array.sameElements(a, []: Array[Int32, r])
    }

    @test
    def indices02(): Bool = region r {
        let a = Array.indices(0, [1]);
        Array.sameElements(a, []: Array[Int32, r])
    }

    @test
    def indices03(): Bool = region r {
        let a = Array.indices(1, [1]);
        Array.sameElements(a, [0])
    }

    @test
    def indices04(): Bool = region r {
        let a = Array.indices(0, [1,2]);
        Array.sameElements(a, []: Array[Int32, r])
    }

    @test
    def indices05(): Bool = region r {
        let a = Array.indices(1, [1,2]);
        Array.sameElements(a, [0])
    }

    @test
    def indices06(): Bool = region r {
        let a = Array.indices(2, [1,2]);
        Array.sameElements(a, [1])
    }

    @test
    def indices07(): Bool = region r {
        let a = Array.indices(1, [1,1]);
        Array.sameElements(a, [0,1])
    }
    /////////////////////////////////////////////////////////////////////////////
    // find                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def find01(): Bool = region r {
        Array.find(i -> i > 2, []) == None
    }

    @test
    def find02(): Bool = region r {
        Array.find(i -> i > 2, [1]) == None
    }

    @test
    def find03(): Bool = region r {
        Array.find(i -> i > 2, [3]) == Some(3)
    }

    @test
    def find04(): Bool = region r {
        Array.find(i -> i > 2, [1,2]) == None
    }

    @test
    def find05(): Bool = region r {
        Array.find(i -> i > 2, [6,-6]) == Some(6)
    }

    @test
    def find06(): Bool = region r {
        Array.find(i -> i > 2, [-6,6]) == Some(6)
    }

    @test
    def find07(): Bool = region r {
        Array.find(i -> i > 2, [6,7]) == Some(6)
    }

    /////////////////////////////////////////////////////////////////////////////
    // findLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def findLeft01(): Bool = region r {
        Array.findLeft(i -> i > 2, []) == None
    }

    @test
    def findLeft02(): Bool = region r {
        Array.findLeft(i -> i > 2, [1]) == None
    }

    @test
    def findLeft03(): Bool = region r {
        Array.findLeft(i -> i > 2, [3]) == Some(3)
    }

    @test
    def findLeft04(): Bool = region r {
        Array.findLeft(i -> i > 2, [1,2]) == None
    }

    @test
    def findLeft05(): Bool = region r {
        Array.findLeft(i -> i > 2, [6,-6]) == Some(6)
    }

    @test
    def findLeft06(): Bool = region r {
        Array.findLeft(i -> i > 2, [-6,6]) == Some(6)
    }

    @test
    def findLeft07(): Bool = region r {
        Array.findLeft(i -> i > 2, [6,7]) == Some(6)
    }

    /////////////////////////////////////////////////////////////////////////////
    // findRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def findRight01(): Bool = region r {
        Array.findRight(i -> i > 2, []) == None
    }

    @test
    def findRight02(): Bool = region r {
        Array.findRight(i -> i > 2, [1]) == None
    }

    @test
    def findRight03(): Bool = region r {
        Array.findRight(i -> i > 2, [3]) == Some(3)
    }

    @test
    def findRight04(): Bool = region r {
        Array.findRight(i -> i > 2, [1,2]) == None
    }

    @test
    def findRight05(): Bool = region r {
        Array.findRight(i -> i > 2, [6,-6]) == Some(6)
    }

    @test
    def findRight06(): Bool = region r {
        Array.findRight(i -> i > 2, [-6,6]) == Some(6)
    }

    @test
    def findRight07(): Bool = region r {
        Array.findRight(i -> i > 2, [6,7]) == Some(7)
    }

    /////////////////////////////////////////////////////////////////////////////
    // range                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def range01(): Bool = region r {
        let a = Array.range(1, 0, r);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def range02(): Bool = region r {
        let a = Array.range(1, 1, r);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def range03(): Bool = region r {
        let a = Array.range(1, 2, r);
        Array.sameElements(a, [1])
    }

    @test
    def range04(): Bool = region r {
        let a = Array.range(1, 3, r);
        Array.sameElements(a, [1,2])
    }

    @test
    def range05(): Bool = region r {
        let a = Array.range(1, 4, r);
        Array.sameElements(a, [1,2,3])
    }

    @test
    def range06(): Bool = region r {
        let a = Array.range(-1, 3, r);
        Array.sameElements(a, [-1,0,1,2])
    }

    /////////////////////////////////////////////////////////////////////////////
    // repeat                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def repeat01(): Bool = region r {
        let a = Array.repeat(-1, 1, r);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def repeat02(): Bool = region r {
        let a = Array.repeat(0, 1, r);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def repeat03(): Bool = region r {
        let a = Array.repeat(1, 1, r);
        Array.sameElements(a, [1])
    }

    @test
    def repeat04(): Bool = region r {
        let a = Array.repeat(2, 1, r);
        Array.sameElements(a, [1,1])
    }

    @test
    def repeat05(): Bool = region r {
        let a = Array.repeat(3, 1, r);
        Array.sameElements(a, [1,1,1])
    }

    /////////////////////////////////////////////////////////////////////////////
    // scan                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def scan01(): Bool = region r {
        let a = Array.scan((i, b) -> if (b) i + 1 else i + 2, 1, []);
        Array.sameElements(a, [1])
    }

    @test
    def scan02(): Bool = region r {
        let a = Array.scan((i, b) -> if (b) i + 1 else i + 2, 1, [false]);
        Array.sameElements(a, [1,3])
    }

    @test
    def scan03(): Bool = region r {
        let a = Array.scan((i, b) -> if (b) i + 1 else i + 2, 1, [true]);
        Array.sameElements(a, [1,2])
    }

    @test
    def scan04(): Bool = region r {
        let a = Array.scan((i, b) -> if (b) i + 1 else i + 2, 1, [false,false]);
        Array.sameElements(a, [1,3,5])
    }

    @test
    def scan05(): Bool = region r {
        let a = Array.scan((i, b) -> if (b) i + 1 else i + 2, 1, [false,true]);
        Array.sameElements(a, [1,3,4])
    }

    @test
    def scan06(): Bool = region r {
        let a = Array.scan((i, b) -> if (b) i + 1 else i + 2, 1, [true,false]);
        Array.sameElements(a, [1,2,4])
    }

    @test
    def scan07(): Bool = region r {
        let a = Array.scan((i, b) -> if (b) i + 1 else i + 2, 1, [true,true]);
        Array.sameElements(a, [1,2,3])
    }

    /////////////////////////////////////////////////////////////////////////////
    // scanLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def scanLeft01(): Bool = region r {
        let a = Array.scanLeft((i, b) -> if (b) i + 1 else i + 2, 1, []: Array[Bool, _]);
        Array.sameElements(a, [1])
    }

    @test
    def scanLeft02(): Bool = region r {
        let a = Array.scanLeft((i, b) -> if (b) i + 1 else i + 2, 1, [false]);
        Array.sameElements(a, [1,3])
    }

    @test
    def scanLeft03(): Bool = region r {
        let a = Array.scanLeft((i, b) -> if (b) i + 1 else i + 2, 1, [true]);
        Array.sameElements(a, [1,2])
    }

    @test
    def scanLeft04(): Bool = region r {
        let a = Array.scanLeft((i, b) -> if (b) i + 1 else i + 2, 1, [false,false]);
        Array.sameElements(a, [1,3,5])
    }

    @test
    def scanLeft05(): Bool = region r {
        let a = Array.scanLeft((i, b) -> if (b) i + 1 else i + 2, 1, [false,true]);
        Array.sameElements(a, [1,3,4])
    }

    @test
    def scanLeft06(): Bool = region r {
        let a = Array.scanLeft((i, b) -> if (b) i + 1 else i + 2, 1, [true,false]);
        Array.sameElements(a, [1,2,4])
    }

    @test
    def scanLeft07(): Bool = region r {
        let a = Array.scanLeft((i, b) -> if (b) i + 1 else i + 2, 1, [true,true]);
        Array.sameElements(a, [1,2,3])
    }

    /////////////////////////////////////////////////////////////////////////////
    // scanRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def scanRight01(): Bool = region r {
        let a = Array.scanRight((b, i) -> if (b) i + 1 else i + 2, 1, []);
        Array.sameElements(a, [1])
    }

    @test
    def scanRight02(): Bool = region r {
        let a = Array.scanRight((b, i) -> if (b) i + 1 else i + 2, 1, [false]);
        Array.sameElements(a, [3,1])
    }

    @test
    def scanRight03(): Bool = region r {
        let a = Array.scanRight((b, i) -> if (b) i + 1 else i + 2, 1, [true]);
        Array.sameElements(a, [2,1])
    }

    @test
    def scanRight04(): Bool = region r {
        let a = Array.scanRight((b, i) -> if (b) i + 1 else i + 2, 1, [false,false]);
        Array.sameElements(a, [5,3,1])
    }

    @test
    def scanRight05(): Bool = region r {
        let a = Array.scanRight((b, i) -> if (b) i + 1 else i + 2, 1, [false,true]);
        Array.sameElements(a, [4,2,1])
    }

    @test
    def scanRight06(): Bool = region r {
        let a = Array.scanRight((b, i) -> if (b) i + 1 else i + 2, 1, [true,false]);
        Array.sameElements(a, [4,3,1])
    }

    @test
    def scanRight07(): Bool = region r {
        let a = Array.scanRight((b, i) -> if (b) i + 1 else i + 2, 1, [true,true]);
        Array.sameElements(a, [3,2,1])
    }

    /////////////////////////////////////////////////////////////////////////////
    // map                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def map01(): Bool = region r {
        let a = Array.map(i -> i > 2, []);
        Array.sameElements(a, []: Array[Bool, _])
    }

    @test
    def map02(): Bool = region r {
        let a = Array.map(i -> i > 2, [1]);
        Array.sameElements(a, [false])
    }

    @test
    def map03(): Bool = region r {
        let a = Array.map(i -> i > 2, [3]);
        Array.sameElements(a, [true])
    }

    @test
    def map04(): Bool = region r {
        let a = Array.map(i -> i > 2, [1,2]);
        Array.sameElements(a, [false,false])
    }

    @test
    def map05(): Bool = region r {
        let a = Array.map(i -> i > 2, [1,8]);
        Array.sameElements(a, [false, true])
    }

    @test
    def map06(): Bool = region r {
        let a = Array.map(i -> i > 2, [8,1]);
        Array.sameElements(a, [true,false])
    }

    @test
    def map07(): Bool = region r {
        let a = Array.map(i -> i > 2, [7,8]);
        Array.sameElements(a, [true,true])
    }

    /////////////////////////////////////////////////////////////////////////////
    // transform!                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def transform01!(): Bool = region r {
        let a: Array[Int32, _] = [];
        Array.transform!(x -> x+1, a);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def transform02!(): Bool = region r {
        let a = [0];
        Array.transform!(x -> x+1, a);
        Array.sameElements(a, [1])
    }

    @test
    def transform03!(): Bool = region r {
        let a = [0,1];
        Array.transform!(x -> x+1, a);
        Array.sameElements(a, [1,2])
    }

    @test
    def transform04!(): Bool = region r {
        let a = [0,1,2];
        Array.transform!(x -> x+1, a);
        Array.sameElements(a, [1,2,3])
    }

    /////////////////////////////////////////////////////////////////////////////
    // mapWithIndex                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapWithIndex01(): Bool = region r {
        let a = Array.mapWithIndex((e, i) -> if (i < 1) e > 2 else e <= 2, []);
        Array.sameElements(a, []: Array[Bool, _])
    }

    @test
    def mapWithIndex02(): Bool = region r {
        let a = Array.mapWithIndex((e, i) -> if (i < 1) e > 2 else e <= 2, [1]);
        Array.sameElements(a, [false])
    }

    @test
    def mapWithIndex03(): Bool = region r {
        let a = Array.mapWithIndex((e, i) -> if (i < 1) e > 2 else e <= 2, [3]);
        Array.sameElements(a, [true])
    }

    @test
    def mapWithIndex04(): Bool = region r {
        let a = Array.mapWithIndex((e, i) -> if (i < 1) e > 2 else e <= 2, [1,2]);
        Array.sameElements(a, [false,true])
    }

    @test
    def mapWithIndex05(): Bool = region r {
        let a = Array.mapWithIndex((e, i) -> if (i < 1) e > 2 else e <= 2, [1,8]);
        Array.sameElements(a, [false,false])
    }

    @test
    def mapWithIndex06(): Bool = region r {
        let a = Array.mapWithIndex((e, i) -> if (i < 1) e > 2 else e <= 2, [8,1]);
        Array.sameElements(a, [true,true])
    }

    @test
    def mapWithIndex07(): Bool = region r {
        let a = Array.mapWithIndex((e, i) -> if (i < 1) e > 2 else e <= 2, [7,8]);
        Array.sameElements(a, [true,false])
    }

    /////////////////////////////////////////////////////////////////////////////
    // transformWithIndex!                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def transformWithIndex01!(): Bool = region r {
        let a = []: Array[Int32, _];
        Array.transformWithIndex!((e, i) -> if (i < 1) e + 10 else e, a);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def transformWithIndex02!(): Bool = region r {
        let a = [1];
        Array.transformWithIndex!((e, i) -> if (i < 1) e + 10 else e, a);
        Array.sameElements(a, [11])
    }

    @test
    def transformWithIndex03!(): Bool = region r {
        let a = [3];
        Array.transformWithIndex!((e, i) -> if (i < 1) e + 10 else e, a);
        Array.sameElements(a, [13])
    }

    @test
    def transformWithIndex04!(): Bool = region r {
        let a = [1,2];
        Array.transformWithIndex!((e, i) -> if (i < 1) e + 10 else e, a);
        Array.sameElements(a, [11,2])
    }

    @test
    def transformWithIndex05!(): Bool = region r {
        let a = [1,8];
        Array.transformWithIndex!((e, i) -> if (i < 1) e + 10 else e, a);
        Array.sameElements(a, [11,8])
    }

    @test
    def transformWithIndex06!(): Bool = region r {
        let a = [8,1];
        Array.transformWithIndex!((e, i) -> if (i < 1) e + 10 else e, a);
        Array.sameElements(a, [18,1])
    }

    @test
    def transformWithIndex07!(): Bool = region r {
        let a = [7,8,9];
        Array.transformWithIndex!((e, i) -> if (i < 1) e + 10 else e, a);
        Array.sameElements(a, [17,8,9])
    }

    /////////////////////////////////////////////////////////////////////////////
    // flatMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    // depends on flatten

    @test
    def flatMap01(): Bool = region r {
        let a = Array.flatMap(i -> Array.repeat(i, i, r), []);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def flatMap02(): Bool = region r {
        let a = Array.flatMap(i -> Array.repeat(i, i, r), [0]);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def flatMap03(): Bool = region r {
        let a = Array.flatMap(i -> Array.repeat(i, i, r), [1]);
        Array.sameElements(a, [1])
    }

    @test
    def flatMap04(): Bool = region r {
        let a = Array.flatMap(i -> Array.repeat(i, i, r), [2]);
        Array.sameElements(a, [2,2])
    }

    @test
    def flatMap05(): Bool = region r {
        let a = Array.flatMap(i -> Array.repeat(i, i, r), [1,2]);
        Array.sameElements(a, [1,2,2])
    }

    @test
    def flatMap06(): Bool = region r {
        let a = Array.flatMap(i -> Array.repeat(i, i, r), [2,3]);
        Array.sameElements(a, [2,2,3,3,3])
    }

    /////////////////////////////////////////////////////////////////////////////
    // reverse                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reverse01(): Bool = region r {
        let a = Array.reverse([]: Array[Int32, _]);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def reverse02(): Bool = region r {
        let a = Array.reverse([1]);
        Array.sameElements(a, [1])
    }

    @test
    def reverse03(): Bool = region r {
        let a = Array.reverse([1,2]);
        Array.sameElements(a, [2,1])
    }

    @test
    def reverse04(): Bool = region r {
        let a = Array.reverse([1,1]);
        Array.sameElements(a, [1,1])
    }

    @test
    def reverse05(): Bool = region r {
        let a = Array.reverse([1,2,3]);
        Array.sameElements(a, [3,2,1])
    }

    @test
    def reverse06(): Bool = region r {
        let a = Array.reverse([1,2,3,4]);
        Array.sameElements(a, [4,3,2,1])
    }

    /////////////////////////////////////////////////////////////////////////////
    // reverse!                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reverse01!(): Bool = region r {
        let a: Array[Int32, _] = [];
        Array.reverse!(a);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def reverse02!(): Bool = region r {
        let a = [1];
        Array.reverse!(a);
        Array.sameElements(a, [1])
    }

    @test
    def reverse03!(): Bool = region r {
        let a = [1,2];
        Array.reverse!(a);
        Array.sameElements(a, [2,1])
    }

    @test
    def reverse04!(): Bool = region r {
        let a = [1,1];
        Array.reverse!(a);
        Array.sameElements(a, [1,1])
    }

    @test
    def reverse05!(): Bool = region r {
        let a = [1,2,3];
        Array.reverse!(a);
        Array.sameElements(a, [3,2,1])
    }

    @test
    def reverse06!(): Bool = region r {
        let a = [1,2,3,4];
        Array.reverse!(a);
        Array.sameElements(a, [4,3,2,1])
    }

    /////////////////////////////////////////////////////////////////////////////
    // rotateLeft                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def rotateLeft01(): Bool = region r {
        let a = Array.rotateLeft(0, []: Array[Int32, _]);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def rotateLeft02(): Bool = region r {
        let a = Array.rotateLeft(1, []: Array[Int32, _]);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def rotateLeft03(): Bool = region r {
        let a = Array.rotateLeft(0, [1]);
        Array.sameElements(a, [1])
    }

    @test
    def rotateLeft04(): Bool = region r {
        let a = Array.rotateLeft(0, [1,2]);
        Array.sameElements(a, [1,2])
    }

    @test
    def rotateLeft05(): Bool = region r {
        let a = Array.rotateLeft(1, [1,2]);
        Array.sameElements(a, [2,1])
    }

    @test
    def rotateLeft06(): Bool = region r {
        let a = Array.rotateLeft(2, [1,2]);
        Array.sameElements(a, [1,2])
    }

    @test
    def rotateLeft07(): Bool = region r {
        let a = Array.rotateLeft(3, [1,2]);
        Array.sameElements(a, [2,1])
    }

    @test
    def rotateLeft08(): Bool = region r {
        let a = Array.rotateLeft(-1, [1,2]);
        Array.sameElements(a, [2,1])
    }

    @test
    def rotateLeft09(): Bool = region r {
        let a = Array.rotateLeft(0, [1,2,3]);
        Array.sameElements(a, [1,2,3])
    }

    @test
    def rotateLeft10(): Bool = region r {
        let a = Array.rotateLeft(1, [1,2,3]);
        Array.sameElements(a, [2,3,1])
    }

    @test
    def rotateLeft11(): Bool = region r {
        let a = Array.rotateLeft(2, [1,2,3]);
        Array.sameElements(a, [3,1,2])
    }

    /////////////////////////////////////////////////////////////////////////////
    // rotateRight                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def rotateRight01(): Bool = region r {
        let a = Array.rotateRight(0, []: Array[Int32, _]);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def rotateRight02(): Bool = region r {
        let a = Array.rotateRight(1, []: Array[Int32, _]);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def rotateRight03(): Bool = region r {
        let a = Array.rotateRight(0, [1]);
        Array.sameElements(a, [1])
    }

    @test
    def rotateRight04(): Bool = region r {
        let a = Array.rotateRight(0, [1,2]);
        Array.sameElements(a, [1,2])
    }

    @test
    def rotateRight05(): Bool = region r {
        let a = Array.rotateRight(1, [1,2]);
        Array.sameElements(a, [2,1])
    }

    @test
    def rotateRight06(): Bool = region r {
        let a = Array.rotateRight(2, [1,2]);
        Array.sameElements(a, [1,2])
    }

    @test
    def rotateRight07(): Bool = region r {
        let a = Array.rotateRight(3, [1,2]);
        Array.sameElements(a, [2,1])
    }

    @test
    def rotateRight08(): Bool = region r {
        let a = Array.rotateRight(-1, [1,2]);
        Array.sameElements(a, [2,1])
    }

    @test
    def rotateRight09(): Bool = region r {
        let a = Array.rotateRight(0, [1,2,3]);
        Array.sameElements(a, [1,2,3])
    }

    @test
    def rotateRight10(): Bool = region r {
        let a = Array.rotateRight(1, [1,2,3]);
        Array.sameElements(a, [3,1,2])
    }

    @test
    def rotateRight11(): Bool = region r {
        let a = Array.rotateRight(2, [1,2,3]);
        Array.sameElements(a, [2,3,1])
    }

    /////////////////////////////////////////////////////////////////////////////
    // update                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def update01(): Bool = region r {
        let a = Array.update(0, 2, []: Array[Int32, _]);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def update02(): Bool = region r {
        let a = Array.update(-1, 2, [1]);
        Array.sameElements(a, [1])
    }

    @test
    def update03(): Bool = region r {
        let a = Array.update(0, 2, [1]);
        Array.sameElements(a, [2])
    }

    @test
    def update04(): Bool = region r {
        let a = Array.update(1, 2, [1]);
        Array.sameElements(a, [1])
    }

    @test
    def update05(): Bool = region r {
        let a = Array.update(0, 5, [1,2]);
        Array.sameElements(a, [5,2])
    }

    @test
    def update06(): Bool = region r {
        let a = Array.update(1, 5, [1,2]);
        Array.sameElements(a, [1,5])
    }

    @test
    def update07(): Bool = region r {
        let a = Array.update(2, 5, [1,2]);
        Array.sameElements(a, [1,2])
    }

    /////////////////////////////////////////////////////////////////////////////
    // replace                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def replace01(): Bool = region r {
        let a = Array.replace(from = 3, to = 4, []);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def replace02(): Bool = region r {
        let a = Array.replace(from = 3, to = 4, [1]);
        Array.sameElements(a, [1])
    }

    @test
    def replace03(): Bool = region r {
        let a = Array.replace(from = 3, to = 4, [3]);
        Array.sameElements(a, [4])
    }

    @test
    def replace04(): Bool = region r {
        let a = Array.replace(from = 3, to = 4, [4]);
        Array.sameElements(a, [4])
    }

    @test
    def replace05(): Bool = region r {
        let a = Array.replace(from = 3, to = 4, [1, 2]);
        Array.sameElements(a, [1,2])
    }

    @test
    def replace06(): Bool = region r {
        let a = Array.replace(from = 3, to = 4, [1,3]);
        Array.sameElements(a, [1,4])
    }

    @test
    def replace07(): Bool = region r {
        let a = Array.replace(from = 3, to = 4, [3,4]);
        Array.sameElements(a, [4,4])
    }

    @test
    def replace08(): Bool = region r {
        let a = Array.replace(from = 3, to = 4, [3,3]);
        Array.sameElements(a, [4,4])
    }

    /////////////////////////////////////////////////////////////////////////////
    // replace!                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def replace01!(): Bool = region r {
        let a: Array[Int32, _] = [];
        Array.replace!(from = 3, to = 4, a);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def replace02!(): Bool = region r {
        let a = [1];
        Array.replace!(from = 3, to = 4, a);
        Array.sameElements(a, [1])
    }

    @test
    def replace03!(): Bool = region r {
        let a = [3];
        Array.replace!(from = 3, to = 4, a);
        Array.sameElements(a, [4])
    }

    @test
    def replace04!(): Bool = region r {
        let a = [4];
        Array.replace!(from = 3, to = 4, a);
        Array.sameElements(a, [4])
    }

    @test
    def replace05!(): Bool = region r {
        let a = [1, 2];
        Array.replace!(from = 3, to = 4, a);
        Array.sameElements(a, [1,2])
    }

    @test
    def replace06!(): Bool = region r {
        let a = [1,3];
        Array.replace!(from = 3, to = 4, a);
        Array.sameElements(a, [1,4])
    }

    @test
    def replace07!(): Bool = region r {
        let a = [3,4];
        Array.replace!(from = 3, to = 4, a);
        Array.sameElements(a, [4,4])
    }

    @test
    def replace08!(): Bool = region r {
        let a = [3,3];
        Array.replace!(from = 3, to = 4, a);
        Array.sameElements(a, [4,4])
    }

    /////////////////////////////////////////////////////////////////////////////
    // patch                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def patch01(): Bool = region r {
        let a = Array.patch(0, 0, []: Array[Int32, _], []: Array[Int32, _]);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def patch02(): Bool = region r {
        let a = Array.patch(0, 2, [1,2], []);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def patch03(): Bool = region r {
        let a = Array.patch(0, 2, [], [1,2]);
        Array.sameElements(a, [1,2])
    }

    @test
    def patch04(): Bool = region r {
        let a = Array.patch(-3, 3, [1,2,4], [1,2]);
        Array.sameElements(a, [1,2])
    }

    @test
    def patch05(): Bool = region r {
        let a = Array.patch(2, 3, [1,2,4], [1,2]);
        Array.sameElements(a, [1,2])
    }

    @test
    def patch06(): Bool = region r {
        let a = Array.patch(0, 0, [], [1]);
        Array.sameElements(a, [1])
    }

    @test
    def patch07(): Bool = region r {
        let a = Array.patch(1, 0, [2], [1]);
        Array.sameElements(a, [1])
    }

    @test
    def patch08(): Bool = region r {
        let a = Array.patch(0, 1, [2], [1]);
        Array.sameElements(a, [2])
    }

    @test
    def patch09(): Bool = region r {
        let a = Array.patch(0, 2, [2,4], [1]);
        Array.sameElements(a, [2])
    }

    @test
    def patch10(): Bool = region r {
        let a = Array.patch(-1, 2, [2,4], [1]);
        Array.sameElements(a, [4])
    }

    @test
    def patch11(): Bool = region r {
        let a = Array.patch(-1, 2, [3,4], [1,2]);
        Array.sameElements(a, [4,2])
    }

    @test
    def patch12(): Bool = region r {
        let a = Array.patch(1, 2, [3,4], [1,2]);
        Array.sameElements(a, [1,3])
    }

    @test
    def patch13(): Bool = region r {
        let a = Array.patch(-2, 2, [3,4], [1,2]);
        Array.sameElements(a, [1,2])
    }

    @test
    def patch14(): Bool = region r {
        let a = Array.patch(2, 2, [3,4], [1,2]);
        Array.sameElements(a, [1,2])
    }

    @test
    def patch15(): Bool = region r {
        let a = Array.patch(1, 1, [3], [1,2]);
        Array.sameElements(a, [1,3])
    }

    @test
    def patch16(): Bool = region r {
        let a = Array.patch(0, 2, [3,4], [1,2]);
        Array.sameElements(a, [3,4])
    }

    @test
    def patch17(): Bool = region r {
        let a = Array.patch(0, 1, [4], [1, 2, 3]);
        Array.sameElements(a, [4, 2, 3])
    }

    @test
    def patch18(): Bool = region r {
        let a = Array.patch(1, 1, [4], [1, 2, 3]);
        Array.sameElements(a, [1, 4, 3])
    }

    @test
    def patch19(): Bool = region r {
        let a = Array.patch(2, 1, [4], [1, 2, 3]);
        Array.sameElements(a, [1, 2, 4])
    }

    @test
    def patch20(): Bool = region r {
        let a = Array.patch(0, 2, [4, 5], [1, 2, 3]);
        Array.sameElements(a, [4, 5, 3])
    }

    @test
    def patch21(): Bool = region r {
        let a = Array.patch(1, 2, [4, 5], [1, 2, 3]);
        Array.sameElements(a, [1, 4, 5])
    }

    @test
    def patch22(): Bool = region r {
        let a = Array.patch(0, 2, [4, 5, 6], [1, 2, 3]);
        Array.sameElements(a, [4, 5, 3])
    }

    @test
    def patch23(): Bool = region r {
        let a = Array.patch(0, 3, [4, 5, 6], [1, 2, 3]);
        Array.sameElements(a, [4, 5, 6])
    }

    @test
    def patch24(): Bool = region r {
        let a = Array.patch(2, 4, [14, 15, 16, 17], [1, 2, 3, 4, 5, 6, 7]);
        Array.sameElements(a, [1, 2, 14, 15, 16, 17, 7])
    }

    @test
    def patch25(): Bool = region r {
        let a = Array.patch(-2, 4, [14, 15, 16, 17], [1, 2, 3, 4, 5, 6, 7]);
        Array.sameElements(a, [16, 17, 3, 4, 5, 6, 7])
    }

    @test
    def patch26(): Bool = region r {
        let a = Array.patch(4, 5, [14, 15, 16, 17], [1, 2, 3, 4, 5, 6, 7]);
        Array.sameElements(a, [1, 2, 3, 4, 14, 15, 16])
    }

    @test
    def patch27(): Bool = region r {
        let a = Array.patch(4, 2, [14, 15, 16, 17], [1, 2, 3, 4, 5, 6, 7]);
        Array.sameElements(a, [1, 2, 3, 4, 14, 15, 7])
    }

    @test
    def patch28(): Bool = region r {
        let a = Array.patch(-1, 10, [-1, -2, -3, -4, -5, -6, -7, -8], [1, 2, 3, 4, 5, 6, 7]);
        Array.sameElements(a, [-2, -3, -4, -5, -6, -7, -8])
    }

    /////////////////////////////////////////////////////////////////////////////
    // patch!                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def patch01!(): Bool = region r {
        let a = []: Array[Int32, _];
        Array.patch!(0, 0, []: Array[Int32, _], a);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def patch02!(): Bool = region r {
        let a = []: Array[Int32, _];
        Array.patch!(0, 2, [1,2], a);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def patch03!(): Bool = region r {
        let a = [1,2];
        Array.patch!(0, 2, [], a);
        Array.sameElements(a, [1,2])
    }

    @test
    def patch04!(): Bool = region r {
        let a = [1,2];
        Array.patch!(-3, 3, [1,2,4], a);
        Array.sameElements(a, [1,2])
    }

    @test
    def patch05!(): Bool = region r {
        let a = [1,2];
        Array.patch!(2, 3, [1,2,4], a);
        Array.sameElements(a, [1,2])
    }

    @test
    def patch06!(): Bool = region r {
        let a = [1];
        Array.patch!(0, 0, [], a);
        Array.sameElements(a, [1])
    }

    @test
    def patch07!(): Bool = region r {
        let a = [1];
        Array.patch!(1, 0, [2], a);
        Array.sameElements(a, [1])
    }

    @test
    def patch08!(): Bool = region r {
        let a = [1];
        Array.patch!(0, 1, [2], a);
        Array.sameElements(a, [2])
    }

    @test
    def patch09!(): Bool = region r {
        let a = [1];
        Array.patch!(0, 2, [2,4], a);
        Array.sameElements(a, [2])
    }

    @test
    def patch10!(): Bool = region r {
        let a = [1];
        Array.patch!(-1, 2, [2,4], a);
        Array.sameElements(a, [4])
    }

    @test
    def patch11!(): Bool = region r {
        let a = [1,2];
        Array.patch!(-1, 2, [3,4], a);
        Array.sameElements(a, [4,2])
    }

    @test
    def patch12!(): Bool = region r {
        let a = [1,2];
        Array.patch!(1, 2, [3,4], a);
        Array.sameElements(a, [1,3])
    }

    @test
    def patch13!(): Bool = region r {
        let a = [1,2];
        Array.patch!(-2, 2, [3,4], a);
        Array.sameElements(a, [1,2])
    }

    @test
    def patch14!(): Bool = region r {
        let a = [1,2];
        Array.patch!(2, 2, [3,4], a);
        Array.sameElements(a, [1,2])
    }

    @test
    def patch15!(): Bool = region r {
        let a = [1,2];
        Array.patch!(1, 1, [3], a);
        Array.sameElements(a, [1,3])
    }

    @test
    def patch16!(): Bool = region r {
        let a = [1,2];
        Array.patch!(0, 2, [3,4], a);
        Array.sameElements(a, [3,4])
    }

    @test
    def patch17!(): Bool = region r {
        let a = [1, 2, 3];
        Array.patch!(0, 1, [4], a);
        Array.sameElements(a, [4, 2, 3])
    }

    @test
    def patch18!(): Bool = region r {
        let a = [1, 2, 3];
        Array.patch!(1, 1, [4], a);
        Array.sameElements(a, [1, 4, 3])
    }

    @test
    def patch19!(): Bool = region r {
        let a = [1, 2, 3];
        Array.patch!(2, 1, [4], a);
        Array.sameElements(a, [1, 2, 4])
    }

    @test
    def patch20!(): Bool = region r {
        let a = [1, 2, 3];
        Array.patch!(0, 2, [4, 5], a);
        Array.sameElements(a, [4, 5, 3])
    }

    @test
    def patch21!(): Bool = region r {
        let a = [1, 2, 3];
        Array.patch!(1, 2, [4, 5], a);
        Array.sameElements(a, [1, 4, 5])
    }

    @test
    def patch22!(): Bool = region r {
        let a = [1, 2, 3];
        Array.patch!(0, 2, [4, 5, 6], a);
        Array.sameElements(a, [4, 5, 3])
    }

    @test
    def patch23!(): Bool = region r {
        let a = [1, 2, 3];
        Array.patch!(0, 3, [4, 5, 6], a);
        Array.sameElements(a, [4, 5, 6])
    }

    @test
    def patch24!(): Bool = region r {
        let a = [1, 2, 3, 4, 5, 6, 7];
        Array.patch!(2, 4, [14, 15, 16, 17], a);
        Array.sameElements(a, [1, 2, 14, 15, 16, 17, 7])
    }

    @test
    def patch25!(): Bool = region r {
        let a = [1, 2, 3, 4, 5, 6, 7];
        Array.patch!(-2, 4, [14, 15, 16, 17], a);
        Array.sameElements(a, [16, 17, 3, 4, 5, 6, 7])
    }

    @test
    def patch26!(): Bool = region r {
        let a = [1, 2, 3, 4, 5, 6, 7];
        Array.patch!(4, 5, [14, 15, 16, 17], a);
        Array.sameElements(a, [1, 2, 3, 4, 14, 15, 16])
    }

    @test
    def patch27!(): Bool = region r {
        let a = [1, 2, 3, 4, 5, 6, 7];
        Array.patch!(4, 2, [14, 15, 16, 17], a);
        Array.sameElements(a, [1, 2, 3, 4, 14, 15, 7])
    }

    @test
    def patch28!(): Bool = region r {
        let a = [1, 2, 3, 4, 5, 6, 7];
        Array.patch!(-1, 10, [-1, -2, -3, -4, -5, -6, -7, -8], a);
        Array.sameElements(a, [-2, -3, -4, -5, -6, -7, -8])
    }

    /////////////////////////////////////////////////////////////////////////////
    // intersperse                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def intersperse01(): Bool = region r {
        let a = Array.intersperse(11, []);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def intersperse02(): Bool = region r {
        let a = Array.intersperse(11, [1]);
        Array.sameElements(a, [1])
    }

    @test
    def intersperse03(): Bool = region r {
        let a = Array.intersperse(11, [1,2]);
        Array.sameElements(a, [1,11,2])
    }

    @test
    def intersperse04(): Bool = region r {
        let a = Array.intersperse(11, [1,2,3]);
        Array.sameElements(a, [1,11,2,11,3])
    }

    @test
    def intersperse05(): Bool = region r {
        let a = Array.intersperse(11, [1,2,3,4]);
        Array.sameElements(a, [1,11,2,11,3,11,4])
    }


    /////////////////////////////////////////////////////////////////////////////
    // intercalate                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def intercalate01(): Bool = region r {
        let a = Array.intercalate([]: Array[Int32, r], []: Array[Array[Int32, r], r]);
        Array.sameElements(a, []: Array[Int32, r])
    }

    @test
    def intercalate02(): Bool = region r {
        let a = Array.intercalate([]: Array[Int32, r], [[1]]);
        Array.sameElements(a, [1])
    }

    @test
    def intercalate03(): Bool = region r {
        let a = Array.intercalate([11,12,13], []: Array[Array[Int32, r], r]);
        Array.sameElements(a, []: Array[Int32, r])
    }

    @test
    def intercalate04(): Bool = region r {
        let a = Array.intercalate([]: Array[Int32, r], [[1],[2,3]]);
        Array.sameElements(a, [1,2,3])
    }

    @test
    def intercalate05(): Bool = region r {
        let a = Array.intercalate([11,12,13], [[1],[2,3]]);
        Array.sameElements(a, [1,11,12,13,2,3])
    }

    @test
    def intercalate06(): Bool = region r {
        let a = Array.intercalate([]: Array[Int32, r], [[1],[2,3],[4]]);
        Array.sameElements(a, [1,2,3,4])
    }

    @test
    def intercalate07(): Bool = region r {
        let a = Array.intercalate([11,12,13], [[1],[2,3],[4]]);
        Array.sameElements(a, [1,11,12,13,2,3,11,12,13,4])
    }

    /////////////////////////////////////////////////////////////////////////////
    // transpose                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def transpose01(): Bool = region r {
        let a = Array.transpose([]);
        let b = Array.toList(a) |> List.map(Array.toList);
        b: List[List[Unit]] == Nil
    }

    @test
    def transpose02(): Bool = region r {
        let a: Array[Array[Int32, _], _] = Array.transpose([[]]);
        let b = Array.toList(a) |> List.map(Array.toList);
        b == Nil :: Nil
    }

    @test
    def transpose03(): Bool = region r {
        let a: Array[Array[Int32, _], _] = Array.transpose([[], []]);
        let b = Array.toList(a) |> List.map(Array.toList);
        b == Nil :: Nil :: Nil
    }

    @test
    def transpose04(): Bool = region r {
        let a: Array[Array[Int32, _], _] = Array.transpose([[], [], []]);
        let b = Array.toList(a) |> List.map(Array.toList);
        b == Nil :: Nil :: Nil :: Nil
    }

    @test
    def transpose05(): Bool = region r {
        let a = Array.transpose([ [1] ]);
        let b = Array.toList(a) |> List.map(Array.toList);
        b == (1 :: Nil) :: Nil
    }

    @test
    def transpose06(): Bool = region r {
        let a = Array.transpose([[1,2]]);
        let b = Array.toList(a) |> List.map(Array.toList);
        b == (1 :: Nil) :: (2 :: Nil) :: Nil
    }

    @test
    def transpose07(): Bool = region r {
        let a = Array.transpose([[1, 2, 3]]);
        let b = Array.toList(a) |> List.map(Array.toList);
        b == (1 :: Nil) :: (2 :: Nil) :: (3 :: Nil) :: Nil
    }

    @test
    def transpose08(): Bool = region r {
        let a = Array.transpose([[1, 2, 3, 4]]);
        let b = Array.toList(a) |> List.map(Array.toList);
        b == (1 :: Nil) :: (2 :: Nil) :: (3 :: Nil) :: (4 :: Nil) :: Nil
    }

    @test
    def transpose09(): Bool = region r {
        let a = Array.transpose([[1], [2]]);
        let b = Array.toList(a) |> List.map(Array.toList);
        b ==   (1 :: 2 :: Nil) :: Nil
    }

    @test
    def transpose10(): Bool = region r {
        let a = Array.transpose([[1], [2], [3]]);
        let b = Array.toList(a) |> List.map(Array.toList);
        b ==  (1 :: 2 :: 3 :: Nil) :: Nil
    }

    @test
    def transpose11(): Bool = region r {
        let a = Array.transpose([[1], [2], [3], [4]]);
        let b = Array.toList(a) |> List.map(Array.toList);
        b == (1 :: 2 :: 3 :: 4 :: Nil) :: Nil
    }

    @test
    def transpose12(): Bool = region r {
        let a = Array.transpose([[1, 2], [3, 4]]);
        let b = Array.toList(a) |> List.map(Array.toList);
        b == (1 :: 3 :: Nil) :: (2 :: 4 :: Nil) :: Nil
    }

    @test
    def transpose13(): Bool = region r {
        let a = Array.transpose([[1, 2, 3], [4, 5, 6]]);
        let b = Array.toList(a) |> List.map(Array.toList);
        b == (1 :: 4 :: Nil) :: (2 :: 5 :: Nil) :: (3 :: 6 :: Nil) :: Nil
    }

    @test
    def transpose14(): Bool = region r {
        let a = Array.transpose([[1, 2, 3, 4], [5, 6, 7, 8]]);
        let b = Array.toList(a) |> List.map(Array.toList);
        b == (1 :: 5 :: Nil) :: (2 :: 6 :: Nil) :: (3 :: 7 :: Nil) :: (4 :: 8 :: Nil) :: Nil
    }

    @test
    def transpose15(): Bool = region r {
        let a = Array.transpose([[1, 2, 3, 4, 5], [6, 7, 8, 9, 10]]);
        let b = Array.toList(a) |> List.map(Array.toList);
        b == (1 :: 6 :: Nil) :: (2 :: 7 :: Nil) :: (3 :: 8 :: Nil) :: (4 :: 9 :: Nil) :: (5 :: 10 :: Nil) :: Nil
    }

    @test
    def transpose16(): Bool = region r {
        let a = Array.transpose([[1, 2], [3, 4], [5, 6]]);
        let b = Array.toList(a) |> List.map(Array.toList);
        b == (1 :: 3 :: 5 :: Nil) :: (2 :: 4 :: 6 :: Nil) :: Nil
    }

    @test
    def transpose17(): Bool = region r {
        let a = Array.transpose([[1, 2], [3, 4], [5, 6], [7,8]]);
        let b = Array.toList(a) |> List.map(Array.toList);
        b == (1 :: 3 :: 5 :: 7 :: Nil) :: (2 :: 4 :: 6 :: 8 :: Nil) :: Nil
    }

    @test
    def transpose18(): Bool = region r {
        let a = Array.transpose([[1, 2], [3, 4], [5, 6], [7, 8], [9, 10]]);
        let b = Array.toList(a) |> List.map(Array.toList);
        b == (1 :: 3 :: 5 :: 7 :: 9 :: Nil) :: (2 :: 4 :: 6 :: 8 :: 10 :: Nil) :: Nil
    }

    @test
    def transpose19(): Bool = region r {
        let a = Array.transpose([[1, 2, 3], [4, 5, 6], [7, 8, 9]]);
        let b = Array.toList(a) |> List.map(Array.toList);
        b == (1 :: 4 :: 7 :: Nil) :: (2 :: 5 :: 8 :: Nil) :: (3 :: 6 :: 9 :: Nil) :: Nil
    }

    @test
    def transpose20(): Bool = region r {
        let a = Array.transpose([[1, 2, 3], [4, 5], [7, 8, 9]]);
        let b = Array.toList(a) |> List.map(Array.toList);
        b == (1 :: 2 :: 3 :: Nil) :: (4 :: 5 :: Nil) :: (7 :: 8 :: 9 :: Nil) :: Nil
    }

    @test
    def transpose21(): Bool = region r {
        let a = Array.transpose([[1, 2, 3], [], [7, 8, 9]]);
        let b = Array.toList(a) |> List.map(Array.toList);
        b == (1 :: 2 :: 3 :: Nil) :: Nil :: (7 :: 8 :: 9 :: Nil) :: Nil
    }

    @test
    def transpose22(): Bool = region r {
        let a = Array.transpose([[1, 2, 3], [4, 5, 6], [7, 8, 9, 10]]);
        let b = Array.toList(a) |> List.map(Array.toList);
        b == (1 :: 2 :: 3 :: Nil) :: (4 :: 5 :: 6 :: Nil) :: (7 :: 8 :: 9 :: 10 :: Nil) :: Nil
    }

    /////////////////////////////////////////////////////////////////////////////
    // isPrefixOf                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def isPrefixOf01(): Bool = region r {
        Array.isPrefixOf([]: Array[Int32, _], []: Array[Int32, _])
    }

    @test
    def isPrefixOf02(): Bool = region r {
        Array.isPrefixOf([1], []: Array[Int32, _]) == false
    }

    @test
    def isPrefixOf03(): Bool = region r {
        Array.isPrefixOf([]: Array[Int32, _], [1])
    }

    @test
    def isPrefixOf04(): Bool = region r {
        Array.isPrefixOf([1], [1])
    }

    @test
    def isPrefixOf05(): Bool = region r {
        Array.isPrefixOf([2], [1]) == false
    }

    @test
    def isPrefixOf06(): Bool = region r {
        Array.isPrefixOf([1,2], [1]) == false
    }

    @test
    def isPrefixOf07(): Bool = region r {
        Array.isPrefixOf([]: Array[Int32, _], [1,2])
    }

    @test
    def isPrefixOf08(): Bool = region r {
        Array.isPrefixOf([1], [1,2])
    }

    @test
    def isPrefixOf09(): Bool = region r {
        Array.isPrefixOf([2], [1,2]) == false
    }

    @test
    def isPrefixOf10(): Bool = region r {
        Array.isPrefixOf([1,2], [1,2])
    }

    @test
    def isPrefixOf11(): Bool = region r {
        Array.isPrefixOf([1,3], [1,2]) == false
    }

    @test
    def isPrefixOf12(): Bool = region r {
        Array.isPrefixOf([1,2,3], [1,2]) == false
    }

    @test
    def isPrefixOf13(): Bool = region r {
        Array.isPrefixOf([1,2,3], [89,11,1,2,3,4]) == false
    }

    /////////////////////////////////////////////////////////////////////////////
    // isInfixOf                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def isInfixOf01(): Bool = region r {
        Array.isInfixOf([]: Array[Int32, _], []: Array[Int32, _])
    }

    @test
    def isInfixOf02(): Bool = region r {
        Array.isInfixOf([1], []: Array[Int32, _]) == false
    }

    @test
    def isInfixOf03(): Bool = region r {
        Array.isInfixOf([]: Array[Int32, _], [1])
    }

    @test
    def isInfixOf04(): Bool = region r {
        Array.isInfixOf([1], [1])
    }

    @test
    def isInfixOf05(): Bool = region r {
        Array.isInfixOf([2], [1]) == false
    }

    @test
    def isInfixOf06(): Bool = region r {
        Array.isInfixOf([1,2], [1]) == false
    }

    @test
    def isInfixOf07(): Bool = region r {
        Array.isInfixOf([]: Array[Int32, _], [1,2])
    }

    @test
    def isInfixOf08(): Bool = region r {
        Array.isInfixOf([1], [1,2])
    }

    @test
    def isInfixOf09(): Bool = region r {
        Array.isInfixOf([2], [1,2])
    }

    @test
    def isInfixOf10(): Bool = region r {
        Array.isInfixOf([1,2], [1,2])
    }

    @test
    def isInfixOf11(): Bool = region r {
        Array.isInfixOf([1,3], [1,2]) == false
    }

    @test
    def isInfixOf12(): Bool = region r {
        Array.isInfixOf([1,2,3], [1,2]) == false
    }

    @test
    def isInfixOf13(): Bool = region r {
        Array.isInfixOf([1,2,3], [89,11,1,2,3,4])
    }

    /////////////////////////////////////////////////////////////////////////////
    // isSuffixOf                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def isSuffixOf01(): Bool = region r {
        Array.isSuffixOf([]: Array[Int32, _], []: Array[Int32, _])
    }

    @test
    def isSuffixOf02(): Bool = region r {
        Array.isSuffixOf([1], []: Array[Int32, _]) == false
    }

    @test
    def isSuffixOf03(): Bool = region r {
        Array.isSuffixOf([]: Array[Int32, _], [1])
    }

    @test
    def isSuffixOf04(): Bool = region r {
        Array.isSuffixOf([1], [1])
    }

    @test
    def isSuffixOf05(): Bool = region r {
        Array.isSuffixOf([2], [1]) == false
    }

    @test
    def isSuffixOf06(): Bool = region r {
        Array.isSuffixOf([1,2], [1]) == false
    }

    @test
    def isSuffixOf07(): Bool = region r {
        Array.isSuffixOf([]: Array[Int32, _], [1,2])
    }

    @test
    def isSuffixOf08(): Bool = region r {
        Array.isSuffixOf([1], [1,2]) == false
    }

    @test
    def isSuffixOf09(): Bool = region r {
        Array.isSuffixOf([2], [1,2])
    }

    @test
    def isSuffixOf10(): Bool = region r {
        Array.isSuffixOf([1,2], [1,2])
    }

    @test
    def isSuffixOf11(): Bool = region r {
        Array.isSuffixOf([1,3], [1,2]) == false
    }

    @test
    def isSuffixOf12(): Bool = region r {
        Array.isSuffixOf([1,2,3], [1,2]) == false
    }

    @test
    def isSuffixOf13(): Bool = region r {
        Array.isSuffixOf([1,2,3], [89,11,1,2,3])
    }

    /////////////////////////////////////////////////////////////////////////////
    // eq                                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testSameElements01(): Bool = region r {
        ([]: Array[Int32, _]) `sameElements` ([]: Array[Int32, _])
    }

    @test
    def testSameElements02(): Bool = region r {
        not ([1] `sameElements` [])
    }

    @test
    def testSameElements03(): Bool = region r {
        not ([] `sameElements` [1])
    }

    @test
    def testSameElements04(): Bool = region r {
        [1] `sameElements` [1]
    }

    @test
    def testSameElements05(): Bool = region r {
        not ([1] `sameElements` [2])
    }

    @test
    def testSameElements06(): Bool = region r {
        not ([1,2] `sameElements` [1])
    }

    @test
    def testSameElements07(): Bool = region r {
        not ([1] `sameElements` [1,2])
    }

    @test
    def testSameElements08(): Bool = region r {
        [1,2,3,4] `sameElements` [1,2,3,4]
    }

    /////////////////////////////////////////////////////////////////////////////
    // compare                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testCompare01(): Bool = region r {
        ([]: Array[Int32, _]) `Array.compare` ([]: Array[Int32, _]) == EqualTo
    }

    @test
    def testCompare02(): Bool = region r {
        ([1] `Array.compare` []) == GreaterThan
    }

    @test
    def testCompare03(): Bool = region r {
        ([] `Array.compare` [1]) == LessThan
    }

    @test
    def testCompare04(): Bool = region r {
        ([1] `Array.compare` [1]) == EqualTo
    }

    @test
    def testCompare05(): Bool = region r {
        ([1] `Array.compare` [2]) == LessThan
    }

    @test
    def testCompare06(): Bool = region r {
        ([2] `Array.compare` [1]) == GreaterThan
    }

    @test
    def testCompare07(): Bool = region r {
        ([1] `Array.compare` [1,2]) == LessThan
    }

    @test
    def testCompare08(): Bool = region r {
        ([1,2] `Array.compare` [1]) == GreaterThan
    }

    @test
    def testCompare09(): Bool = region r {
        ([1] `Array.compare` [0,0]) == GreaterThan
    }

    @test
    def testCompare10(): Bool = region r {
        ([0,0] `Array.compare` [1]) == LessThan
    }

    @test
    def testCompare11(): Bool = region r {
        ([1,2] `Array.compare` [1,1]) == GreaterThan
    }

    @test
    def testCompare12(): Bool = region r {
        ([1,2] `Array.compare` [1,3]) == LessThan
    }

    @test
    def testCompare13(): Bool = region r {
        ([1,2,3,4] `Array.compare` [1,2,3,4]) == EqualTo
    }

    /////////////////////////////////////////////////////////////////////////////
    // fold                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def fold01(): Bool = region r {
        Array.fold([]: Array[Unit, _]) == ()
    }

    @test
    def fold02(): Bool = region r {
        Array.fold(["a"]) == "a"
    }

    @test
    def fold03(): Bool = region r {
        Array.fold(["a", "b", "c"]) == "abc"
    }

    @test
    def fold04(): Bool = region r {
        Array.fold([("yes", "no"), ("no", "yes")]) == ("yesno", "noyes")
    }

    /////////////////////////////////////////////////////////////////////////////
    // foldLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldLeft01(): Bool = region r {
        Array.foldLeft((i, e) -> (i - e) * (e rem 2 + 1), 100, []) == 100
    }

    @test
    def foldLeft02(): Bool = region r {
        Array.foldLeft((i, e) -> (i - e) * (e rem 2 + 1), 100, [1]) == 198
    }

    @test
    def foldLeft03(): Bool = region r {
        Array.foldLeft((i, e) -> (i - e) * (e rem 2 + 1), 100, [1,2]) == 196
    }

    @test
    def foldLeft04(): Bool = region r {
        Array.foldLeft((i, e) -> (i - e) * (e rem 2 + 1), 100, [1,2,3]) == 386
    }

    /////////////////////////////////////////////////////////////////////////////
    // foldRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldRight01(): Bool = region r {
        Array.foldRight((e, i) -> (i - e) * (e rem 2 + 1), 100, []) == 100
    }

    @test
    def foldRight02(): Bool = region r {
        Array.foldRight((e, i) -> (i - e) * (e rem 2 + 1), 100, [1]) == 198
    }

    @test
    def foldRight03(): Bool = region r {
        Array.foldRight((e, i) -> (i - e) * (e rem 2 + 1), 100, [1,2]) == 194
    }

    @test
    def foldRight04(): Bool = region r {
        Array.foldRight((e, i) -> (i - e) * (e rem 2 + 1), 100, [1,2,3]) == 382
    }

    /////////////////////////////////////////////////////////////////////////////
    // foldRightWithCont                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldRightWithCont01(): Bool = region r {
        Array.foldRightWithCont((e, k) -> (k() - e) * (e rem 2 + 1), 100, []) == 100
    }

    @test
    def foldRightWithCont02(): Bool = region r {
        Array.foldRightWithCont((e, k) -> (k() - e) * (e rem 2 + 1), 100, [1]) == 198
    }

    @test
    def foldRightWithCont03(): Bool = region r {
        Array.foldRightWithCont((e, k) -> (k() - e) * (e rem 2 + 1), 100, [1,2]) == 194
    }

    @test
    def foldRightWithCont04(): Bool = region r {
        Array.foldRightWithCont((e, k) -> (k() - e) * (e rem 2 + 1), 100, [1,2,3]) == 382
    }

    /////////////////////////////////////////////////////////////////////////////
    // foldMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldMap01(): Bool = region r {
        Array.foldMap(x -> 2 * x, []) == 0
    }

    @test
    def foldMap02(): Bool = region r {
        Array.foldMap(x -> 2 * x, [1, 2]) == 6
    }

    @test
    def foldMap03(): Bool = region r {
        Array.foldMap(x -> if (x == "a") "b" else x, ["a"]) == "b"
    }

    @test
    def foldMap04(): Bool = region r {
        Array.foldMap(x -> if (x == "c") "b" else x, ["a", "b", "c"]) == "abb"
    }

    @test
    def foldMap05(): Bool = region r {
        Array.foldMap(Int32.toString, [1, 2, 3]) == "123"
    }

    /////////////////////////////////////////////////////////////////////////////
    // reduceLeft                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reduceLeft01(): Bool = region r {
        Array.reduceLeft((a, b) -> a - b, []: Array[Int32, _]) == None
    }

    @test
    def reduceLeft02(): Bool = region r {
        Array.reduceLeft((a, b) -> a - b, [1]) == Some(1)
    }

    @test
    def reduceLeft03(): Bool = region r {
        Array.reduceLeft((a, b) -> a - b, [1,2]) == Some(-1)
    }

    @test
    def reduceLeft04(): Bool = region r {
        Array.reduceLeft((a, b) -> a - b, [1,2,3]) == Some(-4)
    }

    @test
    def reduceLeft05(): Bool = region r {
        Array.reduceLeft((a, b) -> a - b, [1,2,3,4]) == Some(-8)
    }

    /////////////////////////////////////////////////////////////////////////////
    // reduceRight                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reduceRight01(): Bool = region r {
        Array.reduceRight((a, b) -> a - b, []: Array[Int32, _]) == None
    }

    @test
    def reduceRight02(): Bool = region r {
        Array.reduceRight((a, b) -> a - b, [1]) == Some(1)
    }

    @test
    def reduceRight03(): Bool = region r {
        Array.reduceRight((a, b) -> a - b, [1,2]) == Some(-1)
    }

    @test
    def reduceRight04(): Bool = region r {
        Array.reduceRight((a, b) -> a - b, [1,2,3]) == Some(2)
    }

    @test
    def reduceRight05(): Bool = region r {
        Array.reduceRight((a, b) -> a - b, [1,2,3,4]) == Some(-2)
    }

    /////////////////////////////////////////////////////////////////////////////
    // count                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def count01(): Bool = region r {
        Array.count(i -> i > 3, []) == 0
    }

    @test
    def count02(): Bool = region r {
        Array.count(i -> i > 3, [1]) == 0
    }

    @test
    def count03(): Bool = region r {
        Array.count(i -> i > 3, [4]) == 1
    }

    @test
    def count04(): Bool = region r {
        Array.count(i -> i > 3, [1,2]) == 0
    }

    @test
    def count05(): Bool = region r {
        Array.count(i -> i > 3, [1,8]) == 1
    }

    @test
    def count06(): Bool = region r {
        Array.count(i -> i > 3, [8,1]) == 1
    }

    @test
    def count07(): Bool = region r {
        Array.count(i -> i > 3, [6,7]) == 2
    }


    /////////////////////////////////////////////////////////////////////////////
    // product                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def product01(): Bool = region r {
        Array.product([]) == 0
    }

    @test
    def product02(): Bool = region r {
        Array.product([1]) == 1
    }

    @test
    def product03(): Bool = region r {
        Array.product([1, 2, 3]) == 6
    }

    @test
    def product04(): Bool = region r {
        Array.product([1, 2, 3, -3]) == -18
    }

    @test
    def product05(): Bool = region r {
        Array.product([-1, -2, -3, -4]) == 24
    }

    @test
    def product06(): Bool = region r {
        Array.product([10, -10]) == -100
    }


    /////////////////////////////////////////////////////////////////////////////
    // productWith                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def productWith01(): Bool = region r {
        Array.productWith(x -> x + 1, []) == 0
    }

    @test
    def productWith02(): Bool = region r {
        Array.productWith(x -> x + 1, [1]) == 2
    }

    @test
    def productWith03(): Bool = region r {
        Array.productWith(x -> x + 1, [1, 2, 3]) == 24
    }

    @test
    def productWith04(): Bool = region r {
        Array.productWith(x -> x + 1, [1, 2, 3, -3]) == -48
    }

    @test
    def productWith05(): Bool = region r {
        Array.productWith(x -> x + 1, [-2, -3, -4, -5]) == 24
    }

    @test
    def productWith06(): Bool = region r {
        Array.productWith(x -> x + 1, [10, -10]) == -99
    }

    /////////////////////////////////////////////////////////////////////////////
    // flatten                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def flatten01(): Bool = region r {
        let a = Array.flatten([]: Array[Array[Int32, r], r]);
        Array.sameElements(a, []: Array[Int32, r])
    }

    @test
    def flatten02(): Bool = region r {
        let a = Array.flatten([[]]: Array[Array[Int32, r], r]);
        Array.sameElements(a, []: Array[Int32, r])
    }

    @test
    def flatten03(): Bool = region r {
        let a = Array.flatten([[1]]);
        Array.sameElements(a, [1])
    }

    @test
    def flatten04(): Bool = region r {
        let a= Array.flatten([[1,2]]);
        Array.sameElements(a, [1,2])
    }

    @test
    def flatten05(): Bool = region r {
        let a = Array.flatten([[],[]]: Array[Array[Int32, r], r]);
        Array.sameElements(a, []: Array[Int32, r])
    }

    @test
    def flatten06(): Bool = region r {
        let a = Array.flatten([[1],[]]);
        Array.sameElements(a, [1])
    }

    @test
    def flatten07(): Bool = region r {
        let a = Array.flatten([[],[1]]);
        Array.sameElements(a, [1])
    }

    @test
    def flatten08(): Bool = region r {
        let a = Array.flatten([[1],[2]]);
        Array.sameElements(a, [1,2])
    }

    @test
    def flatten09(): Bool = region r {
        let a = Array.flatten([[1,2],[3,4,5]]);
        Array.sameElements(a, [1,2,3,4,5])
    }

    @test
    def flatten10(): Bool = region r {
        let a = Array.flatten([[1],[2,3],[4]]);
        Array.sameElements(a, [1,2,3,4])
    }
    /////////////////////////////////////////////////////////////////////////////
    // exists                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def exists01(): Bool = region r {
        Array.exists(i -> i > 3, []: Array[Int32, _]) == false
    }

    @test
    def exists02(): Bool = region r {
        Array.exists(i -> i > 3, [1]) == false
    }

    @test
    def exists03(): Bool = region r {
        Array.exists(i -> i > 3, [5]) == true
    }

    @test
    def exists04(): Bool = region r {
        Array.exists(i -> i > 3, [1,2]) == false
    }

    @test
    def exists05(): Bool = region r {
        Array.exists(i -> i > 3, [1,6]) == true
    }

    @test
    def exists06(): Bool = region r {
        Array.exists(i -> i > 3, [6,1]) == true
    }

    @test
    def exists07(): Bool = region r {
        Array.exists(i -> i > 3, [16,6]) == true
    }

    @test
    def exists08(): Bool = region r {
        Array.exists(i -> i > 3, [1,-9,3]) == false
    }

    @test
    def exists09(): Bool = region r {
        Array.exists(i -> i > 3, [1,9,3]) == true
    }

    /////////////////////////////////////////////////////////////////////////////
    // forall                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def forall01(): Bool = region r {
        Array.forall(i -> i > 3, []: Array[Int32, _]) == true
    }

    @test
    def forall02(): Bool = region r {
        Array.forall(i -> i > 3, [1]) == false
    }

    @test
    def forall03(): Bool = region r {
        Array.forall(i -> i > 3, [5]) == true
    }

    @test
    def forall04(): Bool = region r {
        Array.forall(i -> i > 3, [1,2]) == false
    }

    @test
    def forall05(): Bool = region r {
        Array.forall(i -> i > 3, [1,6]) == false
    }

    @test
    def forall06(): Bool = region r {
        Array.forall(i -> i > 3, [6,1]) == false
    }

    @test
    def forall07(): Bool = region r {
        Array.forall(i -> i > 3, [16,6]) == true
    }

    @test
    def forall08(): Bool = region r {
        Array.forall(i -> i > 3, [1,-9,3]) == false
    }

    @test
    def forall09(): Bool = region r {
        Array.forall(i -> i > 3, [1,9,3]) == false
    }

    @test
    def forall10(): Bool = region r {
        Array.forall(i -> i > 3, [11,9,31]) == true
    }

    /////////////////////////////////////////////////////////////////////////////
    // filter                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filter01(): Bool = region r {
        let a = Array.filter(i -> i > 3, []: Array[Int32, _]);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def filter02(): Bool = region r {
        let a = Array.filter(i -> i > 3, [2]);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def filter03(): Bool = region r {
        let a = Array.filter(i -> i > 3, [4]);
        Array.sameElements(a, [4])
    }

    @test
    def filter04(): Bool = region r {
        let a = Array.filter(i -> i > 3, [1,3]);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def filter05(): Bool = region r {
        let a = Array.filter(i -> i > 3, [1,8]);
        Array.sameElements(a, [8])
    }

    @test
    def filter06(): Bool = region r {
        let a = Array.filter(i -> i > 3, [8,1]);
        Array.sameElements(a, [8])
    }

    @test
    def filter07(): Bool = region r {
        let a = Array.filter(i -> i > 3, [8,9]);
        Array.sameElements(a, [8,9])
    }

    @test
    def filter08(): Bool = region r {
        let a = Array.filter(i -> i > 3, [1,4,11,2,-22,17]);
        Array.sameElements(a, [4,11,17])
    }

    /////////////////////////////////////////////////////////////////////////////
    // partition                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def partition01(): Bool = region r {
        let (a,b) = Array.partition(i -> i > 3, []: Array[Int32, _]);
        Array.sameElements(a, []: Array[Int32, _]) and Array.sameElements(b, []: Array[Int32, _])
    }

    @test
    def partition02(): Bool = region r {
        let (a,b) = Array.partition(i -> i > 3, [1]);
        Array.sameElements(a, []: Array[Int32, _]) and Array.sameElements(b, [1])
    }

    @test
    def partition03(): Bool = region r {
        let (a,b) = Array.partition(i -> i > 3, [4]);
        Array.sameElements(a, [4]) and Array.sameElements(b, []: Array[Int32, _])
    }

    @test
    def partition04(): Bool = region r {
        let (a,b) = Array.partition(i -> i > 3, [1,2]);
        Array.sameElements(a, []: Array[Int32, _]) and Array.sameElements(b, [1,2])
    }

    @test
    def partition05(): Bool = region r {
        let (a,b) = Array.partition(i -> i > 3, [1,5]);
        Array.sameElements(a, [5]) and Array.sameElements(b, [1])
    }

    @test
    def partition06(): Bool = region r {
        let (a,b) = Array.partition(i -> i > 3, [5,1]);
        Array.sameElements(a, [5]) and Array.sameElements(b, [1])
    }

    @test
    def partition07(): Bool = region r {
        let (a,b) = Array.partition(i -> i > 3, [5,8]);
        Array.sameElements(a, [5,8]) and Array.sameElements(b, []: Array[Int32, _])
    }

    @test
    def partition08(): Bool = region r {
        let (a,b) = Array.partition(i -> i > 3, [4, -3, -5, 1, 2, 16, 7, 1, 7]);
        Array.sameElements(a, [4, 16, 7, 7]) and Array.sameElements(b, [-3, -5, 1, 2, 1])
    }

    /////////////////////////////////////////////////////////////////////////////
    // span                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def span01(): Bool = region r {
        let (a,b) = Array.span(i -> i > 3, []: Array[Int32, _]);
        Array.sameElements(a, []: Array[Int32, _]) and Array.sameElements(b, []: Array[Int32, _])
    }

    @test
    def span02(): Bool = region r {
        let (a,b) = Array.span(i -> i > 3, [1]);
        Array.sameElements(a,[]: Array[Int32, _]) and Array.sameElements(b, [1])
    }

    @test
    def span03(): Bool = region r {
        let (a,b) = Array.span(i -> i > 3, [4]);
        Array.sameElements(a, [4]) and Array.sameElements(b, []: Array[Int32, _])
    }

    @test
    def span04(): Bool = region r {
        let (a,b) = Array.span(i -> i > 3, [1,2]);
        Array.sameElements(a, []: Array[Int32, _]) and Array.sameElements(b, [1,2])
    }

    @test
    def span05(): Bool = region r {
        let (a,b) = Array.span(i -> i > 3, [1,5]);
        Array.sameElements(a, []: Array[Int32, _]) and Array.sameElements(b, [1,5])
    }

    @test
    def span06(): Bool = region r {
        let (a,b) = Array.span(i -> i > 3, [5,1]);
        Array.sameElements(a, [5]) and Array.sameElements(b, [1])
    }

    @test
    def span07(): Bool = region r {
        let (a,b) = Array.span(i -> i > 3, [5,8]);
        Array.sameElements(a, [5,8]) and Array.sameElements(b, []: Array[Int32, _])
    }

    @test
    def span08(): Bool = region r {
        let (a,b) = Array.span(i -> i > 3, [4, 6, -3, 11, -5, 1, 2, 16, 7, 1, 7]);
        Array.sameElements(a, [4, 6]) and Array.sameElements(b, [-3, 11, -5, 1, 2, 16, 7, 1, 7])
    }

    /////////////////////////////////////////////////////////////////////////////
    // drop                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def drop01(): Bool = region r {
        let a = Array.drop(-1, []: Array[Int32, _]);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def drop02(): Bool = region r {
        let a = Array.drop(0, []: Array[Int32, _]);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def drop03(): Bool = region r {
        let a = Array.drop(1, []: Array[Int32, _]);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def drop04(): Bool = region r {
        let a = Array.drop(-1, [1]);
        Array.sameElements(a, [1])
    }

    @test
    def drop05(): Bool = region r {
        let a = Array.drop(0, [1]);
        Array.sameElements(a, [1])
    }

    @test
    def drop06(): Bool = region r {
        let a = Array.drop(1, [1]);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def drop07(): Bool = region r {
        let a = Array.drop(2, [1]);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def drop08(): Bool = region r {
        let a = Array.drop(0, [1,2]);
        Array.sameElements(a, [1,2])
    }

    @test
    def drop09(): Bool = region r {
        let a = Array.drop(1, [1,2]);
        Array.sameElements(a, [2])
    }

    @test
    def drop10(): Bool = region r {
        let a = Array.drop(2, [1,2]);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def drop11(): Bool = region r {
        let a = Array.drop(2, [1,2,3,4,5,6]);
        Array.sameElements(a, [3,4,5,6])
    }

    @test
    def drop12(): Bool = region r {
        let a = Array.drop(4, [1,2,3,4,5,6]);
        Array.sameElements(a, [5,6])
    }

    /////////////////////////////////////////////////////////////////////////////
    // dropLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def dropLeft01(): Bool = region r {
        let a = Array.dropLeft(-1, []: Array[Int32, _]);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def dropLeft02(): Bool = region r {
        let a = Array.dropLeft(0, []: Array[Int32, _]);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def dropLeft03(): Bool = region r {
        let a = Array.dropLeft(1, []: Array[Int32, _]);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def dropLeft04(): Bool = region r {
        let a = Array.dropLeft(-1, [1]);
        Array.sameElements(a, [1])
    }

    @test
    def dropLeft05(): Bool = region r {
        let a = Array.dropLeft(0, [1]);
        Array.sameElements(a, [1])
    }

    @test
    def dropLeft06(): Bool = region r {
        let a = Array.dropLeft(1, [1]);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def dropLeft07(): Bool = region r {
        let a = Array.dropLeft(2, [1]);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def dropLeft08(): Bool = region r {
        let a = Array.dropLeft(0, [1,2]);
        Array.sameElements(a, [1,2])
    }

    @test
    def dropLeft09(): Bool = region r {
        let a = Array.dropLeft(1, [1,2]);
        Array.sameElements(a, [2])
    }

    @test
    def dropLeft10(): Bool = region r {
        let a = Array.dropLeft(2, [1,2]);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def dropLeft11(): Bool = region r {
        let a = Array.dropLeft(2, [1,2,3,4,5,6]);
        Array.sameElements(a, [3,4,5,6])
    }

    @test
    def dropLeft12(): Bool = region r {
        let a = Array.dropLeft(4, [1,2,3,4,5,6]);
        Array.sameElements(a, [5,6])
    }

    /////////////////////////////////////////////////////////////////////////////
    // dropRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def dropRight01(): Bool = region r {
        let a = Array.dropRight(-1, []: Array[Int32, _]);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def dropRight02(): Bool = region r {
        let a = Array.dropRight(0, []: Array[Int32, _]);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def dropRight03(): Bool = region r {
        let a = Array.dropRight(1, []: Array[Int32, _]);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def dropRight04(): Bool = region r {
        let a = Array.dropRight(-1, [1]);
        Array.sameElements(a, [1])
    }

    @test
    def dropRight05(): Bool = region r {
        let a = Array.dropRight(0, [1]);
        Array.sameElements(a, [1])
    }

    @test
    def dropRight06(): Bool = region r {
        let a = Array.dropRight(1, [1]);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def dropRight07(): Bool = region r {
        let a = Array.dropRight(2, [1]);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def dropRight08(): Bool = region r {
        let a = Array.dropRight(0, [1,2]);
        Array.sameElements(a, [1,2])
    }

    @test
    def dropRight09(): Bool = region r {
        let a = Array.dropRight(1, [1,2]);
        Array.sameElements(a, [1])
    }

    @test
    def dropRight10(): Bool = region r {
        let a = Array.dropRight(2, [1,2]);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def dropRight11(): Bool = region r {
        let a = Array.dropRight(2, [1,2,3,4,5,6]);
        Array.sameElements(a, [1,2,3,4])
    }

    @test
    def dropRight12(): Bool = region r {
        let a = Array.dropRight(4, [1,2,3,4,5,6]);
        Array.sameElements(a, [1,2])
    }

    /////////////////////////////////////////////////////////////////////////////
    // dropWhile                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def dropWhile01(): Bool = region r {
        let a = Array.dropWhile(i -> i > 3, []);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def dropWhile02(): Bool = region r {
        let a = Array.dropWhile(i -> i > 3, [1]);
        Array.sameElements(a, [1])
    }

    @test
    def dropWhile03(): Bool = region r {
        let a = Array.dropWhile(i -> i > 3, [4]);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def dropWhile04(): Bool = region r {
        let a = Array.dropWhile(i -> i > 3, [1,2]);
        Array.sameElements(a, [1,2])
    }

    @test
    def dropWhile05(): Bool = region r {
        let a = Array.dropWhile(i -> i > 3, [1,5]);
        Array.sameElements(a, [1,5])
    }

    @test
    def dropWhile06(): Bool = region r {
        let a = Array.dropWhile(i -> i > 3, [5,1]);
        Array.sameElements(a, [1])
    }

    @test
    def dropWhile07(): Bool = region r {
        let a = Array.dropWhile(i -> i > 3, [5,8]);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def dropWhile08(): Bool = region r {
        let a = Array.dropWhile(i -> i > 3, [4, 6, -3, 11, -5, 1, 2, 16, 7, 1, 7]);
        Array.sameElements(a, [-3, 11, -5, 1, 2, 16, 7, 1, 7])
    }

    /////////////////////////////////////////////////////////////////////////////
    // dropWhileLeft                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def dropWhileLeft01(): Bool = region r {
        let a = Array.dropWhileLeft(i -> i > 3, []);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def dropWhileLeft02(): Bool = region r {
        let a = Array.dropWhileLeft(i -> i > 3, [1]);
        Array.sameElements(a, [1])
    }

    @test
    def dropWhileLeft03(): Bool = region r {
        let a = Array.dropWhileLeft(i -> i > 3, [4]);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def dropWhileLeft04(): Bool = region r {
        let a = Array.dropWhileLeft(i -> i > 3, [1,2]);
        Array.sameElements(a, [1,2])
    }

    @test
    def dropWhileLeft05(): Bool = region r {
        let a = Array.dropWhileLeft(i -> i > 3, [1,5]);
        Array.sameElements(a, [1,5])
    }

    @test
    def dropWhileLeft06(): Bool = region r {
        let a = Array.dropWhileLeft(i -> i > 3, [5,1]);
        Array.sameElements(a, [1])
    }

    @test
    def dropWhileLeft07(): Bool = region r {
        let a = Array.dropWhileLeft(i -> i > 3, [5,8]);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def dropWhileLeft08(): Bool = region r {
        let a = Array.dropWhileLeft(i -> i > 3, [4, 6, -3, 11, -5, 1, 2, 16, 7, 1, 7]);
        Array.sameElements(a, [-3, 11, -5, 1, 2, 16, 7, 1, 7])
    }

    /////////////////////////////////////////////////////////////////////////////
    // dropWhileRight                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def dropWhileRight01(): Bool = region r {
        let a = Array.dropWhileRight(i -> i > 3, []);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def dropWhileRight02(): Bool = region r {
        let a = Array.dropWhileRight(i -> i > 3, [1]);
        Array.sameElements(a, [1])
    }

    @test
    def dropWhileRight03(): Bool = region r {
        let a = Array.dropWhileRight(i -> i > 3, [4]);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def dropWhileRight04(): Bool = region r {
        let a = Array.dropWhileRight(i -> i > 3, [1,2]);
        Array.sameElements(a, [1,2])
    }

    @test
    def dropWhileRight05(): Bool = region r {
        let a = Array.dropWhileRight(i -> i > 3, [1,5]);
        Array.sameElements(a, [1])
    }

    @test
    def dropWhileRight06(): Bool = region r {
        let a = Array.dropWhileRight(i -> i > 3, [5,1]);
        Array.sameElements(a, [5,1])
    }

    @test
    def dropWhileRight07(): Bool = region r {
        let a = Array.dropWhileRight(i -> i > 3, [5,8]);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def dropWhileRight08(): Bool = region r {
        let a = Array.dropWhileRight(i -> i > 3, [4, 6, -3, 11, -5, 1, 2, 16, 7, 1, 7]);
        Array.sameElements(a, [4, 6, -3, 11, -5, 1, 2, 16, 7, 1])
    }

    /////////////////////////////////////////////////////////////////////////////
    // take                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def take01(): Bool = region r {
        let a = Array.take(-1, []: Array[Int32, _]);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def take02(): Bool = region r {
        let a = Array.take(0, []: Array[Int32, _]);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def take03(): Bool = region r {
        let a = Array.take(1, []: Array[Int32, _]);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def take04(): Bool = region r {
        let a = Array.take(-1, [1]);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def take05(): Bool = region r {
        let a = Array.take(0, [1]);
        Array.sameElements(a, []: Array[Int32, _])

    }
    @test
    def take06(): Bool = region r {
        let a = Array.take(1, [1]);
        Array.sameElements(a, [1])

    }
    @test
    def take07(): Bool = region r {
        let a = Array.take(2, [1]);
        Array.sameElements(a, [1])

    }
    @test
    def take08(): Bool = region r {
        let a = Array.take(0, [1,2]);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def take09(): Bool = region r {
        let a = Array.take(1, [1,2]);
        Array.sameElements(a, [1])
    }

    @test
    def take10(): Bool = region r {
        let a = Array.take(2, [1,2]);
        Array.sameElements(a, [1,2])
    }

    @test
    def take11(): Bool = region r {
        let a = Array.take(2, [1,2,3,4,5,6]);
        Array.sameElements(a, [1,2])
    }

    @test
    def take12(): Bool = region r {
        let a = Array.take(4, [1,2,3,4,5,6]);
        Array.sameElements(a, [1,2,3,4])
    }

    /////////////////////////////////////////////////////////////////////////////
    // takeLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def takeLeft01(): Bool = region r {
        let a = Array.takeLeft(-1, []: Array[Int32, _]);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def takeLeft02(): Bool = region r {
        let a = Array.takeLeft(0, []: Array[Int32, _]);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def takeLeft03(): Bool = region r {
        let a = Array.takeLeft(1, []: Array[Int32, _]);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def takeLeft04(): Bool = region r {
        let a = Array.takeLeft(-1, [1]);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def takeLeft05(): Bool = region r {
        let a = Array.takeLeft(0, [1]);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def takeLeft06(): Bool = region r {
        let a = Array.takeLeft(1, [1]);
        Array.sameElements(a, [1])
    }

    @test
    def takeLeft07(): Bool = region r {
        let a = Array.takeLeft(2, [1]);
        Array.sameElements(a, [1])
    }

    @test
    def takeLeft08(): Bool = region r {
        let a = Array.takeLeft(0, [1,2]);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def takeLeft09(): Bool = region r {
        let a = Array.takeLeft(1, [1,2]);
        Array.sameElements(a, [1])
    }

    @test
    def takeLeft10(): Bool = region r {
        let a = Array.takeLeft(2, [1,2]);
        Array.sameElements(a, [1,2])
    }

    @test
    def takeLeft11(): Bool = region r {
        let a = Array.takeLeft(2, [1,2,3,4,5,6]);
        Array.sameElements(a, [1,2])
    }

    @test
    def takeLeft12(): Bool = region r {
        let a = Array.takeLeft(4, [1,2,3,4,5,6]);
        Array.sameElements(a, [1,2,3,4])
    }

    /////////////////////////////////////////////////////////////////////////////
    // takeRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def takeRight01(): Bool = region r {
        let a = Array.takeRight(-1, []: Array[Int32, _]);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def takeRight02(): Bool = region r {
        let a = Array.takeRight(0, []: Array[Int32, _]);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def takeRight03(): Bool = region r {
        let a = Array.takeRight(1, []: Array[Int32, _]);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def takeRight04(): Bool = region r {
        let a = Array.takeRight(-1, [1]);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def takeRight05(): Bool = region r {
        let a = Array.takeRight(0, [1]);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def takeRight06(): Bool = region r {
        let a = Array.takeRight(1, [1]);
        Array.sameElements(a, [1])
    }

    @test
    def takeRight07(): Bool = region r {
        let a = Array.takeRight(2, [1]);
        Array.sameElements(a, [1])
    }

    @test
    def takeRight08(): Bool = region r {
        let a = Array.takeRight(0, [1,2]);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def takeRight09(): Bool = region r {
        let a = Array.takeRight(1, [1,2]);
        Array.sameElements(a, [2])
    }

    @test
    def takeRight10(): Bool = region r {
        let a = Array.takeRight(2, [1,2]);
        Array.sameElements(a, [1,2])
    }

    @test
    def takeRight11(): Bool = region r {
        let a = Array.takeRight(2, [1,2,3,4,5,6]);
        Array.sameElements(a, [5,6])
    }

    @test
    def takeRight12(): Bool = region r {
        let a = Array.takeRight(4, [1,2,3,4,5,6]);
        Array.sameElements(a, [3,4,5,6])
    }

    /////////////////////////////////////////////////////////////////////////////
    // takeWhile                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def takeWhile01(): Bool = region r {
        let a = Array.takeWhile(i -> i > 3, []: Array[Int32, _]);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def takeWhile02(): Bool = region r {
        let a = Array.takeWhile(i -> i > 3, [1]);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def takeWhile03(): Bool = region r {
        let a = Array.takeWhile(i -> i > 3, [4]);
        Array.sameElements(a, [4])
    }

    @test
    def takeWhile04(): Bool = region r {
        let a = Array.takeWhile(i -> i > 3, [1,2]);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def takeWhile05(): Bool = region r {
        let a = Array.takeWhile(i -> i > 3, [1,5]);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def takeWhile06(): Bool = region r {
        let a = Array.takeWhile(i -> i > 3, [5,1]);
        Array.sameElements(a, [5])
    }

    @test
    def takeWhile07(): Bool = region r {
        let a = Array.takeWhile(i -> i > 3, [5,8]);
        Array.sameElements(a, [5,8])
    }

    @test
    def takeWhile08(): Bool = region r {
        let a = Array.takeWhile(i -> i > 3, [4, 6, -3, 11, -5, 1, 2, 16, 7, 1, 7]);
        Array.sameElements(a, [4, 6])
    }

    /////////////////////////////////////////////////////////////////////////////
    // takeWhileLeft                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def takeWhileLeft01(): Bool = region r {
        let a = Array.takeWhileLeft(i -> i > 3, []: Array[Int32, _]);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def takeWhileLeft02(): Bool = region r {
        let a = Array.takeWhileLeft(i -> i > 3, [1]);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def takeWhileLeft03(): Bool = region r {
        let a = Array.takeWhileLeft(i -> i > 3, [4]);
        Array.sameElements(a, [4])
    }

    @test
    def takeWhileLeft04(): Bool = region r {
        let a = Array.takeWhileLeft(i -> i > 3, [1,2]);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def takeWhileLeft05(): Bool = region r {
        let a = Array.takeWhileLeft(i -> i > 3, [1,5]);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def takeWhileLeft06(): Bool = region r {
        let a = Array.takeWhileLeft(i -> i > 3, [5,1]);
        Array.sameElements(a, [5])
    }

    @test
    def takeWhileLeft07(): Bool = region r {
        let a = Array.takeWhileLeft(i -> i > 3, [5,8]);
        Array.sameElements(a, [5,8])
    }

    @test
    def takeWhileLeft08(): Bool = region r {
        let a = Array.takeWhileLeft(i -> i > 3, [4, 6, -3, 11, -5, 1, 2, 16, 7, 1, 7]);
        Array.sameElements(a, [4, 6])
    }

    /////////////////////////////////////////////////////////////////////////////
    // takeWhileRight                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def takeWhileRight01(): Bool = region r {
        let a = Array.takeWhileRight(i -> i > 3, []: Array[Int32, _]);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def takeWhileRight02(): Bool = region r {
        let a = Array.takeWhileRight(i -> i > 3, [1]);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def takeWhileRight03(): Bool = region r {
        let a = Array.takeWhileRight(i -> i > 3, [4]);
        Array.sameElements(a, [4])
    }

    @test
    def takeWhileRight04(): Bool = region r {
        let a = Array.takeWhileRight(i -> i > 3, [1,2]);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def takeWhileRight05(): Bool = region r {
        let a = Array.takeWhileRight(i -> i > 3, [1,5]);
        Array.sameElements(a, [5])
    }

    @test
    def takeWhileRight06(): Bool = region r {
        let a = Array.takeWhileRight(i -> i > 3, [5,1]);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def takeWhileRight07(): Bool = region r {
        let a = Array.takeWhileRight(i -> i > 3, [5,8]);
        Array.sameElements(a, [5,8])
    }

    @test
    def takeWhileRight08(): Bool = region r {
        let a = Array.takeWhileRight(i -> i > 3, [4, 6, -3, 11, -5, 1, 2, 16, 7, 1, 7]);
        Array.sameElements(a, [7])
    }

    /////////////////////////////////////////////////////////////////////////////
    // groupBy                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def groupBy01(): Bool = region r {
        let arr = Array.groupBy((a, b) -> a > 3 or b > 8, []: Array[Int32, _], r);
        let xs = Array.toList(arr) |> List.map(Array.toList);
        xs == Nil
    }

    @test
    def groupBy02(): Bool = region r {
        let arr = Array.groupBy((a, b) -> a > 3 or b > 8, [1], r);
        let xs = Array.toList(arr) |> List.map(Array.toList);
        xs == (1 :: Nil) :: Nil
    }

    @test
    def groupBy03(): Bool = region r {
        let arr = Array.groupBy((a, b) -> a > 3 or b > 8, [1,4], r);
        let xs = Array.toList(arr) |> List.map(Array.toList);
        xs == (1 :: Nil) :: (4 :: Nil) :: Nil
    }

    @test
    def groupBy04(): Bool = region r {
        let arr = Array.groupBy((a, b) -> a > 3 or b > 8, [1,9], r);
        let xs = Array.toList(arr) |> List.map(Array.toList);
        xs == (1 :: 9 :: Nil) :: Nil
    }

    @test
    def groupBy05(): Bool = region r {
        let arr = Array.groupBy((a, b) -> a > 3 or b > 8, [1, 4, 7, 6, 9, 2, 4, 4, 8, 16], r);
        let xs = Array.toList(arr) |> List.map(Array.toList);
        xs == (1 :: 9 :: 16 :: Nil) :: (4 :: 7 :: 6 :: 4 :: 4 :: 8 :: Nil) :: (2 :: Nil) :: Nil
    }

    @test
    def groupBy06(): Bool = region r {
        let arr = Array.groupBy((a, b) -> a > -6 or a*b >= 0, [-1, -11, 4, -11, 0, 8, 2, 1, -3, -24], r);
        let xs = Array.toList(arr) |> List.map(Array.toList);
        xs == (-1 :: -11 :: -11 :: 0 :: -3 :: -24 :: Nil) :: (4 :: 8 :: 2 :: 1 :: Nil) :: Nil
    }

    @test
    def groupBy07(): Bool = region r {
        let arr = Array.groupBy((a, b) -> a < 0 or (a > 10 or (b > 10 or a == b)), [-5, 6, 11, 8, 8, -11, -1, 0, 4, -1], r);
        let xs = Array.toList(arr) |> List.map(Array.toList);
        xs == (-5 :: 11 :: -11 :: -1 :: -1 :: Nil) :: (6 :: Nil) :: (8 :: 8 :: Nil) :: (0 :: Nil) :: (4 :: Nil) :: Nil
    }

    /////////////////////////////////////////////////////////////////////////////
    // zip                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def zip01(): Bool = region r {
        let a = Array.zip([]: Array[Int32, _], []: Array[Int32, _]);
        Array.sameElements(a, []: Array[(Int32, Int32), _])
    }

    @test
    def zip02(): Bool = region r {
        let a = Array.zip([1], []: Array[Int32, _]);
        Array.sameElements(a, []: Array[(Int32, Int32), _])
    }

    @test
    def zip03(): Bool = region r {
        let a = Array.zip([]: Array[Int32, _], [2]);
        Array.sameElements(a, []: Array[(Int32, Int32), _])
    }

    @test
    def zip04(): Bool = region r {
        let a = Array.zip([1], [2]);
        Array.sameElements(a, [(1, 2)])
    }

    @test
    def zip05(): Bool = region r {
        let a = Array.zip([1,3], [2,4]);
        Array.sameElements(a, [(1, 2), (3, 4)])
    }

    @test
    def zip06(): Bool = region r {
        let a = Array.zip([1,3,5], [2,4,6]);
        Array.sameElements(a, [(1, 2), (3, 4), (5, 6)])
    }

    @test
    def zip07(): Bool = region r {
        let a = Array.zip([1,3,5,7], [2,4,6,8]);
        Array.sameElements(a, [(1, 2), (3, 4), (5, 6), (7, 8)])
    }

    /////////////////////////////////////////////////////////////////////////////
    // zipWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def zipWith01(): Bool = region r {
        let arr = Array.zipWith((a, b) -> if (b) a + 1 else a, []: Array[Int32, _], []: Array[Bool, _]);
        Array.sameElements(arr, []: Array[Int32, _])
    }

    @test
    def zipWith02(): Bool = region r {
        let arr = Array.zipWith((a, b) -> if (b) a + 1 else a, [1], []: Array[Bool, _]);
        Array.sameElements(arr, []: Array[Int32, _])
    }

    @test
    def zipWith03(): Bool = region r {
        let arr = Array.zipWith((a, b) -> if (b) a + 1 else a, []: Array[Int32, _], [true]);
        Array.sameElements(arr, []: Array[Int32, _])
    }

    @test
    def zipWith04(): Bool = region r {
        let arr = Array.zipWith((a, b) -> if (b) a + 1 else a, [1], [true]);
        Array.sameElements(arr, [2])
    }

    @test
    def zipWith05(): Bool = region r {
        let arr = Array.zipWith((a, b) -> if (b) a + 1 else a, [1], [false]);
        Array.sameElements(arr, [1])
    }

    @test
    def zipWith06(): Bool = region r {
        let arr = Array.zipWith((a, b) -> if (b) a + 1 else a,
                        [1, 2, 3, 4, 5, 6, 7, 8],
                        [false, true, true, false, false, true, true, true]);
        Array.sameElements(arr, [1, 3, 4, 4, 5, 7, 8, 9])
    }

    /////////////////////////////////////////////////////////////////////////////
    // unzip                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def unzip01(): Bool = region r {
        let (a,b) = Array.unzip([]: Array[(Unit, Unit), _]);
        Array.sameElements(a, []) and Array.sameElements(b, [])
    }

    @test
    def unzip02(): Bool = region r {
        let (a,b) = Array.unzip([(1, true)]);
        Array.sameElements(a, [1]) and Array.sameElements(b, [true])
    }

    @test
    def unzip03(): Bool = region r {
        let (a,b) = Array.unzip([(1, true), (2, true)]);
        Array.sameElements(a, [1,2])  and Array.sameElements(b, [true,true])
    }

    @test
    def unzip04(): Bool = region r {
        let (a,b) = Array.unzip([(1, true), (2, true), (3, false)]);
        Array.sameElements(a, [1,2,3]) and Array.sameElements(b, [true,true,false])
    }

    /////////////////////////////////////////////////////////////////////////////
    // fold2                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def fold201(): Bool = region r {
        Array.fold2((c, a, b) -> if (b) a + c else a * c, 4, []: Array[Int32, _], []: Array[Bool, _]) == 4
    }

    @test
    def fold202(): Bool = region r {
        Array.fold2((c, a, b) -> if (b) a + c else a * c, 4, [1], []) == 4
    }

    @test
    def fold203(): Bool = region r {
        Array.fold2((c, a, b) -> if (b) a + c else a * c, 4, [], [true]) == 4
    }

    @test
    def fold204(): Bool = region r {
        Array.fold2((c, a, b) -> if (b) a + c else a * c, 4, [2], [true]) == 6
    }

    @test
    def fold205(): Bool = region r {
        Array.fold2((c, a, b) -> if (b) a + c else a * c, 4, [2], [false]) == 8
    }

    @test
    def fold206(): Bool = region r {
        Array.fold2((c, a, b) -> if (b) a + c else a * c, 4, [3,2], [true,true]) == 9
    }

    @test
    def fold207(): Bool = region r {
        Array.fold2((c, a, b) -> if (b) a + c else a * c, 4, [3,2], [true,false]) == 14
    }

    @test
    def fold208(): Bool = region r {
        Array.fold2((c, a, b) -> if (b) a + c else a * c, 4, [3,2], [false,true]) == 14
    }

    @test
    def fold209(): Bool = region r {
        Array.fold2((c, a, b) -> if (b) a + c else a * c, 4, [3,2], [false,false]) == 24
    }

    @test
    def fold210(): Bool = region r {
        Array.fold2((c, a, b) -> if (b) a + c else a * c, 4, [6,-4,3,2],
            [true,false,false,true]) == -118
    }

    /////////////////////////////////////////////////////////////////////////////
    // foldLeft2                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldLeft201(): Bool = region r {
        Array.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, []: Array[Int32, _], []: Array[Bool, _]) == 4
    }

    @test
    def foldLeft202(): Bool = region r {
        Array.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, [1], []: Array[Bool, _]) == 4
    }

    @test
    def foldLeft203(): Bool = region r {
        Array.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, []: Array[Int32, _], [true]) == 4
    }

    @test
    def foldLeft204(): Bool = region r {
        Array.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, [2], [true]) == 6
    }

    @test
    def foldLeft205(): Bool = region r {
        Array.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, [2], [false]) == 8
    }

    @test
    def foldLeft206(): Bool = region r {
        Array.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, [3,2], [true,true]) == 9
    }

    @test
    def foldLeft207(): Bool = region r {
        Array.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, [3,2], [true,false]) == 14
    }

    @test
    def foldLeft208(): Bool = region r {
        Array.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, [3,2], [false,true]) == 14
    }

    @test
    def foldLeft209(): Bool = region r {
        Array.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, [3,2], [false,false]) == 24
    }

    @test
    def foldLeft210(): Bool = region r {
        Array.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, [6,-4,3,2],
            [true,false,false,true]) == -118
    }

    /////////////////////////////////////////////////////////////////////////////
    // foldRight2                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldRight201(): Bool = region r {
        Array.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, []: Array[Int32, _], []: Array[Bool, _]) == 4
    }

    @test
    def foldRight202(): Bool = region r {
        Array.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, [1], []: Array[Bool, _]) == 4
    }

    @test
    def foldRight203(): Bool = region r {
        Array.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, []: Array[Int32, _], [true]) == 4
    }

    @test
    def foldRight204(): Bool = region r {
        Array.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, [2], [true]) == 6
    }

    @test
    def foldRight205(): Bool = region r {
        Array.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, [2], [false]) == 8
    }

    @test
    def foldRight206(): Bool = region r {
        Array.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, [3,2], [true,true]) == 9
    }

    @test
    def foldRight207(): Bool = region r {
        Array.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, [3,2], [true,false]) == 11
    }

    @test
    def foldRight208(): Bool = region r {
        Array.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, [3,2], [false,true]) == 18
    }

    @test
    def foldRight209(): Bool = region r {
        Array.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, [3,2], [false,false]) == 24
    }

    @test
    def foldRight210(): Bool = region r {
        Array.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, [6,-4,3,2],
            [true,false,false,true]) == -66
    }

    /////////////////////////////////////////////////////////////////////////////
    // filterMap                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filterMap01(): Bool = region r {
        let a = Array.filterMap(i -> if (i rem 2 == 0) Some(i/2) else None, []: Array[Int32, _]);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def filterMap02(): Bool = region r {
        let a = Array.filterMap(i -> if (i rem 2 == 0) Some(i/2) else None, [1]);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def filterMap03(): Bool = region r {
        let a = Array.filterMap(i -> if (i rem 2 == 0) Some(i/2) else None, [2]);
        Array.sameElements(a, [1])
    }

    @test
    def filterMap04(): Bool = region r {
        let a = Array.filterMap(i -> if (i rem 2 == 0) Some(i/2) else None, [1,3]);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def filterMap05(): Bool = region r {
        let a = Array.filterMap(i -> if (i rem 2 == 0) Some(i/2) else None, [1,4]);
        Array.sameElements(a, [2])
    }

    @test
    def filterMap06(): Bool = region r {
        let a = Array.filterMap(i -> if (i rem 2 == 0) Some(i/2) else None, [6,-1]);
        Array.sameElements(a, [3])
    }

    @test
    def filterMap07(): Bool = region r {
        let a = Array.filterMap(i -> if (i rem 2 == 0) Some(i/2) else None, [8,6]);
        Array.sameElements(a, [4,3])
    }

    @test
    def filterMap08(): Bool = region r {
        let a = Array.filterMap(i -> if (i rem 2 == 0) Some(i/2) else None, [0,1,2,3,4,5,10]);
        Array.sameElements(a, [0,1,2,5])
    }

    /////////////////////////////////////////////////////////////////////////////
    // findMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def findMap01(): Bool = region r {
        Array.findMap(i -> if (i rem 2 == 0) Some(i/2) else None, []: Array[Int32, _]) == None
    }

    @test
    def findMap02(): Bool = region r {
        Array.findMap(i -> if (i rem 2 == 0) Some(i/2) else None, [1]) == None
    }

    @test
    def findMap03(): Bool = region r {
        Array.findMap(i -> if (i rem 2 == 0) Some(i/2) else None, [2]) == Some(1)
    }

    @test
    def findMap04(): Bool = region r {
        Array.findMap(i -> if (i rem 2 == 0) Some(i/2) else None, [1,3]) == None
    }

    @test
    def findMap05(): Bool = region r {
        Array.findMap(i -> if (i rem 2 == 0) Some(i/2) else None, [1,4]) == Some(2)
    }

    @test
    def findMap06(): Bool = region r {
        Array.findMap(i -> if (i rem 2 == 0) Some(i/2) else None, [6,-1]) == Some(3)
    }

    @test
    def findMap07(): Bool = region r {
        Array.findMap(i -> if (i rem 2 == 0) Some(i/2) else None, [8,6]) == Some(4)
    }

    @test
    def findMap08(): Bool = region r {
        Array.findMap(i -> if (i rem 2 == 0) Some(i/2) else None, [0,1,2,3,4,5,10]) == Some(0)
    }

    /////////////////////////////////////////////////////////////////////////////
    // toSet                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toSet01(): Bool = region r {
        Array.toSet([]: Array[Int32, _]) == Set#{}
    }

    @test
    def toSet02(): Bool = region r {
        Array.toSet([1]) == Set#{1}
    }

    @test
    def toSet03(): Bool = region r {
        Array.toSet([1,2]) == Set#{1, 2}
    }

    @test
    def toSet04(): Bool = region r {
        Array.toSet([1,1]) == Set#{1}
    }

    @test
    def toSet05(): Bool = region r {
        Array.toSet([1,2,3]) == Set#{1, 2, 3}
    }

    @test
    def toSet06(): Bool = region r {
        Array.toSet([1,2,1]) == Set#{1, 2}
    }

    @test
    def toSet07(): Bool = region r {
        Array.toSet([1,1,2]) == Set#{1, 2}
    }

    @test
    def toSet08(): Bool = region r {
        Array.toSet([2,1,2,3,4,5,6,3,3]) == Set#{1, 2, 3, 4, 5, 6}
    }

    /////////////////////////////////////////////////////////////////////////////
    // toMap                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toMap01(): Bool = region r {
        Array.toMap([]: Array[(Int32, Bool), _]) == Map#{}
    }

    @test
    def toMap02(): Bool = region r {
        Array.toMap([(1, true)]) == Map#{1 => true}
    }

    @test
    def toMap03(): Bool = region r {
        Array.toMap([(1, true), (2, false)]) == Map#{1 => true, 2 => false}
    }

    @test
    def toMap04(): Bool = region r {
        Array.toMap([(1, true), (1, false)]) == Map#{1 => true}
    }

    /////////////////////////////////////////////////////////////////////////////
    // findIndexOf                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def findIndexOf01(): Bool = region r {
        Array.findIndexOf(i -> i > 2, []: Array[Int32, _]) == None
    }

    @test
    def findIndexOf02(): Bool = region r {
        Array.findIndexOf(i -> i > 2, [1]) == None
    }

    @test
    def findIndexOf03(): Bool = region r {
        Array.findIndexOf(i -> i > 2, [3]) == Some(0)
    }

    @test
    def findIndexOf04(): Bool = region r {
        Array.findIndexOf(i -> i > 2, [1,2]) == None
    }

    @test
    def findIndexOf05(): Bool = region r {
        Array.findIndexOf(i -> i > 2, [6,-6]) == Some(0)
    }

    @test
    def findIndexOf06(): Bool = region r {
        Array.findIndexOf(i -> i > 2, [-6,6]) == Some(1)
    }

    @test
    def findIndexOf07(): Bool = region r {
        Array.findIndexOf(i -> i > 2, [6,7]) == Some(0)
    }

    /////////////////////////////////////////////////////////////////////////////
    // findIndexOfLeft                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def findIndexOfLeft01(): Bool = region r {
        Array.findIndexOfLeft(i -> i > 2, []: Array[Int32, _]) == None
    }

    @test
    def findIndexOfLeft02(): Bool = region r {
        Array.findIndexOfLeft(i -> i > 2, [1]) == None
    }

    @test
    def findIndexOfLeft03(): Bool = region r {
        Array.findIndexOfLeft(i -> i > 2, [3]) == Some(0)
    }

    @test
    def findIndexOfLeft04(): Bool = region r {
        Array.findIndexOfLeft(i -> i > 2, [1,2]) == None
    }

    @test
    def findIndexOfLeft05(): Bool = region r {
        Array.findIndexOfLeft(i -> i > 2, [6,-6]) == Some(0)
    }

    @test
    def findIndexOfLeft06(): Bool = region r {
        Array.findIndexOfLeft(i -> i > 2, [-6,6]) == Some(1)
    }

    @test
    def findIndexOfLeft07(): Bool = region r {
        Array.findIndexOfLeft(i -> i > 2, [6,7]) == Some(0)
    }

    /////////////////////////////////////////////////////////////////////////////
    // findIndexOfRight                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def findIndexOfRight01(): Bool = region r {
        Array.findIndexOfRight(i -> i > 2, []: Array[Int32, _]) == None
    }

    @test
    def findIndexOfRight02(): Bool = region r {
        Array.findIndexOfRight(i -> i > 2, [1]) == None
    }

    @test
    def findIndexOfRight03(): Bool = region r {
        Array.findIndexOfRight(i -> i > 2, [3]) == Some(0)
    }

    @test
    def findIndexOfRight04(): Bool = region r {
        Array.findIndexOfRight(i -> i > 2, [1,2]) == None
    }

    @test
    def findIndexOfRight05(): Bool = region r {
        Array.findIndexOfRight(i -> i > 2, [6,-6]) == Some(0)
    }

    @test
    def findIndexOfRight06(): Bool = region r {
        Array.findIndexOfRight(i -> i > 2, [-6,6]) == Some(1)
    }

    @test
    def findIndexOfRight07(): Bool = region r {
        Array.findIndexOfRight(i -> i > 2, [6,7]) == Some(1)
    }

    /////////////////////////////////////////////////////////////////////////////
    // findIndices                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def findIndices01(): Bool = region r {
        let a = Array.findIndices(i -> i > 2, []: Array[Int32, r]);
        Array.sameElements(a, []: Array[Int32, r])
    }

    @test
    def findIndices02(): Bool = region r {
        let a = Array.findIndices(i -> i > 2, [1]);
        Array.sameElements(a, []: Array[Int32, r])
    }

    @test
    def findIndices03(): Bool = region r {
        let a = Array.findIndices(i -> i > 2, [3]);
        Array.sameElements(a, [0])
    }

    @test
    def findIndices04(): Bool = region r {
        let a = Array.findIndices(i -> i > 2, [1,2]);
        Array.sameElements(a, [] : Array[Int32, r])
    }

    @test
    def findIndices05(): Bool = region r {
        let a = Array.findIndices(i -> i > 2, [6,-6]);
        Array.sameElements(a, [0])
    }

    @test
    def findIndices06(): Bool = region r {
        let a = Array.findIndices(i -> i > 2, [-6,6]);
        Array.sameElements(a, [1])
    }

    @test
    def findIndices07(): Bool = region r {
        let a = Array.findIndices(i -> i > 2, [6,7]);
        Array.sameElements(a, [0,1])
    }

    /////////////////////////////////////////////////////////////////////////////
    // init                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def init01(): Bool = region r {
        let a = Array.init(x -> x, 0, r);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def init02(): Bool = region r {
        let a = Array.init(x -> x, -1, r);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def init03(): Bool = region r {
        let a = Array.init(x -> x, 1, r);
        Array.sameElements(a, [0])
    }

    @test
    def init04(): Bool = region r {
        let a = Array.init(x -> x, 2, r);
        Array.sameElements(a, [0,1])
    }

    @test
    def init05(): Bool = region r {
        let a = Array.init(x -> x, 50, r);
        Array.length(a) == 50
    }

    /////////////////////////////////////////////////////////////////////////////
    // init2                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def init201(): Bool = region r {
        let (a,b) = Array.init2(x -> (x, true), 0, r, r);
        Array.sameElements(a, []: Array[Int32, _]) and Array.sameElements(b, []: Array[Bool, _])
    }

    @test
    def init202(): Bool = region r {
        let (a,b) = Array.init2(x -> (x, true), -1, r, r);
        Array.sameElements(a, []: Array[Int32, _]) and Array.sameElements(b, []: Array[Bool, _])
    }

    @test
    def init203(): Bool = region r {
        let (a,b) = Array.init2(x -> (x, true), 1, r, r);
        Array.sameElements(a, [0]) and Array.sameElements(b, [true])
    }

    @test
    def init204(): Bool = region r {
        let (a,b) = Array.init2(x -> (x, true), 2, r, r);
        Array.sameElements(a, [0,1]) and Array.sameElements(b, [true, true])
    }

    @test
    def init205(): Bool = region r {
        let (a,b) = Array.init2(x -> (x, true), 50, r, r);
        Array.length(a) == 50 and Array.length(b) == 50
    }


    /////////////////////////////////////////////////////////////////////////////
    // sameElements                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sameElements01(): Bool = region r {
        let a = []: Array[Int32, _];
        let b = []: Array[Int32, _];
        Array.sameElements(a,b) == true
    }

    @test
    def sameElements02(): Bool = region r {
        let a = [1]: Array[Int32, _];
        let b = []: Array[Int32, _];
        Array.sameElements(a,b) == false
    }

    @test
    def sameElements03(): Bool = region r {
        let a = []: Array[Int32, _];
        let b = [1]: Array[Int32, _];
        Array.sameElements(a,b) == false
    }

    @test
    def sameElements04(): Bool = region r {
        let a = [1]: Array[Int32, _];
        let b = [1]: Array[Int32, _];
        Array.sameElements(a,b) == true
    }

    @test
    def sameElements05(): Bool = region r {
        let a = [1]: Array[Int32, _];
        let b = [0]: Array[Int32, _];
        Array.sameElements(a,b) == false
    }

    @test
    def sameElements06(): Bool = region r {
        let a = [1,2]: Array[Int32, _];
        let b = [1,2]: Array[Int32, _];
        Array.sameElements(a,b) == true
    }

    @test
    def sameElements07(): Bool = region r {
        let a = [1,2]: Array[Int32, _];
        let b = [2,1]: Array[Int32, _];
        Array.sameElements(a,b) == false
    }

    /////////////////////////////////////////////////////////////////////////////
    // foreach                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foreach01(): Bool = region r {
        let a = []: Array[Int32, _];
        let sb = new StringBuilder(r);
        let fn = x -> if (x > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        Array.foreach(fn, a);
        StringBuilder.toString(sb) == ""
    }

    @test
    def foreach02(): Bool = region r {
        let a = [0];
        let sb = new StringBuilder(r);
        let fn = x -> if (x > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        Array.foreach(fn, a);
        StringBuilder.toString(sb) == "F"
    }

    @test
    def foreach03(): Bool = region r {
        let a = [1];
        let sb = new StringBuilder(r);
        let fn = x -> if (x > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        Array.foreach(fn, a);
        StringBuilder.toString(sb) == "T"
    }

    @test
    def foreach04(): Bool = region r {
        let a = [0,1];
        let sb = new StringBuilder(r);
        let fn = x -> if (x > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        Array.foreach(fn, a);
        StringBuilder.toString(sb) == "FT"
    }

    /////////////////////////////////////////////////////////////////////////////
    // foreachWithIndex                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foreachWithIndex01(): Bool = region r {
        let a = []: Array[Float32, _];
        let sb = new StringBuilder(r);
        let fn = (_,ix) -> if (ix > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        Array.foreachWithIndex(fn, a);
        StringBuilder.toString(sb) == ""
    }

    @test
    def foreachWithIndex02(): Bool = region r {
        let a = [0.0];
        let sb = new StringBuilder(r);
        let fn = (_,ix) -> if (ix > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        Array.foreachWithIndex(fn, a);
        StringBuilder.toString(sb) == "F"
    }

    @test
    def foreachWithIndex03(): Bool = region r {
        let a = [0.0, 0.1];
        let sb = new StringBuilder(r);
        let fn = (_,ix) -> if (ix > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        Array.foreachWithIndex(fn, a);
        StringBuilder.toString(sb) == "FT"
    }

    @test
    def foreachWithIndex04(): Bool = region r {
        let a = [0.0, 0.1, 0.2];
        let sb = new StringBuilder(r);
        let fn = (_,ix) -> if (ix > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        Array.foreachWithIndex(fn, a);
        StringBuilder.toString(sb) == "FTT"
    }

    @test
    def foreachWithIndex05(): Bool = region r {
        let a = [0.0, 0.1, 0.2, 0.3];
        let sb = new StringBuilder(r);
        let fn = (_,ix) -> if (ix > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        Array.foreachWithIndex(fn, a);
        StringBuilder.toString(sb) == "FTTT"
    }

    /////////////////////////////////////////////////////////////////////////////
    // updateSequence                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def updateSequence01(): Bool = region r {
         let a = Array.updateSequence(0, []: Array[Int32, _], []: Array[Int32, _]);
         Array.sameElements(a, []: Array[Int32, _])
     }

    @test
    def updateSequence02(): Bool = region r {
        let a = Array.updateSequence(0, [1,2], []);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def updateSequence03(): Bool = region r {
        let a = Array.updateSequence(0, [], [1,2]);
        Array.sameElements(a, [1,2])
    }

    @test
    def updateSequence04(): Bool = region r {
        let a = Array.updateSequence(-3, [1,2,4], [1,2]);
        Array.sameElements(a, [1,2])
    }

    @test
    def updateSequence05(): Bool = region r {
        let a = Array.updateSequence(2, [1,2,4], [1,2]);
        Array.sameElements(a, [1,2])
    }

    @test
    def updateSequence06(): Bool = region r {
        let a = Array.updateSequence(0, [], [1]);
        Array.sameElements(a, [1])
    }

    @test
    def updateSequence07(): Bool = region r {
        let a = Array.updateSequence(1, [2], [1]);
        Array.sameElements(a, [1])
    }

    @test
    def updateSequence08(): Bool = region r {
        let a = Array.updateSequence(0, [2], [1]);
        Array.sameElements(a, [2])
    }

    @test
    def updateSequence09(): Bool = region r {
        let a = Array.updateSequence(0, [2,4], [1]);
        Array.sameElements(a, [2])
    }

    @test
    def updateSequence10(): Bool = region r {
        let a = Array.updateSequence(-1, [2,4], [1]);
        Array.sameElements(a, [4])
    }

    @test
    def updateSequence11(): Bool = region r {
        let a = Array.updateSequence(-1, [3,4], [1,2]);
        Array.sameElements(a, [4,2])
    }

    @test
    def updateSequence12(): Bool = region r {
        let a = Array.updateSequence(1, [3,4], [1,2]);
        Array.sameElements(a, [1,3])
    }

    @test
    def updateSequence13(): Bool = region r {
        let a = Array.updateSequence(-2, [3,4], [1,2]);
        Array.sameElements(a, [1,2])
    }

    @test
    def updateSequence14(): Bool = region r {
        let a = Array.updateSequence(2, [3,4], [1,2]);
        Array.sameElements(a, [1,2])
    }

    @test
    def updateSequence15(): Bool = region r {
        let a = Array.updateSequence(1, [3], [1,2]);
        Array.sameElements(a, [1,3])
    }

    @test
    def updateSequence16(): Bool = region r {
        let a = Array.updateSequence(0, [3,4], [1,2]);
        Array.sameElements(a, [3,4])
    }

    @test
    def updateSequence17(): Bool = region r {
        let a = Array.updateSequence(0, [4], [1, 2, 3]);
        Array.sameElements(a, [4, 2, 3])
    }

    @test
    def updateSequence18(): Bool = region r {
        let a = Array.updateSequence(1, [4], [1, 2, 3]);
        Array.sameElements(a, [1, 4, 3])
    }

    @test
    def updateSequence19(): Bool = region r {
        let a = Array.updateSequence(2, [4], [1, 2, 3]);
        Array.sameElements(a, [1, 2, 4])
    }

    @test
    def updateSequence20(): Bool = region r {
        let a = Array.updateSequence(0, [4, 5], [1, 2, 3]);
        Array.sameElements(a, [4, 5, 3])
    }

    @test
    def updateSequence21(): Bool = region r {
        let a = Array.updateSequence(1, [4, 5], [1, 2, 3]);
        Array.sameElements(a, [1, 4, 5])
    }

    @test
    def updateSequence22(): Bool = region r {
        let a = Array.updateSequence(-1, [4, 5, 6], [1, 2, 3]);
        Array.sameElements(a, [5, 6, 3])
    }

    @test
    def updateSequence23(): Bool = region r {
        let a = Array.updateSequence(0, [4, 5, 6], [1, 2, 3]);
        Array.sameElements(a, [4, 5, 6])
    }

    @test
    def updateSequence24(): Bool = region r {
        let a = Array.updateSequence(2, [14, 15, 16, 17], [1, 2, 3, 4, 5, 6, 7]);
        Array.sameElements(a, [1, 2, 14, 15, 16, 17, 7])
    }

    @test
    def updateSequence25(): Bool = region r {
        let a = Array.updateSequence(-2, [14, 15, 16, 17], [1, 2, 3, 4, 5, 6, 7]);
        Array.sameElements(a, [16, 17, 3, 4, 5, 6, 7])
    }

    @test
    def updateSequence26(): Bool = region r {
        let a = Array.updateSequence(4, [14, 15, 16, 17], [1, 2, 3, 4, 5, 6, 7]);
        Array.sameElements(a, [1, 2, 3, 4, 14, 15, 16])
    }

    @test
    def updateSequence27(): Bool = region r {
        let a = Array.updateSequence(4, [14, 15], [1, 2, 3, 4, 5, 6, 7]);
        Array.sameElements(a, [1, 2, 3, 4, 14, 15, 7])
    }

    @test
    def updateSequence28(): Bool = region r {
        let a = Array.updateSequence(-1, [-1, -2, -3, -4, -5, -6, -7, -8], [1, 2, 3, 4, 5, 6, 7]);
        Array.sameElements(a, [-2, -3, -4, -5, -6, -7, -8])
    }

    /////////////////////////////////////////////////////////////////////////////
    // updateSequence!                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def updateSequence01!(): Bool = region r {
         let a = []: Array[Int32, _];
         Array.updateSequence!(0, []: Array[Int32, _], a);
         Array.sameElements(a, []: Array[Int32, _])
     }

    @test
    def updateSequence02!(): Bool = region r {
        let a = []: Array[Int32, _];
        Array.updateSequence!(0, [1,2], a);
        Array.sameElements(a, []: Array[Int32, _])
    }

    @test
    def updateSequence03!(): Bool = region r {
        let a = [1,2];
        Array.updateSequence!(0, [], a);
        Array.sameElements(a, [1,2])
    }

    @test
    def updateSequence04!(): Bool = region r {
        let a = [1,2];
        Array.updateSequence!(-3, [1,2,4], a);
        Array.sameElements(a, [1,2])
    }

    @test
    def updateSequence05!(): Bool = region r {
        let a = [1,2];
        Array.updateSequence!(2, [1,2,4], a);
        Array.sameElements(a, [1,2])
    }

    @test
    def updateSequence06!(): Bool = region r {
        let a = [1];
        Array.updateSequence!(0, [], a);
        Array.sameElements(a, [1])
    }

    @test
    def updateSequence07!(): Bool = region r {
        let a = [1];
        Array.updateSequence!(1, [2], a);
        Array.sameElements(a, [1])
    }

    @test
    def updateSequence08!(): Bool = region r {
        let a = [1];
        Array.updateSequence!(0, [2], a);
        Array.sameElements(a, [2])
    }

    @test
    def updateSequence09!(): Bool = region r {
        let a = [1];
        Array.updateSequence!(0, [2,4], a);
        Array.sameElements(a, [2])
    }

    @test
    def updateSequence10!(): Bool = region r {
        let a = [1];
        Array.updateSequence!(-1, [2,4], a);
        Array.sameElements(a, [4])
    }

    @test
    def updateSequence11!(): Bool = region r {
        let a = [1,2];
        Array.updateSequence!(-1, [3,4], a);
        Array.sameElements(a, [4,2])
    }

    @test
    def updateSequence12!(): Bool = region r {
        let a = [1,2];
        Array.updateSequence!(1, [3,4], a);
        Array.sameElements(a, [1,3])
    }

    @test
    def updateSequence13!(): Bool = region r {
        let a = [1,2];
        Array.updateSequence!(-2, [3,4], a);
        Array.sameElements(a, [1,2])
    }

    @test
    def updateSequence14!(): Bool = region r {
        let a = [1,2];
        Array.updateSequence!(2, [3,4], a);
        Array.sameElements(a, [1,2])
    }

    @test
    def updateSequence15!(): Bool = region r {
        let a = [1,2];
        Array.updateSequence!(1, [3], a);
        Array.sameElements(a, [1,3])
    }

    @test
    def updateSequence16!(): Bool = region r {
        let a = [1,2];
        Array.updateSequence!(0, [3,4], a);
        Array.sameElements(a, [3,4])
    }

    @test
    def updateSequence17!(): Bool = region r {
        let a = [1, 2, 3];
        Array.updateSequence!(0, [4], a);
        Array.sameElements(a, [4, 2, 3])
    }

    @test
    def updateSequence18!(): Bool = region r {
        let a = [1, 2, 3];
        Array.updateSequence!(1, [4], a);
        Array.sameElements(a, [1, 4, 3])
    }

    @test
    def updateSequence19!(): Bool = region r {
        let a = [1, 2, 3];
        Array.updateSequence!(2, [4], a);
        Array.sameElements(a, [1, 2, 4])
    }

    @test
    def updateSequence20!(): Bool = region r {
        let a = [1, 2, 3];
        Array.updateSequence!(0, [4, 5], a);
        Array.sameElements(a, [4, 5, 3])
    }

    @test
    def updateSequence21!(): Bool = region r {
        let a = [1, 2, 3];
        Array.updateSequence!(1, [4, 5], a);
        Array.sameElements(a, [1, 4, 5])
    }

    @test
    def updateSequence22!(): Bool = region r {
        let a = [1, 2, 3];
        Array.updateSequence!(-1, [4, 5, 6], a);
        Array.sameElements(a, [5, 6, 3])
    }

    @test
    def updateSequence23!(): Bool = region r {
        let a = [1, 2, 3];
        Array.updateSequence!(0, [4, 5, 6], a);
        Array.sameElements(a, [4, 5, 6])
    }

    @test
    def updateSequence24!(): Bool = region r {
        let a = [1, 2, 3, 4, 5, 6, 7];
        Array.updateSequence!(2, [14, 15, 16, 17], a);
        Array.sameElements(a, [1, 2, 14, 15, 16, 17, 7])
    }

    @test
    def updateSequence25!(): Bool = region r {
        let a = [1, 2, 3, 4, 5, 6, 7];
        Array.updateSequence!(-2, [14, 15, 16, 17], a);
        Array.sameElements(a, [16, 17, 3, 4, 5, 6, 7])
    }

    @test
    def updateSequence26!(): Bool = region r {
        let a = [1, 2, 3, 4, 5, 6, 7];
        Array.updateSequence!(4, [14, 15, 16, 17], a);
        Array.sameElements(a, [1, 2, 3, 4, 14, 15, 16])
    }

    @test
    def updateSequence27!(): Bool = region r {
        let a = [1, 2, 3, 4, 5, 6, 7];
        Array.updateSequence!(4, [14, 15], a);
        Array.sameElements(a, [1, 2, 3, 4, 14, 15, 7])
    }

    @test
    def updateSequence28!(): Bool = region r {
        let a = [1, 2, 3, 4, 5, 6, 7];
        Array.updateSequence!(-1, [-1, -2, -3, -4, -5, -6, -7, -8], a);
        Array.sameElements(a, [-2, -3, -4, -5, -6, -7, -8])
    }


    /////////////////////////////////////////////////////////////////////////////
    // sortWith                                                                //
    /////////////////////////////////////////////////////////////////////////////

    def cmp(x: Int32, y: Int32): Comparison =
        if (x < y) LessThan
        else if (x == y) EqualTo
        else GreaterThan

    @test
    def sortWith01(): Bool = region r {
        let a = Array.sortWith(cmp, []: Array[Int32, _]);
        a `sameElements` []: Array[Int32, _]
    }

    @test
    def sortWith02(): Bool = region r {
        let a = Array.sortWith(cmp, [0]);
        a `sameElements` [0]
    }

    @test
    def sortWith03(): Bool = region r {
        let a = Array.sortWith(cmp, [0,1]);
        a `sameElements` [0,1]
    }

    @test
    def sortWith04(): Bool = region r {
        let a = Array.sortWith(cmp, [1,0]);
        a `sameElements` [0,1]
    }

    @test
    def sortWith05(): Bool = region r {
        let a = Array.sortWith(cmp, [1,1]);
        a `sameElements` [1,1]
    }

    @test
    def sortWith06(): Bool = region r {
        let a = Array.sortWith(cmp, [0,1,2,3,4,5]);
        a `sameElements` [0,1,2,3,4,5]
    }

    @test
    def sortWith07(): Bool = region r {
        let a = Array.sortWith(cmp, [5,4,3,2,1,0]);
        a `sameElements` [0,1,2,3,4,5]
    }

    @test
    def sortWith08(): Bool = region r {
        let a = Array.sortWith(cmp, [5,3,0,4,1,2]);
        a `sameElements` [0,1,2,3,4,5]
    }

    @test
    def sortWith09(): Bool = region r {
        let a = Array.sortWith(cmp, [2,3,0,4,1,2]);
        a `sameElements` [0,1,2,2,3,4]
    }

    @test
    def sortWith10(): Bool = region r {
        let a = Array.sortWith(flip(cmp), [0,1,2,3,4,5]);
        a `sameElements` [5,4,3,2,1,0]
    }

    @test
    def sortWith11(): Bool = region r {
        let a = Array.sortWith(flip(cmp), [5,4,3,2,1,0]);
        a `sameElements` [5,4,3,2,1,0]
    }

    @test
    def sortWith12(): Bool = region r {
        let a = Array.sortWith(flip(cmp), [5,3,0,4,1,2]);
        a `sameElements` [5,4,3,2,1,0]
    }

    @test
    def sortWith13(): Bool = region r {
        let a = Array.sortWith(flip(cmp), [2,3,0,4,1,2]);
        a `sameElements` [4,3,2,2,1,0]
    }

    /////////////////////////////////////////////////////////////////////////////
    // sort                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    def testSortVsSortWith(a: Array[Int32, r]): Bool \ { Read(r), Write(r) } =
        Array.sort(a) `sameElements` Array.sortWith(cmp, a)

    @test
    def sort01(): Bool = region r {
        testSortVsSortWith([]: Array[Int32, _])
    }

    @test
    def sort02(): Bool = region r {
        testSortVsSortWith([0])
    }

    @test
    def sort03(): Bool = region r {
        testSortVsSortWith([0,1])
    }

    @test
    def sort04(): Bool = region r {
        testSortVsSortWith([1,0])
    }

    @test
    def sort05(): Bool = region r {
        testSortVsSortWith([1,1])
    }

    @test
    def sort06(): Bool = region r {
        testSortVsSortWith([0,1,2,3,4,5])
    }

    @test
    def sort07(): Bool = region r {
        testSortVsSortWith([5,4,3,2,1,0])
    }

    @test
    def sort08(): Bool = region r {
        testSortVsSortWith([5,3,0,4,1,2])
    }

    @test
    def sort09(): Bool = region r {
        testSortVsSortWith([2,3,0,4,1,2])
    }

    /////////////////////////////////////////////////////////////////////////////
    // sortWith!                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sortWith01!(): Bool = region r {
        let a = []: Array[Int32, _];
        Array.sortWith!(cmp, a);
        a `sameElements` []: Array[Int32, _]
    }

    @test
    def sortWith02!(): Bool = region r {
        let a = [0];
        Array.sortWith!(cmp, a);
        a `sameElements` [0]
    }

    @test
    def sortWith03!(): Bool = region r {
        let a = [0,1];
        Array.sortWith!(cmp, a);
        a `sameElements` [0,1]
    }

    @test
    def sortWith04!(): Bool = region r {
        let a = [1,0];
        Array.sortWith!(cmp, a);
        a `sameElements` [0,1]
    }

    @test
    def sortWith05!(): Bool = region r {
        let a = [1,1];
        Array.sortWith!(cmp, a);
        a `sameElements` [1,1]
    }

    @test
    def sortWith06!(): Bool = region r {
        let a = [0,1,2,3,4,5];
        Array.sortWith!(cmp, a);
        a `sameElements` [0,1,2,3,4,5]
    }

    @test
    def sortWith07!(): Bool = region r {
        let a = [5,4,3,2,1,0];
        Array.sortWith!(cmp, a);
        a `sameElements` [0,1,2,3,4,5]
    }

    @test
    def sortWith08!(): Bool = region r {
        let a = [5,3,0,4,1,2];
        Array.sortWith!(cmp, a);
        a `sameElements` [0,1,2,3,4,5]
    }

    @test
    def sortWith09!(): Bool = region r {
        let a = [2,3,0,4,1,2];
        Array.sortWith!(cmp, a);
        a `sameElements` [0,1,2,2,3,4]
    }

    @test
    def sortWith10!(): Bool = region r {
        let a = [0,1,2,3,4,5];
        Array.sortWith!(flip(cmp), a);
        a `sameElements` [5,4,3,2,1,0]
    }

    @test
    def sortWith11!(): Bool = region r {
        let a = [5,4,3,2,1,0];
        Array.sortWith!(flip(cmp), a);
        a `sameElements` [5,4,3,2,1,0]
    }

    @test
    def sortWith12!(): Bool = region r {
        let a = [5,3,0,4,1,2];
        Array.sortWith!(flip(cmp), a);
        a `sameElements` [5,4,3,2,1,0]
    }

    @test
    def sortWith13!(): Bool = region r {
        let a = [2,3,0,4,1,2];
        Array.sortWith!(flip(cmp), a);
        a `sameElements` [4,3,2,2,1,0]
    }

    /////////////////////////////////////////////////////////////////////////////
    // sort!                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    def testSort!VsSortWith!(a: Array[Int32, r]): Bool \ { Read(r), Write(r) } =
        let b = Array.slice(0, Array.length(a), a);
        let c = Array.slice(0, Array.length(a), a);
        Array.sort!(b);
        Array.sortWith!(cmp, c);
        b `sameElements` c

    @test
    def sort!01(): Bool = region r {
        testSort!VsSortWith!([]: Array[Int32, _])
    }

    @test
    def sort!02(): Bool = region r {
        testSort!VsSortWith!([0])
    }

    @test
    def sort!03(): Bool = region r {
        testSort!VsSortWith!([0,1])
    }

    @test
    def sort!04(): Bool = region r {
        testSort!VsSortWith!([1,0])
    }

    @test
    def sort!05(): Bool = region r {
        testSort!VsSortWith!([1,1])
    }

    @test
    def sort!06(): Bool = region r {
        testSort!VsSortWith!([0,1,2,3,4,5])
    }

    @test
    def sort!07(): Bool = region r {
        testSort!VsSortWith!([5,4,3,2,1,0])
    }

    @test
    def sort!08(): Bool = region r {
        testSort!VsSortWith!([5,3,0,4,1,2])
    }

    @test
    def sort!09(): Bool = region r {
        testSort!VsSortWith!([2,3,0,4,1,2])
    }

    /////////////////////////////////////////////////////////////////////////////
    // sortBy                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    def testSortByVsSort(a: Array[Int32, r]): Bool \ { Read(r), Write(r) } =
        (Array.sortBy(identity, a) `sameElements` Array.sort(a)) and
        (Array.sortBy(x -> 4 * x + 7, a) `sameElements` Array.sort(a)) and
        (Array.sortBy(x -> -x, a) `sameElements` Array.sortWith(flip(cmp),a))

    @test
    def sortBy01(): Bool = region r {
        testSortByVsSort([]: Array[Int32, _])
    }

    @test
    def sortBy02(): Bool = region r {
        testSortByVsSort([0])
    }

    @test
    def sortBy03(): Bool = region r {
        testSortByVsSort([0,1])
    }

    @test
    def sortBy04(): Bool = region r {
        testSortByVsSort([1,0])
    }

    @test
    def sortBy05(): Bool = region r {
        testSortByVsSort([1,1])
    }

    @test
    def sortBy06(): Bool = region r {
        testSortByVsSort([0,1,2,3,4,5])
    }

    @test
    def sortBy07(): Bool = region r {
        testSortByVsSort([5,4,3,2,1,0])
    }

    @test
    def sortBy08(): Bool = region r {
        testSortByVsSort([5,3,0,4,1,2])
    }

    @test
    def sortBy09(): Bool = region r {
        testSortByVsSort([2,3,0,4,1,2])
    }

    enum R {
        case R({i :: Int32, s :: String})
    }

    instance Eq[R] {
        pub def eq(a: R, b: R): Bool =
            let R(x) = a;
            let R(y) = b;
            x.i == y.i and x.s == y.s
    }

    @test
    def sortBy10(): Bool = region reg {
        Array.sortBy(r -> let R(x) = r; x.i, [R({i = 2, s = "A"}), R({i = 1, s = "B"}), R({i = 3, s = "C"})])
        `sameElements` [R({i = 1, s = "B"}), R({i = 2, s = "A"}), R({i = 3, s = "C"})]
    }

    /////////////////////////////////////////////////////////////////////////////
    // sortBy!                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    def testSortBy!VsSortBy(a: Array[Int32, r]) : Bool \ { Read(r), Write(r) } =
        let b = Array.slice(0, Array.length(a), a);
        let c = Array.slice(0, Array.length(a), a);
        Array.sortBy!(identity, b);
        Array.sortBy!(x -> 4 * x + 7, c);
        (b `sameElements` Array.sortBy(x -> 4 * x + 7, a)) and
        (c `sameElements` Array.sortBy(identity, a))

    @test
    def sortBy!01(): Bool = region r {
        testSortBy!VsSortBy([]: Array[Int32, _])
    }

    @test
    def sortBy!02(): Bool = region r {
        testSortBy!VsSortBy([0])
    }

    @test
    def sortBy!03(): Bool = region r {
        testSortBy!VsSortBy([0,1])
    }

    @test
    def sortBy!04(): Bool = region r {
        testSortBy!VsSortBy([1,0])
    }

    @test
    def sortBy!05(): Bool = region r {
        testSortBy!VsSortBy([1,1])
    }

    @test
    def sortBy!06(): Bool = region r {
        testSortBy!VsSortBy([0,1,2,3,4,5])
    }

    @test
    def sortBy!07(): Bool = region r {
        testSortBy!VsSortBy([5,4,3,2,1,0])
    }

    @test
    def sortBy!08(): Bool = region r {
        testSortBy!VsSortBy([5,3,0,4,1,2])
    }

    @test
    def sortBy!09(): Bool = region r {
        testSortBy!VsSortBy([2,3,0,4,1,2])
    }

    /////////////////////////////////////////////////////////////////////////////
    // minimumBy                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def minimumBy01(): Bool = region r {
        Array.minimumBy((x, y) -> x <=> y, []: Array[Int32, _]) == None
    }

    @test
    def minimumBy02(): Bool = region r {
        Array.minimumBy((x, y) -> x <=> y, Array.range(0, 5, r)) == Some(0)
    }

    @test
    def minimumBy03(): Bool = region r {
        Array.minimumBy((x, y) -> x <=> y, Array.range(4, 10, r)) == Some(4)
    }

    @test
    def minimumBy04(): Bool = region r {
        let a = Array.range(10, 20, r);
        a[5] = -22;
        Array.minimumBy((x, y) -> x <=> y, a) == Some(-22)
    }

    /////////////////////////////////////////////////////////////////////////////
    // maximumBy                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def maximumBy01(): Bool = region r {
        Array.maximumBy((x, y) -> x <=> y, []: Array[Int32, _]) == None
    }

    @test
    def maximumBy02(): Bool = region r {
        Array.maximumBy((x, y) -> x <=> y, Array.range(0, 5, r)) == Some(4)
    }

    @test
    def maximumBy03(): Bool = region r {
        Array.maximumBy((x, y) -> x <=> y, Array.range(4, 10, r)) == Some(9)
    }

    @test
    def maximumBy04(): Bool = region r {
        let a = Array.range(10, 20, r);
        a[5] = 42;
        Array.maximumBy((x, y) -> x <=> y, a) == Some(42)
    }


    /////////////////////////////////////////////////////////////////////////////
    // sum                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sum01(): Bool = region r {
        Array.sum([]) == 0
    }

    @test
    def sum02(): Bool = region r {
        Array.sum([1]) == 1
    }

    @test
    def sum03(): Bool = region r {
        Array.sum([1, 2, 3]) == 6
    }

    @test
    def sum04(): Bool = region r {
        Array.sum([1, 2, 3, -3]) == 3
    }

    @test
    def sum05(): Bool = region r {
        Array.sum([-1, -2, -3, -4]) == -10
    }
    @test
    def sum06(): Bool = region r {
        Array.sum([10, -10]) == 0
    }

    @test
    def sum07(): Bool = region r {
        Array.range(1, 101, r) |> Array.sum == 5050
    }


    /////////////////////////////////////////////////////////////////////////////
    // sumWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sumWith01(): Bool = region r {
        Array.sumWith(x -> x + 1, []) == 0
    }

    @test
    def sumWith02(): Bool = region r {
        Array.sumWith(x -> x + 1, [1]) == 2
    }

    @test
    def sumWith03(): Bool = region r {
        Array.sumWith(x -> x + 1, [1, 2, 3]) == 9
    }

    @test
    def sumWith04(): Bool = region r {
        Array.sumWith(x -> x + 1, [1, 2, 3, -3]) == 7
    }

    @test
    def sumWith05(): Bool = region r {
        Array.sumWith(x -> x + 1, [-1, -2, -3, -4]) == -6
    }

    @test
    def sumWith06(): Bool = region r {
        Array.sumWith(x -> x + 1, [10, -10]) == 2
    }


    /////////////////////////////////////////////////////////////////////////////
    // toDelayList                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toDelayList01(): Bool = region r {
        ([] @ r): Array[Unit, _] |> Array.toDelayList == DelayList.empty()
    }

    @test
    def toDelayList02(): Bool = region r {
        ([0; 0] @ r) |> Array.toDelayList == DelayList.empty()
    }

    @test
    def toDelayList03(): Bool = region r {
        Array.range(-1000, 1000, r) |> Array.toDelayList == DelayList.range(-1000, 1000)
    }

    @test
    def toDelayList04(): Bool = region r {
        (Array.range(-1000, 1000, r) |> Array.toDelayList |> DelayList.toArray)(r) `sameElements` Array.range(-1000, 1000, r)
    }

    /////////////////////////////////////////////////////////////////////////////
    // toChain                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toChain01(): Bool = region r {
        Array.toChain([]: Array[Unit, _]) == Chain.empty(): Chain[Unit]
    }

    @test
    def toChain02(): Bool = region r {
        Array.toChain([1]) == Chain.singleton(1)
    }

    @test
    def toChain03(): Bool = region r {
        Array.toChain([1,2]) == List.toChain(1 :: 2 :: Nil)
    }

    @test
    def toChain04(): Bool = region r {
        Array.toChain([1,2,3]) == List.toChain(1 :: 2 :: 3 :: Nil)
    }

    /////////////////////////////////////////////////////////////////////////////
    // copyOfRange                                                             //
    /////////////////////////////////////////////////////////////////////////////

    // depends on flatten

    @test
    def copyOfRange01(): Bool & Impure =
        Array.copyOfRange(0, 3, [true, false, true], Static) `sameElements` [true, false, true]

    @test
    def copyOfRange02(): Bool & Impure =
        Array.copyOfRange(1, 3, [true, false, true], Static) `sameElements` [false, true]

    @test
    def copyOfRange03(): Bool & Impure =
        let a = Array.copyOfRange(2, 5, [true, false, true], Static);
        Array.length(a) == 3 and Array.memberOf(true, a)

    @test
    def copyOfRange04(): Bool & Impure =
        Array.copyOfRange(0, 3, ['a', 'b', 'c'], Static) `sameElements` ['a', 'b', 'c']

    @test
    def copyOfRange05(): Bool & Impure =
        Array.copyOfRange(1, 3, ['a', 'b', 'c'], Static) `sameElements` ['b', 'c']

    @test
    def copyOfRange06(): Bool & Impure =
        let a = Array.copyOfRange(2, 5, ['a', 'b', 'c'], Static);
        Array.length(a) == 3 and Array.memberOf('c', a)

    @test
    def copyOfRange07(): Bool & Impure =
        Array.copyOfRange(0, 3, [1.0f32, 2.0f32, 3.0f32], Static) `sameElements` [1.0f32, 2.0f32, 3.0f32]

    @test
    def copyOfRange08(): Bool & Impure =
        Array.copyOfRange(1, 3, [1.0f32, 2.0f32, 3.0f32], Static) `sameElements` [2.0f32, 3.0f32]

    @test
    def copyOfRange09(): Bool & Impure =
        let a = Array.copyOfRange(2, 5, [1.0f32, 2.0f32, 3.0f32], Static);
        Array.length(a) == 3 and Array.memberOf(3.0f32, a)

    @test
    def copyOfRange10(): Bool & Impure =
        Array.copyOfRange(0, 3, [1.0f64, 2.0f64, 3.0f64], Static) `sameElements` [1.0f64, 2.0f64, 3.0f64]

    @test
    def copyOfRange11(): Bool & Impure =
        Array.copyOfRange(1, 3, [1.0f64, 2.0f64, 3.0f64], Static) `sameElements` [2.0f64, 3.0f64]

    @test
    def copyOfRange12(): Bool & Impure =
        let a = Array.copyOfRange(2, 5, [1.0f64, 2.0f64, 3.0f64], Static);
        Array.length(a) == 3 and Array.memberOf(3.0f64, a)

    @test
    def copyOfRange13(): Bool & Impure =
        Array.copyOfRange(0, 3, [1i8, 2i8, 3i8], Static) `sameElements` [1i8, 2i8, 3i8]

    @test
    def copyOfRange14(): Bool & Impure =
        Array.copyOfRange(1, 3, [1i8, 2i8, 3i8], Static) `sameElements` [2i8, 3i8]

    @test
    def copyOfRange15(): Bool & Impure =
        let a = Array.copyOfRange(2, 5, [1i8, 2i8, 3i8], Static);
        Array.length(a) == 3 and Array.memberOf(3i8, a)

    @test
    def copyOfRange16(): Bool & Impure =
        Array.copyOfRange(0, 3, [1i16, 2i16, 3i16], Static) `sameElements` [1i16, 2i16, 3i16]

    @test
    def copyOfRange17(): Bool & Impure =
        Array.copyOfRange(1, 3, [1i16, 2i16, 3i16], Static) `sameElements` [2i16, 3i16]

    @test
    def copyOfRange18(): Bool & Impure =
        let a = Array.copyOfRange(2, 5, [1i16, 2i16, 3i16], Static);
        Array.length(a) == 3 and Array.memberOf(3i16, a)

    @test
    def copyOfRange19(): Bool & Impure =
        Array.copyOfRange(0, 3, [1, 2, 3], Static) `sameElements` [1, 2, 3]

    @test
    def copyOfRange20(): Bool & Impure =
        Array.copyOfRange(1, 3, [1, 2, 3], Static) `sameElements` [2, 3]

    @test
    def copyOfRange21(): Bool & Impure =
        let a = Array.copyOfRange(2, 5, [1, 2, 3], Static);
        Array.length(a) == 3 and Array.memberOf(3, a)

    @test
    def copyOfRange22(): Bool & Impure =
        Array.copyOfRange(0, 3, [1i64, 2i64, 3i64], Static) `sameElements` [1i64, 2i64, 3i64]

    @test
    def copyOfRange23(): Bool & Impure =
        Array.copyOfRange(1, 3, [1i64, 2i64, 3i64], Static) `sameElements` [2i64, 3i64]

    @test
    def copyOfRange24(): Bool & Impure =
        let a = Array.copyOfRange(2, 5, [1i64, 2i64, 3i64], Static);
        Array.length(a) == 3 and Array.memberOf(3i64, a)

    @test
    def copyOfRange25(): Bool & Impure =
        let a = Array.copyOfRange(0, 3, [['a', 'b', 'c'], ['d', 'e', 'f', 'g'], ['h']], Static);
        let b = [['a', 'b', 'c'], ['d', 'e', 'f', 'g'], ['h']];
        Array.flatten(a) `sameElements` Array.flatten(b)

    @test
    def copyOfRange26(): Bool & Impure =
        let a = Array.copyOfRange(1, 3, [['a', 'b', 'c'], ['d', 'e', 'f', 'g'], ['h']], Static);
        let b = [['d', 'e', 'f', 'g'], ['h']];
        Array.flatten(a) `sameElements` Array.flatten(b)

    @test
    def copyOfRange27(): Bool & Impure =
        Array.copyOfRange(0, 3, [Some(1), Some(2), None], Static) `sameElements` [Some(1), Some(2), None]

    @test
    def copyOfRange28(): Bool & Impure =
        Array.copyOfRange(1, 3, [Some(1), Some(2), None], Static) `sameElements` [Some(2), None]

    @test
    def copyOfRange29(): Bool & Impure =
        let a = Array.copyOfRange(2, 5, [Some(1), Some(2), None], Static);
        Array.length(a) == 3 and Array.memberOf(None, a)

    @test
    def copyOfRange30(): Bool & Impure =
        let a = Array.copyOfRange(0, 3, [[Some('a'), Some('b'), Some('c')], [Some('d'), Some('e'), Some('f'), Some('g')], [None]], Static);
        let b = [[Some('a'), Some('b'), Some('c')], [Some('d'), Some('e'), Some('f'), Some('g')], [None]];
        Array.flatten(a) `sameElements` Array.flatten(b)

    @test
    def copyOfRange31(): Bool & Impure =
        let a = Array.copyOfRange(1, 3, [[Some('a'), Some('b'), Some('c')], [Some('d'), Some('e'), Some('f'), Some('g')], [None]], Static);
        let b = [[Some('d'), Some('e'), Some('f'), Some('g')], [None]];
        Array.flatten(a) `sameElements` Array.flatten(b)


    /////////////////////////////////////////////////////////////////////////////
    // toIterator                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toIterator01(): Bool = region r {
        []: Array[Int32, r] |> Array.toIterator(r) |> Iterator.toList == Nil
    }

    @test
    def toIterator02(): Bool = region r {
        [1, 2, 3] |> Array.toIterator(r) |> Iterator.toList == 1 :: 2 :: 3 :: Nil
    }

    @test
    def toIterator03(): Bool = region r {
        ([1, 2, 3] @ r) |> Array.toIterator(r) |> Iterator.toArray(r) `sameElements` [1, 2, 3] @ r
    }

    @test
    def toIterator04(): Bool = region r {
        Array.range(-100, 100, r) |> Array.toIterator(r) |> Iterator.toArray(r) `sameElements` Array.range(-100, 100, r)
    }

    @test
    def toIterator05(): Bool = region r {
        let a = [1, 2, 3] @ r;
        let iter = Array.toIterator(r, a);
        a[0] = 10000;
        Iterator.toArray(r, iter) `sameElements` [1, 2, 3]
    }


    /////////////////////////////////////////////////////////////////////////////
    // join                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def join01(): Bool = region r {
        Array.join(",", ([] @ r): Array[Int32, _]) == ""
    }

    @test
    def join02(): Bool = region r {
        Array.join(",", [1] @ r) == "1"
    }

    @test
    def join03(): Bool = region r {
        Array.join(",", [1, 2, 3] @ r) == "1,2,3"
    }

    @test
    def join04(): Bool = region r {
        Array.join(",", ["1", "2", "3"] @ r) == "1,2,3"
    }


    /////////////////////////////////////////////////////////////////////////////
    // joinWith                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def joinWith01(): Bool = region r {
        Array.joinWith(x -> "${x + 1}", ",", [] @ r) == ""
    }

    @test
    def joinWith02(): Bool = region r {
        Array.joinWith(x -> "${x + 1}", ",", [1] @ r) == "2"
    }

    @test
    def joinWith03(): Bool = region r {
        Array.joinWith(x -> "${x + 1}", ",", [1, 2, 3] @ r) == "2,3,4"
    }

    @test
    def joinWith04(): Bool = region r {
        Array.joinWith(x -> x + x, ",", ["1", "2", "3"] @ r) == "11,22,33"
    }

    @test
    def joinWith05(): Bool = region r {
        let a: Array[Int32, _] = [] @ r;
        let s = Array.joinWith(Int32.toString, ",", a);
        s == ""
    }

    @test
    def joinWith06(): Bool = region r {
        let a = [1] @ r;
        let s = Array.joinWith(Int32.toString, ",", a);
        s == "1"
    }

    @test
    def joinWith07(): Bool = region r {
        let a = [1,2] @ r;
        let s = Array.joinWith(Int32.toString, ",", a);
        s == "1,2"
    }

    @test
    def joinWith08(): Bool = region r {
        let a = [1,2,3] @ r;
        let s = Array.joinWith(Int32.toString, ",", a);
        s == "1,2,3"
    }

    @test
    def joinWith09(): Bool = region r {
        let a = [1,2,3] @ r;
        let s = Array.joinWith(Int32.toString, "", a);
        s == "123"
    }

    @test
    def joinWith10(): Bool = region r {
        let a = [1,2,3] @ r;
        let s = Array.joinWith(Int32.toString, "..", a);
        s == "1..2..3"
    }


    /////////////////////////////////////////////////////////////////////////////
    // toMutList                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toMutList01(): Bool = region r {
        MutList.sameElements(Array.toMutList(([] @ r): Array[Int32, _], r), new MutList(r))
    }

    @test
    def toMutList02(): Bool = region r {
        let v = new MutList(r);
        MutList.push!(1, v);
        MutList.sameElements(Array.toMutList([1] @ r, r), v)
    }

    @test
    def toMutList03(): Bool = region r {
        let v = new MutList(r);
        MutList.push!(1, v);
        MutList.push!(2, v);
        MutList.push!(3, v);
        MutList.sameElements(Array.toMutList([1, 2, 3] @ r, r), v)
    }

    @test
    def toMutList04(): Bool = region r {
        MutList.sameElements((Array.range(0, 100, r) |> Array.toMutList)(r), MutList.range(0, 100, r))
    }

    @test
    def toMutList05(): Bool = region r {
        let v = Array.toMutList([1, 2, 3] @ r, r);
        MutList.push!(4, v);
        Array.sameElements(MutList.toArray(v, r), [1, 2, 3, 4] @ r)
    }

    @test
    def toMutList06(): Bool = region r {
        let v = Array.toMutList([1, 2, 3] @ r, r);
        MutList.push!(4, v);
        let _ = MutList.pop!(v);
        let _ = MutList.pop!(v);
        let _ = MutList.pop!(v);
        let _ = MutList.pop!(v);
        Array.sameElements(MutList.toArray(v, r), [] @ r)
    }

    /////////////////////////////////////////////////////////////////////////////
    // shuffle                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def shuffle01(): Bool & Impure = region r {
        let a = ([] @ r): Array[Int32, _];
        Array.shuffle(Random.new(), a);
        Array.length(a) == 0 and Array.toSet(a) == Set#{}
    }

    @test
    def shuffle02(): Bool & Impure = region r {
        let a = ([0] @ r);
        Array.shuffle(Random.new(), a);
        Array.length(a) == 1 and Array.toSet(a) == Set#{0}
    }

    @test
    def shuffle03(): Bool & Impure = region r {
        let a = ([0, 1, 2, 3] @ r);
        Array.shuffle(Random.new(), a);
        Array.length(a) == 4 and Array.toSet(a) == Set#{0, 1, 2, 3}
    }

    @test
    def shuffle04(): Bool & Impure = region r {
        let a = ([0, 1, 2, 3, 4, 5, 6, 7, 8, 9] @ r);
        Array.shuffle(Random.new(), a);
        Array.length(a) == 10 and Array.toSet(a) == Set#{0, 1, 2, 3, 4, 5, 6, 7, 8, 9}
    }

}

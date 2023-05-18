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

mod TestArray {

    use Array.sameElements

    /////////////////////////////////////////////////////////////////////////////
    // toString                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toString01(): Bool = region r {
        Array.toString((Array#{} @ r): Array[Int32, _]) == "Array#{}"
    }

    @test
    def toString02(): Bool = region r {
        Array.toString((Array#{1} @ r)) == "Array#{1}"
    }

    @test
    def toString03(): Bool = region r {
        Array.toString((Array#{1, 2, 3, 4} @ r)) == "Array#{1, 2, 3, 4}"
    }

    @test
    def toString04(): Bool = region r {
        Array.toString((Array#{'a', 'b', 'c', 'd'} @ r)) == "Array#{a, b, c, d}"
    }

    /////////////////////////////////////////////////////////////////////////////
    // new                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testNew01(): Bool = region r {
        Array.get(0, Array.new(r, 10)) == 0
    }

    @test
    def testNew02(): Bool = region r {
        Array.get(5, Array.new(r, 10)) == 0
    }

    @test
    def testNew03(): Bool = region r {
        Array.get(9, Array.new(r, 10)) == 0
    }

    @test
    def testNew04(): Bool = region r {
        Array.get(0, Array.new(r, 10)) == false
    }

    @test
    def testNew05(): Bool = region r {
        Array.get(5, Array.new(r, 10)) == false
    }

    @test
    def testNew06(): Bool = region r {
        Array.get(9, Array.new(r, 10)) == false
    }

    /////////////////////////////////////////////////////////////////////////////
    // nth                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testNth01(): Bool = region r {
        Array.nth(0, Array#{1, 2, 3} @ r) == Some(1)
    }

    @test
    def testNth02(): Bool = region r {
        Array.nth(1, Array#{1, 2, 3} @ r) == Some(2)
    }

    @test
    def testNth03(): Bool = region r {
        Array.nth(2, Array#{1, 2, 3} @ r) == Some(3)
    }

    @test
    def testNth04(): Bool = region r {
        Array.nth(3, Array#{1, 2, 3} @ r) == None
    }

    @test
    def testNth05(): Bool = region r {
        Array.nth(-2, Array#{1, 2, 3} @ r) == None
    }

    /////////////////////////////////////////////////////////////////////////////
    // put                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testPut01(): Bool = region r {
        let a = (Array#{1} @ r) !> Array.put(42, 0);
        Array.get(0, a) == 42
    }

    @test
    def testPut02(): Bool = region r {
        let a = (Array#{1, 2} @ r) !> Array.put(42, 1);
        Array.get(1, a) == 42
    }

    @test
    def testPut03(): Bool = region r {
        let a = (Array#{1, 2, 3} @ r) !> Array.put(42, 2);
        Array.get(2, a) == 42
    }

    /////////////////////////////////////////////////////////////////////////////
    // isEmpty                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testIsEmpty01(): Bool = region r {
        Array.isEmpty(Array#{} @ r)
    }

    @test
    def testIsEmpty02(): Bool = region r {
        not Array.isEmpty(Array#{1} @ r)
    }

    @test
    def testIsEmpty03(): Bool = region r {
        not Array.isEmpty(Array#{1, 2, 3} @ r)
    }

    /////////////////////////////////////////////////////////////////////////////
    // length                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testLength01(): Bool = region r {
        Array.length(Array#{} @ r) == 0
    }

    @test
    def testLength02(): Bool = region r {
        Array.length(Array#{1} @ r) == 1
    }

    @test
    def testLength03(): Bool = region r {
        Array.length(Array#{1, 2, 3} @ r) == 3
    }

    /////////////////////////////////////////////////////////////////////////////
    // slice                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testSlice01(): Bool = region r {
        Array.get(0, Array.slice(r, start = 0, end = 1, Array#{1, 2, 3} @ r)) == 1
    }

    @test
    def testSlice02(): Bool = region r {
        Array.get(0, Array.slice(r, start = 1, end = 2, Array#{1, 2, 3} @ r)) == 2
    }

    @test
    def testSlice03(): Bool = region r {
        Array.get(0, Array.slice(r, start = 2, end = 3, Array#{1, 2, 3} @ r)) == 3
    }

    /////////////////////////////////////////////////////////////////////////////
    // toList                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testToList01(): Bool = region r {
        Array.toList((Array#{} @ r): Array[Unit, _]) == Nil
    }

    @test
    def testToList02(): Bool = region r {
        Array.toList(Array#{1} @ r) == 1 :: Nil
    }

    @test
    def testToList03(): Bool = region r {
        Array.toList(Array#{1, 2} @ r) == 1 :: 2 :: Nil
    }

    /////////////////////////////////////////////////////////////////////////////
    // toNel                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toNel01(): Bool = region r {
        Array.toNel((Array#{} @ r): Array[Unit, _]) == None
    }

    @test
    def toNel02(): Bool = region r {
        Array.toNel(Array#{1} @ r) == Some(Nel.singleton(1))
    }

    @test
    def toNel03(): Bool = region r {
        Array.toNel(Array#{1,2} @ r) == List.toNel(1 :: 2 :: Nil)
    }

    @test
    def toNel04(): Bool = region r {
        Array.toNel(Array#{1,2,3} @ r) == List.toNel(1 :: 2 :: 3 :: Nil)
    }

    /////////////////////////////////////////////////////////////////////////////
    // toMutDeque                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toMutDeque01(): Bool = region r {
        let a = (Array#{} @ r): Array[Int32, _];
        let d = MutDeque.new(r): MutDeque[Int32, _];

        Array.toMutDeque(r, a) `MutDeque.sameElements` d
    }

    @test
    def toMutDeque02(): Bool = region r {
        let a = (Array#{1} @ r);

        let d = MutDeque.new(r);
        MutDeque.pushBack(1, d);

        Array.toMutDeque(r, a) `MutDeque.sameElements` d
    }

    @test
    def toMutDeque03(): Bool = region r {
        let a = (Array#{1, 2, 3} @ r);

        let d = MutDeque.new(r);
        MutDeque.pushBack(1, d);
        MutDeque.pushBack(2, d);
        MutDeque.pushBack(3, d);

        Array.toMutDeque(r, a) `MutDeque.sameElements` d
    }

    @test
    def toMutDeque04(): Bool = region r {
        let a = (Array#{1, 2, 3, 4, 5} @ r);

        let d = MutDeque.new(r);
        MutDeque.pushBack(3, d);
        MutDeque.pushBack(4, d);
        MutDeque.pushBack(5, d);
        MutDeque.pushFront(2, d);
        MutDeque.pushFront(1, d);

        Array.toMutDeque(r, a) `MutDeque.sameElements` d
    }

    @test
    def toMutDeque05(): Bool = region r {
        let a = (Array#{'a', 'b', 'c'} @ r);

        let d = MutDeque.new(r);
        MutDeque.pushFront('c', d);
        MutDeque.pushFront('b', d);
        MutDeque.pushFront('a', d);

        Array.toMutDeque(r, a) `MutDeque.sameElements` d
    }

    /////////////////////////////////////////////////////////////////////////////
    // toVector                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toVector01(): Bool = region rc {
        Array.toVector((Array#{} @ rc) : Array[Int32, _]) `Vector.equals` Vector.empty()
    }

    @test
    def toVector02(): Bool = region rc {
        Array.toVector((Array#{0} @ rc) : Array[Int32, _]) `Vector.equals` Vector.singleton(0)
    }

    @test
    def toVector03(): Bool = region rc {
        Array.toVector((Array#{1, 2, 3} @ rc) : Array[Int32, _]) `Vector.equals` List.toVector(1 :: 2 :: 3 :: Nil)
    }

    /////////////////////////////////////////////////////////////////////////////
    // head                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def head01(): Bool = region r {
        Array.head((Array#{} @ r): Array[Int32, _]) == None
    }

    @test
    def head02(): Bool = region r {
        Array.head(Array#{1} @ r) == Some(1)
    }

    @test
    def head03(): Bool = region r {
        Array.head(Array#{2,1} @ r) == Some(2)
    }

    @test
    def head04(): Bool = region r {
        Array.head(Array#{3,2,1} @ r) == Some(3)
    }

    /////////////////////////////////////////////////////////////////////////////
    // last                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def last01(): Bool = region r {
        Array.last(Array#{} @ r): Option[Unit] == None
    }

    @test
    def last02(): Bool = region r {
        Array.last(Array#{1} @ r) == Some(1)
    }

    @test
    def last03(): Bool = region r {
        Array.last(Array#{1,2} @ r) == Some(2)
    }

    @test
    def last04(): Bool = region r {
        Array.last(Array#{1,2,3} @ r) == Some(3)
    }

    /////////////////////////////////////////////////////////////////////////////
    // append                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def append01(): Bool = region r {
        let a = Array.append(r, (Array#{} @ r): Array[Int32, _], (Array#{} @ r): Array[Int32, _]);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def append02(): Bool = region r {
        let a = Array.append(r, (Array#{} @ r): Array[Int32, _], Array#{1} @ r);
        Array.sameElements(a, Array#{1} @ r)
    }

    @test
    def append03(): Bool = region r {
        let a = Array.append(r, (Array#{} @ r): Array[Int32, _], Array#{1,2} @ r);
        Array.sameElements(a, Array#{1,2} @ r)
    }

    @test
    def append04(): Bool = region r {
        let a = Array.append(r, Array#{1} @ r, (Array#{} @ r): Array[Int32, _]);
        Array.sameElements(a, Array#{1} @ r)
    }

    @test
    def append05(): Bool = region r {
        let a = Array.append(r, Array#{1,2} @ r, (Array#{} @ r): Array[Int32, _]);
        Array.sameElements(a, Array#{1,2} @ r)
    }

    @test
    def append06(): Bool = region r {
        let a = Array.append(r, Array#{1} @ r, Array#{2} @ r);
        Array.sameElements(a, Array#{1,2} @ r)
    }

    @test
    def append07(): Bool = region r {
        let a = Array.append(r, Array#{1,2} @ r, Array#{3} @ r);
        Array.sameElements(a, Array#{1,2,3} @ r)
    }

    @test
    def append08(): Bool = region r {
        let a = Array.append(r, Array#{1} @ r, Array#{2,3} @ r);
        Array.sameElements(a, Array#{1,2,3} @ r)
    }

    /////////////////////////////////////////////////////////////////////////////
    // memberOf                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def memberOf01(): Bool = region r {
        Array.memberOf(0, (Array#{} @ r): Array[Int32, _]) == false
    }

    @test
    def memberOf02(): Bool = region r {
        Array.memberOf(0, Array#{1} @ r) == false
    }

    @test
    def memberOf03(): Bool = region r {
        Array.memberOf(0, Array#{0} @ r) == true
    }

    @test
    def memberOf04(): Bool = region r {
        Array.memberOf(0, Array#{1,2} @ r) == false
    }

    @test
    def memberOf05(): Bool = region r {
        Array.memberOf(1, Array#{1,2} @ r) == true
    }

    @test
    def memberOf06(): Bool = region r {
        Array.memberOf(2, Array#{1,2} @ r) == true
    }

    @test
    def memberOf07(): Bool = region r {
        Array.memberOf(3, Array#{1,2} @ r) == false
    }

    /////////////////////////////////////////////////////////////////////////////
    // indexOf                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def indexOf01(): Bool = region r {
        Array.indexOf(0, (Array#{} @ r): Array[Int32, _]) == None
    }

    @test
    def indexOf02(): Bool = region r {
        Array.indexOf(0, Array#{1} @ r) == None
    }

    @test
    def indexOf03(): Bool = region r {
        Array.indexOf(1, Array#{1} @ r) == Some(0)
    }

    @test
    def indexOf04(): Bool = region r {
        Array.indexOf(0, Array#{1,2} @ r) == None
    }

    @test
    def indexOf05(): Bool = region r {
        Array.indexOf(1, Array#{1,2} @ r) == Some(0)
    }

    @test
    def indexOf06(): Bool = region r {
        Array.indexOf(2, Array#{1,2} @ r) == Some(1)
    }

    /////////////////////////////////////////////////////////////////////////////
    // indexOfLeft                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def indexOfLeft01(): Bool = region r {
        Array.indexOfLeft(0, (Array#{} @ r): Array[Int32, _]) == None
    }

    @test
    def indexOfLeft02(): Bool = region r {
        Array.indexOfLeft(0, Array#{1} @ r) == None
    }

    @test
    def indexOfLeft03(): Bool = region r {
        Array.indexOfLeft(1, Array#{1} @ r) == Some(0)
    }

    @test
    def indexOfLeft04(): Bool = region r {
        Array.indexOfLeft(0, Array#{1,2} @ r) == None
    }

    @test
    def indexOfLeft05(): Bool = region r {
        Array.indexOfLeft(1, Array#{1,2} @ r) == Some(0)
    }

    @test
    def indexOfLeft06(): Bool = region r {
        Array.indexOfLeft(2, Array#{1,2} @ r) == Some(1)
    }

    @test
    def indexOfLeft07(): Bool = region r {
        Array.indexOfLeft(1, Array#{1,1} @ r) == Some(0)
    }

    /////////////////////////////////////////////////////////////////////////////
    // indexOfRight                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def indexOfRight01(): Bool = region r {
        Array.indexOfRight(0, (Array#{} @ r): Array[Int32, _]) == None
    }

    @test
    def indexOfRight02(): Bool = region r {
        Array.indexOfRight(0, Array#{1} @ r) == None
    }

    @test
    def indexOfRight03(): Bool = region r {
        Array.indexOfRight(1, Array#{1} @ r) == Some(0)
    }

    @test
    def indexOfRight04(): Bool = region r {
        Array.indexOfRight(0, Array#{1,2} @ r) == None
    }

    @test
    def indexOfRight05(): Bool = region r {
        Array.indexOfRight(1, Array#{1,2} @ r) == Some(0)
    }

    @test
    def indexOfRight06(): Bool = region r {
        Array.indexOfRight(2, Array#{1,2} @ r) == Some(1)
    }

    @test
    def indexOfRight07(): Bool = region r {
        Array.indexOfRight(1, Array#{1,1} @ r) == Some(1)
    }

    /////////////////////////////////////////////////////////////////////////////
    // indices                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def indices01(): Bool = region r {
        let a = Array.indices(r, 0, (Array#{} @ r): Array[Int32, r]);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, r])
    }

    @test
    def indices02(): Bool = region r {
        let a = Array.indices(r, 0, Array#{1} @ r);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, r])
    }

    @test
    def indices03(): Bool = region r {
        let a = Array.indices(r, 1, Array#{1} @ r);
        Array.sameElements(a, Array#{0} @ r)
    }

    @test
    def indices04(): Bool = region r {
        let a = Array.indices(r, 0, Array#{1,2} @ r);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, r])
    }

    @test
    def indices05(): Bool = region r {
        let a = Array.indices(r, 1, Array#{1,2} @ r);
        Array.sameElements(a, Array#{0} @ r)
    }

    @test
    def indices06(): Bool = region r {
        let a = Array.indices(r, 2, Array#{1,2} @ r);
        Array.sameElements(a, Array#{1} @ r)
    }

    @test
    def indices07(): Bool = region r {
        let a = Array.indices(r, 1, Array#{1,1} @ r);
        Array.sameElements(a, Array#{0,1} @ r)
    }
    /////////////////////////////////////////////////////////////////////////////
    // find                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def find01(): Bool = region r {
        Array.find(i -> i > 2, Array#{} @ r) == None
    }

    @test
    def find02(): Bool = region r {
        Array.find(i -> i > 2, Array#{1} @ r) == None
    }

    @test
    def find03(): Bool = region r {
        Array.find(i -> i > 2, Array#{3} @ r) == Some(3)
    }

    @test
    def find04(): Bool = region r {
        Array.find(i -> i > 2, Array#{1,2} @ r) == None
    }

    @test
    def find05(): Bool = region r {
        Array.find(i -> i > 2, Array#{6,-6} @ r) == Some(6)
    }

    @test
    def find06(): Bool = region r {
        Array.find(i -> i > 2, Array#{-6,6} @ r) == Some(6)
    }

    @test
    def find07(): Bool = region r {
        Array.find(i -> i > 2, Array#{6,7} @ r) == Some(6)
    }

    /////////////////////////////////////////////////////////////////////////////
    // findLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def findLeft01(): Bool = region r {
        Array.findLeft(i -> i > 2, Array#{} @ r) == None
    }

    @test
    def findLeft02(): Bool = region r {
        Array.findLeft(i -> i > 2, Array#{1} @ r) == None
    }

    @test
    def findLeft03(): Bool = region r {
        Array.findLeft(i -> i > 2, Array#{3} @ r) == Some(3)
    }

    @test
    def findLeft04(): Bool = region r {
        Array.findLeft(i -> i > 2, Array#{1,2} @ r) == None
    }

    @test
    def findLeft05(): Bool = region r {
        Array.findLeft(i -> i > 2, Array#{6,-6} @ r) == Some(6)
    }

    @test
    def findLeft06(): Bool = region r {
        Array.findLeft(i -> i > 2, Array#{-6,6} @ r) == Some(6)
    }

    @test
    def findLeft07(): Bool = region r {
        Array.findLeft(i -> i > 2, Array#{6,7} @ r) == Some(6)
    }

    /////////////////////////////////////////////////////////////////////////////
    // findRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def findRight01(): Bool = region r {
        Array.findRight(i -> i > 2, Array#{} @ r) == None
    }

    @test
    def findRight02(): Bool = region r {
        Array.findRight(i -> i > 2, Array#{1} @ r) == None
    }

    @test
    def findRight03(): Bool = region r {
        Array.findRight(i -> i > 2, Array#{3} @ r) == Some(3)
    }

    @test
    def findRight04(): Bool = region r {
        Array.findRight(i -> i > 2, Array#{1,2} @ r) == None
    }

    @test
    def findRight05(): Bool = region r {
        Array.findRight(i -> i > 2, Array#{6,-6} @ r) == Some(6)
    }

    @test
    def findRight06(): Bool = region r {
        Array.findRight(i -> i > 2, Array#{-6,6} @ r) == Some(6)
    }

    @test
    def findRight07(): Bool = region r {
        Array.findRight(i -> i > 2, Array#{6,7} @ r) == Some(7)
    }

    /////////////////////////////////////////////////////////////////////////////
    // range                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def range01(): Bool = region r {
        let a = Array.range(r, 1, 0);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def range02(): Bool = region r {
        let a = Array.range(r, 1, 1);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def range03(): Bool = region r {
        let a = Array.range(r, 1, 2);
        Array.sameElements(a, Array#{1} @ r)
    }

    @test
    def range04(): Bool = region r {
        let a = Array.range(r, 1, 3);
        Array.sameElements(a, Array#{1,2} @ r)
    }

    @test
    def range05(): Bool = region r {
        let a = Array.range(r, 1, 4);
        Array.sameElements(a, Array#{1,2,3} @ r)
    }

    @test
    def range06(): Bool = region r {
        let a = Array.range(r, -1, 3);
        Array.sameElements(a, Array#{-1,0,1,2} @ r)
    }

    /////////////////////////////////////////////////////////////////////////////
    // repeat r,
    /////////////////////////////////////////////////////////////////////////////

    @test
    def repeat01(): Bool = region r {
        let a = Array.repeat(r, -1, 1);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def repeat02(): Bool = region r {
        let a = Array.repeat(r, 0, 1);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def repeat03(): Bool = region r {
        let a = Array.repeat(r, 1, 1);
        Array.sameElements(a, Array#{1} @ r)
    }

    @test
    def repeat04(): Bool = region r {
        let a = Array.repeat(r, 2, 1);
        Array.sameElements(a, Array#{1,1} @ r)
    }

    @test
    def repeat05(): Bool = region r {
        let a = Array.repeat(r, 3, 1);
        Array.sameElements(a, Array#{1,1,1} @ r)
    }

    /////////////////////////////////////////////////////////////////////////////
    // scan                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def scan01(): Bool = region r {
        let a = Array.scan(r, (i, b) -> if (b) i + 1 else i + 2, 1, Array#{} @ r);
        Array.sameElements(a, Array#{1} @ r)
    }

    @test
    def scan02(): Bool = region r {
        let a = Array.scan(r, (i, b) -> if (b) i + 1 else i + 2, 1, Array#{false} @ r);
        Array.sameElements(a, Array#{1,3} @ r)
    }

    @test
    def scan03(): Bool = region r {
        let a = Array.scan(r, (i, b) -> if (b) i + 1 else i + 2, 1, Array#{true} @ r);
        Array.sameElements(a, Array#{1,2} @ r)
    }

    @test
    def scan04(): Bool = region r {
        let a = Array.scan(r, (i, b) -> if (b) i + 1 else i + 2, 1, Array#{false,false} @ r);
        Array.sameElements(a, Array#{1,3,5} @ r)
    }

    @test
    def scan05(): Bool = region r {
        let a = Array.scan(r, (i, b) -> if (b) i + 1 else i + 2, 1, Array#{false,true} @ r);
        Array.sameElements(a, Array#{1,3,4} @ r)
    }

    @test
    def scan06(): Bool = region r {
        let a = Array.scan(r, (i, b) -> if (b) i + 1 else i + 2, 1, Array#{true,false} @ r);
        Array.sameElements(a, Array#{1,2,4} @ r)
    }

    @test
    def scan07(): Bool = region r {
        let a = Array.scan(r, (i, b) -> if (b) i + 1 else i + 2, 1, Array#{true,true} @ r);
        Array.sameElements(a, Array#{1,2,3} @ r)
    }

    /////////////////////////////////////////////////////////////////////////////
    // scanLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def scanLeft01(): Bool = region r {
        let a = Array.scanLeft(r, (i, b) -> if (b) i + 1 else i + 2, 1, (Array#{} @ r): Array[Bool, _]);
        Array.sameElements(a, Array#{1} @ r)
    }

    @test
    def scanLeft02(): Bool = region r {
        let a = Array.scanLeft(r, (i, b) -> if (b) i + 1 else i + 2, 1, Array#{false} @ r);
        Array.sameElements(a, Array#{1,3} @ r)
    }

    @test
    def scanLeft03(): Bool = region r {
        let a = Array.scanLeft(r, (i, b) -> if (b) i + 1 else i + 2, 1, Array#{true} @ r);
        Array.sameElements(a, Array#{1,2} @ r)
    }

    @test
    def scanLeft04(): Bool = region r {
        let a = Array.scanLeft(r, (i, b) -> if (b) i + 1 else i + 2, 1, Array#{false,false} @ r);
        Array.sameElements(a, Array#{1,3,5} @ r)
    }

    @test
    def scanLeft05(): Bool = region r {
        let a = Array.scanLeft(r, (i, b) -> if (b) i + 1 else i + 2, 1, Array#{false,true} @ r);
        Array.sameElements(a, Array#{1,3,4} @ r)
    }

    @test
    def scanLeft06(): Bool = region r {
        let a = Array.scanLeft(r, (i, b) -> if (b) i + 1 else i + 2, 1, Array#{true,false} @ r);
        Array.sameElements(a, Array#{1,2,4} @ r)
    }

    @test
    def scanLeft07(): Bool = region r {
        let a = Array.scanLeft(r, (i, b) -> if (b) i + 1 else i + 2, 1, Array#{true,true} @ r);
        Array.sameElements(a, Array#{1,2,3} @ r)
    }

    /////////////////////////////////////////////////////////////////////////////
    // scanRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def scanRight01(): Bool = region r {
        let a = Array.scanRight(r, (b, i) -> if (b) i + 1 else i + 2, 1, Array#{} @ r);
        Array.sameElements(a, Array#{1} @ r)
    }

    @test
    def scanRight02(): Bool = region r {
        let a = Array.scanRight(r, (b, i) -> if (b) i + 1 else i + 2, 1, Array#{false} @ r);
        Array.sameElements(a, Array#{3,1} @ r)
    }

    @test
    def scanRight03(): Bool = region r {
        let a = Array.scanRight(r, (b, i) -> if (b) i + 1 else i + 2, 1, Array#{true} @ r);
        Array.sameElements(a, Array#{2,1} @ r)
    }

    @test
    def scanRight04(): Bool = region r {
        let a = Array.scanRight(r, (b, i) -> if (b) i + 1 else i + 2, 1, Array#{false,false} @ r);
        Array.sameElements(a, Array#{5,3,1} @ r)
    }

    @test
    def scanRight05(): Bool = region r {
        let a = Array.scanRight(r, (b, i) -> if (b) i + 1 else i + 2, 1, Array#{false,true} @ r);
        Array.sameElements(a, Array#{4,2,1} @ r)
    }

    @test
    def scanRight06(): Bool = region r {
        let a = Array.scanRight(r, (b, i) -> if (b) i + 1 else i + 2, 1, Array#{true,false} @ r);
        Array.sameElements(a, Array#{4,3,1} @ r)
    }

    @test
    def scanRight07(): Bool = region r {
        let a = Array.scanRight(r, (b, i) -> if (b) i + 1 else i + 2, 1, Array#{true,true} @ r);
        Array.sameElements(a, Array#{3,2,1} @ r)
    }

    /////////////////////////////////////////////////////////////////////////////
    // map                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def map01(): Bool = region r {
        let a = Array.map(r, i -> i > 2, Array#{} @ r);
        Array.sameElements(a, (Array#{} @ r): Array[Bool, _])
    }

    @test
    def map02(): Bool = region r {
        let a = Array.map(r, i -> i > 2, Array#{1} @ r);
        Array.sameElements(a, Array#{false} @ r)
    }

    @test
    def map03(): Bool = region r {
        let a = Array.map(r, i -> i > 2, Array#{3} @ r);
        Array.sameElements(a, Array#{true} @ r)
    }

    @test
    def map04(): Bool = region r {
        let a = Array.map(r, i -> i > 2, Array#{1,2} @ r);
        Array.sameElements(a, Array#{false,false} @ r)
    }

    @test
    def map05(): Bool = region r {
        let a = Array.map(r, i -> i > 2, Array#{1,8} @ r);
        Array.sameElements(a, Array#{false, true} @ r)
    }

    @test
    def map06(): Bool = region r {
        let a = Array.map(r, i -> i > 2, Array#{8,1} @ r);
        Array.sameElements(a, Array#{true,false} @ r)
    }

    @test
    def map07(): Bool = region r {
        let a = Array.map(r, i -> i > 2, Array#{7,8} @ r);
        Array.sameElements(a, Array#{true,true} @ r)
    }

    /////////////////////////////////////////////////////////////////////////////
    // transform!                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def transform01!(): Bool = region r {
        let a: Array[Int32, _] = Array#{} @ r;
        Array.transform!(x -> x+1, a);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def transform02!(): Bool = region r {
        let a = Array#{0} @ r;
        Array.transform!(x -> x+1, a);
        Array.sameElements(a, Array#{1} @ r)
    }

    @test
    def transform03!(): Bool = region r {
        let a = Array#{0,1} @ r;
        Array.transform!(x -> x+1, a);
        Array.sameElements(a, Array#{1,2} @ r)
    }

    @test
    def transform04!(): Bool = region r {
        let a = Array#{0,1,2} @ r;
        Array.transform!(x -> x+1, a);
        Array.sameElements(a, Array#{1,2,3} @ r)
    }

    /////////////////////////////////////////////////////////////////////////////
    // mapWithIndex                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapWithIndex01(): Bool = region r {
        let a = Array.mapWithIndex(r, (i, e) -> if (i < 1) e > 2 else e <= 2, Array#{} @ r);
        Array.sameElements(a, (Array#{} @ r): Array[Bool, _])
    }

    @test
    def mapWithIndex02(): Bool = region r {
        let a = Array.mapWithIndex(r, (i, e) -> if (i < 1) e > 2 else e <= 2, Array#{1} @ r);
        Array.sameElements(a, Array#{false} @ r)
    }

    @test
    def mapWithIndex03(): Bool = region r {
        let a = Array.mapWithIndex(r, (i, e) -> if (i < 1) e > 2 else e <= 2, Array#{3} @ r);
        Array.sameElements(a, Array#{true} @ r)
    }

    @test
    def mapWithIndex04(): Bool = region r {
        let a = Array.mapWithIndex(r, (i, e) -> if (i < 1) e > 2 else e <= 2, Array#{1,2} @ r);
        Array.sameElements(a, Array#{false,true} @ r)
    }

    @test
    def mapWithIndex05(): Bool = region r {
        let a = Array.mapWithIndex(r, (i, e) -> if (i < 1) e > 2 else e <= 2, Array#{1,8} @ r);
        Array.sameElements(a, Array#{false,false} @ r)
    }

    @test
    def mapWithIndex06(): Bool = region r {
        let a = Array.mapWithIndex(r, (i, e) -> if (i < 1) e > 2 else e <= 2, Array#{8,1} @ r);
        Array.sameElements(a, Array#{true,true} @ r)
    }

    @test
    def mapWithIndex07(): Bool = region r {
        let a = Array.mapWithIndex(r, (i, e) -> if (i < 1) e > 2 else e <= 2, Array#{7,8} @ r);
        Array.sameElements(a, Array#{true,false} @ r)
    }

    /////////////////////////////////////////////////////////////////////////////
    // transformWithIndex!                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def transformWithIndex01!(): Bool = region r {
        let a = (Array#{} @ r): Array[Int32, _];
        Array.transformWithIndex!((i, e) -> if (i < 1) e + 10 else e, a);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def transformWithIndex02!(): Bool = region r {
        let a = Array#{1} @ r;
        Array.transformWithIndex!((i, e) -> if (i < 1) e + 10 else e, a);
        Array.sameElements(a, Array#{11} @ r)
    }

    @test
    def transformWithIndex03!(): Bool = region r {
        let a = Array#{3} @ r;
        Array.transformWithIndex!((i, e) -> if (i < 1) e + 10 else e, a);
        Array.sameElements(a, Array#{13} @ r)
    }

    @test
    def transformWithIndex04!(): Bool = region r {
        let a = Array#{1,2} @ r;
        Array.transformWithIndex!((i, e) -> if (i < 1) e + 10 else e, a);
        Array.sameElements(a, Array#{11,2} @ r)
    }

    @test
    def transformWithIndex05!(): Bool = region r {
        let a = Array#{1,8} @ r;
        Array.transformWithIndex!((i, e) -> if (i < 1) e + 10 else e, a);
        Array.sameElements(a, Array#{11,8} @ r)
    }

    @test
    def transformWithIndex06!(): Bool = region r {
        let a = Array#{8,1} @ r;
        Array.transformWithIndex!((i, e) -> if (i < 1) e + 10 else e, a);
        Array.sameElements(a, Array#{18,1} @ r)
    }

    @test
    def transformWithIndex07!(): Bool = region r {
        let a = Array#{7,8,9} @ r;
        Array.transformWithIndex!((i, e) -> if (i < 1) e + 10 else e, a);
        Array.sameElements(a, Array#{17,8,9} @ r)
    }

    /////////////////////////////////////////////////////////////////////////////
    // flatMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    // depends on flatten

    @test
    def flatMap01(): Bool = region r {
        let a = Array.flatMap(r, i -> Array.repeat(r, i, i), Array#{} @ r);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def flatMap02(): Bool = region r {
        let a = Array.flatMap(r, i -> Array.repeat(r, i, i), Array#{0} @ r);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def flatMap03(): Bool = region r {
        let a = Array.flatMap(r, i -> Array.repeat(r, i, i), Array#{1} @ r);
        Array.sameElements(a, Array#{1} @ r)
    }

    @test
    def flatMap04(): Bool = region r {
        let a = Array.flatMap(r, i -> Array.repeat(r, i, i), Array#{2} @ r);
        Array.sameElements(a, Array#{2,2} @ r)
    }

    @test
    def flatMap05(): Bool = region r {
        let a = Array.flatMap(r, i -> Array.repeat(r, i, i), Array#{1,2} @ r);
        Array.sameElements(a, Array#{1,2,2} @ r)
    }

    @test
    def flatMap06(): Bool = region r {
        let a = Array.flatMap(r, i -> Array.repeat(r, i, i), Array#{2,3} @ r);
        Array.sameElements(a, Array#{2,2,3,3,3} @ r)
    }

    /////////////////////////////////////////////////////////////////////////////
    // reverse                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reverse01(): Bool = region r {
        let a = Array.reverse(r, (Array#{} @ r): Array[Int32, _]);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def reverse02(): Bool = region r {
        let a = Array.reverse(r, Array#{1} @ r);
        Array.sameElements(a, Array#{1} @ r)
    }

    @test
    def reverse03(): Bool = region r {
        let a = Array.reverse(r, Array#{1,2} @ r);
        Array.sameElements(a, Array#{2,1} @ r)
    }

    @test
    def reverse04(): Bool = region r {
        let a = Array.reverse(r, Array#{1,1} @ r);
        Array.sameElements(a, Array#{1,1} @ r)
    }

    @test
    def reverse05(): Bool = region r {
        let a = Array.reverse(r, Array#{1,2,3} @ r);
        Array.sameElements(a, Array#{3,2,1} @ r)
    }

    @test
    def reverse06(): Bool = region r {
        let a = Array.reverse(r, Array#{1,2,3,4} @ r);
        Array.sameElements(a, Array#{4,3,2,1} @ r)
    }

    /////////////////////////////////////////////////////////////////////////////
    // reverse!                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reverse01!(): Bool = region r {
        let a: Array[Int32, _] = Array#{} @ r;
        Array.reverse!(a);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def reverse02!(): Bool = region r {
        let a = Array#{1} @ r;
        Array.reverse!(a);
        Array.sameElements(a, Array#{1} @ r)
    }

    @test
    def reverse03!(): Bool = region r {
        let a = Array#{1,2} @ r;
        Array.reverse!(a);
        Array.sameElements(a, Array#{2,1} @ r)
    }

    @test
    def reverse04!(): Bool = region r {
        let a = Array#{1,1} @ r;
        Array.reverse!(a);
        Array.sameElements(a, Array#{1,1} @ r)
    }

    @test
    def reverse05!(): Bool = region r {
        let a = Array#{1,2,3} @ r;
        Array.reverse!(a);
        Array.sameElements(a, Array#{3,2,1} @ r)
    }

    @test
    def reverse06!(): Bool = region r {
        let a = Array#{1,2,3,4} @ r;
        Array.reverse!(a);
        Array.sameElements(a, Array#{4,3,2,1} @ r)
    }

    /////////////////////////////////////////////////////////////////////////////
    // rotateLeft                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def rotateLeft01(): Bool = region r {
        let a = Array.rotateLeft(r, 0, (Array#{} @ r): Array[Int32, _]);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def rotateLeft02(): Bool = region r {
        let a = Array.rotateLeft(r, 1, (Array#{} @ r): Array[Int32, _]);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def rotateLeft03(): Bool = region r {
        let a = Array.rotateLeft(r, 0, Array#{1} @ r);
        Array.sameElements(a, Array#{1} @ r)
    }

    @test
    def rotateLeft04(): Bool = region r {
        let a = Array.rotateLeft(r, 0, Array#{1,2} @ r);
        Array.sameElements(a, Array#{1,2} @ r)
    }

    @test
    def rotateLeft05(): Bool = region r {
        let a = Array.rotateLeft(r, 1, Array#{1,2} @ r);
        Array.sameElements(a, Array#{2,1} @ r)
    }

    @test
    def rotateLeft06(): Bool = region r {
        let a = Array.rotateLeft(r, 2, Array#{1,2} @ r);
        Array.sameElements(a, Array#{1,2} @ r)
    }

    @test
    def rotateLeft07(): Bool = region r {
        let a = Array.rotateLeft(r, 3, Array#{1,2} @ r);
        Array.sameElements(a, Array#{2,1} @ r)
    }

    @test
    def rotateLeft08(): Bool = region r {
        let a = Array.rotateLeft(r, -1, Array#{1,2} @ r);
        Array.sameElements(a, Array#{2,1} @ r)
    }

    @test
    def rotateLeft09(): Bool = region r {
        let a = Array.rotateLeft(r, 0, Array#{1,2,3} @ r);
        Array.sameElements(a, Array#{1,2,3} @ r)
    }

    @test
    def rotateLeft10(): Bool = region r {
        let a = Array.rotateLeft(r, 1, Array#{1,2,3} @ r);
        Array.sameElements(a, Array#{2,3,1} @ r)
    }

    @test
    def rotateLeft11(): Bool = region r {
        let a = Array.rotateLeft(r, 2, Array#{1,2,3} @ r);
        Array.sameElements(a, Array#{3,1,2} @ r)
    }

    /////////////////////////////////////////////////////////////////////////////
    // rotateRight                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def rotateRight01(): Bool = region r {
        let a = Array.rotateRight(r, 0, (Array#{} @ r): Array[Int32, _]);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def rotateRight02(): Bool = region r {
        let a = Array.rotateRight(r, 1, (Array#{} @ r): Array[Int32, _]);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def rotateRight03(): Bool = region r {
        let a = Array.rotateRight(r, 0, Array#{1} @ r);
        Array.sameElements(a, Array#{1} @ r)
    }

    @test
    def rotateRight04(): Bool = region r {
        let a = Array.rotateRight(r, 0, Array#{1,2} @ r);
        Array.sameElements(a, Array#{1,2} @ r)
    }

    @test
    def rotateRight05(): Bool = region r {
        let a = Array.rotateRight(r, 1, Array#{1,2} @ r);
        Array.sameElements(a, Array#{2,1} @ r)
    }

    @test
    def rotateRight06(): Bool = region r {
        let a = Array.rotateRight(r, 2, Array#{1,2} @ r);
        Array.sameElements(a, Array#{1,2} @ r)
    }

    @test
    def rotateRight07(): Bool = region r {
        let a = Array.rotateRight(r, 3, Array#{1,2} @ r);
        Array.sameElements(a, Array#{2,1} @ r)
    }

    @test
    def rotateRight08(): Bool = region r {
        let a = Array.rotateRight(r, -1, Array#{1,2} @ r);
        Array.sameElements(a, Array#{2,1} @ r)
    }

    @test
    def rotateRight09(): Bool = region r {
        let a = Array.rotateRight(r, 0, Array#{1,2,3} @ r);
        Array.sameElements(a, Array#{1,2,3} @ r)
    }

    @test
    def rotateRight10(): Bool = region r {
        let a = Array.rotateRight(r, 1, Array#{1,2,3} @ r);
        Array.sameElements(a, Array#{3,1,2} @ r)
    }

    @test
    def rotateRight11(): Bool = region r {
        let a = Array.rotateRight(r, 2, Array#{1,2,3} @ r);
        Array.sameElements(a, Array#{2,3,1} @ r)
    }

    /////////////////////////////////////////////////////////////////////////////
    // update                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def update01(): Bool = region r {
        let a = Array.update(r, 0, 2, (Array#{} @ r): Array[Int32, _]);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def update02(): Bool = region r {
        let a = Array.update(r, -1, 2, Array#{1} @ r);
        Array.sameElements(a, Array#{1} @ r)
    }

    @test
    def update03(): Bool = region r {
        let a = Array.update(r, 0, 2, Array#{1} @ r);
        Array.sameElements(a, Array#{2} @ r)
    }

    @test
    def update04(): Bool = region r {
        let a = Array.update(r, 1, 2, Array#{1} @ r);
        Array.sameElements(a, Array#{1} @ r)
    }

    @test
    def update05(): Bool = region r {
        let a = Array.update(r, 0, 5, Array#{1,2} @ r);
        Array.sameElements(a, Array#{5,2} @ r)
    }

    @test
    def update06(): Bool = region r {
        let a = Array.update(r, 1, 5, Array#{1,2} @ r);
        Array.sameElements(a, Array#{1,5} @ r)
    }

    @test
    def update07(): Bool = region r {
        let a = Array.update(r, 2, 5, Array#{1,2} @ r);
        Array.sameElements(a, Array#{1,2} @ r)
    }

    /////////////////////////////////////////////////////////////////////////////
    // replace                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def replace01(): Bool = region r {
        let a = Array.replace(r, from = 3, to = 4, Array#{} @ r);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def replace02(): Bool = region r {
        let a = Array.replace(r, from = 3, to = 4, Array#{1} @ r);
        Array.sameElements(a, Array#{1} @ r)
    }

    @test
    def replace03(): Bool = region r {
        let a = Array.replace(r, from = 3, to = 4, Array#{3} @ r);
        Array.sameElements(a, Array#{4} @ r)
    }

    @test
    def replace04(): Bool = region r {
        let a = Array.replace(r, from = 3, to = 4, Array#{4} @ r);
        Array.sameElements(a, Array#{4} @ r)
    }

    @test
    def replace05(): Bool = region r {
        let a = Array.replace(r, from = 3, to = 4, Array#{1, 2} @ r);
        Array.sameElements(a, Array#{1,2} @ r)
    }

    @test
    def replace06(): Bool = region r {
        let a = Array.replace(r, from = 3, to = 4, Array#{1,3} @ r);
        Array.sameElements(a, Array#{1,4} @ r)
    }

    @test
    def replace07(): Bool = region r {
        let a = Array.replace(r, from = 3, to = 4, Array#{3,4} @ r);
        Array.sameElements(a, Array#{4,4} @ r)
    }

    @test
    def replace08(): Bool = region r {
        let a = Array.replace(r, from = 3, to = 4, Array#{3,3} @ r);
        Array.sameElements(a, Array#{4,4} @ r)
    }

    /////////////////////////////////////////////////////////////////////////////
    // replace!                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def replace01!(): Bool = region r {
        let a: Array[Int32, _] = Array#{} @ r;
        Array.replace!(from = 3, to = 4, a);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def replace02!(): Bool = region r {
        let a = Array#{1} @ r;
        Array.replace!(from = 3, to = 4, a);
        Array.sameElements(a, Array#{1} @ r)
    }

    @test
    def replace03!(): Bool = region r {
        let a = Array#{3} @ r;
        Array.replace!(from = 3, to = 4, a);
        Array.sameElements(a, Array#{4} @ r)
    }

    @test
    def replace04!(): Bool = region r {
        let a = Array#{4} @ r;
        Array.replace!(from = 3, to = 4, a);
        Array.sameElements(a, Array#{4} @ r)
    }

    @test
    def replace05!(): Bool = region r {
        let a = Array#{1, 2} @ r;
        Array.replace!(from = 3, to = 4, a);
        Array.sameElements(a, Array#{1,2} @ r)
    }

    @test
    def replace06!(): Bool = region r {
        let a = Array#{1,3} @ r;
        Array.replace!(from = 3, to = 4, a);
        Array.sameElements(a, Array#{1,4} @ r)
    }

    @test
    def replace07!(): Bool = region r {
        let a = Array#{3,4} @ r;
        Array.replace!(from = 3, to = 4, a);
        Array.sameElements(a, Array#{4,4} @ r)
    }

    @test
    def replace08!(): Bool = region r {
        let a = Array#{3,3} @ r;
        Array.replace!(from = 3, to = 4, a);
        Array.sameElements(a, Array#{4,4} @ r)
    }

    /////////////////////////////////////////////////////////////////////////////
    // patch                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def patch01(): Bool = region r {
        let a = Array.patch(r, 0, 0, (Array#{} @ r): Array[Int32, _], (Array#{} @ r): Array[Int32, _]);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def patch02(): Bool = region r {
        let a = Array.patch(r, 0, 2, Array#{1,2} @ r, Array#{} @ r);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def patch03(): Bool = region r {
        let a = Array.patch(r, 0, 2, Array#{} @ r, Array#{1,2} @ r);
        Array.sameElements(a, Array#{1,2} @ r)
    }

    @test
    def patch04(): Bool = region r {
        let a = Array.patch(r, -3, 3, Array#{1,2,4} @ r, Array#{1,2} @ r);
        Array.sameElements(a, Array#{1,2} @ r)
    }

    @test
    def patch05(): Bool = region r {
        let a = Array.patch(r, 2, 3, Array#{1,2,4} @ r, Array#{1,2} @ r);
        Array.sameElements(a, Array#{1,2} @ r)
    }

    @test
    def patch06(): Bool = region r {
        let a = Array.patch(r, 0, 0, Array#{} @ r, Array#{1} @ r);
        Array.sameElements(a, Array#{1} @ r)
    }

    @test
    def patch07(): Bool = region r {
        let a = Array.patch(r, 1, 0, Array#{2} @ r, Array#{1} @ r);
        Array.sameElements(a, Array#{1} @ r)
    }

    @test
    def patch08(): Bool = region r {
        let a = Array.patch(r, 0, 1, Array#{2} @ r, Array#{1} @ r);
        Array.sameElements(a, Array#{2} @ r)
    }

    @test
    def patch09(): Bool = region r {
        let a = Array.patch(r, 0, 2, Array#{2,4} @ r, Array#{1} @ r);
        Array.sameElements(a, Array#{2} @ r)
    }

    @test
    def patch10(): Bool = region r {
        let a = Array.patch(r, -1, 2, Array#{2,4} @ r, Array#{1} @ r);
        Array.sameElements(a, Array#{4} @ r)
    }

    @test
    def patch11(): Bool = region r {
        let a = Array.patch(r, -1, 2, Array#{3,4} @ r, Array#{1,2} @ r);
        Array.sameElements(a, Array#{4,2} @ r)
    }

    @test
    def patch12(): Bool = region r {
        let a = Array.patch(r, 1, 2, Array#{3,4} @ r, Array#{1,2} @ r);
        Array.sameElements(a, Array#{1,3} @ r)
    }

    @test
    def patch13(): Bool = region r {
        let a = Array.patch(r, -2, 2, Array#{3,4} @ r, Array#{1,2} @ r);
        Array.sameElements(a, Array#{1,2} @ r)
    }

    @test
    def patch14(): Bool = region r {
        let a = Array.patch(r, 2, 2, Array#{3,4} @ r, Array#{1,2} @ r);
        Array.sameElements(a, Array#{1,2} @ r)
    }

    @test
    def patch15(): Bool = region r {
        let a = Array.patch(r, 1, 1, Array#{3} @ r, Array#{1,2} @ r);
        Array.sameElements(a, Array#{1,3} @ r)
    }

    @test
    def patch16(): Bool = region r {
        let a = Array.patch(r, 0, 2, Array#{3,4} @ r, Array#{1,2} @ r);
        Array.sameElements(a, Array#{3,4} @ r)
    }

    @test
    def patch17(): Bool = region r {
        let a = Array.patch(r, 0, 1, Array#{4} @ r, Array#{1, 2, 3} @ r);
        Array.sameElements(a, Array#{4, 2, 3} @ r)
    }

    @test
    def patch18(): Bool = region r {
        let a = Array.patch(r, 1, 1, Array#{4} @ r, Array#{1, 2, 3} @ r);
        Array.sameElements(a, Array#{1, 4, 3} @ r)
    }

    @test
    def patch19(): Bool = region r {
        let a = Array.patch(r, 2, 1, Array#{4} @ r, Array#{1, 2, 3} @ r);
        Array.sameElements(a, Array#{1, 2, 4} @ r)
    }

    @test
    def patch20(): Bool = region r {
        let a = Array.patch(r, 0, 2, Array#{4, 5} @ r, Array#{1, 2, 3} @ r);
        Array.sameElements(a, Array#{4, 5, 3} @ r)
    }

    @test
    def patch21(): Bool = region r {
        let a = Array.patch(r, 1, 2, Array#{4, 5} @ r, Array#{1, 2, 3} @ r);
        Array.sameElements(a, Array#{1, 4, 5} @ r)
    }

    @test
    def patch22(): Bool = region r {
        let a = Array.patch(r, 0, 2, Array#{4, 5, 6} @ r, Array#{1, 2, 3} @ r);
        Array.sameElements(a, Array#{4, 5, 3} @ r)
    }

    @test
    def patch23(): Bool = region r {
        let a = Array.patch(r, 0, 3, Array#{4, 5, 6} @ r, Array#{1, 2, 3} @ r);
        Array.sameElements(a, Array#{4, 5, 6} @ r)
    }

    @test
    def patch24(): Bool = region r {
        let a = Array.patch(r, 2, 4, Array#{14, 15, 16, 17} @ r, Array#{1, 2, 3, 4, 5, 6, 7} @ r);
        Array.sameElements(a, Array#{1, 2, 14, 15, 16, 17, 7} @ r)
    }

    @test
    def patch25(): Bool = region r {
        let a = Array.patch(r, -2, 4, Array#{14, 15, 16, 17} @ r, Array#{1, 2, 3, 4, 5, 6, 7} @ r);
        Array.sameElements(a, Array#{16, 17, 3, 4, 5, 6, 7} @ r)
    }

    @test
    def patch26(): Bool = region r {
        let a = Array.patch(r, 4, 5, Array#{14, 15, 16, 17} @ r, Array#{1, 2, 3, 4, 5, 6, 7} @ r);
        Array.sameElements(a, Array#{1, 2, 3, 4, 14, 15, 16} @ r)
    }

    @test
    def patch27(): Bool = region r {
        let a = Array.patch(r, 4, 2, Array#{14, 15, 16, 17} @ r, Array#{1, 2, 3, 4, 5, 6, 7} @ r);
        Array.sameElements(a, Array#{1, 2, 3, 4, 14, 15, 7} @ r)
    }

    @test
    def patch28(): Bool = region r {
        let a = Array.patch(r, -1, 10, Array#{-1, -2, -3, -4, -5, -6, -7, -8} @ r, Array#{1, 2, 3, 4, 5, 6, 7} @ r);
        Array.sameElements(a, Array#{-2, -3, -4, -5, -6, -7, -8} @ r)
    }

    /////////////////////////////////////////////////////////////////////////////
    // patch!                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def patch01!(): Bool = region r {
        let a = (Array#{} @ r): Array[Int32, _];
        Array.patch!(0, 0, (Array#{} @ r): Array[Int32, _], a);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def patch02!(): Bool = region r {
        let a = (Array#{} @ r): Array[Int32, _];
        Array.patch!(0, 2, Array#{1,2} @ r, a);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def patch03!(): Bool = region r {
        let a = Array#{1,2} @ r;
        Array.patch!(0, 2, Array#{} @ r, a);
        Array.sameElements(a, Array#{1,2} @ r)
    }

    @test
    def patch04!(): Bool = region r {
        let a = Array#{1,2} @ r;
        Array.patch!(-3, 3, Array#{1,2,4} @ r, a);
        Array.sameElements(a, Array#{1,2} @ r)
    }

    @test
    def patch05!(): Bool = region r {
        let a = Array#{1,2} @ r;
        Array.patch!(2, 3, Array#{1,2,4} @ r, a);
        Array.sameElements(a, Array#{1,2} @ r)
    }

    @test
    def patch06!(): Bool = region r {
        let a = Array#{1} @ r;
        Array.patch!(0, 0, Array#{} @ r, a);
        Array.sameElements(a, Array#{1} @ r)
    }

    @test
    def patch07!(): Bool = region r {
        let a = Array#{1} @ r;
        Array.patch!(1, 0, Array#{2} @ r, a);
        Array.sameElements(a, Array#{1} @ r)
    }

    @test
    def patch08!(): Bool = region r {
        let a = Array#{1} @ r;
        Array.patch!(0, 1, Array#{2} @ r, a);
        Array.sameElements(a, Array#{2} @ r)
    }

    @test
    def patch09!(): Bool = region r {
        let a = Array#{1} @ r;
        Array.patch!(0, 2, Array#{2,4} @ r, a);
        Array.sameElements(a, Array#{2} @ r)
    }

    @test
    def patch10!(): Bool = region r {
        let a = Array#{1} @ r;
        Array.patch!(-1, 2, Array#{2,4} @ r, a);
        Array.sameElements(a, Array#{4} @ r)
    }

    @test
    def patch11!(): Bool = region r {
        let a = Array#{1,2} @ r;
        Array.patch!(-1, 2, Array#{3,4} @ r, a);
        Array.sameElements(a, Array#{4,2} @ r)
    }

    @test
    def patch12!(): Bool = region r {
        let a = Array#{1,2} @ r;
        Array.patch!(1, 2, Array#{3,4} @ r, a);
        Array.sameElements(a, Array#{1,3} @ r)
    }

    @test
    def patch13!(): Bool = region r {
        let a = Array#{1,2} @ r;
        Array.patch!(-2, 2, Array#{3,4} @ r, a);
        Array.sameElements(a, Array#{1,2} @ r)
    }

    @test
    def patch14!(): Bool = region r {
        let a = Array#{1,2} @ r;
        Array.patch!(2, 2, Array#{3,4} @ r, a);
        Array.sameElements(a, Array#{1,2} @ r)
    }

    @test
    def patch15!(): Bool = region r {
        let a = Array#{1,2} @ r;
        Array.patch!(1, 1, Array#{3} @ r, a);
        Array.sameElements(a, Array#{1,3} @ r)
    }

    @test
    def patch16!(): Bool = region r {
        let a = Array#{1,2} @ r;
        Array.patch!(0, 2, Array#{3,4} @ r, a);
        Array.sameElements(a, Array#{3,4} @ r)
    }

    @test
    def patch17!(): Bool = region r {
        let a = Array#{1, 2, 3} @ r;
        Array.patch!(0, 1, Array#{4} @ r, a);
        Array.sameElements(a, Array#{4, 2, 3} @ r)
    }

    @test
    def patch18!(): Bool = region r {
        let a = Array#{1, 2, 3} @ r;
        Array.patch!(1, 1, Array#{4} @ r, a);
        Array.sameElements(a, Array#{1, 4, 3} @ r)
    }

    @test
    def patch19!(): Bool = region r {
        let a = Array#{1, 2, 3} @ r;
        Array.patch!(2, 1, Array#{4} @ r, a);
        Array.sameElements(a, Array#{1, 2, 4} @ r)
    }

    @test
    def patch20!(): Bool = region r {
        let a = Array#{1, 2, 3} @ r;
        Array.patch!(0, 2, Array#{4, 5} @ r, a);
        Array.sameElements(a, Array#{4, 5, 3} @ r)
    }

    @test
    def patch21!(): Bool = region r {
        let a = Array#{1, 2, 3} @ r;
        Array.patch!(1, 2, Array#{4, 5} @ r, a);
        Array.sameElements(a, Array#{1, 4, 5} @ r)
    }

    @test
    def patch22!(): Bool = region r {
        let a = Array#{1, 2, 3} @ r;
        Array.patch!(0, 2, Array#{4, 5, 6} @ r, a);
        Array.sameElements(a, Array#{4, 5, 3} @ r)
    }

    @test
    def patch23!(): Bool = region r {
        let a = Array#{1, 2, 3} @ r;
        Array.patch!(0, 3, Array#{4, 5, 6} @ r, a);
        Array.sameElements(a, Array#{4, 5, 6} @ r)
    }

    @test
    def patch24!(): Bool = region r {
        let a = Array#{1, 2, 3, 4, 5, 6, 7} @ r;
        Array.patch!(2, 4, Array#{14, 15, 16, 17} @ r, a);
        Array.sameElements(a, Array#{1, 2, 14, 15, 16, 17, 7} @ r)
    }

    @test
    def patch25!(): Bool = region r {
        let a = Array#{1, 2, 3, 4, 5, 6, 7} @ r;
        Array.patch!(-2, 4, Array#{14, 15, 16, 17} @ r, a);
        Array.sameElements(a, Array#{16, 17, 3, 4, 5, 6, 7} @ r)
    }

    @test
    def patch26!(): Bool = region r {
        let a = Array#{1, 2, 3, 4, 5, 6, 7} @ r;
        Array.patch!(4, 5, Array#{14, 15, 16, 17} @ r, a);
        Array.sameElements(a, Array#{1, 2, 3, 4, 14, 15, 16} @ r)
    }

    @test
    def patch27!(): Bool = region r {
        let a = Array#{1, 2, 3, 4, 5, 6, 7} @ r;
        Array.patch!(4, 2, Array#{14, 15, 16, 17} @ r, a);
        Array.sameElements(a, Array#{1, 2, 3, 4, 14, 15, 7} @ r)
    }

    @test
    def patch28!(): Bool = region r {
        let a = Array#{1, 2, 3, 4, 5, 6, 7} @ r;
        Array.patch!(-1, 10, Array#{-1, -2, -3, -4, -5, -6, -7, -8} @ r, a);
        Array.sameElements(a, Array#{-2, -3, -4, -5, -6, -7, -8} @ r)
    }

    /////////////////////////////////////////////////////////////////////////////
    // intersperse                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def intersperse01(): Bool = region r {
        let a = Array.intersperse(r, 11, Array#{} @ r);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def intersperse02(): Bool = region r {
        let a = Array.intersperse(r, 11, Array#{1} @ r);
        Array.sameElements(a, Array#{1} @ r)
    }

    @test
    def intersperse03(): Bool = region r {
        let a = Array.intersperse(r, 11, Array#{1,2} @ r);
        Array.sameElements(a, Array#{1,11,2} @ r)
    }

    @test
    def intersperse04(): Bool = region r {
        let a = Array.intersperse(r, 11, Array#{1,2,3} @ r);
        Array.sameElements(a, Array#{1,11,2,11,3} @ r)
    }

    @test
    def intersperse05(): Bool = region r {
        let a = Array.intersperse(r, 11, Array#{1,2,3,4} @ r);
        Array.sameElements(a, Array#{1,11,2,11,3,11,4} @ r)
    }


    /////////////////////////////////////////////////////////////////////////////
    // intercalate                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def intercalate01(): Bool = region r {
        let a = Array.intercalate(r, (Array#{} @ r): Array[Int32, r], (Array#{} @ r): Array[Array[Int32, r], r]);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, r])
    }

    @test
    def intercalate02(): Bool = region r {
        let a = Array.intercalate(r, (Array#{} @ r): Array[Int32, r], Array#{Array#{1} @ r} @ r);
        Array.sameElements(a, Array#{1} @ r)
    }

    @test
    def intercalate03(): Bool = region r {
        let a = Array.intercalate(r, Array#{11,12,13} @ r, (Array#{} @ r): Array[Array[Int32, r], r]);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, r])
    }

    @test
    def intercalate04(): Bool = region r {
        let a = Array.intercalate(r, (Array#{} @ r): Array[Int32, r], Array#{Array#{1} @ r, Array#{2,3} @ r} @ r);
        Array.sameElements(a, Array#{1,2,3} @ r)
    }

    @test
    def intercalate05(): Bool = region r {
        let a = Array.intercalate(r, Array#{11,12,13} @ r, Array#{Array#{1} @ r, Array#{2,3} @ r} @ r);
        Array.sameElements(a, Array#{1,11,12,13,2,3} @ r)
    }

    @test
    def intercalate06(): Bool = region r {
        let a = Array.intercalate(r, (Array#{} @ r): Array[Int32, r], Array#{Array#{1} @ r, Array#{2,3} @ r, Array#{4} @ r} @ r);
        Array.sameElements(a, Array#{1,2,3,4} @ r)
    }

    @test
    def intercalate07(): Bool = region r {
        let a = Array.intercalate(r, Array#{11,12,13} @ r, Array#{Array#{1} @ r, Array#{2,3} @ r, Array#{4} @ r} @ r);
        Array.sameElements(a, Array#{1,11,12,13,2,3,11,12,13,4} @ r)
    }

    /////////////////////////////////////////////////////////////////////////////
    // transpose                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def transpose01(): Bool = region r {
        let a = Array.transpose(r, Array#{} @ r);
        let b = Array.toList(a) |> List.map(Array.toList);
        b: List[List[Unit]] == Nil
    }

    @test
    def transpose02(): Bool = region r {
        let a: Array[Array[Int32, _], _] = Array.transpose(r, Array#{Array#{} @ r} @ r);
        let b = Array.toList(a) |> List.map(Array.toList);
        b == Nil :: Nil
    }

    @test
    def transpose03(): Bool = region r {
        let a: Array[Array[Int32, _], _] = Array.transpose(r, Array#{Array#{} @ r, Array#{} @ r} @ r);
        let b = Array.toList(a) |> List.map(Array.toList);
        b == Nil :: Nil :: Nil
    }

    @test
    def transpose04(): Bool = region r {
        let a: Array[Array[Int32, _], _] = Array.transpose(r, Array#{Array#{} @ r, Array#{} @ r, Array#{} @ r} @ r);
        let b = Array.toList(a) |> List.map(Array.toList);
        b == Nil :: Nil :: Nil :: Nil
    }

    @test
    def transpose05(): Bool = region r {
        let a = Array.transpose(r, Array#{ Array#{1} @ r } @ r);
        let b = Array.toList(a) |> List.map(Array.toList);
        b == (1 :: Nil) :: Nil
    }

    @test
    def transpose06(): Bool = region r {
        let a = Array.transpose(r, Array#{Array#{1,2} @ r} @ r);
        let b = Array.toList(a) |> List.map(Array.toList);
        b == (1 :: Nil) :: (2 :: Nil) :: Nil
    }

    @test
    def transpose07(): Bool = region r {
        let a = Array.transpose(r, Array#{Array#{1, 2, 3} @ r} @ r);
        let b = Array.toList(a) |> List.map(Array.toList);
        b == (1 :: Nil) :: (2 :: Nil) :: (3 :: Nil) :: Nil
    }

    @test
    def transpose08(): Bool = region r {
        let a = Array.transpose(r, Array#{Array#{1, 2, 3, 4} @ r} @ r);
        let b = Array.toList(a) |> List.map(Array.toList);
        b == (1 :: Nil) :: (2 :: Nil) :: (3 :: Nil) :: (4 :: Nil) :: Nil
    }

    @test
    def transpose09(): Bool = region r {
        let a = Array.transpose(r, Array#{Array#{1} @ r, Array#{2} @ r} @ r);
        let b = Array.toList(a) |> List.map(Array.toList);
        b ==   (1 :: 2 :: Nil) :: Nil
    }

    @test
    def transpose10(): Bool = region r {
        let a = Array.transpose(r, Array#{Array#{1} @ r, Array#{2} @ r, Array#{3} @ r} @ r);
        let b = Array.toList(a) |> List.map(Array.toList);
        b ==  (1 :: 2 :: 3 :: Nil) :: Nil
    }

    @test
    def transpose11(): Bool = region r {
        let a = Array.transpose(r, Array#{Array#{1} @ r, Array#{2} @ r, Array#{3} @ r, Array#{4} @ r} @ r);
        let b = Array.toList(a) |> List.map(Array.toList);
        b == (1 :: 2 :: 3 :: 4 :: Nil) :: Nil
    }

    @test
    def transpose12(): Bool = region r {
        let a = Array.transpose(r, Array#{Array#{1, 2} @ r, Array#{3, 4} @ r} @ r);
        let b = Array.toList(a) |> List.map(Array.toList);
        b == (1 :: 3 :: Nil) :: (2 :: 4 :: Nil) :: Nil
    }

    @test
    def transpose13(): Bool = region r {
        let a = Array.transpose(r, Array#{Array#{1, 2, 3} @ r, Array#{4, 5, 6} @ r} @ r);
        let b = Array.toList(a) |> List.map(Array.toList);
        b == (1 :: 4 :: Nil) :: (2 :: 5 :: Nil) :: (3 :: 6 :: Nil) :: Nil
    }

    @test
    def transpose14(): Bool = region r {
        let a = Array.transpose(r, Array#{Array#{1, 2, 3, 4} @ r, Array#{5, 6, 7, 8} @ r} @ r);
        let b = Array.toList(a) |> List.map(Array.toList);
        b == (1 :: 5 :: Nil) :: (2 :: 6 :: Nil) :: (3 :: 7 :: Nil) :: (4 :: 8 :: Nil) :: Nil
    }

    @test
    def transpose15(): Bool = region r {
        let a = Array.transpose(r, Array#{Array#{1, 2, 3, 4, 5} @ r, Array#{6, 7, 8, 9, 10} @ r} @ r);
        let b = Array.toList(a) |> List.map(Array.toList);
        b == (1 :: 6 :: Nil) :: (2 :: 7 :: Nil) :: (3 :: 8 :: Nil) :: (4 :: 9 :: Nil) :: (5 :: 10 :: Nil) :: Nil
    }

    @test
    def transpose16(): Bool = region r {
        let a = Array.transpose(r, Array#{Array#{1, 2} @ r, Array#{3, 4} @ r, Array#{5, 6} @ r} @ r);
        let b = Array.toList(a) |> List.map(Array.toList);
        b == (1 :: 3 :: 5 :: Nil) :: (2 :: 4 :: 6 :: Nil) :: Nil
    }

    @test
    def transpose17(): Bool = region r {
        let a = Array.transpose(r, Array#{Array#{1, 2} @ r, Array#{3, 4} @ r, Array#{5, 6} @ r, Array#{7,8} @ r} @ r);
        let b = Array.toList(a) |> List.map(Array.toList);
        b == (1 :: 3 :: 5 :: 7 :: Nil) :: (2 :: 4 :: 6 :: 8 :: Nil) :: Nil
    }

    @test
    def transpose18(): Bool = region r {
        let a = Array.transpose(r, Array#{Array#{1, 2} @ r, Array#{3, 4} @ r, Array#{5, 6} @ r, Array#{7, 8} @ r, Array#{9, 10} @ r} @ r);
        let b = Array.toList(a) |> List.map(Array.toList);
        b == (1 :: 3 :: 5 :: 7 :: 9 :: Nil) :: (2 :: 4 :: 6 :: 8 :: 10 :: Nil) :: Nil
    }

    @test
    def transpose19(): Bool = region r {
        let a = Array.transpose(r, Array#{Array#{1, 2, 3} @ r, Array#{4, 5, 6} @ r, Array#{7, 8, 9} @ r} @ r);
        let b = Array.toList(a) |> List.map(Array.toList);
        b == (1 :: 4 :: 7 :: Nil) :: (2 :: 5 :: 8 :: Nil) :: (3 :: 6 :: 9 :: Nil) :: Nil
    }

    @test
    def transpose20(): Bool = region r {
        let a = Array.transpose(r, Array#{Array#{1, 2, 3} @ r, Array#{4, 5} @ r, Array#{7, 8, 9} @ r} @ r);
        let b = Array.toList(a) |> List.map(Array.toList);
        b == (1 :: 2 :: 3 :: Nil) :: (4 :: 5 :: Nil) :: (7 :: 8 :: 9 :: Nil) :: Nil
    }

    @test
    def transpose21(): Bool = region r {
        let a = Array.transpose(r, Array#{Array#{1, 2, 3} @ r, Array#{} @ r, Array#{7, 8, 9} @ r} @ r);
        let b = Array.toList(a) |> List.map(Array.toList);
        b == (1 :: 2 :: 3 :: Nil) :: Nil :: (7 :: 8 :: 9 :: Nil) :: Nil
    }

    @test
    def transpose22(): Bool = region r {
        let a = Array.transpose(r, Array#{Array#{1, 2, 3} @ r, Array#{4, 5, 6} @ r, Array#{7, 8, 9, 10} @ r} @ r);
        let b = Array.toList(a) |> List.map(Array.toList);
        b == (1 :: 2 :: 3 :: Nil) :: (4 :: 5 :: 6 :: Nil) :: (7 :: 8 :: 9 :: 10 :: Nil) :: Nil
    }

    /////////////////////////////////////////////////////////////////////////////
    // isPrefixOf                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def isPrefixOf01(): Bool = region r {
        Array.isPrefixOf((Array#{} @ r): Array[Int32, _], (Array#{} @ r): Array[Int32, _])
    }

    @test
    def isPrefixOf02(): Bool = region r {
        Array.isPrefixOf(Array#{1} @ r, (Array#{} @ r): Array[Int32, _]) == false
    }

    @test
    def isPrefixOf03(): Bool = region r {
        Array.isPrefixOf((Array#{} @ r): Array[Int32, _], Array#{1} @ r)
    }

    @test
    def isPrefixOf04(): Bool = region r {
        Array.isPrefixOf(Array#{1} @ r, Array#{1} @ r)
    }

    @test
    def isPrefixOf05(): Bool = region r {
        Array.isPrefixOf(Array#{2} @ r, Array#{1} @ r) == false
    }

    @test
    def isPrefixOf06(): Bool = region r {
        Array.isPrefixOf(Array#{1,2} @ r, Array#{1} @ r) == false
    }

    @test
    def isPrefixOf07(): Bool = region r {
        Array.isPrefixOf((Array#{} @ r): Array[Int32, _], Array#{1,2} @ r)
    }

    @test
    def isPrefixOf08(): Bool = region r {
        Array.isPrefixOf(Array#{1} @ r, Array#{1,2} @ r)
    }

    @test
    def isPrefixOf09(): Bool = region r {
        Array.isPrefixOf(Array#{2} @ r, Array#{1,2} @ r) == false
    }

    @test
    def isPrefixOf10(): Bool = region r {
        Array.isPrefixOf(Array#{1,2} @ r, Array#{1,2} @ r)
    }

    @test
    def isPrefixOf11(): Bool = region r {
        Array.isPrefixOf(Array#{1,3} @ r, Array#{1,2} @ r) == false
    }

    @test
    def isPrefixOf12(): Bool = region r {
        Array.isPrefixOf(Array#{1,2,3} @ r, Array#{1,2} @ r) == false
    }

    @test
    def isPrefixOf13(): Bool = region r {
        Array.isPrefixOf(Array#{1,2,3} @ r, Array#{89,11,1,2,3,4} @ r) == false
    }

    /////////////////////////////////////////////////////////////////////////////
    // isInfixOf                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def isInfixOf01(): Bool = region r {
        Array.isInfixOf((Array#{} @ r): Array[Int32, _], (Array#{} @ r): Array[Int32, _])
    }

    @test
    def isInfixOf02(): Bool = region r {
        Array.isInfixOf(Array#{1} @ r, (Array#{} @ r): Array[Int32, _]) == false
    }

    @test
    def isInfixOf03(): Bool = region r {
        Array.isInfixOf((Array#{} @ r): Array[Int32, _], Array#{1} @ r)
    }

    @test
    def isInfixOf04(): Bool = region r {
        Array.isInfixOf(Array#{1} @ r, Array#{1} @ r)
    }

    @test
    def isInfixOf05(): Bool = region r {
        Array.isInfixOf(Array#{2} @ r, Array#{1} @ r) == false
    }

    @test
    def isInfixOf06(): Bool = region r {
        Array.isInfixOf(Array#{1,2} @ r, Array#{1} @ r) == false
    }

    @test
    def isInfixOf07(): Bool = region r {
        Array.isInfixOf((Array#{} @ r): Array[Int32, _], Array#{1,2} @ r)
    }

    @test
    def isInfixOf08(): Bool = region r {
        Array.isInfixOf(Array#{1} @ r, Array#{1,2} @ r)
    }

    @test
    def isInfixOf09(): Bool = region r {
        Array.isInfixOf(Array#{2} @ r, Array#{1,2} @ r)
    }

    @test
    def isInfixOf10(): Bool = region r {
        Array.isInfixOf(Array#{1,2} @ r, Array#{1,2} @ r)
    }

    @test
    def isInfixOf11(): Bool = region r {
        Array.isInfixOf(Array#{1,3} @ r, Array#{1,2} @ r) == false
    }

    @test
    def isInfixOf12(): Bool = region r {
        Array.isInfixOf(Array#{1,2,3} @ r, Array#{1,2} @ r) == false
    }

    @test
    def isInfixOf13(): Bool = region r {
        Array.isInfixOf(Array#{1,2,3} @ r, Array#{89,11,1,2,3,4} @ r)
    }

    /////////////////////////////////////////////////////////////////////////////
    // isSuffixOf                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def isSuffixOf01(): Bool = region r {
        Array.isSuffixOf((Array#{} @ r): Array[Int32, _], (Array#{} @ r): Array[Int32, _])
    }

    @test
    def isSuffixOf02(): Bool = region r {
        Array.isSuffixOf(Array#{1} @ r, (Array#{} @ r): Array[Int32, _]) == false
    }

    @test
    def isSuffixOf03(): Bool = region r {
        Array.isSuffixOf((Array#{} @ r): Array[Int32, _], Array#{1} @ r)
    }

    @test
    def isSuffixOf04(): Bool = region r {
        Array.isSuffixOf(Array#{1} @ r, Array#{1} @ r)
    }

    @test
    def isSuffixOf05(): Bool = region r {
        Array.isSuffixOf(Array#{2} @ r, Array#{1} @ r) == false
    }

    @test
    def isSuffixOf06(): Bool = region r {
        Array.isSuffixOf(Array#{1,2} @ r, Array#{1} @ r) == false
    }

    @test
    def isSuffixOf07(): Bool = region r {
        Array.isSuffixOf((Array#{} @ r): Array[Int32, _], Array#{1,2} @ r)
    }

    @test
    def isSuffixOf08(): Bool = region r {
        Array.isSuffixOf(Array#{1} @ r, Array#{1,2} @ r) == false
    }

    @test
    def isSuffixOf09(): Bool = region r {
        Array.isSuffixOf(Array#{2} @ r, Array#{1,2} @ r)
    }

    @test
    def isSuffixOf10(): Bool = region r {
        Array.isSuffixOf(Array#{1,2} @ r, Array#{1,2} @ r)
    }

    @test
    def isSuffixOf11(): Bool = region r {
        Array.isSuffixOf(Array#{1,3} @ r, Array#{1,2} @ r) == false
    }

    @test
    def isSuffixOf12(): Bool = region r {
        Array.isSuffixOf(Array#{1,2,3} @ r, Array#{1,2} @ r) == false
    }

    @test
    def isSuffixOf13(): Bool = region r {
        Array.isSuffixOf(Array#{1,2,3} @ r, Array#{89,11,1,2,3} @ r)
    }

    /////////////////////////////////////////////////////////////////////////////
    // eq                                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testSameElements01(): Bool = region r {
        ((Array#{} @ r): Array[Int32, _]) `sameElements` ((Array#{} @ r): Array[Int32, _])
    }

    @test
    def testSameElements02(): Bool = region r {
        not ((Array#{1} @ r) `sameElements` Array#{} @ r)
    }

    @test
    def testSameElements03(): Bool = region r {
        not ((Array#{} @ r) `sameElements` Array#{1} @ r)
    }

    @test
    def testSameElements04(): Bool = region r {
        (Array#{1} @ r) `sameElements` Array#{1} @ r
    }

    @test
    def testSameElements05(): Bool = region r {
        not ((Array#{1} @ r) `sameElements` Array#{2} @ r)
    }

    @test
    def testSameElements06(): Bool = region r {
        not ((Array#{1,2} @ r) `sameElements` Array#{1} @ r)
    }

    @test
    def testSameElements07(): Bool = region r {
        not ((Array#{1} @ r) `sameElements` Array#{1,2} @ r)
    }

    @test
    def testSameElements08(): Bool = region r {
        (Array#{1,2,3,4} @ r) `sameElements` Array#{1,2,3,4} @ r
    }

    /////////////////////////////////////////////////////////////////////////////
    // compare                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testCompare01(): Bool = region r {
        ((Array#{} @ r): Array[Int32, _]) `Array.compare` ((Array#{} @ r): Array[Int32, _]) == Comparison.EqualTo
    }

    @test
    def testCompare02(): Bool = region r {
        ((Array#{1} @ r) `Array.compare` (Array#{} @ r)) == Comparison.GreaterThan
    }

    @test
    def testCompare03(): Bool = region r {
        ((Array#{} @ r) `Array.compare` (Array#{1} @ r)) == Comparison.LessThan
    }

    @test
    def testCompare04(): Bool = region r {
        ((Array#{1} @ r) `Array.compare` (Array#{1} @ r)) == Comparison.EqualTo
    }

    @test
    def testCompare05(): Bool = region r {
        ((Array#{1} @ r) `Array.compare` (Array#{2} @ r)) == Comparison.LessThan
    }

    @test
    def testCompare06(): Bool = region r {
        ((Array#{2} @ r) `Array.compare` (Array#{1} @ r)) == Comparison.GreaterThan
    }

    @test
    def testCompare07(): Bool = region r {
        ((Array#{1} @ r) `Array.compare` (Array#{1,2} @ r)) == Comparison.LessThan
    }

    @test
    def testCompare08(): Bool = region r {
        ((Array#{1,2} @ r) `Array.compare` (Array#{1} @ r)) == Comparison.GreaterThan
    }

    @test
    def testCompare09(): Bool = region r {
        ((Array#{1} @ r) `Array.compare` (Array#{0,0} @ r)) == Comparison.GreaterThan
    }

    @test
    def testCompare10(): Bool = region r {
        ((Array#{0,0} @ r) `Array.compare` (Array#{1} @ r)) == Comparison.LessThan
    }

    @test
    def testCompare11(): Bool = region r {
        ((Array#{1,2} @ r) `Array.compare` (Array#{1,1} @ r)) == Comparison.GreaterThan
    }

    @test
    def testCompare12(): Bool = region r {
        ((Array#{1,2} @ r) `Array.compare` (Array#{1,3} @ r)) == Comparison.LessThan
    }

    @test
    def testCompare13(): Bool = region r {
        ((Array#{1,2,3,4} @ r) `Array.compare` (Array#{1,2,3,4} @ r)) == Comparison.EqualTo
    }

    /////////////////////////////////////////////////////////////////////////////
    // fold                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def fold01(): Bool = region r {
        Array.fold((Array#{} @ r): Array[Unit, _]) == ()
    }

    @test
    def fold02(): Bool = region r {
        Array.fold(Array#{"a"} @ r) == "a"
    }

    @test
    def fold03(): Bool = region r {
        Array.fold(Array#{"a", "b", "c"} @ r) == "abc"
    }

    @test
    def fold04(): Bool = region r {
        Array.fold(Array#{("yes", "no"), ("no", "yes")} @ r) == ("yesno", "noyes")
    }

    /////////////////////////////////////////////////////////////////////////////
    // foldLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldLeft01(): Bool = region r {
        Array.foldLeft((i, e) -> (i - e) * (e `Int32.rem` 2 + 1), 100, Array#{} @ r) == 100
    }

    @test
    def foldLeft02(): Bool = region r {
        Array.foldLeft((i, e) -> (i - e) * (e `Int32.rem` 2 + 1), 100, Array#{1} @ r) == 198
    }

    @test
    def foldLeft03(): Bool = region r {
        Array.foldLeft((i, e) -> (i - e) * (e `Int32.rem` 2 + 1), 100, Array#{1,2} @ r) == 196
    }

    @test
    def foldLeft04(): Bool = region r {
        Array.foldLeft((i, e) -> (i - e) * (e `Int32.rem` 2 + 1), 100, Array#{1,2,3} @ r) == 386
    }

    /////////////////////////////////////////////////////////////////////////////
    // foldRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldRight01(): Bool = region r {
        Array.foldRight((e, i) -> (i - e) * (e `Int32.rem` 2 + 1), 100, Array#{} @ r) == 100
    }

    @test
    def foldRight02(): Bool = region r {
        Array.foldRight((e, i) -> (i - e) * (e `Int32.rem` 2 + 1), 100, Array#{1} @ r) == 198
    }

    @test
    def foldRight03(): Bool = region r {
        Array.foldRight((e, i) -> (i - e) * (e `Int32.rem` 2 + 1), 100, Array#{1,2} @ r) == 194
    }

    @test
    def foldRight04(): Bool = region r {
        Array.foldRight((e, i) -> (i - e) * (e `Int32.rem` 2 + 1), 100, Array#{1,2,3} @ r) == 382
    }

    /////////////////////////////////////////////////////////////////////////////
    // foldRightWithCont                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldRightWithCont01(): Bool = region r {
        Array.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.rem` 2 + 1), 100, Array#{} @ r) == 100
    }

    @test
    def foldRightWithCont02(): Bool = region r {
        Array.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.rem` 2 + 1), 100, Array#{1} @ r) == 198
    }

    @test
    def foldRightWithCont03(): Bool = region r {
        Array.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.rem` 2 + 1), 100, Array#{1,2} @ r) == 194
    }

    @test
    def foldRightWithCont04(): Bool = region r {
        Array.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.rem` 2 + 1), 100, Array#{1,2,3} @ r) == 382
    }

    /////////////////////////////////////////////////////////////////////////////
    // foldMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldMap01(): Bool = region r {
        Array.foldMap(x -> 2 * x, Array#{} @ r) == 0
    }

    @test
    def foldMap02(): Bool = region r {
        Array.foldMap(x -> 2 * x, Array#{1, 2} @ r) == 6
    }

    @test
    def foldMap03(): Bool = region r {
        Array.foldMap(x -> if (x == "a") "b" else x, Array#{"a"} @ r) == "b"
    }

    @test
    def foldMap04(): Bool = region r {
        Array.foldMap(x -> if (x == "c") "b" else x, Array#{"a", "b", "c"} @ r) == "abb"
    }

    @test
    def foldMap05(): Bool = region r {
        Array.foldMap(Int32.toString, Array#{1, 2, 3} @ r) == "123"
    }

    /////////////////////////////////////////////////////////////////////////////
    // reduceLeft                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reduceLeft01(): Bool = region r {
        Array.reduceLeft((a, b) -> a - b, (Array#{} @ r): Array[Int32, _]) == None
    }

    @test
    def reduceLeft02(): Bool = region r {
        Array.reduceLeft((a, b) -> a - b, Array#{1} @ r) == Some(1)
    }

    @test
    def reduceLeft03(): Bool = region r {
        Array.reduceLeft((a, b) -> a - b, Array#{1,2} @ r) == Some(-1)
    }

    @test
    def reduceLeft04(): Bool = region r {
        Array.reduceLeft((a, b) -> a - b, Array#{1,2,3} @ r) == Some(-4)
    }

    @test
    def reduceLeft05(): Bool = region r {
        Array.reduceLeft((a, b) -> a - b, Array#{1,2,3,4} @ r) == Some(-8)
    }

    /////////////////////////////////////////////////////////////////////////////
    // reduceRight                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reduceRight01(): Bool = region r {
        Array.reduceRight((a, b) -> a - b, (Array#{} @ r): Array[Int32, _]) == None
    }

    @test
    def reduceRight02(): Bool = region r {
        Array.reduceRight((a, b) -> a - b, Array#{1} @ r) == Some(1)
    }

    @test
    def reduceRight03(): Bool = region r {
        Array.reduceRight((a, b) -> a - b, Array#{1,2} @ r) == Some(-1)
    }

    @test
    def reduceRight04(): Bool = region r {
        Array.reduceRight((a, b) -> a - b, Array#{1,2,3} @ r) == Some(2)
    }

    @test
    def reduceRight05(): Bool = region r {
        Array.reduceRight((a, b) -> a - b, Array#{1,2,3,4} @ r) == Some(-2)
    }

    /////////////////////////////////////////////////////////////////////////////
    // count                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def count01(): Bool = region r {
        Array.count(i -> i > 3, Array#{} @ r) == 0
    }

    @test
    def count02(): Bool = region r {
        Array.count(i -> i > 3, Array#{1} @ r) == 0
    }

    @test
    def count03(): Bool = region r {
        Array.count(i -> i > 3, Array#{4} @ r) == 1
    }

    @test
    def count04(): Bool = region r {
        Array.count(i -> i > 3, Array#{1,2} @ r) == 0
    }

    @test
    def count05(): Bool = region r {
        Array.count(i -> i > 3, Array#{1,8} @ r) == 1
    }

    @test
    def count06(): Bool = region r {
        Array.count(i -> i > 3, Array#{8,1} @ r) == 1
    }

    @test
    def count07(): Bool = region r {
        Array.count(i -> i > 3, Array#{6,7} @ r) == 2
    }


    /////////////////////////////////////////////////////////////////////////////
    // flatten                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def flatten01(): Bool = region r {
        let a = Array.flatten(r, (Array#{} @ r): Array[Array[Int32, r], r]);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, r])
    }

    @test
    def flatten02(): Bool = region r {
        let a = Array.flatten(r, (Array#{Array#{} @ r} @ r): Array[Array[Int32, r], r]);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, r])
    }

    @test
    def flatten03(): Bool = region r {
        let a = Array.flatten(r, Array#{Array#{1} @ r} @ r);
        Array.sameElements(a, Array#{1} @ r)
    }

    @test
    def flatten04(): Bool = region r {
        let a= Array.flatten(r, Array#{Array#{1,2} @ r} @ r);
        Array.sameElements(a, Array#{1,2} @ r)
    }

    @test
    def flatten05(): Bool = region r {
        let a = Array.flatten(r, (Array#{Array#{} @ r,Array#{} @ r} @ r): Array[Array[Int32, r], r]);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, r])
    }

    @test
    def flatten06(): Bool = region r {
        let a = Array.flatten(r, Array#{Array#{1} @ r,Array#{} @ r} @ r);
        Array.sameElements(a, Array#{1} @ r)
    }

    @test
    def flatten07(): Bool = region r {
        let a = Array.flatten(r, Array#{Array#{} @ r,Array#{1} @ r} @ r);
        Array.sameElements(a, Array#{1} @ r)
    }

    @test
    def flatten08(): Bool = region r {
        let a = Array.flatten(r, Array#{Array#{1} @ r,Array#{2} @ r} @ r);
        Array.sameElements(a, Array#{1,2} @ r)
    }

    @test
    def flatten09(): Bool = region r {
        let a = Array.flatten(r, Array#{Array#{1,2} @ r,Array#{3,4,5} @ r} @ r);
        Array.sameElements(a, Array#{1,2,3,4,5} @ r)
    }

    @test
    def flatten10(): Bool = region r {
        let a = Array.flatten(r, Array#{Array#{1} @ r,Array#{2,3} @ r,Array#{4} @ r} @ r);
        Array.sameElements(a, Array#{1,2,3,4} @ r)
    }
    /////////////////////////////////////////////////////////////////////////////
    // exists                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def exists01(): Bool = region r {
        Array.exists(i -> i > 3, (Array#{} @ r): Array[Int32, _]) == false
    }

    @test
    def exists02(): Bool = region r {
        Array.exists(i -> i > 3, Array#{1} @ r) == false
    }

    @test
    def exists03(): Bool = region r {
        Array.exists(i -> i > 3, Array#{5} @ r) == true
    }

    @test
    def exists04(): Bool = region r {
        Array.exists(i -> i > 3, Array#{1,2} @ r) == false
    }

    @test
    def exists05(): Bool = region r {
        Array.exists(i -> i > 3, Array#{1,6} @ r) == true
    }

    @test
    def exists06(): Bool = region r {
        Array.exists(i -> i > 3, Array#{6,1} @ r) == true
    }

    @test
    def exists07(): Bool = region r {
        Array.exists(i -> i > 3, Array#{16,6} @ r) == true
    }

    @test
    def exists08(): Bool = region r {
        Array.exists(i -> i > 3, Array#{1,-9,3} @ r) == false
    }

    @test
    def exists09(): Bool = region r {
        Array.exists(i -> i > 3, Array#{1,9,3} @ r) == true
    }

    /////////////////////////////////////////////////////////////////////////////
    // forAll                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def forAll01(): Bool = region r {
        Array.forAll(i -> i > 3, (Array#{} @ r): Array[Int32, _]) == true
    }

    @test
    def forAll02(): Bool = region r {
        Array.forAll(i -> i > 3, Array#{1} @ r) == false
    }

    @test
    def forAll03(): Bool = region r {
        Array.forAll(i -> i > 3, Array#{5} @ r) == true
    }

    @test
    def forAll04(): Bool = region r {
        Array.forAll(i -> i > 3, Array#{1,2} @ r) == false
    }

    @test
    def forAll05(): Bool = region r {
        Array.forAll(i -> i > 3, Array#{1,6} @ r) == false
    }

    @test
    def forAll06(): Bool = region r {
        Array.forAll(i -> i > 3, Array#{6,1} @ r) == false
    }

    @test
    def forAll07(): Bool = region r {
        Array.forAll(i -> i > 3, Array#{16,6} @ r) == true
    }

    @test
    def forAll08(): Bool = region r {
        Array.forAll(i -> i > 3, Array#{1,-9,3} @ r) == false
    }

    @test
    def forAll09(): Bool = region r {
        Array.forAll(i -> i > 3, Array#{1,9,3} @ r) == false
    }

    @test
    def forAll10(): Bool = region r {
        Array.forAll(i -> i > 3, Array#{11,9,31} @ r) == true
    }

    /////////////////////////////////////////////////////////////////////////////
    // filter                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filter01(): Bool = region r {
        let a = Array.filter(r, i -> i > 3, (Array#{} @ r): Array[Int32, _]);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def filter02(): Bool = region r {
        let a = Array.filter(r, i -> i > 3, Array#{2} @ r);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def filter03(): Bool = region r {
        let a = Array.filter(r, i -> i > 3, Array#{4} @ r);
        Array.sameElements(a, Array#{4} @ r)
    }

    @test
    def filter04(): Bool = region r {
        let a = Array.filter(r, i -> i > 3, Array#{1,3} @ r);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def filter05(): Bool = region r {
        let a = Array.filter(r, i -> i > 3, Array#{1,8} @ r);
        Array.sameElements(a, Array#{8} @ r)
    }

    @test
    def filter06(): Bool = region r {
        let a = Array.filter(r, i -> i > 3, Array#{8,1} @ r);
        Array.sameElements(a, Array#{8} @ r)
    }

    @test
    def filter07(): Bool = region r {
        let a = Array.filter(r, i -> i > 3, Array#{8,9} @ r);
        Array.sameElements(a, Array#{8,9} @ r)
    }

    @test
    def filter08(): Bool = region r {
        let a = Array.filter(r, i -> i > 3, Array#{1,4,11,2,-22,17} @ r);
        Array.sameElements(a, Array#{4,11,17} @ r)
    }

    /////////////////////////////////////////////////////////////////////////////
    // partition                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def partition01(): Bool = region r {
        let (a,b) = Array.partition(r, r, i -> i > 3, (Array#{} @ r): Array[Int32, _]);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _]) and Array.sameElements(b, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def partition02(): Bool = region r {
        let (a,b) = Array.partition(r, r, i -> i > 3, Array#{1} @ r);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _]) and Array.sameElements(b, Array#{1} @ r)
    }

    @test
    def partition03(): Bool = region r {
        let (a,b) = Array.partition(r, r, i -> i > 3, Array#{4} @ r);
        Array.sameElements(a, Array#{4} @ r) and Array.sameElements(b, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def partition04(): Bool = region r {
        let (a,b) = Array.partition(r, r, i -> i > 3, Array#{1,2} @ r);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _]) and Array.sameElements(b, Array#{1,2} @ r)
    }

    @test
    def partition05(): Bool = region r {
        let (a,b) = Array.partition(r, r, i -> i > 3, Array#{1,5} @ r);
        Array.sameElements(a, Array#{5} @ r) and Array.sameElements(b, Array#{1} @ r)
    }

    @test
    def partition06(): Bool = region r {
        let (a,b) = Array.partition(r, r, i -> i > 3, Array#{5,1} @ r);
        Array.sameElements(a, Array#{5} @ r) and Array.sameElements(b, Array#{1} @ r)
    }

    @test
    def partition07(): Bool = region r {
        let (a,b) = Array.partition(r, r, i -> i > 3, Array#{5,8} @ r);
        Array.sameElements(a, Array#{5,8} @ r) and Array.sameElements(b, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def partition08(): Bool = region r {
        let (a,b) = Array.partition(r, r, i -> i > 3, Array#{4, -3, -5, 1, 2, 16, 7, 1, 7} @ r);
        Array.sameElements(a, Array#{4, 16, 7, 7} @ r) and Array.sameElements(b, Array#{-3, -5, 1, 2, 1} @ r)
    }

    /////////////////////////////////////////////////////////////////////////////
    // span                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def span01(): Bool = region r {
        let (a,b) = Array.span(r, r, i -> i > 3, (Array#{} @ r): Array[Int32, _]);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _]) and Array.sameElements(b, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def span02(): Bool = region r {
        let (a,b) = Array.span(r, r, i -> i > 3, Array#{1} @ r);
        Array.sameElements(a,(Array#{} @ r): Array[Int32, _]) and Array.sameElements(b, Array#{1} @ r)
    }

    @test
    def span03(): Bool = region r {
        let (a,b) = Array.span(r, r, i -> i > 3, Array#{4} @ r);
        Array.sameElements(a, Array#{4} @ r) and Array.sameElements(b, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def span04(): Bool = region r {
        let (a,b) = Array.span(r, r, i -> i > 3, Array#{1,2} @ r);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _]) and Array.sameElements(b, Array#{1,2} @ r)
    }

    @test
    def span05(): Bool = region r {
        let (a,b) = Array.span(r, r, i -> i > 3, Array#{1,5} @ r);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _]) and Array.sameElements(b, Array#{1,5} @ r)
    }

    @test
    def span06(): Bool = region r {
        let (a,b) = Array.span(r, r, i -> i > 3, Array#{5,1} @ r);
        Array.sameElements(a, Array#{5} @ r) and Array.sameElements(b, Array#{1} @ r)
    }

    @test
    def span07(): Bool = region r {
        let (a,b) = Array.span(r, r, i -> i > 3, Array#{5,8} @ r);
        Array.sameElements(a, Array#{5,8} @ r) and Array.sameElements(b, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def span08(): Bool = region r {
        let (a,b) = Array.span(r, r, i -> i > 3, Array#{4, 6, -3, 11, -5, 1, 2, 16, 7, 1, 7} @ r);
        Array.sameElements(a, Array#{4, 6} @ r) and Array.sameElements(b, Array#{-3, 11, -5, 1, 2, 16, 7, 1, 7} @ r)
    }

    /////////////////////////////////////////////////////////////////////////////
    // drop                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def drop01(): Bool = region r {
        let a = Array.drop(r, -1, (Array#{} @ r): Array[Int32, _]);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def drop02(): Bool = region r {
        let a = Array.drop(r, 0, (Array#{} @ r): Array[Int32, _]);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def drop03(): Bool = region r {
        let a = Array.drop(r, 1, (Array#{} @ r): Array[Int32, _]);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def drop04(): Bool = region r {
        let a = Array.drop(r, -1, Array#{1} @ r);
        Array.sameElements(a, Array#{1} @ r)
    }

    @test
    def drop05(): Bool = region r {
        let a = Array.drop(r, 0, Array#{1} @ r);
        Array.sameElements(a, Array#{1} @ r)
    }

    @test
    def drop06(): Bool = region r {
        let a = Array.drop(r, 1, Array#{1} @ r);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def drop07(): Bool = region r {
        let a = Array.drop(r, 2, Array#{1} @ r);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def drop08(): Bool = region r {
        let a = Array.drop(r, 0, Array#{1,2} @ r);
        Array.sameElements(a, Array#{1,2} @ r)
    }

    @test
    def drop09(): Bool = region r {
        let a = Array.drop(r, 1, Array#{1,2} @ r);
        Array.sameElements(a, Array#{2} @ r)
    }

    @test
    def drop10(): Bool = region r {
        let a = Array.drop(r, 2, Array#{1,2} @ r);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def drop11(): Bool = region r {
        let a = Array.drop(r, 2, Array#{1,2,3,4,5,6} @ r);
        Array.sameElements(a, Array#{3,4,5,6} @ r)
    }

    @test
    def drop12(): Bool = region r {
        let a = Array.drop(r, 4, Array#{1,2,3,4,5,6} @ r);
        Array.sameElements(a, Array#{5,6} @ r)
    }

    /////////////////////////////////////////////////////////////////////////////
    // dropLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def dropLeft01(): Bool = region r {
        let a = Array.dropLeft(r, -1, (Array#{} @ r): Array[Int32, _]);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def dropLeft02(): Bool = region r {
        let a = Array.dropLeft(r, 0, (Array#{} @ r): Array[Int32, _]);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def dropLeft03(): Bool = region r {
        let a = Array.dropLeft(r, 1, (Array#{} @ r): Array[Int32, _]);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def dropLeft04(): Bool = region r {
        let a = Array.dropLeft(r, -1, Array#{1} @ r);
        Array.sameElements(a, Array#{1} @ r)
    }

    @test
    def dropLeft05(): Bool = region r {
        let a = Array.dropLeft(r, 0, Array#{1} @ r);
        Array.sameElements(a, Array#{1} @ r)
    }

    @test
    def dropLeft06(): Bool = region r {
        let a = Array.dropLeft(r, 1, Array#{1} @ r);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def dropLeft07(): Bool = region r {
        let a = Array.dropLeft(r, 2, Array#{1} @ r);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def dropLeft08(): Bool = region r {
        let a = Array.dropLeft(r, 0, Array#{1,2} @ r);
        Array.sameElements(a, Array#{1,2} @ r)
    }

    @test
    def dropLeft09(): Bool = region r {
        let a = Array.dropLeft(r, 1, Array#{1,2} @ r);
        Array.sameElements(a, Array#{2} @ r)
    }

    @test
    def dropLeft10(): Bool = region r {
        let a = Array.dropLeft(r, 2, Array#{1,2} @ r);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def dropLeft11(): Bool = region r {
        let a = Array.dropLeft(r, 2, Array#{1,2,3,4,5,6} @ r);
        Array.sameElements(a, Array#{3,4,5,6} @ r)
    }

    @test
    def dropLeft12(): Bool = region r {
        let a = Array.dropLeft(r, 4, Array#{1,2,3,4,5,6} @ r);
        Array.sameElements(a, Array#{5,6} @ r)
    }

    /////////////////////////////////////////////////////////////////////////////
    // dropRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def dropRight01(): Bool = region r {
        let a = Array.dropRight(r, -1, (Array#{} @ r): Array[Int32, _]);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def dropRight02(): Bool = region r {
        let a = Array.dropRight(r, 0, (Array#{} @ r): Array[Int32, _]);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def dropRight03(): Bool = region r {
        let a = Array.dropRight(r, 1, (Array#{} @ r): Array[Int32, _]);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def dropRight04(): Bool = region r {
        let a = Array.dropRight(r, -1, Array#{1} @ r);
        Array.sameElements(a, Array#{1} @ r)
    }

    @test
    def dropRight05(): Bool = region r {
        let a = Array.dropRight(r, 0, Array#{1} @ r);
        Array.sameElements(a, Array#{1} @ r)
    }

    @test
    def dropRight06(): Bool = region r {
        let a = Array.dropRight(r, 1, Array#{1} @ r);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def dropRight07(): Bool = region r {
        let a = Array.dropRight(r, 2, Array#{1} @ r);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def dropRight08(): Bool = region r {
        let a = Array.dropRight(r, 0, Array#{1,2} @ r);
        Array.sameElements(a, Array#{1,2} @ r)
    }

    @test
    def dropRight09(): Bool = region r {
        let a = Array.dropRight(r, 1, Array#{1,2} @ r);
        Array.sameElements(a, Array#{1} @ r)
    }

    @test
    def dropRight10(): Bool = region r {
        let a = Array.dropRight(r, 2, Array#{1,2} @ r);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def dropRight11(): Bool = region r {
        let a = Array.dropRight(r, 2, Array#{1,2,3,4,5,6} @ r);
        Array.sameElements(a, Array#{1,2,3,4} @ r)
    }

    @test
    def dropRight12(): Bool = region r {
        let a = Array.dropRight(r, 4, Array#{1,2,3,4,5,6} @ r);
        Array.sameElements(a, Array#{1,2} @ r)
    }

    /////////////////////////////////////////////////////////////////////////////
    // dropWhile                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def dropWhile01(): Bool = region r {
        let a = Array.dropWhile(r, i -> i > 3, Array#{} @ r);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def dropWhile02(): Bool = region r {
        let a = Array.dropWhile(r, i -> i > 3, Array#{1} @ r);
        Array.sameElements(a, Array#{1} @ r)
    }

    @test
    def dropWhile03(): Bool = region r {
        let a = Array.dropWhile(r, i -> i > 3, Array#{4} @ r);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def dropWhile04(): Bool = region r {
        let a = Array.dropWhile(r, i -> i > 3, Array#{1,2} @ r);
        Array.sameElements(a, Array#{1,2} @ r)
    }

    @test
    def dropWhile05(): Bool = region r {
        let a = Array.dropWhile(r, i -> i > 3, Array#{1,5} @ r);
        Array.sameElements(a, Array#{1,5} @ r)
    }

    @test
    def dropWhile06(): Bool = region r {
        let a = Array.dropWhile(r, i -> i > 3, Array#{5,1} @ r);
        Array.sameElements(a, Array#{1} @ r)
    }

    @test
    def dropWhile07(): Bool = region r {
        let a = Array.dropWhile(r, i -> i > 3, Array#{5,8} @ r);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def dropWhile08(): Bool = region r {
        let a = Array.dropWhile(r, i -> i > 3, Array#{4, 6, -3, 11, -5, 1, 2, 16, 7, 1, 7} @ r);
        Array.sameElements(a, Array#{-3, 11, -5, 1, 2, 16, 7, 1, 7} @ r)
    }

    /////////////////////////////////////////////////////////////////////////////
    // dropWhileLeft                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def dropWhileLeft01(): Bool = region r {
        let a = Array.dropWhileLeft(r, i -> i > 3, Array#{} @ r);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def dropWhileLeft02(): Bool = region r {
        let a = Array.dropWhileLeft(r, i -> i > 3, Array#{1} @ r);
        Array.sameElements(a, Array#{1} @ r)
    }

    @test
    def dropWhileLeft03(): Bool = region r {
        let a = Array.dropWhileLeft(r, i -> i > 3, Array#{4} @ r);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def dropWhileLeft04(): Bool = region r {
        let a = Array.dropWhileLeft(r, i -> i > 3, Array#{1,2} @ r);
        Array.sameElements(a, Array#{1,2} @ r)
    }

    @test
    def dropWhileLeft05(): Bool = region r {
        let a = Array.dropWhileLeft(r, i -> i > 3, Array#{1,5} @ r);
        Array.sameElements(a, Array#{1,5} @ r)
    }

    @test
    def dropWhileLeft06(): Bool = region r {
        let a = Array.dropWhileLeft(r, i -> i > 3, Array#{5,1} @ r);
        Array.sameElements(a, Array#{1} @ r)
    }

    @test
    def dropWhileLeft07(): Bool = region r {
        let a = Array.dropWhileLeft(r, i -> i > 3, Array#{5,8} @ r);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def dropWhileLeft08(): Bool = region r {
        let a = Array.dropWhileLeft(r, i -> i > 3, Array#{4, 6, -3, 11, -5, 1, 2, 16, 7, 1, 7} @ r);
        Array.sameElements(a, Array#{-3, 11, -5, 1, 2, 16, 7, 1, 7} @ r)
    }

    /////////////////////////////////////////////////////////////////////////////
    // dropWhileRight                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def dropWhileRight01(): Bool = region r {
        let a = Array.dropWhileRight(r, i -> i > 3, Array#{} @ r);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def dropWhileRight02(): Bool = region r {
        let a = Array.dropWhileRight(r, i -> i > 3, Array#{1} @ r);
        Array.sameElements(a, Array#{1} @ r)
    }

    @test
    def dropWhileRight03(): Bool = region r {
        let a = Array.dropWhileRight(r, i -> i > 3, Array#{4} @ r);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def dropWhileRight04(): Bool = region r {
        let a = Array.dropWhileRight(r, i -> i > 3, Array#{1,2} @ r);
        Array.sameElements(a, Array#{1,2} @ r)
    }

    @test
    def dropWhileRight05(): Bool = region r {
        let a = Array.dropWhileRight(r, i -> i > 3, Array#{1,5} @ r);
        Array.sameElements(a, Array#{1} @ r)
    }

    @test
    def dropWhileRight06(): Bool = region r {
        let a = Array.dropWhileRight(r, i -> i > 3, Array#{5,1} @ r);
        Array.sameElements(a, Array#{5,1} @ r)
    }

    @test
    def dropWhileRight07(): Bool = region r {
        let a = Array.dropWhileRight(r, i -> i > 3, Array#{5,8} @ r);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def dropWhileRight08(): Bool = region r {
        let a = Array.dropWhileRight(r, i -> i > 3, Array#{4, 6, -3, 11, -5, 1, 2, 16, 7, 1, 7} @ r);
        Array.sameElements(a, Array#{4, 6, -3, 11, -5, 1, 2, 16, 7, 1} @ r)
    }

    /////////////////////////////////////////////////////////////////////////////
    // take                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def take01(): Bool = region r {
        let a = Array.take(r, -1, (Array#{} @ r): Array[Int32, _]);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def take02(): Bool = region r {
        let a = Array.take(r, 0, (Array#{} @ r): Array[Int32, _]);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def take03(): Bool = region r {
        let a = Array.take(r, 1, (Array#{} @ r): Array[Int32, _]);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def take04(): Bool = region r {
        let a = Array.take(r, -1, Array#{1} @ r);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def take05(): Bool = region r {
        let a = Array.take(r, 0, Array#{1} @ r);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])

    }
    @test
    def take06(): Bool = region r {
        let a = Array.take(r, 1, Array#{1} @ r);
        Array.sameElements(a, Array#{1} @ r)

    }
    @test
    def take07(): Bool = region r {
        let a = Array.take(r, 2, Array#{1} @ r);
        Array.sameElements(a, Array#{1} @ r)

    }
    @test
    def take08(): Bool = region r {
        let a = Array.take(r, 0, Array#{1,2} @ r);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def take09(): Bool = region r {
        let a = Array.take(r, 1, Array#{1,2} @ r);
        Array.sameElements(a, Array#{1} @ r)
    }

    @test
    def take10(): Bool = region r {
        let a = Array.take(r, 2, Array#{1,2} @ r);
        Array.sameElements(a, Array#{1,2} @ r)
    }

    @test
    def take11(): Bool = region r {
        let a = Array.take(r, 2, Array#{1,2,3,4,5,6} @ r);
        Array.sameElements(a, Array#{1,2} @ r)
    }

    @test
    def take12(): Bool = region r {
        let a = Array.take(r, 4, Array#{1,2,3,4,5,6} @ r);
        Array.sameElements(a, Array#{1,2,3,4} @ r)
    }

    /////////////////////////////////////////////////////////////////////////////
    // takeLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def takeLeft01(): Bool = region r {
        let a = Array.takeLeft(r, -1, (Array#{} @ r): Array[Int32, _]);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def takeLeft02(): Bool = region r {
        let a = Array.takeLeft(r, 0, (Array#{} @ r): Array[Int32, _]);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def takeLeft03(): Bool = region r {
        let a = Array.takeLeft(r, 1, (Array#{} @ r): Array[Int32, _]);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def takeLeft04(): Bool = region r {
        let a = Array.takeLeft(r, -1, Array#{1} @ r);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def takeLeft05(): Bool = region r {
        let a = Array.takeLeft(r, 0, Array#{1} @ r);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def takeLeft06(): Bool = region r {
        let a = Array.takeLeft(r, 1, Array#{1} @ r);
        Array.sameElements(a, Array#{1} @ r)
    }

    @test
    def takeLeft07(): Bool = region r {
        let a = Array.takeLeft(r, 2, Array#{1} @ r);
        Array.sameElements(a, Array#{1} @ r)
    }

    @test
    def takeLeft08(): Bool = region r {
        let a = Array.takeLeft(r, 0, Array#{1,2} @ r);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def takeLeft09(): Bool = region r {
        let a = Array.takeLeft(r, 1, Array#{1,2} @ r);
        Array.sameElements(a, Array#{1} @ r)
    }

    @test
    def takeLeft10(): Bool = region r {
        let a = Array.takeLeft(r, 2, Array#{1,2} @ r);
        Array.sameElements(a, Array#{1,2} @ r)
    }

    @test
    def takeLeft11(): Bool = region r {
        let a = Array.takeLeft(r, 2, Array#{1,2,3,4,5,6} @ r);
        Array.sameElements(a, Array#{1,2} @ r)
    }

    @test
    def takeLeft12(): Bool = region r {
        let a = Array.takeLeft(r, 4, Array#{1,2,3,4,5,6} @ r);
        Array.sameElements(a, Array#{1,2,3,4} @ r)
    }

    /////////////////////////////////////////////////////////////////////////////
    // takeRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def takeRight01(): Bool = region r {
        let a = Array.takeRight(r, -1, (Array#{} @ r): Array[Int32, _]);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def takeRight02(): Bool = region r {
        let a = Array.takeRight(r, 0, (Array#{} @ r): Array[Int32, _]);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def takeRight03(): Bool = region r {
        let a = Array.takeRight(r, 1, (Array#{} @ r): Array[Int32, _]);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def takeRight04(): Bool = region r {
        let a = Array.takeRight(r, -1, Array#{1} @ r);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def takeRight05(): Bool = region r {
        let a = Array.takeRight(r, 0, Array#{1} @ r);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def takeRight06(): Bool = region r {
        let a = Array.takeRight(r, 1, Array#{1} @ r);
        Array.sameElements(a, Array#{1} @ r)
    }

    @test
    def takeRight07(): Bool = region r {
        let a = Array.takeRight(r, 2, Array#{1} @ r);
        Array.sameElements(a, Array#{1} @ r)
    }

    @test
    def takeRight08(): Bool = region r {
        let a = Array.takeRight(r, 0, Array#{1,2} @ r);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def takeRight09(): Bool = region r {
        let a = Array.takeRight(r, 1, Array#{1,2} @ r);
        Array.sameElements(a, Array#{2} @ r)
    }

    @test
    def takeRight10(): Bool = region r {
        let a = Array.takeRight(r, 2, Array#{1,2} @ r);
        Array.sameElements(a, Array#{1,2} @ r)
    }

    @test
    def takeRight11(): Bool = region r {
        let a = Array.takeRight(r, 2, Array#{1,2,3,4,5,6} @ r);
        Array.sameElements(a, Array#{5,6} @ r)
    }

    @test
    def takeRight12(): Bool = region r {
        let a = Array.takeRight(r, 4, Array#{1,2,3,4,5,6} @ r);
        Array.sameElements(a, Array#{3,4,5,6} @ r)
    }

    /////////////////////////////////////////////////////////////////////////////
    // takeWhile                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def takeWhile01(): Bool = region r {
        let a = Array.takeWhile(r, i -> i > 3, (Array#{} @ r): Array[Int32, _]);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def takeWhile02(): Bool = region r {
        let a = Array.takeWhile(r, i -> i > 3, Array#{1} @ r);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def takeWhile03(): Bool = region r {
        let a = Array.takeWhile(r, i -> i > 3, Array#{4} @ r);
        Array.sameElements(a, Array#{4} @ r)
    }

    @test
    def takeWhile04(): Bool = region r {
        let a = Array.takeWhile(r, i -> i > 3, Array#{1,2} @ r);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def takeWhile05(): Bool = region r {
        let a = Array.takeWhile(r, i -> i > 3, Array#{1,5} @ r);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def takeWhile06(): Bool = region r {
        let a = Array.takeWhile(r, i -> i > 3, Array#{5,1} @ r);
        Array.sameElements(a, Array#{5} @ r)
    }

    @test
    def takeWhile07(): Bool = region r {
        let a = Array.takeWhile(r, i -> i > 3, Array#{5,8} @ r);
        Array.sameElements(a, Array#{5,8} @ r)
    }

    @test
    def takeWhile08(): Bool = region r {
        let a = Array.takeWhile(r, i -> i > 3, Array#{4, 6, -3, 11, -5, 1, 2, 16, 7, 1, 7} @ r);
        Array.sameElements(a, Array#{4, 6} @ r)
    }

    /////////////////////////////////////////////////////////////////////////////
    // takeWhileLeft                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def takeWhileLeft01(): Bool = region r {
        let a = Array.takeWhileLeft(r, i -> i > 3, (Array#{} @ r): Array[Int32, _]);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def takeWhileLeft02(): Bool = region r {
        let a = Array.takeWhileLeft(r, i -> i > 3, Array#{1} @ r);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def takeWhileLeft03(): Bool = region r {
        let a = Array.takeWhileLeft(r, i -> i > 3, Array#{4} @ r);
        Array.sameElements(a, Array#{4} @ r)
    }

    @test
    def takeWhileLeft04(): Bool = region r {
        let a = Array.takeWhileLeft(r, i -> i > 3, Array#{1,2} @ r);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def takeWhileLeft05(): Bool = region r {
        let a = Array.takeWhileLeft(r, i -> i > 3, Array#{1,5} @ r);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def takeWhileLeft06(): Bool = region r {
        let a = Array.takeWhileLeft(r, i -> i > 3, Array#{5,1} @ r);
        Array.sameElements(a, Array#{5} @ r)
    }

    @test
    def takeWhileLeft07(): Bool = region r {
        let a = Array.takeWhileLeft(r, i -> i > 3, Array#{5,8} @ r);
        Array.sameElements(a, Array#{5,8} @ r)
    }

    @test
    def takeWhileLeft08(): Bool = region r {
        let a = Array.takeWhileLeft(r, i -> i > 3, Array#{4, 6, -3, 11, -5, 1, 2, 16, 7, 1, 7} @ r);
        Array.sameElements(a, Array#{4, 6} @ r)
    }

    /////////////////////////////////////////////////////////////////////////////
    // takeWhileRight                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def takeWhileRight01(): Bool = region r {
        let a = Array.takeWhileRight(r, i -> i > 3, (Array#{} @ r): Array[Int32, _]);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def takeWhileRight02(): Bool = region r {
        let a = Array.takeWhileRight(r, i -> i > 3, Array#{1} @ r);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def takeWhileRight03(): Bool = region r {
        let a = Array.takeWhileRight(r, i -> i > 3, Array#{4} @ r);
        Array.sameElements(a, Array#{4} @ r)
    }

    @test
    def takeWhileRight04(): Bool = region r {
        let a = Array.takeWhileRight(r, i -> i > 3, Array#{1,2} @ r);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def takeWhileRight05(): Bool = region r {
        let a = Array.takeWhileRight(r, i -> i > 3, Array#{1,5} @ r);
        Array.sameElements(a, Array#{5} @ r)
    }

    @test
    def takeWhileRight06(): Bool = region r {
        let a = Array.takeWhileRight(r, i -> i > 3, Array#{5,1} @ r);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def takeWhileRight07(): Bool = region r {
        let a = Array.takeWhileRight(r, i -> i > 3, Array#{5,8} @ r);
        Array.sameElements(a, Array#{5,8} @ r)
    }

    @test
    def takeWhileRight08(): Bool = region r {
        let a = Array.takeWhileRight(r, i -> i > 3, Array#{4, 6, -3, 11, -5, 1, 2, 16, 7, 1, 7} @ r);
        Array.sameElements(a, Array#{7} @ r)
    }

    /////////////////////////////////////////////////////////////////////////////
    // groupBy                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def groupBy01(): Bool = region r {
        let arr = Array.groupBy(r, (a, b) -> a > 3 or b > 8, (Array#{} @ r): Array[Int32, r]);
        let xs = Array.toList(arr) |> List.map(Array.toList);
        xs == Nil
    }

    @test
    def groupBy02(): Bool = region r {
        let arr = Array.groupBy(r, (a, b) -> a > 3 or b > 8, Array#{1} @ r);
        let xs = Array.toList(arr) |> List.map(Array.toList);
        xs == (1 :: Nil) :: Nil
    }

    @test
    def groupBy03(): Bool = region r {
        let arr = Array.groupBy(r, (a, b) -> a > 3 or b > 8, Array#{1,4} @ r);
        let xs = Array.toList(arr) |> List.map(Array.toList);
        xs == (1 :: Nil) :: (4 :: Nil) :: Nil
    }

    @test
    def groupBy04(): Bool = region r {
        let arr = Array.groupBy(r, (a, b) -> a > 3 or b > 8, Array#{1,9} @ r);
        let xs = Array.toList(arr) |> List.map(Array.toList);
        xs == (1 :: 9 :: Nil) :: Nil
    }

    @test
    def groupBy05(): Bool = region r {
        let arr = Array.groupBy(r, (a, b) -> a > 3 or b > 8, Array#{1, 4, 7, 6, 9, 2, 4, 4, 8, 16} @ r);
        let xs = Array.toList(arr) |> List.map(Array.toList);
        xs == (1 :: 9 :: 16 :: Nil) :: (4 :: 7 :: 6 :: 4 :: 4 :: 8 :: Nil) :: (2 :: Nil) :: Nil
    }

    @test
    def groupBy06(): Bool = region r {
        let arr = Array.groupBy(r, (a, b) -> a > -6 or a*b >= 0, Array#{-1, -11, 4, -11, 0, 8, 2, 1, -3, -24} @ r);
        let xs = Array.toList(arr) |> List.map(Array.toList);
        xs == (-1 :: -11 :: -11 :: 0 :: -3 :: -24 :: Nil) :: (4 :: 8 :: 2 :: 1 :: Nil) :: Nil
    }

    @test
    def groupBy07(): Bool = region r {
        let arr = Array.groupBy(r, (a, b) -> a < 0 or (a > 10 or (b > 10 or a == b)), Array#{-5, 6, 11, 8, 8, -11, -1, 0, 4, -1} @ r);
        let xs = Array.toList(arr) |> List.map(Array.toList);
        xs == (-5 :: 11 :: -11 :: -1 :: -1 :: Nil) :: (6 :: Nil) :: (8 :: 8 :: Nil) :: (0 :: Nil) :: (4 :: Nil) :: Nil
    }

    /////////////////////////////////////////////////////////////////////////////
    // zip                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def zip01(): Bool = region r {
        let a = Array.zip(r, (Array#{} @ r): Array[Int32, _], (Array#{} @ r): Array[Int32, _]);
        Array.sameElements(a, (Array#{} @ r): Array[(Int32, Int32), _])
    }

    @test
    def zip02(): Bool = region r {
        let a = Array.zip(r, Array#{1} @ r, (Array#{} @ r): Array[Int32, _]);
        Array.sameElements(a, (Array#{} @ r): Array[(Int32, Int32), _])
    }

    @test
    def zip03(): Bool = region r {
        let a = Array.zip(r, (Array#{} @ r): Array[Int32, _], Array#{2} @ r);
        Array.sameElements(a, (Array#{} @ r): Array[(Int32, Int32), _])
    }

    @test
    def zip04(): Bool = region r {
        let a = Array.zip(r, Array#{1} @ r, Array#{2} @ r);
        Array.sameElements(a, Array#{(1, 2)} @ r)
    }

    @test
    def zip05(): Bool = region r {
        let a = Array.zip(r, Array#{1,3} @ r, Array#{2,4} @ r);
        Array.sameElements(a, Array#{(1, 2), (3, 4)} @ r)
    }

    @test
    def zip06(): Bool = region r {
        let a = Array.zip(r, Array#{1,3,5} @ r, Array#{2,4,6} @ r);
        Array.sameElements(a, Array#{(1, 2), (3, 4), (5, 6)} @ r)
    }

    @test
    def zip07(): Bool = region r {
        let a = Array.zip(r, Array#{1,3,5,7} @ r, Array#{2,4,6,8} @ r);
        Array.sameElements(a, Array#{(1, 2), (3, 4), (5, 6), (7, 8)} @ r)
    }

    /////////////////////////////////////////////////////////////////////////////
    // zipWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def zipWith01(): Bool = region r {
        let arr = Array.zipWith(r, (a, b) -> if (b) a + 1 else a, (Array#{} @ r): Array[Int32, _], (Array#{} @ r): Array[Bool, _]);
        Array.sameElements(arr, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def zipWith02(): Bool = region r {
        let arr = Array.zipWith(r, (a, b) -> if (b) a + 1 else a, Array#{1} @ r, (Array#{} @ r): Array[Bool, _]);
        Array.sameElements(arr, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def zipWith03(): Bool = region r {
        let arr = Array.zipWith(r, (a, b) -> if (b) a + 1 else a, (Array#{} @ r): Array[Int32, _], Array#{true} @ r);
        Array.sameElements(arr, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def zipWith04(): Bool = region r {
        let arr = Array.zipWith(r, (a, b) -> if (b) a + 1 else a, Array#{1} @ r, Array#{true} @ r);
        Array.sameElements(arr, Array#{2} @ r)
    }

    @test
    def zipWith05(): Bool = region r {
        let arr = Array.zipWith(r, (a, b) -> if (b) a + 1 else a, Array#{1} @ r, Array#{false} @ r);
        Array.sameElements(arr, Array#{1} @ r)
    }

    @test
    def zipWith06(): Bool = region r {
        let arr = Array.zipWith(r, (a, b) -> if (b) a + 1 else a,
                        Array#{1, 2, 3, 4, 5, 6, 7, 8} @ r,
                        Array#{false, true, true, false, false, true, true, true} @ r);
        Array.sameElements(arr, Array#{1, 3, 4, 4, 5, 7, 8, 9} @ r)
    }

    /////////////////////////////////////////////////////////////////////////////
    // unzip                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def unzip01(): Bool = region r {
        let (a,b) = Array.unzip(r, r, (Array#{} @ r): Array[(Unit, Unit), _]);
        Array.sameElements(a, Array#{} @ r) and Array.sameElements(b, Array#{} @ r)
    }

    @test
    def unzip02(): Bool = region r {
        let (a,b) = Array.unzip(r, r, Array#{(1, true)} @ r);
        Array.sameElements(a, Array#{1} @ r) and Array.sameElements(b, Array#{true} @ r)
    }

    @test
    def unzip03(): Bool = region r {
        let (a,b) = Array.unzip(r, r, Array#{(1, true), (2, true)} @ r);
        Array.sameElements(a, Array#{1,2} @ r)  and Array.sameElements(b, Array#{true,true} @ r)
    }

    @test
    def unzip04(): Bool = region r {
        let (a,b) = Array.unzip(r, r, Array#{(1, true), (2, true), (3, false)} @ r);
        Array.sameElements(a, Array#{1,2,3} @ r) and Array.sameElements(b, Array#{true,true,false} @ r)
    }

    @test
    def unzip05(): Bool = region r1 { region r2 {
        unzip05Aux(r1, Array#{(1, "1"), (2, "2"), (3, "3")} @ r2) `Array.sameElements` (Array#{1, 2, 3} @ r2)
    }}

    def unzip05Aux(r1: Region[r1], a: Array[(a, b), r2]): Array[a, r1] \ { r1, r2 } =
        region r3 {
            // snd is local, fst is for caller
            Array.unzip(r1, r3, a) |> fst
        }

    /////////////////////////////////////////////////////////////////////////////
    // fold2                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def fold201(): Bool = region r {
        Array.fold2((c, a, b) -> if (b) a + c else a * c, 4, (Array#{} @ r): Array[Int32, _], (Array#{} @ r): Array[Bool, _]) == 4
    }

    @test
    def fold202(): Bool = region r {
        Array.fold2((c, a, b) -> if (b) a + c else a * c, 4, Array#{1} @ r, Array#{} @ r) == 4
    }

    @test
    def fold203(): Bool = region r {
        Array.fold2((c, a, b) -> if (b) a + c else a * c, 4, Array#{} @ r, Array#{true} @ r) == 4
    }

    @test
    def fold204(): Bool = region r {
        Array.fold2((c, a, b) -> if (b) a + c else a * c, 4, Array#{2} @ r, Array#{true} @ r) == 6
    }

    @test
    def fold205(): Bool = region r {
        Array.fold2((c, a, b) -> if (b) a + c else a * c, 4, Array#{2} @ r, Array#{false} @ r) == 8
    }

    @test
    def fold206(): Bool = region r {
        Array.fold2((c, a, b) -> if (b) a + c else a * c, 4, Array#{3,2} @ r, Array#{true,true} @ r) == 9
    }

    @test
    def fold207(): Bool = region r {
        Array.fold2((c, a, b) -> if (b) a + c else a * c, 4, Array#{3,2} @ r, Array#{true,false} @ r) == 14
    }

    @test
    def fold208(): Bool = region r {
        Array.fold2((c, a, b) -> if (b) a + c else a * c, 4, Array#{3,2} @ r, Array#{false,true} @ r) == 14
    }

    @test
    def fold209(): Bool = region r {
        Array.fold2((c, a, b) -> if (b) a + c else a * c, 4, Array#{3,2} @ r, Array#{false,false} @ r) == 24
    }

    @test
    def fold210(): Bool = region r {
        Array.fold2((c, a, b) -> if (b) a + c else a * c, 4, Array#{6,-4,3,2} @ r,
            Array#{true,false,false,true} @ r) == -118
    }

    /////////////////////////////////////////////////////////////////////////////
    // foldLeft2                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldLeft201(): Bool = region r {
        Array.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, (Array#{} @ r): Array[Int32, _], (Array#{} @ r): Array[Bool, _]) == 4
    }

    @test
    def foldLeft202(): Bool = region r {
        Array.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, Array#{1} @ r, (Array#{} @ r): Array[Bool, _]) == 4
    }

    @test
    def foldLeft203(): Bool = region r {
        Array.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, (Array#{} @ r): Array[Int32, _], Array#{true} @ r) == 4
    }

    @test
    def foldLeft204(): Bool = region r {
        Array.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, Array#{2} @ r, Array#{true} @ r) == 6
    }

    @test
    def foldLeft205(): Bool = region r {
        Array.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, Array#{2} @ r, Array#{false} @ r) == 8
    }

    @test
    def foldLeft206(): Bool = region r {
        Array.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, Array#{3,2} @ r, Array#{true,true} @ r) == 9
    }

    @test
    def foldLeft207(): Bool = region r {
        Array.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, Array#{3,2} @ r, Array#{true,false} @ r) == 14
    }

    @test
    def foldLeft208(): Bool = region r {
        Array.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, Array#{3,2} @ r, Array#{false,true} @ r) == 14
    }

    @test
    def foldLeft209(): Bool = region r {
        Array.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, Array#{3,2} @ r, Array#{false,false} @ r) == 24
    }

    @test
    def foldLeft210(): Bool = region r {
        Array.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, Array#{6,-4,3,2} @ r,
            Array#{true,false,false,true} @ r) == -118
    }

    /////////////////////////////////////////////////////////////////////////////
    // foldRight2                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldRight201(): Bool = region r {
        Array.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, (Array#{} @ r): Array[Int32, _], (Array#{} @ r): Array[Bool, _]) == 4
    }

    @test
    def foldRight202(): Bool = region r {
        Array.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, Array#{1} @ r, (Array#{} @ r): Array[Bool, _]) == 4
    }

    @test
    def foldRight203(): Bool = region r {
        Array.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, (Array#{} @ r): Array[Int32, _], Array#{true} @ r) == 4
    }

    @test
    def foldRight204(): Bool = region r {
        Array.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, Array#{2} @ r, Array#{true} @ r) == 6
    }

    @test
    def foldRight205(): Bool = region r {
        Array.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, Array#{2} @ r, Array#{false} @ r) == 8
    }

    @test
    def foldRight206(): Bool = region r {
        Array.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, Array#{3,2} @ r, Array#{true,true} @ r) == 9
    }

    @test
    def foldRight207(): Bool = region r {
        Array.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, Array#{3,2} @ r, Array#{true,false} @ r) == 11
    }

    @test
    def foldRight208(): Bool = region r {
        Array.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, Array#{3,2} @ r, Array#{false,true} @ r) == 18
    }

    @test
    def foldRight209(): Bool = region r {
        Array.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, Array#{3,2} @ r, Array#{false,false} @ r) == 24
    }

    @test
    def foldRight210(): Bool = region r {
        Array.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, Array#{6,-4,3,2} @ r,
            Array#{true,false,false,true} @ r) == -66
    }

    /////////////////////////////////////////////////////////////////////////////
    // filterMap                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filterMap01(): Bool = region r {
        let a = Array.filterMap(r, i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, (Array#{} @ r): Array[Int32, _]);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def filterMap02(): Bool = region r {
        let a = Array.filterMap(r, i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Array#{1} @ r);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def filterMap03(): Bool = region r {
        let a = Array.filterMap(r, i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Array#{2} @ r);
        Array.sameElements(a, Array#{1} @ r)
    }

    @test
    def filterMap04(): Bool = region r {
        let a = Array.filterMap(r, i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Array#{1,3} @ r);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def filterMap05(): Bool = region r {
        let a = Array.filterMap(r, i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Array#{1,4} @ r);
        Array.sameElements(a, Array#{2} @ r)
    }

    @test
    def filterMap06(): Bool = region r {
        let a = Array.filterMap(r, i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Array#{6,-1} @ r);
        Array.sameElements(a, Array#{3} @ r)
    }

    @test
    def filterMap07(): Bool = region r {
        let a = Array.filterMap(r, i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Array#{8,6} @ r);
        Array.sameElements(a, Array#{4,3} @ r)
    }

    @test
    def filterMap08(): Bool = region r {
        let a = Array.filterMap(r, i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Array#{0,1,2,3,4,5,10} @ r);
        Array.sameElements(a, Array#{0,1,2,5} @ r)
    }

    /////////////////////////////////////////////////////////////////////////////
    // findMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def findMap01(): Bool = region r {
        Array.findMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, (Array#{} @ r): Array[Int32, _]) == None
    }

    @test
    def findMap02(): Bool = region r {
        Array.findMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Array#{1} @ r) == None
    }

    @test
    def findMap03(): Bool = region r {
        Array.findMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Array#{2} @ r) == Some(1)
    }

    @test
    def findMap04(): Bool = region r {
        Array.findMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Array#{1,3} @ r) == None
    }

    @test
    def findMap05(): Bool = region r {
        Array.findMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Array#{1,4} @ r) == Some(2)
    }

    @test
    def findMap06(): Bool = region r {
        Array.findMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Array#{6,-1} @ r) == Some(3)
    }

    @test
    def findMap07(): Bool = region r {
        Array.findMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Array#{8,6} @ r) == Some(4)
    }

    @test
    def findMap08(): Bool = region r {
        Array.findMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Array#{0,1,2,3,4,5,10} @ r) == Some(0)
    }

    /////////////////////////////////////////////////////////////////////////////
    // toSet                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toSet01(): Bool = region r {
        Array.toSet((Array#{} @ r): Array[Int32, _]) == Set#{}
    }

    @test
    def toSet02(): Bool = region r {
        Array.toSet(Array#{1} @ r) == Set#{1}
    }

    @test
    def toSet03(): Bool = region r {
        Array.toSet(Array#{1,2} @ r) == Set#{1, 2}
    }

    @test
    def toSet04(): Bool = region r {
        Array.toSet(Array#{1,1} @ r) == Set#{1}
    }

    @test
    def toSet05(): Bool = region r {
        Array.toSet(Array#{1,2,3} @ r) == Set#{1, 2, 3}
    }

    @test
    def toSet06(): Bool = region r {
        Array.toSet(Array#{1,2,1} @ r) == Set#{1, 2}
    }

    @test
    def toSet07(): Bool = region r {
        Array.toSet(Array#{1,1,2} @ r) == Set#{1, 2}
    }

    @test
    def toSet08(): Bool = region r {
        Array.toSet(Array#{2,1,2,3,4,5,6,3,3} @ r) == Set#{1, 2, 3, 4, 5, 6}
    }

    /////////////////////////////////////////////////////////////////////////////
    // toMap                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toMap01(): Bool = region r {
        Array.toMap((Array#{} @ r): Array[(Int32, Bool), _]) == Map#{}
    }

    @test
    def toMap02(): Bool = region r {
        Array.toMap(Array#{(1, true)} @ r) == Map#{1 => true}
    }

    @test
    def toMap03(): Bool = region r {
        Array.toMap(Array#{(1, true), (2, false)} @ r) == Map#{1 => true, 2 => false}
    }

    @test
    def toMap04(): Bool = region r {
        Array.toMap(Array#{(1, true), (1, false)} @ r) == Map#{1 => true}
    }

    /////////////////////////////////////////////////////////////////////////////
    // findIndexOf                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def findIndexOf01(): Bool = region r {
        Array.findIndexOf(i -> i > 2, (Array#{} @ r): Array[Int32, _]) == None
    }

    @test
    def findIndexOf02(): Bool = region r {
        Array.findIndexOf(i -> i > 2, Array#{1} @ r) == None
    }

    @test
    def findIndexOf03(): Bool = region r {
        Array.findIndexOf(i -> i > 2, Array#{3} @ r) == Some(0)
    }

    @test
    def findIndexOf04(): Bool = region r {
        Array.findIndexOf(i -> i > 2, Array#{1,2} @ r) == None
    }

    @test
    def findIndexOf05(): Bool = region r {
        Array.findIndexOf(i -> i > 2, Array#{6,-6} @ r) == Some(0)
    }

    @test
    def findIndexOf06(): Bool = region r {
        Array.findIndexOf(i -> i > 2, Array#{-6,6} @ r) == Some(1)
    }

    @test
    def findIndexOf07(): Bool = region r {
        Array.findIndexOf(i -> i > 2, Array#{6,7} @ r) == Some(0)
    }

    /////////////////////////////////////////////////////////////////////////////
    // findIndexOfLeft                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def findIndexOfLeft01(): Bool = region r {
        Array.findIndexOfLeft(i -> i > 2, (Array#{} @ r): Array[Int32, _]) == None
    }

    @test
    def findIndexOfLeft02(): Bool = region r {
        Array.findIndexOfLeft(i -> i > 2, Array#{1} @ r) == None
    }

    @test
    def findIndexOfLeft03(): Bool = region r {
        Array.findIndexOfLeft(i -> i > 2, Array#{3} @ r) == Some(0)
    }

    @test
    def findIndexOfLeft04(): Bool = region r {
        Array.findIndexOfLeft(i -> i > 2, Array#{1,2} @ r) == None
    }

    @test
    def findIndexOfLeft05(): Bool = region r {
        Array.findIndexOfLeft(i -> i > 2, Array#{6,-6} @ r) == Some(0)
    }

    @test
    def findIndexOfLeft06(): Bool = region r {
        Array.findIndexOfLeft(i -> i > 2, Array#{-6,6} @ r) == Some(1)
    }

    @test
    def findIndexOfLeft07(): Bool = region r {
        Array.findIndexOfLeft(i -> i > 2, Array#{6,7} @ r) == Some(0)
    }

    /////////////////////////////////////////////////////////////////////////////
    // findIndexOfRight                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def findIndexOfRight01(): Bool = region r {
        Array.findIndexOfRight(i -> i > 2, (Array#{} @ r): Array[Int32, _]) == None
    }

    @test
    def findIndexOfRight02(): Bool = region r {
        Array.findIndexOfRight(i -> i > 2, Array#{1} @ r) == None
    }

    @test
    def findIndexOfRight03(): Bool = region r {
        Array.findIndexOfRight(i -> i > 2, Array#{3} @ r) == Some(0)
    }

    @test
    def findIndexOfRight04(): Bool = region r {
        Array.findIndexOfRight(i -> i > 2, Array#{1,2} @ r) == None
    }

    @test
    def findIndexOfRight05(): Bool = region r {
        Array.findIndexOfRight(i -> i > 2, Array#{6,-6} @ r) == Some(0)
    }

    @test
    def findIndexOfRight06(): Bool = region r {
        Array.findIndexOfRight(i -> i > 2, Array#{-6,6} @ r) == Some(1)
    }

    @test
    def findIndexOfRight07(): Bool = region r {
        Array.findIndexOfRight(i -> i > 2, Array#{6,7} @ r) == Some(1)
    }

    /////////////////////////////////////////////////////////////////////////////
    // findIndices                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def findIndices01(): Bool = region r {
        let a = Array.findIndices(r, i -> i > 2, (Array#{} @ r): Array[Int32, r]);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, r])
    }

    @test
    def findIndices02(): Bool = region r {
        let a = Array.findIndices(r, i -> i > 2, Array#{1} @ r);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, r])
    }

    @test
    def findIndices03(): Bool = region r {
        let a = Array.findIndices(r, i -> i > 2, Array#{3} @ r);
        Array.sameElements(a, Array#{0} @ r)
    }

    @test
    def findIndices04(): Bool = region r {
        let a = Array.findIndices(r, i -> i > 2, Array#{1,2} @ r);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, r])
    }

    @test
    def findIndices05(): Bool = region r {
        let a = Array.findIndices(r, i -> i > 2, Array#{6,-6} @ r);
        Array.sameElements(a, Array#{0} @ r)
    }

    @test
    def findIndices06(): Bool = region r {
        let a = Array.findIndices(r, i -> i > 2, Array#{-6,6} @ r);
        Array.sameElements(a, Array#{1} @ r)
    }

    @test
    def findIndices07(): Bool = region r {
        let a = Array.findIndices(r, i -> i > 2, Array#{6,7} @ r);
        Array.sameElements(a, Array#{0,1} @ r)
    }

    /////////////////////////////////////////////////////////////////////////////
    // init                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def init01(): Bool = region r {
        let a = Array.init(r, x -> x, 0);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def init02(): Bool = region r {
        let a = Array.init(r, x -> x, -1);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def init03(): Bool = region r {
        let a = Array.init(r, x -> x, 1);
        Array.sameElements(a, Array#{0} @ r)
    }

    @test
    def init04(): Bool = region r {
        let a = Array.init(r, x -> x, 2);
        Array.sameElements(a, Array#{0,1} @ r)
    }

    @test
    def init05(): Bool = region r {
        let a = Array.init(r, x -> x, 50);
        Array.length(a) == 50
    }

    /////////////////////////////////////////////////////////////////////////////
    // sameElements                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sameElements01(): Bool = region r {
        let a = (Array#{} @ r): Array[Int32, _];
        let b = (Array#{} @ r): Array[Int32, _];
        Array.sameElements(a,b) == true
    }

    @test
    def sameElements02(): Bool = region r {
        let a = (Array#{1} @ r): Array[Int32, _];
        let b = (Array#{} @ r): Array[Int32, _];
        Array.sameElements(a,b) == false
    }

    @test
    def sameElements03(): Bool = region r {
        let a = (Array#{} @ r): Array[Int32, _];
        let b = (Array#{1} @ r): Array[Int32, _];
        Array.sameElements(a,b) == false
    }

    @test
    def sameElements04(): Bool = region r {
        let a = (Array#{1} @ r): Array[Int32, _];
        let b = (Array#{1} @ r): Array[Int32, _];
        Array.sameElements(a,b) == true
    }

    @test
    def sameElements05(): Bool = region r {
        let a = (Array#{1} @ r): Array[Int32, _];
        let b = (Array#{0} @ r): Array[Int32, _];
        Array.sameElements(a,b) == false
    }

    @test
    def sameElements06(): Bool = region r {
        let a = (Array#{1,2} @ r): Array[Int32, _];
        let b = (Array#{1,2} @ r): Array[Int32, _];
        Array.sameElements(a,b) == true
    }

    @test
    def sameElements07(): Bool = region r {
        let a = (Array#{1,2} @ r): Array[Int32, _];
        let b = (Array#{2,1} @ r): Array[Int32, _];
        Array.sameElements(a,b) == false
    }

    /////////////////////////////////////////////////////////////////////////////
    // forEach                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def forEach01(): Bool = region r {
        let a = (Array#{} @ r): Array[Int32, _];
        let sb = StringBuilder.new(r);
        let fn = x -> if (x > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        Array.forEach(fn, a);
        StringBuilder.toString(sb) == ""
    }

    @test
    def forEach02(): Bool = region r {
        let a = Array#{0} @ r;
        let sb = StringBuilder.new(r);
        let fn = x -> if (x > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        Array.forEach(fn, a);
        StringBuilder.toString(sb) == "F"
    }

    @test
    def forEach03(): Bool = region r {
        let a = Array#{1} @ r;
        let sb = StringBuilder.new(r);
        let fn = x -> if (x > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        Array.forEach(fn, a);
        StringBuilder.toString(sb) == "T"
    }

    @test
    def forEach04(): Bool = region r {
        let a = Array#{0,1} @ r;
        let sb = StringBuilder.new(r);
        let fn = x -> if (x > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        Array.forEach(fn, a);
        StringBuilder.toString(sb) == "FT"
    }

    /////////////////////////////////////////////////////////////////////////////
    // forEachWithIndex                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def forEachWithIndex01(): Bool = region r {
        let a = (Array#{} @ r): Array[Float32, _];
        let sb = StringBuilder.new(r);
        let fn = (ix, _) -> if (ix > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        Array.forEachWithIndex(fn, a);
        StringBuilder.toString(sb) == ""
    }

    @test
    def forEachWithIndex02(): Bool = region r {
        let a = Array#{0.0} @ r;
        let sb = StringBuilder.new(r);
        let fn = (ix, _) -> if (ix > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        Array.forEachWithIndex(fn, a);
        StringBuilder.toString(sb) == "F"
    }

    @test
    def forEachWithIndex03(): Bool = region r {
        let a = Array#{0.0, 0.1} @ r;
        let sb = StringBuilder.new(r);
        let fn = (ix, _) -> if (ix > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        Array.forEachWithIndex(fn, a);
        StringBuilder.toString(sb) == "FT"
    }

    @test
    def forEachWithIndex04(): Bool = region r {
        let a = Array#{0.0, 0.1, 0.2} @ r;
        let sb = StringBuilder.new(r);
        let fn = (ix, _) -> if (ix > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        Array.forEachWithIndex(fn, a);
        StringBuilder.toString(sb) == "FTT"
    }

    @test
    def forEachWithIndex05(): Bool = region r {
        let a = Array#{0.0, 0.1, 0.2, 0.3} @ r;
        let sb = StringBuilder.new(r);
        let fn = (ix, _) -> if (ix > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        Array.forEachWithIndex(fn, a);
        StringBuilder.toString(sb) == "FTTT"
    }

    /////////////////////////////////////////////////////////////////////////////
    // updateSequence                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def updateSequence01(): Bool = region r {
         let a = Array.updateSequence(r, 0, (Array#{} @ r): Array[Int32, _], (Array#{} @ r): Array[Int32, _]);
         Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
     }

    @test
    def updateSequence02(): Bool = region r {
        let a = Array.updateSequence(r, 0, Array#{1,2} @ r, Array#{} @ r);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def updateSequence03(): Bool = region r {
        let a = Array.updateSequence(r, 0, Array#{} @ r, Array#{1,2} @ r);
        Array.sameElements(a, Array#{1,2} @ r)
    }

    @test
    def updateSequence04(): Bool = region r {
        let a = Array.updateSequence(r, -3, Array#{1,2,4} @ r, Array#{1,2} @ r);
        Array.sameElements(a, Array#{1,2} @ r)
    }

    @test
    def updateSequence05(): Bool = region r {
        let a = Array.updateSequence(r, 2, Array#{1,2,4} @ r, Array#{1,2} @ r);
        Array.sameElements(a, Array#{1,2} @ r)
    }

    @test
    def updateSequence06(): Bool = region r {
        let a = Array.updateSequence(r, 0, Array#{} @ r, Array#{1} @ r);
        Array.sameElements(a, Array#{1} @ r)
    }

    @test
    def updateSequence07(): Bool = region r {
        let a = Array.updateSequence(r, 1, Array#{2} @ r, Array#{1} @ r);
        Array.sameElements(a, Array#{1} @ r)
    }

    @test
    def updateSequence08(): Bool = region r {
        let a = Array.updateSequence(r, 0, Array#{2} @ r, Array#{1} @ r);
        Array.sameElements(a, Array#{2} @ r)
    }

    @test
    def updateSequence09(): Bool = region r {
        let a = Array.updateSequence(r, 0, Array#{2,4} @ r, Array#{1} @ r);
        Array.sameElements(a, Array#{2} @ r)
    }

    @test
    def updateSequence10(): Bool = region r {
        let a = Array.updateSequence(r, -1, Array#{2,4} @ r, Array#{1} @ r);
        Array.sameElements(a, Array#{4} @ r)
    }

    @test
    def updateSequence11(): Bool = region r {
        let a = Array.updateSequence(r, -1, Array#{3,4} @ r, Array#{1,2} @ r);
        Array.sameElements(a, Array#{4,2} @ r)
    }

    @test
    def updateSequence12(): Bool = region r {
        let a = Array.updateSequence(r, 1, Array#{3,4} @ r, Array#{1,2} @ r);
        Array.sameElements(a, Array#{1,3} @ r)
    }

    @test
    def updateSequence13(): Bool = region r {
        let a = Array.updateSequence(r, -2, Array#{3,4} @ r, Array#{1,2} @ r);
        Array.sameElements(a, Array#{1,2} @ r)
    }

    @test
    def updateSequence14(): Bool = region r {
        let a = Array.updateSequence(r, 2, Array#{3,4} @ r, Array#{1,2} @ r);
        Array.sameElements(a, Array#{1,2} @ r)
    }

    @test
    def updateSequence15(): Bool = region r {
        let a = Array.updateSequence(r, 1, Array#{3} @ r, Array#{1,2} @ r);
        Array.sameElements(a, Array#{1,3} @ r)
    }

    @test
    def updateSequence16(): Bool = region r {
        let a = Array.updateSequence(r, 0, Array#{3,4} @ r, Array#{1,2} @ r);
        Array.sameElements(a, Array#{3,4} @ r)
    }

    @test
    def updateSequence17(): Bool = region r {
        let a = Array.updateSequence(r, 0, Array#{4} @ r, Array#{1, 2, 3} @ r);
        Array.sameElements(a, Array#{4, 2, 3} @ r)
    }

    @test
    def updateSequence18(): Bool = region r {
        let a = Array.updateSequence(r, 1, Array#{4} @ r, Array#{1, 2, 3} @ r);
        Array.sameElements(a, Array#{1, 4, 3} @ r)
    }

    @test
    def updateSequence19(): Bool = region r {
        let a = Array.updateSequence(r, 2, Array#{4} @ r, Array#{1, 2, 3} @ r);
        Array.sameElements(a, Array#{1, 2, 4} @ r)
    }

    @test
    def updateSequence20(): Bool = region r {
        let a = Array.updateSequence(r, 0, Array#{4, 5} @ r, Array#{1, 2, 3} @ r);
        Array.sameElements(a, Array#{4, 5, 3} @ r)
    }

    @test
    def updateSequence21(): Bool = region r {
        let a = Array.updateSequence(r, 1, Array#{4, 5} @ r, Array#{1, 2, 3} @ r);
        Array.sameElements(a, Array#{1, 4, 5} @ r)
    }

    @test
    def updateSequence22(): Bool = region r {
        let a = Array.updateSequence(r, -1, Array#{4, 5, 6} @ r, Array#{1, 2, 3} @ r);
        Array.sameElements(a, Array#{5, 6, 3} @ r)
    }

    @test
    def updateSequence23(): Bool = region r {
        let a = Array.updateSequence(r, 0, Array#{4, 5, 6} @ r, Array#{1, 2, 3} @ r);
        Array.sameElements(a, Array#{4, 5, 6} @ r)
    }

    @test
    def updateSequence24(): Bool = region r {
        let a = Array.updateSequence(r, 2, Array#{14, 15, 16, 17} @ r, Array#{1, 2, 3, 4, 5, 6, 7} @ r);
        Array.sameElements(a, Array#{1, 2, 14, 15, 16, 17, 7} @ r)
    }

    @test
    def updateSequence25(): Bool = region r {
        let a = Array.updateSequence(r, -2, Array#{14, 15, 16, 17} @ r, Array#{1, 2, 3, 4, 5, 6, 7} @ r);
        Array.sameElements(a, Array#{16, 17, 3, 4, 5, 6, 7} @ r)
    }

    @test
    def updateSequence26(): Bool = region r {
        let a = Array.updateSequence(r, 4, Array#{14, 15, 16, 17} @ r, Array#{1, 2, 3, 4, 5, 6, 7} @ r);
        Array.sameElements(a, Array#{1, 2, 3, 4, 14, 15, 16} @ r)
    }

    @test
    def updateSequence27(): Bool = region r {
        let a = Array.updateSequence(r, 4, Array#{14, 15} @ r, Array#{1, 2, 3, 4, 5, 6, 7} @ r);
        Array.sameElements(a, Array#{1, 2, 3, 4, 14, 15, 7} @ r)
    }

    @test
    def updateSequence28(): Bool = region r {
        let a = Array.updateSequence(r, -1, Array#{-1, -2, -3, -4, -5, -6, -7, -8} @ r, Array#{1, 2, 3, 4, 5, 6, 7} @ r);
        Array.sameElements(a, Array#{-2, -3, -4, -5, -6, -7, -8} @ r)
    }

    /////////////////////////////////////////////////////////////////////////////
    // updateSequence!                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def updateSequence01!(): Bool = region r {
         let a = (Array#{} @ r): Array[Int32, _];
         Array.updateSequence!(0, (Array#{} @ r): Array[Int32, _], a);
         Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
     }

    @test
    def updateSequence02!(): Bool = region r {
        let a = (Array#{} @ r): Array[Int32, _];
        Array.updateSequence!(0, Array#{1,2} @ r, a);
        Array.sameElements(a, (Array#{} @ r): Array[Int32, _])
    }

    @test
    def updateSequence03!(): Bool = region r {
        let a = Array#{1,2} @ r;
        Array.updateSequence!(0, Array#{} @ r, a);
        Array.sameElements(a, Array#{1,2} @ r)
    }

    @test
    def updateSequence04!(): Bool = region r {
        let a = Array#{1,2} @ r;
        Array.updateSequence!(-3, Array#{1,2,4} @ r, a);
        Array.sameElements(a, Array#{1,2} @ r)
    }

    @test
    def updateSequence05!(): Bool = region r {
        let a = Array#{1,2} @ r;
        Array.updateSequence!(2, Array#{1,2,4} @ r, a);
        Array.sameElements(a, Array#{1,2} @ r)
    }

    @test
    def updateSequence06!(): Bool = region r {
        let a = Array#{1} @ r;
        Array.updateSequence!(0, Array#{} @ r, a);
        Array.sameElements(a, Array#{1} @ r)
    }

    @test
    def updateSequence07!(): Bool = region r {
        let a = Array#{1} @ r;
        Array.updateSequence!(1, Array#{2} @ r, a);
        Array.sameElements(a, Array#{1} @ r)
    }

    @test
    def updateSequence08!(): Bool = region r {
        let a = Array#{1} @ r;
        Array.updateSequence!(0, Array#{2} @ r, a);
        Array.sameElements(a, Array#{2} @ r)
    }

    @test
    def updateSequence09!(): Bool = region r {
        let a = Array#{1} @ r;
        Array.updateSequence!(0, Array#{2,4} @ r, a);
        Array.sameElements(a, Array#{2} @ r)
    }

    @test
    def updateSequence10!(): Bool = region r {
        let a = Array#{1} @ r;
        Array.updateSequence!(-1, Array#{2,4} @ r, a);
        Array.sameElements(a, Array#{4} @ r)
    }

    @test
    def updateSequence11!(): Bool = region r {
        let a = Array#{1,2} @ r;
        Array.updateSequence!(-1, Array#{3,4} @ r, a);
        Array.sameElements(a, Array#{4,2} @ r)
    }

    @test
    def updateSequence12!(): Bool = region r {
        let a = Array#{1,2} @ r;
        Array.updateSequence!(1, Array#{3,4} @ r, a);
        Array.sameElements(a, Array#{1,3} @ r)
    }

    @test
    def updateSequence13!(): Bool = region r {
        let a = Array#{1,2} @ r;
        Array.updateSequence!(-2, Array#{3,4} @ r, a);
        Array.sameElements(a, Array#{1,2} @ r)
    }

    @test
    def updateSequence14!(): Bool = region r {
        let a = Array#{1,2} @ r;
        Array.updateSequence!(2, Array#{3,4} @ r, a);
        Array.sameElements(a, Array#{1,2} @ r)
    }

    @test
    def updateSequence15!(): Bool = region r {
        let a = Array#{1,2} @ r;
        Array.updateSequence!(1, Array#{3} @ r, a);
        Array.sameElements(a, Array#{1,3} @ r)
    }

    @test
    def updateSequence16!(): Bool = region r {
        let a = Array#{1,2} @ r;
        Array.updateSequence!(0, Array#{3,4} @ r, a);
        Array.sameElements(a, Array#{3,4} @ r)
    }

    @test
    def updateSequence17!(): Bool = region r {
        let a = Array#{1, 2, 3} @ r;
        Array.updateSequence!(0, Array#{4} @ r, a);
        Array.sameElements(a, Array#{4, 2, 3} @ r)
    }

    @test
    def updateSequence18!(): Bool = region r {
        let a = Array#{1, 2, 3} @ r;
        Array.updateSequence!(1, Array#{4} @ r, a);
        Array.sameElements(a, Array#{1, 4, 3} @ r)
    }

    @test
    def updateSequence19!(): Bool = region r {
        let a = Array#{1, 2, 3} @ r;
        Array.updateSequence!(2, Array#{4} @ r, a);
        Array.sameElements(a, Array#{1, 2, 4} @ r)
    }

    @test
    def updateSequence20!(): Bool = region r {
        let a = Array#{1, 2, 3} @ r;
        Array.updateSequence!(0, Array#{4, 5} @ r, a);
        Array.sameElements(a, Array#{4, 5, 3} @ r)
    }

    @test
    def updateSequence21!(): Bool = region r {
        let a = Array#{1, 2, 3} @ r;
        Array.updateSequence!(1, Array#{4, 5} @ r, a);
        Array.sameElements(a, Array#{1, 4, 5} @ r)
    }

    @test
    def updateSequence22!(): Bool = region r {
        let a = Array#{1, 2, 3} @ r;
        Array.updateSequence!(-1, Array#{4, 5, 6} @ r, a);
        Array.sameElements(a, Array#{5, 6, 3} @ r)
    }

    @test
    def updateSequence23!(): Bool = region r {
        let a = Array#{1, 2, 3} @ r;
        Array.updateSequence!(0, Array#{4, 5, 6} @ r, a);
        Array.sameElements(a, Array#{4, 5, 6} @ r)
    }

    @test
    def updateSequence24!(): Bool = region r {
        let a = Array#{1, 2, 3, 4, 5, 6, 7} @ r;
        Array.updateSequence!(2, Array#{14, 15, 16, 17} @ r, a);
        Array.sameElements(a, Array#{1, 2, 14, 15, 16, 17, 7} @ r)
    }

    @test
    def updateSequence25!(): Bool = region r {
        let a = Array#{1, 2, 3, 4, 5, 6, 7} @ r;
        Array.updateSequence!(-2, Array#{14, 15, 16, 17} @ r, a);
        Array.sameElements(a, Array#{16, 17, 3, 4, 5, 6, 7} @ r)
    }

    @test
    def updateSequence26!(): Bool = region r {
        let a = Array#{1, 2, 3, 4, 5, 6, 7} @ r;
        Array.updateSequence!(4, Array#{14, 15, 16, 17} @ r, a);
        Array.sameElements(a, Array#{1, 2, 3, 4, 14, 15, 16} @ r)
    }

    @test
    def updateSequence27!(): Bool = region r {
        let a = Array#{1, 2, 3, 4, 5, 6, 7} @ r;
        Array.updateSequence!(4, Array#{14, 15} @ r, a);
        Array.sameElements(a, Array#{1, 2, 3, 4, 14, 15, 7} @ r)
    }

    @test
    def updateSequence28!(): Bool = region r {
        let a = Array#{1, 2, 3, 4, 5, 6, 7} @ r;
        Array.updateSequence!(-1, Array#{-1, -2, -3, -4, -5, -6, -7, -8} @ r, a);
        Array.sameElements(a, Array#{-2, -3, -4, -5, -6, -7, -8} @ r)
    }


    /////////////////////////////////////////////////////////////////////////////
    // sortWith                                                                //
    /////////////////////////////////////////////////////////////////////////////

    def cmp(x: Int32, y: Int32): Comparison =
        if (x < y) Comparison.LessThan
        else if (x == y) Comparison.EqualTo
        else Comparison.GreaterThan

    @test
    def sortWith01(): Bool = region r {
        let a = Array.sortWith(r, cmp, (Array#{} @ r): Array[Int32, _]);
        a `sameElements` (Array#{} @ r): Array[Int32, _]
    }

    @test
    def sortWith02(): Bool = region r {
        let a = Array.sortWith(r, cmp, Array#{0} @ r);
        a `sameElements` Array#{0} @ r
    }

    @test
    def sortWith03(): Bool = region r {
        let a = Array.sortWith(r, cmp, Array#{0,1} @ r);
        a `sameElements` Array#{0,1} @ r
    }

    @test
    def sortWith04(): Bool = region r {
        let a = Array.sortWith(r, cmp, Array#{1,0} @ r);
        a `sameElements` Array#{0,1} @ r
    }

    @test
    def sortWith05(): Bool = region r {
        let a = Array.sortWith(r, cmp, Array#{1,1} @ r);
        a `sameElements` Array#{1,1} @ r
    }

    @test
    def sortWith06(): Bool = region r {
        let a = Array.sortWith(r, cmp, Array#{0,1,2,3,4,5} @ r);
        a `sameElements` Array#{0,1,2,3,4,5} @ r
    }

    @test
    def sortWith07(): Bool = region r {
        let a = Array.sortWith(r, cmp, Array#{5,4,3,2,1,0} @ r);
        a `sameElements` Array#{0,1,2,3,4,5} @ r
    }

    @test
    def sortWith08(): Bool = region r {
        let a = Array.sortWith(r, cmp, Array#{5,3,0,4,1,2} @ r);
        a `sameElements` Array#{0,1,2,3,4,5} @ r
    }

    @test
    def sortWith09(): Bool = region r {
        let a = Array.sortWith(r, cmp, Array#{2,3,0,4,1,2} @ r);
        a `sameElements` Array#{0,1,2,2,3,4} @ r
    }

    @test
    def sortWith10(): Bool = region r {
        let a = Array.sortWith(r, flip(cmp), Array#{0,1,2,3,4,5} @ r);
        a `sameElements` Array#{5,4,3,2,1,0} @ r
    }

    @test
    def sortWith11(): Bool = region r {
        let a = Array.sortWith(r, flip(cmp), Array#{5,4,3,2,1,0} @ r);
        a `sameElements` Array#{5,4,3,2,1,0} @ r
    }

    @test
    def sortWith12(): Bool = region r {
        let a = Array.sortWith(r, flip(cmp), Array#{5,3,0,4,1,2} @ r);
        a `sameElements` Array#{5,4,3,2,1,0} @ r
    }

    @test
    def sortWith13(): Bool = region r {
        let a = Array.sortWith(r, flip(cmp), Array#{2,3,0,4,1,2} @ r);
        a `sameElements` Array#{4,3,2,2,1,0} @ r
    }

    /////////////////////////////////////////////////////////////////////////////
    // sort                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    def testSortVsSortWith(a: Array[Int32, r]): Bool \ r = region r1 {
        Array.sort(r1, a) `sameElements` Array.sortWith(r1, cmp, a)
    }

    @test
    def sort01(): Bool = region r {
        testSortVsSortWith((Array#{} @ r): Array[Int32, _])
    }

    @test
    def sort02(): Bool = region r {
        testSortVsSortWith(Array#{0} @ r)
    }

    @test
    def sort03(): Bool = region r {
        testSortVsSortWith(Array#{0,1} @ r)
    }

    @test
    def sort04(): Bool = region r {
        testSortVsSortWith(Array#{1,0} @ r)
    }

    @test
    def sort05(): Bool = region r {
        testSortVsSortWith(Array#{1,1} @ r)
    }

    @test
    def sort06(): Bool = region r {
        testSortVsSortWith(Array#{0,1,2,3,4,5} @ r)
    }

    @test
    def sort07(): Bool = region r {
        testSortVsSortWith(Array#{5,4,3,2,1,0} @ r)
    }

    @test
    def sort08(): Bool = region r {
        testSortVsSortWith(Array#{5,3,0,4,1,2} @ r)
    }

    @test
    def sort09(): Bool = region r {
        testSortVsSortWith(Array#{2,3,0,4,1,2} @ r)
    }

    /////////////////////////////////////////////////////////////////////////////
    // sortWith!                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sortWith01!(): Bool = region r {
        let a = (Array#{} @ r): Array[Int32, _];
        Array.sortWith!(cmp, a);
        a `sameElements` (Array#{} @ r): Array[Int32, _]
    }

    @test
    def sortWith02!(): Bool = region r {
        let a = Array#{0} @ r;
        Array.sortWith!(cmp, a);
        a `sameElements` Array#{0} @ r
    }

    @test
    def sortWith03!(): Bool = region r {
        let a = Array#{0,1} @ r;
        Array.sortWith!(cmp, a);
        a `sameElements` Array#{0,1} @ r
    }

    @test
    def sortWith04!(): Bool = region r {
        let a = Array#{1,0} @ r;
        Array.sortWith!(cmp, a);
        a `sameElements` Array#{0,1} @ r
    }

    @test
    def sortWith05!(): Bool = region r {
        let a = Array#{1,1} @ r;
        Array.sortWith!(cmp, a);
        a `sameElements` Array#{1,1} @ r
    }

    @test
    def sortWith06!(): Bool = region r {
        let a = Array#{0,1,2,3,4,5} @ r;
        Array.sortWith!(cmp, a);
        a `sameElements` Array#{0,1,2,3,4,5} @ r
    }

    @test
    def sortWith07!(): Bool = region r {
        let a = Array#{5,4,3,2,1,0} @ r;
        Array.sortWith!(cmp, a);
        a `sameElements` Array#{0,1,2,3,4,5} @ r
    }

    @test
    def sortWith08!(): Bool = region r {
        let a = Array#{5,3,0,4,1,2} @ r;
        Array.sortWith!(cmp, a);
        a `sameElements` Array#{0,1,2,3,4,5} @ r
    }

    @test
    def sortWith09!(): Bool = region r {
        let a = Array#{2,3,0,4,1,2} @ r;
        Array.sortWith!(cmp, a);
        a `sameElements` Array#{0,1,2,2,3,4} @ r
    }

    @test
    def sortWith10!(): Bool = region r {
        let a = Array#{0,1,2,3,4,5} @ r;
        Array.sortWith!(flip(cmp), a);
        a `sameElements` Array#{5,4,3,2,1,0} @ r
    }

    @test
    def sortWith11!(): Bool = region r {
        let a = Array#{5,4,3,2,1,0} @ r;
        Array.sortWith!(flip(cmp), a);
        a `sameElements` Array#{5,4,3,2,1,0} @ r
    }

    @test
    def sortWith12!(): Bool = region r {
        let a = Array#{5,3,0,4,1,2} @ r;
        Array.sortWith!(flip(cmp), a);
        a `sameElements` Array#{5,4,3,2,1,0} @ r
    }

    @test
    def sortWith13!(): Bool = region r {
        let a = Array#{2,3,0,4,1,2} @ r;
        Array.sortWith!(flip(cmp), a);
        a `sameElements` Array#{4,3,2,2,1,0} @ r
    }

    /////////////////////////////////////////////////////////////////////////////
    // sort!                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    def testSort!VsSortWith!(a: Array[Int32, r]): Bool \ r = region r1 {
        let b = Array.slice(r1, start = 0, end = Array.length(a), a);
        let c = Array.slice(r1, start = 0, end = Array.length(a), a);
        Array.sort!(b);
        Array.sortWith!(cmp, c);
        b `sameElements` c
    }

    @test
    def sort!01(): Bool = region r {
        testSort!VsSortWith!((Array#{} @ r): Array[Int32, _])
    }

    @test
    def sort!02(): Bool = region r {
        testSort!VsSortWith!(Array#{0} @ r)
    }

    @test
    def sort!03(): Bool = region r {
        testSort!VsSortWith!(Array#{0,1} @ r)
    }

    @test
    def sort!04(): Bool = region r {
        testSort!VsSortWith!(Array#{1,0} @ r)
    }

    @test
    def sort!05(): Bool = region r {
        testSort!VsSortWith!(Array#{1,1} @ r)
    }

    @test
    def sort!06(): Bool = region r {
        testSort!VsSortWith!(Array#{0,1,2,3,4,5} @ r)
    }

    @test
    def sort!07(): Bool = region r {
        testSort!VsSortWith!(Array#{5,4,3,2,1,0} @ r)
    }

    @test
    def sort!08(): Bool = region r {
        testSort!VsSortWith!(Array#{5,3,0,4,1,2} @ r)
    }

    @test
    def sort!09(): Bool = region r {
        testSort!VsSortWith!(Array#{2,3,0,4,1,2} @ r)
    }

    /////////////////////////////////////////////////////////////////////////////
    // sortBy                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    def testSortByVsSort(a: Array[Int32, r]): Bool \ r = region r1 {
        (Array.sortBy(r1, identity, a) `sameElements` Array.sort(r1, a)) and
        (Array.sortBy(r1, x -> 4 * x + 7, a) `sameElements` Array.sort(r1, a)) and
        (Array.sortBy(r1, x -> -x, a) `sameElements` Array.sortWith(r1, flip(cmp),a))
    }

    @test
    def sortBy01(): Bool = region r {
        testSortByVsSort((Array#{} @ r): Array[Int32, _])
    }

    @test
    def sortBy02(): Bool = region r {
        testSortByVsSort(Array#{0} @ r)
    }

    @test
    def sortBy03(): Bool = region r {
        testSortByVsSort(Array#{0,1} @ r)
    }

    @test
    def sortBy04(): Bool = region r {
        testSortByVsSort(Array#{1,0} @ r)
    }

    @test
    def sortBy05(): Bool = region r {
        testSortByVsSort(Array#{1,1} @ r)
    }

    @test
    def sortBy06(): Bool = region r {
        testSortByVsSort(Array#{0,1,2,3,4,5} @ r)
    }

    @test
    def sortBy07(): Bool = region r {
        testSortByVsSort(Array#{5,4,3,2,1,0} @ r)
    }

    @test
    def sortBy08(): Bool = region r {
        testSortByVsSort(Array#{5,3,0,4,1,2} @ r)
    }

    @test
    def sortBy09(): Bool = region r {
        testSortByVsSort(Array#{2,3,0,4,1,2} @ r)
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
    def sortBy10(): Bool = region reg {
        Array.sortBy(reg, r -> let R.R(x) = r; x.i, Array#{R.R({i = 2, s = "A"}), R.R({i = 1, s = "B"}), R.R({i = 3, s = "C"})} @ reg)
        `sameElements` Array#{R.R({i = 1, s = "B"}), R.R({i = 2, s = "A"}), R.R({i = 3, s = "C"})} @ reg
    }

    /////////////////////////////////////////////////////////////////////////////
    // sortBy!                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    def testSortBy!VsSortBy(a: Array[Int32, r]) : Bool \ r = region r1 {
        let b = Array.slice(r1, start = 0, end = Array.length(a), a);
        let c = Array.slice(r1, start = 0, end = Array.length(a), a);
        Array.sortBy!(identity, b);
        Array.sortBy!(x -> 4 * x + 7, c);
        (b `sameElements` Array.sortBy(r1, x -> 4 * x + 7, a)) and
        (c `sameElements` Array.sortBy(r1, identity, a))
    }

    @test
    def sortBy!01(): Bool = region r {
        testSortBy!VsSortBy((Array#{} @ r): Array[Int32, _])
    }

    @test
    def sortBy!02(): Bool = region r {
        testSortBy!VsSortBy(Array#{0} @ r)
    }

    @test
    def sortBy!03(): Bool = region r {
        testSortBy!VsSortBy(Array#{0,1} @ r)
    }

    @test
    def sortBy!04(): Bool = region r {
        testSortBy!VsSortBy(Array#{1,0} @ r)
    }

    @test
    def sortBy!05(): Bool = region r {
        testSortBy!VsSortBy(Array#{1,1} @ r)
    }

    @test
    def sortBy!06(): Bool = region r {
        testSortBy!VsSortBy(Array#{0,1,2,3,4,5} @ r)
    }

    @test
    def sortBy!07(): Bool = region r {
        testSortBy!VsSortBy(Array#{5,4,3,2,1,0} @ r)
    }

    @test
    def sortBy!08(): Bool = region r {
        testSortBy!VsSortBy(Array#{5,3,0,4,1,2} @ r)
    }

    @test
    def sortBy!09(): Bool = region r {
        testSortBy!VsSortBy(Array#{2,3,0,4,1,2} @ r)
    }

    /////////////////////////////////////////////////////////////////////////////
    // minimumBy                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def minimumBy01(): Bool = region r {
        Array.minimumBy((x, y) -> x <=> y, (Array#{} @ r): Array[Int32, _]) == None
    }

    @test
    def minimumBy02(): Bool = region r {
        Array.minimumBy((x, y) -> x <=> y, Array.range(r, 0, 5)) == Some(0)
    }

    @test
    def minimumBy03(): Bool = region r {
        Array.minimumBy((x, y) -> x <=> y, Array.range(r, 4, 10)) == Some(4)
    }

    @test
    def minimumBy04(): Bool = region r {
        let a = Array.range(r, 10, 20);
        Array.put(-22, 5, a);
        Array.minimumBy((x, y) -> x <=> y, a) == Some(-22)
    }

    /////////////////////////////////////////////////////////////////////////////
    // maximumBy                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def maximumBy01(): Bool = region r {
        Array.maximumBy((x, y) -> x <=> y, (Array#{} @ r): Array[Int32, _]) == None
    }

    @test
    def maximumBy02(): Bool = region r {
        Array.maximumBy((x, y) -> x <=> y, Array.range(r, 0, 5)) == Some(4)
    }

    @test
    def maximumBy03(): Bool = region r {
        Array.maximumBy((x, y) -> x <=> y, Array.range(r, 4, 10)) == Some(9)
    }

    @test
    def maximumBy04(): Bool = region r {
        let a = Array.range(r, 10, 20);
        Array.put(42, 5, a);
        Array.maximumBy((x, y) -> x <=> y, a) == Some(42)
    }


    /////////////////////////////////////////////////////////////////////////////
    // sum                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sum01(): Bool = region r {
        Array.sum(Array#{} @ r) == 0
    }

    @test
    def sum02(): Bool = region r {
        Array.sum(Array#{1} @ r) == 1
    }

    @test
    def sum03(): Bool = region r {
        Array.sum(Array#{1, 2, 3} @ r) == 6
    }

    @test
    def sum04(): Bool = region r {
        Array.sum(Array#{1, 2, 3, -3} @ r) == 3
    }

    @test
    def sum05(): Bool = region r {
        Array.sum(Array#{-1, -2, -3, -4} @ r) == -10
    }
    @test
    def sum06(): Bool = region r {
        Array.sum(Array#{10, -10} @ r) == 0
    }

    @test
    def sum07(): Bool = region r {
        Array.range(r, 1, 101) |> Array.sum == 5050
    }


    /////////////////////////////////////////////////////////////////////////////
    // sumWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sumWith01(): Bool = region r {
        Array.sumWith(x -> x + 1, Array#{} @ r) == 0
    }

    @test
    def sumWith02(): Bool = region r {
        Array.sumWith(x -> x + 1, Array#{1} @ r) == 2
    }

    @test
    def sumWith03(): Bool = region r {
        Array.sumWith(x -> x + 1, Array#{1, 2, 3} @ r) == 9
    }

    @test
    def sumWith04(): Bool = region r {
        Array.sumWith(x -> x + 1, Array#{1, 2, 3, -3} @ r) == 7
    }

    @test
    def sumWith05(): Bool = region r {
        Array.sumWith(x -> x + 1, Array#{-1, -2, -3, -4} @ r) == -6
    }

    @test
    def sumWith06(): Bool = region r {
        Array.sumWith(x -> x + 1, Array#{10, -10} @ r) == 2
    }


    /////////////////////////////////////////////////////////////////////////////
    // toDelayList                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toDelayList01(): Bool = region r {
        (Array#{} @ r): Array[Unit, _] |> Array.toDelayList == DelayList.empty()
    }

    @test
    def toDelayList02(): Bool = region r {
        (Array.new(r, 0): Array[String, r]) |> Array.toDelayList == DelayList.empty()
    }

    @test
    def toDelayList03(): Bool = region r {
        Array.range(r, -1000, 1000) |> Array.toDelayList == DelayList.range(-1000, 1000)
    }

    @test
    def toDelayList04(): Bool = region r {
        Array.range(r, -1000, 1000) |> Array.toDelayList |> DelayList.toArray(r) `sameElements` Array.range(r, -1000, 1000)
    }

    /////////////////////////////////////////////////////////////////////////////
    // toChain                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toChain01(): Bool = region r {
        Array.toChain((Array#{} @ r): Array[Unit, _]) == Chain.empty(): Chain[Unit]
    }

    @test
    def toChain02(): Bool = region r {
        Array.toChain(Array#{1} @ r) == Chain.singleton(1)
    }

    @test
    def toChain03(): Bool = region r {
        Array.toChain(Array#{1,2} @ r) == List.toChain(1 :: 2 :: Nil)
    }

    @test
    def toChain04(): Bool = region r {
        Array.toChain(Array#{1,2,3} @ r) == List.toChain(1 :: 2 :: 3 :: Nil)
    }

    /////////////////////////////////////////////////////////////////////////////
    // toNec                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toNec01(): Bool = region r {
        Array.toNec((Array#{} @ r): Array[Unit, _]) == None
    }

    @test
    def toNec02(): Bool = region r {
        Array.toNec(Array#{1} @ r) == Some(Nec.singleton(1))
    }

    @test
    def toNec03(): Bool = region r {
        Array.toNec(Array#{1,2} @ r) == List.toNec(1 :: 2 :: Nil)
    }

    @test
    def toNec04(): Bool = region r {
        Array.toNec(Array#{1,2,3} @ r) == List.toNec(1 :: 2 :: 3 :: Nil)
    }

    /////////////////////////////////////////////////////////////////////////////
    // copyOfRange                                                             //
    /////////////////////////////////////////////////////////////////////////////

    // depends on flatten

    @test
    def copyOfRange01(): Bool = region r {
        Array.copyOfRange(r, 0, 3, Array#{true, false, true} @ r) `sameElements` Array#{true, false, true} @ r
    }

    @test
    def copyOfRange02(): Bool = region r {
        Array.copyOfRange(r, 1, 3, Array#{true, false, true} @ r) `sameElements` Array#{false, true} @ r
    }

    @test
    def copyOfRange03(): Bool = region r {
        let a = Array.copyOfRange(r, 2, 5, Array#{true, false, true} @ r);
        Array.length(a) == 3 and Array.memberOf(true, a)
    }

    @test
    def copyOfRange04(): Bool = region r {
        Array.copyOfRange(r, 0, 3, Array#{'a', 'b', 'c'} @ r) `sameElements` Array#{'a', 'b', 'c'} @ r
    }

    @test
    def copyOfRange05(): Bool = region r {
        Array.copyOfRange(r, 1, 3, Array#{'a', 'b', 'c'} @ r) `sameElements` Array#{'b', 'c'} @ r
    }

    @test
    def copyOfRange06(): Bool = region r {
        let a = Array.copyOfRange(r, 2, 5, Array#{'a', 'b', 'c'} @ r);
        Array.length(a) == 3 and Array.memberOf('c', a)
    }

    @test
    def copyOfRange07(): Bool = region r {
        Array.copyOfRange(r, 0, 3, Array#{1.0f32, 2.0f32, 3.0f32} @ r) `sameElements` Array#{1.0f32, 2.0f32, 3.0f32} @ r
    }

    @test
    def copyOfRange08(): Bool = region r {
        Array.copyOfRange(r, 1, 3, Array#{1.0f32, 2.0f32, 3.0f32} @ r) `sameElements` Array#{2.0f32, 3.0f32} @ r
    }

    @test
    def copyOfRange09(): Bool = region r {
        let a = Array.copyOfRange(r, 2, 5, Array#{1.0f32, 2.0f32, 3.0f32} @ r);
        Array.length(a) == 3 and Array.memberOf(3.0f32, a)
    }

    @test
    def copyOfRange10(): Bool = region r {
        Array.copyOfRange(r, 0, 3, Array#{1.0f64, 2.0f64, 3.0f64} @ r) `sameElements` Array#{1.0f64, 2.0f64, 3.0f64} @ r
    }

    @test
    def copyOfRange11(): Bool = region r {
        Array.copyOfRange(r, 1, 3, Array#{1.0f64, 2.0f64, 3.0f64} @ r) `sameElements` Array#{2.0f64, 3.0f64} @ r
    }

    @test
    def copyOfRange12(): Bool = region r {
        let a = Array.copyOfRange(r, 2, 5, Array#{1.0f64, 2.0f64, 3.0f64} @ r);
        Array.length(a) == 3 and Array.memberOf(3.0f64, a)
    }

    @test
    def copyOfRange13(): Bool = region r {
        Array.copyOfRange(r, 0, 3, Array#{1i8, 2i8, 3i8} @ r) `sameElements` Array#{1i8, 2i8, 3i8} @ r
    }

    @test
    def copyOfRange14(): Bool = region r {
        Array.copyOfRange(r, 1, 3, Array#{1i8, 2i8, 3i8} @ r) `sameElements` Array#{2i8, 3i8} @ r
    }

    @test
    def copyOfRange15(): Bool = region r {
        let a = Array.copyOfRange(r, 2, 5, Array#{1i8, 2i8, 3i8} @ r);
        Array.length(a) == 3 and Array.memberOf(3i8, a)
    }

    @test
    def copyOfRange16(): Bool = region r {
        Array.copyOfRange(r, 0, 3, Array#{1i16, 2i16, 3i16} @ r) `sameElements` Array#{1i16, 2i16, 3i16} @ r
    }

    @test
    def copyOfRange17(): Bool = region r {
        Array.copyOfRange(r, 1, 3, Array#{1i16, 2i16, 3i16} @ r) `sameElements` Array#{2i16, 3i16} @ r
    }

    @test
    def copyOfRange18(): Bool = region r {
        let a = Array.copyOfRange(r, 2, 5, Array#{1i16, 2i16, 3i16} @ r);
        Array.length(a) == 3 and Array.memberOf(3i16, a)
    }

    @test
    def copyOfRange19(): Bool = region r {
        Array.copyOfRange(r, 0, 3, Array#{1, 2, 3} @ r) `sameElements` Array#{1, 2, 3} @ r
    }

    @test
    def copyOfRange20(): Bool = region r {
        Array.copyOfRange(r, 1, 3, Array#{1, 2, 3} @ r) `sameElements` Array#{2, 3} @ r
    }

    @test
    def copyOfRange21(): Bool = region r {
        let a = Array.copyOfRange(r, 2, 5, Array#{1, 2, 3} @ r);
        Array.length(a) == 3 and Array.memberOf(3, a)
    }

    @test
    def copyOfRange22(): Bool = region r {
        Array.copyOfRange(r, 0, 3, Array#{1i64, 2i64, 3i64} @ r) `sameElements` Array#{1i64, 2i64, 3i64} @ r
    }

    @test
    def copyOfRange23(): Bool = region r {
        Array.copyOfRange(r, 1, 3, Array#{1i64, 2i64, 3i64} @ r) `sameElements` Array#{2i64, 3i64} @ r
    }

    @test
    def copyOfRange24(): Bool = region r {
        let a = Array.copyOfRange(r, 2, 5, Array#{1i64, 2i64, 3i64} @ r);
        Array.length(a) == 3 and Array.memberOf(3i64, a)
    }

    @test
    def copyOfRange25(): Bool = region r {
        let a = Array.copyOfRange(r, 0, 3, Array#{Array#{'a', 'b', 'c'} @ r, Array#{'d', 'e', 'f', 'g'} @ r, Array#{'h'} @ r} @ r);
        let b = Array#{Array#{'a', 'b', 'c'} @ r, Array#{'d', 'e', 'f', 'g'} @ r, Array#{'h'} @ r} @ r;
        Array.flatten(r, a) `sameElements` Array.flatten(r, b)
    }

    @test
    def copyOfRange26(): Bool = region r {
        let a = Array.copyOfRange(r, 1, 3, Array#{Array#{'a', 'b', 'c'} @ r, Array#{'d', 'e', 'f', 'g'} @ r, Array#{'h'} @ r} @ r);
        let b = Array#{Array#{'d', 'e', 'f', 'g'} @ r, Array#{'h'} @ r } @ r;
        Array.flatten(r, a) `sameElements` Array.flatten(r, b)
    }

    @test
    def copyOfRange27(): Bool = region r {
        Array.copyOfRange(r, 0, 3, Array#{Some(1), Some(2), None} @ r) `sameElements` Array#{Some(1), Some(2), None} @ r
    }

    @test
    def copyOfRange28(): Bool = region r {
        Array.copyOfRange(r, 1, 3, Array#{Some(1), Some(2), None} @ r) `sameElements` Array#{Some(2), None} @ r
    }

    @test
    def copyOfRange29(): Bool = region r {
        let a = Array.copyOfRange(r, 2, 5, Array#{Some(1), Some(2), None} @ r);
        Array.length(a) == 3 and Array.memberOf(None, a)
    }

    @test
    def copyOfRange30(): Bool = region r {
        let a = Array.copyOfRange(r, 0, 3, Array#{Array#{Some('a'), Some('b'), Some('c')} @ r, Array#{Some('d'), Some('e'), Some('f'), Some('g')} @ r, Array#{None} @ r} @ r);
        let b = Array#{Array#{Some('a'), Some('b'), Some('c')} @ r, Array#{Some('d'), Some('e'), Some('f'), Some('g')} @ r, Array#{None} @ r } @ r;
        Array.flatten(r, a) `sameElements` Array.flatten(r, b)
    }

    @test
    def copyOfRange31(): Bool = region r {
        let a = Array.copyOfRange(r, 1, 3, Array#{Array#{Some('a'), Some('b'), Some('c')} @ r, Array#{Some('d'), Some('e'), Some('f'), Some('g')} @ r, Array#{None} @ r} @ r);
        let b = Array#{Array#{Some('d'), Some('e'), Some('f'), Some('g')} @ r, Array#{None} @ r} @ r;
        Array.flatten(r, a) `sameElements` Array.flatten(r, b)
    }

    /////////////////////////////////////////////////////////////////////////////
    // iterator                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def iterator01(): Bool = region r {
        (Array#{} @ r): Array[Int32, r] |> Array.iterator(r) |> Iterator.toList == Nil
    }

    @test
    def iterator02(): Bool = region r {
        (Array#{1, 2, 3} @ r) |> Array.iterator(r) |> Iterator.toList == 1 :: 2 :: 3 :: Nil
    }

    @test
    def iterator03(): Bool = region r {
        (Array#{1, 2, 3} @ r) |> Array.iterator(r) |> Iterator.toArray(r) `sameElements` Array#{1, 2, 3} @ r
    }

    @test
    def iterator04(): Bool = region r {
        Array.range(r, -100, 100) |> Array.iterator(r) |> Iterator.toArray(r) `sameElements` Array.range(r, -100, 100)
    }


    /////////////////////////////////////////////////////////////////////////////
    // join                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def join01(): Bool = region r {
        Array.join(",", (Array#{} @ r): Array[Int32, _]) == ""
    }

    @test
    def join02(): Bool = region r {
        Array.join(",", Array#{1} @ r) == "1"
    }

    @test
    def join03(): Bool = region r {
        Array.join(",", Array#{1, 2, 3} @ r) == "1,2,3"
    }

    @test
    def join04(): Bool = region r {
        Array.join(",", Array#{"1", "2", "3"} @ r) == "1,2,3"
    }


    /////////////////////////////////////////////////////////////////////////////
    // joinWith                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def joinWith01(): Bool = region r {
        Array.joinWith(x -> "${x + 1}", ",", Array#{} @ r) == ""
    }

    @test
    def joinWith02(): Bool = region r {
        Array.joinWith(x -> "${x + 1}", ",", Array#{1} @ r) == "2"
    }

    @test
    def joinWith03(): Bool = region r {
        Array.joinWith(x -> "${x + 1}", ",", Array#{1, 2, 3} @ r) == "2,3,4"
    }

    @test
    def joinWith04(): Bool = region r {
        Array.joinWith(x -> x + x, ",", Array#{"1", "2", "3"} @ r) == "11,22,33"
    }

    @test
    def joinWith05(): Bool = region r {
        let a: Array[Int32, _] = Array#{} @ r;
        let s = Array.joinWith(Int32.toString, ",", a);
        s == ""
    }

    @test
    def joinWith06(): Bool = region r {
        let a = Array#{1} @ r;
        let s = Array.joinWith(Int32.toString, ",", a);
        s == "1"
    }

    @test
    def joinWith07(): Bool = region r {
        let a = Array#{1,2} @ r;
        let s = Array.joinWith(Int32.toString, ",", a);
        s == "1,2"
    }

    @test
    def joinWith08(): Bool = region r {
        let a = Array#{1,2,3} @ r;
        let s = Array.joinWith(Int32.toString, ",", a);
        s == "1,2,3"
    }

    @test
    def joinWith09(): Bool = region r {
        let a = Array#{1,2,3} @ r;
        let s = Array.joinWith(Int32.toString, "", a);
        s == "123"
    }

    @test
    def joinWith10(): Bool = region r {
        let a = Array#{1,2,3} @ r;
        let s = Array.joinWith(Int32.toString, "..", a);
        s == "1..2..3"
    }


    /////////////////////////////////////////////////////////////////////////////
    // toMutList                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toMutList01(): Bool = region r {
        MutList.sameElements(Array.toMutList(r, (Array#{} @ r): Array[Int32, r]), MutList.new(r))
    }

    @test
    def toMutList02(): Bool = region r {
        let v = MutList.new(r);
        MutList.push!(1, v);
        MutList.sameElements(Array.toMutList(r, Array#{1} @ r), v)
    }

    @test
    def toMutList03(): Bool = region r {
        let v = MutList.new(r);
        MutList.push!(1, v);
        MutList.push!(2, v);
        MutList.push!(3, v);
        MutList.sameElements(Array.toMutList(r, Array#{1, 2, 3} @ r), v)
    }

    @test
    def toMutList04(): Bool = region r {
        MutList.sameElements((Array.range(r, 0, 100) |> Array.toMutList(r)), MutList.range(r, 0, 100))
    }

    @test
    def toMutList05(): Bool = region r {
        let v = Array.toMutList(r, Array#{1, 2, 3} @ r);
        MutList.push!(4, v);
        Array.sameElements(MutList.toArray(r, v), Array#{1, 2, 3, 4} @ r)
    }

    @test
    def toMutList06(): Bool = region r {
        let v = Array.toMutList(r, Array#{1, 2, 3} @ r);
        MutList.push!(4, v);
        discard MutList.pop!(v);
        discard MutList.pop!(v);
        discard MutList.pop!(v);
        discard MutList.pop!(v);
        Array.sameElements(MutList.toArray(r, v), Array#{} @ r)
    }

    /////////////////////////////////////////////////////////////////////////////
    // shuffle                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def shuffle01(): Bool \ NonDet = region r {
        let a = (Array#{} @ r): Array[Int32, _];
        Array.shuffle(Random.new(), a);
        Array.length(a) == 0 and Array.toSet(a) == Set#{}
    }

    @test
    def shuffle02(): Bool \ NonDet = region r {
        let a = (Array#{0} @ r);
        Array.shuffle(Random.new(), a);
        Array.length(a) == 1 and Array.toSet(a) == Set#{0}
    }

    @test
    def shuffle03(): Bool \ NonDet = region r {
        let a = (Array#{0, 1, 2, 3} @ r);
        Array.shuffle(Random.new(), a);
        Array.length(a) == 4 and Array.toSet(a) == Set#{0, 1, 2, 3}
    }

    @test
    def shuffle04(): Bool \ NonDet = region r {
        let a = (Array#{0, 1, 2, 3, 4, 5, 6, 7, 8, 9} @ r);
        Array.shuffle(Random.new(), a);
        Array.length(a) == 10 and Array.toSet(a) == Set#{0, 1, 2, 3, 4, 5, 6, 7, 8, 9}
    }

}

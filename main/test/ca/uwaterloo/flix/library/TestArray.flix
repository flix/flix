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

    @Test
    def toString01(): Bool = region rc {
        Array.toString((Array#{} @ rc): Array[Int32, _]) == "Array#{}"
    }

    @Test
    def toString02(): Bool = region rc {
        Array.toString((Array#{1} @ rc)) == "Array#{1}"
    }

    @Test
    def toString03(): Bool = region rc {
        Array.toString((Array#{1, 2, 3, 4} @ rc)) == "Array#{1, 2, 3, 4}"
    }

    @Test
    def toString04(): Bool = region rc {
        Array.toString((Array#{'a', 'b', 'c', 'd'} @ rc)) == "Array#{a, b, c, d}"
    }

    /////////////////////////////////////////////////////////////////////////////
    // new                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testNew01(): Bool = region rc {
        Array.get(0, Array.new(rc, 10)) == 0
    }

    @Test
    def testNew02(): Bool = region rc {
        Array.get(5, Array.new(rc, 10)) == 0
    }

    @Test
    def testNew03(): Bool = region rc {
        Array.get(9, Array.new(rc, 10)) == 0
    }

    @Test
    def testNew04(): Bool = region rc {
        Array.get(0, Array.new(rc, 10)) == false
    }

    @Test
    def testNew05(): Bool = region rc {
        Array.get(5, Array.new(rc, 10)) == false
    }

    @Test
    def testNew06(): Bool = region rc {
        Array.get(9, Array.new(rc, 10)) == false
    }

    /////////////////////////////////////////////////////////////////////////////
    // nth                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testNth01(): Bool = region rc {
        Array.nth(0, Array#{1, 2, 3} @ rc) == Some(1)
    }

    @Test
    def testNth02(): Bool = region rc {
        Array.nth(1, Array#{1, 2, 3} @ rc) == Some(2)
    }

    @Test
    def testNth03(): Bool = region rc {
        Array.nth(2, Array#{1, 2, 3} @ rc) == Some(3)
    }

    @Test
    def testNth04(): Bool = region rc {
        Array.nth(3, Array#{1, 2, 3} @ rc) == None
    }

    @Test
    def testNth05(): Bool = region rc {
        Array.nth(-2, Array#{1, 2, 3} @ rc) == None
    }

    /////////////////////////////////////////////////////////////////////////////
    // put                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testPut01(): Bool = region rc {
        let a = (Array#{1} @ rc) !> Array.put(42, 0);
        Array.get(0, a) == 42
    }

    @Test
    def testPut02(): Bool = region rc {
        let a = (Array#{1, 2} @ rc) !> Array.put(42, 1);
        Array.get(1, a) == 42
    }

    @Test
    def testPut03(): Bool = region rc {
        let a = (Array#{1, 2, 3} @ rc) !> Array.put(42, 2);
        Array.get(2, a) == 42
    }

    /////////////////////////////////////////////////////////////////////////////
    // isEmpty                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testIsEmpty01(): Bool = region rc {
        Array.isEmpty(Array#{} @ rc)
    }

    @Test
    def testIsEmpty02(): Bool = region rc {
        not Array.isEmpty(Array#{1} @ rc)
    }

    @Test
    def testIsEmpty03(): Bool = region rc {
        not Array.isEmpty(Array#{1, 2, 3} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // length                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testLength01(): Bool = region rc {
        Array.length(Array#{} @ rc) == 0
    }

    @Test
    def testLength02(): Bool = region rc {
        Array.length(Array#{1} @ rc) == 1
    }

    @Test
    def testLength03(): Bool = region rc {
        Array.length(Array#{1, 2, 3} @ rc) == 3
    }

    /////////////////////////////////////////////////////////////////////////////
    // slice                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testSlice01(): Bool = region rc {
        Array.get(0, Array.slice(rc, start = 0, end = 1, Array#{1, 2, 3} @ rc)) == 1
    }

    @Test
    def testSlice02(): Bool = region rc {
        Array.get(0, Array.slice(rc, start = 1, end = 2, Array#{1, 2, 3} @ rc)) == 2
    }

    @Test
    def testSlice03(): Bool = region rc {
        Array.get(0, Array.slice(rc, start = 2, end = 3, Array#{1, 2, 3} @ rc)) == 3
    }

    /////////////////////////////////////////////////////////////////////////////
    // toList                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testToList01(): Bool = region rc {
        Array.toList((Array#{} @ rc): Array[Unit, _]) == Nil
    }

    @Test
    def testToList02(): Bool = region rc {
        Array.toList(Array#{1} @ rc) == 1 :: Nil
    }

    @Test
    def testToList03(): Bool = region rc {
        Array.toList(Array#{1, 2} @ rc) == 1 :: 2 :: Nil
    }

    /////////////////////////////////////////////////////////////////////////////
    // toNel                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toNel01(): Bool = region rc {
        Array.toNel((Array#{} @ rc): Array[Unit, _]) == None
    }

    @Test
    def toNel02(): Bool = region rc {
        Array.toNel(Array#{1} @ rc) == Some(Nel.singleton(1))
    }

    @Test
    def toNel03(): Bool = region rc {
        Array.toNel(Array#{1,2} @ rc) == List.toNel(1 :: 2 :: Nil)
    }

    @Test
    def toNel04(): Bool = region rc {
        Array.toNel(Array#{1,2,3} @ rc) == List.toNel(1 :: 2 :: 3 :: Nil)
    }

    /////////////////////////////////////////////////////////////////////////////
    // toMutDeque                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toMutDeque01(): Bool = region rc {
        let a = (Array#{} @ rc): Array[Int32, _];
        let d = MutDeque.new(rc): MutDeque[Int32, _];

        Array.toMutDeque(rc, a) `MutDeque.sameElements` d
    }

    @Test
    def toMutDeque02(): Bool = region rc {
        let a = (Array#{1} @ rc);

        let d = MutDeque.new(rc);
        MutDeque.pushBack(1, d);

        Array.toMutDeque(rc, a) `MutDeque.sameElements` d
    }

    @Test
    def toMutDeque03(): Bool = region rc {
        let a = (Array#{1, 2, 3} @ rc);

        let d = MutDeque.new(rc);
        MutDeque.pushBack(1, d);
        MutDeque.pushBack(2, d);
        MutDeque.pushBack(3, d);

        Array.toMutDeque(rc, a) `MutDeque.sameElements` d
    }

    @Test
    def toMutDeque04(): Bool = region rc {
        let a = (Array#{1, 2, 3, 4, 5} @ rc);

        let d = MutDeque.new(rc);
        MutDeque.pushBack(3, d);
        MutDeque.pushBack(4, d);
        MutDeque.pushBack(5, d);
        MutDeque.pushFront(2, d);
        MutDeque.pushFront(1, d);

        Array.toMutDeque(rc, a) `MutDeque.sameElements` d
    }

    @Test
    def toMutDeque05(): Bool = region rc {
        let a = (Array#{'a', 'b', 'c'} @ rc);

        let d = MutDeque.new(rc);
        MutDeque.pushFront('c', d);
        MutDeque.pushFront('b', d);
        MutDeque.pushFront('a', d);

        Array.toMutDeque(rc, a) `MutDeque.sameElements` d
    }

    /////////////////////////////////////////////////////////////////////////////
    // toVector                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toVector01(): Bool = region rc {
        Array.toVector((Array#{} @ rc) : Array[Int32, _]) `Vector.equals` Vector.empty()
    }

    @Test
    def toVector02(): Bool = region rc {
        Array.toVector((Array#{0} @ rc) : Array[Int32, _]) `Vector.equals` Vector.singleton(0)
    }

    @Test
    def toVector03(): Bool = region rc {
        Array.toVector((Array#{1, 2, 3} @ rc) : Array[Int32, _]) `Vector.equals` List.toVector(1 :: 2 :: 3 :: Nil)
    }

    /////////////////////////////////////////////////////////////////////////////
    // head                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def head01(): Bool = region rc {
        Array.head((Array#{} @ rc): Array[Int32, _]) == None
    }

    @Test
    def head02(): Bool = region rc {
        Array.head(Array#{1} @ rc) == Some(1)
    }

    @Test
    def head03(): Bool = region rc {
        Array.head(Array#{2,1} @ rc) == Some(2)
    }

    @Test
    def head04(): Bool = region rc {
        Array.head(Array#{3,2,1} @ rc) == Some(3)
    }

    /////////////////////////////////////////////////////////////////////////////
    // last                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def last01(): Bool = region rc {
        Array.last(Array#{} @ rc): Option[Unit] == None
    }

    @Test
    def last02(): Bool = region rc {
        Array.last(Array#{1} @ rc) == Some(1)
    }

    @Test
    def last03(): Bool = region rc {
        Array.last(Array#{1,2} @ rc) == Some(2)
    }

    @Test
    def last04(): Bool = region rc {
        Array.last(Array#{1,2,3} @ rc) == Some(3)
    }

    /////////////////////////////////////////////////////////////////////////////
    // append                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def append01(): Bool = region rc {
        let a = Array.append(rc, (Array#{} @ rc): Array[Int32, _], (Array#{} @ rc): Array[Int32, _]);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def append02(): Bool = region rc {
        let a = Array.append(rc, (Array#{} @ rc): Array[Int32, _], Array#{1} @ rc);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @Test
    def append03(): Bool = region rc {
        let a = Array.append(rc, (Array#{} @ rc): Array[Int32, _], Array#{1,2} @ rc);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @Test
    def append04(): Bool = region rc {
        let a = Array.append(rc, Array#{1} @ rc, (Array#{} @ rc): Array[Int32, _]);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @Test
    def append05(): Bool = region rc {
        let a = Array.append(rc, Array#{1,2} @ rc, (Array#{} @ rc): Array[Int32, _]);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @Test
    def append06(): Bool = region rc {
        let a = Array.append(rc, Array#{1} @ rc, Array#{2} @ rc);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @Test
    def append07(): Bool = region rc {
        let a = Array.append(rc, Array#{1,2} @ rc, Array#{3} @ rc);
        Array.sameElements(a, Array#{1,2,3} @ rc)
    }

    @Test
    def append08(): Bool = region rc {
        let a = Array.append(rc, Array#{1} @ rc, Array#{2,3} @ rc);
        Array.sameElements(a, Array#{1,2,3} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // memberOf                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def memberOf01(): Bool = region rc {
        Array.memberOf(0, (Array#{} @ rc): Array[Int32, _]) == false
    }

    @Test
    def memberOf02(): Bool = region rc {
        Array.memberOf(0, Array#{1} @ rc) == false
    }

    @Test
    def memberOf03(): Bool = region rc {
        Array.memberOf(0, Array#{0} @ rc) == true
    }

    @Test
    def memberOf04(): Bool = region rc {
        Array.memberOf(0, Array#{1,2} @ rc) == false
    }

    @Test
    def memberOf05(): Bool = region rc {
        Array.memberOf(1, Array#{1,2} @ rc) == true
    }

    @Test
    def memberOf06(): Bool = region rc {
        Array.memberOf(2, Array#{1,2} @ rc) == true
    }

    @Test
    def memberOf07(): Bool = region rc {
        Array.memberOf(3, Array#{1,2} @ rc) == false
    }

    /////////////////////////////////////////////////////////////////////////////
    // indexOf                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def indexOf01(): Bool = region rc {
        Array.indexOf(0, (Array#{} @ rc): Array[Int32, _]) == None
    }

    @Test
    def indexOf02(): Bool = region rc {
        Array.indexOf(0, Array#{1} @ rc) == None
    }

    @Test
    def indexOf03(): Bool = region rc {
        Array.indexOf(1, Array#{1} @ rc) == Some(0)
    }

    @Test
    def indexOf04(): Bool = region rc {
        Array.indexOf(0, Array#{1,2} @ rc) == None
    }

    @Test
    def indexOf05(): Bool = region rc {
        Array.indexOf(1, Array#{1,2} @ rc) == Some(0)
    }

    @Test
    def indexOf06(): Bool = region rc {
        Array.indexOf(2, Array#{1,2} @ rc) == Some(1)
    }

    /////////////////////////////////////////////////////////////////////////////
    // indexOfLeft                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def indexOfLeft01(): Bool = region rc {
        Array.indexOfLeft(0, (Array#{} @ rc): Array[Int32, _]) == None
    }

    @Test
    def indexOfLeft02(): Bool = region rc {
        Array.indexOfLeft(0, Array#{1} @ rc) == None
    }

    @Test
    def indexOfLeft03(): Bool = region rc {
        Array.indexOfLeft(1, Array#{1} @ rc) == Some(0)
    }

    @Test
    def indexOfLeft04(): Bool = region rc {
        Array.indexOfLeft(0, Array#{1,2} @ rc) == None
    }

    @Test
    def indexOfLeft05(): Bool = region rc {
        Array.indexOfLeft(1, Array#{1,2} @ rc) == Some(0)
    }

    @Test
    def indexOfLeft06(): Bool = region rc {
        Array.indexOfLeft(2, Array#{1,2} @ rc) == Some(1)
    }

    @Test
    def indexOfLeft07(): Bool = region rc {
        Array.indexOfLeft(1, Array#{1,1} @ rc) == Some(0)
    }

    /////////////////////////////////////////////////////////////////////////////
    // indexOfRight                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def indexOfRight01(): Bool = region rc {
        Array.indexOfRight(0, (Array#{} @ rc): Array[Int32, _]) == None
    }

    @Test
    def indexOfRight02(): Bool = region rc {
        Array.indexOfRight(0, Array#{1} @ rc) == None
    }

    @Test
    def indexOfRight03(): Bool = region rc {
        Array.indexOfRight(1, Array#{1} @ rc) == Some(0)
    }

    @Test
    def indexOfRight04(): Bool = region rc {
        Array.indexOfRight(0, Array#{1,2} @ rc) == None
    }

    @Test
    def indexOfRight05(): Bool = region rc {
        Array.indexOfRight(1, Array#{1,2} @ rc) == Some(0)
    }

    @Test
    def indexOfRight06(): Bool = region rc {
        Array.indexOfRight(2, Array#{1,2} @ rc) == Some(1)
    }

    @Test
    def indexOfRight07(): Bool = region rc {
        Array.indexOfRight(1, Array#{1,1} @ rc) == Some(1)
    }

    /////////////////////////////////////////////////////////////////////////////
    // indices                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def indices01(): Bool = region rc {
        let a = Array.indices(rc, 0, (Array#{} @ rc): Array[Int32, rc]);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, rc])
    }

    @Test
    def indices02(): Bool = region rc {
        let a = Array.indices(rc, 0, Array#{1} @ rc);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, rc])
    }

    @Test
    def indices03(): Bool = region rc {
        let a = Array.indices(rc, 1, Array#{1} @ rc);
        Array.sameElements(a, Array#{0} @ rc)
    }

    @Test
    def indices04(): Bool = region rc {
        let a = Array.indices(rc, 0, Array#{1,2} @ rc);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, rc])
    }

    @Test
    def indices05(): Bool = region rc {
        let a = Array.indices(rc, 1, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{0} @ rc)
    }

    @Test
    def indices06(): Bool = region rc {
        let a = Array.indices(rc, 2, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @Test
    def indices07(): Bool = region rc {
        let a = Array.indices(rc, 1, Array#{1,1} @ rc);
        Array.sameElements(a, Array#{0,1} @ rc)
    }
    /////////////////////////////////////////////////////////////////////////////
    // find                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def find01(): Bool = region rc {
        Array.find(i -> i > 2, Array#{} @ rc) == None
    }

    @Test
    def find02(): Bool = region rc {
        Array.find(i -> i > 2, Array#{1} @ rc) == None
    }

    @Test
    def find03(): Bool = region rc {
        Array.find(i -> i > 2, Array#{3} @ rc) == Some(3)
    }

    @Test
    def find04(): Bool = region rc {
        Array.find(i -> i > 2, Array#{1,2} @ rc) == None
    }

    @Test
    def find05(): Bool = region rc {
        Array.find(i -> i > 2, Array#{6,-6} @ rc) == Some(6)
    }

    @Test
    def find06(): Bool = region rc {
        Array.find(i -> i > 2, Array#{-6,6} @ rc) == Some(6)
    }

    @Test
    def find07(): Bool = region rc {
        Array.find(i -> i > 2, Array#{6,7} @ rc) == Some(6)
    }

    /////////////////////////////////////////////////////////////////////////////
    // findLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def findLeft01(): Bool = region rc {
        Array.findLeft(i -> i > 2, Array#{} @ rc) == None
    }

    @Test
    def findLeft02(): Bool = region rc {
        Array.findLeft(i -> i > 2, Array#{1} @ rc) == None
    }

    @Test
    def findLeft03(): Bool = region rc {
        Array.findLeft(i -> i > 2, Array#{3} @ rc) == Some(3)
    }

    @Test
    def findLeft04(): Bool = region rc {
        Array.findLeft(i -> i > 2, Array#{1,2} @ rc) == None
    }

    @Test
    def findLeft05(): Bool = region rc {
        Array.findLeft(i -> i > 2, Array#{6,-6} @ rc) == Some(6)
    }

    @Test
    def findLeft06(): Bool = region rc {
        Array.findLeft(i -> i > 2, Array#{-6,6} @ rc) == Some(6)
    }

    @Test
    def findLeft07(): Bool = region rc {
        Array.findLeft(i -> i > 2, Array#{6,7} @ rc) == Some(6)
    }

    /////////////////////////////////////////////////////////////////////////////
    // findRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def findRight01(): Bool = region rc {
        Array.findRight(i -> i > 2, Array#{} @ rc) == None
    }

    @Test
    def findRight02(): Bool = region rc {
        Array.findRight(i -> i > 2, Array#{1} @ rc) == None
    }

    @Test
    def findRight03(): Bool = region rc {
        Array.findRight(i -> i > 2, Array#{3} @ rc) == Some(3)
    }

    @Test
    def findRight04(): Bool = region rc {
        Array.findRight(i -> i > 2, Array#{1,2} @ rc) == None
    }

    @Test
    def findRight05(): Bool = region rc {
        Array.findRight(i -> i > 2, Array#{6,-6} @ rc) == Some(6)
    }

    @Test
    def findRight06(): Bool = region rc {
        Array.findRight(i -> i > 2, Array#{-6,6} @ rc) == Some(6)
    }

    @Test
    def findRight07(): Bool = region rc {
        Array.findRight(i -> i > 2, Array#{6,7} @ rc) == Some(7)
    }

    /////////////////////////////////////////////////////////////////////////////
    // range                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def range01(): Bool = region rc {
        let a = Array.range(rc, 1, 0);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def range02(): Bool = region rc {
        let a = Array.range(rc, 1, 1);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def range03(): Bool = region rc {
        let a = Array.range(rc, 1, 2);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @Test
    def range04(): Bool = region rc {
        let a = Array.range(rc, 1, 3);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @Test
    def range05(): Bool = region rc {
        let a = Array.range(rc, 1, 4);
        Array.sameElements(a, Array#{1,2,3} @ rc)
    }

    @Test
    def range06(): Bool = region rc {
        let a = Array.range(rc, -1, 3);
        Array.sameElements(a, Array#{-1,0,1,2} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // repeat rc,
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def repeat01(): Bool = region rc {
        let a = Array.repeat(rc, -1, 1);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def repeat02(): Bool = region rc {
        let a = Array.repeat(rc, 0, 1);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def repeat03(): Bool = region rc {
        let a = Array.repeat(rc, 1, 1);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @Test
    def repeat04(): Bool = region rc {
        let a = Array.repeat(rc, 2, 1);
        Array.sameElements(a, Array#{1,1} @ rc)
    }

    @Test
    def repeat05(): Bool = region rc {
        let a = Array.repeat(rc, 3, 1);
        Array.sameElements(a, Array#{1,1,1} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // scan                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def scan01(): Bool = region rc {
        let a = Array.scan(rc, (i, b) -> if (b) i + 1 else i + 2, 1, Array#{} @ rc);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @Test
    def scan02(): Bool = region rc {
        let a = Array.scan(rc, (i, b) -> if (b) i + 1 else i + 2, 1, Array#{false} @ rc);
        Array.sameElements(a, Array#{1,3} @ rc)
    }

    @Test
    def scan03(): Bool = region rc {
        let a = Array.scan(rc, (i, b) -> if (b) i + 1 else i + 2, 1, Array#{true} @ rc);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @Test
    def scan04(): Bool = region rc {
        let a = Array.scan(rc, (i, b) -> if (b) i + 1 else i + 2, 1, Array#{false,false} @ rc);
        Array.sameElements(a, Array#{1,3,5} @ rc)
    }

    @Test
    def scan05(): Bool = region rc {
        let a = Array.scan(rc, (i, b) -> if (b) i + 1 else i + 2, 1, Array#{false,true} @ rc);
        Array.sameElements(a, Array#{1,3,4} @ rc)
    }

    @Test
    def scan06(): Bool = region rc {
        let a = Array.scan(rc, (i, b) -> if (b) i + 1 else i + 2, 1, Array#{true,false} @ rc);
        Array.sameElements(a, Array#{1,2,4} @ rc)
    }

    @Test
    def scan07(): Bool = region rc {
        let a = Array.scan(rc, (i, b) -> if (b) i + 1 else i + 2, 1, Array#{true,true} @ rc);
        Array.sameElements(a, Array#{1,2,3} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // scanLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def scanLeft01(): Bool = region rc {
        let a = Array.scanLeft(rc, (i, b) -> if (b) i + 1 else i + 2, 1, (Array#{} @ rc): Array[Bool, _]);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @Test
    def scanLeft02(): Bool = region rc {
        let a = Array.scanLeft(rc, (i, b) -> if (b) i + 1 else i + 2, 1, Array#{false} @ rc);
        Array.sameElements(a, Array#{1,3} @ rc)
    }

    @Test
    def scanLeft03(): Bool = region rc {
        let a = Array.scanLeft(rc, (i, b) -> if (b) i + 1 else i + 2, 1, Array#{true} @ rc);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @Test
    def scanLeft04(): Bool = region rc {
        let a = Array.scanLeft(rc, (i, b) -> if (b) i + 1 else i + 2, 1, Array#{false,false} @ rc);
        Array.sameElements(a, Array#{1,3,5} @ rc)
    }

    @Test
    def scanLeft05(): Bool = region rc {
        let a = Array.scanLeft(rc, (i, b) -> if (b) i + 1 else i + 2, 1, Array#{false,true} @ rc);
        Array.sameElements(a, Array#{1,3,4} @ rc)
    }

    @Test
    def scanLeft06(): Bool = region rc {
        let a = Array.scanLeft(rc, (i, b) -> if (b) i + 1 else i + 2, 1, Array#{true,false} @ rc);
        Array.sameElements(a, Array#{1,2,4} @ rc)
    }

    @Test
    def scanLeft07(): Bool = region rc {
        let a = Array.scanLeft(rc, (i, b) -> if (b) i + 1 else i + 2, 1, Array#{true,true} @ rc);
        Array.sameElements(a, Array#{1,2,3} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // scanRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def scanRight01(): Bool = region rc {
        let a = Array.scanRight(rc, (b, i) -> if (b) i + 1 else i + 2, 1, Array#{} @ rc);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @Test
    def scanRight02(): Bool = region rc {
        let a = Array.scanRight(rc, (b, i) -> if (b) i + 1 else i + 2, 1, Array#{false} @ rc);
        Array.sameElements(a, Array#{3,1} @ rc)
    }

    @Test
    def scanRight03(): Bool = region rc {
        let a = Array.scanRight(rc, (b, i) -> if (b) i + 1 else i + 2, 1, Array#{true} @ rc);
        Array.sameElements(a, Array#{2,1} @ rc)
    }

    @Test
    def scanRight04(): Bool = region rc {
        let a = Array.scanRight(rc, (b, i) -> if (b) i + 1 else i + 2, 1, Array#{false,false} @ rc);
        Array.sameElements(a, Array#{5,3,1} @ rc)
    }

    @Test
    def scanRight05(): Bool = region rc {
        let a = Array.scanRight(rc, (b, i) -> if (b) i + 1 else i + 2, 1, Array#{false,true} @ rc);
        Array.sameElements(a, Array#{4,2,1} @ rc)
    }

    @Test
    def scanRight06(): Bool = region rc {
        let a = Array.scanRight(rc, (b, i) -> if (b) i + 1 else i + 2, 1, Array#{true,false} @ rc);
        Array.sameElements(a, Array#{4,3,1} @ rc)
    }

    @Test
    def scanRight07(): Bool = region rc {
        let a = Array.scanRight(rc, (b, i) -> if (b) i + 1 else i + 2, 1, Array#{true,true} @ rc);
        Array.sameElements(a, Array#{3,2,1} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // map                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def map01(): Bool = region rc {
        let a = Array.map(rc, i -> i > 2, Array#{} @ rc);
        Array.sameElements(a, (Array#{} @ rc): Array[Bool, _])
    }

    @Test
    def map02(): Bool = region rc {
        let a = Array.map(rc, i -> i > 2, Array#{1} @ rc);
        Array.sameElements(a, Array#{false} @ rc)
    }

    @Test
    def map03(): Bool = region rc {
        let a = Array.map(rc, i -> i > 2, Array#{3} @ rc);
        Array.sameElements(a, Array#{true} @ rc)
    }

    @Test
    def map04(): Bool = region rc {
        let a = Array.map(rc, i -> i > 2, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{false,false} @ rc)
    }

    @Test
    def map05(): Bool = region rc {
        let a = Array.map(rc, i -> i > 2, Array#{1,8} @ rc);
        Array.sameElements(a, Array#{false, true} @ rc)
    }

    @Test
    def map06(): Bool = region rc {
        let a = Array.map(rc, i -> i > 2, Array#{8,1} @ rc);
        Array.sameElements(a, Array#{true,false} @ rc)
    }

    @Test
    def map07(): Bool = region rc {
        let a = Array.map(rc, i -> i > 2, Array#{7,8} @ rc);
        Array.sameElements(a, Array#{true,true} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // transform!                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def transform01!(): Bool = region rc {
        let a: Array[Int32, _] = Array#{} @ rc;
        Array.transform!(x -> x+1, a);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def transform02!(): Bool = region rc {
        let a = Array#{0} @ rc;
        Array.transform!(x -> x+1, a);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @Test
    def transform03!(): Bool = region rc {
        let a = Array#{0,1} @ rc;
        Array.transform!(x -> x+1, a);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @Test
    def transform04!(): Bool = region rc {
        let a = Array#{0,1,2} @ rc;
        Array.transform!(x -> x+1, a);
        Array.sameElements(a, Array#{1,2,3} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // mapWithIndex                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def mapWithIndex01(): Bool = region rc {
        let a = Array.mapWithIndex(rc, (i, e) -> if (i < 1) e > 2 else e <= 2, Array#{} @ rc);
        Array.sameElements(a, (Array#{} @ rc): Array[Bool, _])
    }

    @Test
    def mapWithIndex02(): Bool = region rc {
        let a = Array.mapWithIndex(rc, (i, e) -> if (i < 1) e > 2 else e <= 2, Array#{1} @ rc);
        Array.sameElements(a, Array#{false} @ rc)
    }

    @Test
    def mapWithIndex03(): Bool = region rc {
        let a = Array.mapWithIndex(rc, (i, e) -> if (i < 1) e > 2 else e <= 2, Array#{3} @ rc);
        Array.sameElements(a, Array#{true} @ rc)
    }

    @Test
    def mapWithIndex04(): Bool = region rc {
        let a = Array.mapWithIndex(rc, (i, e) -> if (i < 1) e > 2 else e <= 2, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{false,true} @ rc)
    }

    @Test
    def mapWithIndex05(): Bool = region rc {
        let a = Array.mapWithIndex(rc, (i, e) -> if (i < 1) e > 2 else e <= 2, Array#{1,8} @ rc);
        Array.sameElements(a, Array#{false,false} @ rc)
    }

    @Test
    def mapWithIndex06(): Bool = region rc {
        let a = Array.mapWithIndex(rc, (i, e) -> if (i < 1) e > 2 else e <= 2, Array#{8,1} @ rc);
        Array.sameElements(a, Array#{true,true} @ rc)
    }

    @Test
    def mapWithIndex07(): Bool = region rc {
        let a = Array.mapWithIndex(rc, (i, e) -> if (i < 1) e > 2 else e <= 2, Array#{7,8} @ rc);
        Array.sameElements(a, Array#{true,false} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // transformWithIndex!                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def transformWithIndex01!(): Bool = region rc {
        let a = (Array#{} @ rc): Array[Int32, _];
        Array.transformWithIndex!((i, e) -> if (i < 1) e + 10 else e, a);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def transformWithIndex02!(): Bool = region rc {
        let a = Array#{1} @ rc;
        Array.transformWithIndex!((i, e) -> if (i < 1) e + 10 else e, a);
        Array.sameElements(a, Array#{11} @ rc)
    }

    @Test
    def transformWithIndex03!(): Bool = region rc {
        let a = Array#{3} @ rc;
        Array.transformWithIndex!((i, e) -> if (i < 1) e + 10 else e, a);
        Array.sameElements(a, Array#{13} @ rc)
    }

    @Test
    def transformWithIndex04!(): Bool = region rc {
        let a = Array#{1,2} @ rc;
        Array.transformWithIndex!((i, e) -> if (i < 1) e + 10 else e, a);
        Array.sameElements(a, Array#{11,2} @ rc)
    }

    @Test
    def transformWithIndex05!(): Bool = region rc {
        let a = Array#{1,8} @ rc;
        Array.transformWithIndex!((i, e) -> if (i < 1) e + 10 else e, a);
        Array.sameElements(a, Array#{11,8} @ rc)
    }

    @Test
    def transformWithIndex06!(): Bool = region rc {
        let a = Array#{8,1} @ rc;
        Array.transformWithIndex!((i, e) -> if (i < 1) e + 10 else e, a);
        Array.sameElements(a, Array#{18,1} @ rc)
    }

    @Test
    def transformWithIndex07!(): Bool = region rc {
        let a = Array#{7,8,9} @ rc;
        Array.transformWithIndex!((i, e) -> if (i < 1) e + 10 else e, a);
        Array.sameElements(a, Array#{17,8,9} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // flatMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    // depends on flatten

    @Test
    def flatMap01(): Bool = region rc {
        let a = Array.flatMap(rc, i -> Array.repeat(rc, i, i), Array#{} @ rc);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def flatMap02(): Bool = region rc {
        let a = Array.flatMap(rc, i -> Array.repeat(rc, i, i), Array#{0} @ rc);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def flatMap03(): Bool = region rc {
        let a = Array.flatMap(rc, i -> Array.repeat(rc, i, i), Array#{1} @ rc);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @Test
    def flatMap04(): Bool = region rc {
        let a = Array.flatMap(rc, i -> Array.repeat(rc, i, i), Array#{2} @ rc);
        Array.sameElements(a, Array#{2,2} @ rc)
    }

    @Test
    def flatMap05(): Bool = region rc {
        let a = Array.flatMap(rc, i -> Array.repeat(rc, i, i), Array#{1,2} @ rc);
        Array.sameElements(a, Array#{1,2,2} @ rc)
    }

    @Test
    def flatMap06(): Bool = region rc {
        let a = Array.flatMap(rc, i -> Array.repeat(rc, i, i), Array#{2,3} @ rc);
        Array.sameElements(a, Array#{2,2,3,3,3} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // reverse                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def reverse01(): Bool = region rc {
        let a = Array.reverse(rc, (Array#{} @ rc): Array[Int32, _]);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def reverse02(): Bool = region rc {
        let a = Array.reverse(rc, Array#{1} @ rc);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @Test
    def reverse03(): Bool = region rc {
        let a = Array.reverse(rc, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{2,1} @ rc)
    }

    @Test
    def reverse04(): Bool = region rc {
        let a = Array.reverse(rc, Array#{1,1} @ rc);
        Array.sameElements(a, Array#{1,1} @ rc)
    }

    @Test
    def reverse05(): Bool = region rc {
        let a = Array.reverse(rc, Array#{1,2,3} @ rc);
        Array.sameElements(a, Array#{3,2,1} @ rc)
    }

    @Test
    def reverse06(): Bool = region rc {
        let a = Array.reverse(rc, Array#{1,2,3,4} @ rc);
        Array.sameElements(a, Array#{4,3,2,1} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // reverse!                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def reverse01!(): Bool = region rc {
        let a: Array[Int32, _] = Array#{} @ rc;
        Array.reverse!(a);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def reverse02!(): Bool = region rc {
        let a = Array#{1} @ rc;
        Array.reverse!(a);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @Test
    def reverse03!(): Bool = region rc {
        let a = Array#{1,2} @ rc;
        Array.reverse!(a);
        Array.sameElements(a, Array#{2,1} @ rc)
    }

    @Test
    def reverse04!(): Bool = region rc {
        let a = Array#{1,1} @ rc;
        Array.reverse!(a);
        Array.sameElements(a, Array#{1,1} @ rc)
    }

    @Test
    def reverse05!(): Bool = region rc {
        let a = Array#{1,2,3} @ rc;
        Array.reverse!(a);
        Array.sameElements(a, Array#{3,2,1} @ rc)
    }

    @Test
    def reverse06!(): Bool = region rc {
        let a = Array#{1,2,3,4} @ rc;
        Array.reverse!(a);
        Array.sameElements(a, Array#{4,3,2,1} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // rotateLeft                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def rotateLeft01(): Bool = region rc {
        let a = Array.rotateLeft(rc, 0, (Array#{} @ rc): Array[Int32, _]);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def rotateLeft02(): Bool = region rc {
        let a = Array.rotateLeft(rc, 1, (Array#{} @ rc): Array[Int32, _]);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def rotateLeft03(): Bool = region rc {
        let a = Array.rotateLeft(rc, 0, Array#{1} @ rc);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @Test
    def rotateLeft04(): Bool = region rc {
        let a = Array.rotateLeft(rc, 0, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @Test
    def rotateLeft05(): Bool = region rc {
        let a = Array.rotateLeft(rc, 1, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{2,1} @ rc)
    }

    @Test
    def rotateLeft06(): Bool = region rc {
        let a = Array.rotateLeft(rc, 2, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @Test
    def rotateLeft07(): Bool = region rc {
        let a = Array.rotateLeft(rc, 3, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{2,1} @ rc)
    }

    @Test
    def rotateLeft08(): Bool = region rc {
        let a = Array.rotateLeft(rc, -1, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{2,1} @ rc)
    }

    @Test
    def rotateLeft09(): Bool = region rc {
        let a = Array.rotateLeft(rc, 0, Array#{1,2,3} @ rc);
        Array.sameElements(a, Array#{1,2,3} @ rc)
    }

    @Test
    def rotateLeft10(): Bool = region rc {
        let a = Array.rotateLeft(rc, 1, Array#{1,2,3} @ rc);
        Array.sameElements(a, Array#{2,3,1} @ rc)
    }

    @Test
    def rotateLeft11(): Bool = region rc {
        let a = Array.rotateLeft(rc, 2, Array#{1,2,3} @ rc);
        Array.sameElements(a, Array#{3,1,2} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // rotateRight                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def rotateRight01(): Bool = region rc {
        let a = Array.rotateRight(rc, 0, (Array#{} @ rc): Array[Int32, _]);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def rotateRight02(): Bool = region rc {
        let a = Array.rotateRight(rc, 1, (Array#{} @ rc): Array[Int32, _]);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def rotateRight03(): Bool = region rc {
        let a = Array.rotateRight(rc, 0, Array#{1} @ rc);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @Test
    def rotateRight04(): Bool = region rc {
        let a = Array.rotateRight(rc, 0, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @Test
    def rotateRight05(): Bool = region rc {
        let a = Array.rotateRight(rc, 1, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{2,1} @ rc)
    }

    @Test
    def rotateRight06(): Bool = region rc {
        let a = Array.rotateRight(rc, 2, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @Test
    def rotateRight07(): Bool = region rc {
        let a = Array.rotateRight(rc, 3, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{2,1} @ rc)
    }

    @Test
    def rotateRight08(): Bool = region rc {
        let a = Array.rotateRight(rc, -1, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{2,1} @ rc)
    }

    @Test
    def rotateRight09(): Bool = region rc {
        let a = Array.rotateRight(rc, 0, Array#{1,2,3} @ rc);
        Array.sameElements(a, Array#{1,2,3} @ rc)
    }

    @Test
    def rotateRight10(): Bool = region rc {
        let a = Array.rotateRight(rc, 1, Array#{1,2,3} @ rc);
        Array.sameElements(a, Array#{3,1,2} @ rc)
    }

    @Test
    def rotateRight11(): Bool = region rc {
        let a = Array.rotateRight(rc, 2, Array#{1,2,3} @ rc);
        Array.sameElements(a, Array#{2,3,1} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // update                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def update01(): Bool = region rc {
        let a = Array.update(rc, 0, 2, (Array#{} @ rc): Array[Int32, _]);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def update02(): Bool = region rc {
        let a = Array.update(rc, -1, 2, Array#{1} @ rc);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @Test
    def update03(): Bool = region rc {
        let a = Array.update(rc, 0, 2, Array#{1} @ rc);
        Array.sameElements(a, Array#{2} @ rc)
    }

    @Test
    def update04(): Bool = region rc {
        let a = Array.update(rc, 1, 2, Array#{1} @ rc);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @Test
    def update05(): Bool = region rc {
        let a = Array.update(rc, 0, 5, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{5,2} @ rc)
    }

    @Test
    def update06(): Bool = region rc {
        let a = Array.update(rc, 1, 5, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{1,5} @ rc)
    }

    @Test
    def update07(): Bool = region rc {
        let a = Array.update(rc, 2, 5, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // replace                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def replace01(): Bool = region rc {
        let a = Array.replace(rc, from = 3, to = 4, Array#{} @ rc);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def replace02(): Bool = region rc {
        let a = Array.replace(rc, from = 3, to = 4, Array#{1} @ rc);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @Test
    def replace03(): Bool = region rc {
        let a = Array.replace(rc, from = 3, to = 4, Array#{3} @ rc);
        Array.sameElements(a, Array#{4} @ rc)
    }

    @Test
    def replace04(): Bool = region rc {
        let a = Array.replace(rc, from = 3, to = 4, Array#{4} @ rc);
        Array.sameElements(a, Array#{4} @ rc)
    }

    @Test
    def replace05(): Bool = region rc {
        let a = Array.replace(rc, from = 3, to = 4, Array#{1, 2} @ rc);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @Test
    def replace06(): Bool = region rc {
        let a = Array.replace(rc, from = 3, to = 4, Array#{1,3} @ rc);
        Array.sameElements(a, Array#{1,4} @ rc)
    }

    @Test
    def replace07(): Bool = region rc {
        let a = Array.replace(rc, from = 3, to = 4, Array#{3,4} @ rc);
        Array.sameElements(a, Array#{4,4} @ rc)
    }

    @Test
    def replace08(): Bool = region rc {
        let a = Array.replace(rc, from = 3, to = 4, Array#{3,3} @ rc);
        Array.sameElements(a, Array#{4,4} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // replace!                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def replace01!(): Bool = region rc {
        let a: Array[Int32, _] = Array#{} @ rc;
        Array.replace!(from = 3, to = 4, a);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def replace02!(): Bool = region rc {
        let a = Array#{1} @ rc;
        Array.replace!(from = 3, to = 4, a);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @Test
    def replace03!(): Bool = region rc {
        let a = Array#{3} @ rc;
        Array.replace!(from = 3, to = 4, a);
        Array.sameElements(a, Array#{4} @ rc)
    }

    @Test
    def replace04!(): Bool = region rc {
        let a = Array#{4} @ rc;
        Array.replace!(from = 3, to = 4, a);
        Array.sameElements(a, Array#{4} @ rc)
    }

    @Test
    def replace05!(): Bool = region rc {
        let a = Array#{1, 2} @ rc;
        Array.replace!(from = 3, to = 4, a);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @Test
    def replace06!(): Bool = region rc {
        let a = Array#{1,3} @ rc;
        Array.replace!(from = 3, to = 4, a);
        Array.sameElements(a, Array#{1,4} @ rc)
    }

    @Test
    def replace07!(): Bool = region rc {
        let a = Array#{3,4} @ rc;
        Array.replace!(from = 3, to = 4, a);
        Array.sameElements(a, Array#{4,4} @ rc)
    }

    @Test
    def replace08!(): Bool = region rc {
        let a = Array#{3,3} @ rc;
        Array.replace!(from = 3, to = 4, a);
        Array.sameElements(a, Array#{4,4} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // patch                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def patch01(): Bool = region rc {
        let a = Array.patch(rc, 0, 0, (Array#{} @ rc): Array[Int32, _], (Array#{} @ rc): Array[Int32, _]);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def patch02(): Bool = region rc {
        let a = Array.patch(rc, 0, 2, Array#{1,2} @ rc, Array#{} @ rc);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def patch03(): Bool = region rc {
        let a = Array.patch(rc, 0, 2, Array#{} @ rc, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @Test
    def patch04(): Bool = region rc {
        let a = Array.patch(rc, -3, 3, Array#{1,2,4} @ rc, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @Test
    def patch05(): Bool = region rc {
        let a = Array.patch(rc, 2, 3, Array#{1,2,4} @ rc, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @Test
    def patch06(): Bool = region rc {
        let a = Array.patch(rc, 0, 0, Array#{} @ rc, Array#{1} @ rc);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @Test
    def patch07(): Bool = region rc {
        let a = Array.patch(rc, 1, 0, Array#{2} @ rc, Array#{1} @ rc);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @Test
    def patch08(): Bool = region rc {
        let a = Array.patch(rc, 0, 1, Array#{2} @ rc, Array#{1} @ rc);
        Array.sameElements(a, Array#{2} @ rc)
    }

    @Test
    def patch09(): Bool = region rc {
        let a = Array.patch(rc, 0, 2, Array#{2,4} @ rc, Array#{1} @ rc);
        Array.sameElements(a, Array#{2} @ rc)
    }

    @Test
    def patch10(): Bool = region rc {
        let a = Array.patch(rc, -1, 2, Array#{2,4} @ rc, Array#{1} @ rc);
        Array.sameElements(a, Array#{4} @ rc)
    }

    @Test
    def patch11(): Bool = region rc {
        let a = Array.patch(rc, -1, 2, Array#{3,4} @ rc, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{4,2} @ rc)
    }

    @Test
    def patch12(): Bool = region rc {
        let a = Array.patch(rc, 1, 2, Array#{3,4} @ rc, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{1,3} @ rc)
    }

    @Test
    def patch13(): Bool = region rc {
        let a = Array.patch(rc, -2, 2, Array#{3,4} @ rc, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @Test
    def patch14(): Bool = region rc {
        let a = Array.patch(rc, 2, 2, Array#{3,4} @ rc, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @Test
    def patch15(): Bool = region rc {
        let a = Array.patch(rc, 1, 1, Array#{3} @ rc, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{1,3} @ rc)
    }

    @Test
    def patch16(): Bool = region rc {
        let a = Array.patch(rc, 0, 2, Array#{3,4} @ rc, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{3,4} @ rc)
    }

    @Test
    def patch17(): Bool = region rc {
        let a = Array.patch(rc, 0, 1, Array#{4} @ rc, Array#{1, 2, 3} @ rc);
        Array.sameElements(a, Array#{4, 2, 3} @ rc)
    }

    @Test
    def patch18(): Bool = region rc {
        let a = Array.patch(rc, 1, 1, Array#{4} @ rc, Array#{1, 2, 3} @ rc);
        Array.sameElements(a, Array#{1, 4, 3} @ rc)
    }

    @Test
    def patch19(): Bool = region rc {
        let a = Array.patch(rc, 2, 1, Array#{4} @ rc, Array#{1, 2, 3} @ rc);
        Array.sameElements(a, Array#{1, 2, 4} @ rc)
    }

    @Test
    def patch20(): Bool = region rc {
        let a = Array.patch(rc, 0, 2, Array#{4, 5} @ rc, Array#{1, 2, 3} @ rc);
        Array.sameElements(a, Array#{4, 5, 3} @ rc)
    }

    @Test
    def patch21(): Bool = region rc {
        let a = Array.patch(rc, 1, 2, Array#{4, 5} @ rc, Array#{1, 2, 3} @ rc);
        Array.sameElements(a, Array#{1, 4, 5} @ rc)
    }

    @Test
    def patch22(): Bool = region rc {
        let a = Array.patch(rc, 0, 2, Array#{4, 5, 6} @ rc, Array#{1, 2, 3} @ rc);
        Array.sameElements(a, Array#{4, 5, 3} @ rc)
    }

    @Test
    def patch23(): Bool = region rc {
        let a = Array.patch(rc, 0, 3, Array#{4, 5, 6} @ rc, Array#{1, 2, 3} @ rc);
        Array.sameElements(a, Array#{4, 5, 6} @ rc)
    }

    @Test
    def patch24(): Bool = region rc {
        let a = Array.patch(rc, 2, 4, Array#{14, 15, 16, 17} @ rc, Array#{1, 2, 3, 4, 5, 6, 7} @ rc);
        Array.sameElements(a, Array#{1, 2, 14, 15, 16, 17, 7} @ rc)
    }

    @Test
    def patch25(): Bool = region rc {
        let a = Array.patch(rc, -2, 4, Array#{14, 15, 16, 17} @ rc, Array#{1, 2, 3, 4, 5, 6, 7} @ rc);
        Array.sameElements(a, Array#{16, 17, 3, 4, 5, 6, 7} @ rc)
    }

    @Test
    def patch26(): Bool = region rc {
        let a = Array.patch(rc, 4, 5, Array#{14, 15, 16, 17} @ rc, Array#{1, 2, 3, 4, 5, 6, 7} @ rc);
        Array.sameElements(a, Array#{1, 2, 3, 4, 14, 15, 16} @ rc)
    }

    @Test
    def patch27(): Bool = region rc {
        let a = Array.patch(rc, 4, 2, Array#{14, 15, 16, 17} @ rc, Array#{1, 2, 3, 4, 5, 6, 7} @ rc);
        Array.sameElements(a, Array#{1, 2, 3, 4, 14, 15, 7} @ rc)
    }

    @Test
    def patch28(): Bool = region rc {
        let a = Array.patch(rc, -1, 10, Array#{-1, -2, -3, -4, -5, -6, -7, -8} @ rc, Array#{1, 2, 3, 4, 5, 6, 7} @ rc);
        Array.sameElements(a, Array#{-2, -3, -4, -5, -6, -7, -8} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // patch!                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def patch01!(): Bool = region rc {
        let a = (Array#{} @ rc): Array[Int32, _];
        Array.patch!(0, 0, (Array#{} @ rc): Array[Int32, _], a);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def patch02!(): Bool = region rc {
        let a = (Array#{} @ rc): Array[Int32, _];
        Array.patch!(0, 2, Array#{1,2} @ rc, a);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def patch03!(): Bool = region rc {
        let a = Array#{1,2} @ rc;
        Array.patch!(0, 2, Array#{} @ rc, a);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @Test
    def patch04!(): Bool = region rc {
        let a = Array#{1,2} @ rc;
        Array.patch!(-3, 3, Array#{1,2,4} @ rc, a);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @Test
    def patch05!(): Bool = region rc {
        let a = Array#{1,2} @ rc;
        Array.patch!(2, 3, Array#{1,2,4} @ rc, a);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @Test
    def patch06!(): Bool = region rc {
        let a = Array#{1} @ rc;
        Array.patch!(0, 0, Array#{} @ rc, a);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @Test
    def patch07!(): Bool = region rc {
        let a = Array#{1} @ rc;
        Array.patch!(1, 0, Array#{2} @ rc, a);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @Test
    def patch08!(): Bool = region rc {
        let a = Array#{1} @ rc;
        Array.patch!(0, 1, Array#{2} @ rc, a);
        Array.sameElements(a, Array#{2} @ rc)
    }

    @Test
    def patch09!(): Bool = region rc {
        let a = Array#{1} @ rc;
        Array.patch!(0, 2, Array#{2,4} @ rc, a);
        Array.sameElements(a, Array#{2} @ rc)
    }

    @Test
    def patch10!(): Bool = region rc {
        let a = Array#{1} @ rc;
        Array.patch!(-1, 2, Array#{2,4} @ rc, a);
        Array.sameElements(a, Array#{4} @ rc)
    }

    @Test
    def patch11!(): Bool = region rc {
        let a = Array#{1,2} @ rc;
        Array.patch!(-1, 2, Array#{3,4} @ rc, a);
        Array.sameElements(a, Array#{4,2} @ rc)
    }

    @Test
    def patch12!(): Bool = region rc {
        let a = Array#{1,2} @ rc;
        Array.patch!(1, 2, Array#{3,4} @ rc, a);
        Array.sameElements(a, Array#{1,3} @ rc)
    }

    @Test
    def patch13!(): Bool = region rc {
        let a = Array#{1,2} @ rc;
        Array.patch!(-2, 2, Array#{3,4} @ rc, a);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @Test
    def patch14!(): Bool = region rc {
        let a = Array#{1,2} @ rc;
        Array.patch!(2, 2, Array#{3,4} @ rc, a);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @Test
    def patch15!(): Bool = region rc {
        let a = Array#{1,2} @ rc;
        Array.patch!(1, 1, Array#{3} @ rc, a);
        Array.sameElements(a, Array#{1,3} @ rc)
    }

    @Test
    def patch16!(): Bool = region rc {
        let a = Array#{1,2} @ rc;
        Array.patch!(0, 2, Array#{3,4} @ rc, a);
        Array.sameElements(a, Array#{3,4} @ rc)
    }

    @Test
    def patch17!(): Bool = region rc {
        let a = Array#{1, 2, 3} @ rc;
        Array.patch!(0, 1, Array#{4} @ rc, a);
        Array.sameElements(a, Array#{4, 2, 3} @ rc)
    }

    @Test
    def patch18!(): Bool = region rc {
        let a = Array#{1, 2, 3} @ rc;
        Array.patch!(1, 1, Array#{4} @ rc, a);
        Array.sameElements(a, Array#{1, 4, 3} @ rc)
    }

    @Test
    def patch19!(): Bool = region rc {
        let a = Array#{1, 2, 3} @ rc;
        Array.patch!(2, 1, Array#{4} @ rc, a);
        Array.sameElements(a, Array#{1, 2, 4} @ rc)
    }

    @Test
    def patch20!(): Bool = region rc {
        let a = Array#{1, 2, 3} @ rc;
        Array.patch!(0, 2, Array#{4, 5} @ rc, a);
        Array.sameElements(a, Array#{4, 5, 3} @ rc)
    }

    @Test
    def patch21!(): Bool = region rc {
        let a = Array#{1, 2, 3} @ rc;
        Array.patch!(1, 2, Array#{4, 5} @ rc, a);
        Array.sameElements(a, Array#{1, 4, 5} @ rc)
    }

    @Test
    def patch22!(): Bool = region rc {
        let a = Array#{1, 2, 3} @ rc;
        Array.patch!(0, 2, Array#{4, 5, 6} @ rc, a);
        Array.sameElements(a, Array#{4, 5, 3} @ rc)
    }

    @Test
    def patch23!(): Bool = region rc {
        let a = Array#{1, 2, 3} @ rc;
        Array.patch!(0, 3, Array#{4, 5, 6} @ rc, a);
        Array.sameElements(a, Array#{4, 5, 6} @ rc)
    }

    @Test
    def patch24!(): Bool = region rc {
        let a = Array#{1, 2, 3, 4, 5, 6, 7} @ rc;
        Array.patch!(2, 4, Array#{14, 15, 16, 17} @ rc, a);
        Array.sameElements(a, Array#{1, 2, 14, 15, 16, 17, 7} @ rc)
    }

    @Test
    def patch25!(): Bool = region rc {
        let a = Array#{1, 2, 3, 4, 5, 6, 7} @ rc;
        Array.patch!(-2, 4, Array#{14, 15, 16, 17} @ rc, a);
        Array.sameElements(a, Array#{16, 17, 3, 4, 5, 6, 7} @ rc)
    }

    @Test
    def patch26!(): Bool = region rc {
        let a = Array#{1, 2, 3, 4, 5, 6, 7} @ rc;
        Array.patch!(4, 5, Array#{14, 15, 16, 17} @ rc, a);
        Array.sameElements(a, Array#{1, 2, 3, 4, 14, 15, 16} @ rc)
    }

    @Test
    def patch27!(): Bool = region rc {
        let a = Array#{1, 2, 3, 4, 5, 6, 7} @ rc;
        Array.patch!(4, 2, Array#{14, 15, 16, 17} @ rc, a);
        Array.sameElements(a, Array#{1, 2, 3, 4, 14, 15, 7} @ rc)
    }

    @Test
    def patch28!(): Bool = region rc {
        let a = Array#{1, 2, 3, 4, 5, 6, 7} @ rc;
        Array.patch!(-1, 10, Array#{-1, -2, -3, -4, -5, -6, -7, -8} @ rc, a);
        Array.sameElements(a, Array#{-2, -3, -4, -5, -6, -7, -8} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // intersperse                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def intersperse01(): Bool = region rc {
        let a = Array.intersperse(rc, 11, Array#{} @ rc);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def intersperse02(): Bool = region rc {
        let a = Array.intersperse(rc, 11, Array#{1} @ rc);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @Test
    def intersperse03(): Bool = region rc {
        let a = Array.intersperse(rc, 11, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{1,11,2} @ rc)
    }

    @Test
    def intersperse04(): Bool = region rc {
        let a = Array.intersperse(rc, 11, Array#{1,2,3} @ rc);
        Array.sameElements(a, Array#{1,11,2,11,3} @ rc)
    }

    @Test
    def intersperse05(): Bool = region rc {
        let a = Array.intersperse(rc, 11, Array#{1,2,3,4} @ rc);
        Array.sameElements(a, Array#{1,11,2,11,3,11,4} @ rc)
    }


    /////////////////////////////////////////////////////////////////////////////
    // intercalate                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def intercalate01(): Bool = region rc {
        let a = Array.intercalate(rc, (Array#{} @ rc): Array[Int32, rc], (Array#{} @ rc): Array[Array[Int32, rc], rc]);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, rc])
    }

    @Test
    def intercalate02(): Bool = region rc {
        let a = Array.intercalate(rc, (Array#{} @ rc): Array[Int32, rc], Array#{Array#{1} @ rc} @ rc);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @Test
    def intercalate03(): Bool = region rc {
        let a = Array.intercalate(rc, Array#{11,12,13} @ rc, (Array#{} @ rc): Array[Array[Int32, rc], rc]);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, rc])
    }

    @Test
    def intercalate04(): Bool = region rc {
        let a = Array.intercalate(rc, (Array#{} @ rc): Array[Int32, rc], Array#{Array#{1} @ rc, Array#{2,3} @ rc} @ rc);
        Array.sameElements(a, Array#{1,2,3} @ rc)
    }

    @Test
    def intercalate05(): Bool = region rc {
        let a = Array.intercalate(rc, Array#{11,12,13} @ rc, Array#{Array#{1} @ rc, Array#{2,3} @ rc} @ rc);
        Array.sameElements(a, Array#{1,11,12,13,2,3} @ rc)
    }

    @Test
    def intercalate06(): Bool = region rc {
        let a = Array.intercalate(rc, (Array#{} @ rc): Array[Int32, rc], Array#{Array#{1} @ rc, Array#{2,3} @ rc, Array#{4} @ rc} @ rc);
        Array.sameElements(a, Array#{1,2,3,4} @ rc)
    }

    @Test
    def intercalate07(): Bool = region rc {
        let a = Array.intercalate(rc, Array#{11,12,13} @ rc, Array#{Array#{1} @ rc, Array#{2,3} @ rc, Array#{4} @ rc} @ rc);
        Array.sameElements(a, Array#{1,11,12,13,2,3,11,12,13,4} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // transpose                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def transpose01(): Bool = region rc {
        let a = Array.transpose(rc, Array#{} @ rc);
        let b = Array.toList(a) |> List.map(Array.toList);
        b: List[List[Unit]] == Nil
    }

    @Test
    def transpose02(): Bool = region rc {
        let a: Array[Array[Int32, _], _] = Array.transpose(rc, Array#{Array#{} @ rc} @ rc);
        let b = Array.toList(a) |> List.map(Array.toList);
        b == Nil :: Nil
    }

    @Test
    def transpose03(): Bool = region rc {
        let a: Array[Array[Int32, _], _] = Array.transpose(rc, Array#{Array#{} @ rc, Array#{} @ rc} @ rc);
        let b = Array.toList(a) |> List.map(Array.toList);
        b == Nil :: Nil :: Nil
    }

    @Test
    def transpose04(): Bool = region rc {
        let a: Array[Array[Int32, _], _] = Array.transpose(rc, Array#{Array#{} @ rc, Array#{} @ rc, Array#{} @ rc} @ rc);
        let b = Array.toList(a) |> List.map(Array.toList);
        b == Nil :: Nil :: Nil :: Nil
    }

    @Test
    def transpose05(): Bool = region rc {
        let a = Array.transpose(rc, Array#{ Array#{1} @ rc } @ rc);
        let b = Array.toList(a) |> List.map(Array.toList);
        b == (1 :: Nil) :: Nil
    }

    @Test
    def transpose06(): Bool = region rc {
        let a = Array.transpose(rc, Array#{Array#{1,2} @ rc} @ rc);
        let b = Array.toList(a) |> List.map(Array.toList);
        b == (1 :: Nil) :: (2 :: Nil) :: Nil
    }

    @Test
    def transpose07(): Bool = region rc {
        let a = Array.transpose(rc, Array#{Array#{1, 2, 3} @ rc} @ rc);
        let b = Array.toList(a) |> List.map(Array.toList);
        b == (1 :: Nil) :: (2 :: Nil) :: (3 :: Nil) :: Nil
    }

    @Test
    def transpose08(): Bool = region rc {
        let a = Array.transpose(rc, Array#{Array#{1, 2, 3, 4} @ rc} @ rc);
        let b = Array.toList(a) |> List.map(Array.toList);
        b == (1 :: Nil) :: (2 :: Nil) :: (3 :: Nil) :: (4 :: Nil) :: Nil
    }

    @Test
    def transpose09(): Bool = region rc {
        let a = Array.transpose(rc, Array#{Array#{1} @ rc, Array#{2} @ rc} @ rc);
        let b = Array.toList(a) |> List.map(Array.toList);
        b ==   (1 :: 2 :: Nil) :: Nil
    }

    @Test
    def transpose10(): Bool = region rc {
        let a = Array.transpose(rc, Array#{Array#{1} @ rc, Array#{2} @ rc, Array#{3} @ rc} @ rc);
        let b = Array.toList(a) |> List.map(Array.toList);
        b ==  (1 :: 2 :: 3 :: Nil) :: Nil
    }

    @Test
    def transpose11(): Bool = region rc {
        let a = Array.transpose(rc, Array#{Array#{1} @ rc, Array#{2} @ rc, Array#{3} @ rc, Array#{4} @ rc} @ rc);
        let b = Array.toList(a) |> List.map(Array.toList);
        b == (1 :: 2 :: 3 :: 4 :: Nil) :: Nil
    }

    @Test
    def transpose12(): Bool = region rc {
        let a = Array.transpose(rc, Array#{Array#{1, 2} @ rc, Array#{3, 4} @ rc} @ rc);
        let b = Array.toList(a) |> List.map(Array.toList);
        b == (1 :: 3 :: Nil) :: (2 :: 4 :: Nil) :: Nil
    }

    @Test
    def transpose13(): Bool = region rc {
        let a = Array.transpose(rc, Array#{Array#{1, 2, 3} @ rc, Array#{4, 5, 6} @ rc} @ rc);
        let b = Array.toList(a) |> List.map(Array.toList);
        b == (1 :: 4 :: Nil) :: (2 :: 5 :: Nil) :: (3 :: 6 :: Nil) :: Nil
    }

    @Test
    def transpose14(): Bool = region rc {
        let a = Array.transpose(rc, Array#{Array#{1, 2, 3, 4} @ rc, Array#{5, 6, 7, 8} @ rc} @ rc);
        let b = Array.toList(a) |> List.map(Array.toList);
        b == (1 :: 5 :: Nil) :: (2 :: 6 :: Nil) :: (3 :: 7 :: Nil) :: (4 :: 8 :: Nil) :: Nil
    }

    @Test
    def transpose15(): Bool = region rc {
        let a = Array.transpose(rc, Array#{Array#{1, 2, 3, 4, 5} @ rc, Array#{6, 7, 8, 9, 10} @ rc} @ rc);
        let b = Array.toList(a) |> List.map(Array.toList);
        b == (1 :: 6 :: Nil) :: (2 :: 7 :: Nil) :: (3 :: 8 :: Nil) :: (4 :: 9 :: Nil) :: (5 :: 10 :: Nil) :: Nil
    }

    @Test
    def transpose16(): Bool = region rc {
        let a = Array.transpose(rc, Array#{Array#{1, 2} @ rc, Array#{3, 4} @ rc, Array#{5, 6} @ rc} @ rc);
        let b = Array.toList(a) |> List.map(Array.toList);
        b == (1 :: 3 :: 5 :: Nil) :: (2 :: 4 :: 6 :: Nil) :: Nil
    }

    @Test
    def transpose17(): Bool = region rc {
        let a = Array.transpose(rc, Array#{Array#{1, 2} @ rc, Array#{3, 4} @ rc, Array#{5, 6} @ rc, Array#{7,8} @ rc} @ rc);
        let b = Array.toList(a) |> List.map(Array.toList);
        b == (1 :: 3 :: 5 :: 7 :: Nil) :: (2 :: 4 :: 6 :: 8 :: Nil) :: Nil
    }

    @Test
    def transpose18(): Bool = region rc {
        let a = Array.transpose(rc, Array#{Array#{1, 2} @ rc, Array#{3, 4} @ rc, Array#{5, 6} @ rc, Array#{7, 8} @ rc, Array#{9, 10} @ rc} @ rc);
        let b = Array.toList(a) |> List.map(Array.toList);
        b == (1 :: 3 :: 5 :: 7 :: 9 :: Nil) :: (2 :: 4 :: 6 :: 8 :: 10 :: Nil) :: Nil
    }

    @Test
    def transpose19(): Bool = region rc {
        let a = Array.transpose(rc, Array#{Array#{1, 2, 3} @ rc, Array#{4, 5, 6} @ rc, Array#{7, 8, 9} @ rc} @ rc);
        let b = Array.toList(a) |> List.map(Array.toList);
        b == (1 :: 4 :: 7 :: Nil) :: (2 :: 5 :: 8 :: Nil) :: (3 :: 6 :: 9 :: Nil) :: Nil
    }

    @Test
    def transpose20(): Bool = region rc {
        let a = Array.transpose(rc, Array#{Array#{1, 2, 3} @ rc, Array#{4, 5} @ rc, Array#{7, 8, 9} @ rc} @ rc);
        let b = Array.toList(a) |> List.map(Array.toList);
        b == (1 :: 2 :: 3 :: Nil) :: (4 :: 5 :: Nil) :: (7 :: 8 :: 9 :: Nil) :: Nil
    }

    @Test
    def transpose21(): Bool = region rc {
        let a = Array.transpose(rc, Array#{Array#{1, 2, 3} @ rc, Array#{} @ rc, Array#{7, 8, 9} @ rc} @ rc);
        let b = Array.toList(a) |> List.map(Array.toList);
        b == (1 :: 2 :: 3 :: Nil) :: Nil :: (7 :: 8 :: 9 :: Nil) :: Nil
    }

    @Test
    def transpose22(): Bool = region rc {
        let a = Array.transpose(rc, Array#{Array#{1, 2, 3} @ rc, Array#{4, 5, 6} @ rc, Array#{7, 8, 9, 10} @ rc} @ rc);
        let b = Array.toList(a) |> List.map(Array.toList);
        b == (1 :: 2 :: 3 :: Nil) :: (4 :: 5 :: 6 :: Nil) :: (7 :: 8 :: 9 :: 10 :: Nil) :: Nil
    }

    /////////////////////////////////////////////////////////////////////////////
    // isPrefixOf                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isPrefixOf01(): Bool = region rc {
        Array.isPrefixOf((Array#{} @ rc): Array[Int32, _], (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def isPrefixOf02(): Bool = region rc {
        Array.isPrefixOf(Array#{1} @ rc, (Array#{} @ rc): Array[Int32, _]) == false
    }

    @Test
    def isPrefixOf03(): Bool = region rc {
        Array.isPrefixOf((Array#{} @ rc): Array[Int32, _], Array#{1} @ rc)
    }

    @Test
    def isPrefixOf04(): Bool = region rc {
        Array.isPrefixOf(Array#{1} @ rc, Array#{1} @ rc)
    }

    @Test
    def isPrefixOf05(): Bool = region rc {
        Array.isPrefixOf(Array#{2} @ rc, Array#{1} @ rc) == false
    }

    @Test
    def isPrefixOf06(): Bool = region rc {
        Array.isPrefixOf(Array#{1,2} @ rc, Array#{1} @ rc) == false
    }

    @Test
    def isPrefixOf07(): Bool = region rc {
        Array.isPrefixOf((Array#{} @ rc): Array[Int32, _], Array#{1,2} @ rc)
    }

    @Test
    def isPrefixOf08(): Bool = region rc {
        Array.isPrefixOf(Array#{1} @ rc, Array#{1,2} @ rc)
    }

    @Test
    def isPrefixOf09(): Bool = region rc {
        Array.isPrefixOf(Array#{2} @ rc, Array#{1,2} @ rc) == false
    }

    @Test
    def isPrefixOf10(): Bool = region rc {
        Array.isPrefixOf(Array#{1,2} @ rc, Array#{1,2} @ rc)
    }

    @Test
    def isPrefixOf11(): Bool = region rc {
        Array.isPrefixOf(Array#{1,3} @ rc, Array#{1,2} @ rc) == false
    }

    @Test
    def isPrefixOf12(): Bool = region rc {
        Array.isPrefixOf(Array#{1,2,3} @ rc, Array#{1,2} @ rc) == false
    }

    @Test
    def isPrefixOf13(): Bool = region rc {
        Array.isPrefixOf(Array#{1,2,3} @ rc, Array#{89,11,1,2,3,4} @ rc) == false
    }

    /////////////////////////////////////////////////////////////////////////////
    // isInfixOf                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isInfixOf01(): Bool = region rc {
        Array.isInfixOf((Array#{} @ rc): Array[Int32, _], (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def isInfixOf02(): Bool = region rc {
        Array.isInfixOf(Array#{1} @ rc, (Array#{} @ rc): Array[Int32, _]) == false
    }

    @Test
    def isInfixOf03(): Bool = region rc {
        Array.isInfixOf((Array#{} @ rc): Array[Int32, _], Array#{1} @ rc)
    }

    @Test
    def isInfixOf04(): Bool = region rc {
        Array.isInfixOf(Array#{1} @ rc, Array#{1} @ rc)
    }

    @Test
    def isInfixOf05(): Bool = region rc {
        Array.isInfixOf(Array#{2} @ rc, Array#{1} @ rc) == false
    }

    @Test
    def isInfixOf06(): Bool = region rc {
        Array.isInfixOf(Array#{1,2} @ rc, Array#{1} @ rc) == false
    }

    @Test
    def isInfixOf07(): Bool = region rc {
        Array.isInfixOf((Array#{} @ rc): Array[Int32, _], Array#{1,2} @ rc)
    }

    @Test
    def isInfixOf08(): Bool = region rc {
        Array.isInfixOf(Array#{1} @ rc, Array#{1,2} @ rc)
    }

    @Test
    def isInfixOf09(): Bool = region rc {
        Array.isInfixOf(Array#{2} @ rc, Array#{1,2} @ rc)
    }

    @Test
    def isInfixOf10(): Bool = region rc {
        Array.isInfixOf(Array#{1,2} @ rc, Array#{1,2} @ rc)
    }

    @Test
    def isInfixOf11(): Bool = region rc {
        Array.isInfixOf(Array#{1,3} @ rc, Array#{1,2} @ rc) == false
    }

    @Test
    def isInfixOf12(): Bool = region rc {
        Array.isInfixOf(Array#{1,2,3} @ rc, Array#{1,2} @ rc) == false
    }

    @Test
    def isInfixOf13(): Bool = region rc {
        Array.isInfixOf(Array#{1,2,3} @ rc, Array#{89,11,1,2,3,4} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // isSuffixOf                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isSuffixOf01(): Bool = region rc {
        Array.isSuffixOf((Array#{} @ rc): Array[Int32, _], (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def isSuffixOf02(): Bool = region rc {
        Array.isSuffixOf(Array#{1} @ rc, (Array#{} @ rc): Array[Int32, _]) == false
    }

    @Test
    def isSuffixOf03(): Bool = region rc {
        Array.isSuffixOf((Array#{} @ rc): Array[Int32, _], Array#{1} @ rc)
    }

    @Test
    def isSuffixOf04(): Bool = region rc {
        Array.isSuffixOf(Array#{1} @ rc, Array#{1} @ rc)
    }

    @Test
    def isSuffixOf05(): Bool = region rc {
        Array.isSuffixOf(Array#{2} @ rc, Array#{1} @ rc) == false
    }

    @Test
    def isSuffixOf06(): Bool = region rc {
        Array.isSuffixOf(Array#{1,2} @ rc, Array#{1} @ rc) == false
    }

    @Test
    def isSuffixOf07(): Bool = region rc {
        Array.isSuffixOf((Array#{} @ rc): Array[Int32, _], Array#{1,2} @ rc)
    }

    @Test
    def isSuffixOf08(): Bool = region rc {
        Array.isSuffixOf(Array#{1} @ rc, Array#{1,2} @ rc) == false
    }

    @Test
    def isSuffixOf09(): Bool = region rc {
        Array.isSuffixOf(Array#{2} @ rc, Array#{1,2} @ rc)
    }

    @Test
    def isSuffixOf10(): Bool = region rc {
        Array.isSuffixOf(Array#{1,2} @ rc, Array#{1,2} @ rc)
    }

    @Test
    def isSuffixOf11(): Bool = region rc {
        Array.isSuffixOf(Array#{1,3} @ rc, Array#{1,2} @ rc) == false
    }

    @Test
    def isSuffixOf12(): Bool = region rc {
        Array.isSuffixOf(Array#{1,2,3} @ rc, Array#{1,2} @ rc) == false
    }

    @Test
    def isSuffixOf13(): Bool = region rc {
        Array.isSuffixOf(Array#{1,2,3} @ rc, Array#{89,11,1,2,3} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // eq                                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testSameElements01(): Bool = region rc {
        ((Array#{} @ rc): Array[Int32, _]) `sameElements` ((Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def testSameElements02(): Bool = region rc {
        not ((Array#{1} @ rc) `sameElements` Array#{} @ rc)
    }

    @Test
    def testSameElements03(): Bool = region rc {
        not ((Array#{} @ rc) `sameElements` Array#{1} @ rc)
    }

    @Test
    def testSameElements04(): Bool = region rc {
        (Array#{1} @ rc) `sameElements` Array#{1} @ rc
    }

    @Test
    def testSameElements05(): Bool = region rc {
        not ((Array#{1} @ rc) `sameElements` Array#{2} @ rc)
    }

    @Test
    def testSameElements06(): Bool = region rc {
        not ((Array#{1,2} @ rc) `sameElements` Array#{1} @ rc)
    }

    @Test
    def testSameElements07(): Bool = region rc {
        not ((Array#{1} @ rc) `sameElements` Array#{1,2} @ rc)
    }

    @Test
    def testSameElements08(): Bool = region rc {
        (Array#{1,2,3,4} @ rc) `sameElements` Array#{1,2,3,4} @ rc
    }

    /////////////////////////////////////////////////////////////////////////////
    // compare                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testCompare01(): Bool = region rc {
        ((Array#{} @ rc): Array[Int32, _]) `Array.compare` ((Array#{} @ rc): Array[Int32, _]) == Comparison.EqualTo
    }

    @Test
    def testCompare02(): Bool = region rc {
        ((Array#{1} @ rc) `Array.compare` (Array#{} @ rc)) == Comparison.GreaterThan
    }

    @Test
    def testCompare03(): Bool = region rc {
        ((Array#{} @ rc) `Array.compare` (Array#{1} @ rc)) == Comparison.LessThan
    }

    @Test
    def testCompare04(): Bool = region rc {
        ((Array#{1} @ rc) `Array.compare` (Array#{1} @ rc)) == Comparison.EqualTo
    }

    @Test
    def testCompare05(): Bool = region rc {
        ((Array#{1} @ rc) `Array.compare` (Array#{2} @ rc)) == Comparison.LessThan
    }

    @Test
    def testCompare06(): Bool = region rc {
        ((Array#{2} @ rc) `Array.compare` (Array#{1} @ rc)) == Comparison.GreaterThan
    }

    @Test
    def testCompare07(): Bool = region rc {
        ((Array#{1} @ rc) `Array.compare` (Array#{1,2} @ rc)) == Comparison.LessThan
    }

    @Test
    def testCompare08(): Bool = region rc {
        ((Array#{1,2} @ rc) `Array.compare` (Array#{1} @ rc)) == Comparison.GreaterThan
    }

    @Test
    def testCompare09(): Bool = region rc {
        ((Array#{1} @ rc) `Array.compare` (Array#{0,0} @ rc)) == Comparison.GreaterThan
    }

    @Test
    def testCompare10(): Bool = region rc {
        ((Array#{0,0} @ rc) `Array.compare` (Array#{1} @ rc)) == Comparison.LessThan
    }

    @Test
    def testCompare11(): Bool = region rc {
        ((Array#{1,2} @ rc) `Array.compare` (Array#{1,1} @ rc)) == Comparison.GreaterThan
    }

    @Test
    def testCompare12(): Bool = region rc {
        ((Array#{1,2} @ rc) `Array.compare` (Array#{1,3} @ rc)) == Comparison.LessThan
    }

    @Test
    def testCompare13(): Bool = region rc {
        ((Array#{1,2,3,4} @ rc) `Array.compare` (Array#{1,2,3,4} @ rc)) == Comparison.EqualTo
    }

    /////////////////////////////////////////////////////////////////////////////
    // fold                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def fold01(): Bool = region rc {
        Array.fold((Array#{} @ rc): Array[Unit, _]) == ()
    }

    @Test
    def fold02(): Bool = region rc {
        Array.fold(Array#{"a"} @ rc) == "a"
    }

    @Test
    def fold03(): Bool = region rc {
        Array.fold(Array#{"a", "b", "c"} @ rc) == "abc"
    }

    @Test
    def fold04(): Bool = region rc {
        Array.fold(Array#{("yes", "no"), ("no", "yes")} @ rc) == ("yesno", "noyes")
    }

    /////////////////////////////////////////////////////////////////////////////
    // foldLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldLeft01(): Bool = region rc {
        Array.foldLeft((i, e) -> (i - e) * (e `Int32.rem` 2 + 1), 100, Array#{} @ rc) == 100
    }

    @Test
    def foldLeft02(): Bool = region rc {
        Array.foldLeft((i, e) -> (i - e) * (e `Int32.rem` 2 + 1), 100, Array#{1} @ rc) == 198
    }

    @Test
    def foldLeft03(): Bool = region rc {
        Array.foldLeft((i, e) -> (i - e) * (e `Int32.rem` 2 + 1), 100, Array#{1,2} @ rc) == 196
    }

    @Test
    def foldLeft04(): Bool = region rc {
        Array.foldLeft((i, e) -> (i - e) * (e `Int32.rem` 2 + 1), 100, Array#{1,2,3} @ rc) == 386
    }

    /////////////////////////////////////////////////////////////////////////////
    // foldRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldRight01(): Bool = region rc {
        Array.foldRight((e, i) -> (i - e) * (e `Int32.rem` 2 + 1), 100, Array#{} @ rc) == 100
    }

    @Test
    def foldRight02(): Bool = region rc {
        Array.foldRight((e, i) -> (i - e) * (e `Int32.rem` 2 + 1), 100, Array#{1} @ rc) == 198
    }

    @Test
    def foldRight03(): Bool = region rc {
        Array.foldRight((e, i) -> (i - e) * (e `Int32.rem` 2 + 1), 100, Array#{1,2} @ rc) == 194
    }

    @Test
    def foldRight04(): Bool = region rc {
        Array.foldRight((e, i) -> (i - e) * (e `Int32.rem` 2 + 1), 100, Array#{1,2,3} @ rc) == 382
    }

    /////////////////////////////////////////////////////////////////////////////
    // foldRightWithCont                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldRightWithCont01(): Bool = region rc {
        Array.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.rem` 2 + 1), 100, Array#{} @ rc) == 100
    }

    @Test
    def foldRightWithCont02(): Bool = region rc {
        Array.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.rem` 2 + 1), 100, Array#{1} @ rc) == 198
    }

    @Test
    def foldRightWithCont03(): Bool = region rc {
        Array.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.rem` 2 + 1), 100, Array#{1,2} @ rc) == 194
    }

    @Test
    def foldRightWithCont04(): Bool = region rc {
        Array.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.rem` 2 + 1), 100, Array#{1,2,3} @ rc) == 382
    }

    /////////////////////////////////////////////////////////////////////////////
    // foldMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldMap01(): Bool = region rc {
        Array.foldMap(x -> 2 * x, Array#{} @ rc) == 0
    }

    @Test
    def foldMap02(): Bool = region rc {
        Array.foldMap(x -> 2 * x, Array#{1, 2} @ rc) == 6
    }

    @Test
    def foldMap03(): Bool = region rc {
        Array.foldMap(x -> if (x == "a") "b" else x, Array#{"a"} @ rc) == "b"
    }

    @Test
    def foldMap04(): Bool = region rc {
        Array.foldMap(x -> if (x == "c") "b" else x, Array#{"a", "b", "c"} @ rc) == "abb"
    }

    @Test
    def foldMap05(): Bool = region rc {
        Array.foldMap(Int32.toString, Array#{1, 2, 3} @ rc) == "123"
    }

    /////////////////////////////////////////////////////////////////////////////
    // reduceLeft                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def reduceLeft01(): Bool = region rc {
        Array.reduceLeft((a, b) -> a - b, (Array#{} @ rc): Array[Int32, _]) == None
    }

    @Test
    def reduceLeft02(): Bool = region rc {
        Array.reduceLeft((a, b) -> a - b, Array#{1} @ rc) == Some(1)
    }

    @Test
    def reduceLeft03(): Bool = region rc {
        Array.reduceLeft((a, b) -> a - b, Array#{1,2} @ rc) == Some(-1)
    }

    @Test
    def reduceLeft04(): Bool = region rc {
        Array.reduceLeft((a, b) -> a - b, Array#{1,2,3} @ rc) == Some(-4)
    }

    @Test
    def reduceLeft05(): Bool = region rc {
        Array.reduceLeft((a, b) -> a - b, Array#{1,2,3,4} @ rc) == Some(-8)
    }

    /////////////////////////////////////////////////////////////////////////////
    // reduceRight                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def reduceRight01(): Bool = region rc {
        Array.reduceRight((a, b) -> a - b, (Array#{} @ rc): Array[Int32, _]) == None
    }

    @Test
    def reduceRight02(): Bool = region rc {
        Array.reduceRight((a, b) -> a - b, Array#{1} @ rc) == Some(1)
    }

    @Test
    def reduceRight03(): Bool = region rc {
        Array.reduceRight((a, b) -> a - b, Array#{1,2} @ rc) == Some(-1)
    }

    @Test
    def reduceRight04(): Bool = region rc {
        Array.reduceRight((a, b) -> a - b, Array#{1,2,3} @ rc) == Some(2)
    }

    @Test
    def reduceRight05(): Bool = region rc {
        Array.reduceRight((a, b) -> a - b, Array#{1,2,3,4} @ rc) == Some(-2)
    }

    /////////////////////////////////////////////////////////////////////////////
    // count                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def count01(): Bool = region rc {
        Array.count(i -> i > 3, Array#{} @ rc) == 0
    }

    @Test
    def count02(): Bool = region rc {
        Array.count(i -> i > 3, Array#{1} @ rc) == 0
    }

    @Test
    def count03(): Bool = region rc {
        Array.count(i -> i > 3, Array#{4} @ rc) == 1
    }

    @Test
    def count04(): Bool = region rc {
        Array.count(i -> i > 3, Array#{1,2} @ rc) == 0
    }

    @Test
    def count05(): Bool = region rc {
        Array.count(i -> i > 3, Array#{1,8} @ rc) == 1
    }

    @Test
    def count06(): Bool = region rc {
        Array.count(i -> i > 3, Array#{8,1} @ rc) == 1
    }

    @Test
    def count07(): Bool = region rc {
        Array.count(i -> i > 3, Array#{6,7} @ rc) == 2
    }


    /////////////////////////////////////////////////////////////////////////////
    // flatten                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def flatten01(): Bool = region rc {
        let a = Array.flatten(rc, (Array#{} @ rc): Array[Array[Int32, rc], rc]);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, rc])
    }

    @Test
    def flatten02(): Bool = region rc {
        let a = Array.flatten(rc, (Array#{Array#{} @ rc} @ rc): Array[Array[Int32, rc], rc]);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, rc])
    }

    @Test
    def flatten03(): Bool = region rc {
        let a = Array.flatten(rc, Array#{Array#{1} @ rc} @ rc);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @Test
    def flatten04(): Bool = region rc {
        let a= Array.flatten(rc, Array#{Array#{1,2} @ rc} @ rc);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @Test
    def flatten05(): Bool = region rc {
        let a = Array.flatten(rc, (Array#{Array#{} @ rc,Array#{} @ rc} @ rc): Array[Array[Int32, rc], rc]);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, rc])
    }

    @Test
    def flatten06(): Bool = region rc {
        let a = Array.flatten(rc, Array#{Array#{1} @ rc,Array#{} @ rc} @ rc);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @Test
    def flatten07(): Bool = region rc {
        let a = Array.flatten(rc, Array#{Array#{} @ rc,Array#{1} @ rc} @ rc);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @Test
    def flatten08(): Bool = region rc {
        let a = Array.flatten(rc, Array#{Array#{1} @ rc,Array#{2} @ rc} @ rc);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @Test
    def flatten09(): Bool = region rc {
        let a = Array.flatten(rc, Array#{Array#{1,2} @ rc,Array#{3,4,5} @ rc} @ rc);
        Array.sameElements(a, Array#{1,2,3,4,5} @ rc)
    }

    @Test
    def flatten10(): Bool = region rc {
        let a = Array.flatten(rc, Array#{Array#{1} @ rc,Array#{2,3} @ rc,Array#{4} @ rc} @ rc);
        Array.sameElements(a, Array#{1,2,3,4} @ rc)
    }
    /////////////////////////////////////////////////////////////////////////////
    // exists                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def exists01(): Bool = region rc {
        Array.exists(i -> i > 3, (Array#{} @ rc): Array[Int32, _]) == false
    }

    @Test
    def exists02(): Bool = region rc {
        Array.exists(i -> i > 3, Array#{1} @ rc) == false
    }

    @Test
    def exists03(): Bool = region rc {
        Array.exists(i -> i > 3, Array#{5} @ rc) == true
    }

    @Test
    def exists04(): Bool = region rc {
        Array.exists(i -> i > 3, Array#{1,2} @ rc) == false
    }

    @Test
    def exists05(): Bool = region rc {
        Array.exists(i -> i > 3, Array#{1,6} @ rc) == true
    }

    @Test
    def exists06(): Bool = region rc {
        Array.exists(i -> i > 3, Array#{6,1} @ rc) == true
    }

    @Test
    def exists07(): Bool = region rc {
        Array.exists(i -> i > 3, Array#{16,6} @ rc) == true
    }

    @Test
    def exists08(): Bool = region rc {
        Array.exists(i -> i > 3, Array#{1,-9,3} @ rc) == false
    }

    @Test
    def exists09(): Bool = region rc {
        Array.exists(i -> i > 3, Array#{1,9,3} @ rc) == true
    }

    /////////////////////////////////////////////////////////////////////////////
    // forAll                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forAll01(): Bool = region rc {
        Array.forAll(i -> i > 3, (Array#{} @ rc): Array[Int32, _]) == true
    }

    @Test
    def forAll02(): Bool = region rc {
        Array.forAll(i -> i > 3, Array#{1} @ rc) == false
    }

    @Test
    def forAll03(): Bool = region rc {
        Array.forAll(i -> i > 3, Array#{5} @ rc) == true
    }

    @Test
    def forAll04(): Bool = region rc {
        Array.forAll(i -> i > 3, Array#{1,2} @ rc) == false
    }

    @Test
    def forAll05(): Bool = region rc {
        Array.forAll(i -> i > 3, Array#{1,6} @ rc) == false
    }

    @Test
    def forAll06(): Bool = region rc {
        Array.forAll(i -> i > 3, Array#{6,1} @ rc) == false
    }

    @Test
    def forAll07(): Bool = region rc {
        Array.forAll(i -> i > 3, Array#{16,6} @ rc) == true
    }

    @Test
    def forAll08(): Bool = region rc {
        Array.forAll(i -> i > 3, Array#{1,-9,3} @ rc) == false
    }

    @Test
    def forAll09(): Bool = region rc {
        Array.forAll(i -> i > 3, Array#{1,9,3} @ rc) == false
    }

    @Test
    def forAll10(): Bool = region rc {
        Array.forAll(i -> i > 3, Array#{11,9,31} @ rc) == true
    }

    /////////////////////////////////////////////////////////////////////////////
    // filter                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def filter01(): Bool = region rc {
        let a = Array.filter(rc, i -> i > 3, (Array#{} @ rc): Array[Int32, _]);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def filter02(): Bool = region rc {
        let a = Array.filter(rc, i -> i > 3, Array#{2} @ rc);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def filter03(): Bool = region rc {
        let a = Array.filter(rc, i -> i > 3, Array#{4} @ rc);
        Array.sameElements(a, Array#{4} @ rc)
    }

    @Test
    def filter04(): Bool = region rc {
        let a = Array.filter(rc, i -> i > 3, Array#{1,3} @ rc);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def filter05(): Bool = region rc {
        let a = Array.filter(rc, i -> i > 3, Array#{1,8} @ rc);
        Array.sameElements(a, Array#{8} @ rc)
    }

    @Test
    def filter06(): Bool = region rc {
        let a = Array.filter(rc, i -> i > 3, Array#{8,1} @ rc);
        Array.sameElements(a, Array#{8} @ rc)
    }

    @Test
    def filter07(): Bool = region rc {
        let a = Array.filter(rc, i -> i > 3, Array#{8,9} @ rc);
        Array.sameElements(a, Array#{8,9} @ rc)
    }

    @Test
    def filter08(): Bool = region rc {
        let a = Array.filter(rc, i -> i > 3, Array#{1,4,11,2,-22,17} @ rc);
        Array.sameElements(a, Array#{4,11,17} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // partition                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def partition01(): Bool = region rc {
        let (a,b) = Array.partition(rc, rc, i -> i > 3, (Array#{} @ rc): Array[Int32, _]);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _]) and Array.sameElements(b, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def partition02(): Bool = region rc {
        let (a,b) = Array.partition(rc, rc, i -> i > 3, Array#{1} @ rc);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _]) and Array.sameElements(b, Array#{1} @ rc)
    }

    @Test
    def partition03(): Bool = region rc {
        let (a,b) = Array.partition(rc, rc, i -> i > 3, Array#{4} @ rc);
        Array.sameElements(a, Array#{4} @ rc) and Array.sameElements(b, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def partition04(): Bool = region rc {
        let (a,b) = Array.partition(rc, rc, i -> i > 3, Array#{1,2} @ rc);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _]) and Array.sameElements(b, Array#{1,2} @ rc)
    }

    @Test
    def partition05(): Bool = region rc {
        let (a,b) = Array.partition(rc, rc, i -> i > 3, Array#{1,5} @ rc);
        Array.sameElements(a, Array#{5} @ rc) and Array.sameElements(b, Array#{1} @ rc)
    }

    @Test
    def partition06(): Bool = region rc {
        let (a,b) = Array.partition(rc, rc, i -> i > 3, Array#{5,1} @ rc);
        Array.sameElements(a, Array#{5} @ rc) and Array.sameElements(b, Array#{1} @ rc)
    }

    @Test
    def partition07(): Bool = region rc {
        let (a,b) = Array.partition(rc, rc, i -> i > 3, Array#{5,8} @ rc);
        Array.sameElements(a, Array#{5,8} @ rc) and Array.sameElements(b, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def partition08(): Bool = region rc {
        let (a,b) = Array.partition(rc, rc, i -> i > 3, Array#{4, -3, -5, 1, 2, 16, 7, 1, 7} @ rc);
        Array.sameElements(a, Array#{4, 16, 7, 7} @ rc) and Array.sameElements(b, Array#{-3, -5, 1, 2, 1} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // span                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def span01(): Bool = region rc {
        let (a,b) = Array.span(rc, rc, i -> i > 3, (Array#{} @ rc): Array[Int32, _]);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _]) and Array.sameElements(b, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def span02(): Bool = region rc {
        let (a,b) = Array.span(rc, rc, i -> i > 3, Array#{1} @ rc);
        Array.sameElements(a,(Array#{} @ rc): Array[Int32, _]) and Array.sameElements(b, Array#{1} @ rc)
    }

    @Test
    def span03(): Bool = region rc {
        let (a,b) = Array.span(rc, rc, i -> i > 3, Array#{4} @ rc);
        Array.sameElements(a, Array#{4} @ rc) and Array.sameElements(b, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def span04(): Bool = region rc {
        let (a,b) = Array.span(rc, rc, i -> i > 3, Array#{1,2} @ rc);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _]) and Array.sameElements(b, Array#{1,2} @ rc)
    }

    @Test
    def span05(): Bool = region rc {
        let (a,b) = Array.span(rc, rc, i -> i > 3, Array#{1,5} @ rc);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _]) and Array.sameElements(b, Array#{1,5} @ rc)
    }

    @Test
    def span06(): Bool = region rc {
        let (a,b) = Array.span(rc, rc, i -> i > 3, Array#{5,1} @ rc);
        Array.sameElements(a, Array#{5} @ rc) and Array.sameElements(b, Array#{1} @ rc)
    }

    @Test
    def span07(): Bool = region rc {
        let (a,b) = Array.span(rc, rc, i -> i > 3, Array#{5,8} @ rc);
        Array.sameElements(a, Array#{5,8} @ rc) and Array.sameElements(b, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def span08(): Bool = region rc {
        let (a,b) = Array.span(rc, rc, i -> i > 3, Array#{4, 6, -3, 11, -5, 1, 2, 16, 7, 1, 7} @ rc);
        Array.sameElements(a, Array#{4, 6} @ rc) and Array.sameElements(b, Array#{-3, 11, -5, 1, 2, 16, 7, 1, 7} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // drop                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def drop01(): Bool = region rc {
        let a = Array.drop(rc, -1, (Array#{} @ rc): Array[Int32, _]);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def drop02(): Bool = region rc {
        let a = Array.drop(rc, 0, (Array#{} @ rc): Array[Int32, _]);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def drop03(): Bool = region rc {
        let a = Array.drop(rc, 1, (Array#{} @ rc): Array[Int32, _]);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def drop04(): Bool = region rc {
        let a = Array.drop(rc, -1, Array#{1} @ rc);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @Test
    def drop05(): Bool = region rc {
        let a = Array.drop(rc, 0, Array#{1} @ rc);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @Test
    def drop06(): Bool = region rc {
        let a = Array.drop(rc, 1, Array#{1} @ rc);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def drop07(): Bool = region rc {
        let a = Array.drop(rc, 2, Array#{1} @ rc);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def drop08(): Bool = region rc {
        let a = Array.drop(rc, 0, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @Test
    def drop09(): Bool = region rc {
        let a = Array.drop(rc, 1, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{2} @ rc)
    }

    @Test
    def drop10(): Bool = region rc {
        let a = Array.drop(rc, 2, Array#{1,2} @ rc);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def drop11(): Bool = region rc {
        let a = Array.drop(rc, 2, Array#{1,2,3,4,5,6} @ rc);
        Array.sameElements(a, Array#{3,4,5,6} @ rc)
    }

    @Test
    def drop12(): Bool = region rc {
        let a = Array.drop(rc, 4, Array#{1,2,3,4,5,6} @ rc);
        Array.sameElements(a, Array#{5,6} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // dropLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def dropLeft01(): Bool = region rc {
        let a = Array.dropLeft(rc, -1, (Array#{} @ rc): Array[Int32, _]);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def dropLeft02(): Bool = region rc {
        let a = Array.dropLeft(rc, 0, (Array#{} @ rc): Array[Int32, _]);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def dropLeft03(): Bool = region rc {
        let a = Array.dropLeft(rc, 1, (Array#{} @ rc): Array[Int32, _]);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def dropLeft04(): Bool = region rc {
        let a = Array.dropLeft(rc, -1, Array#{1} @ rc);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @Test
    def dropLeft05(): Bool = region rc {
        let a = Array.dropLeft(rc, 0, Array#{1} @ rc);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @Test
    def dropLeft06(): Bool = region rc {
        let a = Array.dropLeft(rc, 1, Array#{1} @ rc);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def dropLeft07(): Bool = region rc {
        let a = Array.dropLeft(rc, 2, Array#{1} @ rc);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def dropLeft08(): Bool = region rc {
        let a = Array.dropLeft(rc, 0, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @Test
    def dropLeft09(): Bool = region rc {
        let a = Array.dropLeft(rc, 1, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{2} @ rc)
    }

    @Test
    def dropLeft10(): Bool = region rc {
        let a = Array.dropLeft(rc, 2, Array#{1,2} @ rc);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def dropLeft11(): Bool = region rc {
        let a = Array.dropLeft(rc, 2, Array#{1,2,3,4,5,6} @ rc);
        Array.sameElements(a, Array#{3,4,5,6} @ rc)
    }

    @Test
    def dropLeft12(): Bool = region rc {
        let a = Array.dropLeft(rc, 4, Array#{1,2,3,4,5,6} @ rc);
        Array.sameElements(a, Array#{5,6} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // dropRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def dropRight01(): Bool = region rc {
        let a = Array.dropRight(rc, -1, (Array#{} @ rc): Array[Int32, _]);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def dropRight02(): Bool = region rc {
        let a = Array.dropRight(rc, 0, (Array#{} @ rc): Array[Int32, _]);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def dropRight03(): Bool = region rc {
        let a = Array.dropRight(rc, 1, (Array#{} @ rc): Array[Int32, _]);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def dropRight04(): Bool = region rc {
        let a = Array.dropRight(rc, -1, Array#{1} @ rc);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @Test
    def dropRight05(): Bool = region rc {
        let a = Array.dropRight(rc, 0, Array#{1} @ rc);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @Test
    def dropRight06(): Bool = region rc {
        let a = Array.dropRight(rc, 1, Array#{1} @ rc);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def dropRight07(): Bool = region rc {
        let a = Array.dropRight(rc, 2, Array#{1} @ rc);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def dropRight08(): Bool = region rc {
        let a = Array.dropRight(rc, 0, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @Test
    def dropRight09(): Bool = region rc {
        let a = Array.dropRight(rc, 1, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @Test
    def dropRight10(): Bool = region rc {
        let a = Array.dropRight(rc, 2, Array#{1,2} @ rc);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def dropRight11(): Bool = region rc {
        let a = Array.dropRight(rc, 2, Array#{1,2,3,4,5,6} @ rc);
        Array.sameElements(a, Array#{1,2,3,4} @ rc)
    }

    @Test
    def dropRight12(): Bool = region rc {
        let a = Array.dropRight(rc, 4, Array#{1,2,3,4,5,6} @ rc);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // dropWhile                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def dropWhile01(): Bool = region rc {
        let a = Array.dropWhile(rc, i -> i > 3, Array#{} @ rc);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def dropWhile02(): Bool = region rc {
        let a = Array.dropWhile(rc, i -> i > 3, Array#{1} @ rc);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @Test
    def dropWhile03(): Bool = region rc {
        let a = Array.dropWhile(rc, i -> i > 3, Array#{4} @ rc);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def dropWhile04(): Bool = region rc {
        let a = Array.dropWhile(rc, i -> i > 3, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @Test
    def dropWhile05(): Bool = region rc {
        let a = Array.dropWhile(rc, i -> i > 3, Array#{1,5} @ rc);
        Array.sameElements(a, Array#{1,5} @ rc)
    }

    @Test
    def dropWhile06(): Bool = region rc {
        let a = Array.dropWhile(rc, i -> i > 3, Array#{5,1} @ rc);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @Test
    def dropWhile07(): Bool = region rc {
        let a = Array.dropWhile(rc, i -> i > 3, Array#{5,8} @ rc);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def dropWhile08(): Bool = region rc {
        let a = Array.dropWhile(rc, i -> i > 3, Array#{4, 6, -3, 11, -5, 1, 2, 16, 7, 1, 7} @ rc);
        Array.sameElements(a, Array#{-3, 11, -5, 1, 2, 16, 7, 1, 7} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // dropWhileLeft                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def dropWhileLeft01(): Bool = region rc {
        let a = Array.dropWhileLeft(rc, i -> i > 3, Array#{} @ rc);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def dropWhileLeft02(): Bool = region rc {
        let a = Array.dropWhileLeft(rc, i -> i > 3, Array#{1} @ rc);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @Test
    def dropWhileLeft03(): Bool = region rc {
        let a = Array.dropWhileLeft(rc, i -> i > 3, Array#{4} @ rc);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def dropWhileLeft04(): Bool = region rc {
        let a = Array.dropWhileLeft(rc, i -> i > 3, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @Test
    def dropWhileLeft05(): Bool = region rc {
        let a = Array.dropWhileLeft(rc, i -> i > 3, Array#{1,5} @ rc);
        Array.sameElements(a, Array#{1,5} @ rc)
    }

    @Test
    def dropWhileLeft06(): Bool = region rc {
        let a = Array.dropWhileLeft(rc, i -> i > 3, Array#{5,1} @ rc);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @Test
    def dropWhileLeft07(): Bool = region rc {
        let a = Array.dropWhileLeft(rc, i -> i > 3, Array#{5,8} @ rc);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def dropWhileLeft08(): Bool = region rc {
        let a = Array.dropWhileLeft(rc, i -> i > 3, Array#{4, 6, -3, 11, -5, 1, 2, 16, 7, 1, 7} @ rc);
        Array.sameElements(a, Array#{-3, 11, -5, 1, 2, 16, 7, 1, 7} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // dropWhileRight                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def dropWhileRight01(): Bool = region rc {
        let a = Array.dropWhileRight(rc, i -> i > 3, Array#{} @ rc);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def dropWhileRight02(): Bool = region rc {
        let a = Array.dropWhileRight(rc, i -> i > 3, Array#{1} @ rc);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @Test
    def dropWhileRight03(): Bool = region rc {
        let a = Array.dropWhileRight(rc, i -> i > 3, Array#{4} @ rc);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def dropWhileRight04(): Bool = region rc {
        let a = Array.dropWhileRight(rc, i -> i > 3, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @Test
    def dropWhileRight05(): Bool = region rc {
        let a = Array.dropWhileRight(rc, i -> i > 3, Array#{1,5} @ rc);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @Test
    def dropWhileRight06(): Bool = region rc {
        let a = Array.dropWhileRight(rc, i -> i > 3, Array#{5,1} @ rc);
        Array.sameElements(a, Array#{5,1} @ rc)
    }

    @Test
    def dropWhileRight07(): Bool = region rc {
        let a = Array.dropWhileRight(rc, i -> i > 3, Array#{5,8} @ rc);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def dropWhileRight08(): Bool = region rc {
        let a = Array.dropWhileRight(rc, i -> i > 3, Array#{4, 6, -3, 11, -5, 1, 2, 16, 7, 1, 7} @ rc);
        Array.sameElements(a, Array#{4, 6, -3, 11, -5, 1, 2, 16, 7, 1} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // take                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def take01(): Bool = region rc {
        let a = Array.take(rc, -1, (Array#{} @ rc): Array[Int32, _]);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def take02(): Bool = region rc {
        let a = Array.take(rc, 0, (Array#{} @ rc): Array[Int32, _]);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def take03(): Bool = region rc {
        let a = Array.take(rc, 1, (Array#{} @ rc): Array[Int32, _]);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def take04(): Bool = region rc {
        let a = Array.take(rc, -1, Array#{1} @ rc);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def take05(): Bool = region rc {
        let a = Array.take(rc, 0, Array#{1} @ rc);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])

    }
    @Test
    def take06(): Bool = region rc {
        let a = Array.take(rc, 1, Array#{1} @ rc);
        Array.sameElements(a, Array#{1} @ rc)

    }
    @Test
    def take07(): Bool = region rc {
        let a = Array.take(rc, 2, Array#{1} @ rc);
        Array.sameElements(a, Array#{1} @ rc)

    }
    @Test
    def take08(): Bool = region rc {
        let a = Array.take(rc, 0, Array#{1,2} @ rc);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def take09(): Bool = region rc {
        let a = Array.take(rc, 1, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @Test
    def take10(): Bool = region rc {
        let a = Array.take(rc, 2, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @Test
    def take11(): Bool = region rc {
        let a = Array.take(rc, 2, Array#{1,2,3,4,5,6} @ rc);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @Test
    def take12(): Bool = region rc {
        let a = Array.take(rc, 4, Array#{1,2,3,4,5,6} @ rc);
        Array.sameElements(a, Array#{1,2,3,4} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // takeLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def takeLeft01(): Bool = region rc {
        let a = Array.takeLeft(rc, -1, (Array#{} @ rc): Array[Int32, _]);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def takeLeft02(): Bool = region rc {
        let a = Array.takeLeft(rc, 0, (Array#{} @ rc): Array[Int32, _]);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def takeLeft03(): Bool = region rc {
        let a = Array.takeLeft(rc, 1, (Array#{} @ rc): Array[Int32, _]);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def takeLeft04(): Bool = region rc {
        let a = Array.takeLeft(rc, -1, Array#{1} @ rc);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def takeLeft05(): Bool = region rc {
        let a = Array.takeLeft(rc, 0, Array#{1} @ rc);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def takeLeft06(): Bool = region rc {
        let a = Array.takeLeft(rc, 1, Array#{1} @ rc);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @Test
    def takeLeft07(): Bool = region rc {
        let a = Array.takeLeft(rc, 2, Array#{1} @ rc);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @Test
    def takeLeft08(): Bool = region rc {
        let a = Array.takeLeft(rc, 0, Array#{1,2} @ rc);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def takeLeft09(): Bool = region rc {
        let a = Array.takeLeft(rc, 1, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @Test
    def takeLeft10(): Bool = region rc {
        let a = Array.takeLeft(rc, 2, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @Test
    def takeLeft11(): Bool = region rc {
        let a = Array.takeLeft(rc, 2, Array#{1,2,3,4,5,6} @ rc);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @Test
    def takeLeft12(): Bool = region rc {
        let a = Array.takeLeft(rc, 4, Array#{1,2,3,4,5,6} @ rc);
        Array.sameElements(a, Array#{1,2,3,4} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // takeRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def takeRight01(): Bool = region rc {
        let a = Array.takeRight(rc, -1, (Array#{} @ rc): Array[Int32, _]);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def takeRight02(): Bool = region rc {
        let a = Array.takeRight(rc, 0, (Array#{} @ rc): Array[Int32, _]);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def takeRight03(): Bool = region rc {
        let a = Array.takeRight(rc, 1, (Array#{} @ rc): Array[Int32, _]);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def takeRight04(): Bool = region rc {
        let a = Array.takeRight(rc, -1, Array#{1} @ rc);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def takeRight05(): Bool = region rc {
        let a = Array.takeRight(rc, 0, Array#{1} @ rc);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def takeRight06(): Bool = region rc {
        let a = Array.takeRight(rc, 1, Array#{1} @ rc);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @Test
    def takeRight07(): Bool = region rc {
        let a = Array.takeRight(rc, 2, Array#{1} @ rc);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @Test
    def takeRight08(): Bool = region rc {
        let a = Array.takeRight(rc, 0, Array#{1,2} @ rc);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def takeRight09(): Bool = region rc {
        let a = Array.takeRight(rc, 1, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{2} @ rc)
    }

    @Test
    def takeRight10(): Bool = region rc {
        let a = Array.takeRight(rc, 2, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @Test
    def takeRight11(): Bool = region rc {
        let a = Array.takeRight(rc, 2, Array#{1,2,3,4,5,6} @ rc);
        Array.sameElements(a, Array#{5,6} @ rc)
    }

    @Test
    def takeRight12(): Bool = region rc {
        let a = Array.takeRight(rc, 4, Array#{1,2,3,4,5,6} @ rc);
        Array.sameElements(a, Array#{3,4,5,6} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // takeWhile                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def takeWhile01(): Bool = region rc {
        let a = Array.takeWhile(rc, i -> i > 3, (Array#{} @ rc): Array[Int32, _]);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def takeWhile02(): Bool = region rc {
        let a = Array.takeWhile(rc, i -> i > 3, Array#{1} @ rc);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def takeWhile03(): Bool = region rc {
        let a = Array.takeWhile(rc, i -> i > 3, Array#{4} @ rc);
        Array.sameElements(a, Array#{4} @ rc)
    }

    @Test
    def takeWhile04(): Bool = region rc {
        let a = Array.takeWhile(rc, i -> i > 3, Array#{1,2} @ rc);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def takeWhile05(): Bool = region rc {
        let a = Array.takeWhile(rc, i -> i > 3, Array#{1,5} @ rc);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def takeWhile06(): Bool = region rc {
        let a = Array.takeWhile(rc, i -> i > 3, Array#{5,1} @ rc);
        Array.sameElements(a, Array#{5} @ rc)
    }

    @Test
    def takeWhile07(): Bool = region rc {
        let a = Array.takeWhile(rc, i -> i > 3, Array#{5,8} @ rc);
        Array.sameElements(a, Array#{5,8} @ rc)
    }

    @Test
    def takeWhile08(): Bool = region rc {
        let a = Array.takeWhile(rc, i -> i > 3, Array#{4, 6, -3, 11, -5, 1, 2, 16, 7, 1, 7} @ rc);
        Array.sameElements(a, Array#{4, 6} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // takeWhileLeft                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def takeWhileLeft01(): Bool = region rc {
        let a = Array.takeWhileLeft(rc, i -> i > 3, (Array#{} @ rc): Array[Int32, _]);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def takeWhileLeft02(): Bool = region rc {
        let a = Array.takeWhileLeft(rc, i -> i > 3, Array#{1} @ rc);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def takeWhileLeft03(): Bool = region rc {
        let a = Array.takeWhileLeft(rc, i -> i > 3, Array#{4} @ rc);
        Array.sameElements(a, Array#{4} @ rc)
    }

    @Test
    def takeWhileLeft04(): Bool = region rc {
        let a = Array.takeWhileLeft(rc, i -> i > 3, Array#{1,2} @ rc);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def takeWhileLeft05(): Bool = region rc {
        let a = Array.takeWhileLeft(rc, i -> i > 3, Array#{1,5} @ rc);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def takeWhileLeft06(): Bool = region rc {
        let a = Array.takeWhileLeft(rc, i -> i > 3, Array#{5,1} @ rc);
        Array.sameElements(a, Array#{5} @ rc)
    }

    @Test
    def takeWhileLeft07(): Bool = region rc {
        let a = Array.takeWhileLeft(rc, i -> i > 3, Array#{5,8} @ rc);
        Array.sameElements(a, Array#{5,8} @ rc)
    }

    @Test
    def takeWhileLeft08(): Bool = region rc {
        let a = Array.takeWhileLeft(rc, i -> i > 3, Array#{4, 6, -3, 11, -5, 1, 2, 16, 7, 1, 7} @ rc);
        Array.sameElements(a, Array#{4, 6} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // takeWhileRight                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def takeWhileRight01(): Bool = region rc {
        let a = Array.takeWhileRight(rc, i -> i > 3, (Array#{} @ rc): Array[Int32, _]);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def takeWhileRight02(): Bool = region rc {
        let a = Array.takeWhileRight(rc, i -> i > 3, Array#{1} @ rc);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def takeWhileRight03(): Bool = region rc {
        let a = Array.takeWhileRight(rc, i -> i > 3, Array#{4} @ rc);
        Array.sameElements(a, Array#{4} @ rc)
    }

    @Test
    def takeWhileRight04(): Bool = region rc {
        let a = Array.takeWhileRight(rc, i -> i > 3, Array#{1,2} @ rc);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def takeWhileRight05(): Bool = region rc {
        let a = Array.takeWhileRight(rc, i -> i > 3, Array#{1,5} @ rc);
        Array.sameElements(a, Array#{5} @ rc)
    }

    @Test
    def takeWhileRight06(): Bool = region rc {
        let a = Array.takeWhileRight(rc, i -> i > 3, Array#{5,1} @ rc);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def takeWhileRight07(): Bool = region rc {
        let a = Array.takeWhileRight(rc, i -> i > 3, Array#{5,8} @ rc);
        Array.sameElements(a, Array#{5,8} @ rc)
    }

    @Test
    def takeWhileRight08(): Bool = region rc {
        let a = Array.takeWhileRight(rc, i -> i > 3, Array#{4, 6, -3, 11, -5, 1, 2, 16, 7, 1, 7} @ rc);
        Array.sameElements(a, Array#{7} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // groupBy                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def groupBy01(): Bool = region rc {
        let arr = Array.groupBy(rc, (a, b) -> a > 3 or b > 8, (Array#{} @ rc): Array[Int32, rc]);
        let xs = Array.toList(arr) |> List.map(Array.toList);
        xs == Nil
    }

    @Test
    def groupBy02(): Bool = region rc {
        let arr = Array.groupBy(rc, (a, b) -> a > 3 or b > 8, Array#{1} @ rc);
        let xs = Array.toList(arr) |> List.map(Array.toList);
        xs == (1 :: Nil) :: Nil
    }

    @Test
    def groupBy03(): Bool = region rc {
        let arr = Array.groupBy(rc, (a, b) -> a > 3 or b > 8, Array#{1,4} @ rc);
        let xs = Array.toList(arr) |> List.map(Array.toList);
        xs == (1 :: Nil) :: (4 :: Nil) :: Nil
    }

    @Test
    def groupBy04(): Bool = region rc {
        let arr = Array.groupBy(rc, (a, b) -> a > 3 or b > 8, Array#{1,9} @ rc);
        let xs = Array.toList(arr) |> List.map(Array.toList);
        xs == (1 :: 9 :: Nil) :: Nil
    }

    @Test
    def groupBy05(): Bool = region rc {
        let arr = Array.groupBy(rc, (a, b) -> a > 3 or b > 8, Array#{1, 4, 7, 6, 9, 2, 4, 4, 8, 16} @ rc);
        let xs = Array.toList(arr) |> List.map(Array.toList);
        xs == (1 :: 9 :: 16 :: Nil) :: (4 :: 7 :: 6 :: 4 :: 4 :: 8 :: Nil) :: (2 :: Nil) :: Nil
    }

    @Test
    def groupBy06(): Bool = region rc {
        let arr = Array.groupBy(rc, (a, b) -> a > -6 or a*b >= 0, Array#{-1, -11, 4, -11, 0, 8, 2, 1, -3, -24} @ rc);
        let xs = Array.toList(arr) |> List.map(Array.toList);
        xs == (-1 :: -11 :: -11 :: 0 :: -3 :: -24 :: Nil) :: (4 :: 8 :: 2 :: 1 :: Nil) :: Nil
    }

    @Test
    def groupBy07(): Bool = region rc {
        let arr = Array.groupBy(rc, (a, b) -> a < 0 or (a > 10 or (b > 10 or a == b)), Array#{-5, 6, 11, 8, 8, -11, -1, 0, 4, -1} @ rc);
        let xs = Array.toList(arr) |> List.map(Array.toList);
        xs == (-5 :: 11 :: -11 :: -1 :: -1 :: Nil) :: (6 :: Nil) :: (8 :: 8 :: Nil) :: (0 :: Nil) :: (4 :: Nil) :: Nil
    }

    /////////////////////////////////////////////////////////////////////////////
    // zip                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def zip01(): Bool = region rc {
        let a = Array.zip(rc, (Array#{} @ rc): Array[Int32, _], (Array#{} @ rc): Array[Int32, _]);
        Array.sameElements(a, (Array#{} @ rc): Array[(Int32, Int32), _])
    }

    @Test
    def zip02(): Bool = region rc {
        let a = Array.zip(rc, Array#{1} @ rc, (Array#{} @ rc): Array[Int32, _]);
        Array.sameElements(a, (Array#{} @ rc): Array[(Int32, Int32), _])
    }

    @Test
    def zip03(): Bool = region rc {
        let a = Array.zip(rc, (Array#{} @ rc): Array[Int32, _], Array#{2} @ rc);
        Array.sameElements(a, (Array#{} @ rc): Array[(Int32, Int32), _])
    }

    @Test
    def zip04(): Bool = region rc {
        let a = Array.zip(rc, Array#{1} @ rc, Array#{2} @ rc);
        Array.sameElements(a, Array#{(1, 2)} @ rc)
    }

    @Test
    def zip05(): Bool = region rc {
        let a = Array.zip(rc, Array#{1,3} @ rc, Array#{2,4} @ rc);
        Array.sameElements(a, Array#{(1, 2), (3, 4)} @ rc)
    }

    @Test
    def zip06(): Bool = region rc {
        let a = Array.zip(rc, Array#{1,3,5} @ rc, Array#{2,4,6} @ rc);
        Array.sameElements(a, Array#{(1, 2), (3, 4), (5, 6)} @ rc)
    }

    @Test
    def zip07(): Bool = region rc {
        let a = Array.zip(rc, Array#{1,3,5,7} @ rc, Array#{2,4,6,8} @ rc);
        Array.sameElements(a, Array#{(1, 2), (3, 4), (5, 6), (7, 8)} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // zipWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def zipWith01(): Bool = region rc {
        let arr = Array.zipWith(rc, (a, b) -> if (b) a + 1 else a, (Array#{} @ rc): Array[Int32, _], (Array#{} @ rc): Array[Bool, _]);
        Array.sameElements(arr, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def zipWith02(): Bool = region rc {
        let arr = Array.zipWith(rc, (a, b) -> if (b) a + 1 else a, Array#{1} @ rc, (Array#{} @ rc): Array[Bool, _]);
        Array.sameElements(arr, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def zipWith03(): Bool = region rc {
        let arr = Array.zipWith(rc, (a, b) -> if (b) a + 1 else a, (Array#{} @ rc): Array[Int32, _], Array#{true} @ rc);
        Array.sameElements(arr, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def zipWith04(): Bool = region rc {
        let arr = Array.zipWith(rc, (a, b) -> if (b) a + 1 else a, Array#{1} @ rc, Array#{true} @ rc);
        Array.sameElements(arr, Array#{2} @ rc)
    }

    @Test
    def zipWith05(): Bool = region rc {
        let arr = Array.zipWith(rc, (a, b) -> if (b) a + 1 else a, Array#{1} @ rc, Array#{false} @ rc);
        Array.sameElements(arr, Array#{1} @ rc)
    }

    @Test
    def zipWith06(): Bool = region rc {
        let arr = Array.zipWith(rc, (a, b) -> if (b) a + 1 else a,
                        Array#{1, 2, 3, 4, 5, 6, 7, 8} @ rc,
                        Array#{false, true, true, false, false, true, true, true} @ rc);
        Array.sameElements(arr, Array#{1, 3, 4, 4, 5, 7, 8, 9} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // unzip                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def unzip01(): Bool = region rc {
        let (a,b) = Array.unzip(rc, rc, (Array#{} @ rc): Array[(Unit, Unit), _]);
        Array.sameElements(a, Array#{} @ rc) and Array.sameElements(b, Array#{} @ rc)
    }

    @Test
    def unzip02(): Bool = region rc {
        let (a,b) = Array.unzip(rc, rc, Array#{(1, true)} @ rc);
        Array.sameElements(a, Array#{1} @ rc) and Array.sameElements(b, Array#{true} @ rc)
    }

    @Test
    def unzip03(): Bool = region rc {
        let (a,b) = Array.unzip(rc, rc, Array#{(1, true), (2, true)} @ rc);
        Array.sameElements(a, Array#{1,2} @ rc)  and Array.sameElements(b, Array#{true,true} @ rc)
    }

    @Test
    def unzip04(): Bool = region rc {
        let (a,b) = Array.unzip(rc, rc, Array#{(1, true), (2, true), (3, false)} @ rc);
        Array.sameElements(a, Array#{1,2,3} @ rc) and Array.sameElements(b, Array#{true,true,false} @ rc)
    }

    @Test
    def unzip05(): Bool = region rc1 { region rc2 {
        unzip05Aux(rc1, Array#{(1, "1"), (2, "2"), (3, "3")} @ rc2) `Array.sameElements` (Array#{1, 2, 3} @ rc2)
    }}

    def unzip05Aux(rc1: Region[r1], a: Array[(a, b), r2]): Array[a, r1] \ { r1, r2 } =
        region rc3 {
            // snd is local, fst is for caller
            Array.unzip(rc1, rc3, a) |> fst
        }

    /////////////////////////////////////////////////////////////////////////////
    // fold2                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def fold201(): Bool = region rc {
        Array.fold2((c, a, b) -> if (b) a + c else a * c, 4, (Array#{} @ rc): Array[Int32, _], (Array#{} @ rc): Array[Bool, _]) == 4
    }

    @Test
    def fold202(): Bool = region rc {
        Array.fold2((c, a, b) -> if (b) a + c else a * c, 4, Array#{1} @ rc, Array#{} @ rc) == 4
    }

    @Test
    def fold203(): Bool = region rc {
        Array.fold2((c, a, b) -> if (b) a + c else a * c, 4, Array#{} @ rc, Array#{true} @ rc) == 4
    }

    @Test
    def fold204(): Bool = region rc {
        Array.fold2((c, a, b) -> if (b) a + c else a * c, 4, Array#{2} @ rc, Array#{true} @ rc) == 6
    }

    @Test
    def fold205(): Bool = region rc {
        Array.fold2((c, a, b) -> if (b) a + c else a * c, 4, Array#{2} @ rc, Array#{false} @ rc) == 8
    }

    @Test
    def fold206(): Bool = region rc {
        Array.fold2((c, a, b) -> if (b) a + c else a * c, 4, Array#{3,2} @ rc, Array#{true,true} @ rc) == 9
    }

    @Test
    def fold207(): Bool = region rc {
        Array.fold2((c, a, b) -> if (b) a + c else a * c, 4, Array#{3,2} @ rc, Array#{true,false} @ rc) == 14
    }

    @Test
    def fold208(): Bool = region rc {
        Array.fold2((c, a, b) -> if (b) a + c else a * c, 4, Array#{3,2} @ rc, Array#{false,true} @ rc) == 14
    }

    @Test
    def fold209(): Bool = region rc {
        Array.fold2((c, a, b) -> if (b) a + c else a * c, 4, Array#{3,2} @ rc, Array#{false,false} @ rc) == 24
    }

    @Test
    def fold210(): Bool = region rc {
        Array.fold2((c, a, b) -> if (b) a + c else a * c, 4, Array#{6,-4,3,2} @ rc,
            Array#{true,false,false,true} @ rc) == -118
    }

    /////////////////////////////////////////////////////////////////////////////
    // foldLeft2                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldLeft201(): Bool = region rc {
        Array.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, (Array#{} @ rc): Array[Int32, _], (Array#{} @ rc): Array[Bool, _]) == 4
    }

    @Test
    def foldLeft202(): Bool = region rc {
        Array.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, Array#{1} @ rc, (Array#{} @ rc): Array[Bool, _]) == 4
    }

    @Test
    def foldLeft203(): Bool = region rc {
        Array.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, (Array#{} @ rc): Array[Int32, _], Array#{true} @ rc) == 4
    }

    @Test
    def foldLeft204(): Bool = region rc {
        Array.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, Array#{2} @ rc, Array#{true} @ rc) == 6
    }

    @Test
    def foldLeft205(): Bool = region rc {
        Array.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, Array#{2} @ rc, Array#{false} @ rc) == 8
    }

    @Test
    def foldLeft206(): Bool = region rc {
        Array.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, Array#{3,2} @ rc, Array#{true,true} @ rc) == 9
    }

    @Test
    def foldLeft207(): Bool = region rc {
        Array.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, Array#{3,2} @ rc, Array#{true,false} @ rc) == 14
    }

    @Test
    def foldLeft208(): Bool = region rc {
        Array.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, Array#{3,2} @ rc, Array#{false,true} @ rc) == 14
    }

    @Test
    def foldLeft209(): Bool = region rc {
        Array.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, Array#{3,2} @ rc, Array#{false,false} @ rc) == 24
    }

    @Test
    def foldLeft210(): Bool = region rc {
        Array.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, Array#{6,-4,3,2} @ rc,
            Array#{true,false,false,true} @ rc) == -118
    }

    /////////////////////////////////////////////////////////////////////////////
    // foldRight2                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldRight201(): Bool = region rc {
        Array.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, (Array#{} @ rc): Array[Int32, _], (Array#{} @ rc): Array[Bool, _]) == 4
    }

    @Test
    def foldRight202(): Bool = region rc {
        Array.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, Array#{1} @ rc, (Array#{} @ rc): Array[Bool, _]) == 4
    }

    @Test
    def foldRight203(): Bool = region rc {
        Array.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, (Array#{} @ rc): Array[Int32, _], Array#{true} @ rc) == 4
    }

    @Test
    def foldRight204(): Bool = region rc {
        Array.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, Array#{2} @ rc, Array#{true} @ rc) == 6
    }

    @Test
    def foldRight205(): Bool = region rc {
        Array.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, Array#{2} @ rc, Array#{false} @ rc) == 8
    }

    @Test
    def foldRight206(): Bool = region rc {
        Array.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, Array#{3,2} @ rc, Array#{true,true} @ rc) == 9
    }

    @Test
    def foldRight207(): Bool = region rc {
        Array.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, Array#{3,2} @ rc, Array#{true,false} @ rc) == 11
    }

    @Test
    def foldRight208(): Bool = region rc {
        Array.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, Array#{3,2} @ rc, Array#{false,true} @ rc) == 18
    }

    @Test
    def foldRight209(): Bool = region rc {
        Array.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, Array#{3,2} @ rc, Array#{false,false} @ rc) == 24
    }

    @Test
    def foldRight210(): Bool = region rc {
        Array.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, Array#{6,-4,3,2} @ rc,
            Array#{true,false,false,true} @ rc) == -66
    }

    /////////////////////////////////////////////////////////////////////////////
    // filterMap                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def filterMap01(): Bool = region rc {
        let a = Array.filterMap(rc, i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, (Array#{} @ rc): Array[Int32, _]);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def filterMap02(): Bool = region rc {
        let a = Array.filterMap(rc, i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Array#{1} @ rc);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def filterMap03(): Bool = region rc {
        let a = Array.filterMap(rc, i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Array#{2} @ rc);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @Test
    def filterMap04(): Bool = region rc {
        let a = Array.filterMap(rc, i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Array#{1,3} @ rc);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def filterMap05(): Bool = region rc {
        let a = Array.filterMap(rc, i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Array#{1,4} @ rc);
        Array.sameElements(a, Array#{2} @ rc)
    }

    @Test
    def filterMap06(): Bool = region rc {
        let a = Array.filterMap(rc, i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Array#{6,-1} @ rc);
        Array.sameElements(a, Array#{3} @ rc)
    }

    @Test
    def filterMap07(): Bool = region rc {
        let a = Array.filterMap(rc, i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Array#{8,6} @ rc);
        Array.sameElements(a, Array#{4,3} @ rc)
    }

    @Test
    def filterMap08(): Bool = region rc {
        let a = Array.filterMap(rc, i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Array#{0,1,2,3,4,5,10} @ rc);
        Array.sameElements(a, Array#{0,1,2,5} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // findMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def findMap01(): Bool = region rc {
        Array.findMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, (Array#{} @ rc): Array[Int32, _]) == None
    }

    @Test
    def findMap02(): Bool = region rc {
        Array.findMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Array#{1} @ rc) == None
    }

    @Test
    def findMap03(): Bool = region rc {
        Array.findMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Array#{2} @ rc) == Some(1)
    }

    @Test
    def findMap04(): Bool = region rc {
        Array.findMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Array#{1,3} @ rc) == None
    }

    @Test
    def findMap05(): Bool = region rc {
        Array.findMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Array#{1,4} @ rc) == Some(2)
    }

    @Test
    def findMap06(): Bool = region rc {
        Array.findMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Array#{6,-1} @ rc) == Some(3)
    }

    @Test
    def findMap07(): Bool = region rc {
        Array.findMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Array#{8,6} @ rc) == Some(4)
    }

    @Test
    def findMap08(): Bool = region rc {
        Array.findMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Array#{0,1,2,3,4,5,10} @ rc) == Some(0)
    }

    /////////////////////////////////////////////////////////////////////////////
    // toSet                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toSet01(): Bool = region rc {
        Array.toSet((Array#{} @ rc): Array[Int32, _]) == Set#{}
    }

    @Test
    def toSet02(): Bool = region rc {
        Array.toSet(Array#{1} @ rc) == Set#{1}
    }

    @Test
    def toSet03(): Bool = region rc {
        Array.toSet(Array#{1,2} @ rc) == Set#{1, 2}
    }

    @Test
    def toSet04(): Bool = region rc {
        Array.toSet(Array#{1,1} @ rc) == Set#{1}
    }

    @Test
    def toSet05(): Bool = region rc {
        Array.toSet(Array#{1,2,3} @ rc) == Set#{1, 2, 3}
    }

    @Test
    def toSet06(): Bool = region rc {
        Array.toSet(Array#{1,2,1} @ rc) == Set#{1, 2}
    }

    @Test
    def toSet07(): Bool = region rc {
        Array.toSet(Array#{1,1,2} @ rc) == Set#{1, 2}
    }

    @Test
    def toSet08(): Bool = region rc {
        Array.toSet(Array#{2,1,2,3,4,5,6,3,3} @ rc) == Set#{1, 2, 3, 4, 5, 6}
    }

    /////////////////////////////////////////////////////////////////////////////
    // toMap                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toMap01(): Bool = region rc {
        Array.toMap((Array#{} @ rc): Array[(Int32, Bool), _]) == Map#{}
    }

    @Test
    def toMap02(): Bool = region rc {
        Array.toMap(Array#{(1, true)} @ rc) == Map#{1 => true}
    }

    @Test
    def toMap03(): Bool = region rc {
        Array.toMap(Array#{(1, true), (2, false)} @ rc) == Map#{1 => true, 2 => false}
    }

    @Test
    def toMap04(): Bool = region rc {
        Array.toMap(Array#{(1, true), (1, false)} @ rc) == Map#{1 => true}
    }

    /////////////////////////////////////////////////////////////////////////////
    // findIndexOf                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def findIndexOf01(): Bool = region rc {
        Array.findIndexOf(i -> i > 2, (Array#{} @ rc): Array[Int32, _]) == None
    }

    @Test
    def findIndexOf02(): Bool = region rc {
        Array.findIndexOf(i -> i > 2, Array#{1} @ rc) == None
    }

    @Test
    def findIndexOf03(): Bool = region rc {
        Array.findIndexOf(i -> i > 2, Array#{3} @ rc) == Some(0)
    }

    @Test
    def findIndexOf04(): Bool = region rc {
        Array.findIndexOf(i -> i > 2, Array#{1,2} @ rc) == None
    }

    @Test
    def findIndexOf05(): Bool = region rc {
        Array.findIndexOf(i -> i > 2, Array#{6,-6} @ rc) == Some(0)
    }

    @Test
    def findIndexOf06(): Bool = region rc {
        Array.findIndexOf(i -> i > 2, Array#{-6,6} @ rc) == Some(1)
    }

    @Test
    def findIndexOf07(): Bool = region rc {
        Array.findIndexOf(i -> i > 2, Array#{6,7} @ rc) == Some(0)
    }

    /////////////////////////////////////////////////////////////////////////////
    // findIndexOfLeft                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def findIndexOfLeft01(): Bool = region rc {
        Array.findIndexOfLeft(i -> i > 2, (Array#{} @ rc): Array[Int32, _]) == None
    }

    @Test
    def findIndexOfLeft02(): Bool = region rc {
        Array.findIndexOfLeft(i -> i > 2, Array#{1} @ rc) == None
    }

    @Test
    def findIndexOfLeft03(): Bool = region rc {
        Array.findIndexOfLeft(i -> i > 2, Array#{3} @ rc) == Some(0)
    }

    @Test
    def findIndexOfLeft04(): Bool = region rc {
        Array.findIndexOfLeft(i -> i > 2, Array#{1,2} @ rc) == None
    }

    @Test
    def findIndexOfLeft05(): Bool = region rc {
        Array.findIndexOfLeft(i -> i > 2, Array#{6,-6} @ rc) == Some(0)
    }

    @Test
    def findIndexOfLeft06(): Bool = region rc {
        Array.findIndexOfLeft(i -> i > 2, Array#{-6,6} @ rc) == Some(1)
    }

    @Test
    def findIndexOfLeft07(): Bool = region rc {
        Array.findIndexOfLeft(i -> i > 2, Array#{6,7} @ rc) == Some(0)
    }

    /////////////////////////////////////////////////////////////////////////////
    // findIndexOfRight                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def findIndexOfRight01(): Bool = region rc {
        Array.findIndexOfRight(i -> i > 2, (Array#{} @ rc): Array[Int32, _]) == None
    }

    @Test
    def findIndexOfRight02(): Bool = region rc {
        Array.findIndexOfRight(i -> i > 2, Array#{1} @ rc) == None
    }

    @Test
    def findIndexOfRight03(): Bool = region rc {
        Array.findIndexOfRight(i -> i > 2, Array#{3} @ rc) == Some(0)
    }

    @Test
    def findIndexOfRight04(): Bool = region rc {
        Array.findIndexOfRight(i -> i > 2, Array#{1,2} @ rc) == None
    }

    @Test
    def findIndexOfRight05(): Bool = region rc {
        Array.findIndexOfRight(i -> i > 2, Array#{6,-6} @ rc) == Some(0)
    }

    @Test
    def findIndexOfRight06(): Bool = region rc {
        Array.findIndexOfRight(i -> i > 2, Array#{-6,6} @ rc) == Some(1)
    }

    @Test
    def findIndexOfRight07(): Bool = region rc {
        Array.findIndexOfRight(i -> i > 2, Array#{6,7} @ rc) == Some(1)
    }

    /////////////////////////////////////////////////////////////////////////////
    // findIndices                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def findIndices01(): Bool = region rc {
        let a = Array.findIndices(rc, i -> i > 2, (Array#{} @ rc): Array[Int32, rc]);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, rc])
    }

    @Test
    def findIndices02(): Bool = region rc {
        let a = Array.findIndices(rc, i -> i > 2, Array#{1} @ rc);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, rc])
    }

    @Test
    def findIndices03(): Bool = region rc {
        let a = Array.findIndices(rc, i -> i > 2, Array#{3} @ rc);
        Array.sameElements(a, Array#{0} @ rc)
    }

    @Test
    def findIndices04(): Bool = region rc {
        let a = Array.findIndices(rc, i -> i > 2, Array#{1,2} @ rc);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, rc])
    }

    @Test
    def findIndices05(): Bool = region rc {
        let a = Array.findIndices(rc, i -> i > 2, Array#{6,-6} @ rc);
        Array.sameElements(a, Array#{0} @ rc)
    }

    @Test
    def findIndices06(): Bool = region rc {
        let a = Array.findIndices(rc, i -> i > 2, Array#{-6,6} @ rc);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @Test
    def findIndices07(): Bool = region rc {
        let a = Array.findIndices(rc, i -> i > 2, Array#{6,7} @ rc);
        Array.sameElements(a, Array#{0,1} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // init                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def init01(): Bool = region rc {
        let a = Array.init(rc, x -> x, 0);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def init02(): Bool = region rc {
        let a = Array.init(rc, x -> x, -1);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def init03(): Bool = region rc {
        let a = Array.init(rc, x -> x, 1);
        Array.sameElements(a, Array#{0} @ rc)
    }

    @Test
    def init04(): Bool = region rc {
        let a = Array.init(rc, x -> x, 2);
        Array.sameElements(a, Array#{0,1} @ rc)
    }

    @Test
    def init05(): Bool = region rc {
        let a = Array.init(rc, x -> x, 50);
        Array.length(a) == 50
    }

    /////////////////////////////////////////////////////////////////////////////
    // sameElements                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sameElements01(): Bool = region rc {
        let a = (Array#{} @ rc): Array[Int32, _];
        let b = (Array#{} @ rc): Array[Int32, _];
        Array.sameElements(a,b) == true
    }

    @Test
    def sameElements02(): Bool = region rc {
        let a = (Array#{1} @ rc): Array[Int32, _];
        let b = (Array#{} @ rc): Array[Int32, _];
        Array.sameElements(a,b) == false
    }

    @Test
    def sameElements03(): Bool = region rc {
        let a = (Array#{} @ rc): Array[Int32, _];
        let b = (Array#{1} @ rc): Array[Int32, _];
        Array.sameElements(a,b) == false
    }

    @Test
    def sameElements04(): Bool = region rc {
        let a = (Array#{1} @ rc): Array[Int32, _];
        let b = (Array#{1} @ rc): Array[Int32, _];
        Array.sameElements(a,b) == true
    }

    @Test
    def sameElements05(): Bool = region rc {
        let a = (Array#{1} @ rc): Array[Int32, _];
        let b = (Array#{0} @ rc): Array[Int32, _];
        Array.sameElements(a,b) == false
    }

    @Test
    def sameElements06(): Bool = region rc {
        let a = (Array#{1,2} @ rc): Array[Int32, _];
        let b = (Array#{1,2} @ rc): Array[Int32, _];
        Array.sameElements(a,b) == true
    }

    @Test
    def sameElements07(): Bool = region rc {
        let a = (Array#{1,2} @ rc): Array[Int32, _];
        let b = (Array#{2,1} @ rc): Array[Int32, _];
        Array.sameElements(a,b) == false
    }

    /////////////////////////////////////////////////////////////////////////////
    // forEach                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forEach01(): Bool = region rc {
        let a = (Array#{} @ rc): Array[Int32, _];
        let sb = StringBuilder.new(rc);
        let fn = x -> if (x > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        Array.forEach(fn, a);
        StringBuilder.toString(sb) == ""
    }

    @Test
    def forEach02(): Bool = region rc {
        let a = Array#{0} @ rc;
        let sb = StringBuilder.new(rc);
        let fn = x -> if (x > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        Array.forEach(fn, a);
        StringBuilder.toString(sb) == "F"
    }

    @Test
    def forEach03(): Bool = region rc {
        let a = Array#{1} @ rc;
        let sb = StringBuilder.new(rc);
        let fn = x -> if (x > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        Array.forEach(fn, a);
        StringBuilder.toString(sb) == "T"
    }

    @Test
    def forEach04(): Bool = region rc {
        let a = Array#{0,1} @ rc;
        let sb = StringBuilder.new(rc);
        let fn = x -> if (x > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        Array.forEach(fn, a);
        StringBuilder.toString(sb) == "FT"
    }

    /////////////////////////////////////////////////////////////////////////////
    // forEachWithIndex                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forEachWithIndex01(): Bool = region rc {
        let a = (Array#{} @ rc): Array[Float32, _];
        let sb = StringBuilder.new(rc);
        let fn = (ix, _) -> if (ix > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        Array.forEachWithIndex(fn, a);
        StringBuilder.toString(sb) == ""
    }

    @Test
    def forEachWithIndex02(): Bool = region rc {
        let a = Array#{0.0} @ rc;
        let sb = StringBuilder.new(rc);
        let fn = (ix, _) -> if (ix > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        Array.forEachWithIndex(fn, a);
        StringBuilder.toString(sb) == "F"
    }

    @Test
    def forEachWithIndex03(): Bool = region rc {
        let a = Array#{0.0, 0.1} @ rc;
        let sb = StringBuilder.new(rc);
        let fn = (ix, _) -> if (ix > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        Array.forEachWithIndex(fn, a);
        StringBuilder.toString(sb) == "FT"
    }

    @Test
    def forEachWithIndex04(): Bool = region rc {
        let a = Array#{0.0, 0.1, 0.2} @ rc;
        let sb = StringBuilder.new(rc);
        let fn = (ix, _) -> if (ix > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        Array.forEachWithIndex(fn, a);
        StringBuilder.toString(sb) == "FTT"
    }

    @Test
    def forEachWithIndex05(): Bool = region rc {
        let a = Array#{0.0, 0.1, 0.2, 0.3} @ rc;
        let sb = StringBuilder.new(rc);
        let fn = (ix, _) -> if (ix > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        Array.forEachWithIndex(fn, a);
        StringBuilder.toString(sb) == "FTTT"
    }

    /////////////////////////////////////////////////////////////////////////////
    // updateSequence                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def updateSequence01(): Bool = region rc {
         let a = Array.updateSequence(rc, 0, (Array#{} @ rc): Array[Int32, _], (Array#{} @ rc): Array[Int32, _]);
         Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
     }

    @Test
    def updateSequence02(): Bool = region rc {
        let a = Array.updateSequence(rc, 0, Array#{1,2} @ rc, Array#{} @ rc);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def updateSequence03(): Bool = region rc {
        let a = Array.updateSequence(rc, 0, Array#{} @ rc, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @Test
    def updateSequence04(): Bool = region rc {
        let a = Array.updateSequence(rc, -3, Array#{1,2,4} @ rc, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @Test
    def updateSequence05(): Bool = region rc {
        let a = Array.updateSequence(rc, 2, Array#{1,2,4} @ rc, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @Test
    def updateSequence06(): Bool = region rc {
        let a = Array.updateSequence(rc, 0, Array#{} @ rc, Array#{1} @ rc);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @Test
    def updateSequence07(): Bool = region rc {
        let a = Array.updateSequence(rc, 1, Array#{2} @ rc, Array#{1} @ rc);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @Test
    def updateSequence08(): Bool = region rc {
        let a = Array.updateSequence(rc, 0, Array#{2} @ rc, Array#{1} @ rc);
        Array.sameElements(a, Array#{2} @ rc)
    }

    @Test
    def updateSequence09(): Bool = region rc {
        let a = Array.updateSequence(rc, 0, Array#{2,4} @ rc, Array#{1} @ rc);
        Array.sameElements(a, Array#{2} @ rc)
    }

    @Test
    def updateSequence10(): Bool = region rc {
        let a = Array.updateSequence(rc, -1, Array#{2,4} @ rc, Array#{1} @ rc);
        Array.sameElements(a, Array#{4} @ rc)
    }

    @Test
    def updateSequence11(): Bool = region rc {
        let a = Array.updateSequence(rc, -1, Array#{3,4} @ rc, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{4,2} @ rc)
    }

    @Test
    def updateSequence12(): Bool = region rc {
        let a = Array.updateSequence(rc, 1, Array#{3,4} @ rc, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{1,3} @ rc)
    }

    @Test
    def updateSequence13(): Bool = region rc {
        let a = Array.updateSequence(rc, -2, Array#{3,4} @ rc, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @Test
    def updateSequence14(): Bool = region rc {
        let a = Array.updateSequence(rc, 2, Array#{3,4} @ rc, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @Test
    def updateSequence15(): Bool = region rc {
        let a = Array.updateSequence(rc, 1, Array#{3} @ rc, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{1,3} @ rc)
    }

    @Test
    def updateSequence16(): Bool = region rc {
        let a = Array.updateSequence(rc, 0, Array#{3,4} @ rc, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{3,4} @ rc)
    }

    @Test
    def updateSequence17(): Bool = region rc {
        let a = Array.updateSequence(rc, 0, Array#{4} @ rc, Array#{1, 2, 3} @ rc);
        Array.sameElements(a, Array#{4, 2, 3} @ rc)
    }

    @Test
    def updateSequence18(): Bool = region rc {
        let a = Array.updateSequence(rc, 1, Array#{4} @ rc, Array#{1, 2, 3} @ rc);
        Array.sameElements(a, Array#{1, 4, 3} @ rc)
    }

    @Test
    def updateSequence19(): Bool = region rc {
        let a = Array.updateSequence(rc, 2, Array#{4} @ rc, Array#{1, 2, 3} @ rc);
        Array.sameElements(a, Array#{1, 2, 4} @ rc)
    }

    @Test
    def updateSequence20(): Bool = region rc {
        let a = Array.updateSequence(rc, 0, Array#{4, 5} @ rc, Array#{1, 2, 3} @ rc);
        Array.sameElements(a, Array#{4, 5, 3} @ rc)
    }

    @Test
    def updateSequence21(): Bool = region rc {
        let a = Array.updateSequence(rc, 1, Array#{4, 5} @ rc, Array#{1, 2, 3} @ rc);
        Array.sameElements(a, Array#{1, 4, 5} @ rc)
    }

    @Test
    def updateSequence22(): Bool = region rc {
        let a = Array.updateSequence(rc, -1, Array#{4, 5, 6} @ rc, Array#{1, 2, 3} @ rc);
        Array.sameElements(a, Array#{5, 6, 3} @ rc)
    }

    @Test
    def updateSequence23(): Bool = region rc {
        let a = Array.updateSequence(rc, 0, Array#{4, 5, 6} @ rc, Array#{1, 2, 3} @ rc);
        Array.sameElements(a, Array#{4, 5, 6} @ rc)
    }

    @Test
    def updateSequence24(): Bool = region rc {
        let a = Array.updateSequence(rc, 2, Array#{14, 15, 16, 17} @ rc, Array#{1, 2, 3, 4, 5, 6, 7} @ rc);
        Array.sameElements(a, Array#{1, 2, 14, 15, 16, 17, 7} @ rc)
    }

    @Test
    def updateSequence25(): Bool = region rc {
        let a = Array.updateSequence(rc, -2, Array#{14, 15, 16, 17} @ rc, Array#{1, 2, 3, 4, 5, 6, 7} @ rc);
        Array.sameElements(a, Array#{16, 17, 3, 4, 5, 6, 7} @ rc)
    }

    @Test
    def updateSequence26(): Bool = region rc {
        let a = Array.updateSequence(rc, 4, Array#{14, 15, 16, 17} @ rc, Array#{1, 2, 3, 4, 5, 6, 7} @ rc);
        Array.sameElements(a, Array#{1, 2, 3, 4, 14, 15, 16} @ rc)
    }

    @Test
    def updateSequence27(): Bool = region rc {
        let a = Array.updateSequence(rc, 4, Array#{14, 15} @ rc, Array#{1, 2, 3, 4, 5, 6, 7} @ rc);
        Array.sameElements(a, Array#{1, 2, 3, 4, 14, 15, 7} @ rc)
    }

    @Test
    def updateSequence28(): Bool = region rc {
        let a = Array.updateSequence(rc, -1, Array#{-1, -2, -3, -4, -5, -6, -7, -8} @ rc, Array#{1, 2, 3, 4, 5, 6, 7} @ rc);
        Array.sameElements(a, Array#{-2, -3, -4, -5, -6, -7, -8} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // updateSequence!                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def updateSequence01!(): Bool = region rc {
         let a = (Array#{} @ rc): Array[Int32, _];
         Array.updateSequence!(0, (Array#{} @ rc): Array[Int32, _], a);
         Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
     }

    @Test
    def updateSequence02!(): Bool = region rc {
        let a = (Array#{} @ rc): Array[Int32, _];
        Array.updateSequence!(0, Array#{1,2} @ rc, a);
        Array.sameElements(a, (Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def updateSequence03!(): Bool = region rc {
        let a = Array#{1,2} @ rc;
        Array.updateSequence!(0, Array#{} @ rc, a);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @Test
    def updateSequence04!(): Bool = region rc {
        let a = Array#{1,2} @ rc;
        Array.updateSequence!(-3, Array#{1,2,4} @ rc, a);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @Test
    def updateSequence05!(): Bool = region rc {
        let a = Array#{1,2} @ rc;
        Array.updateSequence!(2, Array#{1,2,4} @ rc, a);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @Test
    def updateSequence06!(): Bool = region rc {
        let a = Array#{1} @ rc;
        Array.updateSequence!(0, Array#{} @ rc, a);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @Test
    def updateSequence07!(): Bool = region rc {
        let a = Array#{1} @ rc;
        Array.updateSequence!(1, Array#{2} @ rc, a);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @Test
    def updateSequence08!(): Bool = region rc {
        let a = Array#{1} @ rc;
        Array.updateSequence!(0, Array#{2} @ rc, a);
        Array.sameElements(a, Array#{2} @ rc)
    }

    @Test
    def updateSequence09!(): Bool = region rc {
        let a = Array#{1} @ rc;
        Array.updateSequence!(0, Array#{2,4} @ rc, a);
        Array.sameElements(a, Array#{2} @ rc)
    }

    @Test
    def updateSequence10!(): Bool = region rc {
        let a = Array#{1} @ rc;
        Array.updateSequence!(-1, Array#{2,4} @ rc, a);
        Array.sameElements(a, Array#{4} @ rc)
    }

    @Test
    def updateSequence11!(): Bool = region rc {
        let a = Array#{1,2} @ rc;
        Array.updateSequence!(-1, Array#{3,4} @ rc, a);
        Array.sameElements(a, Array#{4,2} @ rc)
    }

    @Test
    def updateSequence12!(): Bool = region rc {
        let a = Array#{1,2} @ rc;
        Array.updateSequence!(1, Array#{3,4} @ rc, a);
        Array.sameElements(a, Array#{1,3} @ rc)
    }

    @Test
    def updateSequence13!(): Bool = region rc {
        let a = Array#{1,2} @ rc;
        Array.updateSequence!(-2, Array#{3,4} @ rc, a);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @Test
    def updateSequence14!(): Bool = region rc {
        let a = Array#{1,2} @ rc;
        Array.updateSequence!(2, Array#{3,4} @ rc, a);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @Test
    def updateSequence15!(): Bool = region rc {
        let a = Array#{1,2} @ rc;
        Array.updateSequence!(1, Array#{3} @ rc, a);
        Array.sameElements(a, Array#{1,3} @ rc)
    }

    @Test
    def updateSequence16!(): Bool = region rc {
        let a = Array#{1,2} @ rc;
        Array.updateSequence!(0, Array#{3,4} @ rc, a);
        Array.sameElements(a, Array#{3,4} @ rc)
    }

    @Test
    def updateSequence17!(): Bool = region rc {
        let a = Array#{1, 2, 3} @ rc;
        Array.updateSequence!(0, Array#{4} @ rc, a);
        Array.sameElements(a, Array#{4, 2, 3} @ rc)
    }

    @Test
    def updateSequence18!(): Bool = region rc {
        let a = Array#{1, 2, 3} @ rc;
        Array.updateSequence!(1, Array#{4} @ rc, a);
        Array.sameElements(a, Array#{1, 4, 3} @ rc)
    }

    @Test
    def updateSequence19!(): Bool = region rc {
        let a = Array#{1, 2, 3} @ rc;
        Array.updateSequence!(2, Array#{4} @ rc, a);
        Array.sameElements(a, Array#{1, 2, 4} @ rc)
    }

    @Test
    def updateSequence20!(): Bool = region rc {
        let a = Array#{1, 2, 3} @ rc;
        Array.updateSequence!(0, Array#{4, 5} @ rc, a);
        Array.sameElements(a, Array#{4, 5, 3} @ rc)
    }

    @Test
    def updateSequence21!(): Bool = region rc {
        let a = Array#{1, 2, 3} @ rc;
        Array.updateSequence!(1, Array#{4, 5} @ rc, a);
        Array.sameElements(a, Array#{1, 4, 5} @ rc)
    }

    @Test
    def updateSequence22!(): Bool = region rc {
        let a = Array#{1, 2, 3} @ rc;
        Array.updateSequence!(-1, Array#{4, 5, 6} @ rc, a);
        Array.sameElements(a, Array#{5, 6, 3} @ rc)
    }

    @Test
    def updateSequence23!(): Bool = region rc {
        let a = Array#{1, 2, 3} @ rc;
        Array.updateSequence!(0, Array#{4, 5, 6} @ rc, a);
        Array.sameElements(a, Array#{4, 5, 6} @ rc)
    }

    @Test
    def updateSequence24!(): Bool = region rc {
        let a = Array#{1, 2, 3, 4, 5, 6, 7} @ rc;
        Array.updateSequence!(2, Array#{14, 15, 16, 17} @ rc, a);
        Array.sameElements(a, Array#{1, 2, 14, 15, 16, 17, 7} @ rc)
    }

    @Test
    def updateSequence25!(): Bool = region rc {
        let a = Array#{1, 2, 3, 4, 5, 6, 7} @ rc;
        Array.updateSequence!(-2, Array#{14, 15, 16, 17} @ rc, a);
        Array.sameElements(a, Array#{16, 17, 3, 4, 5, 6, 7} @ rc)
    }

    @Test
    def updateSequence26!(): Bool = region rc {
        let a = Array#{1, 2, 3, 4, 5, 6, 7} @ rc;
        Array.updateSequence!(4, Array#{14, 15, 16, 17} @ rc, a);
        Array.sameElements(a, Array#{1, 2, 3, 4, 14, 15, 16} @ rc)
    }

    @Test
    def updateSequence27!(): Bool = region rc {
        let a = Array#{1, 2, 3, 4, 5, 6, 7} @ rc;
        Array.updateSequence!(4, Array#{14, 15} @ rc, a);
        Array.sameElements(a, Array#{1, 2, 3, 4, 14, 15, 7} @ rc)
    }

    @Test
    def updateSequence28!(): Bool = region rc {
        let a = Array#{1, 2, 3, 4, 5, 6, 7} @ rc;
        Array.updateSequence!(-1, Array#{-1, -2, -3, -4, -5, -6, -7, -8} @ rc, a);
        Array.sameElements(a, Array#{-2, -3, -4, -5, -6, -7, -8} @ rc)
    }


    /////////////////////////////////////////////////////////////////////////////
    // sortWith                                                                //
    /////////////////////////////////////////////////////////////////////////////

    def cmp(x: Int32, y: Int32): Comparison =
        if (x < y) Comparison.LessThan
        else if (x == y) Comparison.EqualTo
        else Comparison.GreaterThan

    @Test
    def sortWith01(): Bool = region rc {
        let a = Array.sortWith(rc, cmp, (Array#{} @ rc): Array[Int32, _]);
        a `sameElements` (Array#{} @ rc): Array[Int32, _]
    }

    @Test
    def sortWith02(): Bool = region rc {
        let a = Array.sortWith(rc, cmp, Array#{0} @ rc);
        a `sameElements` Array#{0} @ rc
    }

    @Test
    def sortWith03(): Bool = region rc {
        let a = Array.sortWith(rc, cmp, Array#{0,1} @ rc);
        a `sameElements` Array#{0,1} @ rc
    }

    @Test
    def sortWith04(): Bool = region rc {
        let a = Array.sortWith(rc, cmp, Array#{1,0} @ rc);
        a `sameElements` Array#{0,1} @ rc
    }

    @Test
    def sortWith05(): Bool = region rc {
        let a = Array.sortWith(rc, cmp, Array#{1,1} @ rc);
        a `sameElements` Array#{1,1} @ rc
    }

    @Test
    def sortWith06(): Bool = region rc {
        let a = Array.sortWith(rc, cmp, Array#{0,1,2,3,4,5} @ rc);
        a `sameElements` Array#{0,1,2,3,4,5} @ rc
    }

    @Test
    def sortWith07(): Bool = region rc {
        let a = Array.sortWith(rc, cmp, Array#{5,4,3,2,1,0} @ rc);
        a `sameElements` Array#{0,1,2,3,4,5} @ rc
    }

    @Test
    def sortWith08(): Bool = region rc {
        let a = Array.sortWith(rc, cmp, Array#{5,3,0,4,1,2} @ rc);
        a `sameElements` Array#{0,1,2,3,4,5} @ rc
    }

    @Test
    def sortWith09(): Bool = region rc {
        let a = Array.sortWith(rc, cmp, Array#{2,3,0,4,1,2} @ rc);
        a `sameElements` Array#{0,1,2,2,3,4} @ rc
    }

    @Test
    def sortWith10(): Bool = region rc {
        let a = Array.sortWith(rc, flip(cmp), Array#{0,1,2,3,4,5} @ rc);
        a `sameElements` Array#{5,4,3,2,1,0} @ rc
    }

    @Test
    def sortWith11(): Bool = region rc {
        let a = Array.sortWith(rc, flip(cmp), Array#{5,4,3,2,1,0} @ rc);
        a `sameElements` Array#{5,4,3,2,1,0} @ rc
    }

    @Test
    def sortWith12(): Bool = region rc {
        let a = Array.sortWith(rc, flip(cmp), Array#{5,3,0,4,1,2} @ rc);
        a `sameElements` Array#{5,4,3,2,1,0} @ rc
    }

    @Test
    def sortWith13(): Bool = region rc {
        let a = Array.sortWith(rc, flip(cmp), Array#{2,3,0,4,1,2} @ rc);
        a `sameElements` Array#{4,3,2,2,1,0} @ rc
    }

    /////////////////////////////////////////////////////////////////////////////
    // sort                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    def testSortVsSortWith(a: Array[Int32, r]): Bool \ r = region rc1 {
        Array.sort(rc1, a) `sameElements` Array.sortWith(rc1, cmp, a)
    }

    @Test
    def sort01(): Bool = region rc {
        testSortVsSortWith((Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def sort02(): Bool = region rc {
        testSortVsSortWith(Array#{0} @ rc)
    }

    @Test
    def sort03(): Bool = region rc {
        testSortVsSortWith(Array#{0,1} @ rc)
    }

    @Test
    def sort04(): Bool = region rc {
        testSortVsSortWith(Array#{1,0} @ rc)
    }

    @Test
    def sort05(): Bool = region rc {
        testSortVsSortWith(Array#{1,1} @ rc)
    }

    @Test
    def sort06(): Bool = region rc {
        testSortVsSortWith(Array#{0,1,2,3,4,5} @ rc)
    }

    @Test
    def sort07(): Bool = region rc {
        testSortVsSortWith(Array#{5,4,3,2,1,0} @ rc)
    }

    @Test
    def sort08(): Bool = region rc {
        testSortVsSortWith(Array#{5,3,0,4,1,2} @ rc)
    }

    @Test
    def sort09(): Bool = region rc {
        testSortVsSortWith(Array#{2,3,0,4,1,2} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // sortWith!                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sortWith01!(): Bool = region rc {
        let a = (Array#{} @ rc): Array[Int32, _];
        Array.sortWith!(cmp, a);
        a `sameElements` (Array#{} @ rc): Array[Int32, _]
    }

    @Test
    def sortWith02!(): Bool = region rc {
        let a = Array#{0} @ rc;
        Array.sortWith!(cmp, a);
        a `sameElements` Array#{0} @ rc
    }

    @Test
    def sortWith03!(): Bool = region rc {
        let a = Array#{0,1} @ rc;
        Array.sortWith!(cmp, a);
        a `sameElements` Array#{0,1} @ rc
    }

    @Test
    def sortWith04!(): Bool = region rc {
        let a = Array#{1,0} @ rc;
        Array.sortWith!(cmp, a);
        a `sameElements` Array#{0,1} @ rc
    }

    @Test
    def sortWith05!(): Bool = region rc {
        let a = Array#{1,1} @ rc;
        Array.sortWith!(cmp, a);
        a `sameElements` Array#{1,1} @ rc
    }

    @Test
    def sortWith06!(): Bool = region rc {
        let a = Array#{0,1,2,3,4,5} @ rc;
        Array.sortWith!(cmp, a);
        a `sameElements` Array#{0,1,2,3,4,5} @ rc
    }

    @Test
    def sortWith07!(): Bool = region rc {
        let a = Array#{5,4,3,2,1,0} @ rc;
        Array.sortWith!(cmp, a);
        a `sameElements` Array#{0,1,2,3,4,5} @ rc
    }

    @Test
    def sortWith08!(): Bool = region rc {
        let a = Array#{5,3,0,4,1,2} @ rc;
        Array.sortWith!(cmp, a);
        a `sameElements` Array#{0,1,2,3,4,5} @ rc
    }

    @Test
    def sortWith09!(): Bool = region rc {
        let a = Array#{2,3,0,4,1,2} @ rc;
        Array.sortWith!(cmp, a);
        a `sameElements` Array#{0,1,2,2,3,4} @ rc
    }

    @Test
    def sortWith10!(): Bool = region rc {
        let a = Array#{0,1,2,3,4,5} @ rc;
        Array.sortWith!(flip(cmp), a);
        a `sameElements` Array#{5,4,3,2,1,0} @ rc
    }

    @Test
    def sortWith11!(): Bool = region rc {
        let a = Array#{5,4,3,2,1,0} @ rc;
        Array.sortWith!(flip(cmp), a);
        a `sameElements` Array#{5,4,3,2,1,0} @ rc
    }

    @Test
    def sortWith12!(): Bool = region rc {
        let a = Array#{5,3,0,4,1,2} @ rc;
        Array.sortWith!(flip(cmp), a);
        a `sameElements` Array#{5,4,3,2,1,0} @ rc
    }

    @Test
    def sortWith13!(): Bool = region rc {
        let a = Array#{2,3,0,4,1,2} @ rc;
        Array.sortWith!(flip(cmp), a);
        a `sameElements` Array#{4,3,2,2,1,0} @ rc
    }

    /////////////////////////////////////////////////////////////////////////////
    // sort!                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    def testSort!VsSortWith!(a: Array[Int32, r]): Bool \ r = region rc1 {
        let b = Array.slice(rc1, start = 0, end = Array.length(a), a);
        let c = Array.slice(rc1, start = 0, end = Array.length(a), a);
        Array.sort!(b);
        Array.sortWith!(cmp, c);
        b `sameElements` c
    }

    @Test
    def sort!01(): Bool = region rc {
        testSort!VsSortWith!((Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def sort!02(): Bool = region rc {
        testSort!VsSortWith!(Array#{0} @ rc)
    }

    @Test
    def sort!03(): Bool = region rc {
        testSort!VsSortWith!(Array#{0,1} @ rc)
    }

    @Test
    def sort!04(): Bool = region rc {
        testSort!VsSortWith!(Array#{1,0} @ rc)
    }

    @Test
    def sort!05(): Bool = region rc {
        testSort!VsSortWith!(Array#{1,1} @ rc)
    }

    @Test
    def sort!06(): Bool = region rc {
        testSort!VsSortWith!(Array#{0,1,2,3,4,5} @ rc)
    }

    @Test
    def sort!07(): Bool = region rc {
        testSort!VsSortWith!(Array#{5,4,3,2,1,0} @ rc)
    }

    @Test
    def sort!08(): Bool = region rc {
        testSort!VsSortWith!(Array#{5,3,0,4,1,2} @ rc)
    }

    @Test
    def sort!09(): Bool = region rc {
        testSort!VsSortWith!(Array#{2,3,0,4,1,2} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // sortBy                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    def testSortByVsSort(a: Array[Int32, r]): Bool \ r = region rc1 {
        (Array.sortBy(rc1, identity, a) `sameElements` Array.sort(rc1, a)) and
        (Array.sortBy(rc1, x -> 4 * x + 7, a) `sameElements` Array.sort(rc1, a)) and
        (Array.sortBy(rc1, x -> -x, a) `sameElements` Array.sortWith(rc1, flip(cmp),a))
    }

    @Test
    def sortBy01(): Bool = region rc {
        testSortByVsSort((Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def sortBy02(): Bool = region rc {
        testSortByVsSort(Array#{0} @ rc)
    }

    @Test
    def sortBy03(): Bool = region rc {
        testSortByVsSort(Array#{0,1} @ rc)
    }

    @Test
    def sortBy04(): Bool = region rc {
        testSortByVsSort(Array#{1,0} @ rc)
    }

    @Test
    def sortBy05(): Bool = region rc {
        testSortByVsSort(Array#{1,1} @ rc)
    }

    @Test
    def sortBy06(): Bool = region rc {
        testSortByVsSort(Array#{0,1,2,3,4,5} @ rc)
    }

    @Test
    def sortBy07(): Bool = region rc {
        testSortByVsSort(Array#{5,4,3,2,1,0} @ rc)
    }

    @Test
    def sortBy08(): Bool = region rc {
        testSortByVsSort(Array#{5,3,0,4,1,2} @ rc)
    }

    @Test
    def sortBy09(): Bool = region rc {
        testSortByVsSort(Array#{2,3,0,4,1,2} @ rc)
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
        Array.sortBy(rc, r -> let R.R(x) = r; x.i, Array#{R.R({i = 2, s = "A"}), R.R({i = 1, s = "B"}), R.R({i = 3, s = "C"})} @ rc)
        `sameElements` Array#{R.R({i = 1, s = "B"}), R.R({i = 2, s = "A"}), R.R({i = 3, s = "C"})} @ rc
    }

    /////////////////////////////////////////////////////////////////////////////
    // sortBy!                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    def testSortBy!VsSortBy(a: Array[Int32, r]) : Bool \ r = region rc1 {
        let b = Array.slice(rc1, start = 0, end = Array.length(a), a);
        let c = Array.slice(rc1, start = 0, end = Array.length(a), a);
        Array.sortBy!(identity, b);
        Array.sortBy!(x -> 4 * x + 7, c);
        (b `sameElements` Array.sortBy(rc1, x -> 4 * x + 7, a)) and
        (c `sameElements` Array.sortBy(rc1, identity, a))
    }

    @Test
    def sortBy!01(): Bool = region rc {
        testSortBy!VsSortBy((Array#{} @ rc): Array[Int32, _])
    }

    @Test
    def sortBy!02(): Bool = region rc {
        testSortBy!VsSortBy(Array#{0} @ rc)
    }

    @Test
    def sortBy!03(): Bool = region rc {
        testSortBy!VsSortBy(Array#{0,1} @ rc)
    }

    @Test
    def sortBy!04(): Bool = region rc {
        testSortBy!VsSortBy(Array#{1,0} @ rc)
    }

    @Test
    def sortBy!05(): Bool = region rc {
        testSortBy!VsSortBy(Array#{1,1} @ rc)
    }

    @Test
    def sortBy!06(): Bool = region rc {
        testSortBy!VsSortBy(Array#{0,1,2,3,4,5} @ rc)
    }

    @Test
    def sortBy!07(): Bool = region rc {
        testSortBy!VsSortBy(Array#{5,4,3,2,1,0} @ rc)
    }

    @Test
    def sortBy!08(): Bool = region rc {
        testSortBy!VsSortBy(Array#{5,3,0,4,1,2} @ rc)
    }

    @Test
    def sortBy!09(): Bool = region rc {
        testSortBy!VsSortBy(Array#{2,3,0,4,1,2} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // minimumBy                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def minimumBy01(): Bool = region rc {
        Array.minimumBy((x, y) -> x <=> y, (Array#{} @ rc): Array[Int32, _]) == None
    }

    @Test
    def minimumBy02(): Bool = region rc {
        Array.minimumBy((x, y) -> x <=> y, Array.range(rc, 0, 5)) == Some(0)
    }

    @Test
    def minimumBy03(): Bool = region rc {
        Array.minimumBy((x, y) -> x <=> y, Array.range(rc, 4, 10)) == Some(4)
    }

    @Test
    def minimumBy04(): Bool = region rc {
        let a = Array.range(rc, 10, 20);
        Array.put(-22, 5, a);
        Array.minimumBy((x, y) -> x <=> y, a) == Some(-22)
    }

    /////////////////////////////////////////////////////////////////////////////
    // maximumBy                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def maximumBy01(): Bool = region rc {
        Array.maximumBy((x, y) -> x <=> y, (Array#{} @ rc): Array[Int32, _]) == None
    }

    @Test
    def maximumBy02(): Bool = region rc {
        Array.maximumBy((x, y) -> x <=> y, Array.range(rc, 0, 5)) == Some(4)
    }

    @Test
    def maximumBy03(): Bool = region rc {
        Array.maximumBy((x, y) -> x <=> y, Array.range(rc, 4, 10)) == Some(9)
    }

    @Test
    def maximumBy04(): Bool = region rc {
        let a = Array.range(rc, 10, 20);
        Array.put(42, 5, a);
        Array.maximumBy((x, y) -> x <=> y, a) == Some(42)
    }


    /////////////////////////////////////////////////////////////////////////////
    // sum                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sum01(): Bool = region rc {
        Array.sum(Array#{} @ rc) == 0
    }

    @Test
    def sum02(): Bool = region rc {
        Array.sum(Array#{1} @ rc) == 1
    }

    @Test
    def sum03(): Bool = region rc {
        Array.sum(Array#{1, 2, 3} @ rc) == 6
    }

    @Test
    def sum04(): Bool = region rc {
        Array.sum(Array#{1, 2, 3, -3} @ rc) == 3
    }

    @Test
    def sum05(): Bool = region rc {
        Array.sum(Array#{-1, -2, -3, -4} @ rc) == -10
    }
    @Test
    def sum06(): Bool = region rc {
        Array.sum(Array#{10, -10} @ rc) == 0
    }

    @Test
    def sum07(): Bool = region rc {
        Array.range(rc, 1, 101) |> Array.sum == 5050
    }


    /////////////////////////////////////////////////////////////////////////////
    // sumWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sumWith01(): Bool = region rc {
        Array.sumWith(x -> x + 1, Array#{} @ rc) == 0
    }

    @Test
    def sumWith02(): Bool = region rc {
        Array.sumWith(x -> x + 1, Array#{1} @ rc) == 2
    }

    @Test
    def sumWith03(): Bool = region rc {
        Array.sumWith(x -> x + 1, Array#{1, 2, 3} @ rc) == 9
    }

    @Test
    def sumWith04(): Bool = region rc {
        Array.sumWith(x -> x + 1, Array#{1, 2, 3, -3} @ rc) == 7
    }

    @Test
    def sumWith05(): Bool = region rc {
        Array.sumWith(x -> x + 1, Array#{-1, -2, -3, -4} @ rc) == -6
    }

    @Test
    def sumWith06(): Bool = region rc {
        Array.sumWith(x -> x + 1, Array#{10, -10} @ rc) == 2
    }


    /////////////////////////////////////////////////////////////////////////////
    // toDelayList                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toDelayList01(): Bool = region rc {
        (Array#{} @ rc): Array[Unit, _] |> Array.toDelayList == DelayList.empty()
    }

    @Test
    def toDelayList02(): Bool = region rc {
        (Array.new(rc, 0): Array[String, rc]) |> Array.toDelayList == DelayList.empty()
    }

    @Test
    def toDelayList03(): Bool = region rc {
        Array.range(rc, -1000, 1000) |> Array.toDelayList == DelayList.range(-1000, 1000)
    }

    @Test
    def toDelayList04(): Bool = region rc {
        Array.range(rc, -1000, 1000) |> Array.toDelayList |> DelayList.toArray(rc) `sameElements` Array.range(rc, -1000, 1000)
    }

    /////////////////////////////////////////////////////////////////////////////
    // toChain                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toChain01(): Bool = region rc {
        Array.toChain((Array#{} @ rc): Array[Unit, _]) == Chain.empty(): Chain[Unit]
    }

    @Test
    def toChain02(): Bool = region rc {
        Array.toChain(Array#{1} @ rc) == Chain.singleton(1)
    }

    @Test
    def toChain03(): Bool = region rc {
        Array.toChain(Array#{1,2} @ rc) == List.toChain(1 :: 2 :: Nil)
    }

    @Test
    def toChain04(): Bool = region rc {
        Array.toChain(Array#{1,2,3} @ rc) == List.toChain(1 :: 2 :: 3 :: Nil)
    }

    /////////////////////////////////////////////////////////////////////////////
    // toNec                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toNec01(): Bool = region rc {
        Array.toNec((Array#{} @ rc): Array[Unit, _]) == None
    }

    @Test
    def toNec02(): Bool = region rc {
        Array.toNec(Array#{1} @ rc) == Some(Nec.singleton(1))
    }

    @Test
    def toNec03(): Bool = region rc {
        Array.toNec(Array#{1,2} @ rc) == List.toNec(1 :: 2 :: Nil)
    }

    @Test
    def toNec04(): Bool = region rc {
        Array.toNec(Array#{1,2,3} @ rc) == List.toNec(1 :: 2 :: 3 :: Nil)
    }

    /////////////////////////////////////////////////////////////////////////////
    // copyOfRange                                                             //
    /////////////////////////////////////////////////////////////////////////////

    // depends on flatten

    @Test
    def copyOfRange01(): Bool = region rc {
        Array.copyOfRange(rc, 0, 3, Array#{true, false, true} @ rc) `sameElements` Array#{true, false, true} @ rc
    }

    @Test
    def copyOfRange02(): Bool = region rc {
        Array.copyOfRange(rc, 1, 3, Array#{true, false, true} @ rc) `sameElements` Array#{false, true} @ rc
    }

    @Test
    def copyOfRange03(): Bool = region rc {
        let a = Array.copyOfRange(rc, 2, 5, Array#{true, false, true} @ rc);
        Array.length(a) == 3 and Array.memberOf(true, a)
    }

    @Test
    def copyOfRange04(): Bool = region rc {
        Array.copyOfRange(rc, 0, 3, Array#{'a', 'b', 'c'} @ rc) `sameElements` Array#{'a', 'b', 'c'} @ rc
    }

    @Test
    def copyOfRange05(): Bool = region rc {
        Array.copyOfRange(rc, 1, 3, Array#{'a', 'b', 'c'} @ rc) `sameElements` Array#{'b', 'c'} @ rc
    }

    @Test
    def copyOfRange06(): Bool = region rc {
        let a = Array.copyOfRange(rc, 2, 5, Array#{'a', 'b', 'c'} @ rc);
        Array.length(a) == 3 and Array.memberOf('c', a)
    }

    @Test
    def copyOfRange07(): Bool = region rc {
        Array.copyOfRange(rc, 0, 3, Array#{1.0f32, 2.0f32, 3.0f32} @ rc) `sameElements` Array#{1.0f32, 2.0f32, 3.0f32} @ rc
    }

    @Test
    def copyOfRange08(): Bool = region rc {
        Array.copyOfRange(rc, 1, 3, Array#{1.0f32, 2.0f32, 3.0f32} @ rc) `sameElements` Array#{2.0f32, 3.0f32} @ rc
    }

    @Test
    def copyOfRange09(): Bool = region rc {
        let a = Array.copyOfRange(rc, 2, 5, Array#{1.0f32, 2.0f32, 3.0f32} @ rc);
        Array.length(a) == 3 and Array.memberOf(3.0f32, a)
    }

    @Test
    def copyOfRange10(): Bool = region rc {
        Array.copyOfRange(rc, 0, 3, Array#{1.0f64, 2.0f64, 3.0f64} @ rc) `sameElements` Array#{1.0f64, 2.0f64, 3.0f64} @ rc
    }

    @Test
    def copyOfRange11(): Bool = region rc {
        Array.copyOfRange(rc, 1, 3, Array#{1.0f64, 2.0f64, 3.0f64} @ rc) `sameElements` Array#{2.0f64, 3.0f64} @ rc
    }

    @Test
    def copyOfRange12(): Bool = region rc {
        let a = Array.copyOfRange(rc, 2, 5, Array#{1.0f64, 2.0f64, 3.0f64} @ rc);
        Array.length(a) == 3 and Array.memberOf(3.0f64, a)
    }

    @Test
    def copyOfRange13(): Bool = region rc {
        Array.copyOfRange(rc, 0, 3, Array#{1i8, 2i8, 3i8} @ rc) `sameElements` Array#{1i8, 2i8, 3i8} @ rc
    }

    @Test
    def copyOfRange14(): Bool = region rc {
        Array.copyOfRange(rc, 1, 3, Array#{1i8, 2i8, 3i8} @ rc) `sameElements` Array#{2i8, 3i8} @ rc
    }

    @Test
    def copyOfRange15(): Bool = region rc {
        let a = Array.copyOfRange(rc, 2, 5, Array#{1i8, 2i8, 3i8} @ rc);
        Array.length(a) == 3 and Array.memberOf(3i8, a)
    }

    @Test
    def copyOfRange16(): Bool = region rc {
        Array.copyOfRange(rc, 0, 3, Array#{1i16, 2i16, 3i16} @ rc) `sameElements` Array#{1i16, 2i16, 3i16} @ rc
    }

    @Test
    def copyOfRange17(): Bool = region rc {
        Array.copyOfRange(rc, 1, 3, Array#{1i16, 2i16, 3i16} @ rc) `sameElements` Array#{2i16, 3i16} @ rc
    }

    @Test
    def copyOfRange18(): Bool = region rc {
        let a = Array.copyOfRange(rc, 2, 5, Array#{1i16, 2i16, 3i16} @ rc);
        Array.length(a) == 3 and Array.memberOf(3i16, a)
    }

    @Test
    def copyOfRange19(): Bool = region rc {
        Array.copyOfRange(rc, 0, 3, Array#{1, 2, 3} @ rc) `sameElements` Array#{1, 2, 3} @ rc
    }

    @Test
    def copyOfRange20(): Bool = region rc {
        Array.copyOfRange(rc, 1, 3, Array#{1, 2, 3} @ rc) `sameElements` Array#{2, 3} @ rc
    }

    @Test
    def copyOfRange21(): Bool = region rc {
        let a = Array.copyOfRange(rc, 2, 5, Array#{1, 2, 3} @ rc);
        Array.length(a) == 3 and Array.memberOf(3, a)
    }

    @Test
    def copyOfRange22(): Bool = region rc {
        Array.copyOfRange(rc, 0, 3, Array#{1i64, 2i64, 3i64} @ rc) `sameElements` Array#{1i64, 2i64, 3i64} @ rc
    }

    @Test
    def copyOfRange23(): Bool = region rc {
        Array.copyOfRange(rc, 1, 3, Array#{1i64, 2i64, 3i64} @ rc) `sameElements` Array#{2i64, 3i64} @ rc
    }

    @Test
    def copyOfRange24(): Bool = region rc {
        let a = Array.copyOfRange(rc, 2, 5, Array#{1i64, 2i64, 3i64} @ rc);
        Array.length(a) == 3 and Array.memberOf(3i64, a)
    }

    @Test
    def copyOfRange25(): Bool = region rc {
        let a = Array.copyOfRange(rc, 0, 3, Array#{Array#{'a', 'b', 'c'} @ rc, Array#{'d', 'e', 'f', 'g'} @ rc, Array#{'h'} @ rc} @ rc);
        let b = Array#{Array#{'a', 'b', 'c'} @ rc, Array#{'d', 'e', 'f', 'g'} @ rc, Array#{'h'} @ rc} @ rc;
        Array.flatten(rc, a) `sameElements` Array.flatten(rc, b)
    }

    @Test
    def copyOfRange26(): Bool = region rc {
        let a = Array.copyOfRange(rc, 1, 3, Array#{Array#{'a', 'b', 'c'} @ rc, Array#{'d', 'e', 'f', 'g'} @ rc, Array#{'h'} @ rc} @ rc);
        let b = Array#{Array#{'d', 'e', 'f', 'g'} @ rc, Array#{'h'} @ rc } @ rc;
        Array.flatten(rc, a) `sameElements` Array.flatten(rc, b)
    }

    @Test
    def copyOfRange27(): Bool = region rc {
        Array.copyOfRange(rc, 0, 3, Array#{Some(1), Some(2), None} @ rc) `sameElements` Array#{Some(1), Some(2), None} @ rc
    }

    @Test
    def copyOfRange28(): Bool = region rc {
        Array.copyOfRange(rc, 1, 3, Array#{Some(1), Some(2), None} @ rc) `sameElements` Array#{Some(2), None} @ rc
    }

    @Test
    def copyOfRange29(): Bool = region rc {
        let a = Array.copyOfRange(rc, 2, 5, Array#{Some(1), Some(2), None} @ rc);
        Array.length(a) == 3 and Array.memberOf(None, a)
    }

    @Test
    def copyOfRange30(): Bool = region rc {
        let a = Array.copyOfRange(rc, 0, 3, Array#{Array#{Some('a'), Some('b'), Some('c')} @ rc, Array#{Some('d'), Some('e'), Some('f'), Some('g')} @ rc, Array#{None} @ rc} @ rc);
        let b = Array#{Array#{Some('a'), Some('b'), Some('c')} @ rc, Array#{Some('d'), Some('e'), Some('f'), Some('g')} @ rc, Array#{None} @ rc } @ rc;
        Array.flatten(rc, a) `sameElements` Array.flatten(rc, b)
    }

    @Test
    def copyOfRange31(): Bool = region rc {
        let a = Array.copyOfRange(rc, 1, 3, Array#{Array#{Some('a'), Some('b'), Some('c')} @ rc, Array#{Some('d'), Some('e'), Some('f'), Some('g')} @ rc, Array#{None} @ rc} @ rc);
        let b = Array#{Array#{Some('d'), Some('e'), Some('f'), Some('g')} @ rc, Array#{None} @ rc} @ rc;
        Array.flatten(rc, a) `sameElements` Array.flatten(rc, b)
    }

    /////////////////////////////////////////////////////////////////////////////
    // iterator                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def iterator01(): Bool = region rc {
        (Array#{} @ rc): Array[Int32, rc] |> Array.iterator(rc) |> Iterator.toList == Nil
    }

    @Test
    def iterator02(): Bool = region rc {
        (Array#{1, 2, 3} @ rc) |> Array.iterator(rc) |> Iterator.toList == 1 :: 2 :: 3 :: Nil
    }

    @Test
    def iterator03(): Bool = region rc {
        (Array#{1, 2, 3} @ rc) |> Array.iterator(rc) |> Iterator.toArray(rc) `sameElements` Array#{1, 2, 3} @ rc
    }

    @Test
    def iterator04(): Bool = region rc {
        Array.range(rc, -100, 100) |> Array.iterator(rc) |> Iterator.toArray(rc) `sameElements` Array.range(rc, -100, 100)
    }


    /////////////////////////////////////////////////////////////////////////////
    // join                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def join01(): Bool = region rc {
        Array.join(",", (Array#{} @ rc): Array[Int32, _]) == ""
    }

    @Test
    def join02(): Bool = region rc {
        Array.join(",", Array#{1} @ rc) == "1"
    }

    @Test
    def join03(): Bool = region rc {
        Array.join(",", Array#{1, 2, 3} @ rc) == "1,2,3"
    }

    @Test
    def join04(): Bool = region rc {
        Array.join(",", Array#{"1", "2", "3"} @ rc) == "1,2,3"
    }


    /////////////////////////////////////////////////////////////////////////////
    // joinWith                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def joinWith01(): Bool = region rc {
        Array.joinWith(x -> "${x + 1}", ",", Array#{} @ rc) == ""
    }

    @Test
    def joinWith02(): Bool = region rc {
        Array.joinWith(x -> "${x + 1}", ",", Array#{1} @ rc) == "2"
    }

    @Test
    def joinWith03(): Bool = region rc {
        Array.joinWith(x -> "${x + 1}", ",", Array#{1, 2, 3} @ rc) == "2,3,4"
    }

    @Test
    def joinWith04(): Bool = region rc {
        Array.joinWith(x -> x + x, ",", Array#{"1", "2", "3"} @ rc) == "11,22,33"
    }

    @Test
    def joinWith05(): Bool = region rc {
        let a: Array[Int32, _] = Array#{} @ rc;
        let s = Array.joinWith(Int32.toString, ",", a);
        s == ""
    }

    @Test
    def joinWith06(): Bool = region rc {
        let a = Array#{1} @ rc;
        let s = Array.joinWith(Int32.toString, ",", a);
        s == "1"
    }

    @Test
    def joinWith07(): Bool = region rc {
        let a = Array#{1,2} @ rc;
        let s = Array.joinWith(Int32.toString, ",", a);
        s == "1,2"
    }

    @Test
    def joinWith08(): Bool = region rc {
        let a = Array#{1,2,3} @ rc;
        let s = Array.joinWith(Int32.toString, ",", a);
        s == "1,2,3"
    }

    @Test
    def joinWith09(): Bool = region rc {
        let a = Array#{1,2,3} @ rc;
        let s = Array.joinWith(Int32.toString, "", a);
        s == "123"
    }

    @Test
    def joinWith10(): Bool = region rc {
        let a = Array#{1,2,3} @ rc;
        let s = Array.joinWith(Int32.toString, "..", a);
        s == "1..2..3"
    }


    /////////////////////////////////////////////////////////////////////////////
    // toMutList                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toMutList01(): Bool = region rc {
        MutList.sameElements(Array.toMutList(rc, (Array#{} @ rc): Array[Int32, rc]), MutList.new(rc))
    }

    @Test
    def toMutList02(): Bool = region rc {
        let v = MutList.new(rc);
        MutList.push!(1, v);
        MutList.sameElements(Array.toMutList(rc, Array#{1} @ rc), v)
    }

    @Test
    def toMutList03(): Bool = region rc {
        let v = MutList.new(rc);
        MutList.push!(1, v);
        MutList.push!(2, v);
        MutList.push!(3, v);
        MutList.sameElements(Array.toMutList(rc, Array#{1, 2, 3} @ rc), v)
    }

    @Test
    def toMutList04(): Bool = region rc {
        MutList.sameElements((Array.range(rc, 0, 100) |> Array.toMutList(rc)), MutList.range(rc, 0, 100))
    }

    @Test
    def toMutList05(): Bool = region rc {
        let v = Array.toMutList(rc, Array#{1, 2, 3} @ rc);
        MutList.push!(4, v);
        Array.sameElements(MutList.toArray(rc, v), Array#{1, 2, 3, 4} @ rc)
    }

    @Test
    def toMutList06(): Bool = region rc {
        let v = Array.toMutList(rc, Array#{1, 2, 3} @ rc);
        MutList.push!(4, v);
        discard MutList.pop!(v);
        discard MutList.pop!(v);
        discard MutList.pop!(v);
        discard MutList.pop!(v);
        Array.sameElements(MutList.toArray(rc, v), Array#{} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // shuffle                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def shuffle01(): Bool \ NonDet = region rc {
        let a = (Array#{} @ rc): Array[Int32, _];
        Array.shuffle(Random.new(), a);
        Array.length(a) == 0 and Array.toSet(a) == Set#{}
    }

    @Test
    def shuffle02(): Bool \ NonDet = region rc {
        let a = (Array#{0} @ rc);
        Array.shuffle(Random.new(), a);
        Array.length(a) == 1 and Array.toSet(a) == Set#{0}
    }

    @Test
    def shuffle03(): Bool \ NonDet = region rc {
        let a = (Array#{0, 1, 2, 3} @ rc);
        Array.shuffle(Random.new(), a);
        Array.length(a) == 4 and Array.toSet(a) == Set#{0, 1, 2, 3}
    }

    @Test
    def shuffle04(): Bool \ NonDet = region rc {
        let a = (Array#{0, 1, 2, 3, 4, 5, 6, 7, 8, 9} @ rc);
        Array.shuffle(Random.new(), a);
        Array.length(a) == 10 and Array.toSet(a) == Set#{0, 1, 2, 3, 4, 5, 6, 7, 8, 9}
    }

}

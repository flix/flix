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
    def toString01(): Bool = region rc {
        Array.toString((Array#{} @ rc: Array[Int32, _])) == "Array#{}"
    }

    @test
    def toString02(): Bool = region rc {
        Array.toString((Array#{1} @ rc)) == "Array#{1}"
    }

    @test
    def toString03(): Bool = region rc {
        Array.toString((Array#{1, 2, 3, 4} @ rc)) == "Array#{1, 2, 3, 4}"
    }

    @test
    def toString04(): Bool = region rc {
        Array.toString((Array#{'a', 'b', 'c', 'd'} @ rc)) == "Array#{a, b, c, d}"
    }


    /////////////////////////////////////////////////////////////////////////////
    // Indexable                                                               //
    /////////////////////////////////////////////////////////////////////////////

    def outOfBoundsHandler(_: Region[rc], f: Unit -> Bool \ OutOfBounds + rc): Bool = {
        try {
            f()
        } with OutOfBounds {
            def outOfBounds(_, _) = false
        }
    }

    @test
    def testIndexable01(): Bool = region rc {
        let a = Array#{1, 2, 3, 4} @ rc;
        outOfBoundsHandler(rc, () -> a[0] == 1)
    }

    @test
    def testIndexable02(): Bool = region rc {
        let a = Array#{1, 2, 3, 4} @ rc;
        outOfBoundsHandler(rc, () -> a[1] == 2)
    }

    @test
    def testIndexable03(): Bool = region rc {
        let a = Array#{1, 2, 3, 4} @ rc;
        outOfBoundsHandler(rc, () -> a[2] == 3)
    }

    @test
    def testIndexable04(): Bool = region rc {
        let a = Array#{1, 2, 3, 4} @ rc;
        outOfBoundsHandler(rc, () -> a[3] == 4)
    }

    @test
    def testIndexable05(): Bool = region rc {
        let a = Array#{1, 2, 3, 4} @ rc;
        not outOfBoundsHandler(rc, () -> a[4] == -1)
    }

    @test
    def testIndexable06(): Bool = region rc {
        let a = Array#{1, 2, 3, 4} @ rc;
        not outOfBoundsHandler(rc, () -> a[-1] == -1)
    }


    /////////////////////////////////////////////////////////////////////////////
    // new                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testEmpty01(): Bool = region rc {
        Array.get(0, Array.empty(rc, 10)) == 0
    }

    @test
    def testEmpty02(): Bool = region rc {
        Array.get(5, Array.empty(rc, 10)) == 0
    }

    @test
    def testEmpty03(): Bool = region rc {
        Array.get(9, Array.empty(rc, 10)) == 0
    }

    @test
    def testEmpty04(): Bool = region rc {
        Array.get(0, Array.empty(rc, 10)) == false
    }

    @test
    def testEmpty05(): Bool = region rc {
        Array.get(5, Array.empty(rc, 10)) == false
    }

    @test
    def testEmpty06(): Bool = region rc {
        Array.get(9, Array.empty(rc, 10)) == false
    }

    /////////////////////////////////////////////////////////////////////////////
    // nth                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testNth01(): Bool = region rc {
        Array.nth(0, Array#{1, 2, 3} @ rc) == Some(1)
    }

    @test
    def testNth02(): Bool = region rc {
        Array.nth(1, Array#{1, 2, 3} @ rc) == Some(2)
    }

    @test
    def testNth03(): Bool = region rc {
        Array.nth(2, Array#{1, 2, 3} @ rc) == Some(3)
    }

    @test
    def testNth04(): Bool = region rc {
        Array.nth(3, Array#{1, 2, 3} @ rc) == None
    }

    @test
    def testNth05(): Bool = region rc {
        Array.nth(-2, Array#{1, 2, 3} @ rc) == None
    }

    /////////////////////////////////////////////////////////////////////////////
    // put                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testPut01(): Bool = region rc {
        let a = (Array#{1} @ rc) !> Array.put(42, 0);
        Array.get(0, a) == 42
    }

    @test
    def testPut02(): Bool = region rc {
        let a = (Array#{1, 2} @ rc) !> Array.put(42, 1);
        Array.get(1, a) == 42
    }

    @test
    def testPut03(): Bool = region rc {
        let a = (Array#{1, 2, 3} @ rc) !> Array.put(42, 2);
        Array.get(2, a) == 42
    }

    /////////////////////////////////////////////////////////////////////////////
    // isEmpty                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testIsEmpty01(): Bool = region rc {
        Array.isEmpty(Array#{} @ rc)
    }

    @test
    def testIsEmpty02(): Bool = region rc {
        not Array.isEmpty(Array#{1} @ rc)
    }

    @test
    def testIsEmpty03(): Bool = region rc {
        not Array.isEmpty(Array#{1, 2, 3} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // nonEmpty                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testNonEmpty01(): Bool = region rc {
        not Array.nonEmpty(Array#{} @ rc)
    }

    @test
    def testNonEmpty02(): Bool = region rc {
        Array.nonEmpty(Array#{1} @ rc)
    }

    @test
    def testNonEmpty03(): Bool = region rc {
        Array.nonEmpty(Array#{1, 2, 3} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // length                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testLength01(): Bool = region rc {
        Array.length(Array#{} @ rc) == 0
    }

    @test
    def testLength02(): Bool = region rc {
        Array.length(Array#{1} @ rc) == 1
    }

    @test
    def testLength03(): Bool = region rc {
        Array.length(Array#{1, 2, 3} @ rc) == 3
    }

    /////////////////////////////////////////////////////////////////////////////
    // size                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testSize01(): Bool = region rc {
        Array.size(Array#{} @ rc) == 0
    }

    @test
    def testSize02(): Bool = region rc {
        Array.size(Array#{1} @ rc) == 1
    }

    @test
    def testSize03(): Bool = region rc {
        Array.size(Array#{1, 2, 3} @ rc) == 3
    }

    /////////////////////////////////////////////////////////////////////////////
    // slice                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testSlice01(): Bool = region rc {
        Array.get(0, Array.slice(rc, start = 0, end = 1, Array#{1, 2, 3} @ rc)) == 1
    }

    @test
    def testSlice02(): Bool = region rc {
        Array.get(0, Array.slice(rc, start = 1, end = 2, Array#{1, 2, 3} @ rc)) == 2
    }

    @test
    def testSlice03(): Bool = region rc {
        Array.get(0, Array.slice(rc, start = 2, end = 3, Array#{1, 2, 3} @ rc)) == 3
    }

    /////////////////////////////////////////////////////////////////////////////
    // toList                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testToList01(): Bool = region rc {
        Array.toList((Array#{} @ rc: Array[Unit, _])) == Nil
    }

    @test
    def testToList02(): Bool = region rc {
        Array.toList(Array#{1} @ rc) == 1 :: Nil
    }

    @test
    def testToList03(): Bool = region rc {
        Array.toList(Array#{1, 2} @ rc) == 1 :: 2 :: Nil
    }

    /////////////////////////////////////////////////////////////////////////////
    // toNel                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toNel01(): Bool = region rc {
        Array.toNel((Array#{} @ rc: Array[Unit, _])) == None
    }

    @test
    def toNel02(): Bool = region rc {
        Array.toNel(Array#{1} @ rc) == Some(Nel.singleton(1))
    }

    @test
    def toNel03(): Bool = region rc {
        Array.toNel(Array#{1,2} @ rc) == List.toNel(1 :: 2 :: Nil)
    }

    @test
    def toNel04(): Bool = region rc {
        Array.toNel(Array#{1,2,3} @ rc) == List.toNel(1 :: 2 :: 3 :: Nil)
    }

    /////////////////////////////////////////////////////////////////////////////
    // toMutDeque                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toMutDeque01(): Bool = region rc {
        let a = (Array#{} @ rc: Array[Int32, _]);
        let d = (MutDeque.empty(rc): MutDeque[Int32, _]);
        Array.toMutDeque(rc, a) `MutDeque.sameElements` d
    }

    @test
    def toMutDeque02(): Bool = region rc {
        let a = (Array#{1} @ rc);

        let d = MutDeque.empty(rc);
        MutDeque.pushBack(1, d);

        Array.toMutDeque(rc, a) `MutDeque.sameElements` d
    }

    @test
    def toMutDeque03(): Bool = region rc {
        let a = (Array#{1, 2, 3} @ rc);

        let d = MutDeque.empty(rc);
        MutDeque.pushBack(1, d);
        MutDeque.pushBack(2, d);
        MutDeque.pushBack(3, d);

        Array.toMutDeque(rc, a) `MutDeque.sameElements` d
    }

    @test
    def toMutDeque04(): Bool = region rc {
        let a = (Array#{1, 2, 3, 4, 5} @ rc);

        let d = MutDeque.empty(rc);
        MutDeque.pushBack(3, d);
        MutDeque.pushBack(4, d);
        MutDeque.pushBack(5, d);
        MutDeque.pushFront(2, d);
        MutDeque.pushFront(1, d);

        Array.toMutDeque(rc, a) `MutDeque.sameElements` d
    }

    @test
    def toMutDeque05(): Bool = region rc {
        let a = (Array#{'a', 'b', 'c'} @ rc);

        let d = MutDeque.empty(rc);
        MutDeque.pushFront('c', d);
        MutDeque.pushFront('b', d);
        MutDeque.pushFront('a', d);

        Array.toMutDeque(rc, a) `MutDeque.sameElements` d
    }

    /////////////////////////////////////////////////////////////////////////////
    // toVector                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toVector01(): Bool = region rc {
        Array.toVector((Array#{} @ rc : Array[Int32, _])) `Vector.equals` Vector.empty()
    }

    @test
    def toVector02(): Bool = region rc {
        Array.toVector((Array#{0} @ rc : Array[Int32, _])) `Vector.equals` Vector.singleton(0)
    }

    @test
    def toVector03(): Bool = region rc {
        Array.toVector((Array#{1, 2, 3} @ rc : Array[Int32, _])) `Vector.equals` List.toVector(1 :: 2 :: 3 :: Nil)
    }

    /////////////////////////////////////////////////////////////////////////////
    // head                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def head01(): Bool = region rc {
        Array.head((Array#{} @ rc: Array[Int32, _])) == None
    }

    @test
    def head02(): Bool = region rc {
        Array.head(Array#{1} @ rc) == Some(1)
    }

    @test
    def head03(): Bool = region rc {
        Array.head(Array#{2,1} @ rc) == Some(2)
    }

    @test
    def head04(): Bool = region rc {
        Array.head(Array#{3,2,1} @ rc) == Some(3)
    }

    /////////////////////////////////////////////////////////////////////////////
    // last                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def last01(): Bool = region rc {
        (Array.last(Array#{} @ rc): Option[Unit]) == None
    }

    @test
    def last02(): Bool = region rc {
        Array.last(Array#{1} @ rc) == Some(1)
    }

    @test
    def last03(): Bool = region rc {
        Array.last(Array#{1,2} @ rc) == Some(2)
    }

    @test
    def last04(): Bool = region rc {
        Array.last(Array#{1,2,3} @ rc) == Some(3)
    }

    /////////////////////////////////////////////////////////////////////////////
    // append                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def append01(): Bool = region rc {
        let a = Array.append(rc, ((Array#{} @ rc): Array[Int32, _]), ((Array#{} @ rc): Array[Int32, _]));
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def append02(): Bool = region rc {
        let a = Array.append(rc, ((Array#{} @ rc): Array[Int32, _]), Array#{1} @ rc);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @test
    def append03(): Bool = region rc {
        let a = Array.append(rc, ((Array#{} @ rc): Array[Int32, _]), Array#{1,2} @ rc);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @test
    def append04(): Bool = region rc {
        let a = Array.append(rc, Array#{1} @ rc, ((Array#{} @ rc): Array[Int32, _]));
        Array.sameElements(a, Array#{1} @ rc)
    }

    @test
    def append05(): Bool = region rc {
        let a = Array.append(rc, Array#{1,2} @ rc, ((Array#{} @ rc): Array[Int32, _]));
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @test
    def append06(): Bool = region rc {
        let a = Array.append(rc, Array#{1} @ rc, Array#{2} @ rc);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @test
    def append07(): Bool = region rc {
        let a = Array.append(rc, Array#{1,2} @ rc, Array#{3} @ rc);
        Array.sameElements(a, Array#{1,2,3} @ rc)
    }

    @test
    def append08(): Bool = region rc {
        let a = Array.append(rc, Array#{1} @ rc, Array#{2,3} @ rc);
        Array.sameElements(a, Array#{1,2,3} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // memberOf                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def memberOf01(): Bool = region rc {
        Array.memberOf(0, ((Array#{} @ rc): Array[Int32, _])) == false
    }

    @test
    def memberOf02(): Bool = region rc {
        Array.memberOf(0, Array#{1} @ rc) == false
    }

    @test
    def memberOf03(): Bool = region rc {
        Array.memberOf(0, Array#{0} @ rc) == true
    }

    @test
    def memberOf04(): Bool = region rc {
        Array.memberOf(0, Array#{1,2} @ rc) == false
    }

    @test
    def memberOf05(): Bool = region rc {
        Array.memberOf(1, Array#{1,2} @ rc) == true
    }

    @test
    def memberOf06(): Bool = region rc {
        Array.memberOf(2, Array#{1,2} @ rc) == true
    }

    @test
    def memberOf07(): Bool = region rc {
        Array.memberOf(3, Array#{1,2} @ rc) == false
    }

    /////////////////////////////////////////////////////////////////////////////
    // indexOf                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def indexOf01(): Bool = region rc {
        Array.indexOf(0, ((Array#{} @ rc): Array[Int32, _])) == None
    }

    @test
    def indexOf02(): Bool = region rc {
        Array.indexOf(0, Array#{1} @ rc) == None
    }

    @test
    def indexOf03(): Bool = region rc {
        Array.indexOf(1, Array#{1} @ rc) == Some(0)
    }

    @test
    def indexOf04(): Bool = region rc {
        Array.indexOf(0, Array#{1,2} @ rc) == None
    }

    @test
    def indexOf05(): Bool = region rc {
        Array.indexOf(1, Array#{1,2} @ rc) == Some(0)
    }

    @test
    def indexOf06(): Bool = region rc {
        Array.indexOf(2, Array#{1,2} @ rc) == Some(1)
    }

    /////////////////////////////////////////////////////////////////////////////
    // indexOfLeft                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def indexOfLeft01(): Bool = region rc {
        Array.indexOfLeft(0, ((Array#{} @ rc): Array[Int32, _])) == None
    }

    @test
    def indexOfLeft02(): Bool = region rc {
        Array.indexOfLeft(0, Array#{1} @ rc) == None
    }

    @test
    def indexOfLeft03(): Bool = region rc {
        Array.indexOfLeft(1, Array#{1} @ rc) == Some(0)
    }

    @test
    def indexOfLeft04(): Bool = region rc {
        Array.indexOfLeft(0, Array#{1,2} @ rc) == None
    }

    @test
    def indexOfLeft05(): Bool = region rc {
        Array.indexOfLeft(1, Array#{1,2} @ rc) == Some(0)
    }

    @test
    def indexOfLeft06(): Bool = region rc {
        Array.indexOfLeft(2, Array#{1,2} @ rc) == Some(1)
    }

    @test
    def indexOfLeft07(): Bool = region rc {
        Array.indexOfLeft(1, Array#{1,1} @ rc) == Some(0)
    }

    /////////////////////////////////////////////////////////////////////////////
    // indexOfRight                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def indexOfRight01(): Bool = region rc {
        Array.indexOfRight(0, ((Array#{} @ rc): Array[Int32, _])) == None
    }

    @test
    def indexOfRight02(): Bool = region rc {
        Array.indexOfRight(0, Array#{1} @ rc) == None
    }

    @test
    def indexOfRight03(): Bool = region rc {
        Array.indexOfRight(1, Array#{1} @ rc) == Some(0)
    }

    @test
    def indexOfRight04(): Bool = region rc {
        Array.indexOfRight(0, Array#{1,2} @ rc) == None
    }

    @test
    def indexOfRight05(): Bool = region rc {
        Array.indexOfRight(1, Array#{1,2} @ rc) == Some(0)
    }

    @test
    def indexOfRight06(): Bool = region rc {
        Array.indexOfRight(2, Array#{1,2} @ rc) == Some(1)
    }

    @test
    def indexOfRight07(): Bool = region rc {
        Array.indexOfRight(1, Array#{1,1} @ rc) == Some(1)
    }

    /////////////////////////////////////////////////////////////////////////////
    // indices                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def indices01(): Bool = region rc {
        let a = Array.indices(rc, 0, ((Array#{} @ rc): Array[Int32, rc]));
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, rc]))
    }

    @test
    def indices02(): Bool = region rc {
        let a = Array.indices(rc, 0, Array#{1} @ rc);
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, rc]))
    }

    @test
    def indices03(): Bool = region rc {
        let a = Array.indices(rc, 1, Array#{1} @ rc);
        Array.sameElements(a, Array#{0} @ rc)
    }

    @test
    def indices04(): Bool = region rc {
        let a = Array.indices(rc, 0, Array#{1,2} @ rc);
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, rc]))
    }

    @test
    def indices05(): Bool = region rc {
        let a = Array.indices(rc, 1, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{0} @ rc)
    }

    @test
    def indices06(): Bool = region rc {
        let a = Array.indices(rc, 2, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @test
    def indices07(): Bool = region rc {
        let a = Array.indices(rc, 1, Array#{1,1} @ rc);
        Array.sameElements(a, Array#{0,1} @ rc)
    }
    /////////////////////////////////////////////////////////////////////////////
    // find                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def find01(): Bool = region rc {
        Array.find(i -> i > 2, Array#{} @ rc) == None
    }

    @test
    def find02(): Bool = region rc {
        Array.find(i -> i > 2, Array#{1} @ rc) == None
    }

    @test
    def find03(): Bool = region rc {
        Array.find(i -> i > 2, Array#{3} @ rc) == Some(3)
    }

    @test
    def find04(): Bool = region rc {
        Array.find(i -> i > 2, Array#{1,2} @ rc) == None
    }

    @test
    def find05(): Bool = region rc {
        Array.find(i -> i > 2, Array#{6,-6} @ rc) == Some(6)
    }

    @test
    def find06(): Bool = region rc {
        Array.find(i -> i > 2, Array#{-6,6} @ rc) == Some(6)
    }

    @test
    def find07(): Bool = region rc {
        Array.find(i -> i > 2, Array#{6,7} @ rc) == Some(6)
    }

    /////////////////////////////////////////////////////////////////////////////
    // findLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def findLeft01(): Bool = region rc {
        Array.findLeft(i -> i > 2, Array#{} @ rc) == None
    }

    @test
    def findLeft02(): Bool = region rc {
        Array.findLeft(i -> i > 2, Array#{1} @ rc) == None
    }

    @test
    def findLeft03(): Bool = region rc {
        Array.findLeft(i -> i > 2, Array#{3} @ rc) == Some(3)
    }

    @test
    def findLeft04(): Bool = region rc {
        Array.findLeft(i -> i > 2, Array#{1,2} @ rc) == None
    }

    @test
    def findLeft05(): Bool = region rc {
        Array.findLeft(i -> i > 2, Array#{6,-6} @ rc) == Some(6)
    }

    @test
    def findLeft06(): Bool = region rc {
        Array.findLeft(i -> i > 2, Array#{-6,6} @ rc) == Some(6)
    }

    @test
    def findLeft07(): Bool = region rc {
        Array.findLeft(i -> i > 2, Array#{6,7} @ rc) == Some(6)
    }

    /////////////////////////////////////////////////////////////////////////////
    // findRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def findRight01(): Bool = region rc {
        Array.findRight(i -> i > 2, Array#{} @ rc) == None
    }

    @test
    def findRight02(): Bool = region rc {
        Array.findRight(i -> i > 2, Array#{1} @ rc) == None
    }

    @test
    def findRight03(): Bool = region rc {
        Array.findRight(i -> i > 2, Array#{3} @ rc) == Some(3)
    }

    @test
    def findRight04(): Bool = region rc {
        Array.findRight(i -> i > 2, Array#{1,2} @ rc) == None
    }

    @test
    def findRight05(): Bool = region rc {
        Array.findRight(i -> i > 2, Array#{6,-6} @ rc) == Some(6)
    }

    @test
    def findRight06(): Bool = region rc {
        Array.findRight(i -> i > 2, Array#{-6,6} @ rc) == Some(6)
    }

    @test
    def findRight07(): Bool = region rc {
        Array.findRight(i -> i > 2, Array#{6,7} @ rc) == Some(7)
    }

    /////////////////////////////////////////////////////////////////////////////
    // range                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def range01(): Bool = region rc {
        let a = Array.range(rc, 1, 0);
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def range02(): Bool = region rc {
        let a = Array.range(rc, 1, 1);
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def range03(): Bool = region rc {
        let a = Array.range(rc, 1, 2);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @test
    def range04(): Bool = region rc {
        let a = Array.range(rc, 1, 3);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @test
    def range05(): Bool = region rc {
        let a = Array.range(rc, 1, 4);
        Array.sameElements(a, Array#{1,2,3} @ rc)
    }

    @test
    def range06(): Bool = region rc {
        let a = Array.range(rc, -1, 3);
        Array.sameElements(a, Array#{-1,0,1,2} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // repeat rc,
    /////////////////////////////////////////////////////////////////////////////

    @test
    def repeat01(): Bool = region rc {
        let a = Array.repeat(rc, -1, 1);
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def repeat02(): Bool = region rc {
        let a = Array.repeat(rc, 0, 1);
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def repeat03(): Bool = region rc {
        let a = Array.repeat(rc, 1, 1);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @test
    def repeat04(): Bool = region rc {
        let a = Array.repeat(rc, 2, 1);
        Array.sameElements(a, Array#{1,1} @ rc)
    }

    @test
    def repeat05(): Bool = region rc {
        let a = Array.repeat(rc, 3, 1);
        Array.sameElements(a, Array#{1,1,1} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // scan                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def scan01(): Bool = region rc {
        let a = Array.scan(rc, (i, b) -> if (b) i + 1 else i + 2, 1, Array#{} @ rc);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @test
    def scan02(): Bool = region rc {
        let a = Array.scan(rc, (i, b) -> if (b) i + 1 else i + 2, 1, Array#{false} @ rc);
        Array.sameElements(a, Array#{1,3} @ rc)
    }

    @test
    def scan03(): Bool = region rc {
        let a = Array.scan(rc, (i, b) -> if (b) i + 1 else i + 2, 1, Array#{true} @ rc);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @test
    def scan04(): Bool = region rc {
        let a = Array.scan(rc, (i, b) -> if (b) i + 1 else i + 2, 1, Array#{false,false} @ rc);
        Array.sameElements(a, Array#{1,3,5} @ rc)
    }

    @test
    def scan05(): Bool = region rc {
        let a = Array.scan(rc, (i, b) -> if (b) i + 1 else i + 2, 1, Array#{false,true} @ rc);
        Array.sameElements(a, Array#{1,3,4} @ rc)
    }

    @test
    def scan06(): Bool = region rc {
        let a = Array.scan(rc, (i, b) -> if (b) i + 1 else i + 2, 1, Array#{true,false} @ rc);
        Array.sameElements(a, Array#{1,2,4} @ rc)
    }

    @test
    def scan07(): Bool = region rc {
        let a = Array.scan(rc, (i, b) -> if (b) i + 1 else i + 2, 1, Array#{true,true} @ rc);
        Array.sameElements(a, Array#{1,2,3} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // scanLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def scanLeft01(): Bool = region rc {
        let a = Array.scanLeft(rc, (i, b) -> if (b) i + 1 else i + 2, 1, ((Array#{} @ rc): Array[Bool, _]));
        Array.sameElements(a, Array#{1} @ rc)
    }

    @test
    def scanLeft02(): Bool = region rc {
        let a = Array.scanLeft(rc, (i, b) -> if (b) i + 1 else i + 2, 1, Array#{false} @ rc);
        Array.sameElements(a, Array#{1,3} @ rc)
    }

    @test
    def scanLeft03(): Bool = region rc {
        let a = Array.scanLeft(rc, (i, b) -> if (b) i + 1 else i + 2, 1, Array#{true} @ rc);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @test
    def scanLeft04(): Bool = region rc {
        let a = Array.scanLeft(rc, (i, b) -> if (b) i + 1 else i + 2, 1, Array#{false,false} @ rc);
        Array.sameElements(a, Array#{1,3,5} @ rc)
    }

    @test
    def scanLeft05(): Bool = region rc {
        let a = Array.scanLeft(rc, (i, b) -> if (b) i + 1 else i + 2, 1, Array#{false,true} @ rc);
        Array.sameElements(a, Array#{1,3,4} @ rc)
    }

    @test
    def scanLeft06(): Bool = region rc {
        let a = Array.scanLeft(rc, (i, b) -> if (b) i + 1 else i + 2, 1, Array#{true,false} @ rc);
        Array.sameElements(a, Array#{1,2,4} @ rc)
    }

    @test
    def scanLeft07(): Bool = region rc {
        let a = Array.scanLeft(rc, (i, b) -> if (b) i + 1 else i + 2, 1, Array#{true,true} @ rc);
        Array.sameElements(a, Array#{1,2,3} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // scanRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def scanRight01(): Bool = region rc {
        let a = Array.scanRight(rc, (b, i) -> if (b) i + 1 else i + 2, 1, Array#{} @ rc);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @test
    def scanRight02(): Bool = region rc {
        let a = Array.scanRight(rc, (b, i) -> if (b) i + 1 else i + 2, 1, Array#{false} @ rc);
        Array.sameElements(a, Array#{3,1} @ rc)
    }

    @test
    def scanRight03(): Bool = region rc {
        let a = Array.scanRight(rc, (b, i) -> if (b) i + 1 else i + 2, 1, Array#{true} @ rc);
        Array.sameElements(a, Array#{2,1} @ rc)
    }

    @test
    def scanRight04(): Bool = region rc {
        let a = Array.scanRight(rc, (b, i) -> if (b) i + 1 else i + 2, 1, Array#{false,false} @ rc);
        Array.sameElements(a, Array#{5,3,1} @ rc)
    }

    @test
    def scanRight05(): Bool = region rc {
        let a = Array.scanRight(rc, (b, i) -> if (b) i + 1 else i + 2, 1, Array#{false,true} @ rc);
        Array.sameElements(a, Array#{4,2,1} @ rc)
    }

    @test
    def scanRight06(): Bool = region rc {
        let a = Array.scanRight(rc, (b, i) -> if (b) i + 1 else i + 2, 1, Array#{true,false} @ rc);
        Array.sameElements(a, Array#{4,3,1} @ rc)
    }

    @test
    def scanRight07(): Bool = region rc {
        let a = Array.scanRight(rc, (b, i) -> if (b) i + 1 else i + 2, 1, Array#{true,true} @ rc);
        Array.sameElements(a, Array#{3,2,1} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // map                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def map01(): Bool = region rc {
        let a = Array.map(rc, i -> i > 2, Array#{} @ rc);
        Array.sameElements(a, ((Array#{} @ rc): Array[Bool, _]))
    }

    @test
    def map02(): Bool = region rc {
        let a = Array.map(rc, i -> i > 2, Array#{1} @ rc);
        Array.sameElements(a, Array#{false} @ rc)
    }

    @test
    def map03(): Bool = region rc {
        let a = Array.map(rc, i -> i > 2, Array#{3} @ rc);
        Array.sameElements(a, Array#{true} @ rc)
    }

    @test
    def map04(): Bool = region rc {
        let a = Array.map(rc, i -> i > 2, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{false,false} @ rc)
    }

    @test
    def map05(): Bool = region rc {
        let a = Array.map(rc, i -> i > 2, Array#{1,8} @ rc);
        Array.sameElements(a, Array#{false, true} @ rc)
    }

    @test
    def map06(): Bool = region rc {
        let a = Array.map(rc, i -> i > 2, Array#{8,1} @ rc);
        Array.sameElements(a, Array#{true,false} @ rc)
    }

    @test
    def map07(): Bool = region rc {
        let a = Array.map(rc, i -> i > 2, Array#{7,8} @ rc);
        Array.sameElements(a, Array#{true,true} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // transform!                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def transform01!(): Bool = region rc {
        let a: Array[Int32, _] = Array#{} @ rc;
        Array.transform!(x -> x+1, a);
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def transform02!(): Bool = region rc {
        let a = Array#{0} @ rc;
        Array.transform!(x -> x+1, a);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @test
    def transform03!(): Bool = region rc {
        let a = Array#{0,1} @ rc;
        Array.transform!(x -> x+1, a);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @test
    def transform04!(): Bool = region rc {
        let a = Array#{0,1,2} @ rc;
        Array.transform!(x -> x+1, a);
        Array.sameElements(a, Array#{1,2,3} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // mapWithIndex                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapWithIndex01(): Bool = region rc {
        let a = Array.mapWithIndex(rc, (i, e) -> if (i < 1) e > 2 else e <= 2, Array#{} @ rc);
        Array.sameElements(a, ((Array#{} @ rc): Array[Bool, _]))
    }

    @test
    def mapWithIndex02(): Bool = region rc {
        let a = Array.mapWithIndex(rc, (i, e) -> if (i < 1) e > 2 else e <= 2, Array#{1} @ rc);
        Array.sameElements(a, Array#{false} @ rc)
    }

    @test
    def mapWithIndex03(): Bool = region rc {
        let a = Array.mapWithIndex(rc, (i, e) -> if (i < 1) e > 2 else e <= 2, Array#{3} @ rc);
        Array.sameElements(a, Array#{true} @ rc)
    }

    @test
    def mapWithIndex04(): Bool = region rc {
        let a = Array.mapWithIndex(rc, (i, e) -> if (i < 1) e > 2 else e <= 2, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{false,true} @ rc)
    }

    @test
    def mapWithIndex05(): Bool = region rc {
        let a = Array.mapWithIndex(rc, (i, e) -> if (i < 1) e > 2 else e <= 2, Array#{1,8} @ rc);
        Array.sameElements(a, Array#{false,false} @ rc)
    }

    @test
    def mapWithIndex06(): Bool = region rc {
        let a = Array.mapWithIndex(rc, (i, e) -> if (i < 1) e > 2 else e <= 2, Array#{8,1} @ rc);
        Array.sameElements(a, Array#{true,true} @ rc)
    }

    @test
    def mapWithIndex07(): Bool = region rc {
        let a = Array.mapWithIndex(rc, (i, e) -> if (i < 1) e > 2 else e <= 2, Array#{7,8} @ rc);
        Array.sameElements(a, Array#{true,false} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // transformWithIndex!                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def transformWithIndex01!(): Bool = region rc {
        let a = ((Array#{} @ rc): Array[Int32, _]);
        Array.transformWithIndex!((i, e) -> if (i < 1) e + 10 else e, a);
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def transformWithIndex02!(): Bool = region rc {
        let a = Array#{1} @ rc;
        Array.transformWithIndex!((i, e) -> if (i < 1) e + 10 else e, a);
        Array.sameElements(a, Array#{11} @ rc)
    }

    @test
    def transformWithIndex03!(): Bool = region rc {
        let a = Array#{3} @ rc;
        Array.transformWithIndex!((i, e) -> if (i < 1) e + 10 else e, a);
        Array.sameElements(a, Array#{13} @ rc)
    }

    @test
    def transformWithIndex04!(): Bool = region rc {
        let a = Array#{1,2} @ rc;
        Array.transformWithIndex!((i, e) -> if (i < 1) e + 10 else e, a);
        Array.sameElements(a, Array#{11,2} @ rc)
    }

    @test
    def transformWithIndex05!(): Bool = region rc {
        let a = Array#{1,8} @ rc;
        Array.transformWithIndex!((i, e) -> if (i < 1) e + 10 else e, a);
        Array.sameElements(a, Array#{11,8} @ rc)
    }

    @test
    def transformWithIndex06!(): Bool = region rc {
        let a = Array#{8,1} @ rc;
        Array.transformWithIndex!((i, e) -> if (i < 1) e + 10 else e, a);
        Array.sameElements(a, Array#{18,1} @ rc)
    }

    @test
    def transformWithIndex07!(): Bool = region rc {
        let a = Array#{7,8,9} @ rc;
        Array.transformWithIndex!((i, e) -> if (i < 1) e + 10 else e, a);
        Array.sameElements(a, Array#{17,8,9} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // flatMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    // depends on flatten

    @test
    def flatMap01(): Bool = region rc {
        let a = Array.flatMap(rc, i -> Array.repeat(rc, i, i), Array#{} @ rc);
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def flatMap02(): Bool = region rc {
        let a = Array.flatMap(rc, i -> Array.repeat(rc, i, i), Array#{0} @ rc);
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def flatMap03(): Bool = region rc {
        let a = Array.flatMap(rc, i -> Array.repeat(rc, i, i), Array#{1} @ rc);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @test
    def flatMap04(): Bool = region rc {
        let a = Array.flatMap(rc, i -> Array.repeat(rc, i, i), Array#{2} @ rc);
        Array.sameElements(a, Array#{2,2} @ rc)
    }

    @test
    def flatMap05(): Bool = region rc {
        let a = Array.flatMap(rc, i -> Array.repeat(rc, i, i), Array#{1,2} @ rc);
        Array.sameElements(a, Array#{1,2,2} @ rc)
    }

    @test
    def flatMap06(): Bool = region rc {
        let a = Array.flatMap(rc, i -> Array.repeat(rc, i, i), Array#{2,3} @ rc);
        Array.sameElements(a, Array#{2,2,3,3,3} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // reverse                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reverse01(): Bool = region rc {
        let a = Array.reverse(rc, ((Array#{} @ rc): Array[Int32, _]));
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def reverse02(): Bool = region rc {
        let a = Array.reverse(rc, Array#{1} @ rc);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @test
    def reverse03(): Bool = region rc {
        let a = Array.reverse(rc, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{2,1} @ rc)
    }

    @test
    def reverse04(): Bool = region rc {
        let a = Array.reverse(rc, Array#{1,1} @ rc);
        Array.sameElements(a, Array#{1,1} @ rc)
    }

    @test
    def reverse05(): Bool = region rc {
        let a = Array.reverse(rc, Array#{1,2,3} @ rc);
        Array.sameElements(a, Array#{3,2,1} @ rc)
    }

    @test
    def reverse06(): Bool = region rc {
        let a = Array.reverse(rc, Array#{1,2,3,4} @ rc);
        Array.sameElements(a, Array#{4,3,2,1} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // reverse!                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reverse01!(): Bool = region rc {
        let a: Array[Int32, _] = Array#{} @ rc;
        Array.reverse!(a);
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def reverse02!(): Bool = region rc {
        let a = Array#{1} @ rc;
        Array.reverse!(a);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @test
    def reverse03!(): Bool = region rc {
        let a = Array#{1,2} @ rc;
        Array.reverse!(a);
        Array.sameElements(a, Array#{2,1} @ rc)
    }

    @test
    def reverse04!(): Bool = region rc {
        let a = Array#{1,1} @ rc;
        Array.reverse!(a);
        Array.sameElements(a, Array#{1,1} @ rc)
    }

    @test
    def reverse05!(): Bool = region rc {
        let a = Array#{1,2,3} @ rc;
        Array.reverse!(a);
        Array.sameElements(a, Array#{3,2,1} @ rc)
    }

    @test
    def reverse06!(): Bool = region rc {
        let a = Array#{1,2,3,4} @ rc;
        Array.reverse!(a);
        Array.sameElements(a, Array#{4,3,2,1} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // rotateLeft                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def rotateLeft01(): Bool = region rc {
        let a = Array.rotateLeft(rc, 0, ((Array#{} @ rc): Array[Int32, _]));
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def rotateLeft02(): Bool = region rc {
        let a = Array.rotateLeft(rc, 1, ((Array#{} @ rc): Array[Int32, _]));
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def rotateLeft03(): Bool = region rc {
        let a = Array.rotateLeft(rc, 0, Array#{1} @ rc);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @test
    def rotateLeft04(): Bool = region rc {
        let a = Array.rotateLeft(rc, 0, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @test
    def rotateLeft05(): Bool = region rc {
        let a = Array.rotateLeft(rc, 1, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{2,1} @ rc)
    }

    @test
    def rotateLeft06(): Bool = region rc {
        let a = Array.rotateLeft(rc, 2, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @test
    def rotateLeft07(): Bool = region rc {
        let a = Array.rotateLeft(rc, 3, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{2,1} @ rc)
    }

    @test
    def rotateLeft08(): Bool = region rc {
        let a = Array.rotateLeft(rc, -1, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{2,1} @ rc)
    }

    @test
    def rotateLeft09(): Bool = region rc {
        let a = Array.rotateLeft(rc, 0, Array#{1,2,3} @ rc);
        Array.sameElements(a, Array#{1,2,3} @ rc)
    }

    @test
    def rotateLeft10(): Bool = region rc {
        let a = Array.rotateLeft(rc, 1, Array#{1,2,3} @ rc);
        Array.sameElements(a, Array#{2,3,1} @ rc)
    }

    @test
    def rotateLeft11(): Bool = region rc {
        let a = Array.rotateLeft(rc, 2, Array#{1,2,3} @ rc);
        Array.sameElements(a, Array#{3,1,2} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // rotateRight                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def rotateRight01(): Bool = region rc {
        let a = Array.rotateRight(rc, 0, ((Array#{} @ rc): Array[Int32, _]));
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def rotateRight02(): Bool = region rc {
        let a = Array.rotateRight(rc, 1, ((Array#{} @ rc): Array[Int32, _]));
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def rotateRight03(): Bool = region rc {
        let a = Array.rotateRight(rc, 0, Array#{1} @ rc);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @test
    def rotateRight04(): Bool = region rc {
        let a = Array.rotateRight(rc, 0, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @test
    def rotateRight05(): Bool = region rc {
        let a = Array.rotateRight(rc, 1, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{2,1} @ rc)
    }

    @test
    def rotateRight06(): Bool = region rc {
        let a = Array.rotateRight(rc, 2, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @test
    def rotateRight07(): Bool = region rc {
        let a = Array.rotateRight(rc, 3, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{2,1} @ rc)
    }

    @test
    def rotateRight08(): Bool = region rc {
        let a = Array.rotateRight(rc, -1, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{2,1} @ rc)
    }

    @test
    def rotateRight09(): Bool = region rc {
        let a = Array.rotateRight(rc, 0, Array#{1,2,3} @ rc);
        Array.sameElements(a, Array#{1,2,3} @ rc)
    }

    @test
    def rotateRight10(): Bool = region rc {
        let a = Array.rotateRight(rc, 1, Array#{1,2,3} @ rc);
        Array.sameElements(a, Array#{3,1,2} @ rc)
    }

    @test
    def rotateRight11(): Bool = region rc {
        let a = Array.rotateRight(rc, 2, Array#{1,2,3} @ rc);
        Array.sameElements(a, Array#{2,3,1} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // update                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def update01(): Bool = region rc {
        let a = Array.update(rc, 0, 2, ((Array#{} @ rc): Array[Int32, _]));
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def update02(): Bool = region rc {
        let a = Array.update(rc, -1, 2, Array#{1} @ rc);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @test
    def update03(): Bool = region rc {
        let a = Array.update(rc, 0, 2, Array#{1} @ rc);
        Array.sameElements(a, Array#{2} @ rc)
    }

    @test
    def update04(): Bool = region rc {
        let a = Array.update(rc, 1, 2, Array#{1} @ rc);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @test
    def update05(): Bool = region rc {
        let a = Array.update(rc, 0, 5, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{5,2} @ rc)
    }

    @test
    def update06(): Bool = region rc {
        let a = Array.update(rc, 1, 5, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{1,5} @ rc)
    }

    @test
    def update07(): Bool = region rc {
        let a = Array.update(rc, 2, 5, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // replace                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def replace01(): Bool = region rc {
        let a = Array.replace(rc, src = 3, dst =  4, Array#{} @ rc);
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def replace02(): Bool = region rc {
        let a = Array.replace(rc, src = 3, dst =  4, Array#{1} @ rc);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @test
    def replace03(): Bool = region rc {
        let a = Array.replace(rc, src = 3, dst =  4, Array#{3} @ rc);
        Array.sameElements(a, Array#{4} @ rc)
    }

    @test
    def replace04(): Bool = region rc {
        let a = Array.replace(rc, src = 3, dst =  4, Array#{4} @ rc);
        Array.sameElements(a, Array#{4} @ rc)
    }

    @test
    def replace05(): Bool = region rc {
        let a = Array.replace(rc, src = 3, dst =  4, Array#{1, 2} @ rc);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @test
    def replace06(): Bool = region rc {
        let a = Array.replace(rc, src = 3, dst =  4, Array#{1,3} @ rc);
        Array.sameElements(a, Array#{1,4} @ rc)
    }

    @test
    def replace07(): Bool = region rc {
        let a = Array.replace(rc, src = 3, dst =  4, Array#{3,4} @ rc);
        Array.sameElements(a, Array#{4,4} @ rc)
    }

    @test
    def replace08(): Bool = region rc {
        let a = Array.replace(rc, src = 3, dst =  4, Array#{3,3} @ rc);
        Array.sameElements(a, Array#{4,4} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // replace!                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def replace01!(): Bool = region rc {
        let a: Array[Int32, _] = Array#{} @ rc;
        Array.replace!(src = 3, dst =  4, a);
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def replace02!(): Bool = region rc {
        let a = Array#{1} @ rc;
        Array.replace!(src = 3, dst =  4, a);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @test
    def replace03!(): Bool = region rc {
        let a = Array#{3} @ rc;
        Array.replace!(src = 3, dst =  4, a);
        Array.sameElements(a, Array#{4} @ rc)
    }

    @test
    def replace04!(): Bool = region rc {
        let a = Array#{4} @ rc;
        Array.replace!(src = 3, dst =  4, a);
        Array.sameElements(a, Array#{4} @ rc)
    }

    @test
    def replace05!(): Bool = region rc {
        let a = Array#{1, 2} @ rc;
        Array.replace!(src = 3, dst =  4, a);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @test
    def replace06!(): Bool = region rc {
        let a = Array#{1,3} @ rc;
        Array.replace!(src = 3, dst =  4, a);
        Array.sameElements(a, Array#{1,4} @ rc)
    }

    @test
    def replace07!(): Bool = region rc {
        let a = Array#{3,4} @ rc;
        Array.replace!(src = 3, dst =  4, a);
        Array.sameElements(a, Array#{4,4} @ rc)
    }

    @test
    def replace08!(): Bool = region rc {
        let a = Array#{3,3} @ rc;
        Array.replace!(src = 3, dst =  4, a);
        Array.sameElements(a, Array#{4,4} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // patch                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def patch01(): Bool = region rc {
        let a = Array.patch(rc, 0, 0, ((Array#{} @ rc): Array[Int32, _]), ((Array#{} @ rc): Array[Int32, _]));
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def patch02(): Bool = region rc {
        let a = Array.patch(rc, 0, 2, Array#{1,2} @ rc, Array#{} @ rc);
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def patch03(): Bool = region rc {
        let a = Array.patch(rc, 0, 2, Array#{} @ rc, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @test
    def patch04(): Bool = region rc {
        let a = Array.patch(rc, -3, 3, Array#{1,2,4} @ rc, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @test
    def patch05(): Bool = region rc {
        let a = Array.patch(rc, 2, 3, Array#{1,2,4} @ rc, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @test
    def patch06(): Bool = region rc {
        let a = Array.patch(rc, 0, 0, Array#{} @ rc, Array#{1} @ rc);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @test
    def patch07(): Bool = region rc {
        let a = Array.patch(rc, 1, 0, Array#{2} @ rc, Array#{1} @ rc);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @test
    def patch08(): Bool = region rc {
        let a = Array.patch(rc, 0, 1, Array#{2} @ rc, Array#{1} @ rc);
        Array.sameElements(a, Array#{2} @ rc)
    }

    @test
    def patch09(): Bool = region rc {
        let a = Array.patch(rc, 0, 2, Array#{2,4} @ rc, Array#{1} @ rc);
        Array.sameElements(a, Array#{2} @ rc)
    }

    @test
    def patch10(): Bool = region rc {
        let a = Array.patch(rc, -1, 2, Array#{2,4} @ rc, Array#{1} @ rc);
        Array.sameElements(a, Array#{4} @ rc)
    }

    @test
    def patch11(): Bool = region rc {
        let a = Array.patch(rc, -1, 2, Array#{3,4} @ rc, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{4,2} @ rc)
    }

    @test
    def patch12(): Bool = region rc {
        let a = Array.patch(rc, 1, 2, Array#{3,4} @ rc, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{1,3} @ rc)
    }

    @test
    def patch13(): Bool = region rc {
        let a = Array.patch(rc, -2, 2, Array#{3,4} @ rc, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @test
    def patch14(): Bool = region rc {
        let a = Array.patch(rc, 2, 2, Array#{3,4} @ rc, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @test
    def patch15(): Bool = region rc {
        let a = Array.patch(rc, 1, 1, Array#{3} @ rc, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{1,3} @ rc)
    }

    @test
    def patch16(): Bool = region rc {
        let a = Array.patch(rc, 0, 2, Array#{3,4} @ rc, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{3,4} @ rc)
    }

    @test
    def patch17(): Bool = region rc {
        let a = Array.patch(rc, 0, 1, Array#{4} @ rc, Array#{1, 2, 3} @ rc);
        Array.sameElements(a, Array#{4, 2, 3} @ rc)
    }

    @test
    def patch18(): Bool = region rc {
        let a = Array.patch(rc, 1, 1, Array#{4} @ rc, Array#{1, 2, 3} @ rc);
        Array.sameElements(a, Array#{1, 4, 3} @ rc)
    }

    @test
    def patch19(): Bool = region rc {
        let a = Array.patch(rc, 2, 1, Array#{4} @ rc, Array#{1, 2, 3} @ rc);
        Array.sameElements(a, Array#{1, 2, 4} @ rc)
    }

    @test
    def patch20(): Bool = region rc {
        let a = Array.patch(rc, 0, 2, Array#{4, 5} @ rc, Array#{1, 2, 3} @ rc);
        Array.sameElements(a, Array#{4, 5, 3} @ rc)
    }

    @test
    def patch21(): Bool = region rc {
        let a = Array.patch(rc, 1, 2, Array#{4, 5} @ rc, Array#{1, 2, 3} @ rc);
        Array.sameElements(a, Array#{1, 4, 5} @ rc)
    }

    @test
    def patch22(): Bool = region rc {
        let a = Array.patch(rc, 0, 2, Array#{4, 5, 6} @ rc, Array#{1, 2, 3} @ rc);
        Array.sameElements(a, Array#{4, 5, 3} @ rc)
    }

    @test
    def patch23(): Bool = region rc {
        let a = Array.patch(rc, 0, 3, Array#{4, 5, 6} @ rc, Array#{1, 2, 3} @ rc);
        Array.sameElements(a, Array#{4, 5, 6} @ rc)
    }

    @test
    def patch24(): Bool = region rc {
        let a = Array.patch(rc, 2, 4, Array#{14, 15, 16, 17} @ rc, Array#{1, 2, 3, 4, 5, 6, 7} @ rc);
        Array.sameElements(a, Array#{1, 2, 14, 15, 16, 17, 7} @ rc)
    }

    @test
    def patch25(): Bool = region rc {
        let a = Array.patch(rc, -2, 4, Array#{14, 15, 16, 17} @ rc, Array#{1, 2, 3, 4, 5, 6, 7} @ rc);
        Array.sameElements(a, Array#{16, 17, 3, 4, 5, 6, 7} @ rc)
    }

    @test
    def patch26(): Bool = region rc {
        let a = Array.patch(rc, 4, 5, Array#{14, 15, 16, 17} @ rc, Array#{1, 2, 3, 4, 5, 6, 7} @ rc);
        Array.sameElements(a, Array#{1, 2, 3, 4, 14, 15, 16} @ rc)
    }

    @test
    def patch27(): Bool = region rc {
        let a = Array.patch(rc, 4, 2, Array#{14, 15, 16, 17} @ rc, Array#{1, 2, 3, 4, 5, 6, 7} @ rc);
        Array.sameElements(a, Array#{1, 2, 3, 4, 14, 15, 7} @ rc)
    }

    @test
    def patch28(): Bool = region rc {
        let a = Array.patch(rc, -1, 10, Array#{-1, -2, -3, -4, -5, -6, -7, -8} @ rc, Array#{1, 2, 3, 4, 5, 6, 7} @ rc);
        Array.sameElements(a, Array#{-2, -3, -4, -5, -6, -7, -8} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // patch!                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def patch01!(): Bool = region rc {
        let a = ((Array#{} @ rc): Array[Int32, _]);
        Array.patch!(0, 0, ((Array#{} @ rc): Array[Int32, _]), a);
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def patch02!(): Bool = region rc {
        let a = ((Array#{} @ rc): Array[Int32, _]);
        Array.patch!(0, 2, Array#{1,2} @ rc, a);
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def patch03!(): Bool = region rc {
        let a = Array#{1,2} @ rc;
        Array.patch!(0, 2, Array#{} @ rc, a);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @test
    def patch04!(): Bool = region rc {
        let a = Array#{1,2} @ rc;
        Array.patch!(-3, 3, Array#{1,2,4} @ rc, a);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @test
    def patch05!(): Bool = region rc {
        let a = Array#{1,2} @ rc;
        Array.patch!(2, 3, Array#{1,2,4} @ rc, a);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @test
    def patch06!(): Bool = region rc {
        let a = Array#{1} @ rc;
        Array.patch!(0, 0, Array#{} @ rc, a);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @test
    def patch07!(): Bool = region rc {
        let a = Array#{1} @ rc;
        Array.patch!(1, 0, Array#{2} @ rc, a);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @test
    def patch08!(): Bool = region rc {
        let a = Array#{1} @ rc;
        Array.patch!(0, 1, Array#{2} @ rc, a);
        Array.sameElements(a, Array#{2} @ rc)
    }

    @test
    def patch09!(): Bool = region rc {
        let a = Array#{1} @ rc;
        Array.patch!(0, 2, Array#{2,4} @ rc, a);
        Array.sameElements(a, Array#{2} @ rc)
    }

    @test
    def patch10!(): Bool = region rc {
        let a = Array#{1} @ rc;
        Array.patch!(-1, 2, Array#{2,4} @ rc, a);
        Array.sameElements(a, Array#{4} @ rc)
    }

    @test
    def patch11!(): Bool = region rc {
        let a = Array#{1,2} @ rc;
        Array.patch!(-1, 2, Array#{3,4} @ rc, a);
        Array.sameElements(a, Array#{4,2} @ rc)
    }

    @test
    def patch12!(): Bool = region rc {
        let a = Array#{1,2} @ rc;
        Array.patch!(1, 2, Array#{3,4} @ rc, a);
        Array.sameElements(a, Array#{1,3} @ rc)
    }

    @test
    def patch13!(): Bool = region rc {
        let a = Array#{1,2} @ rc;
        Array.patch!(-2, 2, Array#{3,4} @ rc, a);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @test
    def patch14!(): Bool = region rc {
        let a = Array#{1,2} @ rc;
        Array.patch!(2, 2, Array#{3,4} @ rc, a);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @test
    def patch15!(): Bool = region rc {
        let a = Array#{1,2} @ rc;
        Array.patch!(1, 1, Array#{3} @ rc, a);
        Array.sameElements(a, Array#{1,3} @ rc)
    }

    @test
    def patch16!(): Bool = region rc {
        let a = Array#{1,2} @ rc;
        Array.patch!(0, 2, Array#{3,4} @ rc, a);
        Array.sameElements(a, Array#{3,4} @ rc)
    }

    @test
    def patch17!(): Bool = region rc {
        let a = Array#{1, 2, 3} @ rc;
        Array.patch!(0, 1, Array#{4} @ rc, a);
        Array.sameElements(a, Array#{4, 2, 3} @ rc)
    }

    @test
    def patch18!(): Bool = region rc {
        let a = Array#{1, 2, 3} @ rc;
        Array.patch!(1, 1, Array#{4} @ rc, a);
        Array.sameElements(a, Array#{1, 4, 3} @ rc)
    }

    @test
    def patch19!(): Bool = region rc {
        let a = Array#{1, 2, 3} @ rc;
        Array.patch!(2, 1, Array#{4} @ rc, a);
        Array.sameElements(a, Array#{1, 2, 4} @ rc)
    }

    @test
    def patch20!(): Bool = region rc {
        let a = Array#{1, 2, 3} @ rc;
        Array.patch!(0, 2, Array#{4, 5} @ rc, a);
        Array.sameElements(a, Array#{4, 5, 3} @ rc)
    }

    @test
    def patch21!(): Bool = region rc {
        let a = Array#{1, 2, 3} @ rc;
        Array.patch!(1, 2, Array#{4, 5} @ rc, a);
        Array.sameElements(a, Array#{1, 4, 5} @ rc)
    }

    @test
    def patch22!(): Bool = region rc {
        let a = Array#{1, 2, 3} @ rc;
        Array.patch!(0, 2, Array#{4, 5, 6} @ rc, a);
        Array.sameElements(a, Array#{4, 5, 3} @ rc)
    }

    @test
    def patch23!(): Bool = region rc {
        let a = Array#{1, 2, 3} @ rc;
        Array.patch!(0, 3, Array#{4, 5, 6} @ rc, a);
        Array.sameElements(a, Array#{4, 5, 6} @ rc)
    }

    @test
    def patch24!(): Bool = region rc {
        let a = Array#{1, 2, 3, 4, 5, 6, 7} @ rc;
        Array.patch!(2, 4, Array#{14, 15, 16, 17} @ rc, a);
        Array.sameElements(a, Array#{1, 2, 14, 15, 16, 17, 7} @ rc)
    }

    @test
    def patch25!(): Bool = region rc {
        let a = Array#{1, 2, 3, 4, 5, 6, 7} @ rc;
        Array.patch!(-2, 4, Array#{14, 15, 16, 17} @ rc, a);
        Array.sameElements(a, Array#{16, 17, 3, 4, 5, 6, 7} @ rc)
    }

    @test
    def patch26!(): Bool = region rc {
        let a = Array#{1, 2, 3, 4, 5, 6, 7} @ rc;
        Array.patch!(4, 5, Array#{14, 15, 16, 17} @ rc, a);
        Array.sameElements(a, Array#{1, 2, 3, 4, 14, 15, 16} @ rc)
    }

    @test
    def patch27!(): Bool = region rc {
        let a = Array#{1, 2, 3, 4, 5, 6, 7} @ rc;
        Array.patch!(4, 2, Array#{14, 15, 16, 17} @ rc, a);
        Array.sameElements(a, Array#{1, 2, 3, 4, 14, 15, 7} @ rc)
    }

    @test
    def patch28!(): Bool = region rc {
        let a = Array#{1, 2, 3, 4, 5, 6, 7} @ rc;
        Array.patch!(-1, 10, Array#{-1, -2, -3, -4, -5, -6, -7, -8} @ rc, a);
        Array.sameElements(a, Array#{-2, -3, -4, -5, -6, -7, -8} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // intersperse                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def intersperse01(): Bool = region rc {
        let a = Array.intersperse(rc, 11, Array#{} @ rc);
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def intersperse02(): Bool = region rc {
        let a = Array.intersperse(rc, 11, Array#{1} @ rc);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @test
    def intersperse03(): Bool = region rc {
        let a = Array.intersperse(rc, 11, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{1,11,2} @ rc)
    }

    @test
    def intersperse04(): Bool = region rc {
        let a = Array.intersperse(rc, 11, Array#{1,2,3} @ rc);
        Array.sameElements(a, Array#{1,11,2,11,3} @ rc)
    }

    @test
    def intersperse05(): Bool = region rc {
        let a = Array.intersperse(rc, 11, Array#{1,2,3,4} @ rc);
        Array.sameElements(a, Array#{1,11,2,11,3,11,4} @ rc)
    }


    /////////////////////////////////////////////////////////////////////////////
    // intercalate                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def intercalate01(): Bool = region rc {
        let a = Array.intercalate(rc, ((Array#{} @ rc): Array[Int32, rc]), ((Array#{} @ rc): Array[Array[Int32, rc], rc]));
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, rc]))
    }

    @test
    def intercalate02(): Bool = region rc {
        let a = Array.intercalate(rc, ((Array#{} @ rc): Array[Int32, rc]), Array#{Array#{1} @ rc} @ rc);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @test
    def intercalate03(): Bool = region rc {
        let a = Array.intercalate(rc, Array#{11,12,13} @ rc, ((Array#{} @ rc): Array[Array[Int32, rc], rc]));
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, rc]))
    }

    @test
    def intercalate04(): Bool = region rc {
        let a = Array.intercalate(rc, ((Array#{} @ rc): Array[Int32, rc]), Array#{Array#{1} @ rc, Array#{2,3} @ rc} @ rc);
        Array.sameElements(a, Array#{1,2,3} @ rc)
    }

    @test
    def intercalate05(): Bool = region rc {
        let a = Array.intercalate(rc, Array#{11,12,13} @ rc, Array#{Array#{1} @ rc, Array#{2,3} @ rc} @ rc);
        Array.sameElements(a, Array#{1,11,12,13,2,3} @ rc)
    }

    @test
    def intercalate06(): Bool = region rc {
        let a = Array.intercalate(rc, ((Array#{} @ rc): Array[Int32, rc]), Array#{Array#{1} @ rc, Array#{2,3} @ rc, Array#{4} @ rc} @ rc);
        Array.sameElements(a, Array#{1,2,3,4} @ rc)
    }

    @test
    def intercalate07(): Bool = region rc {
        let a = Array.intercalate(rc, Array#{11,12,13} @ rc, Array#{Array#{1} @ rc, Array#{2,3} @ rc, Array#{4} @ rc} @ rc);
        Array.sameElements(a, Array#{1,11,12,13,2,3,11,12,13,4} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // transpose                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def transpose01(): Bool = region rc {
        let a = Array.transpose(rc, Array#{} @ rc);
        let b = Array.toList(a) |> List.map(Array.toList);
        (b: List[List[Unit]]) == Nil
    }

    @test
    def transpose02(): Bool = region rc {
        let a: Array[Array[Int32, _], _] = Array.transpose(rc, Array#{Array#{} @ rc} @ rc);
        let b = Array.toList(a) |> List.map(Array.toList);
        b == Nil :: Nil
    }

    @test
    def transpose03(): Bool = region rc {
        let a: Array[Array[Int32, _], _] = Array.transpose(rc, Array#{Array#{} @ rc, Array#{} @ rc} @ rc);
        let b = Array.toList(a) |> List.map(Array.toList);
        b == Nil :: Nil :: Nil
    }

    @test
    def transpose04(): Bool = region rc {
        let a: Array[Array[Int32, _], _] = Array.transpose(rc, Array#{Array#{} @ rc, Array#{} @ rc, Array#{} @ rc} @ rc);
        let b = Array.toList(a) |> List.map(Array.toList);
        b == Nil :: Nil :: Nil :: Nil
    }

    @test
    def transpose05(): Bool = region rc {
        let a = Array.transpose(rc, Array#{ Array#{1} @ rc } @ rc);
        let b = Array.toList(a) |> List.map(Array.toList);
        b == (1 :: Nil) :: Nil
    }

    @test
    def transpose06(): Bool = region rc {
        let a = Array.transpose(rc, Array#{Array#{1,2} @ rc} @ rc);
        let b = Array.toList(a) |> List.map(Array.toList);
        b == (1 :: Nil) :: (2 :: Nil) :: Nil
    }

    @test
    def transpose07(): Bool = region rc {
        let a = Array.transpose(rc, Array#{Array#{1, 2, 3} @ rc} @ rc);
        let b = Array.toList(a) |> List.map(Array.toList);
        b == (1 :: Nil) :: (2 :: Nil) :: (3 :: Nil) :: Nil
    }

    @test
    def transpose08(): Bool = region rc {
        let a = Array.transpose(rc, Array#{Array#{1, 2, 3, 4} @ rc} @ rc);
        let b = Array.toList(a) |> List.map(Array.toList);
        b == (1 :: Nil) :: (2 :: Nil) :: (3 :: Nil) :: (4 :: Nil) :: Nil
    }

    @test
    def transpose09(): Bool = region rc {
        let a = Array.transpose(rc, Array#{Array#{1} @ rc, Array#{2} @ rc} @ rc);
        let b = Array.toList(a) |> List.map(Array.toList);
        b ==   (1 :: 2 :: Nil) :: Nil
    }

    @test
    def transpose10(): Bool = region rc {
        let a = Array.transpose(rc, Array#{Array#{1} @ rc, Array#{2} @ rc, Array#{3} @ rc} @ rc);
        let b = Array.toList(a) |> List.map(Array.toList);
        b ==  (1 :: 2 :: 3 :: Nil) :: Nil
    }

    @test
    def transpose11(): Bool = region rc {
        let a = Array.transpose(rc, Array#{Array#{1} @ rc, Array#{2} @ rc, Array#{3} @ rc, Array#{4} @ rc} @ rc);
        let b = Array.toList(a) |> List.map(Array.toList);
        b == (1 :: 2 :: 3 :: 4 :: Nil) :: Nil
    }

    @test
    def transpose12(): Bool = region rc {
        let a = Array.transpose(rc, Array#{Array#{1, 2} @ rc, Array#{3, 4} @ rc} @ rc);
        let b = Array.toList(a) |> List.map(Array.toList);
        b == (1 :: 3 :: Nil) :: (2 :: 4 :: Nil) :: Nil
    }

    @test
    def transpose13(): Bool = region rc {
        let a = Array.transpose(rc, Array#{Array#{1, 2, 3} @ rc, Array#{4, 5, 6} @ rc} @ rc);
        let b = Array.toList(a) |> List.map(Array.toList);
        b == (1 :: 4 :: Nil) :: (2 :: 5 :: Nil) :: (3 :: 6 :: Nil) :: Nil
    }

    @test
    def transpose14(): Bool = region rc {
        let a = Array.transpose(rc, Array#{Array#{1, 2, 3, 4} @ rc, Array#{5, 6, 7, 8} @ rc} @ rc);
        let b = Array.toList(a) |> List.map(Array.toList);
        b == (1 :: 5 :: Nil) :: (2 :: 6 :: Nil) :: (3 :: 7 :: Nil) :: (4 :: 8 :: Nil) :: Nil
    }

    @test
    def transpose15(): Bool = region rc {
        let a = Array.transpose(rc, Array#{Array#{1, 2, 3, 4, 5} @ rc, Array#{6, 7, 8, 9, 10} @ rc} @ rc);
        let b = Array.toList(a) |> List.map(Array.toList);
        b == (1 :: 6 :: Nil) :: (2 :: 7 :: Nil) :: (3 :: 8 :: Nil) :: (4 :: 9 :: Nil) :: (5 :: 10 :: Nil) :: Nil
    }

    @test
    def transpose16(): Bool = region rc {
        let a = Array.transpose(rc, Array#{Array#{1, 2} @ rc, Array#{3, 4} @ rc, Array#{5, 6} @ rc} @ rc);
        let b = Array.toList(a) |> List.map(Array.toList);
        b == (1 :: 3 :: 5 :: Nil) :: (2 :: 4 :: 6 :: Nil) :: Nil
    }

    @test
    def transpose17(): Bool = region rc {
        let a = Array.transpose(rc, Array#{Array#{1, 2} @ rc, Array#{3, 4} @ rc, Array#{5, 6} @ rc, Array#{7,8} @ rc} @ rc);
        let b = Array.toList(a) |> List.map(Array.toList);
        b == (1 :: 3 :: 5 :: 7 :: Nil) :: (2 :: 4 :: 6 :: 8 :: Nil) :: Nil
    }

    @test
    def transpose18(): Bool = region rc {
        let a = Array.transpose(rc, Array#{Array#{1, 2} @ rc, Array#{3, 4} @ rc, Array#{5, 6} @ rc, Array#{7, 8} @ rc, Array#{9, 10} @ rc} @ rc);
        let b = Array.toList(a) |> List.map(Array.toList);
        b == (1 :: 3 :: 5 :: 7 :: 9 :: Nil) :: (2 :: 4 :: 6 :: 8 :: 10 :: Nil) :: Nil
    }

    @test
    def transpose19(): Bool = region rc {
        let a = Array.transpose(rc, Array#{Array#{1, 2, 3} @ rc, Array#{4, 5, 6} @ rc, Array#{7, 8, 9} @ rc} @ rc);
        let b = Array.toList(a) |> List.map(Array.toList);
        b == (1 :: 4 :: 7 :: Nil) :: (2 :: 5 :: 8 :: Nil) :: (3 :: 6 :: 9 :: Nil) :: Nil
    }

    @test
    def transpose20(): Bool = region rc {
        let a = Array.transpose(rc, Array#{Array#{1, 2, 3} @ rc, Array#{4, 5} @ rc, Array#{7, 8, 9} @ rc} @ rc);
        let b = Array.toList(a) |> List.map(Array.toList);
        b == (1 :: 2 :: 3 :: Nil) :: (4 :: 5 :: Nil) :: (7 :: 8 :: 9 :: Nil) :: Nil
    }

    @test
    def transpose21(): Bool = region rc {
        let a = Array.transpose(rc, Array#{Array#{1, 2, 3} @ rc, Array#{} @ rc, Array#{7, 8, 9} @ rc} @ rc);
        let b = Array.toList(a) |> List.map(Array.toList);
        b == (1 :: 2 :: 3 :: Nil) :: Nil :: (7 :: 8 :: 9 :: Nil) :: Nil
    }

    @test
    def transpose22(): Bool = region rc {
        let a = Array.transpose(rc, Array#{Array#{1, 2, 3} @ rc, Array#{4, 5, 6} @ rc, Array#{7, 8, 9, 10} @ rc} @ rc);
        let b = Array.toList(a) |> List.map(Array.toList);
        b == (1 :: 2 :: 3 :: Nil) :: (4 :: 5 :: 6 :: Nil) :: (7 :: 8 :: 9 :: 10 :: Nil) :: Nil
    }

    /////////////////////////////////////////////////////////////////////////////
    // isPrefixOf                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def isPrefixOf01(): Bool = region rc {
        Array.isPrefixOf(((Array#{} @ rc): Array[Int32, _]), ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def isPrefixOf02(): Bool = region rc {
        Array.isPrefixOf(Array#{1} @ rc, ((Array#{} @ rc): Array[Int32, _])) == false
    }

    @test
    def isPrefixOf03(): Bool = region rc {
        Array.isPrefixOf(((Array#{} @ rc): Array[Int32, _]), Array#{1} @ rc)
    }

    @test
    def isPrefixOf04(): Bool = region rc {
        Array.isPrefixOf(Array#{1} @ rc, Array#{1} @ rc)
    }

    @test
    def isPrefixOf05(): Bool = region rc {
        Array.isPrefixOf(Array#{2} @ rc, Array#{1} @ rc) == false
    }

    @test
    def isPrefixOf06(): Bool = region rc {
        Array.isPrefixOf(Array#{1,2} @ rc, Array#{1} @ rc) == false
    }

    @test
    def isPrefixOf07(): Bool = region rc {
        Array.isPrefixOf(((Array#{} @ rc): Array[Int32, _]), Array#{1,2} @ rc)
    }

    @test
    def isPrefixOf08(): Bool = region rc {
        Array.isPrefixOf(Array#{1} @ rc, Array#{1,2} @ rc)
    }

    @test
    def isPrefixOf09(): Bool = region rc {
        Array.isPrefixOf(Array#{2} @ rc, Array#{1,2} @ rc) == false
    }

    @test
    def isPrefixOf10(): Bool = region rc {
        Array.isPrefixOf(Array#{1,2} @ rc, Array#{1,2} @ rc)
    }

    @test
    def isPrefixOf11(): Bool = region rc {
        Array.isPrefixOf(Array#{1,3} @ rc, Array#{1,2} @ rc) == false
    }

    @test
    def isPrefixOf12(): Bool = region rc {
        Array.isPrefixOf(Array#{1,2,3} @ rc, Array#{1,2} @ rc) == false
    }

    @test
    def isPrefixOf13(): Bool = region rc {
        Array.isPrefixOf(Array#{1,2,3} @ rc, Array#{89,11,1,2,3,4} @ rc) == false
    }

    /////////////////////////////////////////////////////////////////////////////
    // isInfixOf                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def isInfixOf01(): Bool = region rc {
        Array.isInfixOf(((Array#{} @ rc): Array[Int32, _]), ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def isInfixOf02(): Bool = region rc {
        Array.isInfixOf(Array#{1} @ rc, ((Array#{} @ rc): Array[Int32, _])) == false
    }

    @test
    def isInfixOf03(): Bool = region rc {
        Array.isInfixOf(((Array#{} @ rc): Array[Int32, _]), Array#{1} @ rc)
    }

    @test
    def isInfixOf04(): Bool = region rc {
        Array.isInfixOf(Array#{1} @ rc, Array#{1} @ rc)
    }

    @test
    def isInfixOf05(): Bool = region rc {
        Array.isInfixOf(Array#{2} @ rc, Array#{1} @ rc) == false
    }

    @test
    def isInfixOf06(): Bool = region rc {
        Array.isInfixOf(Array#{1,2} @ rc, Array#{1} @ rc) == false
    }

    @test
    def isInfixOf07(): Bool = region rc {
        Array.isInfixOf(((Array#{} @ rc): Array[Int32, _]), Array#{1,2} @ rc)
    }

    @test
    def isInfixOf08(): Bool = region rc {
        Array.isInfixOf(Array#{1} @ rc, Array#{1,2} @ rc)
    }

    @test
    def isInfixOf09(): Bool = region rc {
        Array.isInfixOf(Array#{2} @ rc, Array#{1,2} @ rc)
    }

    @test
    def isInfixOf10(): Bool = region rc {
        Array.isInfixOf(Array#{1,2} @ rc, Array#{1,2} @ rc)
    }

    @test
    def isInfixOf11(): Bool = region rc {
        Array.isInfixOf(Array#{1,3} @ rc, Array#{1,2} @ rc) == false
    }

    @test
    def isInfixOf12(): Bool = region rc {
        Array.isInfixOf(Array#{1,2,3} @ rc, Array#{1,2} @ rc) == false
    }

    @test
    def isInfixOf13(): Bool = region rc {
        Array.isInfixOf(Array#{1,2,3} @ rc, Array#{89,11,1,2,3,4} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // isSuffixOf                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def isSuffixOf01(): Bool = region rc {
        Array.isSuffixOf(((Array#{} @ rc): Array[Int32, _]), ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def isSuffixOf02(): Bool = region rc {
        Array.isSuffixOf(Array#{1} @ rc, ((Array#{} @ rc): Array[Int32, _])) == false
    }

    @test
    def isSuffixOf03(): Bool = region rc {
        Array.isSuffixOf(((Array#{} @ rc): Array[Int32, _]), Array#{1} @ rc)
    }

    @test
    def isSuffixOf04(): Bool = region rc {
        Array.isSuffixOf(Array#{1} @ rc, Array#{1} @ rc)
    }

    @test
    def isSuffixOf05(): Bool = region rc {
        Array.isSuffixOf(Array#{2} @ rc, Array#{1} @ rc) == false
    }

    @test
    def isSuffixOf06(): Bool = region rc {
        Array.isSuffixOf(Array#{1,2} @ rc, Array#{1} @ rc) == false
    }

    @test
    def isSuffixOf07(): Bool = region rc {
        Array.isSuffixOf(((Array#{} @ rc): Array[Int32, _]), Array#{1,2} @ rc)
    }

    @test
    def isSuffixOf08(): Bool = region rc {
        Array.isSuffixOf(Array#{1} @ rc, Array#{1,2} @ rc) == false
    }

    @test
    def isSuffixOf09(): Bool = region rc {
        Array.isSuffixOf(Array#{2} @ rc, Array#{1,2} @ rc)
    }

    @test
    def isSuffixOf10(): Bool = region rc {
        Array.isSuffixOf(Array#{1,2} @ rc, Array#{1,2} @ rc)
    }

    @test
    def isSuffixOf11(): Bool = region rc {
        Array.isSuffixOf(Array#{1,3} @ rc, Array#{1,2} @ rc) == false
    }

    @test
    def isSuffixOf12(): Bool = region rc {
        Array.isSuffixOf(Array#{1,2,3} @ rc, Array#{1,2} @ rc) == false
    }

    @test
    def isSuffixOf13(): Bool = region rc {
        Array.isSuffixOf(Array#{1,2,3} @ rc, Array#{89,11,1,2,3} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // eq                                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testSameElements01(): Bool = region rc {
        (((Array#{} @ rc): Array[Int32, _])) `sameElements` (((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def testSameElements02(): Bool = region rc {
        not ((Array#{1} @ rc) `sameElements` Array#{} @ rc)
    }

    @test
    def testSameElements03(): Bool = region rc {
        not ((Array#{} @ rc) `sameElements` Array#{1} @ rc)
    }

    @test
    def testSameElements04(): Bool = region rc {
        (Array#{1} @ rc) `sameElements` Array#{1} @ rc
    }

    @test
    def testSameElements05(): Bool = region rc {
        not ((Array#{1} @ rc) `sameElements` Array#{2} @ rc)
    }

    @test
    def testSameElements06(): Bool = region rc {
        not ((Array#{1,2} @ rc) `sameElements` Array#{1} @ rc)
    }

    @test
    def testSameElements07(): Bool = region rc {
        not ((Array#{1} @ rc) `sameElements` Array#{1,2} @ rc)
    }

    @test
    def testSameElements08(): Bool = region rc {
        (Array#{1,2,3,4} @ rc) `sameElements` Array#{1,2,3,4} @ rc
    }

    /////////////////////////////////////////////////////////////////////////////
    // compare                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testCompare01(): Bool = region rc {
        (((Array#{} @ rc): Array[Int32, _])) `Array.compare` (((Array#{} @ rc): Array[Int32, _])) == Comparison.EqualTo
    }

    @test
    def testCompare02(): Bool = region rc {
        ((Array#{1} @ rc) `Array.compare` (Array#{} @ rc)) == Comparison.GreaterThan
    }

    @test
    def testCompare03(): Bool = region rc {
        ((Array#{} @ rc) `Array.compare` (Array#{1} @ rc)) == Comparison.LessThan
    }

    @test
    def testCompare04(): Bool = region rc {
        ((Array#{1} @ rc) `Array.compare` (Array#{1} @ rc)) == Comparison.EqualTo
    }

    @test
    def testCompare05(): Bool = region rc {
        ((Array#{1} @ rc) `Array.compare` (Array#{2} @ rc)) == Comparison.LessThan
    }

    @test
    def testCompare06(): Bool = region rc {
        ((Array#{2} @ rc) `Array.compare` (Array#{1} @ rc)) == Comparison.GreaterThan
    }

    @test
    def testCompare07(): Bool = region rc {
        ((Array#{1} @ rc) `Array.compare` (Array#{1,2} @ rc)) == Comparison.LessThan
    }

    @test
    def testCompare08(): Bool = region rc {
        ((Array#{1,2} @ rc) `Array.compare` (Array#{1} @ rc)) == Comparison.GreaterThan
    }

    @test
    def testCompare09(): Bool = region rc {
        ((Array#{1} @ rc) `Array.compare` (Array#{0,0} @ rc)) == Comparison.GreaterThan
    }

    @test
    def testCompare10(): Bool = region rc {
        ((Array#{0,0} @ rc) `Array.compare` (Array#{1} @ rc)) == Comparison.LessThan
    }

    @test
    def testCompare11(): Bool = region rc {
        ((Array#{1,2} @ rc) `Array.compare` (Array#{1,1} @ rc)) == Comparison.GreaterThan
    }

    @test
    def testCompare12(): Bool = region rc {
        ((Array#{1,2} @ rc) `Array.compare` (Array#{1,3} @ rc)) == Comparison.LessThan
    }

    @test
    def testCompare13(): Bool = region rc {
        ((Array#{1,2,3,4} @ rc) `Array.compare` (Array#{1,2,3,4} @ rc)) == Comparison.EqualTo
    }

    /////////////////////////////////////////////////////////////////////////////
    // fold                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def fold01(): Bool = region rc {
        Array.fold(((Array#{} @ rc: Array[Unit, _]))) == ()
    }

    @test
    def fold02(): Bool = region rc {
        Array.fold(Array#{"a"} @ rc) == "a"
    }

    @test
    def fold03(): Bool = region rc {
        Array.fold(Array#{"a", "b", "c"} @ rc) == "abc"
    }

    @test
    def fold04(): Bool = region rc {
        Array.fold(Array#{("yes", "no"), ("no", "yes")} @ rc) == ("yesno", "noyes")
    }

    /////////////////////////////////////////////////////////////////////////////
    // foldLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldLeft01(): Bool = region rc {
        Array.foldLeft((i, e) -> (i - e) * (e `Int32.remainder` 2 + 1), 100, Array#{} @ rc) == 100
    }

    @test
    def foldLeft02(): Bool = region rc {
        Array.foldLeft((i, e) -> (i - e) * (e `Int32.remainder` 2 + 1), 100, Array#{1} @ rc) == 198
    }

    @test
    def foldLeft03(): Bool = region rc {
        Array.foldLeft((i, e) -> (i - e) * (e `Int32.remainder` 2 + 1), 100, Array#{1,2} @ rc) == 196
    }

    @test
    def foldLeft04(): Bool = region rc {
        Array.foldLeft((i, e) -> (i - e) * (e `Int32.remainder` 2 + 1), 100, Array#{1,2,3} @ rc) == 386
    }

    /////////////////////////////////////////////////////////////////////////////
    // foldRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldRight01(): Bool = region rc {
        Array.foldRight((e, i) -> (i - e) * (e `Int32.remainder` 2 + 1), 100, Array#{} @ rc) == 100
    }

    @test
    def foldRight02(): Bool = region rc {
        Array.foldRight((e, i) -> (i - e) * (e `Int32.remainder` 2 + 1), 100, Array#{1} @ rc) == 198
    }

    @test
    def foldRight03(): Bool = region rc {
        Array.foldRight((e, i) -> (i - e) * (e `Int32.remainder` 2 + 1), 100, Array#{1,2} @ rc) == 194
    }

    @test
    def foldRight04(): Bool = region rc {
        Array.foldRight((e, i) -> (i - e) * (e `Int32.remainder` 2 + 1), 100, Array#{1,2,3} @ rc) == 382
    }

    /////////////////////////////////////////////////////////////////////////////
    // foldRightWithCont                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldRightWithCont01(): Bool = region rc {
        Array.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.remainder` 2 + 1), 100, Array#{} @ rc) == 100
    }

    @test
    def foldRightWithCont02(): Bool = region rc {
        Array.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.remainder` 2 + 1), 100, Array#{1} @ rc) == 198
    }

    @test
    def foldRightWithCont03(): Bool = region rc {
        Array.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.remainder` 2 + 1), 100, Array#{1,2} @ rc) == 194
    }

    @test
    def foldRightWithCont04(): Bool = region rc {
        Array.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.remainder` 2 + 1), 100, Array#{1,2,3} @ rc) == 382
    }

    /////////////////////////////////////////////////////////////////////////////
    // foldMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldMap01(): Bool = region rc {
        Array.foldMap(x -> 2 * x, Array#{} @ rc) == 0
    }

    @test
    def foldMap02(): Bool = region rc {
        Array.foldMap(x -> 2 * x, Array#{1, 2} @ rc) == 6
    }

    @test
    def foldMap03(): Bool = region rc {
        Array.foldMap(x -> if (x == "a") "b" else x, Array#{"a"} @ rc) == "b"
    }

    @test
    def foldMap04(): Bool = region rc {
        Array.foldMap(x -> if (x == "c") "b" else x, Array#{"a", "b", "c"} @ rc) == "abb"
    }

    @test
    def foldMap05(): Bool = region rc {
        Array.foldMap(Int32.toString, Array#{1, 2, 3} @ rc) == "123"
    }

    /////////////////////////////////////////////////////////////////////////////
    // reduceLeft                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reduceLeft01(): Bool = region rc {
        Array.reduceLeft((a, b) -> a - b, ((Array#{} @ rc): Array[Int32, _])) == None
    }

    @test
    def reduceLeft02(): Bool = region rc {
        Array.reduceLeft((a, b) -> a - b, Array#{1} @ rc) == Some(1)
    }

    @test
    def reduceLeft03(): Bool = region rc {
        Array.reduceLeft((a, b) -> a - b, Array#{1,2} @ rc) == Some(-1)
    }

    @test
    def reduceLeft04(): Bool = region rc {
        Array.reduceLeft((a, b) -> a - b, Array#{1,2,3} @ rc) == Some(-4)
    }

    @test
    def reduceLeft05(): Bool = region rc {
        Array.reduceLeft((a, b) -> a - b, Array#{1,2,3,4} @ rc) == Some(-8)
    }

    /////////////////////////////////////////////////////////////////////////////
    // reduceRight                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reduceRight01(): Bool = region rc {
        Array.reduceRight((a, b) -> a - b, ((Array#{} @ rc): Array[Int32, _])) == None
    }

    @test
    def reduceRight02(): Bool = region rc {
        Array.reduceRight((a, b) -> a - b, Array#{1} @ rc) == Some(1)
    }

    @test
    def reduceRight03(): Bool = region rc {
        Array.reduceRight((a, b) -> a - b, Array#{1,2} @ rc) == Some(-1)
    }

    @test
    def reduceRight04(): Bool = region rc {
        Array.reduceRight((a, b) -> a - b, Array#{1,2,3} @ rc) == Some(2)
    }

    @test
    def reduceRight05(): Bool = region rc {
        Array.reduceRight((a, b) -> a - b, Array#{1,2,3,4} @ rc) == Some(-2)
    }

    /////////////////////////////////////////////////////////////////////////////
    // count                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def count01(): Bool = region rc {
        Array.count(i -> i > 3, Array#{} @ rc) == 0
    }

    @test
    def count02(): Bool = region rc {
        Array.count(i -> i > 3, Array#{1} @ rc) == 0
    }

    @test
    def count03(): Bool = region rc {
        Array.count(i -> i > 3, Array#{4} @ rc) == 1
    }

    @test
    def count04(): Bool = region rc {
        Array.count(i -> i > 3, Array#{1,2} @ rc) == 0
    }

    @test
    def count05(): Bool = region rc {
        Array.count(i -> i > 3, Array#{1,8} @ rc) == 1
    }

    @test
    def count06(): Bool = region rc {
        Array.count(i -> i > 3, Array#{8,1} @ rc) == 1
    }

    @test
    def count07(): Bool = region rc {
        Array.count(i -> i > 3, Array#{6,7} @ rc) == 2
    }


    /////////////////////////////////////////////////////////////////////////////
    // flatten                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def flatten01(): Bool = region rc {
        let a = Array.flatten(rc, ((Array#{} @ rc): Array[Array[Int32, rc], rc]));
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, rc]))
    }

    @test
    def flatten02(): Bool = region rc {
        let a = Array.flatten(rc, ((Array#{Array#{} @ rc} @ rc): Array[Array[Int32, rc], rc]));
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, rc]))
    }

    @test
    def flatten03(): Bool = region rc {
        let a = Array.flatten(rc, Array#{Array#{1} @ rc} @ rc);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @test
    def flatten04(): Bool = region rc {
        let a= Array.flatten(rc, Array#{Array#{1,2} @ rc} @ rc);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @test
    def flatten05(): Bool = region rc {
        let a = Array.flatten(rc, ((Array#{Array#{} @ rc,Array#{} @ rc} @ rc): Array[Array[Int32, rc], rc]));
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, rc]))
    }

    @test
    def flatten06(): Bool = region rc {
        let a = Array.flatten(rc, Array#{Array#{1} @ rc,Array#{} @ rc} @ rc);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @test
    def flatten07(): Bool = region rc {
        let a = Array.flatten(rc, Array#{Array#{} @ rc,Array#{1} @ rc} @ rc);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @test
    def flatten08(): Bool = region rc {
        let a = Array.flatten(rc, Array#{Array#{1} @ rc,Array#{2} @ rc} @ rc);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @test
    def flatten09(): Bool = region rc {
        let a = Array.flatten(rc, Array#{Array#{1,2} @ rc,Array#{3,4,5} @ rc} @ rc);
        Array.sameElements(a, Array#{1,2,3,4,5} @ rc)
    }

    @test
    def flatten10(): Bool = region rc {
        let a = Array.flatten(rc, Array#{Array#{1} @ rc,Array#{2,3} @ rc,Array#{4} @ rc} @ rc);
        Array.sameElements(a, Array#{1,2,3,4} @ rc)
    }
    /////////////////////////////////////////////////////////////////////////////
    // exists                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def exists01(): Bool = region rc {
        Array.exists(i -> i > 3, ((Array#{} @ rc): Array[Int32, _])) == false
    }

    @test
    def exists02(): Bool = region rc {
        Array.exists(i -> i > 3, Array#{1} @ rc) == false
    }

    @test
    def exists03(): Bool = region rc {
        Array.exists(i -> i > 3, Array#{5} @ rc) == true
    }

    @test
    def exists04(): Bool = region rc {
        Array.exists(i -> i > 3, Array#{1,2} @ rc) == false
    }

    @test
    def exists05(): Bool = region rc {
        Array.exists(i -> i > 3, Array#{1,6} @ rc) == true
    }

    @test
    def exists06(): Bool = region rc {
        Array.exists(i -> i > 3, Array#{6,1} @ rc) == true
    }

    @test
    def exists07(): Bool = region rc {
        Array.exists(i -> i > 3, Array#{16,6} @ rc) == true
    }

    @test
    def exists08(): Bool = region rc {
        Array.exists(i -> i > 3, Array#{1,-9,3} @ rc) == false
    }

    @test
    def exists09(): Bool = region rc {
        Array.exists(i -> i > 3, Array#{1,9,3} @ rc) == true
    }

    /////////////////////////////////////////////////////////////////////////////
    // forAll                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def forAll01(): Bool = region rc {
        Array.forAll(i -> i > 3, ((Array#{} @ rc): Array[Int32, _])) == true
    }

    @test
    def forAll02(): Bool = region rc {
        Array.forAll(i -> i > 3, Array#{1} @ rc) == false
    }

    @test
    def forAll03(): Bool = region rc {
        Array.forAll(i -> i > 3, Array#{5} @ rc) == true
    }

    @test
    def forAll04(): Bool = region rc {
        Array.forAll(i -> i > 3, Array#{1,2} @ rc) == false
    }

    @test
    def forAll05(): Bool = region rc {
        Array.forAll(i -> i > 3, Array#{1,6} @ rc) == false
    }

    @test
    def forAll06(): Bool = region rc {
        Array.forAll(i -> i > 3, Array#{6,1} @ rc) == false
    }

    @test
    def forAll07(): Bool = region rc {
        Array.forAll(i -> i > 3, Array#{16,6} @ rc) == true
    }

    @test
    def forAll08(): Bool = region rc {
        Array.forAll(i -> i > 3, Array#{1,-9,3} @ rc) == false
    }

    @test
    def forAll09(): Bool = region rc {
        Array.forAll(i -> i > 3, Array#{1,9,3} @ rc) == false
    }

    @test
    def forAll10(): Bool = region rc {
        Array.forAll(i -> i > 3, Array#{11,9,31} @ rc) == true
    }

    /////////////////////////////////////////////////////////////////////////////
    // filter                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filter01(): Bool = region rc {
        let a = Array.filter(rc, i -> i > 3, ((Array#{} @ rc): Array[Int32, _]));
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def filter02(): Bool = region rc {
        let a = Array.filter(rc, i -> i > 3, Array#{2} @ rc);
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def filter03(): Bool = region rc {
        let a = Array.filter(rc, i -> i > 3, Array#{4} @ rc);
        Array.sameElements(a, Array#{4} @ rc)
    }

    @test
    def filter04(): Bool = region rc {
        let a = Array.filter(rc, i -> i > 3, Array#{1,3} @ rc);
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def filter05(): Bool = region rc {
        let a = Array.filter(rc, i -> i > 3, Array#{1,8} @ rc);
        Array.sameElements(a, Array#{8} @ rc)
    }

    @test
    def filter06(): Bool = region rc {
        let a = Array.filter(rc, i -> i > 3, Array#{8,1} @ rc);
        Array.sameElements(a, Array#{8} @ rc)
    }

    @test
    def filter07(): Bool = region rc {
        let a = Array.filter(rc, i -> i > 3, Array#{8,9} @ rc);
        Array.sameElements(a, Array#{8,9} @ rc)
    }

    @test
    def filter08(): Bool = region rc {
        let a = Array.filter(rc, i -> i > 3, Array#{1,4,11,2,-22,17} @ rc);
        Array.sameElements(a, Array#{4,11,17} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // partition                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def partition01(): Bool = region rc {
        let (a,b) = Array.partition(rc, rc, i -> i > 3, ((Array#{} @ rc): Array[Int32, _]));
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])) and Array.sameElements(b, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def partition02(): Bool = region rc {
        let (a,b) = Array.partition(rc, rc, i -> i > 3, Array#{1} @ rc);
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])) and Array.sameElements(b, Array#{1} @ rc)
    }

    @test
    def partition03(): Bool = region rc {
        let (a,b) = Array.partition(rc, rc, i -> i > 3, Array#{4} @ rc);
        Array.sameElements(a, Array#{4} @ rc) and Array.sameElements(b, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def partition04(): Bool = region rc {
        let (a,b) = Array.partition(rc, rc, i -> i > 3, Array#{1,2} @ rc);
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])) and Array.sameElements(b, Array#{1,2} @ rc)
    }

    @test
    def partition05(): Bool = region rc {
        let (a,b) = Array.partition(rc, rc, i -> i > 3, Array#{1,5} @ rc);
        Array.sameElements(a, Array#{5} @ rc) and Array.sameElements(b, Array#{1} @ rc)
    }

    @test
    def partition06(): Bool = region rc {
        let (a,b) = Array.partition(rc, rc, i -> i > 3, Array#{5,1} @ rc);
        Array.sameElements(a, Array#{5} @ rc) and Array.sameElements(b, Array#{1} @ rc)
    }

    @test
    def partition07(): Bool = region rc {
        let (a,b) = Array.partition(rc, rc, i -> i > 3, Array#{5,8} @ rc);
        Array.sameElements(a, Array#{5,8} @ rc) and Array.sameElements(b, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def partition08(): Bool = region rc {
        let (a,b) = Array.partition(rc, rc, i -> i > 3, Array#{4, -3, -5, 1, 2, 16, 7, 1, 7} @ rc);
        Array.sameElements(a, Array#{4, 16, 7, 7} @ rc) and Array.sameElements(b, Array#{-3, -5, 1, 2, 1} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // span                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def span01(): Bool = region rc {
        let (a,b) = Array.span(rc, rc, i -> i > 3, ((Array#{} @ rc): Array[Int32, _]));
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])) and Array.sameElements(b, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def span02(): Bool = region rc {
        let (a,b) = Array.span(rc, rc, i -> i > 3, Array#{1} @ rc);
        Array.sameElements(a,((Array#{} @ rc): Array[Int32, _])) and Array.sameElements(b, Array#{1} @ rc)
    }

    @test
    def span03(): Bool = region rc {
        let (a,b) = Array.span(rc, rc, i -> i > 3, Array#{4} @ rc);
        Array.sameElements(a, Array#{4} @ rc) and Array.sameElements(b, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def span04(): Bool = region rc {
        let (a,b) = Array.span(rc, rc, i -> i > 3, Array#{1,2} @ rc);
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])) and Array.sameElements(b, Array#{1,2} @ rc)
    }

    @test
    def span05(): Bool = region rc {
        let (a,b) = Array.span(rc, rc, i -> i > 3, Array#{1,5} @ rc);
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _])) and Array.sameElements(b, Array#{1,5} @ rc)
    }

    @test
    def span06(): Bool = region rc {
        let (a,b) = Array.span(rc, rc, i -> i > 3, Array#{5,1} @ rc);
        Array.sameElements(a, Array#{5} @ rc) and Array.sameElements(b, Array#{1} @ rc)
    }

    @test
    def span07(): Bool = region rc {
        let (a,b) = Array.span(rc, rc, i -> i > 3, Array#{5,8} @ rc);
        Array.sameElements(a, Array#{5,8} @ rc) and Array.sameElements(b, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def span08(): Bool = region rc {
        let (a,b) = Array.span(rc, rc, i -> i > 3, Array#{4, 6, -3, 11, -5, 1, 2, 16, 7, 1, 7} @ rc);
        Array.sameElements(a, Array#{4, 6} @ rc) and Array.sameElements(b, Array#{-3, 11, -5, 1, 2, 16, 7, 1, 7} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // drop                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def drop01(): Bool = region rc {
        let a = Array.drop(rc, -1, ((Array#{} @ rc): Array[Int32, _]));
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def drop02(): Bool = region rc {
        let a = Array.drop(rc, 0, ((Array#{} @ rc): Array[Int32, _]));
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def drop03(): Bool = region rc {
        let a = Array.drop(rc, 1, ((Array#{} @ rc): Array[Int32, _]));
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def drop04(): Bool = region rc {
        let a = Array.drop(rc, -1, Array#{1} @ rc);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @test
    def drop05(): Bool = region rc {
        let a = Array.drop(rc, 0, Array#{1} @ rc);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @test
    def drop06(): Bool = region rc {
        let a = Array.drop(rc, 1, Array#{1} @ rc);
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def drop07(): Bool = region rc {
        let a = Array.drop(rc, 2, Array#{1} @ rc);
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def drop08(): Bool = region rc {
        let a = Array.drop(rc, 0, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @test
    def drop09(): Bool = region rc {
        let a = Array.drop(rc, 1, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{2} @ rc)
    }

    @test
    def drop10(): Bool = region rc {
        let a = Array.drop(rc, 2, Array#{1,2} @ rc);
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def drop11(): Bool = region rc {
        let a = Array.drop(rc, 2, Array#{1,2,3,4,5,6} @ rc);
        Array.sameElements(a, Array#{3,4,5,6} @ rc)
    }

    @test
    def drop12(): Bool = region rc {
        let a = Array.drop(rc, 4, Array#{1,2,3,4,5,6} @ rc);
        Array.sameElements(a, Array#{5,6} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // dropLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def dropLeft01(): Bool = region rc {
        let a = Array.dropLeft(rc, -1, ((Array#{} @ rc): Array[Int32, _]));
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def dropLeft02(): Bool = region rc {
        let a = Array.dropLeft(rc, 0, ((Array#{} @ rc): Array[Int32, _]));
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def dropLeft03(): Bool = region rc {
        let a = Array.dropLeft(rc, 1, ((Array#{} @ rc): Array[Int32, _]));
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def dropLeft04(): Bool = region rc {
        let a = Array.dropLeft(rc, -1, Array#{1} @ rc);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @test
    def dropLeft05(): Bool = region rc {
        let a = Array.dropLeft(rc, 0, Array#{1} @ rc);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @test
    def dropLeft06(): Bool = region rc {
        let a = Array.dropLeft(rc, 1, Array#{1} @ rc);
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def dropLeft07(): Bool = region rc {
        let a = Array.dropLeft(rc, 2, Array#{1} @ rc);
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def dropLeft08(): Bool = region rc {
        let a = Array.dropLeft(rc, 0, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @test
    def dropLeft09(): Bool = region rc {
        let a = Array.dropLeft(rc, 1, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{2} @ rc)
    }

    @test
    def dropLeft10(): Bool = region rc {
        let a = Array.dropLeft(rc, 2, Array#{1,2} @ rc);
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def dropLeft11(): Bool = region rc {
        let a = Array.dropLeft(rc, 2, Array#{1,2,3,4,5,6} @ rc);
        Array.sameElements(a, Array#{3,4,5,6} @ rc)
    }

    @test
    def dropLeft12(): Bool = region rc {
        let a = Array.dropLeft(rc, 4, Array#{1,2,3,4,5,6} @ rc);
        Array.sameElements(a, Array#{5,6} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // dropRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def dropRight01(): Bool = region rc {
        let a = Array.dropRight(rc, -1, ((Array#{} @ rc): Array[Int32, _]));
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def dropRight02(): Bool = region rc {
        let a = Array.dropRight(rc, 0, ((Array#{} @ rc): Array[Int32, _]));
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def dropRight03(): Bool = region rc {
        let a = Array.dropRight(rc, 1, ((Array#{} @ rc): Array[Int32, _]));
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def dropRight04(): Bool = region rc {
        let a = Array.dropRight(rc, -1, Array#{1} @ rc);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @test
    def dropRight05(): Bool = region rc {
        let a = Array.dropRight(rc, 0, Array#{1} @ rc);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @test
    def dropRight06(): Bool = region rc {
        let a = Array.dropRight(rc, 1, Array#{1} @ rc);
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def dropRight07(): Bool = region rc {
        let a = Array.dropRight(rc, 2, Array#{1} @ rc);
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def dropRight08(): Bool = region rc {
        let a = Array.dropRight(rc, 0, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @test
    def dropRight09(): Bool = region rc {
        let a = Array.dropRight(rc, 1, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @test
    def dropRight10(): Bool = region rc {
        let a = Array.dropRight(rc, 2, Array#{1,2} @ rc);
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def dropRight11(): Bool = region rc {
        let a = Array.dropRight(rc, 2, Array#{1,2,3,4,5,6} @ rc);
        Array.sameElements(a, Array#{1,2,3,4} @ rc)
    }

    @test
    def dropRight12(): Bool = region rc {
        let a = Array.dropRight(rc, 4, Array#{1,2,3,4,5,6} @ rc);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // dropWhile                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def dropWhile01(): Bool = region rc {
        let a = Array.dropWhile(rc, i -> i > 3, Array#{} @ rc);
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def dropWhile02(): Bool = region rc {
        let a = Array.dropWhile(rc, i -> i > 3, Array#{1} @ rc);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @test
    def dropWhile03(): Bool = region rc {
        let a = Array.dropWhile(rc, i -> i > 3, Array#{4} @ rc);
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def dropWhile04(): Bool = region rc {
        let a = Array.dropWhile(rc, i -> i > 3, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @test
    def dropWhile05(): Bool = region rc {
        let a = Array.dropWhile(rc, i -> i > 3, Array#{1,5} @ rc);
        Array.sameElements(a, Array#{1,5} @ rc)
    }

    @test
    def dropWhile06(): Bool = region rc {
        let a = Array.dropWhile(rc, i -> i > 3, Array#{5,1} @ rc);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @test
    def dropWhile07(): Bool = region rc {
        let a = Array.dropWhile(rc, i -> i > 3, Array#{5,8} @ rc);
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def dropWhile08(): Bool = region rc {
        let a = Array.dropWhile(rc, i -> i > 3, Array#{4, 6, -3, 11, -5, 1, 2, 16, 7, 1, 7} @ rc);
        Array.sameElements(a, Array#{-3, 11, -5, 1, 2, 16, 7, 1, 7} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // dropWhileLeft                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def dropWhileLeft01(): Bool = region rc {
        let a = Array.dropWhileLeft(rc, i -> i > 3, Array#{} @ rc);
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def dropWhileLeft02(): Bool = region rc {
        let a = Array.dropWhileLeft(rc, i -> i > 3, Array#{1} @ rc);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @test
    def dropWhileLeft03(): Bool = region rc {
        let a = Array.dropWhileLeft(rc, i -> i > 3, Array#{4} @ rc);
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def dropWhileLeft04(): Bool = region rc {
        let a = Array.dropWhileLeft(rc, i -> i > 3, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @test
    def dropWhileLeft05(): Bool = region rc {
        let a = Array.dropWhileLeft(rc, i -> i > 3, Array#{1,5} @ rc);
        Array.sameElements(a, Array#{1,5} @ rc)
    }

    @test
    def dropWhileLeft06(): Bool = region rc {
        let a = Array.dropWhileLeft(rc, i -> i > 3, Array#{5,1} @ rc);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @test
    def dropWhileLeft07(): Bool = region rc {
        let a = Array.dropWhileLeft(rc, i -> i > 3, Array#{5,8} @ rc);
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def dropWhileLeft08(): Bool = region rc {
        let a = Array.dropWhileLeft(rc, i -> i > 3, Array#{4, 6, -3, 11, -5, 1, 2, 16, 7, 1, 7} @ rc);
        Array.sameElements(a, Array#{-3, 11, -5, 1, 2, 16, 7, 1, 7} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // dropWhileRight                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def dropWhileRight01(): Bool = region rc {
        let a = Array.dropWhileRight(rc, i -> i > 3, Array#{} @ rc);
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def dropWhileRight02(): Bool = region rc {
        let a = Array.dropWhileRight(rc, i -> i > 3, Array#{1} @ rc);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @test
    def dropWhileRight03(): Bool = region rc {
        let a = Array.dropWhileRight(rc, i -> i > 3, Array#{4} @ rc);
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def dropWhileRight04(): Bool = region rc {
        let a = Array.dropWhileRight(rc, i -> i > 3, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @test
    def dropWhileRight05(): Bool = region rc {
        let a = Array.dropWhileRight(rc, i -> i > 3, Array#{1,5} @ rc);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @test
    def dropWhileRight06(): Bool = region rc {
        let a = Array.dropWhileRight(rc, i -> i > 3, Array#{5,1} @ rc);
        Array.sameElements(a, Array#{5,1} @ rc)
    }

    @test
    def dropWhileRight07(): Bool = region rc {
        let a = Array.dropWhileRight(rc, i -> i > 3, Array#{5,8} @ rc);
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def dropWhileRight08(): Bool = region rc {
        let a = Array.dropWhileRight(rc, i -> i > 3, Array#{4, 6, -3, 11, -5, 1, 2, 16, 7, 1, 7} @ rc);
        Array.sameElements(a, Array#{4, 6, -3, 11, -5, 1, 2, 16, 7, 1} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // take                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def take01(): Bool = region rc {
        let a = Array.take(rc, -1, ((Array#{} @ rc): Array[Int32, _]));
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def take02(): Bool = region rc {
        let a = Array.take(rc, 0, ((Array#{} @ rc): Array[Int32, _]));
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def take03(): Bool = region rc {
        let a = Array.take(rc, 1, ((Array#{} @ rc): Array[Int32, _]));
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def take04(): Bool = region rc {
        let a = Array.take(rc, -1, Array#{1} @ rc);
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def take05(): Bool = region rc {
        let a = Array.take(rc, 0, Array#{1} @ rc);
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))

    }
    @test
    def take06(): Bool = region rc {
        let a = Array.take(rc, 1, Array#{1} @ rc);
        Array.sameElements(a, Array#{1} @ rc)

    }
    @test
    def take07(): Bool = region rc {
        let a = Array.take(rc, 2, Array#{1} @ rc);
        Array.sameElements(a, Array#{1} @ rc)

    }
    @test
    def take08(): Bool = region rc {
        let a = Array.take(rc, 0, Array#{1,2} @ rc);
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def take09(): Bool = region rc {
        let a = Array.take(rc, 1, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @test
    def take10(): Bool = region rc {
        let a = Array.take(rc, 2, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @test
    def take11(): Bool = region rc {
        let a = Array.take(rc, 2, Array#{1,2,3,4,5,6} @ rc);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @test
    def take12(): Bool = region rc {
        let a = Array.take(rc, 4, Array#{1,2,3,4,5,6} @ rc);
        Array.sameElements(a, Array#{1,2,3,4} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // takeLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def takeLeft01(): Bool = region rc {
        let a = Array.takeLeft(rc, -1, ((Array#{} @ rc): Array[Int32, _]));
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def takeLeft02(): Bool = region rc {
        let a = Array.takeLeft(rc, 0, ((Array#{} @ rc): Array[Int32, _]));
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def takeLeft03(): Bool = region rc {
        let a = Array.takeLeft(rc, 1, ((Array#{} @ rc): Array[Int32, _]));
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def takeLeft04(): Bool = region rc {
        let a = Array.takeLeft(rc, -1, Array#{1} @ rc);
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def takeLeft05(): Bool = region rc {
        let a = Array.takeLeft(rc, 0, Array#{1} @ rc);
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def takeLeft06(): Bool = region rc {
        let a = Array.takeLeft(rc, 1, Array#{1} @ rc);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @test
    def takeLeft07(): Bool = region rc {
        let a = Array.takeLeft(rc, 2, Array#{1} @ rc);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @test
    def takeLeft08(): Bool = region rc {
        let a = Array.takeLeft(rc, 0, Array#{1,2} @ rc);
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def takeLeft09(): Bool = region rc {
        let a = Array.takeLeft(rc, 1, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @test
    def takeLeft10(): Bool = region rc {
        let a = Array.takeLeft(rc, 2, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @test
    def takeLeft11(): Bool = region rc {
        let a = Array.takeLeft(rc, 2, Array#{1,2,3,4,5,6} @ rc);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @test
    def takeLeft12(): Bool = region rc {
        let a = Array.takeLeft(rc, 4, Array#{1,2,3,4,5,6} @ rc);
        Array.sameElements(a, Array#{1,2,3,4} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // takeRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def takeRight01(): Bool = region rc {
        let a = Array.takeRight(rc, -1, ((Array#{} @ rc): Array[Int32, _]));
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def takeRight02(): Bool = region rc {
        let a = Array.takeRight(rc, 0, ((Array#{} @ rc): Array[Int32, _]));
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def takeRight03(): Bool = region rc {
        let a = Array.takeRight(rc, 1, ((Array#{} @ rc): Array[Int32, _]));
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def takeRight04(): Bool = region rc {
        let a = Array.takeRight(rc, -1, Array#{1} @ rc);
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def takeRight05(): Bool = region rc {
        let a = Array.takeRight(rc, 0, Array#{1} @ rc);
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def takeRight06(): Bool = region rc {
        let a = Array.takeRight(rc, 1, Array#{1} @ rc);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @test
    def takeRight07(): Bool = region rc {
        let a = Array.takeRight(rc, 2, Array#{1} @ rc);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @test
    def takeRight08(): Bool = region rc {
        let a = Array.takeRight(rc, 0, Array#{1,2} @ rc);
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def takeRight09(): Bool = region rc {
        let a = Array.takeRight(rc, 1, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{2} @ rc)
    }

    @test
    def takeRight10(): Bool = region rc {
        let a = Array.takeRight(rc, 2, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @test
    def takeRight11(): Bool = region rc {
        let a = Array.takeRight(rc, 2, Array#{1,2,3,4,5,6} @ rc);
        Array.sameElements(a, Array#{5,6} @ rc)
    }

    @test
    def takeRight12(): Bool = region rc {
        let a = Array.takeRight(rc, 4, Array#{1,2,3,4,5,6} @ rc);
        Array.sameElements(a, Array#{3,4,5,6} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // takeWhile                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def takeWhile01(): Bool = region rc {
        let a = Array.takeWhile(rc, i -> i > 3, ((Array#{} @ rc): Array[Int32, _]));
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def takeWhile02(): Bool = region rc {
        let a = Array.takeWhile(rc, i -> i > 3, Array#{1} @ rc);
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def takeWhile03(): Bool = region rc {
        let a = Array.takeWhile(rc, i -> i > 3, Array#{4} @ rc);
        Array.sameElements(a, Array#{4} @ rc)
    }

    @test
    def takeWhile04(): Bool = region rc {
        let a = Array.takeWhile(rc, i -> i > 3, Array#{1,2} @ rc);
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def takeWhile05(): Bool = region rc {
        let a = Array.takeWhile(rc, i -> i > 3, Array#{1,5} @ rc);
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def takeWhile06(): Bool = region rc {
        let a = Array.takeWhile(rc, i -> i > 3, Array#{5,1} @ rc);
        Array.sameElements(a, Array#{5} @ rc)
    }

    @test
    def takeWhile07(): Bool = region rc {
        let a = Array.takeWhile(rc, i -> i > 3, Array#{5,8} @ rc);
        Array.sameElements(a, Array#{5,8} @ rc)
    }

    @test
    def takeWhile08(): Bool = region rc {
        let a = Array.takeWhile(rc, i -> i > 3, Array#{4, 6, -3, 11, -5, 1, 2, 16, 7, 1, 7} @ rc);
        Array.sameElements(a, Array#{4, 6} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // takeWhileLeft                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def takeWhileLeft01(): Bool = region rc {
        let a = Array.takeWhileLeft(rc, i -> i > 3, ((Array#{} @ rc): Array[Int32, _]));
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def takeWhileLeft02(): Bool = region rc {
        let a = Array.takeWhileLeft(rc, i -> i > 3, Array#{1} @ rc);
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def takeWhileLeft03(): Bool = region rc {
        let a = Array.takeWhileLeft(rc, i -> i > 3, Array#{4} @ rc);
        Array.sameElements(a, Array#{4} @ rc)
    }

    @test
    def takeWhileLeft04(): Bool = region rc {
        let a = Array.takeWhileLeft(rc, i -> i > 3, Array#{1,2} @ rc);
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def takeWhileLeft05(): Bool = region rc {
        let a = Array.takeWhileLeft(rc, i -> i > 3, Array#{1,5} @ rc);
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def takeWhileLeft06(): Bool = region rc {
        let a = Array.takeWhileLeft(rc, i -> i > 3, Array#{5,1} @ rc);
        Array.sameElements(a, Array#{5} @ rc)
    }

    @test
    def takeWhileLeft07(): Bool = region rc {
        let a = Array.takeWhileLeft(rc, i -> i > 3, Array#{5,8} @ rc);
        Array.sameElements(a, Array#{5,8} @ rc)
    }

    @test
    def takeWhileLeft08(): Bool = region rc {
        let a = Array.takeWhileLeft(rc, i -> i > 3, Array#{4, 6, -3, 11, -5, 1, 2, 16, 7, 1, 7} @ rc);
        Array.sameElements(a, Array#{4, 6} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // takeWhileRight                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def takeWhileRight01(): Bool = region rc {
        let a = Array.takeWhileRight(rc, i -> i > 3, ((Array#{} @ rc): Array[Int32, _]));
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def takeWhileRight02(): Bool = region rc {
        let a = Array.takeWhileRight(rc, i -> i > 3, Array#{1} @ rc);
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def takeWhileRight03(): Bool = region rc {
        let a = Array.takeWhileRight(rc, i -> i > 3, Array#{4} @ rc);
        Array.sameElements(a, Array#{4} @ rc)
    }

    @test
    def takeWhileRight04(): Bool = region rc {
        let a = Array.takeWhileRight(rc, i -> i > 3, Array#{1,2} @ rc);
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def takeWhileRight05(): Bool = region rc {
        let a = Array.takeWhileRight(rc, i -> i > 3, Array#{1,5} @ rc);
        Array.sameElements(a, Array#{5} @ rc)
    }

    @test
    def takeWhileRight06(): Bool = region rc {
        let a = Array.takeWhileRight(rc, i -> i > 3, Array#{5,1} @ rc);
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def takeWhileRight07(): Bool = region rc {
        let a = Array.takeWhileRight(rc, i -> i > 3, Array#{5,8} @ rc);
        Array.sameElements(a, Array#{5,8} @ rc)
    }

    @test
    def takeWhileRight08(): Bool = region rc {
        let a = Array.takeWhileRight(rc, i -> i > 3, Array#{4, 6, -3, 11, -5, 1, 2, 16, 7, 1, 7} @ rc);
        Array.sameElements(a, Array#{7} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // groupBy                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def groupBy01(): Bool = region rc {
        let arr = Array.groupBy(rc, (a, b) -> a > 3 or b > 8, ((Array#{} @ rc): Array[Int32, rc]));
        let xs = Array.toList(arr) |> List.map(Array.toList);
        xs == Nil
    }

    @test
    def groupBy02(): Bool = region rc {
        let arr = Array.groupBy(rc, (a, b) -> a > 3 or b > 8, Array#{1} @ rc);
        let xs = Array.toList(arr) |> List.map(Array.toList);
        xs == (1 :: Nil) :: Nil
    }

    @test
    def groupBy03(): Bool = region rc {
        let arr = Array.groupBy(rc, (a, b) -> a > 3 or b > 8, Array#{1,4} @ rc);
        let xs = Array.toList(arr) |> List.map(Array.toList);
        xs == (1 :: Nil) :: (4 :: Nil) :: Nil
    }

    @test
    def groupBy04(): Bool = region rc {
        let arr = Array.groupBy(rc, (a, b) -> a > 3 or b > 8, Array#{1,9} @ rc);
        let xs = Array.toList(arr) |> List.map(Array.toList);
        xs == (1 :: 9 :: Nil) :: Nil
    }

    @test
    def groupBy05(): Bool = region rc {
        let arr = Array.groupBy(rc, (a, b) -> a > 3 or b > 8, Array#{1, 4, 7, 6, 9, 2, 4, 4, 8, 16} @ rc);
        let xs = Array.toList(arr) |> List.map(Array.toList);
        xs == (1 :: 9 :: 16 :: Nil) :: (4 :: 7 :: 6 :: 4 :: 4 :: 8 :: Nil) :: (2 :: Nil) :: Nil
    }

    @test
    def groupBy06(): Bool = region rc {
        let arr = Array.groupBy(rc, (a, b) -> a > -6 or a*b >= 0, Array#{-1, -11, 4, -11, 0, 8, 2, 1, -3, -24} @ rc);
        let xs = Array.toList(arr) |> List.map(Array.toList);
        xs == (-1 :: -11 :: -11 :: 0 :: -3 :: -24 :: Nil) :: (4 :: 8 :: 2 :: 1 :: Nil) :: Nil
    }

    @test
    def groupBy07(): Bool = region rc {
        let arr = Array.groupBy(rc, (a, b) -> a < 0 or (a > 10 or (b > 10 or a == b)), Array#{-5, 6, 11, 8, 8, -11, -1, 0, 4, -1} @ rc);
        let xs = Array.toList(arr) |> List.map(Array.toList);
        xs == (-5 :: 11 :: -11 :: -1 :: -1 :: Nil) :: (6 :: Nil) :: (8 :: 8 :: Nil) :: (0 :: Nil) :: (4 :: Nil) :: Nil
    }

    /////////////////////////////////////////////////////////////////////////////
    // zip                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def zip01(): Bool = region rc {
        let a = Array.zip(rc, ((Array#{} @ rc): Array[Int32, _]), ((Array#{} @ rc): Array[Int32, _]));
        Array.sameElements(a, ((Array#{} @ rc): Array[(Int32, Int32), _]))
    }

    @test
    def zip02(): Bool = region rc {
        let a = Array.zip(rc, Array#{1} @ rc, ((Array#{} @ rc): Array[Int32, _]));
        Array.sameElements(a, ((Array#{} @ rc): Array[(Int32, Int32), _]))
    }

    @test
    def zip03(): Bool = region rc {
        let a = Array.zip(rc, ((Array#{} @ rc): Array[Int32, _]), Array#{2} @ rc);
        Array.sameElements(a, ((Array#{} @ rc): Array[(Int32, Int32), _]))
    }

    @test
    def zip04(): Bool = region rc {
        let a = Array.zip(rc, Array#{1} @ rc, Array#{2} @ rc);
        Array.sameElements(a, Array#{(1, 2)} @ rc)
    }

    @test
    def zip05(): Bool = region rc {
        let a = Array.zip(rc, Array#{1,3} @ rc, Array#{2,4} @ rc);
        Array.sameElements(a, Array#{(1, 2), (3, 4)} @ rc)
    }

    @test
    def zip06(): Bool = region rc {
        let a = Array.zip(rc, Array#{1,3,5} @ rc, Array#{2,4,6} @ rc);
        Array.sameElements(a, Array#{(1, 2), (3, 4), (5, 6)} @ rc)
    }

    @test
    def zip07(): Bool = region rc {
        let a = Array.zip(rc, Array#{1,3,5,7} @ rc, Array#{2,4,6,8} @ rc);
        Array.sameElements(a, Array#{(1, 2), (3, 4), (5, 6), (7, 8)} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // zipWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def zipWith01(): Bool = region rc {
        let arr = Array.zipWith(rc, (a, b) -> if (b) a + 1 else a, ((Array#{} @ rc): Array[Int32, _]), ((Array#{} @ rc): Array[Bool, _]));
        Array.sameElements(arr, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def zipWith02(): Bool = region rc {
        let arr = Array.zipWith(rc, (a, b) -> if (b) a + 1 else a, Array#{1} @ rc, ((Array#{} @ rc): Array[Bool, _]));
        Array.sameElements(arr, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def zipWith03(): Bool = region rc {
        let arr = Array.zipWith(rc, (a, b) -> if (b) a + 1 else a, ((Array#{} @ rc): Array[Int32, _]), Array#{true} @ rc);
        Array.sameElements(arr, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def zipWith04(): Bool = region rc {
        let arr = Array.zipWith(rc, (a, b) -> if (b) a + 1 else a, Array#{1} @ rc, Array#{true} @ rc);
        Array.sameElements(arr, Array#{2} @ rc)
    }

    @test
    def zipWith05(): Bool = region rc {
        let arr = Array.zipWith(rc, (a, b) -> if (b) a + 1 else a, Array#{1} @ rc, Array#{false} @ rc);
        Array.sameElements(arr, Array#{1} @ rc)
    }

    @test
    def zipWith06(): Bool = region rc {
        let arr = Array.zipWith(rc, (a, b) -> if (b) a + 1 else a,
                        Array#{1, 2, 3, 4, 5, 6, 7, 8} @ rc,
                        Array#{false, true, true, false, false, true, true, true} @ rc);
        Array.sameElements(arr, Array#{1, 3, 4, 4, 5, 7, 8, 9} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // unzip                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def unzip01(): Bool = region rc {
        let (a,b) = Array.unzip(rc, rc, ((Array#{} @ rc): Array[(Unit, Unit), _]));
        Array.sameElements(a, Array#{} @ rc) and Array.sameElements(b, Array#{} @ rc)
    }

    @test
    def unzip02(): Bool = region rc {
        let (a,b) = Array.unzip(rc, rc, Array#{(1, true)} @ rc);
        Array.sameElements(a, Array#{1} @ rc) and Array.sameElements(b, Array#{true} @ rc)
    }

    @test
    def unzip03(): Bool = region rc {
        let (a,b) = Array.unzip(rc, rc, Array#{(1, true), (2, true)} @ rc);
        Array.sameElements(a, Array#{1,2} @ rc)  and Array.sameElements(b, Array#{true,true} @ rc)
    }

    @test
    def unzip04(): Bool = region rc {
        let (a,b) = Array.unzip(rc, rc, Array#{(1, true), (2, true), (3, false)} @ rc);
        Array.sameElements(a, Array#{1,2,3} @ rc) and Array.sameElements(b, Array#{true,true,false} @ rc)
    }

    @test
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

    @test
    def fold201(): Bool = region rc {
        Array.fold2((c, a, b) -> if (b) a + c else a * c, 4, ((Array#{} @ rc): Array[Int32, _]), ((Array#{} @ rc): Array[Bool, _])) == 4
    }

    @test
    def fold202(): Bool = region rc {
        Array.fold2((c, a, b) -> if (b) a + c else a * c, 4, Array#{1} @ rc, Array#{} @ rc) == 4
    }

    @test
    def fold203(): Bool = region rc {
        Array.fold2((c, a, b) -> if (b) a + c else a * c, 4, Array#{} @ rc, Array#{true} @ rc) == 4
    }

    @test
    def fold204(): Bool = region rc {
        Array.fold2((c, a, b) -> if (b) a + c else a * c, 4, Array#{2} @ rc, Array#{true} @ rc) == 6
    }

    @test
    def fold205(): Bool = region rc {
        Array.fold2((c, a, b) -> if (b) a + c else a * c, 4, Array#{2} @ rc, Array#{false} @ rc) == 8
    }

    @test
    def fold206(): Bool = region rc {
        Array.fold2((c, a, b) -> if (b) a + c else a * c, 4, Array#{3,2} @ rc, Array#{true,true} @ rc) == 9
    }

    @test
    def fold207(): Bool = region rc {
        Array.fold2((c, a, b) -> if (b) a + c else a * c, 4, Array#{3,2} @ rc, Array#{true,false} @ rc) == 14
    }

    @test
    def fold208(): Bool = region rc {
        Array.fold2((c, a, b) -> if (b) a + c else a * c, 4, Array#{3,2} @ rc, Array#{false,true} @ rc) == 14
    }

    @test
    def fold209(): Bool = region rc {
        Array.fold2((c, a, b) -> if (b) a + c else a * c, 4, Array#{3,2} @ rc, Array#{false,false} @ rc) == 24
    }

    @test
    def fold210(): Bool = region rc {
        Array.fold2((c, a, b) -> if (b) a + c else a * c, 4, Array#{6,-4,3,2} @ rc,
            Array#{true,false,false,true} @ rc) == -118
    }

    /////////////////////////////////////////////////////////////////////////////
    // foldLeft2                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldLeft201(): Bool = region rc {
        Array.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, ((Array#{} @ rc): Array[Int32, _]), ((Array#{} @ rc): Array[Bool, _])) == 4
    }

    @test
    def foldLeft202(): Bool = region rc {
        Array.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, Array#{1} @ rc, ((Array#{} @ rc): Array[Bool, _])) == 4
    }

    @test
    def foldLeft203(): Bool = region rc {
        Array.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, ((Array#{} @ rc): Array[Int32, _]), Array#{true} @ rc) == 4
    }

    @test
    def foldLeft204(): Bool = region rc {
        Array.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, Array#{2} @ rc, Array#{true} @ rc) == 6
    }

    @test
    def foldLeft205(): Bool = region rc {
        Array.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, Array#{2} @ rc, Array#{false} @ rc) == 8
    }

    @test
    def foldLeft206(): Bool = region rc {
        Array.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, Array#{3,2} @ rc, Array#{true,true} @ rc) == 9
    }

    @test
    def foldLeft207(): Bool = region rc {
        Array.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, Array#{3,2} @ rc, Array#{true,false} @ rc) == 14
    }

    @test
    def foldLeft208(): Bool = region rc {
        Array.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, Array#{3,2} @ rc, Array#{false,true} @ rc) == 14
    }

    @test
    def foldLeft209(): Bool = region rc {
        Array.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, Array#{3,2} @ rc, Array#{false,false} @ rc) == 24
    }

    @test
    def foldLeft210(): Bool = region rc {
        Array.foldLeft2((c, a, b) -> if (b) a + c else a * c, 4, Array#{6,-4,3,2} @ rc,
            Array#{true,false,false,true} @ rc) == -118
    }

    /////////////////////////////////////////////////////////////////////////////
    // foldRight2                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldRight201(): Bool = region rc {
        Array.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, ((Array#{} @ rc): Array[Int32, _]), ((Array#{} @ rc): Array[Bool, _])) == 4
    }

    @test
    def foldRight202(): Bool = region rc {
        Array.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, Array#{1} @ rc, ((Array#{} @ rc): Array[Bool, _])) == 4
    }

    @test
    def foldRight203(): Bool = region rc {
        Array.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, ((Array#{} @ rc): Array[Int32, _]), Array#{true} @ rc) == 4
    }

    @test
    def foldRight204(): Bool = region rc {
        Array.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, Array#{2} @ rc, Array#{true} @ rc) == 6
    }

    @test
    def foldRight205(): Bool = region rc {
        Array.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, Array#{2} @ rc, Array#{false} @ rc) == 8
    }

    @test
    def foldRight206(): Bool = region rc {
        Array.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, Array#{3,2} @ rc, Array#{true,true} @ rc) == 9
    }

    @test
    def foldRight207(): Bool = region rc {
        Array.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, Array#{3,2} @ rc, Array#{true,false} @ rc) == 11
    }

    @test
    def foldRight208(): Bool = region rc {
        Array.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, Array#{3,2} @ rc, Array#{false,true} @ rc) == 18
    }

    @test
    def foldRight209(): Bool = region rc {
        Array.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, Array#{3,2} @ rc, Array#{false,false} @ rc) == 24
    }

    @test
    def foldRight210(): Bool = region rc {
        Array.foldRight2((a, b, c) -> if (b) a + c else a * c, 4, Array#{6,-4,3,2} @ rc,
            Array#{true,false,false,true} @ rc) == -66
    }

    /////////////////////////////////////////////////////////////////////////////
    // filterMap                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filterMap01(): Bool = region rc {
        let a = Array.filterMap(rc, i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, ((Array#{} @ rc): Array[Int32, _]));
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def filterMap02(): Bool = region rc {
        let a = Array.filterMap(rc, i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, Array#{1} @ rc);
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def filterMap03(): Bool = region rc {
        let a = Array.filterMap(rc, i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, Array#{2} @ rc);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @test
    def filterMap04(): Bool = region rc {
        let a = Array.filterMap(rc, i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, Array#{1,3} @ rc);
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def filterMap05(): Bool = region rc {
        let a = Array.filterMap(rc, i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, Array#{1,4} @ rc);
        Array.sameElements(a, Array#{2} @ rc)
    }

    @test
    def filterMap06(): Bool = region rc {
        let a = Array.filterMap(rc, i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, Array#{6,-1} @ rc);
        Array.sameElements(a, Array#{3} @ rc)
    }

    @test
    def filterMap07(): Bool = region rc {
        let a = Array.filterMap(rc, i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, Array#{8,6} @ rc);
        Array.sameElements(a, Array#{4,3} @ rc)
    }

    @test
    def filterMap08(): Bool = region rc {
        let a = Array.filterMap(rc, i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, Array#{0,1,2,3,4,5,10} @ rc);
        Array.sameElements(a, Array#{0,1,2,5} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // findMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def findMap01(): Bool = region rc {
        Array.findMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, ((Array#{} @ rc): Array[Int32, _])) == None
    }

    @test
    def findMap02(): Bool = region rc {
        Array.findMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, Array#{1} @ rc) == None
    }

    @test
    def findMap03(): Bool = region rc {
        Array.findMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, Array#{2} @ rc) == Some(1)
    }

    @test
    def findMap04(): Bool = region rc {
        Array.findMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, Array#{1,3} @ rc) == None
    }

    @test
    def findMap05(): Bool = region rc {
        Array.findMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, Array#{1,4} @ rc) == Some(2)
    }

    @test
    def findMap06(): Bool = region rc {
        Array.findMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, Array#{6,-1} @ rc) == Some(3)
    }

    @test
    def findMap07(): Bool = region rc {
        Array.findMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, Array#{8,6} @ rc) == Some(4)
    }

    @test
    def findMap08(): Bool = region rc {
        Array.findMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, Array#{0,1,2,3,4,5,10} @ rc) == Some(0)
    }

    /////////////////////////////////////////////////////////////////////////////
    // toSet                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toSet01(): Bool = region rc {
        Array.toSet(((Array#{} @ rc): Array[Int32, _])) == Set#{}
    }

    @test
    def toSet02(): Bool = region rc {
        Array.toSet(Array#{1} @ rc) == Set#{1}
    }

    @test
    def toSet03(): Bool = region rc {
        Array.toSet(Array#{1,2} @ rc) == Set#{1, 2}
    }

    @test
    def toSet04(): Bool = region rc {
        Array.toSet(Array#{1,1} @ rc) == Set#{1}
    }

    @test
    def toSet05(): Bool = region rc {
        Array.toSet(Array#{1,2,3} @ rc) == Set#{1, 2, 3}
    }

    @test
    def toSet06(): Bool = region rc {
        Array.toSet(Array#{1,2,1} @ rc) == Set#{1, 2}
    }

    @test
    def toSet07(): Bool = region rc {
        Array.toSet(Array#{1,1,2} @ rc) == Set#{1, 2}
    }

    @test
    def toSet08(): Bool = region rc {
        Array.toSet(Array#{2,1,2,3,4,5,6,3,3} @ rc) == Set#{1, 2, 3, 4, 5, 6}
    }

    /////////////////////////////////////////////////////////////////////////////
    // toMap                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toMap01(): Bool = region rc {
        Array.toMap(((Array#{} @ rc): Array[(Int32, Bool), _])) == Map#{}
    }

    @test
    def toMap02(): Bool = region rc {
        Array.toMap(Array#{(1, true)} @ rc) == Map#{1 => true}
    }

    @test
    def toMap03(): Bool = region rc {
        Array.toMap(Array#{(1, true), (2, false)} @ rc) == Map#{1 => true, 2 => false}
    }

    @test
    def toMap04(): Bool = region rc {
        Array.toMap(Array#{(1, true), (1, false)} @ rc) == Map#{1 => true}
    }

    /////////////////////////////////////////////////////////////////////////////
    // findIndexOf                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def findIndexOf01(): Bool = region rc {
        Array.findIndexOf(i -> i > 2, ((Array#{} @ rc): Array[Int32, _])) == None
    }

    @test
    def findIndexOf02(): Bool = region rc {
        Array.findIndexOf(i -> i > 2, Array#{1} @ rc) == None
    }

    @test
    def findIndexOf03(): Bool = region rc {
        Array.findIndexOf(i -> i > 2, Array#{3} @ rc) == Some(0)
    }

    @test
    def findIndexOf04(): Bool = region rc {
        Array.findIndexOf(i -> i > 2, Array#{1,2} @ rc) == None
    }

    @test
    def findIndexOf05(): Bool = region rc {
        Array.findIndexOf(i -> i > 2, Array#{6,-6} @ rc) == Some(0)
    }

    @test
    def findIndexOf06(): Bool = region rc {
        Array.findIndexOf(i -> i > 2, Array#{-6,6} @ rc) == Some(1)
    }

    @test
    def findIndexOf07(): Bool = region rc {
        Array.findIndexOf(i -> i > 2, Array#{6,7} @ rc) == Some(0)
    }

    /////////////////////////////////////////////////////////////////////////////
    // findIndexOfLeft                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def findIndexOfLeft01(): Bool = region rc {
        Array.findIndexOfLeft(i -> i > 2, ((Array#{} @ rc): Array[Int32, _])) == None
    }

    @test
    def findIndexOfLeft02(): Bool = region rc {
        Array.findIndexOfLeft(i -> i > 2, Array#{1} @ rc) == None
    }

    @test
    def findIndexOfLeft03(): Bool = region rc {
        Array.findIndexOfLeft(i -> i > 2, Array#{3} @ rc) == Some(0)
    }

    @test
    def findIndexOfLeft04(): Bool = region rc {
        Array.findIndexOfLeft(i -> i > 2, Array#{1,2} @ rc) == None
    }

    @test
    def findIndexOfLeft05(): Bool = region rc {
        Array.findIndexOfLeft(i -> i > 2, Array#{6,-6} @ rc) == Some(0)
    }

    @test
    def findIndexOfLeft06(): Bool = region rc {
        Array.findIndexOfLeft(i -> i > 2, Array#{-6,6} @ rc) == Some(1)
    }

    @test
    def findIndexOfLeft07(): Bool = region rc {
        Array.findIndexOfLeft(i -> i > 2, Array#{6,7} @ rc) == Some(0)
    }

    /////////////////////////////////////////////////////////////////////////////
    // findIndexOfRight                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def findIndexOfRight01(): Bool = region rc {
        Array.findIndexOfRight(i -> i > 2, ((Array#{} @ rc): Array[Int32, _])) == None
    }

    @test
    def findIndexOfRight02(): Bool = region rc {
        Array.findIndexOfRight(i -> i > 2, Array#{1} @ rc) == None
    }

    @test
    def findIndexOfRight03(): Bool = region rc {
        Array.findIndexOfRight(i -> i > 2, Array#{3} @ rc) == Some(0)
    }

    @test
    def findIndexOfRight04(): Bool = region rc {
        Array.findIndexOfRight(i -> i > 2, Array#{1,2} @ rc) == None
    }

    @test
    def findIndexOfRight05(): Bool = region rc {
        Array.findIndexOfRight(i -> i > 2, Array#{6,-6} @ rc) == Some(0)
    }

    @test
    def findIndexOfRight06(): Bool = region rc {
        Array.findIndexOfRight(i -> i > 2, Array#{-6,6} @ rc) == Some(1)
    }

    @test
    def findIndexOfRight07(): Bool = region rc {
        Array.findIndexOfRight(i -> i > 2, Array#{6,7} @ rc) == Some(1)
    }

    /////////////////////////////////////////////////////////////////////////////
    // findIndices                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def findIndices01(): Bool = region rc {
        let a = Array.findIndices(rc, i -> i > 2, ((Array#{} @ rc): Array[Int32, rc]));
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, rc]))
    }

    @test
    def findIndices02(): Bool = region rc {
        let a = Array.findIndices(rc, i -> i > 2, Array#{1} @ rc);
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, rc]))
    }

    @test
    def findIndices03(): Bool = region rc {
        let a = Array.findIndices(rc, i -> i > 2, Array#{3} @ rc);
        Array.sameElements(a, Array#{0} @ rc)
    }

    @test
    def findIndices04(): Bool = region rc {
        let a = Array.findIndices(rc, i -> i > 2, Array#{1,2} @ rc);
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, rc]))
    }

    @test
    def findIndices05(): Bool = region rc {
        let a = Array.findIndices(rc, i -> i > 2, Array#{6,-6} @ rc);
        Array.sameElements(a, Array#{0} @ rc)
    }

    @test
    def findIndices06(): Bool = region rc {
        let a = Array.findIndices(rc, i -> i > 2, Array#{-6,6} @ rc);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @test
    def findIndices07(): Bool = region rc {
        let a = Array.findIndices(rc, i -> i > 2, Array#{6,7} @ rc);
        Array.sameElements(a, Array#{0,1} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // init                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def init01(): Bool = region rc {
        let a = Array.init(rc, x -> x, 0);
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def init02(): Bool = region rc {
        let a = Array.init(rc, x -> x, -1);
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def init03(): Bool = region rc {
        let a = Array.init(rc, x -> x, 1);
        Array.sameElements(a, Array#{0} @ rc)
    }

    @test
    def init04(): Bool = region rc {
        let a = Array.init(rc, x -> x, 2);
        Array.sameElements(a, Array#{0,1} @ rc)
    }

    @test
    def init05(): Bool = region rc {
        let a = Array.init(rc, x -> x, 50);
        Array.length(a) == 50
    }

    /////////////////////////////////////////////////////////////////////////////
    // sameElements                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sameElements01(): Bool = region rc {
        let a = ((Array#{} @ rc): Array[Int32, _]);
        let b = ((Array#{} @ rc): Array[Int32, _]);
        Array.sameElements(a,b) == true
    }

    @test
    def sameElements02(): Bool = region rc {
        let a = (Array#{1} @ rc: Array[Int32, _]);
        let b = (Array#{} @ rc: Array[Int32, _]);
        Array.sameElements(a,b) == false
    }

    @test
    def sameElements03(): Bool = region rc {
        let a = ((Array#{} @ rc): Array[Int32, _]);
        let b = ((Array#{1} @ rc): Array[Int32, _]);
        Array.sameElements(a,b) == false
    }

    @test
    def sameElements04(): Bool = region rc {
        let a = ((Array#{1} @ rc): Array[Int32, _]);
        let b = ((Array#{1} @ rc): Array[Int32, _]);
        Array.sameElements(a,b) == true
    }

    @test
    def sameElements05(): Bool = region rc {
        let a = ((Array#{1} @ rc): Array[Int32, _]);
        let b = ((Array#{0} @ rc): Array[Int32, _]);
        Array.sameElements(a,b) == false
    }

    @test
    def sameElements06(): Bool = region rc {
        let a = ((Array#{1,2} @ rc): Array[Int32, _]);
        let b = ((Array#{1,2} @ rc): Array[Int32, _]);
        Array.sameElements(a,b) == true
    }

    @test
    def sameElements07(): Bool = region rc {
        let a = ((Array#{1,2} @ rc): Array[Int32, _]);
        let b = ((Array#{2,1} @ rc): Array[Int32, _]);
        Array.sameElements(a,b) == false
    }

    /////////////////////////////////////////////////////////////////////////////
    // forEach                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def forEach01(): Bool = region rc {
        let a = ((Array#{} @ rc): Array[Int32, _]);
        let sb = StringBuilder.empty(rc);
        let fn = x -> if (x > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        Array.forEach(fn, a);
        StringBuilder.toString(sb) == ""
    }

    @test
    def forEach02(): Bool = region rc {
        let a = Array#{0} @ rc;
        let sb = StringBuilder.empty(rc);
        let fn = x -> if (x > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        Array.forEach(fn, a);
        StringBuilder.toString(sb) == "F"
    }

    @test
    def forEach03(): Bool = region rc {
        let a = Array#{1} @ rc;
        let sb = StringBuilder.empty(rc);
        let fn = x -> if (x > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        Array.forEach(fn, a);
        StringBuilder.toString(sb) == "T"
    }

    @test
    def forEach04(): Bool = region rc {
        let a = Array#{0,1} @ rc;
        let sb = StringBuilder.empty(rc);
        let fn = x -> if (x > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        Array.forEach(fn, a);
        StringBuilder.toString(sb) == "FT"
    }

    /////////////////////////////////////////////////////////////////////////////
    // forEachWithIndex                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def forEachWithIndex01(): Bool = region rc {
        let a = ((Array#{} @ rc): Array[Float32, _]);
        let sb = StringBuilder.empty(rc);
        let fn = (ix, _) -> if (ix > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        Array.forEachWithIndex(fn, a);
        StringBuilder.toString(sb) == ""
    }

    @test
    def forEachWithIndex02(): Bool = region rc {
        let a = Array#{0.0} @ rc;
        let sb = StringBuilder.empty(rc);
        let fn = (ix, _) -> if (ix > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        Array.forEachWithIndex(fn, a);
        StringBuilder.toString(sb) == "F"
    }

    @test
    def forEachWithIndex03(): Bool = region rc {
        let a = Array#{0.0, 0.1} @ rc;
        let sb = StringBuilder.empty(rc);
        let fn = (ix, _) -> if (ix > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        Array.forEachWithIndex(fn, a);
        StringBuilder.toString(sb) == "FT"
    }

    @test
    def forEachWithIndex04(): Bool = region rc {
        let a = Array#{0.0, 0.1, 0.2} @ rc;
        let sb = StringBuilder.empty(rc);
        let fn = (ix, _) -> if (ix > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        Array.forEachWithIndex(fn, a);
        StringBuilder.toString(sb) == "FTT"
    }

    @test
    def forEachWithIndex05(): Bool = region rc {
        let a = Array#{0.0, 0.1, 0.2, 0.3} @ rc;
        let sb = StringBuilder.empty(rc);
        let fn = (ix, _) -> if (ix > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        Array.forEachWithIndex(fn, a);
        StringBuilder.toString(sb) == "FTTT"
    }

    /////////////////////////////////////////////////////////////////////////////
    // updateSequence                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def updateSequence01(): Bool = region rc {
         let a = Array.updateSequence(rc, 0, ((Array#{} @ rc): Array[Int32, _]), ((Array#{} @ rc): Array[Int32, _]));
         Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
     }

    @test
    def updateSequence02(): Bool = region rc {
        let a = Array.updateSequence(rc, 0, Array#{1,2} @ rc, Array#{} @ rc);
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def updateSequence03(): Bool = region rc {
        let a = Array.updateSequence(rc, 0, Array#{} @ rc, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @test
    def updateSequence04(): Bool = region rc {
        let a = Array.updateSequence(rc, -3, Array#{1,2,4} @ rc, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @test
    def updateSequence05(): Bool = region rc {
        let a = Array.updateSequence(rc, 2, Array#{1,2,4} @ rc, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @test
    def updateSequence06(): Bool = region rc {
        let a = Array.updateSequence(rc, 0, Array#{} @ rc, Array#{1} @ rc);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @test
    def updateSequence07(): Bool = region rc {
        let a = Array.updateSequence(rc, 1, Array#{2} @ rc, Array#{1} @ rc);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @test
    def updateSequence08(): Bool = region rc {
        let a = Array.updateSequence(rc, 0, Array#{2} @ rc, Array#{1} @ rc);
        Array.sameElements(a, Array#{2} @ rc)
    }

    @test
    def updateSequence09(): Bool = region rc {
        let a = Array.updateSequence(rc, 0, Array#{2,4} @ rc, Array#{1} @ rc);
        Array.sameElements(a, Array#{2} @ rc)
    }

    @test
    def updateSequence10(): Bool = region rc {
        let a = Array.updateSequence(rc, -1, Array#{2,4} @ rc, Array#{1} @ rc);
        Array.sameElements(a, Array#{4} @ rc)
    }

    @test
    def updateSequence11(): Bool = region rc {
        let a = Array.updateSequence(rc, -1, Array#{3,4} @ rc, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{4,2} @ rc)
    }

    @test
    def updateSequence12(): Bool = region rc {
        let a = Array.updateSequence(rc, 1, Array#{3,4} @ rc, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{1,3} @ rc)
    }

    @test
    def updateSequence13(): Bool = region rc {
        let a = Array.updateSequence(rc, -2, Array#{3,4} @ rc, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @test
    def updateSequence14(): Bool = region rc {
        let a = Array.updateSequence(rc, 2, Array#{3,4} @ rc, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @test
    def updateSequence15(): Bool = region rc {
        let a = Array.updateSequence(rc, 1, Array#{3} @ rc, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{1,3} @ rc)
    }

    @test
    def updateSequence16(): Bool = region rc {
        let a = Array.updateSequence(rc, 0, Array#{3,4} @ rc, Array#{1,2} @ rc);
        Array.sameElements(a, Array#{3,4} @ rc)
    }

    @test
    def updateSequence17(): Bool = region rc {
        let a = Array.updateSequence(rc, 0, Array#{4} @ rc, Array#{1, 2, 3} @ rc);
        Array.sameElements(a, Array#{4, 2, 3} @ rc)
    }

    @test
    def updateSequence18(): Bool = region rc {
        let a = Array.updateSequence(rc, 1, Array#{4} @ rc, Array#{1, 2, 3} @ rc);
        Array.sameElements(a, Array#{1, 4, 3} @ rc)
    }

    @test
    def updateSequence19(): Bool = region rc {
        let a = Array.updateSequence(rc, 2, Array#{4} @ rc, Array#{1, 2, 3} @ rc);
        Array.sameElements(a, Array#{1, 2, 4} @ rc)
    }

    @test
    def updateSequence20(): Bool = region rc {
        let a = Array.updateSequence(rc, 0, Array#{4, 5} @ rc, Array#{1, 2, 3} @ rc);
        Array.sameElements(a, Array#{4, 5, 3} @ rc)
    }

    @test
    def updateSequence21(): Bool = region rc {
        let a = Array.updateSequence(rc, 1, Array#{4, 5} @ rc, Array#{1, 2, 3} @ rc);
        Array.sameElements(a, Array#{1, 4, 5} @ rc)
    }

    @test
    def updateSequence22(): Bool = region rc {
        let a = Array.updateSequence(rc, -1, Array#{4, 5, 6} @ rc, Array#{1, 2, 3} @ rc);
        Array.sameElements(a, Array#{5, 6, 3} @ rc)
    }

    @test
    def updateSequence23(): Bool = region rc {
        let a = Array.updateSequence(rc, 0, Array#{4, 5, 6} @ rc, Array#{1, 2, 3} @ rc);
        Array.sameElements(a, Array#{4, 5, 6} @ rc)
    }

    @test
    def updateSequence24(): Bool = region rc {
        let a = Array.updateSequence(rc, 2, Array#{14, 15, 16, 17} @ rc, Array#{1, 2, 3, 4, 5, 6, 7} @ rc);
        Array.sameElements(a, Array#{1, 2, 14, 15, 16, 17, 7} @ rc)
    }

    @test
    def updateSequence25(): Bool = region rc {
        let a = Array.updateSequence(rc, -2, Array#{14, 15, 16, 17} @ rc, Array#{1, 2, 3, 4, 5, 6, 7} @ rc);
        Array.sameElements(a, Array#{16, 17, 3, 4, 5, 6, 7} @ rc)
    }

    @test
    def updateSequence26(): Bool = region rc {
        let a = Array.updateSequence(rc, 4, Array#{14, 15, 16, 17} @ rc, Array#{1, 2, 3, 4, 5, 6, 7} @ rc);
        Array.sameElements(a, Array#{1, 2, 3, 4, 14, 15, 16} @ rc)
    }

    @test
    def updateSequence27(): Bool = region rc {
        let a = Array.updateSequence(rc, 4, Array#{14, 15} @ rc, Array#{1, 2, 3, 4, 5, 6, 7} @ rc);
        Array.sameElements(a, Array#{1, 2, 3, 4, 14, 15, 7} @ rc)
    }

    @test
    def updateSequence28(): Bool = region rc {
        let a = Array.updateSequence(rc, -1, Array#{-1, -2, -3, -4, -5, -6, -7, -8} @ rc, Array#{1, 2, 3, 4, 5, 6, 7} @ rc);
        Array.sameElements(a, Array#{-2, -3, -4, -5, -6, -7, -8} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // updateSequence!                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def updateSequence01!(): Bool = region rc {
         let a = ((Array#{} @ rc): Array[Int32, _]);
         Array.updateSequence!(0, ((Array#{} @ rc): Array[Int32, _]), a);
         Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
     }

    @test
    def updateSequence02!(): Bool = region rc {
        let a = ((Array#{} @ rc): Array[Int32, _]);
        Array.updateSequence!(0, Array#{1,2} @ rc, a);
        Array.sameElements(a, ((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def updateSequence03!(): Bool = region rc {
        let a = Array#{1,2} @ rc;
        Array.updateSequence!(0, Array#{} @ rc, a);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @test
    def updateSequence04!(): Bool = region rc {
        let a = Array#{1,2} @ rc;
        Array.updateSequence!(-3, Array#{1,2,4} @ rc, a);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @test
    def updateSequence05!(): Bool = region rc {
        let a = Array#{1,2} @ rc;
        Array.updateSequence!(2, Array#{1,2,4} @ rc, a);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @test
    def updateSequence06!(): Bool = region rc {
        let a = Array#{1} @ rc;
        Array.updateSequence!(0, Array#{} @ rc, a);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @test
    def updateSequence07!(): Bool = region rc {
        let a = Array#{1} @ rc;
        Array.updateSequence!(1, Array#{2} @ rc, a);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @test
    def updateSequence08!(): Bool = region rc {
        let a = Array#{1} @ rc;
        Array.updateSequence!(0, Array#{2} @ rc, a);
        Array.sameElements(a, Array#{2} @ rc)
    }

    @test
    def updateSequence09!(): Bool = region rc {
        let a = Array#{1} @ rc;
        Array.updateSequence!(0, Array#{2,4} @ rc, a);
        Array.sameElements(a, Array#{2} @ rc)
    }

    @test
    def updateSequence10!(): Bool = region rc {
        let a = Array#{1} @ rc;
        Array.updateSequence!(-1, Array#{2,4} @ rc, a);
        Array.sameElements(a, Array#{4} @ rc)
    }

    @test
    def updateSequence11!(): Bool = region rc {
        let a = Array#{1,2} @ rc;
        Array.updateSequence!(-1, Array#{3,4} @ rc, a);
        Array.sameElements(a, Array#{4,2} @ rc)
    }

    @test
    def updateSequence12!(): Bool = region rc {
        let a = Array#{1,2} @ rc;
        Array.updateSequence!(1, Array#{3,4} @ rc, a);
        Array.sameElements(a, Array#{1,3} @ rc)
    }

    @test
    def updateSequence13!(): Bool = region rc {
        let a = Array#{1,2} @ rc;
        Array.updateSequence!(-2, Array#{3,4} @ rc, a);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @test
    def updateSequence14!(): Bool = region rc {
        let a = Array#{1,2} @ rc;
        Array.updateSequence!(2, Array#{3,4} @ rc, a);
        Array.sameElements(a, Array#{1,2} @ rc)
    }

    @test
    def updateSequence15!(): Bool = region rc {
        let a = Array#{1,2} @ rc;
        Array.updateSequence!(1, Array#{3} @ rc, a);
        Array.sameElements(a, Array#{1,3} @ rc)
    }

    @test
    def updateSequence16!(): Bool = region rc {
        let a = Array#{1,2} @ rc;
        Array.updateSequence!(0, Array#{3,4} @ rc, a);
        Array.sameElements(a, Array#{3,4} @ rc)
    }

    @test
    def updateSequence17!(): Bool = region rc {
        let a = Array#{1, 2, 3} @ rc;
        Array.updateSequence!(0, Array#{4} @ rc, a);
        Array.sameElements(a, Array#{4, 2, 3} @ rc)
    }

    @test
    def updateSequence18!(): Bool = region rc {
        let a = Array#{1, 2, 3} @ rc;
        Array.updateSequence!(1, Array#{4} @ rc, a);
        Array.sameElements(a, Array#{1, 4, 3} @ rc)
    }

    @test
    def updateSequence19!(): Bool = region rc {
        let a = Array#{1, 2, 3} @ rc;
        Array.updateSequence!(2, Array#{4} @ rc, a);
        Array.sameElements(a, Array#{1, 2, 4} @ rc)
    }

    @test
    def updateSequence20!(): Bool = region rc {
        let a = Array#{1, 2, 3} @ rc;
        Array.updateSequence!(0, Array#{4, 5} @ rc, a);
        Array.sameElements(a, Array#{4, 5, 3} @ rc)
    }

    @test
    def updateSequence21!(): Bool = region rc {
        let a = Array#{1, 2, 3} @ rc;
        Array.updateSequence!(1, Array#{4, 5} @ rc, a);
        Array.sameElements(a, Array#{1, 4, 5} @ rc)
    }

    @test
    def updateSequence22!(): Bool = region rc {
        let a = Array#{1, 2, 3} @ rc;
        Array.updateSequence!(-1, Array#{4, 5, 6} @ rc, a);
        Array.sameElements(a, Array#{5, 6, 3} @ rc)
    }

    @test
    def updateSequence23!(): Bool = region rc {
        let a = Array#{1, 2, 3} @ rc;
        Array.updateSequence!(0, Array#{4, 5, 6} @ rc, a);
        Array.sameElements(a, Array#{4, 5, 6} @ rc)
    }

    @test
    def updateSequence24!(): Bool = region rc {
        let a = Array#{1, 2, 3, 4, 5, 6, 7} @ rc;
        Array.updateSequence!(2, Array#{14, 15, 16, 17} @ rc, a);
        Array.sameElements(a, Array#{1, 2, 14, 15, 16, 17, 7} @ rc)
    }

    @test
    def updateSequence25!(): Bool = region rc {
        let a = Array#{1, 2, 3, 4, 5, 6, 7} @ rc;
        Array.updateSequence!(-2, Array#{14, 15, 16, 17} @ rc, a);
        Array.sameElements(a, Array#{16, 17, 3, 4, 5, 6, 7} @ rc)
    }

    @test
    def updateSequence26!(): Bool = region rc {
        let a = Array#{1, 2, 3, 4, 5, 6, 7} @ rc;
        Array.updateSequence!(4, Array#{14, 15, 16, 17} @ rc, a);
        Array.sameElements(a, Array#{1, 2, 3, 4, 14, 15, 16} @ rc)
    }

    @test
    def updateSequence27!(): Bool = region rc {
        let a = Array#{1, 2, 3, 4, 5, 6, 7} @ rc;
        Array.updateSequence!(4, Array#{14, 15} @ rc, a);
        Array.sameElements(a, Array#{1, 2, 3, 4, 14, 15, 7} @ rc)
    }

    @test
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

    @test
    def sortWith01(): Bool = region rc {
        let a = Array.sortWith(rc, cmp, ((Array#{} @ rc): Array[Int32, _]));
        a `sameElements` ((Array#{} @ rc): Array[Int32, _])
    }

    @test
    def sortWith02(): Bool = region rc {
        let a = Array.sortWith(rc, cmp, Array#{0} @ rc);
        a `sameElements` Array#{0} @ rc
    }

    @test
    def sortWith03(): Bool = region rc {
        let a = Array.sortWith(rc, cmp, Array#{0,1} @ rc);
        a `sameElements` Array#{0,1} @ rc
    }

    @test
    def sortWith04(): Bool = region rc {
        let a = Array.sortWith(rc, cmp, Array#{1,0} @ rc);
        a `sameElements` Array#{0,1} @ rc
    }

    @test
    def sortWith05(): Bool = region rc {
        let a = Array.sortWith(rc, cmp, Array#{1,1} @ rc);
        a `sameElements` Array#{1,1} @ rc
    }

    @test
    def sortWith06(): Bool = region rc {
        let a = Array.sortWith(rc, cmp, Array#{0,1,2,3,4,5} @ rc);
        a `sameElements` Array#{0,1,2,3,4,5} @ rc
    }

    @test
    def sortWith07(): Bool = region rc {
        let a = Array.sortWith(rc, cmp, Array#{5,4,3,2,1,0} @ rc);
        a `sameElements` Array#{0,1,2,3,4,5} @ rc
    }

    @test
    def sortWith08(): Bool = region rc {
        let a = Array.sortWith(rc, cmp, Array#{5,3,0,4,1,2} @ rc);
        a `sameElements` Array#{0,1,2,3,4,5} @ rc
    }

    @test
    def sortWith09(): Bool = region rc {
        let a = Array.sortWith(rc, cmp, Array#{2,3,0,4,1,2} @ rc);
        a `sameElements` Array#{0,1,2,2,3,4} @ rc
    }

    @test
    def sortWith10(): Bool = region rc {
        let a = Array.sortWith(rc, flip(cmp), Array#{0,1,2,3,4,5} @ rc);
        a `sameElements` Array#{5,4,3,2,1,0} @ rc
    }

    @test
    def sortWith11(): Bool = region rc {
        let a = Array.sortWith(rc, flip(cmp), Array#{5,4,3,2,1,0} @ rc);
        a `sameElements` Array#{5,4,3,2,1,0} @ rc
    }

    @test
    def sortWith12(): Bool = region rc {
        let a = Array.sortWith(rc, flip(cmp), Array#{5,3,0,4,1,2} @ rc);
        a `sameElements` Array#{5,4,3,2,1,0} @ rc
    }

    @test
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

    @test
    def sort01(): Bool = region rc {
        testSortVsSortWith(((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def sort02(): Bool = region rc {
        testSortVsSortWith(Array#{0} @ rc)
    }

    @test
    def sort03(): Bool = region rc {
        testSortVsSortWith(Array#{0,1} @ rc)
    }

    @test
    def sort04(): Bool = region rc {
        testSortVsSortWith(Array#{1,0} @ rc)
    }

    @test
    def sort05(): Bool = region rc {
        testSortVsSortWith(Array#{1,1} @ rc)
    }

    @test
    def sort06(): Bool = region rc {
        testSortVsSortWith(Array#{0,1,2,3,4,5} @ rc)
    }

    @test
    def sort07(): Bool = region rc {
        testSortVsSortWith(Array#{5,4,3,2,1,0} @ rc)
    }

    @test
    def sort08(): Bool = region rc {
        testSortVsSortWith(Array#{5,3,0,4,1,2} @ rc)
    }

    @test
    def sort09(): Bool = region rc {
        testSortVsSortWith(Array#{2,3,0,4,1,2} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // sortWith!                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sortWith01!(): Bool = region rc {
        let a = ((Array#{} @ rc): Array[Int32, _]);
        Array.sortWith!(cmp, a);
        a `sameElements` ((Array#{} @ rc): Array[Int32, _])
    }

    @test
    def sortWith02!(): Bool = region rc {
        let a = Array#{0} @ rc;
        Array.sortWith!(cmp, a);
        a `sameElements` Array#{0} @ rc
    }

    @test
    def sortWith03!(): Bool = region rc {
        let a = Array#{0,1} @ rc;
        Array.sortWith!(cmp, a);
        a `sameElements` Array#{0,1} @ rc
    }

    @test
    def sortWith04!(): Bool = region rc {
        let a = Array#{1,0} @ rc;
        Array.sortWith!(cmp, a);
        a `sameElements` Array#{0,1} @ rc
    }

    @test
    def sortWith05!(): Bool = region rc {
        let a = Array#{1,1} @ rc;
        Array.sortWith!(cmp, a);
        a `sameElements` Array#{1,1} @ rc
    }

    @test
    def sortWith06!(): Bool = region rc {
        let a = Array#{0,1,2,3,4,5} @ rc;
        Array.sortWith!(cmp, a);
        a `sameElements` Array#{0,1,2,3,4,5} @ rc
    }

    @test
    def sortWith07!(): Bool = region rc {
        let a = Array#{5,4,3,2,1,0} @ rc;
        Array.sortWith!(cmp, a);
        a `sameElements` Array#{0,1,2,3,4,5} @ rc
    }

    @test
    def sortWith08!(): Bool = region rc {
        let a = Array#{5,3,0,4,1,2} @ rc;
        Array.sortWith!(cmp, a);
        a `sameElements` Array#{0,1,2,3,4,5} @ rc
    }

    @test
    def sortWith09!(): Bool = region rc {
        let a = Array#{2,3,0,4,1,2} @ rc;
        Array.sortWith!(cmp, a);
        a `sameElements` Array#{0,1,2,2,3,4} @ rc
    }

    @test
    def sortWith10!(): Bool = region rc {
        let a = Array#{0,1,2,3,4,5} @ rc;
        Array.sortWith!(flip(cmp), a);
        a `sameElements` Array#{5,4,3,2,1,0} @ rc
    }

    @test
    def sortWith11!(): Bool = region rc {
        let a = Array#{5,4,3,2,1,0} @ rc;
        Array.sortWith!(flip(cmp), a);
        a `sameElements` Array#{5,4,3,2,1,0} @ rc
    }

    @test
    def sortWith12!(): Bool = region rc {
        let a = Array#{5,3,0,4,1,2} @ rc;
        Array.sortWith!(flip(cmp), a);
        a `sameElements` Array#{5,4,3,2,1,0} @ rc
    }

    @test
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

    @test
    def sort!01(): Bool = region rc {
        testSort!VsSortWith!(((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def sort!02(): Bool = region rc {
        testSort!VsSortWith!(Array#{0} @ rc)
    }

    @test
    def sort!03(): Bool = region rc {
        testSort!VsSortWith!(Array#{0,1} @ rc)
    }

    @test
    def sort!04(): Bool = region rc {
        testSort!VsSortWith!(Array#{1,0} @ rc)
    }

    @test
    def sort!05(): Bool = region rc {
        testSort!VsSortWith!(Array#{1,1} @ rc)
    }

    @test
    def sort!06(): Bool = region rc {
        testSort!VsSortWith!(Array#{0,1,2,3,4,5} @ rc)
    }

    @test
    def sort!07(): Bool = region rc {
        testSort!VsSortWith!(Array#{5,4,3,2,1,0} @ rc)
    }

    @test
    def sort!08(): Bool = region rc {
        testSort!VsSortWith!(Array#{5,3,0,4,1,2} @ rc)
    }

    @test
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

    @test
    def sortBy01(): Bool = region rc {
        testSortByVsSort(((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def sortBy02(): Bool = region rc {
        testSortByVsSort(Array#{0} @ rc)
    }

    @test
    def sortBy03(): Bool = region rc {
        testSortByVsSort(Array#{0,1} @ rc)
    }

    @test
    def sortBy04(): Bool = region rc {
        testSortByVsSort(Array#{1,0} @ rc)
    }

    @test
    def sortBy05(): Bool = region rc {
        testSortByVsSort(Array#{1,1} @ rc)
    }

    @test
    def sortBy06(): Bool = region rc {
        testSortByVsSort(Array#{0,1,2,3,4,5} @ rc)
    }

    @test
    def sortBy07(): Bool = region rc {
        testSortByVsSort(Array#{5,4,3,2,1,0} @ rc)
    }

    @test
    def sortBy08(): Bool = region rc {
        testSortByVsSort(Array#{5,3,0,4,1,2} @ rc)
    }

    @test
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
            x#i == y#i and x#s == y#s
    }

    @test
    def sortBy10(): Bool = region rc {
        Array.sortBy(rc, r -> let R.R(x) = r; x#i, Array#{R.R({i = 2, s = "A"}), R.R({i = 1, s = "B"}), R.R({i = 3, s = "C"})} @ rc)
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

    @test
    def sortBy!01(): Bool = region rc {
        testSortBy!VsSortBy(((Array#{} @ rc): Array[Int32, _]))
    }

    @test
    def sortBy!02(): Bool = region rc {
        testSortBy!VsSortBy(Array#{0} @ rc)
    }

    @test
    def sortBy!03(): Bool = region rc {
        testSortBy!VsSortBy(Array#{0,1} @ rc)
    }

    @test
    def sortBy!04(): Bool = region rc {
        testSortBy!VsSortBy(Array#{1,0} @ rc)
    }

    @test
    def sortBy!05(): Bool = region rc {
        testSortBy!VsSortBy(Array#{1,1} @ rc)
    }

    @test
    def sortBy!06(): Bool = region rc {
        testSortBy!VsSortBy(Array#{0,1,2,3,4,5} @ rc)
    }

    @test
    def sortBy!07(): Bool = region rc {
        testSortBy!VsSortBy(Array#{5,4,3,2,1,0} @ rc)
    }

    @test
    def sortBy!08(): Bool = region rc {
        testSortBy!VsSortBy(Array#{5,3,0,4,1,2} @ rc)
    }

    @test
    def sortBy!09(): Bool = region rc {
        testSortBy!VsSortBy(Array#{2,3,0,4,1,2} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // minimumBy                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def minimumBy01(): Bool = region rc {
        Array.minimumBy((x, y) -> x <=> y, ((Array#{} @ rc): Array[Int32, _])) == None
    }

    @test
    def minimumBy02(): Bool = region rc {
        Array.minimumBy((x, y) -> x <=> y, Array.range(rc, 0, 5)) == Some(0)
    }

    @test
    def minimumBy03(): Bool = region rc {
        Array.minimumBy((x, y) -> x <=> y, Array.range(rc, 4, 10)) == Some(4)
    }

    @test
    def minimumBy04(): Bool = region rc {
        let a = Array.range(rc, 10, 20);
        Array.put(-22, 5, a);
        Array.minimumBy((x, y) -> x <=> y, a) == Some(-22)
    }

    /////////////////////////////////////////////////////////////////////////////
    // maximumBy                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def maximumBy01(): Bool = region rc {
        Array.maximumBy((x, y) -> x <=> y, ((Array#{} @ rc): Array[Int32, _])) == None
    }

    @test
    def maximumBy02(): Bool = region rc {
        Array.maximumBy((x, y) -> x <=> y, Array.range(rc, 0, 5)) == Some(4)
    }

    @test
    def maximumBy03(): Bool = region rc {
        Array.maximumBy((x, y) -> x <=> y, Array.range(rc, 4, 10)) == Some(9)
    }

    @test
    def maximumBy04(): Bool = region rc {
        let a = Array.range(rc, 10, 20);
        Array.put(42, 5, a);
        Array.maximumBy((x, y) -> x <=> y, a) == Some(42)
    }


    /////////////////////////////////////////////////////////////////////////////
    // sum                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sum01(): Bool = region rc {
        Array.sum(Array#{} @ rc) == 0
    }

    @test
    def sum02(): Bool = region rc {
        Array.sum(Array#{1} @ rc) == 1
    }

    @test
    def sum03(): Bool = region rc {
        Array.sum(Array#{1, 2, 3} @ rc) == 6
    }

    @test
    def sum04(): Bool = region rc {
        Array.sum(Array#{1, 2, 3, -3} @ rc) == 3
    }

    @test
    def sum05(): Bool = region rc {
        Array.sum(Array#{-1, -2, -3, -4} @ rc) == -10
    }
    @test
    def sum06(): Bool = region rc {
        Array.sum(Array#{10, -10} @ rc) == 0
    }

    @test
    def sum07(): Bool = region rc {
        Array.range(rc, 1, 101) |> Array.sum == 5050
    }


    /////////////////////////////////////////////////////////////////////////////
    // sumWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sumWith01(): Bool = region rc {
        Array.sumWith(x -> x + 1, Array#{} @ rc) == 0
    }

    @test
    def sumWith02(): Bool = region rc {
        Array.sumWith(x -> x + 1, Array#{1} @ rc) == 2
    }

    @test
    def sumWith03(): Bool = region rc {
        Array.sumWith(x -> x + 1, Array#{1, 2, 3} @ rc) == 9
    }

    @test
    def sumWith04(): Bool = region rc {
        Array.sumWith(x -> x + 1, Array#{1, 2, 3, -3} @ rc) == 7
    }

    @test
    def sumWith05(): Bool = region rc {
        Array.sumWith(x -> x + 1, Array#{-1, -2, -3, -4} @ rc) == -6
    }

    @test
    def sumWith06(): Bool = region rc {
        Array.sumWith(x -> x + 1, Array#{10, -10} @ rc) == 2
    }


    /////////////////////////////////////////////////////////////////////////////
    // toDelayList                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toDelayList01(): Bool = region rc {
        ((Array#{} @ rc: Array[Unit, _])) |> Array.toDelayList == DelayList.empty()
    }

    @test
    def toDelayList02(): Bool = region rc {
        (Array.empty(rc, 0): Array[String, rc]) |> Array.toDelayList == DelayList.empty()
    }

    @test
    def toDelayList03(): Bool = region rc {
        Array.range(rc, -1000, 1000) |> Array.toDelayList == DelayList.range(-1000, 1000)
    }

    @test
    def toDelayList04(): Bool = region rc {
        Array.range(rc, -1000, 1000) |> Array.toDelayList |> DelayList.toArray(rc) `sameElements` Array.range(rc, -1000, 1000)
    }

    /////////////////////////////////////////////////////////////////////////////
    // toChain                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toChain01(): Bool = region rc {
        Array.toChain(((Array#{} @ rc: Array[Unit, _]))) == (Chain.empty(): Chain[Unit])
    }

    @test
    def toChain02(): Bool = region rc {
        Array.toChain(Array#{1} @ rc) == Chain.singleton(1)
    }

    @test
    def toChain03(): Bool = region rc {
        Array.toChain(Array#{1,2} @ rc) == List.toChain(1 :: 2 :: Nil)
    }

    @test
    def toChain04(): Bool = region rc {
        Array.toChain(Array#{1,2,3} @ rc) == List.toChain(1 :: 2 :: 3 :: Nil)
    }

    /////////////////////////////////////////////////////////////////////////////
    // toNec                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toNec01(): Bool = region rc {
        Array.toNec(((Array#{} @ rc: Array[Unit, _]))) == None
    }

    @test
    def toNec02(): Bool = region rc {
        Array.toNec(Array#{1} @ rc) == Some(Nec.singleton(1))
    }

    @test
    def toNec03(): Bool = region rc {
        Array.toNec(Array#{1,2} @ rc) == List.toNec(1 :: 2 :: Nil)
    }

    @test
    def toNec04(): Bool = region rc {
        Array.toNec(Array#{1,2,3} @ rc) == List.toNec(1 :: 2 :: 3 :: Nil)
    }

    /////////////////////////////////////////////////////////////////////////////
    // copyOfRange                                                             //
    /////////////////////////////////////////////////////////////////////////////

    // depends on flatten

    @test
    def copyOfRange01(): Bool = region rc {
        Array.copyOfRange(rc, 0, 3, Array#{true, false, true} @ rc) `sameElements` Array#{true, false, true} @ rc
    }

    @test
    def copyOfRange02(): Bool = region rc {
        Array.copyOfRange(rc, 1, 3, Array#{true, false, true} @ rc) `sameElements` Array#{false, true} @ rc
    }

    @test
    def copyOfRange03(): Bool = region rc {
        let a = Array.copyOfRange(rc, 2, 5, Array#{true, false, true} @ rc);
        Array.length(a) == 3 and Array.memberOf(true, a)
    }

    @test
    def copyOfRange04(): Bool = region rc {
        Array.copyOfRange(rc, 0, 3, Array#{'a', 'b', 'c'} @ rc) `sameElements` Array#{'a', 'b', 'c'} @ rc
    }

    @test
    def copyOfRange05(): Bool = region rc {
        Array.copyOfRange(rc, 1, 3, Array#{'a', 'b', 'c'} @ rc) `sameElements` Array#{'b', 'c'} @ rc
    }

    @test
    def copyOfRange06(): Bool = region rc {
        let a = Array.copyOfRange(rc, 2, 5, Array#{'a', 'b', 'c'} @ rc);
        Array.length(a) == 3 and Array.memberOf('c', a)
    }

    @test
    def copyOfRange07(): Bool = region rc {
        Array.copyOfRange(rc, 0, 3, Array#{1.0f32, 2.0f32, 3.0f32} @ rc) `sameElements` Array#{1.0f32, 2.0f32, 3.0f32} @ rc
    }

    @test
    def copyOfRange08(): Bool = region rc {
        Array.copyOfRange(rc, 1, 3, Array#{1.0f32, 2.0f32, 3.0f32} @ rc) `sameElements` Array#{2.0f32, 3.0f32} @ rc
    }

    @test
    def copyOfRange09(): Bool = region rc {
        let a = Array.copyOfRange(rc, 2, 5, Array#{1.0f32, 2.0f32, 3.0f32} @ rc);
        Array.length(a) == 3 and Array.memberOf(3.0f32, a)
    }

    @test
    def copyOfRange10(): Bool = region rc {
        Array.copyOfRange(rc, 0, 3, Array#{1.0f64, 2.0f64, 3.0f64} @ rc) `sameElements` Array#{1.0f64, 2.0f64, 3.0f64} @ rc
    }

    @test
    def copyOfRange11(): Bool = region rc {
        Array.copyOfRange(rc, 1, 3, Array#{1.0f64, 2.0f64, 3.0f64} @ rc) `sameElements` Array#{2.0f64, 3.0f64} @ rc
    }

    @test
    def copyOfRange12(): Bool = region rc {
        let a = Array.copyOfRange(rc, 2, 5, Array#{1.0f64, 2.0f64, 3.0f64} @ rc);
        Array.length(a) == 3 and Array.memberOf(3.0f64, a)
    }

    @test
    def copyOfRange13(): Bool = region rc {
        Array.copyOfRange(rc, 0, 3, Array#{1i8, 2i8, 3i8} @ rc) `sameElements` Array#{1i8, 2i8, 3i8} @ rc
    }

    @test
    def copyOfRange14(): Bool = region rc {
        Array.copyOfRange(rc, 1, 3, Array#{1i8, 2i8, 3i8} @ rc) `sameElements` Array#{2i8, 3i8} @ rc
    }

    @test
    def copyOfRange15(): Bool = region rc {
        let a = Array.copyOfRange(rc, 2, 5, Array#{1i8, 2i8, 3i8} @ rc);
        Array.length(a) == 3 and Array.memberOf(3i8, a)
    }

    @test
    def copyOfRange16(): Bool = region rc {
        Array.copyOfRange(rc, 0, 3, Array#{1i16, 2i16, 3i16} @ rc) `sameElements` Array#{1i16, 2i16, 3i16} @ rc
    }

    @test
    def copyOfRange17(): Bool = region rc {
        Array.copyOfRange(rc, 1, 3, Array#{1i16, 2i16, 3i16} @ rc) `sameElements` Array#{2i16, 3i16} @ rc
    }

    @test
    def copyOfRange18(): Bool = region rc {
        let a = Array.copyOfRange(rc, 2, 5, Array#{1i16, 2i16, 3i16} @ rc);
        Array.length(a) == 3 and Array.memberOf(3i16, a)
    }

    @test
    def copyOfRange19(): Bool = region rc {
        Array.copyOfRange(rc, 0, 3, Array#{1, 2, 3} @ rc) `sameElements` Array#{1, 2, 3} @ rc
    }

    @test
    def copyOfRange20(): Bool = region rc {
        Array.copyOfRange(rc, 1, 3, Array#{1, 2, 3} @ rc) `sameElements` Array#{2, 3} @ rc
    }

    @test
    def copyOfRange21(): Bool = region rc {
        let a = Array.copyOfRange(rc, 2, 5, Array#{1, 2, 3} @ rc);
        Array.length(a) == 3 and Array.memberOf(3, a)
    }

    @test
    def copyOfRange22(): Bool = region rc {
        Array.copyOfRange(rc, 0, 3, Array#{1i64, 2i64, 3i64} @ rc) `sameElements` Array#{1i64, 2i64, 3i64} @ rc
    }

    @test
    def copyOfRange23(): Bool = region rc {
        Array.copyOfRange(rc, 1, 3, Array#{1i64, 2i64, 3i64} @ rc) `sameElements` Array#{2i64, 3i64} @ rc
    }

    @test
    def copyOfRange24(): Bool = region rc {
        let a = Array.copyOfRange(rc, 2, 5, Array#{1i64, 2i64, 3i64} @ rc);
        Array.length(a) == 3 and Array.memberOf(3i64, a)
    }

    @test
    def copyOfRange25(): Bool = region rc {
        let a = Array.copyOfRange(rc, 0, 3, Array#{Array#{'a', 'b', 'c'} @ rc, Array#{'d', 'e', 'f', 'g'} @ rc, Array#{'h'} @ rc} @ rc);
        let b = Array#{Array#{'a', 'b', 'c'} @ rc, Array#{'d', 'e', 'f', 'g'} @ rc, Array#{'h'} @ rc} @ rc;
        Array.flatten(rc, a) `sameElements` Array.flatten(rc, b)
    }

    @test
    def copyOfRange26(): Bool = region rc {
        let a = Array.copyOfRange(rc, 1, 3, Array#{Array#{'a', 'b', 'c'} @ rc, Array#{'d', 'e', 'f', 'g'} @ rc, Array#{'h'} @ rc} @ rc);
        let b = Array#{Array#{'d', 'e', 'f', 'g'} @ rc, Array#{'h'} @ rc } @ rc;
        Array.flatten(rc, a) `sameElements` Array.flatten(rc, b)
    }

    @test
    def copyOfRange27(): Bool = region rc {
        Array.copyOfRange(rc, 0, 3, Array#{Some(1), Some(2), None} @ rc) `sameElements` Array#{Some(1), Some(2), None} @ rc
    }

    @test
    def copyOfRange28(): Bool = region rc {
        Array.copyOfRange(rc, 1, 3, Array#{Some(1), Some(2), None} @ rc) `sameElements` Array#{Some(2), None} @ rc
    }

    @test
    def copyOfRange29(): Bool = region rc {
        let a = Array.copyOfRange(rc, 2, 5, Array#{Some(1), Some(2), None} @ rc);
        Array.length(a) == 3 and Array.memberOf(None, a)
    }

    @test
    def copyOfRange30(): Bool = region rc {
        let a = Array.copyOfRange(rc, 0, 3, Array#{Array#{Some('a'), Some('b'), Some('c')} @ rc, Array#{Some('d'), Some('e'), Some('f'), Some('g')} @ rc, Array#{None} @ rc} @ rc);
        let b = Array#{Array#{Some('a'), Some('b'), Some('c')} @ rc, Array#{Some('d'), Some('e'), Some('f'), Some('g')} @ rc, Array#{None} @ rc } @ rc;
        Array.flatten(rc, a) `sameElements` Array.flatten(rc, b)
    }

    @test
    def copyOfRange31(): Bool = region rc {
        let a = Array.copyOfRange(rc, 1, 3, Array#{Array#{Some('a'), Some('b'), Some('c')} @ rc, Array#{Some('d'), Some('e'), Some('f'), Some('g')} @ rc, Array#{None} @ rc} @ rc);
        let b = Array#{Array#{Some('d'), Some('e'), Some('f'), Some('g')} @ rc, Array#{None} @ rc} @ rc;
        Array.flatten(rc, a) `sameElements` Array.flatten(rc, b)
    }

    /////////////////////////////////////////////////////////////////////////////
    // iterator                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def iterator01(): Bool = region rc {
        ((Array#{} @ rc): Array[Int32, rc]) |> Array.iterator(rc) |> Iterator.toList == Nil
    }

    @test
    def iterator02(): Bool = region rc {
        (Array#{1, 2, 3} @ rc) |> Array.iterator(rc) |> Iterator.toList == 1 :: 2 :: 3 :: Nil
    }

    @test
    def iterator03(): Bool = region rc {
        (Array#{1, 2, 3} @ rc) |> Array.iterator(rc) |> Iterator.toArray(rc) `sameElements` Array#{1, 2, 3} @ rc
    }

    @test
    def iterator04(): Bool = region rc {
        Array.range(rc, -100, 100) |> Array.iterator(rc) |> Iterator.toArray(rc) `sameElements` Array.range(rc, -100, 100)
    }


    /////////////////////////////////////////////////////////////////////////////
    // join                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def join01(): Bool = region rc {
        Array.join(",", ((Array#{} @ rc): Array[Int32, _])) == ""
    }

    @test
    def join02(): Bool = region rc {
        Array.join(",", Array#{1} @ rc) == "1"
    }

    @test
    def join03(): Bool = region rc {
        Array.join(",", Array#{1, 2, 3} @ rc) == "1,2,3"
    }

    @test
    def join04(): Bool = region rc {
        Array.join(",", Array#{"1", "2", "3"} @ rc) == "1,2,3"
    }


    /////////////////////////////////////////////////////////////////////////////
    // joinWith                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def joinWith01(): Bool = region rc {
        Array.joinWith(x -> "${x + 1}", ",", Array#{} @ rc) == ""
    }

    @test
    def joinWith02(): Bool = region rc {
        Array.joinWith(x -> "${x + 1}", ",", Array#{1} @ rc) == "2"
    }

    @test
    def joinWith03(): Bool = region rc {
        Array.joinWith(x -> "${x + 1}", ",", Array#{1, 2, 3} @ rc) == "2,3,4"
    }

    @test
    def joinWith04(): Bool = region rc {
        Array.joinWith(x -> x + x, ",", Array#{"1", "2", "3"} @ rc) == "11,22,33"
    }

    @test
    def joinWith05(): Bool = region rc {
        let a: Array[Int32, _] = Array#{} @ rc;
        let s = Array.joinWith(Int32.toString, ",", a);
        s == ""
    }

    @test
    def joinWith06(): Bool = region rc {
        let a = Array#{1} @ rc;
        let s = Array.joinWith(Int32.toString, ",", a);
        s == "1"
    }

    @test
    def joinWith07(): Bool = region rc {
        let a = Array#{1,2} @ rc;
        let s = Array.joinWith(Int32.toString, ",", a);
        s == "1,2"
    }

    @test
    def joinWith08(): Bool = region rc {
        let a = Array#{1,2,3} @ rc;
        let s = Array.joinWith(Int32.toString, ",", a);
        s == "1,2,3"
    }

    @test
    def joinWith09(): Bool = region rc {
        let a = Array#{1,2,3} @ rc;
        let s = Array.joinWith(Int32.toString, "", a);
        s == "123"
    }

    @test
    def joinWith10(): Bool = region rc {
        let a = Array#{1,2,3} @ rc;
        let s = Array.joinWith(Int32.toString, "..", a);
        s == "1..2..3"
    }


    /////////////////////////////////////////////////////////////////////////////
    // toMutList                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toMutList01(): Bool = region rc {
        MutList.sameElements(Array.toMutList(rc, ((Array#{} @ rc): Array[Int32, rc])), MutList.empty(rc))
    }

    @test
    def toMutList02(): Bool = region rc {
        let v = MutList.empty(rc);
        MutList.push!(1, v);
        MutList.sameElements(Array.toMutList(rc, Array#{1} @ rc), v)
    }

    @test
    def toMutList03(): Bool = region rc {
        let v = MutList.empty(rc);
        MutList.push!(1, v);
        MutList.push!(2, v);
        MutList.push!(3, v);
        MutList.sameElements(Array.toMutList(rc, Array#{1, 2, 3} @ rc), v)
    }

    @test
    def toMutList04(): Bool = region rc {
        MutList.sameElements((Array.range(rc, 0, 100) |> Array.toMutList(rc)), MutList.range(rc, 0, 100))
    }

    @test
    def toMutList05(): Bool = region rc {
        let v = Array.toMutList(rc, Array#{1, 2, 3} @ rc);
        MutList.push!(4, v);
        Array.sameElements(MutList.toArray(rc, v), Array#{1, 2, 3, 4} @ rc)
    }

    @test
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

    @test
    def shuffle01(): Bool \ IO = region rc {
        let a = ((Array#{} @ rc): Array[Int32, _]);
        Array.shuffle(Random.fresh(), a);
        Array.length(a) == 0 and Array.toSet(a) == Set#{}
    }

    @test
    def shuffle02(): Bool \ IO = region rc {
        let a = (Array#{0} @ rc);
        Array.shuffle(Random.fresh(), a);
        Array.length(a) == 1 and Array.toSet(a) == Set#{0}
    }

    @test
    def shuffle03(): Bool \ IO = region rc {
        let a = (Array#{0, 1, 2, 3} @ rc);
        Array.shuffle(Random.fresh(), a);
        Array.length(a) == 4 and Array.toSet(a) == Set#{0, 1, 2, 3}
    }

    @test
    def shuffle04(): Bool \ IO = region rc {
        let a = (Array#{0, 1, 2, 3, 4, 5, 6, 7, 8, 9} @ rc);
        Array.shuffle(Random.fresh(), a);
        Array.length(a) == 10 and Array.toSet(a) == Set#{0, 1, 2, 3, 4, 5, 6, 7, 8, 9}
    }

}

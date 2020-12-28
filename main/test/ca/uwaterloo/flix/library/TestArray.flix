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

    use ToString.toString;

/////////////////////////////////////////////////////////////////////////////
// new                                                                     //
/////////////////////////////////////////////////////////////////////////////

@test
def testNew01(): Bool & Impure = Array.new(42, 10)[0] == 42

@test
def testNew02(): Bool & Impure = Array.new(42, 10)[5] == 42

@test
def testNew03(): Bool & Impure = Array.new(42, 10)[9] == 42

/////////////////////////////////////////////////////////////////////////////
// get                                                                     //
/////////////////////////////////////////////////////////////////////////////

@test
def testGet01(): Bool & Impure = Array.get([1, 2, 3], 0) == 1

@test
def testGet02(): Bool & Impure = Array.get([1, 2, 3], 1) == 2

@test
def testGet03(): Bool & Impure = Array.get([1, 2, 3], 2) == 3

/////////////////////////////////////////////////////////////////////////////
// put                                                                     //
/////////////////////////////////////////////////////////////////////////////

@test
def testPut01(): Bool & Impure = Array.put([1], 0, 42)[0] == 42

@test
def testPut02(): Bool & Impure = Array.put([1, 2], 1, 42)[1] == 42

@test
def testPut03(): Bool & Impure = Array.put([1, 2, 3], 2, 42)[2] == 42

/////////////////////////////////////////////////////////////////////////////
// length                                                                  //
/////////////////////////////////////////////////////////////////////////////

@test
def testLength01(): Bool & Impure = Array.length([]) == 0

@test
def testLength02(): Bool & Impure = Array.length([1]) == 1

@test
def testLength03(): Bool & Impure = Array.length([1, 2, 3]) == 3

/////////////////////////////////////////////////////////////////////////////
// slice                                                                   //
/////////////////////////////////////////////////////////////////////////////

@test
def testSlice01(): Bool & Impure = Array.slice([1, 2, 3], 0, 1)[0] == 1

@test
def testSlice02(): Bool & Impure = Array.slice([1, 2, 3], 1, 2)[0] == 2

@test
def testSlice03(): Bool & Impure = Array.slice([1, 2, 3], 2, 3)[0] == 3

/////////////////////////////////////////////////////////////////////////////
// toList                                                                  //
/////////////////////////////////////////////////////////////////////////////

@test
def testToList01(): Bool & Impure = Array.toList([]: Array[Unit]) == Nil

@test
def testToList02(): Bool & Impure = Array.toList([1]) == 1 :: Nil

@test
def testToList03(): Bool & Impure = Array.toList([1, 2]) == 1 :: 2 :: Nil

/////////////////////////////////////////////////////////////////////////////
// head                                                                    //
/////////////////////////////////////////////////////////////////////////////

@test
def head01(): Bool & Impure = Array.head([]: Array[Int32]) == None

@test
def head02(): Bool & Impure = Array.head([1]) == Some(1)

@test
def head03(): Bool & Impure = Array.head([2,1]) == Some(2)

@test
def head04(): Bool & Impure = Array.head([3,2,1]) == Some(3)

/////////////////////////////////////////////////////////////////////////////
// last                                                                    //
/////////////////////////////////////////////////////////////////////////////

@test
def last01(): Bool & Impure = Array.last([]): Option[Unit] == None

@test
def last02(): Bool & Impure = Array.last([1]) == Some(1)

@test
def last03(): Bool & Impure = Array.last([1,2]) == Some(2)

@test
def last04(): Bool & Impure = Array.last([1,2,3]) == Some(3)

/////////////////////////////////////////////////////////////////////////////
// append                                                                  //
/////////////////////////////////////////////////////////////////////////////

@test
def append01(): Bool & Impure =
    let a = Array.append([]: Array[Int32], []: Array[Int32]);
    Array.sameElements(a, []: Array[Int32])

@test
def append02(): Bool & Impure =
    let a = Array.append([]: Array[Int32], [1]);
    Array.sameElements(a, [1])

@test
def append03(): Bool & Impure =
    let a = Array.append([]: Array[Int32], [1,2]);
    Array.sameElements(a, [1,2])

@test
def append04(): Bool & Impure =
    let a = Array.append([1], []: Array[Int32]);
    Array.sameElements(a, [1])

@test
def append05(): Bool & Impure =
    let a = Array.append([1,2], []: Array[Int32]);
    Array.sameElements(a, [1,2])

@test
def append06(): Bool & Impure =
    let a = Array.append([1], [2]);
    Array.sameElements(a, [1,2])

@test
def append07(): Bool & Impure =
    let a = Array.append([1,2], [3]);
    Array.sameElements(a, [1,2,3])

@test
def append08(): Bool & Impure =
    let a = Array.append([1], [2,3]);
    Array.sameElements(a, [1,2,3])

/////////////////////////////////////////////////////////////////////////////
// memberOf                                                                //
/////////////////////////////////////////////////////////////////////////////

@test
def memberOf01(): Bool & Impure = Array.memberOf(0, []: Array[Int32]) == false

@test
def memberOf02(): Bool & Impure = Array.memberOf(0, [1]) == false

@test
def memberOf03(): Bool & Impure = Array.memberOf(0, [0]) == true

@test
def memberOf04(): Bool & Impure = Array.memberOf(0, [1,2]) == false

@test
def memberOf05(): Bool & Impure = Array.memberOf(1, [1,2]) == true

@test
def memberOf06(): Bool & Impure = Array.memberOf(2, [1,2]) == true

@test
def memberOf07(): Bool & Impure = Array.memberOf(3, [1,2]) == false

/////////////////////////////////////////////////////////////////////////////
// indexOf                                                                 //
/////////////////////////////////////////////////////////////////////////////

@test
def indexOf01(): Bool & Impure = Array.indexOf(0, []: Array[Int32]) == None

@test
def indexOf02(): Bool & Impure = Array.indexOf(0, [1]) == None

@test
def indexOf03(): Bool & Impure = Array.indexOf(1, [1]) == Some(0)

@test
def indexOf04(): Bool & Impure = Array.indexOf(0, [1,2]) == None

@test
def indexOf05(): Bool & Impure = Array.indexOf(1, [1,2]) == Some(0)

@test
def indexOf06(): Bool & Impure = Array.indexOf(2, [1,2]) == Some(1)

/////////////////////////////////////////////////////////////////////////////
// indexOfLeft                                                             //
/////////////////////////////////////////////////////////////////////////////

@test
def indexOfLeft01(): Bool & Impure = Array.indexOfLeft(0, []: Array[Int32]) == None

@test
def indexOfLeft02(): Bool & Impure = Array.indexOfLeft(0, [1]) == None

@test
def indexOfLeft03(): Bool & Impure = Array.indexOfLeft(1, [1]) == Some(0)

@test
def indexOfLeft04(): Bool & Impure = Array.indexOfLeft(0, [1,2]) == None

@test
def indexOfLeft05(): Bool & Impure = Array.indexOfLeft(1, [1,2]) == Some(0)

@test
def indexOfLeft06(): Bool & Impure = Array.indexOfLeft(2, [1,2]) == Some(1)

@test
def indexOfLeft07(): Bool & Impure = Array.indexOfLeft(1, [1,1]) == Some(0)

/////////////////////////////////////////////////////////////////////////////
// indexOfRight                                                            //
/////////////////////////////////////////////////////////////////////////////

@test
def indexOfRight01(): Bool & Impure = Array.indexOfRight(0, []: Array[Int32]) == None

@test
def indexOfRight02(): Bool & Impure = Array.indexOfRight(0, [1]) == None

@test
def indexOfRight03(): Bool & Impure = Array.indexOfRight(1, [1]) == Some(0)

@test
def indexOfRight04(): Bool & Impure = Array.indexOfRight(0, [1,2]) == None

@test
def indexOfRight05(): Bool & Impure = Array.indexOfRight(1, [1,2]) == Some(0)

@test
def indexOfRight06(): Bool & Impure = Array.indexOfRight(2, [1,2]) == Some(1)

@test
def indexOfRight07(): Bool & Impure = Array.indexOfRight(1, [1,1]) == Some(1)

/////////////////////////////////////////////////////////////////////////////
// indices                                                                 //
/////////////////////////////////////////////////////////////////////////////

@test
def indices01(): Bool & Impure =
    let a = Array.indices(0, []: Array[Int32]);
    Array.sameElements(a, []: Array[Int32])

@test
def indices02(): Bool & Impure =
    let a = Array.indices(0, [1]);
    Array.sameElements(a, []: Array[Int32])

@test
def indices03(): Bool & Impure =
    let a = Array.indices(1, [1]);
    Array.sameElements(a, [0])

@test
def indices04(): Bool & Impure =
    let a = Array.indices(0, [1,2]);
    Array.sameElements(a, []: Array[Int32])

@test
def indices05(): Bool & Impure =
    let a = Array.indices(1, [1,2]);
    Array.sameElements(a, [0])

@test
def indices06(): Bool & Impure =
    let a = Array.indices(2, [1,2]);
    Array.sameElements(a, [1])

@test
def indices07(): Bool & Impure =
    let a = Array.indices(1, [1,1]);
    Array.sameElements(a, [0,1])

/////////////////////////////////////////////////////////////////////////////
// find                                                                    //
/////////////////////////////////////////////////////////////////////////////

@test
def find01(): Bool & Impure = Array.find(i -> i > 2, []: Array[Int32]) == None

@test
def find02(): Bool & Impure = Array.find(i -> i > 2, [1]) == None

@test
def find03(): Bool & Impure = Array.find(i -> i > 2, [3]) == Some(3)

@test
def find04(): Bool & Impure = Array.find(i -> i > 2, [1,2]) == None

@test
def find05(): Bool & Impure = Array.find(i -> i > 2, [6,-6]) == Some(6)

@test
def find06(): Bool & Impure = Array.find(i -> i > 2, [-6,6]) == Some(6)

@test
def find07(): Bool & Impure = Array.find(i -> i > 2, [6,7]) == Some(6)

/////////////////////////////////////////////////////////////////////////////
// findLeft                                                                //
/////////////////////////////////////////////////////////////////////////////

@test
def findLeft01(): Bool & Impure = Array.findLeft(i -> i > 2, []) == None

@test
def findLeft02(): Bool & Impure = Array.findLeft(i -> i > 2, [1]) == None

@test
def findLeft03(): Bool & Impure = Array.findLeft(i -> i > 2, [3]) == Some(3)

@test
def findLeft04(): Bool & Impure = Array.findLeft(i -> i > 2, [1,2]) == None

@test
def findLeft05(): Bool & Impure = Array.findLeft(i -> i > 2, [6,-6]) == Some(6)

@test
def findLeft06(): Bool & Impure = Array.findLeft(i -> i > 2, [-6,6]) == Some(6)

@test
def findLeft07(): Bool & Impure = Array.findLeft(i -> i > 2, [6,7]) == Some(6)

/////////////////////////////////////////////////////////////////////////////
// findRight                                                               //
/////////////////////////////////////////////////////////////////////////////

@test
def findRight01(): Bool & Impure = Array.findRight(i -> i > 2, []: Array[Int32]) == None

@test
def findRight02(): Bool & Impure = Array.findRight(i -> i > 2, [1]) == None

@test
def findRight03(): Bool & Impure = Array.findRight(i -> i > 2, [3]) == Some(3)

@test
def findRight04(): Bool & Impure = Array.findRight(i -> i > 2, [1,2]) == None

@test
def findRight05(): Bool & Impure = Array.findRight(i -> i > 2, [6,-6]) == Some(6)

@test
def findRight06(): Bool & Impure = Array.findRight(i -> i > 2, [-6,6]) == Some(6)

@test
def findRight07(): Bool & Impure = Array.findRight(i -> i > 2, [6,7]) == Some(7)

/////////////////////////////////////////////////////////////////////////////
// range                                                                   //
/////////////////////////////////////////////////////////////////////////////

@test
def range01(): Bool & Impure =
    let a = Array.range(1, 0);
    Array.sameElements(a, []: Array[Int32])

@test
def range02(): Bool & Impure =
    let a = Array.range(1, 1);
    Array.sameElements(a, []: Array[Int32])

@test
def range03(): Bool & Impure =
    let a = Array.range(1, 2);
    Array.sameElements(a, [1])

@test
def range04(): Bool & Impure =
    let a = Array.range(1, 3);
    Array.sameElements(a, [1,2])

@test
def range05(): Bool & Impure =
    let a = Array.range(1, 4);
    Array.sameElements(a, [1,2,3])

@test
def range06(): Bool & Impure =
    let a = Array.range(-1, 3);
    Array.sameElements(a, [-1,0,1,2])

/////////////////////////////////////////////////////////////////////////////
// repeat                                                                  //
/////////////////////////////////////////////////////////////////////////////

@test
def repeat01(): Bool & Impure =
    let a = Array.repeat(1, -1);
    Array.sameElements(a, []: Array[Int32])

@test
def repeat02(): Bool & Impure =
    let a = Array.repeat(1, 0);
    Array.sameElements(a, []: Array[Int32])

@test
def repeat03(): Bool & Impure =
    let a = Array.repeat(1, 1);
    Array.sameElements(a, [1])

@test
def repeat04(): Bool & Impure =
    let a = Array.repeat(1, 2);
    Array.sameElements(a, [1,1])

@test
def repeat05(): Bool & Impure =
    let a = Array.repeat(1, 3);
    Array.sameElements(a, [1,1,1])

/////////////////////////////////////////////////////////////////////////////
// scan                                                                    //
/////////////////////////////////////////////////////////////////////////////

@test
def scan01(): Bool & Impure =
    let a = Array.scan((i, b) -> if (b) i+1 else i+2, 1, []);
    Array.sameElements(a, [1])

@test
def scan02(): Bool & Impure =
    let a = Array.scan((i, b) -> if (b) i+1 else i+2, 1, [false]);
    Array.sameElements(a, [1,3])

@test
def scan03(): Bool & Impure =
    let a = Array.scan((i, b) -> if (b) i+1 else i+2, 1, [true]);
    Array.sameElements(a, [1,2])

@test
def scan04(): Bool & Impure =
    let a = Array.scan((i, b) -> if (b) i+1 else i+2, 1, [false,false]);
    Array.sameElements(a, [1,3,5])

@test
def scan05(): Bool & Impure =
    let a = Array.scan((i, b) -> if (b) i+1 else i+2, 1, [false,true]);
    Array.sameElements(a, [1,3,4])

@test
def scan06(): Bool & Impure =
    let a = Array.scan((i, b) -> if (b) i+1 else i+2, 1, [true,false]);
    Array.sameElements(a, [1,2,4])

@test
def scan07(): Bool & Impure =
    let a = Array.scan((i, b) -> if (b) i+1 else i+2, 1, [true,true]);
    Array.sameElements(a, [1,2,3])

/////////////////////////////////////////////////////////////////////////////
// scanLeft                                                                //
/////////////////////////////////////////////////////////////////////////////

@test
def scanLeft01(): Bool & Impure =
    let a = Array.scanLeft((i, b) -> if (b) i+1 else i+2, 1, []: Array[Bool]);
    Array.sameElements(a, [1])

@test
def scanLeft02(): Bool & Impure =
    let a = Array.scanLeft((i, b) -> if (b) i+1 else i+2, 1, [false]);
    Array.sameElements(a, [1,3])

@test
def scanLeft03(): Bool & Impure =
    let a = Array.scanLeft((i, b) -> if (b) i+1 else i+2, 1, [true]);
    Array.sameElements(a, [1,2])

@test
def scanLeft04(): Bool & Impure =
    let a = Array.scanLeft((i, b) -> if (b) i+1 else i+2, 1, [false,false]);
    Array.sameElements(a, [1,3,5])

@test
def scanLeft05(): Bool & Impure =
    let a = Array.scanLeft((i, b) -> if (b) i+1 else i+2, 1, [false,true]);
    Array.sameElements(a, [1,3,4])

@test
def scanLeft06(): Bool & Impure =
    let a = Array.scanLeft((i, b) -> if (b) i+1 else i+2, 1, [true,false]);
    Array.sameElements(a, [1,2,4])

@test
def scanLeft07(): Bool & Impure =
    let a = Array.scanLeft((i, b) -> if (b) i+1 else i+2, 1, [true,true]);
    Array.sameElements(a, [1,2,3])

/////////////////////////////////////////////////////////////////////////////
// scanRight                                                               //
/////////////////////////////////////////////////////////////////////////////

@test
def scanRight01(): Bool & Impure =
    let a = Array.scanRight((b, i) -> if (b) i+1 else i+2, 1, []);
    Array.sameElements(a, [1])

@test
def scanRight02(): Bool & Impure =
    let a = Array.scanRight((b, i) -> if (b) i+1 else i+2, 1, [false]);
    Array.sameElements(a, [3,1])

@test
def scanRight03(): Bool & Impure =
    let a = Array.scanRight((b, i) -> if (b) i+1 else i+2, 1, [true]);
    Array.sameElements(a, [2,1])

@test
def scanRight04(): Bool & Impure =
    let a = Array.scanRight((b, i) -> if (b) i+1 else i+2, 1, [false,false]);
    Array.sameElements(a, [5,3,1])

@test
def scanRight05(): Bool & Impure =
    let a = Array.scanRight((b, i) -> if (b) i+1 else i+2, 1, [false,true]);
    Array.sameElements(a, [4,2,1])

@test
def scanRight06(): Bool & Impure =
    let a = Array.scanRight((b, i) -> if (b) i+1 else i+2, 1, [true,false]);
    Array.sameElements(a, [4,3,1])

@test
def scanRight07(): Bool & Impure =
    let a = Array.scanRight((b, i) -> if (b) i+1 else i+2, 1, [true,true]);
    Array.sameElements(a, [3,2,1])

/////////////////////////////////////////////////////////////////////////////
// map                                                                     //
/////////////////////////////////////////////////////////////////////////////

@test
def map01(): Bool & Impure =
    let a = Array.map(i -> i > 2, []);
    Array.sameElements(a, []: Array[Bool])

@test
def map02(): Bool & Impure =
    let a = Array.map(i -> i > 2, [1]);
    Array.sameElements(a, [false])

@test
def map03(): Bool & Impure =
    let a = Array.map(i -> i > 2, [3]);
    Array.sameElements(a, [true])

@test
def map04(): Bool & Impure =
    let a = Array.map(i -> i > 2, [1,2]);
    Array.sameElements(a, [false,false])

@test
def map05(): Bool & Impure =
    let a = Array.map(i -> i > 2, [1,8]);
    Array.sameElements(a, [false, true])

@test
def map06(): Bool & Impure =
    let a = Array.map(i -> i > 2, [8,1]);
    Array.sameElements(a, [true,false])

@test
def map07(): Bool & Impure =
    let a = Array.map(i -> i > 2, [7,8]);
    Array.sameElements(a, [true,true])

/////////////////////////////////////////////////////////////////////////////
// transform!                                                              //
/////////////////////////////////////////////////////////////////////////////

@test
def transform01!(): Bool & Impure =
    let a: Array[Int32] = [];
    Array.transform!(x -> x+1, a);
    Array.sameElements(a, []: Array[Int32])

@test
def transform02!(): Bool & Impure =
    let a = [0];
    Array.transform!(x -> x+1, a);
    Array.sameElements(a, [1])

@test
def transform03!(): Bool & Impure =
    let a = [0,1];
    Array.transform!(x -> x+1, a);
    Array.sameElements(a, [1,2])

@test
def transform04!(): Bool & Impure =
    let a = [0,1,2];
    Array.transform!(x -> x+1, a);
    Array.sameElements(a, [1,2,3])

/////////////////////////////////////////////////////////////////////////////
// mapWithIndex                                                            //
/////////////////////////////////////////////////////////////////////////////

@test
def mapWithIndex01(): Bool & Impure =
    let a = Array.mapWithIndex((e, i) -> if (i < 1) e > 2 else e <= 2, []);
    Array.sameElements(a, []: Array[Bool])

@test
def mapWithIndex02(): Bool & Impure =
    let a = Array.mapWithIndex((e, i) -> if (i < 1) e > 2 else e <= 2, [1]);
    Array.sameElements(a, [false])

@test
def mapWithIndex03(): Bool & Impure =
    let a = Array.mapWithIndex((e, i) -> if (i < 1) e > 2 else e <= 2, [3]);
    Array.sameElements(a, [true])

@test
def mapWithIndex04(): Bool & Impure =
    let a = Array.mapWithIndex((e, i) -> if (i < 1) e > 2 else e <= 2, [1,2]);
    Array.sameElements(a, [false,true])

@test
def mapWithIndex05(): Bool & Impure =
    let a = Array.mapWithIndex((e, i) -> if (i < 1) e > 2 else e <= 2, [1,8]);
    Array.sameElements(a, [false,false])

@test
def mapWithIndex06(): Bool & Impure =
    let a = Array.mapWithIndex((e, i) -> if (i < 1) e > 2 else e <= 2, [8,1]);
    Array.sameElements(a, [true,true])

@test
def mapWithIndex07(): Bool & Impure =
    let a = Array.mapWithIndex((e, i) -> if (i < 1) e > 2 else e <= 2, [7,8]);
    Array.sameElements(a, [true,false])

/////////////////////////////////////////////////////////////////////////////
// transformWithIndex!                                                     //
/////////////////////////////////////////////////////////////////////////////

@test
def transformWithIndex01!(): Bool & Impure =
    let a = []: Array[Int32];
    Array.transformWithIndex!((e, i) -> if (i < 1) e + 10 else e, a);
    Array.sameElements(a, []: Array[Int32])

@test
def transformWithIndex02!(): Bool & Impure =
    let a = [1];
    Array.transformWithIndex!((e, i) -> if (i < 1) e + 10 else e, a);
    Array.sameElements(a, [11])

@test
def transformWithIndex03!(): Bool & Impure =
    let a = [3];
    Array.transformWithIndex!((e, i) -> if (i < 1) e + 10 else e, a);
    Array.sameElements(a, [13])

@test
def transformWithIndex04!(): Bool & Impure =
    let a = [1,2];
    Array.transformWithIndex!((e, i) -> if (i < 1) e + 10 else e, a);
    Array.sameElements(a, [11,2])

@test
def transformWithIndex05!(): Bool & Impure =
    let a = [1,8];
    Array.transformWithIndex!((e, i) -> if (i < 1) e + 10 else e, a);
    Array.sameElements(a, [11,8])

@test
def transformWithIndex06!(): Bool & Impure =
    let a = [8,1];
    Array.transformWithIndex!((e, i) -> if (i < 1) e + 10 else e, a);
    Array.sameElements(a, [18,1])

@test
def transformWithIndex07!(): Bool & Impure =
    let a = [7,8,9];
    Array.transformWithIndex!((e, i) -> if (i < 1) e + 10 else e, a);
    Array.sameElements(a, [17,8,9])

/////////////////////////////////////////////////////////////////////////////
// flatMap                                                                 //
/////////////////////////////////////////////////////////////////////////////

@test
def flatMap01(): Bool & Impure =
    let a = Array.flatMap(i -> Array.repeat(i, i), []);
    Array.sameElements(a, []: Array[Int32])

@test
def flatMap02(): Bool & Impure =
    let a = Array.flatMap(i -> Array.repeat(i, i), [0]);
    Array.sameElements(a, []: Array[Int32])

@test
def flatMap03(): Bool & Impure =
    let a = Array.flatMap(i -> Array.repeat(i, i), [1]);
    Array.sameElements(a, [1])

@test
def flatMap04(): Bool & Impure =
    let a = Array.flatMap(i -> Array.repeat(i, i), [2]);
    Array.sameElements(a, [2,2])

@test
def flatMap05(): Bool & Impure =
    let a = Array.flatMap(i -> Array.repeat(i, i), [1,2]);
    Array.sameElements(a, [1,2,2])

@test
def flatMap06(): Bool & Impure =
    let a = Array.flatMap(i -> Array.repeat(i, i), [2,3]);
    Array.sameElements(a, [2,2,3,3,3])

/////////////////////////////////////////////////////////////////////////////
// reverse                                                                 //
/////////////////////////////////////////////////////////////////////////////

@test
def reverse01(): Bool & Impure =
    let a = Array.reverse([]: Array[Int32]);
    Array.sameElements(a, []: Array[Int32])

@test
def reverse02(): Bool & Impure =
    let a = Array.reverse([1]);
    Array.sameElements(a, [1])

@test
def reverse03(): Bool & Impure =
    let a = Array.reverse([1,2]);
    Array.sameElements(a, [2,1])

@test
def reverse04(): Bool & Impure =
    let a = Array.reverse([1,1]);
    Array.sameElements(a, [1,1])

@test
def reverse05(): Bool & Impure =
    let a = Array.reverse([1,2,3]);
    Array.sameElements(a, [3,2,1])

@test
def reverse06(): Bool & Impure =
    let a = Array.reverse([1,2,3,4]);
    Array.sameElements(a, [4,3,2,1])

/////////////////////////////////////////////////////////////////////////////
// reverse!                                                                //
/////////////////////////////////////////////////////////////////////////////

@test
def reverse01!(): Bool & Impure =
    let a: Array[Int32] = [];
    Array.reverse!(a);
    Array.sameElements(a, []: Array[Int32])

@test
def reverse02!(): Bool & Impure =
    let a = [1];
    Array.reverse!(a);
    Array.sameElements(a, [1])

@test
def reverse03!(): Bool & Impure =
    let a = [1,2];
    Array.reverse!(a);
    Array.sameElements(a, [2,1])

@test
def reverse04!(): Bool & Impure =
    let a = [1,1];
    Array.reverse!(a);
    Array.sameElements(a, [1,1])

@test
def reverse05!(): Bool & Impure =
    let a = [1,2,3];
    Array.reverse!(a);
    Array.sameElements(a, [3,2,1])

@test
def reverse06!(): Bool & Impure =
    let a = [1,2,3,4];
    Array.reverse!(a);
    Array.sameElements(a, [4,3,2,1])

/////////////////////////////////////////////////////////////////////////////
// rotateLeft                                                              //
/////////////////////////////////////////////////////////////////////////////

@test
def rotateLeft01(): Bool & Impure =
    let a = Array.rotateLeft(0, []: Array[Int32]);
    Array.sameElements(a, []: Array[Int32])

@test
def rotateLeft02(): Bool & Impure =
    let a = Array.rotateLeft(1, []: Array[Int32]);
    Array.sameElements(a, []: Array[Int32])

@test
def rotateLeft03(): Bool & Impure =
    let a = Array.rotateLeft(0, [1]);
    Array.sameElements(a, [1])

@test
def rotateLeft04(): Bool & Impure =
    let a = Array.rotateLeft(0, [1,2]);
    Array.sameElements(a, [1,2])

@test
def rotateLeft05(): Bool & Impure =
    let a = Array.rotateLeft(1, [1,2]);
    Array.sameElements(a, [2,1])

@test
def rotateLeft06(): Bool & Impure =
    let a = Array.rotateLeft(2, [1,2]);
    Array.sameElements(a, [1,2])

@test
def rotateLeft07(): Bool & Impure =
    let a = Array.rotateLeft(3, [1,2]);
    Array.sameElements(a, [2,1])

@test
def rotateLeft08(): Bool & Impure =
    let a = Array.rotateLeft(-1, [1,2]);
    Array.sameElements(a, [2,1])

@test
def rotateLeft09(): Bool & Impure =
    let a = Array.rotateLeft(0, [1,2,3]);
    Array.sameElements(a, [1,2,3])

@test
def rotateLeft10(): Bool & Impure =
    let a = Array.rotateLeft(1, [1,2,3]);
    Array.sameElements(a, [2,3,1])

@test
def rotateLeft11(): Bool & Impure =
    let a = Array.rotateLeft(2, [1,2,3]);
    Array.sameElements(a, [3,1,2])

/////////////////////////////////////////////////////////////////////////////
// rotateRight                                                             //
/////////////////////////////////////////////////////////////////////////////

@test
def rotateRight01(): Bool & Impure =
    let a = Array.rotateRight(0, []: Array[Int32]);
    Array.sameElements(a, []: Array[Int32])

@test
def rotateRight02(): Bool & Impure =
    let a = Array.rotateRight(1, []: Array[Int32]);
    Array.sameElements(a, []: Array[Int32])

@test
def rotateRight03(): Bool & Impure =
    let a = Array.rotateRight(0, [1]);
    Array.sameElements(a, [1])

@test
def rotateRight04(): Bool & Impure =
    let a = Array.rotateRight(0, [1,2]);
    Array.sameElements(a, [1,2])

@test
def rotateRight05(): Bool & Impure =
    let a = Array.rotateRight(1, [1,2]);
    Array.sameElements(a, [2,1])

@test
def rotateRight06(): Bool & Impure =
    let a = Array.rotateRight(2, [1,2]);
    Array.sameElements(a, [1,2])

@test
def rotateRight07(): Bool & Impure =
    let a = Array.rotateRight(3, [1,2]);
    Array.sameElements(a, [2,1])

@test
def rotateRight08(): Bool & Impure =
    let a = Array.rotateRight(-1, [1,2]);
    Array.sameElements(a, [2,1])

@test
def rotateRight09(): Bool & Impure =
    let a = Array.rotateRight(0, [1,2,3]);
    Array.sameElements(a, [1,2,3])

@test
def rotateRight10(): Bool & Impure =
    let a = Array.rotateRight(1, [1,2,3]);
    Array.sameElements(a, [3,1,2])

@test
def rotateRight11(): Bool & Impure =
    let a = Array.rotateRight(2, [1,2,3]);
    Array.sameElements(a, [2,3,1])

/////////////////////////////////////////////////////////////////////////////
// update                                                                  //
/////////////////////////////////////////////////////////////////////////////

@test
def update01(): Bool & Impure =
    let a = Array.update(0, 2, []: Array[Int32]);
    Array.sameElements(a, []: Array[Int32])

@test
def update02(): Bool & Impure =
    let a = Array.update(-1, 2, [1]);
    Array.sameElements(a, [1])

@test
def update03(): Bool & Impure =
    let a = Array.update(0, 2, [1]);
    Array.sameElements(a, [2])

@test
def update04(): Bool & Impure =
    let a = Array.update(1, 2, [1]);
    Array.sameElements(a, [1])

@test
def update05(): Bool & Impure =
    let a = Array.update(0, 5, [1,2]);
    Array.sameElements(a, [5,2])

@test
def update06(): Bool & Impure =
    let a = Array.update(1, 5, [1,2]);
    Array.sameElements(a, [1,5])

@test
def update07(): Bool & Impure =
    let a = Array.update(2, 5, [1,2]);
    Array.sameElements(a, [1,2])

/////////////////////////////////////////////////////////////////////////////
// replace                                                                 //
/////////////////////////////////////////////////////////////////////////////

@test
def replace01(): Bool & Impure =
    let a = Array.replace(3, 4, []);
    Array.sameElements(a, []: Array[Int32])

@test
def replace02(): Bool & Impure =
    let a = Array.replace(3, 4, [1]);
    Array.sameElements(a, [1])

@test
def replace03(): Bool & Impure =
    let a = Array.replace(3, 4, [3]);
    Array.sameElements(a, [4])

@test
def replace04(): Bool & Impure =
    let a = Array.replace(3, 4, [4]);
    Array.sameElements(a, [4])

@test
def replace05(): Bool & Impure =
    let a = Array.replace(3, 4, [1, 2]);
    Array.sameElements(a, [1,2])

@test
def replace06(): Bool & Impure =
    let a = Array.replace(3, 4, [1,3]);
    Array.sameElements(a, [1,4])

@test
def replace07(): Bool & Impure =
    let a = Array.replace(3, 4, [3,4]);
    Array.sameElements(a, [4,4])

@test
def replace08(): Bool & Impure =
    let a = Array.replace(3, 4, [3,3]);
    Array.sameElements(a, [4,4])

/////////////////////////////////////////////////////////////////////////////
// replace!                                                                //
/////////////////////////////////////////////////////////////////////////////

@test
def replace01!(): Bool & Impure =
    let a: Array[Int32] = [];
    Array.replace!(3, 4, a);
    Array.sameElements(a, []: Array[Int32])

@test
def replace02!(): Bool & Impure =
    let a = [1];
    Array.replace!(3, 4, a);
    Array.sameElements(a, [1])

@test
def replace03!(): Bool & Impure =
    let a = [3];
    Array.replace!(3, 4, a);
    Array.sameElements(a, [4])

@test
def replace04!(): Bool & Impure =
    let a = [4];
    Array.replace!(3, 4, a);
    Array.sameElements(a, [4])

@test
def replace05!(): Bool & Impure =
    let a = [1, 2];
    Array.replace!(3, 4, a);
    Array.sameElements(a, [1,2])

@test
def replace06!(): Bool & Impure =
    let a = [1,3];
    Array.replace!(3, 4, a);
    Array.sameElements(a, [1,4])

@test
def replace07!(): Bool & Impure =
    let a = [3,4];
    Array.replace!(3, 4, a);
    Array.sameElements(a, [4,4])

@test
def replace08!(): Bool & Impure =
    let a = [3,3];
    Array.replace!(3, 4, a);
    Array.sameElements(a, [4,4])

/////////////////////////////////////////////////////////////////////////////
// patch                                                                   //
/////////////////////////////////////////////////////////////////////////////

@test
def patch01(): Bool & Impure =
    let a = Array.patch(0, 0, []: Array[Int32], []: Array[Int32]);
    Array.sameElements(a, []: Array[Int32])

@test
def patch02(): Bool & Impure =
    let a = Array.patch(0, 2, [1,2], []);
    Array.sameElements(a, []: Array[Int32])

@test
def patch03(): Bool & Impure =
    let a = Array.patch(0, 2, [], [1,2]);
    Array.sameElements(a, [1,2])

@test
def patch04(): Bool & Impure =
    let a = Array.patch(-3, 3, [1,2,4], [1,2]);
    Array.sameElements(a, [1,2])

@test
def patch05(): Bool & Impure =
    let a = Array.patch(2, 3, [1,2,4], [1,2]);
    Array.sameElements(a, [1,2])

@test
def patch06(): Bool & Impure =
    let a = Array.patch(0, 0, [], [1]);
    Array.sameElements(a, [1])

@test
def patch07(): Bool & Impure =
    let a = Array.patch(1, 0, [2], [1]);
    Array.sameElements(a, [1])

@test
def patch08(): Bool & Impure =
    let a = Array.patch(0, 1, [2], [1]);
    Array.sameElements(a, [2])

@test
def patch09(): Bool & Impure =
    let a = Array.patch(0, 2, [2,4], [1]);
    Array.sameElements(a, [2])

@test
def patch10(): Bool & Impure =
    let a = Array.patch(-1, 2, [2,4], [1]);
    Array.sameElements(a, [4])

@test
def patch11(): Bool & Impure =
    let a = Array.patch(-1, 2, [3,4], [1,2]);
    Array.sameElements(a, [4,2])

@test
def patch12(): Bool & Impure =
    let a = Array.patch(1, 2, [3,4], [1,2]);
    Array.sameElements(a, [1,3])

@test
def patch13(): Bool & Impure =
    let a = Array.patch(-2, 2, [3,4], [1,2]);
    Array.sameElements(a, [1,2])

@test
def patch14(): Bool & Impure =
    let a = Array.patch(2, 2, [3,4], [1,2]);
    Array.sameElements(a, [1,2])

@test
def patch15(): Bool & Impure =
    let a = Array.patch(1, 1, [3], [1,2]);
    Array.sameElements(a, [1,3])

@test
def patch16(): Bool & Impure =
    let a = Array.patch(0, 2, [3,4], [1,2]);
    Array.sameElements(a, [3,4])

@test
def patch17(): Bool & Impure =
    let a = Array.patch(0, 1, [4], [1, 2, 3]);
    Array.sameElements(a, [4, 2, 3])

@test
def patch18(): Bool & Impure =
    let a = Array.patch(1, 1, [4], [1, 2, 3]);
    Array.sameElements(a, [1, 4, 3])

@test
def patch19(): Bool & Impure =
    let a = Array.patch(2, 1, [4], [1, 2, 3]);
    Array.sameElements(a, [1, 2, 4])

@test
def patch20(): Bool & Impure =
    let a = Array.patch(0, 2, [4, 5], [1, 2, 3]);
    Array.sameElements(a, [4, 5, 3])

@test
def patch21(): Bool & Impure =
    let a = Array.patch(1, 2, [4, 5], [1, 2, 3]);
    Array.sameElements(a, [1, 4, 5])

@test
def patch22(): Bool & Impure =
    let a = Array.patch(0, 2, [4, 5, 6], [1, 2, 3]);
    Array.sameElements(a, [4, 5, 3])

@test
def patch23(): Bool & Impure =
    let a = Array.patch(0, 3, [4, 5, 6], [1, 2, 3]);
    Array.sameElements(a, [4, 5, 6])

@test
def patch24(): Bool & Impure =
    let a = Array.patch(2, 4, [14, 15, 16, 17], [1, 2, 3, 4, 5, 6, 7]);
    Array.sameElements(a, [1, 2, 14, 15, 16, 17, 7])

@test
def patch25(): Bool & Impure =
    let a = Array.patch(-2, 4, [14, 15, 16, 17], [1, 2, 3, 4, 5, 6, 7]);
    Array.sameElements(a, [16, 17, 3, 4, 5, 6, 7])

@test
def patch26(): Bool & Impure =
    let a = Array.patch(4, 5, [14, 15, 16, 17], [1, 2, 3, 4, 5, 6, 7]);
    Array.sameElements(a, [1, 2, 3, 4, 14, 15, 16])

@test
def patch27(): Bool & Impure =
    let a = Array.patch(4, 2, [14, 15, 16, 17], [1, 2, 3, 4, 5, 6, 7]);
    Array.sameElements(a, [1, 2, 3, 4, 14, 15, 7])

@test
def patch28(): Bool & Impure =
    let a = Array.patch(-1, 10, [-1, -2, -3, -4, -5, -6, -7, -8], [1, 2, 3, 4, 5, 6, 7]);
    Array.sameElements(a, [-2, -3, -4, -5, -6, -7, -8])

/////////////////////////////////////////////////////////////////////////////
// patch!                                                                  //
/////////////////////////////////////////////////////////////////////////////

@test
def patch01!(): Bool & Impure =
    let a = []: Array[Int32];
    Array.patch!(0, 0, []: Array[Int32], a);
    Array.sameElements(a, []: Array[Int32])

@test
def patch02!(): Bool & Impure =
    let a = []: Array[Int32];
    Array.patch!(0, 2, [1,2], a);
    Array.sameElements(a, []: Array[Int32])

@test
def patch03!(): Bool & Impure =
    let a = [1,2];
    Array.patch!(0, 2, [], a);
    Array.sameElements(a, [1,2])

@test
def patch04!(): Bool & Impure =
    let a = [1,2];
    Array.patch!(-3, 3, [1,2,4], a);
    Array.sameElements(a, [1,2])

@test
def patch05!(): Bool & Impure =
    let a = [1,2];
    Array.patch!(2, 3, [1,2,4], a);
    Array.sameElements(a, [1,2])

@test
def patch06!(): Bool & Impure =
    let a = [1];
    Array.patch!(0, 0, [], a);
    Array.sameElements(a, [1])

@test
def patch07!(): Bool & Impure =
    let a = [1];
    Array.patch!(1, 0, [2], a);
    Array.sameElements(a, [1])

@test
def patch08!(): Bool & Impure =
    let a = [1];
    Array.patch!(0, 1, [2], a);
    Array.sameElements(a, [2])

@test
def patch09!(): Bool & Impure =
    let a = [1];
    Array.patch!(0, 2, [2,4], a);
    Array.sameElements(a, [2])

@test
def patch10!(): Bool & Impure =
    let a = [1];
    Array.patch!(-1, 2, [2,4], a);
    Array.sameElements(a, [4])

@test
def patch11!(): Bool & Impure =
    let a = [1,2];
    Array.patch!(-1, 2, [3,4], a);
    Array.sameElements(a, [4,2])

@test
def patch12!(): Bool & Impure =
    let a = [1,2];
    Array.patch!(1, 2, [3,4], a);
    Array.sameElements(a, [1,3])

@test
def patch13!(): Bool & Impure =
    let a = [1,2];
    Array.patch!(-2, 2, [3,4], a);
    Array.sameElements(a, [1,2])

@test
def patch14!(): Bool & Impure =
    let a = [1,2];
    Array.patch!(2, 2, [3,4], a);
    Array.sameElements(a, [1,2])

@test
def patch15!(): Bool & Impure =
    let a = [1,2];
    Array.patch!(1, 1, [3], a);
    Array.sameElements(a, [1,3])

@test
def patch16!(): Bool & Impure =
    let a = [1,2];
    Array.patch!(0, 2, [3,4], a);
    Array.sameElements(a, [3,4])

@test
def patch17!(): Bool & Impure =
    let a = [1, 2, 3];
    Array.patch!(0, 1, [4], a);
    Array.sameElements(a, [4, 2, 3])

@test
def patch18!(): Bool & Impure =
    let a = [1, 2, 3];
    Array.patch!(1, 1, [4], a);
    Array.sameElements(a, [1, 4, 3])

@test
def patch19!(): Bool & Impure =
    let a = [1, 2, 3];
    Array.patch!(2, 1, [4], a);
    Array.sameElements(a, [1, 2, 4])

@test
def patch20!(): Bool & Impure =
    let a = [1, 2, 3];
    Array.patch!(0, 2, [4, 5], a);
    Array.sameElements(a, [4, 5, 3])

@test
def patch21!(): Bool & Impure =
    let a = [1, 2, 3];
    Array.patch!(1, 2, [4, 5], a);
    Array.sameElements(a, [1, 4, 5])

@test
def patch22!(): Bool & Impure =
    let a = [1, 2, 3];
    Array.patch!(0, 2, [4, 5, 6], a);
    Array.sameElements(a, [4, 5, 3])

@test
def patch23!(): Bool & Impure =
    let a = [1, 2, 3];
    Array.patch!(0, 3, [4, 5, 6], a);
    Array.sameElements(a, [4, 5, 6])

@test
def patch24!(): Bool & Impure =
    let a = [1, 2, 3, 4, 5, 6, 7];
    Array.patch!(2, 4, [14, 15, 16, 17], a);
    Array.sameElements(a, [1, 2, 14, 15, 16, 17, 7])

@test
def patch25!(): Bool & Impure =
    let a = [1, 2, 3, 4, 5, 6, 7];
    Array.patch!(-2, 4, [14, 15, 16, 17], a);
    Array.sameElements(a, [16, 17, 3, 4, 5, 6, 7])

@test
def patch26!(): Bool & Impure =
    let a = [1, 2, 3, 4, 5, 6, 7];
    Array.patch!(4, 5, [14, 15, 16, 17], a);
    Array.sameElements(a, [1, 2, 3, 4, 14, 15, 16])

@test
def patch27!(): Bool & Impure =
    let a = [1, 2, 3, 4, 5, 6, 7];
    Array.patch!(4, 2, [14, 15, 16, 17], a);
    Array.sameElements(a, [1, 2, 3, 4, 14, 15, 7])

@test
def patch28!(): Bool & Impure =
    let a = [1, 2, 3, 4, 5, 6, 7];
    Array.patch!(-1, 10, [-1, -2, -3, -4, -5, -6, -7, -8], a);
    Array.sameElements(a, [-2, -3, -4, -5, -6, -7, -8])

/////////////////////////////////////////////////////////////////////////////
// intersperse                                                             //
/////////////////////////////////////////////////////////////////////////////

@test
def intersperse01(): Bool & Impure =
    let a = Array.intersperse(11, []);
    Array.sameElements(a, []: Array[Int32])

@test
def intersperse02(): Bool & Impure =
    let a = Array.intersperse(11, [1]);
    Array.sameElements(a, [1])

@test
def intersperse03(): Bool & Impure =
    let a = Array.intersperse(11, [1,2]);
    Array.sameElements(a, [1,11,2])

@test
def intersperse04(): Bool & Impure =
    let a = Array.intersperse(11, [1,2,3]);
    Array.sameElements(a, [1,11,2,11,3])

@test
def intersperse05(): Bool & Impure =
    let a = Array.intersperse(11, [1,2,3,4]);
    Array.sameElements(a, [1,11,2,11,3,11,4])


/////////////////////////////////////////////////////////////////////////////
// intercalate                                                             //
/////////////////////////////////////////////////////////////////////////////

@test
def intercalate01(): Bool & Impure =
    let a = Array.intercalate([]: Array[Int32], []: Array[Array[Int32]]);
    Array.sameElements(a, []: Array[Int32])

@test
def intercalate02(): Bool & Impure =
    let a = Array.intercalate([]: Array[Int32], [[1]]);
    Array.sameElements(a, [1])

@test
def intercalate03(): Bool & Impure =
    let a = Array.intercalate([11,12,13], []: Array[Array[Int32]]);
    Array.sameElements(a, []: Array[Int32])

@test
def intercalate04(): Bool & Impure =
    let a = Array.intercalate([]: Array[Int32], [[1],[2,3]]);
    Array.sameElements(a, [1,2,3])

@test
def intercalate05(): Bool & Impure =
    let a = Array.intercalate([11,12,13], [[1],[2,3]]);
    Array.sameElements(a, [1,11,12,13,2,3])

@test
def intercalate06(): Bool & Impure =
    let a = Array.intercalate([]: Array[Int32], [[1],[2,3],[4]]);
    Array.sameElements(a, [1,2,3,4])

@test
def intercalate07(): Bool & Impure =
    let a = Array.intercalate([11,12,13], [[1],[2,3],[4]]);
    Array.sameElements(a, [1,11,12,13,2,3,11,12,13,4])

/////////////////////////////////////////////////////////////////////////////
// transpose                                                               //
/////////////////////////////////////////////////////////////////////////////

@test
def transpose01(): Bool & Impure =
    let a = Array.transpose([]);
    let b = Array.toList(a) |> List.map(Array.toList);
    b: List[List[Unit]] == Nil

@test
def transpose02(): Bool & Impure =
    let a: Array[Array[Int32]] = Array.transpose([[]]);
    let b = Array.toList(a) |> List.map(Array.toList);
    b == Nil :: Nil

@test
def transpose03(): Bool & Impure =
    let a: Array[Array[Int32]] = Array.transpose([[], []]);
    let b = Array.toList(a) |> List.map(Array.toList);
    b == Nil :: Nil :: Nil

@test
def transpose04(): Bool & Impure =
    let a: Array[Array[Int32]] = Array.transpose([[], [], []]);
    let b = Array.toList(a) |> List.map(Array.toList);
    b == Nil :: Nil :: Nil :: Nil

@test
def transpose05(): Bool & Impure =
    let a = Array.transpose([ [1] ]);
    let b = Array.toList(a) |> List.map(Array.toList);
    b == (1 :: Nil) :: Nil

@test
def transpose06(): Bool & Impure =
    let a = Array.transpose([[1,2]]);
    let b = Array.toList(a) |> List.map(Array.toList);
    b == (1 :: Nil) :: (2 :: Nil) :: Nil

@test
def transpose07(): Bool & Impure =
    let a = Array.transpose([[1, 2, 3]]);
    let b = Array.toList(a) |> List.map(Array.toList);
    b == (1 :: Nil) :: (2 :: Nil) :: (3 :: Nil) :: Nil

@test
def transpose08(): Bool & Impure =
    let a = Array.transpose([[1, 2, 3, 4]]);
    let b = Array.toList(a) |> List.map(Array.toList);
    b == (1 :: Nil) :: (2 :: Nil) :: (3 :: Nil) :: (4 :: Nil) :: Nil

@test
def transpose09(): Bool & Impure =
    let a = Array.transpose([[1], [2]]);
    let b = Array.toList(a) |> List.map(Array.toList);
    b ==   (1 :: 2 :: Nil) :: Nil

@test
def transpose10(): Bool & Impure =
    let a = Array.transpose([[1], [2], [3]]);
    let b = Array.toList(a) |> List.map(Array.toList);
    b ==  (1 :: 2 :: 3 :: Nil) :: Nil

@test
def transpose11(): Bool & Impure =
    let a = Array.transpose([[1], [2], [3], [4]]);
    let b = Array.toList(a) |> List.map(Array.toList);
    b == (1 :: 2 :: 3 :: 4 :: Nil) :: Nil

@test
def transpose12(): Bool & Impure =
    let a = Array.transpose([[1, 2], [3, 4]]);
    let b = Array.toList(a) |> List.map(Array.toList);
    b == (1 :: 3 :: Nil) :: (2 :: 4 :: Nil) :: Nil

@test
def transpose13(): Bool & Impure =
    let a = Array.transpose([[1, 2, 3], [4, 5, 6]]);
    let b = Array.toList(a) |> List.map(Array.toList);
    b == (1 :: 4 :: Nil) :: (2 :: 5 :: Nil) :: (3 :: 6 :: Nil) :: Nil

@test
def transpose14(): Bool & Impure =
    let a = Array.transpose([[1, 2, 3, 4], [5, 6, 7, 8]]);
    let b = Array.toList(a) |> List.map(Array.toList);
    b == (1 :: 5 :: Nil) :: (2 :: 6 :: Nil) :: (3 :: 7 :: Nil) :: (4 :: 8 :: Nil) :: Nil

@test
def transpose15(): Bool & Impure =
    let a = Array.transpose([[1, 2, 3, 4, 5], [6, 7, 8, 9, 10]]);
    let b = Array.toList(a) |> List.map(Array.toList);
    b == (1 :: 6 :: Nil) :: (2 :: 7 :: Nil) :: (3 :: 8 :: Nil) :: (4 :: 9 :: Nil) :: (5 :: 10 :: Nil) :: Nil

@test
def transpose16(): Bool & Impure =
    let a = Array.transpose([[1, 2], [3, 4], [5, 6]]);
    let b = Array.toList(a) |> List.map(Array.toList);
    b == (1 :: 3 :: 5 :: Nil) :: (2 :: 4 :: 6 :: Nil) :: Nil

@test
def transpose17(): Bool & Impure =
    let a = Array.transpose([[1, 2], [3, 4], [5, 6], [7,8]]);
    let b = Array.toList(a) |> List.map(Array.toList);
    b == (1 :: 3 :: 5 :: 7 :: Nil) :: (2 :: 4 :: 6 :: 8 :: Nil) :: Nil

@test
def transpose18(): Bool & Impure =
    let a = Array.transpose([[1, 2], [3, 4], [5, 6], [7, 8], [9, 10]]);
    let b = Array.toList(a) |> List.map(Array.toList);
    b == (1 :: 3 :: 5 :: 7 :: 9 :: Nil) :: (2 :: 4 :: 6 :: 8 :: 10 :: Nil) :: Nil

@test
def transpose19(): Bool & Impure =
    let a = Array.transpose([[1, 2, 3], [4, 5, 6], [7, 8, 9]]);
    let b = Array.toList(a) |> List.map(Array.toList);
    b == (1 :: 4 :: 7 :: Nil) :: (2 :: 5 :: 8 :: Nil) :: (3 :: 6 :: 9 :: Nil) :: Nil

@test
def transpose20(): Bool & Impure =
    let a = Array.transpose([[1, 2, 3], [4, 5], [7, 8, 9]]);
    let b = Array.toList(a) |> List.map(Array.toList);
    b == (1 :: 2 :: 3 :: Nil) :: (4 :: 5 :: Nil) :: (7 :: 8 :: 9 :: Nil) :: Nil

@test
def transpose21(): Bool & Impure =
    let a = Array.transpose([[1, 2, 3], [], [7, 8, 9]]);
    let b = Array.toList(a) |> List.map(Array.toList);
    b == (1 :: 2 :: 3 :: Nil) :: Nil :: (7 :: 8 :: 9 :: Nil) :: Nil

@test
def transpose22(): Bool & Impure =
    let a = Array.transpose([[1, 2, 3], [4, 5, 6], [7, 8, 9, 10]]);
    let b = Array.toList(a) |> List.map(Array.toList);
    b == (1 :: 2 :: 3 :: Nil) :: (4 :: 5 :: 6 :: Nil) :: (7 :: 8 :: 9 :: 10 :: Nil) :: Nil

/////////////////////////////////////////////////////////////////////////////
// isPrefixOf                                                              //
/////////////////////////////////////////////////////////////////////////////

@test
def isPrefixOf01(): Bool & Impure = Array.isPrefixOf([]: Array[Int32], []: Array[Int32])

@test
def isPrefixOf02(): Bool & Impure = Array.isPrefixOf([1], []: Array[Int32]) == false

@test
def isPrefixOf03(): Bool & Impure = Array.isPrefixOf([]: Array[Int32], [1])

@test
def isPrefixOf04(): Bool & Impure = Array.isPrefixOf([1], [1])

@test
def isPrefixOf05(): Bool & Impure = Array.isPrefixOf([2], [1]) == false

@test
def isPrefixOf06(): Bool & Impure = Array.isPrefixOf([1,2], [1]) == false

@test
def isPrefixOf07(): Bool & Impure = Array.isPrefixOf([]: Array[Int32], [1,2])

@test
def isPrefixOf08(): Bool & Impure = Array.isPrefixOf([1], [1,2])

@test
def isPrefixOf09(): Bool & Impure = Array.isPrefixOf([2], [1,2]) == false

@test
def isPrefixOf10(): Bool & Impure = Array.isPrefixOf([1,2], [1,2])

@test
def isPrefixOf11(): Bool & Impure = Array.isPrefixOf([1,3], [1,2]) == false

@test
def isPrefixOf12(): Bool & Impure = Array.isPrefixOf([1,2,3], [1,2]) == false

@test
def isPrefixOf13(): Bool & Impure = Array.isPrefixOf([1,2,3], [89,11,1,2,3,4]) == false

/////////////////////////////////////////////////////////////////////////////
// isInfixOf                                                               //
/////////////////////////////////////////////////////////////////////////////

@test
def isInfixOf01(): Bool & Impure = Array.isInfixOf([]: Array[Int32], []: Array[Int32])

@test
def isInfixOf02(): Bool & Impure = Array.isInfixOf([1], []: Array[Int32]) == false

@test
def isInfixOf03(): Bool & Impure = Array.isInfixOf([]: Array[Int32], [1])

@test
def isInfixOf04(): Bool & Impure = Array.isInfixOf([1], [1])

@test
def isInfixOf05(): Bool & Impure = Array.isInfixOf([2], [1]) == false

@test
def isInfixOf06(): Bool & Impure = Array.isInfixOf([1,2], [1]) == false

@test
def isInfixOf07(): Bool & Impure = Array.isInfixOf([]: Array[Int32], [1,2])

@test
def isInfixOf08(): Bool & Impure = Array.isInfixOf([1], [1,2])

@test
def isInfixOf09(): Bool & Impure = Array.isInfixOf([2], [1,2])

@test
def isInfixOf10(): Bool & Impure = Array.isInfixOf([1,2], [1,2])

@test
def isInfixOf11(): Bool & Impure = Array.isInfixOf([1,3], [1,2]) == false

@test
def isInfixOf12(): Bool & Impure = Array.isInfixOf([1,2,3], [1,2]) == false

@test
def isInfixOf13(): Bool & Impure = Array.isInfixOf([1,2,3], [89,11,1,2,3,4])

/////////////////////////////////////////////////////////////////////////////
// isSuffixOf                                                              //
/////////////////////////////////////////////////////////////////////////////

@test
def isSuffixOf01(): Bool & Impure = Array.isSuffixOf([]: Array[Int32], []: Array[Int32])

@test
def isSuffixOf02(): Bool & Impure = Array.isSuffixOf([1], []: Array[Int32]) == false

@test
def isSuffixOf03(): Bool & Impure = Array.isSuffixOf([]: Array[Int32], [1])

@test
def isSuffixOf04(): Bool & Impure = Array.isSuffixOf([1], [1])

@test
def isSuffixOf05(): Bool & Impure = Array.isSuffixOf([2], [1]) == false

@test
def isSuffixOf06(): Bool & Impure = Array.isSuffixOf([1,2], [1]) == false

@test
def isSuffixOf07(): Bool & Impure = Array.isSuffixOf([]: Array[Int32], [1,2])

@test
def isSuffixOf08(): Bool & Impure = Array.isSuffixOf([1], [1,2]) == false

@test
def isSuffixOf09(): Bool & Impure = Array.isSuffixOf([2], [1,2])

@test
def isSuffixOf10(): Bool & Impure = Array.isSuffixOf([1,2], [1,2])

@test
def isSuffixOf11(): Bool & Impure = Array.isSuffixOf([1,3], [1,2]) == false

@test
def isSuffixOf12(): Bool & Impure = Array.isSuffixOf([1,2,3], [1,2]) == false

@test
def isSuffixOf13(): Bool & Impure = Array.isSuffixOf([1,2,3], [89,11,1,2,3])

/////////////////////////////////////////////////////////////////////////////
// fold                                                                    //
/////////////////////////////////////////////////////////////////////////////

@test
def fold01(): Bool & Impure = Array.fold((i, e) -> (i - e)*(e % 2 + 1), 100, []) == 100

@test
def fold02(): Bool & Impure = Array.fold((i, e) -> (i - e)*(e % 2 + 1), 100, [1]) == 198

@test
def fold03(): Bool & Impure = Array.fold((i, e) -> (i - e)*(e % 2 + 1), 100, [1,2]) == 196

@test
def fold04(): Bool & Impure = Array.fold((i, e) -> (i - e)*(e % 2 + 1), 100, [1,2,3]) == 386

/////////////////////////////////////////////////////////////////////////////
// foldLeft                                                                //
/////////////////////////////////////////////////////////////////////////////

@test
def foldLeft01(): Bool & Impure = Array.foldLeft((i, e) -> (i - e)*(e % 2 + 1), 100, []) == 100

@test
def foldLeft02(): Bool & Impure = Array.foldLeft((i, e) -> (i - e)*(e % 2 + 1), 100, [1]) == 198

@test
def foldLeft03(): Bool & Impure = Array.foldLeft((i, e) -> (i - e)*(e % 2 + 1), 100, [1,2]) == 196

@test
def foldLeft04(): Bool & Impure = Array.foldLeft((i, e) -> (i - e)*(e % 2 + 1), 100, [1,2,3]) == 386

/////////////////////////////////////////////////////////////////////////////
// foldRight                                                               //
/////////////////////////////////////////////////////////////////////////////

@test
def foldRight01(): Bool & Impure = Array.foldRight((e, i) -> (i - e)*(e % 2 + 1), 100, []) == 100

@test
def foldRight02(): Bool & Impure = Array.foldRight((e, i) -> (i - e)*(e % 2 + 1), 100, [1]) == 198

@test
def foldRight03(): Bool & Impure = Array.foldRight((e, i) -> (i - e)*(e % 2 + 1), 100, [1,2]) == 194

@test
def foldRight04(): Bool & Impure = Array.foldRight((e, i) -> (i - e)*(e % 2 + 1), 100, [1,2,3]) == 382

/////////////////////////////////////////////////////////////////////////////
// reduce                                                                  //
/////////////////////////////////////////////////////////////////////////////

@test
def reduce01(): Bool & Impure = Array.reduce((a, b) -> a-b, []: Array[Int]) == None

@test
def reduce02(): Bool & Impure = Array.reduce((a, b) -> a-b, [1]) == Some(1)

@test
def reduce03(): Bool & Impure = Array.reduce((a, b) -> a-b, [1,2]) == Some(-1)

@test
def reduce04(): Bool & Impure = Array.reduce((a, b) -> a-b, [1,2,3]) == Some(-4)

@test
def reduce05(): Bool & Impure = Array.reduce((a, b) -> a-b, [1,2,3,4]) == Some(-8)

/////////////////////////////////////////////////////////////////////////////
// reduceLeft                                                              //
/////////////////////////////////////////////////////////////////////////////

@test
def reduceLeft01(): Bool & Impure = Array.reduceLeft((a, b) -> a-b, []: Array[Int]) == None

@test
def reduceLeft02(): Bool & Impure = Array.reduceLeft((a, b) -> a-b, [1]) == Some(1)

@test
def reduceLeft03(): Bool & Impure = Array.reduceLeft((a, b) -> a-b, [1,2]) == Some(-1)

@test
def reduceLeft04(): Bool & Impure = Array.reduceLeft((a, b) -> a-b, [1,2,3]) == Some(-4)

@test
def reduceLeft05(): Bool & Impure = Array.reduceLeft((a, b) -> a-b, [1,2,3,4]) == Some(-8)

/////////////////////////////////////////////////////////////////////////////
// reduceRight                                                             //
/////////////////////////////////////////////////////////////////////////////

@test
def reduceRight01(): Bool & Impure = Array.reduceRight((a, b) -> a-b, []: Array[Int]) == None

@test
def reduceRight02(): Bool & Impure = Array.reduceRight((a, b) -> a-b, [1]) == Some(1)

@test
def reduceRight03(): Bool & Impure = Array.reduceRight((a, b) -> a-b, [1,2]) == Some(-1)

@test
def reduceRight04(): Bool & Impure = Array.reduceRight((a, b) -> a-b, [1,2,3]) == Some(2)

@test
def reduceRight05(): Bool & Impure = Array.reduceRight((a, b) -> a-b, [1,2,3,4]) == Some(-2)

/////////////////////////////////////////////////////////////////////////////
// count                                                                   //
/////////////////////////////////////////////////////////////////////////////

@test
def count01(): Bool & Impure = Array.count(i -> i > 3, []) == 0

@test
def count02(): Bool & Impure = Array.count(i -> i > 3, [1]) == 0

@test
def count03(): Bool & Impure = Array.count(i -> i > 3, [4]) == 1

@test
def count04(): Bool & Impure = Array.count(i -> i > 3, [1,2]) == 0

@test
def count05(): Bool & Impure = Array.count(i -> i > 3, [1,8]) == 1

@test
def count06(): Bool & Impure = Array.count(i -> i > 3, [8,1]) == 1

@test
def count07(): Bool & Impure = Array.count(i -> i > 3, [6,7]) == 2

/////////////////////////////////////////////////////////////////////////////
// flatten                                                                 //
/////////////////////////////////////////////////////////////////////////////

@test
def flatten01(): Bool & Impure =
    let a = Array.flatten([]: Array[Array[Int32]]);
    Array.sameElements(a, []: Array[Int32])

@test
def flatten02(): Bool & Impure =
    let a = Array.flatten([[]]: Array[Array[Int32]]);
    Array.sameElements(a, []: Array[Int32])

@test
def flatten03(): Bool & Impure =
    let a = Array.flatten([[1]]);
    Array.sameElements(a, [1])

@test
def flatten04(): Bool & Impure =
    let a= Array.flatten([[1,2]]);
    Array.sameElements(a, [1,2])

@test
def flatten05(): Bool & Impure =
    let a = Array.flatten([[],[]]: Array[Array[Int32]]);
    Array.sameElements(a, []: Array[Int32])

@test
def flatten06(): Bool & Impure =
    let a = Array.flatten([[1],[]]);
    Array.sameElements(a, [1])

@test
def flatten07(): Bool & Impure =
    let a = Array.flatten([[],[1]]);
    Array.sameElements(a, [1])

@test
def flatten08(): Bool & Impure =
    let a = Array.flatten([[1],[2]]);
    Array.sameElements(a, [1,2])

@test
def flatten09(): Bool & Impure =
    let a = Array.flatten([[1,2],[3,4,5]]);
    Array.sameElements(a, [1,2,3,4,5])

@test
def flatten10(): Bool & Impure =
    let a = Array.flatten([[1],[2,3],[4]]);
    Array.sameElements(a, [1,2,3,4])

/////////////////////////////////////////////////////////////////////////////
// exists                                                                  //
/////////////////////////////////////////////////////////////////////////////

@test
def exists01(): Bool & Impure = Array.exists(i -> i > 3, [] : Array[Int32]) == false

@test
def exists02(): Bool & Impure = Array.exists(i -> i > 3, [1]) == false

@test
def exists03(): Bool & Impure = Array.exists(i -> i > 3, [5]) == true

@test
def exists04(): Bool & Impure = Array.exists(i -> i > 3, [1,2]) == false

@test
def exists05(): Bool & Impure = Array.exists(i -> i > 3, [1,6]) == true

@test
def exists06(): Bool & Impure = Array.exists(i -> i > 3, [6,1]) == true

@test
def exists07(): Bool & Impure = Array.exists(i -> i > 3, [16,6]) == true

@test
def exists08(): Bool & Impure = Array.exists(i -> i > 3, [1,-9,3]) == false

@test
def exists09(): Bool & Impure = Array.exists(i -> i > 3, [1,9,3]) == true

/////////////////////////////////////////////////////////////////////////////
// forall                                                                  //
/////////////////////////////////////////////////////////////////////////////

@test
def forall01(): Bool & Impure = Array.forall(i -> i > 3, []: Array[Int32]) == true

@test
def forall02(): Bool & Impure = Array.forall(i -> i > 3, [1]) == false

@test
def forall03(): Bool & Impure = Array.forall(i -> i > 3, [5]) == true

@test
def forall04(): Bool & Impure = Array.forall(i -> i > 3, [1,2]) == false

@test
def forall05(): Bool & Impure = Array.forall(i -> i > 3, [1,6]) == false

@test
def forall06(): Bool & Impure = Array.forall(i -> i > 3, [6,1]) == false

@test
def forall07(): Bool & Impure = Array.forall(i -> i > 3, [16,6]) == true

@test
def forall08(): Bool & Impure = Array.forall(i -> i > 3, [1,-9,3]) == false

@test
def forall09(): Bool & Impure = Array.forall(i -> i > 3, [1,9,3]) == false

@test
def forall10(): Bool & Impure = Array.forall(i -> i > 3, [11,9,31]) == true

/////////////////////////////////////////////////////////////////////////////
// filter                                                                  //
/////////////////////////////////////////////////////////////////////////////

@test
def filter01(): Bool & Impure =
    let a = Array.filter(i -> i > 3, []: Array[Int32]);
    Array.sameElements(a, []: Array[Int32])

@test
def filter02(): Bool & Impure =
    let a = Array.filter(i -> i > 3, [2]);
    Array.sameElements(a, []: Array[Int32])

@test
def filter03(): Bool & Impure =
    let a = Array.filter(i -> i > 3, [4]);
    Array.sameElements(a, [4])

@test
def filter04(): Bool & Impure =
    let a = Array.filter(i -> i > 3, [1,3]);
    Array.sameElements(a, []: Array[Int32])

@test
def filter05(): Bool & Impure =
    let a = Array.filter(i -> i > 3, [1,8]);
    Array.sameElements(a, [8])

@test
def filter06(): Bool & Impure =
    let a = Array.filter(i -> i > 3, [8,1]);
    Array.sameElements(a, [8])

@test
def filter07(): Bool & Impure =
    let a = Array.filter(i -> i > 3, [8,9]);
    Array.sameElements(a, [8,9])

@test
def filter08(): Bool & Impure =
    let a = Array.filter(i -> i > 3, [1,4,11,2,-22,17]);
    Array.sameElements(a, [4,11,17])

/////////////////////////////////////////////////////////////////////////////
// partition                                                               //
/////////////////////////////////////////////////////////////////////////////

@test
def partition01(): Bool & Impure =
    let (a,b) = Array.partition(i -> i > 3, []: Array[Int32]);
    Array.sameElements(a, []: Array[Int32]) and Array.sameElements(b, []: Array[Int32])

@test
def partition02(): Bool & Impure =
    let (a,b) = Array.partition(i -> i > 3, [1]);
    Array.sameElements(a, []: Array[Int32]) and Array.sameElements(b, [1])

@test
def partition03(): Bool & Impure =
    let (a,b) = Array.partition(i -> i > 3, [4]);
    Array.sameElements(a, [4]) and Array.sameElements(b, []: Array[Int32])

@test
def partition04(): Bool & Impure =
    let (a,b) = Array.partition(i -> i > 3, [1,2]);
    Array.sameElements(a, []: Array[Int32]) and Array.sameElements(b, [1,2])

@test
def partition05(): Bool & Impure =
    let (a,b) = Array.partition(i -> i > 3, [1,5]);
    Array.sameElements(a, [5]) and Array.sameElements(b, [1])

@test
def partition06(): Bool & Impure =
    let (a,b) = Array.partition(i -> i > 3, [5,1]);
    Array.sameElements(a, [5]) and Array.sameElements(b, [1])

@test
def partition07(): Bool & Impure =
    let (a,b) = Array.partition(i -> i > 3, [5,8]);
    Array.sameElements(a, [5,8]) and Array.sameElements(b, []: Array[Int32])

@test
def partition08(): Bool & Impure =
    let (a,b) = Array.partition(i -> i > 3, [4, -3, -5, 1, 2, 16, 7, 1, 7]);
    Array.sameElements(a, [4, 16, 7, 7]) and Array.sameElements(b, [-3, -5, 1, 2, 1])

/////////////////////////////////////////////////////////////////////////////
// span                                                                    //
/////////////////////////////////////////////////////////////////////////////

@test
def span01(): Bool & Impure =
    let (a,b) = Array.span(i -> i > 3, []: Array[Int32]);
    Array.sameElements(a, []: Array[Int32]) and Array.sameElements(b, []: Array[Int32])

@test
def span02(): Bool & Impure =
    let (a,b) = Array.span(i -> i > 3, [1]);
    Array.sameElements(a,[]: Array[Int32]) and Array.sameElements(b, [1])

@test
def span03(): Bool & Impure =
    let (a,b) = Array.span(i -> i > 3, [4]);
    Array.sameElements(a, [4]) and Array.sameElements(b, []: Array[Int32])

@test
def span04(): Bool & Impure =
    let (a,b) = Array.span(i -> i > 3, [1,2]);
    Array.sameElements(a, []: Array[Int32]) and Array.sameElements(b, [1,2])

@test
def span05(): Bool & Impure =
    let (a,b) = Array.span(i -> i > 3, [1,5]);
    Array.sameElements(a, []: Array[Int32]) and Array.sameElements(b, [1,5])

@test
def span06(): Bool & Impure =
    let (a,b) = Array.span(i -> i > 3, [5,1]);
    Array.sameElements(a, [5]) and Array.sameElements(b, [1])

@test
def span07(): Bool & Impure =
    let (a,b) = Array.span(i -> i > 3, [5,8]);
    Array.sameElements(a, [5,8]) and Array.sameElements(b, []: Array[Int32])

@test
def span08(): Bool & Impure =
    let (a,b) = Array.span(i -> i > 3, [4, 6, -3, 11, -5, 1, 2, 16, 7, 1, 7]);
    Array.sameElements(a, [4, 6]) and Array.sameElements(b, [-3, 11, -5, 1, 2, 16, 7, 1, 7])

/////////////////////////////////////////////////////////////////////////////
// drop                                                                    //
/////////////////////////////////////////////////////////////////////////////

@test
def drop01(): Bool & Impure =
    let a = Array.drop(-1, []: Array[Int32]);
    Array.sameElements(a, []: Array[Int32])

@test
def drop02(): Bool & Impure =
    let a = Array.drop(0, []: Array[Int32]);
    Array.sameElements(a, []: Array[Int32])

@test
def drop03(): Bool & Impure =
    let a = Array.drop(1, []: Array[Int32]);
    Array.sameElements(a, []: Array[Int32])

@test
def drop04(): Bool & Impure =
    let a = Array.drop(-1, [1]);
    Array.sameElements(a, [1])

@test
def drop05(): Bool & Impure =
    let a = Array.drop(0, [1]);
    Array.sameElements(a, [1])

@test
def drop06(): Bool & Impure =
    let a = Array.drop(1, [1]);
    Array.sameElements(a, []: Array[Int32])

@test
def drop07(): Bool & Impure =
    let a = Array.drop(2, [1]);
    Array.sameElements(a, []: Array[Int32])

@test
def drop08(): Bool & Impure =
    let a = Array.drop(0, [1,2]);
    Array.sameElements(a, [1,2])

@test
def drop09(): Bool & Impure =
    let a = Array.drop(1, [1,2]);
    Array.sameElements(a, [2])

@test
def drop10(): Bool & Impure =
    let a = Array.drop(2, [1,2]);
    Array.sameElements(a, []: Array[Int32])

@test
def drop11(): Bool & Impure =
    let a = Array.drop(2, [1,2,3,4,5,6]);
    Array.sameElements(a, [3,4,5,6])

@test
def drop12(): Bool & Impure =
    let a = Array.drop(4, [1,2,3,4,5,6]);
    Array.sameElements(a, [5,6])

/////////////////////////////////////////////////////////////////////////////
// dropLeft                                                                //
/////////////////////////////////////////////////////////////////////////////

@test
def dropLeft01(): Bool & Impure =
    let a = Array.dropLeft(-1, []: Array[Int32]);
    Array.sameElements(a, []: Array[Int32])

@test
def dropLeft02(): Bool & Impure =
    let a = Array.dropLeft(0, []: Array[Int32]);
    Array.sameElements(a, []: Array[Int32])

@test
def dropLeft03(): Bool & Impure =
    let a = Array.dropLeft(1, []: Array[Int32]);
    Array.sameElements(a, []: Array[Int32])

@test
def dropLeft04(): Bool & Impure =
    let a = Array.dropLeft(-1, [1]);
    Array.sameElements(a, [1])

@test
def dropLeft05(): Bool & Impure =
    let a = Array.dropLeft(0, [1]);
    Array.sameElements(a, [1])

@test
def dropLeft06(): Bool & Impure =
    let a = Array.dropLeft(1, [1]);
    Array.sameElements(a, []: Array[Int32])

@test
def dropLeft07(): Bool & Impure =
    let a = Array.dropLeft(2, [1]);
    Array.sameElements(a, []: Array[Int32])

@test
def dropLeft08(): Bool & Impure =
    let a = Array.dropLeft(0, [1,2]);
    Array.sameElements(a, [1,2])

@test
def dropLeft09(): Bool & Impure =
    let a = Array.dropLeft(1, [1,2]);
    Array.sameElements(a, [2])

@test
def dropLeft10(): Bool & Impure =
    let a = Array.dropLeft(2, [1,2]);
    Array.sameElements(a, []: Array[Int32])

@test
def dropLeft11(): Bool & Impure =
    let a = Array.dropLeft(2, [1,2,3,4,5,6]);
    Array.sameElements(a, [3,4,5,6])

@test
def dropLeft12(): Bool & Impure =
    let a = Array.dropLeft(4, [1,2,3,4,5,6]);
    Array.sameElements(a, [5,6])

/////////////////////////////////////////////////////////////////////////////
// dropRight                                                               //
/////////////////////////////////////////////////////////////////////////////

@test
def dropRight01(): Bool & Impure =
    let a = Array.dropRight(-1, []: Array[Int32]);
    Array.sameElements(a, []: Array[Int32])

@test
def dropRight02(): Bool & Impure =
    let a = Array.dropRight(0, []: Array[Int32]);
    Array.sameElements(a, []: Array[Int32])

@test
def dropRight03(): Bool & Impure =
    let a = Array.dropRight(1, []: Array[Int32]);
    Array.sameElements(a, []: Array[Int32])

@test
def dropRight04(): Bool & Impure =
    let a = Array.dropRight(-1, [1]);
    Array.sameElements(a, [1])

@test
def dropRight05(): Bool & Impure =
    let a = Array.dropRight(0, [1]);
    Array.sameElements(a, [1])

@test
def dropRight06(): Bool & Impure =
    let a = Array.dropRight(1, [1]);
    Array.sameElements(a, []: Array[Int32])

@test
def dropRight07(): Bool & Impure =
    let a = Array.dropRight(2, [1]);
    Array.sameElements(a, []: Array[Int32])

@test
def dropRight08(): Bool & Impure =
    let a = Array.dropRight(0, [1,2]);
    Array.sameElements(a, [1,2])

@test
def dropRight09(): Bool & Impure =
    let a = Array.dropRight(1, [1,2]);
    Array.sameElements(a, [1])

@test
def dropRight10(): Bool & Impure =
    let a = Array.dropRight(2, [1,2]);
    Array.sameElements(a, []: Array[Int32])

@test
def dropRight11(): Bool & Impure =
    let a = Array.dropRight(2, [1,2,3,4,5,6]);
    Array.sameElements(a, [1,2,3,4])

@test
def dropRight12(): Bool & Impure =
    let a = Array.dropRight(4, [1,2,3,4,5,6]);
    Array.sameElements(a, [1,2])

/////////////////////////////////////////////////////////////////////////////
// dropWhile                                                               //
/////////////////////////////////////////////////////////////////////////////

@test
def dropWhile01(): Bool & Impure =
    let a = Array.dropWhile(i -> i > 3, []);
    Array.sameElements(a, []: Array[Int32])

@test
def dropWhile02(): Bool & Impure =
    let a = Array.dropWhile(i -> i > 3, [1]);
    Array.sameElements(a, [1])

@test
def dropWhile03(): Bool & Impure =
    let a = Array.dropWhile(i -> i > 3, [4]);
    Array.sameElements(a, []: Array[Int32])

@test
def dropWhile04(): Bool & Impure =
    let a = Array.dropWhile(i -> i > 3, [1,2]);
    Array.sameElements(a, [1,2])

@test
def dropWhile05(): Bool & Impure =
    let a = Array.dropWhile(i -> i > 3, [1,5]);
    Array.sameElements(a, [1,5])

@test
def dropWhile06(): Bool & Impure =
    let a = Array.dropWhile(i -> i > 3, [5,1]);
    Array.sameElements(a, [1])

@test
def dropWhile07(): Bool & Impure =
    let a = Array.dropWhile(i -> i > 3, [5,8]);
    Array.sameElements(a, []: Array[Int32])

@test
def dropWhile08(): Bool & Impure =
    let a = Array.dropWhile(i -> i > 3, [4, 6, -3, 11, -5, 1, 2, 16, 7, 1, 7]);
    Array.sameElements(a, [-3, 11, -5, 1, 2, 16, 7, 1, 7])

/////////////////////////////////////////////////////////////////////////////
// dropWhileLeft                                                           //
/////////////////////////////////////////////////////////////////////////////

@test
def dropWhileLeft01(): Bool & Impure =
    let a = Array.dropWhileLeft(i -> i > 3, []);
    Array.sameElements(a, []: Array[Int32])

@test
def dropWhileLeft02(): Bool & Impure =
    let a = Array.dropWhileLeft(i -> i > 3, [1]);
    Array.sameElements(a, [1])

@test
def dropWhileLeft03(): Bool & Impure =
    let a = Array.dropWhileLeft(i -> i > 3, [4]);
    Array.sameElements(a, []: Array[Int32])

@test
def dropWhileLeft04(): Bool & Impure =
    let a = Array.dropWhileLeft(i -> i > 3, [1,2]);
    Array.sameElements(a, [1,2])

@test
def dropWhileLeft05(): Bool & Impure =
    let a = Array.dropWhileLeft(i -> i > 3, [1,5]);
    Array.sameElements(a, [1,5])

@test
def dropWhileLeft06(): Bool & Impure =
    let a = Array.dropWhileLeft(i -> i > 3, [5,1]);
    Array.sameElements(a, [1])

@test
def dropWhileLeft07(): Bool & Impure =
    let a = Array.dropWhileLeft(i -> i > 3, [5,8]);
    Array.sameElements(a, []: Array[Int32])

@test
def dropWhileLeft08(): Bool & Impure =
    let a = Array.dropWhileLeft(i -> i > 3, [4, 6, -3, 11, -5, 1, 2, 16, 7, 1, 7]);
    Array.sameElements(a, [-3, 11, -5, 1, 2, 16, 7, 1, 7])

/////////////////////////////////////////////////////////////////////////////
// dropWhileRight                                                          //
/////////////////////////////////////////////////////////////////////////////

@test
def dropWhileRight01(): Bool & Impure =
    let a = Array.dropWhileRight(i -> i > 3, []);
    Array.sameElements(a, []: Array[Int32])

@test
def dropWhileRight02(): Bool & Impure =
    let a = Array.dropWhileRight(i -> i > 3, [1]);
    Array.sameElements(a, [1])

@test
def dropWhileRight03(): Bool & Impure =
    let a = Array.dropWhileRight(i -> i > 3, [4]);
    Array.sameElements(a, []: Array[Int32])

@test
def dropWhileRight04(): Bool & Impure =
    let a = Array.dropWhileRight(i -> i > 3, [1,2]);
    Array.sameElements(a, [1,2])

@test
def dropWhileRight05(): Bool & Impure =
    let a = Array.dropWhileRight(i -> i > 3, [1,5]);
    Array.sameElements(a, [1])

@test
def dropWhileRight06(): Bool & Impure =
    let a = Array.dropWhileRight(i -> i > 3, [5,1]);
    Array.sameElements(a, [5,1])

@test
def dropWhileRight07(): Bool & Impure =
    let a = Array.dropWhileRight(i -> i > 3, [5,8]);
    Array.sameElements(a, []: Array[Int32])

@test
def dropWhileRight08(): Bool & Impure =
    let a = Array.dropWhileRight(i -> i > 3, [4, 6, -3, 11, -5, 1, 2, 16, 7, 1, 7]);
    Array.sameElements(a, [4, 6, -3, 11, -5, 1, 2, 16, 7, 1])

/////////////////////////////////////////////////////////////////////////////
// take                                                                    //
/////////////////////////////////////////////////////////////////////////////

@test
def take01(): Bool & Impure =
    let a = Array.take(-1, []: Array[Int32]);
    Array.sameElements(a, []: Array[Int32])

@test
def take02(): Bool & Impure =
    let a = Array.take(0, []: Array[Int32]);
    Array.sameElements(a, []: Array[Int32])

@test
def take03(): Bool & Impure =
    let a = Array.take(1, []: Array[Int32]);
    Array.sameElements(a, []: Array[Int32])

@test
def take04(): Bool & Impure =
    let a = Array.take(-1, [1]);
    Array.sameElements(a, []: Array[Int32])

@test
def take05(): Bool & Impure =
    let a = Array.take(0, [1]);
    Array.sameElements(a, []: Array[Int32])

@test
def take06(): Bool & Impure =
    let a = Array.take(1, [1]);
    Array.sameElements(a, [1])

@test
def take07(): Bool & Impure =
    let a = Array.take(2, [1]);
    Array.sameElements(a, [1])

@test
def take08(): Bool & Impure =
    let a = Array.take(0, [1,2]);
    Array.sameElements(a, []: Array[Int32])

@test
def take09(): Bool & Impure =
    let a = Array.take(1, [1,2]);
    Array.sameElements(a, [1])

@test
def take10(): Bool & Impure =
    let a = Array.take(2, [1,2]);
    Array.sameElements(a, [1,2])

@test
def take11(): Bool & Impure =
    let a = Array.take(2, [1,2,3,4,5,6]);
    Array.sameElements(a, [1,2])

@test
def take12(): Bool & Impure =
    let a = Array.take(4, [1,2,3,4,5,6]);
    Array.sameElements(a, [1,2,3,4])

/////////////////////////////////////////////////////////////////////////////
// takeLeft                                                                //
/////////////////////////////////////////////////////////////////////////////

@test
def takeLeft01(): Bool & Impure =
    let a = Array.takeLeft(-1, []: Array[Int32]);
    Array.sameElements(a, []: Array[Int32])

@test
def takeLeft02(): Bool & Impure =
    let a = Array.takeLeft(0, []: Array[Int32]);
    Array.sameElements(a, []: Array[Int32])

@test
def takeLeft03(): Bool & Impure =
    let a = Array.takeLeft(1, []: Array[Int32]);
    Array.sameElements(a, []: Array[Int32])

@test
def takeLeft04(): Bool & Impure =
    let a = Array.takeLeft(-1, [1]);
    Array.sameElements(a, []: Array[Int32])

@test
def takeLeft05(): Bool & Impure =
    let a = Array.takeLeft(0, [1]);
    Array.sameElements(a, []: Array[Int32])

@test
def takeLeft06(): Bool & Impure =
    let a = Array.takeLeft(1, [1]);
    Array.sameElements(a, [1])

@test
def takeLeft07(): Bool & Impure =
    let a = Array.takeLeft(2, [1]);
    Array.sameElements(a, [1])

@test
def takeLeft08(): Bool & Impure =
    let a = Array.takeLeft(0, [1,2]);
    Array.sameElements(a, []: Array[Int32])

@test
def takeLeft09(): Bool & Impure =
    let a = Array.takeLeft(1, [1,2]);
    Array.sameElements(a, [1])

@test
def takeLeft10(): Bool & Impure =
    let a = Array.takeLeft(2, [1,2]);
    Array.sameElements(a, [1,2])

@test
def takeLeft11(): Bool & Impure =
    let a = Array.takeLeft(2, [1,2,3,4,5,6]);
    Array.sameElements(a, [1,2])

@test
def takeLeft12(): Bool & Impure =
    let a = Array.takeLeft(4, [1,2,3,4,5,6]);
    Array.sameElements(a, [1,2,3,4])

/////////////////////////////////////////////////////////////////////////////
// takeRight                                                               //
/////////////////////////////////////////////////////////////////////////////

@test
def takeRight01(): Bool & Impure =
    let a = Array.takeRight(-1, []: Array[Int32]);
    Array.sameElements(a, []: Array[Int32])

@test
def takeRight02(): Bool & Impure =
    let a = Array.takeRight(0, []: Array[Int32]);
    Array.sameElements(a, []: Array[Int32])

@test
def takeRight03(): Bool & Impure =
    let a = Array.takeRight(1, []: Array[Int32]);
    Array.sameElements(a, []: Array[Int32])

@test
def takeRight04(): Bool & Impure =
    let a = Array.takeRight(-1, [1]);
    Array.sameElements(a, []: Array[Int32])

@test
def takeRight05(): Bool & Impure =
    let a = Array.takeRight(0, [1]);
    Array.sameElements(a, []: Array[Int32])

@test
def takeRight06(): Bool & Impure =
    let a = Array.takeRight(1, [1]);
    Array.sameElements(a, [1])

@test
def takeRight07(): Bool & Impure =
    let a = Array.takeRight(2, [1]);
    Array.sameElements(a, [1])

@test
def takeRight08(): Bool & Impure =
    let a = Array.takeRight(0, [1,2]);
    Array.sameElements(a, []: Array[Int32])

@test
def takeRight09(): Bool & Impure =
    let a = Array.takeRight(1, [1,2]);
    Array.sameElements(a, [2])

@test
def takeRight10(): Bool & Impure =
    let a = Array.takeRight(2, [1,2]);
    Array.sameElements(a, [1,2])

@test
def takeRight11(): Bool & Impure =
    let a = Array.takeRight(2, [1,2,3,4,5,6]);
    Array.sameElements(a, [5,6])

@test
def takeRight12(): Bool & Impure =
    let a = Array.takeRight(4, [1,2,3,4,5,6]);
    Array.sameElements(a, [3,4,5,6])

/////////////////////////////////////////////////////////////////////////////
// takeWhile                                                               //
/////////////////////////////////////////////////////////////////////////////

@test
def takeWhile01(): Bool & Impure =
    let a = Array.takeWhile(i -> i > 3, []: Array[Int32]);
    Array.sameElements(a, []: Array[Int32])

@test
def takeWhile02(): Bool & Impure =
    let a = Array.takeWhile(i -> i > 3, [1]);
    Array.sameElements(a, []: Array[Int32])

@test
def takeWhile03(): Bool & Impure =
    let a = Array.takeWhile(i -> i > 3, [4]);
    Array.sameElements(a, [4])

@test
def takeWhile04(): Bool & Impure =
    let a = Array.takeWhile(i -> i > 3, [1,2]);
    Array.sameElements(a, []: Array[Int32])

@test
def takeWhile05(): Bool & Impure =
    let a = Array.takeWhile(i -> i > 3, [1,5]);
    Array.sameElements(a, []: Array[Int32])

@test
def takeWhile06(): Bool & Impure =
    let a = Array.takeWhile(i -> i > 3, [5,1]);
    Array.sameElements(a, [5])

@test
def takeWhile07(): Bool & Impure =
    let a = Array.takeWhile(i -> i > 3, [5,8]);
    Array.sameElements(a, [5,8])

@test
def takeWhile08(): Bool & Impure =
    let a = Array.takeWhile(i -> i > 3, [4, 6, -3, 11, -5, 1, 2, 16, 7, 1, 7]);
    Array.sameElements(a, [4, 6])

/////////////////////////////////////////////////////////////////////////////
// takeWhileLeft                                                           //
/////////////////////////////////////////////////////////////////////////////

@test
def takeWhileLeft01(): Bool & Impure =
    let a = Array.takeWhileLeft(i -> i > 3, []: Array[Int32]);
    Array.sameElements(a, []: Array[Int32])

@test
def takeWhileLeft02(): Bool & Impure =
    let a = Array.takeWhileLeft(i -> i > 3, [1]);
    Array.sameElements(a, []: Array[Int32])

@test
def takeWhileLeft03(): Bool & Impure =
    let a = Array.takeWhileLeft(i -> i > 3, [4]);
    Array.sameElements(a, [4])

@test
def takeWhileLeft04(): Bool & Impure =
    let a = Array.takeWhileLeft(i -> i > 3, [1,2]);
    Array.sameElements(a, []: Array[Int32])

@test
def takeWhileLeft05(): Bool & Impure =
    let a = Array.takeWhileLeft(i -> i > 3, [1,5]);
    Array.sameElements(a, []: Array[Int32])

@test
def takeWhileLeft06(): Bool & Impure =
    let a = Array.takeWhileLeft(i -> i > 3, [5,1]);
    Array.sameElements(a, [5])

@test
def takeWhileLeft07(): Bool & Impure =
    let a = Array.takeWhileLeft(i -> i > 3, [5,8]);
    Array.sameElements(a, [5,8])

@test
def takeWhileLeft08(): Bool & Impure =
    let a = Array.takeWhileLeft(i -> i > 3, [4, 6, -3, 11, -5, 1, 2, 16, 7, 1, 7]);
    Array.sameElements(a, [4, 6])

/////////////////////////////////////////////////////////////////////////////
// takeWhileRight                                                          //
/////////////////////////////////////////////////////////////////////////////

@test
def takeWhileRight01(): Bool & Impure =
    let a = Array.takeWhileRight(i -> i > 3, []: Array[Int32]);
    Array.sameElements(a, []: Array[Int32])

@test
def takeWhileRight02(): Bool & Impure =
    let a = Array.takeWhileRight(i -> i > 3, [1]);
    Array.sameElements(a, []: Array[Int32])

@test
def takeWhileRight03(): Bool & Impure =
    let a = Array.takeWhileRight(i -> i > 3, [4]);
    Array.sameElements(a, [4])

@test
def takeWhileRight04(): Bool & Impure =
    let a = Array.takeWhileRight(i -> i > 3, [1,2]);
    Array.sameElements(a, []: Array[Int32])

@test
def takeWhileRight05(): Bool & Impure =
    let a = Array.takeWhileRight(i -> i > 3, [1,5]);
    Array.sameElements(a, [5])

@test
def takeWhileRight06(): Bool & Impure =
    let a = Array.takeWhileRight(i -> i > 3, [5,1]);
    Array.sameElements(a, []: Array[Int32])

@test
def takeWhileRight07(): Bool & Impure =
    let a = Array.takeWhileRight(i -> i > 3, [5,8]);
    Array.sameElements(a, [5,8])

@test
def takeWhileRight08(): Bool & Impure =
    let a = Array.takeWhileRight(i -> i > 3, [4, 6, -3, 11, -5, 1, 2, 16, 7, 1, 7]);
    Array.sameElements(a, [7])

/////////////////////////////////////////////////////////////////////////////
// groupBy                                                                 //
/////////////////////////////////////////////////////////////////////////////

@test
def groupBy01(): Bool & Impure =
    let arr = Array.groupBy((a, b) -> a > 3 or b > 8, []: Array[Int32]);
    let xs = Array.toList(arr) |> List.map(Array.toList);
    xs == Nil

@test
def groupBy02(): Bool & Impure =
    let arr = Array.groupBy((a, b) -> a > 3 or b > 8, [1]);
    let xs = Array.toList(arr) |> List.map(Array.toList);
    xs == (1 :: Nil) :: Nil

@test
def groupBy03(): Bool & Impure =
    let arr = Array.groupBy((a, b) -> a > 3 or b > 8, [1,4]);
    let xs = Array.toList(arr) |> List.map(Array.toList);
    xs == (1 :: Nil) :: (4 :: Nil) :: Nil

@test
def groupBy04(): Bool & Impure =
    let arr = Array.groupBy((a, b) -> a > 3 or b > 8, [1,9]);
    let xs = Array.toList(arr) |> List.map(Array.toList);
    xs == (1 :: 9 :: Nil) :: Nil

@test
def groupBy05(): Bool & Impure =
    let arr = Array.groupBy((a, b) -> a > 3 or b > 8, [1, 4, 7, 6, 9, 2, 4, 4, 8, 16]);
    let xs = Array.toList(arr) |> List.map(Array.toList);
    xs == (1 :: 9 :: 16 :: Nil) :: (4 :: 7 :: 6 :: 4 :: 4 :: 8 :: Nil) :: (2 :: Nil) :: Nil

@test
def groupBy06(): Bool & Impure =
    let arr = Array.groupBy((a, b) -> a > -6 or a*b >= 0, [-1, -11, 4, -11, 0, 8, 2, 1, -3, -24]);
    let xs = Array.toList(arr) |> List.map(Array.toList);
    xs == (-1 :: -11 :: -11 :: 0 :: -3 :: -24 :: Nil) :: (4 :: 8 :: 2 :: 1 :: Nil) :: Nil

@test
def groupBy07(): Bool & Impure =
    let arr = Array.groupBy((a, b) -> a < 0 or (a > 10 or (b > 10 or a == b)), [-5, 6, 11, 8, 8, -11, -1, 0, 4, -1]);
    let xs = Array.toList(arr) |> List.map(Array.toList);
    xs == (-5 :: 11 :: -11 :: -1 :: -1 :: Nil) :: (6 :: Nil) :: (8 :: 8 :: Nil) :: (0 :: Nil) :: (4 :: Nil) :: Nil

/////////////////////////////////////////////////////////////////////////////
// zip                                                                     //
/////////////////////////////////////////////////////////////////////////////

@test
def zip01(): Bool & Impure =
    let a = Array.zip([]: Array[Int32], []: Array[Int32]);
    Array.sameElements(a, []: Array[(Int32,Int32)])

@test
def zip02(): Bool & Impure =
    let a = Array.zip([1], []: Array[Int32]);
    Array.sameElements(a, []: Array[(Int32,Int32)])

@test
def zip03(): Bool & Impure =
    let a = Array.zip([]: Array[Int32], [2]);
    Array.sameElements(a, []: Array[(Int32,Int32)])

@test
def zip04(): Bool & Impure =
    let a = Array.zip([1], [2]);
    Array.sameElements(a, [(1, 2)])

@test
def zip05(): Bool & Impure =
    let a = Array.zip([1,3], [2,4]);
    Array.sameElements(a, [(1, 2), (3, 4)])

@test
def zip06(): Bool & Impure =
    let a = Array.zip([1,3,5], [2,4,6]);
    Array.sameElements(a, [(1, 2), (3, 4), (5, 6)])

@test
def zip07(): Bool & Impure =
    let a = Array.zip([1,3,5,7], [2,4,6,8]);
    Array.sameElements(a, [(1, 2), (3, 4), (5, 6), (7, 8)])

/////////////////////////////////////////////////////////////////////////////
// zipWith                                                                 //
/////////////////////////////////////////////////////////////////////////////

@test
def zipWith01(): Bool & Impure =
    let arr = Array.zipWith((a, b) -> if (b) a+1 else a, []: Array[Int32], []: Array[Bool]);
    Array.sameElements(arr, []: Array[Int32])

@test
def zipWith02(): Bool & Impure =
    let arr = Array.zipWith((a, b) -> if (b) a+1 else a, [1], []: Array[Bool]);
    Array.sameElements(arr, []: Array[Int32])

@test
def zipWith03(): Bool & Impure =
    let arr = Array.zipWith((a, b) -> if (b) a+1 else a, []: Array[Int32], [true]);
    Array.sameElements(arr, []: Array[Int32])

@test
def zipWith04(): Bool & Impure =
    let arr = Array.zipWith((a, b) -> if (b) a+1 else a, [1], [true]);
    Array.sameElements(arr, [2])

@test
def zipWith05(): Bool & Impure =
    let arr = Array.zipWith((a, b) -> if (b) a+1 else a, [1], [false]);
    Array.sameElements(arr, [1])

@test
def zipWith06(): Bool & Impure =
    let arr = Array.zipWith((a, b) -> if (b) a+1 else a,
                    [1, 2, 3, 4, 5, 6, 7, 8],
                    [false, true, true, false, false, true, true, true]);
    Array.sameElements(arr, [1, 3, 4, 4, 5, 7, 8, 9])

/////////////////////////////////////////////////////////////////////////////
// unzip                                                                   //
/////////////////////////////////////////////////////////////////////////////

@test
def unzip01(): Bool & Impure =
    let (a,b) = Array.unzip([]: Array[(Unit, Unit)]);
    Array.sameElements(a, []) and Array.sameElements(b, [])

@test
def unzip02(): Bool & Impure =
    let (a,b) = Array.unzip([(1, true)]);
    Array.sameElements(a, [1]) and Array.sameElements(b, [true])

@test
def unzip03(): Bool & Impure =
    let (a,b) = Array.unzip([(1, true), (2, true)]);
    Array.sameElements(a, [1,2])  and Array.sameElements(b, [true,true])

@test
def unzip04(): Bool & Impure =
    let (a,b) = Array.unzip([(1, true), (2, true), (3, false)]);
    Array.sameElements(a, [1,2,3]) and Array.sameElements(b, [true,true,false])

/////////////////////////////////////////////////////////////////////////////
// map2                                                                    //
/////////////////////////////////////////////////////////////////////////////

@test
def map201(): Bool & Impure =
    let arr = Array.map2((a, b) -> if (b) a+1 else a, [], []);
    Array.sameElements(arr, []: Array[Int32])

@test
def map202(): Bool & Impure =
    let arr = Array.map2((a, b) -> if (b) a+1 else a, [1], []);
    Array.sameElements(arr, []: Array[Int32])

@test
def map203(): Bool & Impure =
    let arr = Array.map2((a, b) -> if (b) a+1 else a, [], [true]);
    Array.sameElements(arr, []: Array[Int32])

@test
def map204(): Bool & Impure =
    let arr = Array.map2((a, b) -> if (b) a+1 else a, [1], [true]);
    Array.sameElements(arr, [2])

@test
def map205(): Bool & Impure =
    let arr = Array.map2((a, b) -> if (b) a+1 else a, [1], [false]);
    Array.sameElements(arr, [1])

@test
def map206(): Bool & Impure =
    let arr = Array.map2((a, b) -> if (b) a+1 else a, [1,2,3,4,5,6,7,8], [false,true,true,false,false,true,true,true]);
    Array.sameElements(arr, [1,3,4,4,5,7,8,9])

/////////////////////////////////////////////////////////////////////////////
// flatMap2                                                                //
/////////////////////////////////////////////////////////////////////////////

@test
def flatMap201(): Bool & Impure =
    let arr = Array.flatMap2((a, b) -> if (b) Array.repeat(a, a) else Array.repeat(a+1, a+1), [], []);
    Array.sameElements(arr, []: Array[Int32])

@test
def flatMap202(): Bool & Impure =
    let arr = Array.flatMap2((a, b) -> if (b) Array.repeat(a, a) else Array.repeat(a+1, a+1), [1], []);
    Array.sameElements(arr, []: Array[Int32])

@test
def flatMap203(): Bool & Impure =
    let arr = Array.flatMap2((a, b) -> if (b) Array.repeat(a, a) else Array.repeat(a+1, a+1), [], [true]);
    Array.sameElements(arr, []: Array[Int32])

@test
def flatMap204(): Bool & Impure =
    let arr = Array.flatMap2((a, b) -> if (b) Array.repeat(a, a) else Array.repeat(a+1, a+1), [2], [true]);
    Array.sameElements(arr, [2,2])

@test
def flatMap205(): Bool & Impure =
    let arr = Array.flatMap2((a, b) -> if (b) Array.repeat(a, a) else Array.repeat(a+1, a+1), [2], [false]);
    Array.sameElements(arr, [3,3,3])

@test
def flatMap206(): Bool & Impure =
    let arr = Array.flatMap2((a, b) -> if (b) Array.repeat(a, a) else Array.repeat(a+1, a+1), [1,2,3,4,5],
                   [false,true,true,false,false]);
    Array.sameElements(arr, [2, 2, 2, 2, 3, 3, 3, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6])

/////////////////////////////////////////////////////////////////////////////
// fold2                                                                   //
/////////////////////////////////////////////////////////////////////////////

@test
def fold201(): Bool & Impure = Array.fold2((c, a, b) -> if (b) a+c else a*c, 4, []: Array[Int32], []: Array[Bool]) == 4

@test
def fold202(): Bool & Impure = Array.fold2((c, a, b) -> if (b) a+c else a*c, 4, [1], []) == 4

@test
def fold203(): Bool & Impure = Array.fold2((c, a, b) -> if (b) a+c else a*c, 4, [], [true]) == 4

@test
def fold204(): Bool & Impure = Array.fold2((c, a, b) -> if (b) a+c else a*c, 4, [2], [true]) == 6

@test
def fold205(): Bool & Impure = Array.fold2((c, a, b) -> if (b) a+c else a*c, 4, [2], [false]) == 8

@test
def fold206(): Bool & Impure = Array.fold2((c, a, b) -> if (b) a+c else a*c, 4, [3,2], [true,true]) == 9

@test
def fold207(): Bool & Impure = Array.fold2((c, a, b) -> if (b) a+c else a*c, 4, [3,2], [true,false]) == 14

@test
def fold208(): Bool & Impure = Array.fold2((c, a, b) -> if (b) a+c else a*c, 4, [3,2], [false,true]) == 14

@test
def fold209(): Bool & Impure = Array.fold2((c, a, b) -> if (b) a+c else a*c, 4, [3,2], [false,false]) == 24

@test
def fold210(): Bool & Impure = Array.fold2((c, a, b) -> if (b) a+c else a*c, 4, [6,-4,3,2],
                    [true,false,false,true]) == -118

/////////////////////////////////////////////////////////////////////////////
// foldLeft2                                                               //
/////////////////////////////////////////////////////////////////////////////

@test
def foldLeft201(): Bool & Impure = Array.foldLeft2((c, a, b) -> if (b) a+c else a*c, 4, []: Array[Int32], []: Array[Bool]) == 4

@test
def foldLeft202(): Bool & Impure = Array.foldLeft2((c, a, b) -> if (b) a+c else a*c, 4, [1], []: Array[Bool]) == 4

@test
def foldLeft203(): Bool & Impure = Array.foldLeft2((c, a, b) -> if (b) a+c else a*c, 4, []: Array[Int32], [true]) == 4

@test
def foldLeft204(): Bool & Impure = Array.foldLeft2((c, a, b) -> if (b) a+c else a*c, 4, [2], [true]) == 6

@test
def foldLeft205(): Bool & Impure = Array.foldLeft2((c, a, b) -> if (b) a+c else a*c, 4, [2], [false]) == 8

@test
def foldLeft206(): Bool & Impure = Array.foldLeft2((c, a, b) -> if (b) a+c else a*c, 4, [3,2], [true,true]) == 9

@test
def foldLeft207(): Bool & Impure = Array.foldLeft2((c, a, b) -> if (b) a+c else a*c, 4, [3,2], [true,false]) == 14

@test
def foldLeft208(): Bool & Impure = Array.foldLeft2((c, a, b) -> if (b) a+c else a*c, 4, [3,2], [false,true]) == 14

@test
def foldLeft209(): Bool & Impure = Array.foldLeft2((c, a, b) -> if (b) a+c else a*c, 4, [3,2], [false,false]) == 24

@test
def foldLeft210(): Bool & Impure = Array.foldLeft2((c, a, b) -> if (b) a+c else a*c, 4, [6,-4,3,2],
                        [true,false,false,true]) == -118

/////////////////////////////////////////////////////////////////////////////
// foldRight2                                                              //
/////////////////////////////////////////////////////////////////////////////

@test
def foldRight201(): Bool & Impure = Array.foldRight2((a, b, c) -> if (b) a+c else a*c, 4, []: Array[Int32], []: Array[Bool]) == 4

@test
def foldRight202(): Bool & Impure = Array.foldRight2((a, b, c) -> if (b) a+c else a*c, 4, [1], []: Array[Bool]) == 4

@test
def foldRight203(): Bool & Impure = Array.foldRight2((a, b, c) -> if (b) a+c else a*c, 4, []: Array[Int32], [true]) == 4

@test
def foldRight204(): Bool & Impure = Array.foldRight2((a, b, c) -> if (b) a+c else a*c, 4, [2], [true]) == 6

@test
def foldRight205(): Bool & Impure = Array.foldRight2((a, b, c) -> if (b) a+c else a*c, 4, [2], [false]) == 8

@test
def foldRight206(): Bool & Impure = Array.foldRight2((a, b, c) -> if (b) a+c else a*c, 4, [3,2], [true,true]) == 9

@test
def foldRight207(): Bool & Impure = Array.foldRight2((a, b, c) -> if (b) a+c else a*c, 4, [3,2], [true,false]) == 11

@test
def foldRight208(): Bool & Impure = Array.foldRight2((a, b, c) -> if (b) a+c else a*c, 4, [3,2], [false,true]) == 18

@test
def foldRight209(): Bool & Impure = Array.foldRight2((a, b, c) -> if (b) a+c else a*c, 4, [3,2], [false,false]) == 24

@test
def foldRight210(): Bool & Impure = Array.foldRight2((a, b, c) -> if (b) a+c else a*c, 4, [6,-4,3,2],
                         [true,false,false,true]) == -66

/////////////////////////////////////////////////////////////////////////////
// filterMap                                                               //
/////////////////////////////////////////////////////////////////////////////

@test
def filterMap01(): Bool & Impure =
    let a = Array.filterMap(i -> if (i % 2 == 0) Some(i/2) else None, []: Array[Int32]);
    Array.sameElements(a, []: Array[Int32])

@test
def filterMap02(): Bool & Impure =
    let a = Array.filterMap(i -> if (i % 2 == 0) Some(i/2) else None, [1]);
    Array.sameElements(a, []: Array[Int32])

@test
def filterMap03(): Bool & Impure =
    let a = Array.filterMap(i -> if (i % 2 == 0) Some(i/2) else None, [2]);
    Array.sameElements(a, [1])

@test
def filterMap04(): Bool & Impure =
    let a = Array.filterMap(i -> if (i % 2 == 0) Some(i/2) else None, [1,3]);
    Array.sameElements(a, []: Array[Int32])

@test
def filterMap05(): Bool & Impure =
    let a = Array.filterMap(i -> if (i % 2 == 0) Some(i/2) else None, [1,4]);
    Array.sameElements(a, [2])

@test
def filterMap06(): Bool & Impure =
    let a = Array.filterMap(i -> if (i % 2 == 0) Some(i/2) else None, [6,-1]);
    Array.sameElements(a, [3])

@test
def filterMap07(): Bool & Impure =
    let a = Array.filterMap(i -> if (i % 2 == 0) Some(i/2) else None, [8,6]);
    Array.sameElements(a, [4,3])

@test
def filterMap08(): Bool & Impure =
    let a = Array.filterMap(i -> if (i % 2 == 0) Some(i/2) else None, [0,1,2,3,4,5,10]);
    Array.sameElements(a, [0,1,2,5])

/////////////////////////////////////////////////////////////////////////////
// findMap                                                                 //
/////////////////////////////////////////////////////////////////////////////

@test
def findMap01(): Bool & Impure = Array.findMap(i -> if (i % 2 == 0) Some(i/2) else None, []: Array[Int32]) == None

@test
def findMap02(): Bool & Impure = Array.findMap(i -> if (i % 2 == 0) Some(i/2) else None, [1]) == None

@test
def findMap03(): Bool & Impure = Array.findMap(i -> if (i % 2 == 0) Some(i/2) else None, [2]) == Some(1)

@test
def findMap04(): Bool & Impure = Array.findMap(i -> if (i % 2 == 0) Some(i/2) else None, [1,3]) == None

@test
def findMap05(): Bool & Impure = Array.findMap(i -> if (i % 2 == 0) Some(i/2) else None, [1,4]) == Some(2)

@test
def findMap06(): Bool & Impure = Array.findMap(i -> if (i % 2 == 0) Some(i/2) else None, [6,-1]) == Some(3)

@test
def findMap07(): Bool & Impure = Array.findMap(i -> if (i % 2 == 0) Some(i/2) else None, [8,6]) == Some(4)

@test
def findMap08(): Bool & Impure = Array.findMap(i -> if (i % 2 == 0) Some(i/2) else None, [0,1,2,3,4,5,10]) == Some(0)

/////////////////////////////////////////////////////////////////////////////
// toSet                                                                   //
/////////////////////////////////////////////////////////////////////////////

@test
def toSet01(): Bool & Impure = Array.toSet([]: Array[Int32]) == Set#{}

@test
def toSet02(): Bool & Impure = Array.toSet([1]) == Set#{1}

@test
def toSet03(): Bool & Impure = Array.toSet([1,2]) == Set#{1, 2}

@test
def toSet04(): Bool & Impure = Array.toSet([1,1]) == Set#{1}

@test
def toSet05(): Bool & Impure = Array.toSet([1,2,3]) == Set#{1, 2, 3}

@test
def toSet06(): Bool & Impure = Array.toSet([1,2,1]) == Set#{1, 2}

@test
def toSet07(): Bool & Impure = Array.toSet([1,1,2]) == Set#{1, 2}

@test
def toSet08(): Bool & Impure = Array.toSet([2,1,2,3,4,5,6,3,3]) == Set#{1, 2, 3, 4, 5, 6}

/////////////////////////////////////////////////////////////////////////////
// toMap                                                                   //
/////////////////////////////////////////////////////////////////////////////

@test
def toMap01(): Bool & Impure = Array.toMap([]: Array[(Int32, Bool)]) == Map#{}

@test
def toMap02(): Bool & Impure = Array.toMap([(1, true)]) == Map#{1 -> true}

@test
def toMap03(): Bool & Impure = Array.toMap([(1, true), (2, false)]) == Map#{1 -> true, 2 -> false}

@test
def toMap04(): Bool & Impure = Array.toMap([(1, true), (1, false)]) == Map#{1 -> true}

/////////////////////////////////////////////////////////////////////////////
// findIndexOf                                                             //
/////////////////////////////////////////////////////////////////////////////

@test
def findIndexOf01(): Bool & Impure = Array.findIndexOf(i -> i > 2, []: Array[Int32]) == None

@test
def findIndexOf02(): Bool & Impure = Array.findIndexOf(i -> i > 2, [1]) == None

@test
def findIndexOf03(): Bool & Impure = Array.findIndexOf(i -> i > 2, [3]) == Some(0)

@test
def findIndexOf04(): Bool & Impure = Array.findIndexOf(i -> i > 2, [1,2]) == None

@test
def findIndexOf05(): Bool & Impure = Array.findIndexOf(i -> i > 2, [6,-6]) == Some(0)

@test
def findIndexOf06(): Bool & Impure = Array.findIndexOf(i -> i > 2, [-6,6]) == Some(1)

@test
def findIndexOf07(): Bool & Impure = Array.findIndexOf(i -> i > 2, [6,7]) == Some(0)

/////////////////////////////////////////////////////////////////////////////
// findIndexOfLeft                                                         //
/////////////////////////////////////////////////////////////////////////////

@test
def findIndexOfLeft01(): Bool & Impure = Array.findIndexOfLeft(i -> i > 2, []: Array[Int32]) == None

@test
def findIndexOfLeft02(): Bool & Impure = Array.findIndexOfLeft(i -> i > 2, [1]) == None

@test
def findIndexOfLeft03(): Bool & Impure = Array.findIndexOfLeft(i -> i > 2, [3]) == Some(0)

@test
def findIndexOfLeft04(): Bool & Impure = Array.findIndexOfLeft(i -> i > 2, [1,2]) == None

@test
def findIndexOfLeft05(): Bool & Impure = Array.findIndexOfLeft(i -> i > 2, [6,-6]) == Some(0)

@test
def findIndexOfLeft06(): Bool & Impure = Array.findIndexOfLeft(i -> i > 2, [-6,6]) == Some(1)

@test
def findIndexOfLeft07(): Bool & Impure = Array.findIndexOfLeft(i -> i > 2, [6,7]) == Some(0)

/////////////////////////////////////////////////////////////////////////////
// findIndexOfRight                                                        //
/////////////////////////////////////////////////////////////////////////////

@test
def findIndexOfRight01(): Bool & Impure = Array.findIndexOfRight(i -> i > 2, []: Array[Int32]) == None

@test
def findIndexOfRight02(): Bool & Impure = Array.findIndexOfRight(i -> i > 2, [1]) == None

@test
def findIndexOfRight03(): Bool & Impure = Array.findIndexOfRight(i -> i > 2, [3]) == Some(0)

@test
def findIndexOfRight04(): Bool & Impure = Array.findIndexOfRight(i -> i > 2, [1,2]) == None

@test
def findIndexOfRight05(): Bool & Impure = Array.findIndexOfRight(i -> i > 2, [6,-6]) == Some(0)

@test
def findIndexOfRight06(): Bool & Impure = Array.findIndexOfRight(i -> i > 2, [-6,6]) == Some(1)

@test
def findIndexOfRight07(): Bool & Impure = Array.findIndexOfRight(i -> i > 2, [6,7]) == Some(1)

/////////////////////////////////////////////////////////////////////////////
// findIndices                                                             //
/////////////////////////////////////////////////////////////////////////////

@test
def findIndices01(): Bool & Impure =
    let a = Array.findIndices(i -> i > 2, []: Array[Int32]);
    Array.sameElements(a, []: Array[Int32])

@test
def findIndices02(): Bool & Impure =
    let a = Array.findIndices(i -> i > 2, [1]);
    Array.sameElements(a, []: Array[Int32])

@test
def findIndices03(): Bool & Impure =
    let a = Array.findIndices(i -> i > 2, [3]);
    Array.sameElements(a, [0])

@test
def findIndices04(): Bool & Impure =
    let a = Array.findIndices(i -> i > 2, [1,2]);
    Array.sameElements(a, [] : Array[Int32])

@test
def findIndices05(): Bool & Impure =
    let a = Array.findIndices(i -> i > 2, [6,-6]);
    Array.sameElements(a, [0])

@test
def findIndices06(): Bool & Impure =
    let a = Array.findIndices(i -> i > 2, [-6,6]);
    Array.sameElements(a, [1])

@test
def findIndices07(): Bool & Impure =
    let a = Array.findIndices(i -> i > 2, [6,7]);
    Array.sameElements(a, [0,1])

/////////////////////////////////////////////////////////////////////////////
// init                                                                    //
/////////////////////////////////////////////////////////////////////////////

@test
def init01(): Bool & Impure =
    let a = Array.init(x -> x, 0);
    Array.sameElements(a, []: Array[Int32])

@test
def init02(): Bool & Impure =
    let a = Array.init(x -> x, -1);
    Array.sameElements(a, []: Array[Int32])

@test
def init03(): Bool & Impure =
    let a = Array.init(x -> x, 1);
    Array.sameElements(a, [0])

@test
def init04(): Bool & Impure =
    let a = Array.init(x -> x, 2);
    Array.sameElements(a, [0,1])

@test
def init05(): Bool & Impure =
    let a = Array.init(x -> x, 50);
    Array.length(a) == 50

/////////////////////////////////////////////////////////////////////////////
// init2                                                                   //
/////////////////////////////////////////////////////////////////////////////

@test
def init201(): Bool & Impure =
    let (a,b) = Array.init2(x -> (x,true), 0);
    Array.sameElements(a, []: Array[Int32]) and Array.sameElements(b, []: Array[Bool])

@test
def init202(): Bool & Impure =
    let (a,b) = Array.init2(x -> (x,true), -1);
    Array.sameElements(a, []: Array[Int32]) and Array.sameElements(b, []: Array[Bool])

@test
def init203(): Bool & Impure =
    let (a,b) = Array.init2(x -> (x,true), 1);
    Array.sameElements(a, [0]) and Array.sameElements(b, [true])

@test
def init204(): Bool & Impure =
    let (a,b) = Array.init2(x -> (x,true), 2);
    Array.sameElements(a, [0,1]) and Array.sameElements(b, [true, true])

@test
def init205(): Bool & Impure =
    let (a,b) = Array.init2(x -> (x,true), 50);
    Array.length(a) == 50 and Array.length(b) == 50


/////////////////////////////////////////////////////////////////////////////
// sameElements                                                            //
/////////////////////////////////////////////////////////////////////////////

@test
def sameElements01(): Bool & Impure =
    let a = []: Array[Int32];
    let b = []: Array[Int32];
    Array.sameElements(a,b) == true

@test
def sameElements02(): Bool & Impure =
    let a = [1]: Array[Int32];
    let b = []: Array[Int32];
    Array.sameElements(a,b) == false

@test
def sameElements03(): Bool & Impure =
    let a = []: Array[Int32];
    let b = [1]: Array[Int32];
    Array.sameElements(a,b) == false

@test
def sameElements04(): Bool & Impure =
    let a = [1]: Array[Int32];
    let b = [1]: Array[Int32];
    Array.sameElements(a,b) == true

@test
def sameElements05(): Bool & Impure =
    let a = [1]: Array[Int32];
    let b = [0]: Array[Int32];
    Array.sameElements(a,b) == false

@test
def sameElements06(): Bool & Impure =
    let a = [1,2]: Array[Int32];
    let b = [1,2]: Array[Int32];
    Array.sameElements(a,b) == true

@test
def sameElements07(): Bool & Impure =
    let a = [1,2]: Array[Int32];
    let b = [2,1]: Array[Int32];
    Array.sameElements(a,b) == false

/////////////////////////////////////////////////////////////////////////////
// foreach                                                                 //
/////////////////////////////////////////////////////////////////////////////

@test
def foreach01(): Bool & Impure =
    let a = []: Array[Int32];
    let sb = StringBuilder.new();
    let fn = x -> if (x > 0) StringBuilder.appendChar!(sb, 'T') else StringBuilder.appendChar!(sb, 'F');
    Array.foreach(fn, a);
    StringBuilder.toString(sb) == ""

@test
def foreach02(): Bool & Impure =
    let a = [0];
    let sb = StringBuilder.new();
    let fn = x -> if (x > 0) StringBuilder.appendChar!(sb, 'T') else StringBuilder.appendChar!(sb, 'F');
    Array.foreach(fn, a);
    StringBuilder.toString(sb) == "F"

@test
def foreach03(): Bool & Impure =
    let a = [1];
    let sb = StringBuilder.new();
    let fn = x -> if (x > 0) StringBuilder.appendChar!(sb, 'T') else StringBuilder.appendChar!(sb, 'F');
    Array.foreach(fn, a);
    StringBuilder.toString(sb) == "T"

@test
def foreach04(): Bool & Impure =
    let a = [0,1];
    let sb = StringBuilder.new();
    let fn = x -> if (x > 0) StringBuilder.appendChar!(sb, 'T') else StringBuilder.appendChar!(sb, 'F');
    Array.foreach(fn, a);
    StringBuilder.toString(sb) == "FT"

/////////////////////////////////////////////////////////////////////////////
// foreachWithIndex                                                        //
/////////////////////////////////////////////////////////////////////////////

@test
def foreachWithIndex01(): Bool & Impure =
    let a = []: Array[Float32];
    let sb = StringBuilder.new();
    let fn = (_,ix) -> if (ix > 0) StringBuilder.appendChar!(sb, 'T') else StringBuilder.appendChar!(sb, 'F');
    Array.foreachWithIndex(fn, a);
    StringBuilder.toString(sb) == ""

@test
def foreachWithIndex02(): Bool & Impure =
    let a = [0.0];
    let sb = StringBuilder.new();
    let fn = (_,ix) -> if (ix > 0) StringBuilder.appendChar!(sb, 'T') else StringBuilder.appendChar!(sb, 'F');
    Array.foreachWithIndex(fn, a);
    StringBuilder.toString(sb) == "F"

@test
def foreachWithIndex03(): Bool & Impure =
    let a = [0.0, 0.1];
    let sb = StringBuilder.new();
    let fn = (_,ix) -> if (ix > 0) StringBuilder.appendChar!(sb, 'T') else StringBuilder.appendChar!(sb, 'F');
    Array.foreachWithIndex(fn, a);
    StringBuilder.toString(sb) == "FT"

@test
def foreachWithIndex04(): Bool & Impure =
    let a = [0.0, 0.1, 0.2];
    let sb = StringBuilder.new();
    let fn = (_,ix) -> if (ix > 0) StringBuilder.appendChar!(sb, 'T') else StringBuilder.appendChar!(sb, 'F');
    Array.foreachWithIndex(fn, a);
    StringBuilder.toString(sb) == "FTT"

@test
def foreachWithIndex05(): Bool & Impure =
    let a = [0.0, 0.1, 0.2, 0.3];
    let sb = StringBuilder.new();
    let fn = (_,ix) -> if (ix > 0) StringBuilder.appendChar!(sb, 'T') else StringBuilder.appendChar!(sb, 'F');
    Array.foreachWithIndex(fn, a);
    StringBuilder.toString(sb) == "FTTT"

/////////////////////////////////////////////////////////////////////////////
// foreachAccum                                                            //
/////////////////////////////////////////////////////////////////////////////

@test
def foreachAccum01(): Bool & Impure =
    let a = []: Array[Int32];
    let sb = StringBuilder.new();
    let fn = (x,st) -> {
        if (x > 0) StringBuilder.appendChar!(sb, 'T') else StringBuilder.appendChar!(sb, 'F');
        st+1
    };
    let n = Array.foreachAccum(fn, 0, a);
    StringBuilder.toString(sb) == "" and n == 0

@test
def foreachAccum02(): Bool & Impure =
    let a = [0];
    let sb = StringBuilder.new();
    let fn = (x,st) -> {
        if (x > 0) StringBuilder.appendChar!(sb, 'T') else StringBuilder.appendChar!(sb, 'F');
        st+1
    };
    let n = Array.foreachAccum(fn, 0, a);
    StringBuilder.toString(sb) == "F" and n == 1

@test
def foreachAccum03(): Bool & Impure =
    let a = [1];
    let sb = StringBuilder.new();
    let fn = (x,st) -> {
        if (x > 0) StringBuilder.appendChar!(sb, 'T') else StringBuilder.appendChar!(sb, 'F');
        st+1
    };
    let n = Array.foreachAccum(fn, 0, a);
    StringBuilder.toString(sb) == "T" and n == 1

@test
def foreachAccum04(): Bool & Impure =
    let a = [0,1];
    let sb = StringBuilder.new();
    let fn = (x,st) -> {
        if (x > 0) StringBuilder.appendChar!(sb, 'T') else StringBuilder.appendChar!(sb, 'F');
        st+1
    };
    let n = Array.foreachAccum(fn, 0, a);
    StringBuilder.toString(sb) == "FT" and n == 2

/////////////////////////////////////////////////////////////////////////////
// updateSequence                                                          //
/////////////////////////////////////////////////////////////////////////////

@test
def updateSequence01(): Bool & Impure =
     let a = Array.updateSequence(0, []: Array[Int32], []: Array[Int32]);
     Array.sameElements(a, []: Array[Int32])

@test
def updateSequence02(): Bool & Impure =
    let a = Array.updateSequence(0, [1,2], []);
    Array.sameElements(a, []: Array[Int32])

@test
def updateSequence03(): Bool & Impure =
    let a = Array.updateSequence(0, [], [1,2]);
    Array.sameElements(a, [1,2])

@test
def updateSequence04(): Bool & Impure =
    let a = Array.updateSequence(-3, [1,2,4], [1,2]);
    Array.sameElements(a, [1,2])

@test
def updateSequence05(): Bool & Impure =
    let a = Array.updateSequence(2, [1,2,4], [1,2]);
    Array.sameElements(a, [1,2])

@test
def updateSequence06(): Bool & Impure =
    let a = Array.updateSequence(0, [], [1]);
    Array.sameElements(a, [1])

@test
def updateSequence07(): Bool & Impure =
    let a = Array.updateSequence(1, [2], [1]);
    Array.sameElements(a, [1])

@test
def updateSequence08(): Bool & Impure =
    let a = Array.updateSequence(0, [2], [1]);
    Array.sameElements(a, [2])

@test
def updateSequence09(): Bool & Impure =
    let a = Array.updateSequence(0, [2,4], [1]);
    Array.sameElements(a, [2])

@test
def updateSequence10(): Bool & Impure =
    let a = Array.updateSequence(-1, [2,4], [1]);
    Array.sameElements(a, [4])

@test
def updateSequence11(): Bool & Impure =
    let a = Array.updateSequence(-1, [3,4], [1,2]);
    Array.sameElements(a, [4,2])

@test
def updateSequence12(): Bool & Impure =
    let a = Array.updateSequence(1, [3,4], [1,2]);
    Array.sameElements(a, [1,3])

@test
def updateSequence13(): Bool & Impure =
    let a = Array.updateSequence(-2, [3,4], [1,2]);
    Array.sameElements(a, [1,2])

@test
def updateSequence14(): Bool & Impure =
    let a = Array.updateSequence(2, [3,4], [1,2]);
    Array.sameElements(a, [1,2])

@test
def updateSequence15(): Bool & Impure =
    let a = Array.updateSequence(1, [3], [1,2]);
    Array.sameElements(a, [1,3])

@test
def updateSequence16(): Bool & Impure =
    let a = Array.updateSequence(0, [3,4], [1,2]);
    Array.sameElements(a, [3,4])

@test
def updateSequence17(): Bool & Impure =
    let a = Array.updateSequence(0, [4], [1, 2, 3]);
    Array.sameElements(a, [4, 2, 3])

@test
def updateSequence18(): Bool & Impure =
    let a = Array.updateSequence(1, [4], [1, 2, 3]);
    Array.sameElements(a, [1, 4, 3])

@test
def updateSequence19(): Bool & Impure =
    let a = Array.updateSequence(2, [4], [1, 2, 3]);
    Array.sameElements(a, [1, 2, 4])

@test
def updateSequence20(): Bool & Impure =
    let a = Array.updateSequence(0, [4, 5], [1, 2, 3]);
    Array.sameElements(a, [4, 5, 3])

@test
def updateSequence21(): Bool & Impure =
    let a = Array.updateSequence(1, [4, 5], [1, 2, 3]);
    Array.sameElements(a, [1, 4, 5])

@test
def updateSequence22(): Bool & Impure =
    let a = Array.updateSequence(-1, [4, 5, 6], [1, 2, 3]);
    Array.sameElements(a, [5, 6, 3])

@test
def updateSequence23(): Bool & Impure =
    let a = Array.updateSequence(0, [4, 5, 6], [1, 2, 3]);
    Array.sameElements(a, [4, 5, 6])

@test
def updateSequence24(): Bool & Impure =
    let a = Array.updateSequence(2, [14, 15, 16, 17], [1, 2, 3, 4, 5, 6, 7]);
    Array.sameElements(a, [1, 2, 14, 15, 16, 17, 7])

@test
def updateSequence25(): Bool & Impure =
    let a = Array.updateSequence(-2, [14, 15, 16, 17], [1, 2, 3, 4, 5, 6, 7]);
    Array.sameElements(a, [16, 17, 3, 4, 5, 6, 7])

@test
def updateSequence26(): Bool & Impure =
    let a = Array.updateSequence(4, [14, 15, 16, 17], [1, 2, 3, 4, 5, 6, 7]);
    Array.sameElements(a, [1, 2, 3, 4, 14, 15, 16])

@test
def updateSequence27(): Bool & Impure =
    let a = Array.updateSequence(4, [14, 15], [1, 2, 3, 4, 5, 6, 7]);
    Array.sameElements(a, [1, 2, 3, 4, 14, 15, 7])

@test
def updateSequence28(): Bool & Impure =
    let a = Array.updateSequence(-1, [-1, -2, -3, -4, -5, -6, -7, -8], [1, 2, 3, 4, 5, 6, 7]);
    Array.sameElements(a, [-2, -3, -4, -5, -6, -7, -8])

/////////////////////////////////////////////////////////////////////////////
// updateSequence!                                                   //
/////////////////////////////////////////////////////////////////////////////

@test
def updateSequence01!(): Bool & Impure =
     let a = []: Array[Int32];
     Array.updateSequence!(0, []: Array[Int32], a);
     Array.sameElements(a, []: Array[Int32])

@test
def updateSequence02!(): Bool & Impure =
    let a = []: Array[Int32];
    Array.updateSequence!(0, [1,2], a);
    Array.sameElements(a, []: Array[Int32])

@test
def updateSequence03!(): Bool & Impure =
    let a = [1,2];
    Array.updateSequence!(0, [], a);
    Array.sameElements(a, [1,2])

@test
def updateSequence04!(): Bool & Impure =
    let a = [1,2];
    Array.updateSequence!(-3, [1,2,4], a);
    Array.sameElements(a, [1,2])

@test
def updateSequence05!(): Bool & Impure =
    let a = [1,2];
    Array.updateSequence!(2, [1,2,4], a);
    Array.sameElements(a, [1,2])

@test
def updateSequence06!(): Bool & Impure =
    let a = [1];
    Array.updateSequence!(0, [], a);
    Array.sameElements(a, [1])

@test
def updateSequence07!(): Bool & Impure =
    let a = [1];
    Array.updateSequence!(1, [2], a);
    Array.sameElements(a, [1])

@test
def updateSequence08!(): Bool & Impure =
    let a = [1];
    Array.updateSequence!(0, [2], a);
    Array.sameElements(a, [2])

@test
def updateSequence09!(): Bool & Impure =
    let a = [1];
    Array.updateSequence!(0, [2,4], a);
    Array.sameElements(a, [2])

@test
def updateSequence10!(): Bool & Impure =
    let a = [1];
    Array.updateSequence!(-1, [2,4], a);
    Array.sameElements(a, [4])

@test
def updateSequence11!(): Bool & Impure =
    let a = [1,2];
    Array.updateSequence!(-1, [3,4], a);
    Array.sameElements(a, [4,2])

@test
def updateSequence12!(): Bool & Impure =
    let a = [1,2];
    Array.updateSequence!(1, [3,4], a);
    Array.sameElements(a, [1,3])

@test
def updateSequence13!(): Bool & Impure =
    let a = [1,2];
    Array.updateSequence!(-2, [3,4], a);
    Array.sameElements(a, [1,2])

@test
def updateSequence14!(): Bool & Impure =
    let a = [1,2];
    Array.updateSequence!(2, [3,4], a);
    Array.sameElements(a, [1,2])

@test
def updateSequence15!(): Bool & Impure =
    let a = [1,2];
    Array.updateSequence!(1, [3], a);
    Array.sameElements(a, [1,3])

@test
def updateSequence16!(): Bool & Impure =
    let a = [1,2];
    Array.updateSequence!(0, [3,4], a);
    Array.sameElements(a, [3,4])

@test
def updateSequence17!(): Bool & Impure =
    let a = [1, 2, 3];
    Array.updateSequence!(0, [4], a);
    Array.sameElements(a, [4, 2, 3])

@test
def updateSequence18!(): Bool & Impure =
    let a = [1, 2, 3];
    Array.updateSequence!(1, [4], a);
    Array.sameElements(a, [1, 4, 3])

@test
def updateSequence19!(): Bool & Impure =
    let a = [1, 2, 3];
    Array.updateSequence!(2, [4], a);
    Array.sameElements(a, [1, 2, 4])

@test
def updateSequence20!(): Bool & Impure =
    let a = [1, 2, 3];
    Array.updateSequence!(0, [4, 5], a);
    Array.sameElements(a, [4, 5, 3])

@test
def updateSequence21!(): Bool & Impure =
    let a = [1, 2, 3];
    Array.updateSequence!(1, [4, 5], a);
    Array.sameElements(a, [1, 4, 5])

@test
def updateSequence22!(): Bool & Impure =
    let a = [1, 2, 3];
    Array.updateSequence!(-1, [4, 5, 6], a);
    Array.sameElements(a, [5, 6, 3])

@test
def updateSequence23!(): Bool & Impure =
    let a = [1, 2, 3];
    Array.updateSequence!(0, [4, 5, 6], a);
    Array.sameElements(a, [4, 5, 6])

@test
def updateSequence24!(): Bool & Impure =
    let a = [1, 2, 3, 4, 5, 6, 7];
    Array.updateSequence!(2, [14, 15, 16, 17], a);
    Array.sameElements(a, [1, 2, 14, 15, 16, 17, 7])

@test
def updateSequence25!(): Bool & Impure =
    let a = [1, 2, 3, 4, 5, 6, 7];
    Array.updateSequence!(-2, [14, 15, 16, 17], a);
    Array.sameElements(a, [16, 17, 3, 4, 5, 6, 7])

@test
def updateSequence26!(): Bool & Impure =
    let a = [1, 2, 3, 4, 5, 6, 7];
    Array.updateSequence!(4, [14, 15, 16, 17], a);
    Array.sameElements(a, [1, 2, 3, 4, 14, 15, 16])

@test
def updateSequence27!(): Bool & Impure =
    let a = [1, 2, 3, 4, 5, 6, 7];
    Array.updateSequence!(4, [14, 15], a);
    Array.sameElements(a, [1, 2, 3, 4, 14, 15, 7])

@test
def updateSequence28!(): Bool & Impure =
    let a = [1, 2, 3, 4, 5, 6, 7];
    Array.updateSequence!(-1, [-1, -2, -3, -4, -5, -6, -7, -8], a);
    Array.sameElements(a, [-2, -3, -4, -5, -6, -7, -8])

/////////////////////////////////////////////////////////////////////////////
// formatWith                                                              //
/////////////////////////////////////////////////////////////////////////////

@test
def formatWith01(): Bool & Impure =
    let a: Array[Int32] = [];
    let s = Array.formatWith(Int32.toString, ",", a);
    s == ""

@test
def formatWith02(): Bool & Impure =
    let a = [1];
    let s = Array.formatWith(Int32.toString, ",", a);
    s == "1"

@test
def formatWith03(): Bool & Impure =
    let a = [1,2];
    let s = Array.formatWith(Int32.toString, ",", a);
    s == "1,2"

@test
def formatWith04(): Bool & Impure =
    let a = [1,2,3];
    let s = Array.formatWith(Int32.toString, ",", a);
    s == "1,2,3"

@test
def formatWith05(): Bool & Impure =
    let a = [1,2,3];
    let s = Array.formatWith(Int32.toString, "", a);
    s == "123"

@test
def formatWith06(): Bool & Impure =
    let a = [1,2,3];
    let s = Array.formatWith(Int32.toString, "..", a);
    s == "1..2..3"

/////////////////////////////////////////////////////////////////////////////
// sortWith                                                                  //
/////////////////////////////////////////////////////////////////////////////

@test
def sortWith01(): Bool & Impure =
    let a = Array.sortWith((x,y) -> if (x < y) -1 else 1, []: Array[Int32]);
    Array.sameElements(a, []: Array[Int32])

@test
def sortWith02(): Bool & Impure =
    let a = Array.sortWith((x,y) -> if (x < y) -1 else 1, [0]);
    Array.sameElements(a, [0])

@test
def sortWith03(): Bool & Impure =
    let a = Array.sortWith((x,y) -> if (x < y) -1 else 1, [0,1]);
    Array.sameElements(a, [0,1])

@test
def sortWith04(): Bool & Impure =
    let a = Array.sortWith((x,y) -> if (x < y) -1 else 1, [1,0]);
    Array.sameElements(a, [0,1])

@test
def sortWith05(): Bool & Impure =
    let a = Array.sortWith((x,y) -> if (x < y) -1 else 1, [1,1]);
    Array.sameElements(a, [1,1])

@test
def sortWith06(): Bool & Impure =
    let a = Array.sortWith((x,y) -> if (x < y) -1 else 1, [0,1,2,3,4,5]);
    Array.sameElements(a, [0,1,2,3,4,5])

@test
def sortWith07(): Bool & Impure =
    let a = Array.sortWith((x,y) -> if (x < y) -1 else 1, [5,4,3,2,1,0]);
    Array.sameElements(a, [0,1,2,3,4,5])

@test
def sortWith08(): Bool & Impure =
    let a = Array.sortWith((x,y) -> if (x < y) -1 else 1, [5,3,0,4,1,2]);
    Array.sameElements(a, [0,1,2,3,4,5])

@test
def sortWith09(): Bool & Impure =
    let a = Array.sortWith((x,y) -> if (x < y) -1 else 1, [2,3,0,4,1,2]);
    Array.sameElements(a, [0,1,2,2,3,4])

@test
def sortWith10(): Bool & Impure =
    let a = Array.sortWith((x,y) -> if (x > y) -1 else 1, [0,1,2,3,4,5]);
    Array.sameElements(a, [5,4,3,2,1,0])

@test
def sortWith11(): Bool & Impure =
    let a = Array.sortWith((x,y) -> if (x > y) -1 else 1, [5,4,3,2,1,0]);
    Array.sameElements(a, [5,4,3,2,1,0])

@test
def sortWith12(): Bool & Impure =
    let a = Array.sortWith((x,y) -> if (x > y) -1 else 1, [5,3,0,4,1,2]);
    Array.sameElements(a, [5,4,3,2,1,0])

@test
def sortWith13(): Bool & Impure =
    let a = Array.sortWith((x,y) -> if (x > y) -1 else 1, [2,3,0,4,1,2]);
    Array.sameElements(a, [4,3,2,2,1,0])

/////////////////////////////////////////////////////////////////////////////
// sortWith!                                                           //
/////////////////////////////////////////////////////////////////////////////

@test
def sortWith01!(): Bool & Impure =
    let a = []: Array[Int32];
    Array.sortWith((x,y) -> if (x < y) -1 else 1, a);
    Array.sameElements(a, []: Array[Int32])

@test
def sortWith02!(): Bool & Impure =
    let a = [0];
     Array.sortWith!((x,y) -> if (x < y) -1 else 1, a);
    Array.sameElements(a, [0])

@test
def sortWith03!(): Bool & Impure =
    let a = [0,1];
    Array.sortWith!((x,y) -> if (x < y) -1 else 1, a);
    Array.sameElements(a, [0,1])

@test
def sortWith04!(): Bool & Impure =
    let a = [1,0];
    Array.sortWith!((x,y) -> if (x < y) -1 else 1, a);
    Array.sameElements(a, [0,1])

@test
def sortWith05!(): Bool & Impure =
    let a = [1,1];
    Array.sortWith!((x,y) -> if (x < y) -1 else 1, a);
    Array.sameElements(a, [1,1])

@test
def sortWith06!(): Bool & Impure =
    let a = [0,1,2,3,4,5];
    Array.sortWith!((x,y) -> if (x < y) -1 else 1, a);
    Array.sameElements(a, [0,1,2,3,4,5])

@test
def sortWith07!(): Bool & Impure =
    let a = [5,4,3,2,1,0];
    Array.sortWith!((x,y) -> if (x < y) -1 else 1, a);
    Array.sameElements(a, [0,1,2,3,4,5])

@test
def sortWith08!(): Bool & Impure =
    let a = [5,3,0,4,1,2];
    Array.sortWith!((x,y) -> if (x < y) -1 else 1, a);
    Array.sameElements(a, [0,1,2,3,4,5])

@test
def sortWith09!(): Bool & Impure =
    let a = [2,3,0,4,1,2];
    Array.sortWith!((x,y) -> if (x < y) -1 else 1, a);
    Array.sameElements(a, [0,1,2,2,3,4])

@test
def sortWith10!(): Bool & Impure =
    let a = [0,1,2,3,4,5];
    Array.sortWith!((x,y) -> if (x > y) -1 else 1, a);
    Array.sameElements(a, [5,4,3,2,1,0])

@test
def sortWith11!(): Bool & Impure =
    let a = [5,4,3,2,1,0];
    Array.sortWith!((x,y) -> if (x > y) -1 else 1, a);
    Array.sameElements(a, [5,4,3,2,1,0])

@test
def sortWith12!(): Bool & Impure =
    let a = [5,3,0,4,1,2];
    Array.sortWith!((x,y) -> if (x > y) -1 else 1, a);
    Array.sameElements(a, [5,4,3,2,1,0])

@test
def sortWith13!(): Bool & Impure =
    let a = [2,3,0,4,1,2];
    Array.sortWith!((x,y) -> if (x > y) -1 else 1, a);
    Array.sameElements(a, [4,3,2,2,1,0])

    /////////////////////////////////////////////////////////////////////////////
    // toString                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toString01(): Bool & Impure =
        toString(Array.range(0, 5)) == "[0, 1, 2, 3, 4]"

    @test
    def toString02(): Bool & Impure =
        toString(Array.range(0, 1)) == "[0]"

    @test
    def toString03(): Bool & Impure =
        toString(Array.new('a', 3)) == "[a, a, a]"

    @test
    def toString04(): Bool & Impure =
        toString(Array.range(1, 3)) == "[1, 2]"

    @test
    def toString05(): Bool & Impure =
        toString(Array.new([1, 2, 3], 3)) == "[[1, 2, 3], [1, 2, 3], [1, 2, 3]]"

}
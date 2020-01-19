/*
 * Copyright 2019 Magnus Madsen
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
@test
def testNew01(): Bool = Array.new(42, 10)[0] == 42

@test
def testNew02(): Bool = Array.new(42, 10)[5] == 42

@test
def testNew03(): Bool = Array.new(42, 10)[9] == 42

@test
def testGet01(): Bool = Array.get([1, 2, 3], 0) == 1

@test
def testGet02(): Bool = Array.get([1, 2, 3], 1) == 2

@test
def testGet03(): Bool = Array.get([1, 2, 3], 2) == 3

@test
def testPut01(): Bool = Array.put([1], 0, 42)[0] == 42

@test
def testPut02(): Bool = Array.put([1, 2], 1, 42)[1] == 42

@test
def testPut03(): Bool = Array.put([1, 2, 3], 2, 42)[2] == 42

@test
def testLength01(): Bool = Array.length([]) == 0

@test
def testLength02(): Bool = Array.length([1]) == 1

@test
def testLength03(): Bool = Array.length([1, 2, 3]) == 3

@test
def testSlice01(): Bool = Array.slice([1, 2, 3], 0, 1)[0] == 1

@test
def testSlice02(): Bool = Array.slice([1, 2, 3], 1, 2)[0] == 2

@test
def testSlice03(): Bool = Array.slice([1, 2, 3], 2, 3)[0] == 3

@test
def testToList01(): Bool = Array.toList([]) == Nil

@test
def testToList02(): Bool = Array.toList([1]) == 1 :: Nil

@test
def testToList03(): Bool = Array.toList([1, 2]) == 1 :: 2 :: Nil

/////////////////////////////////////////////////////////////////////////////
// sameElements                                                            //
/////////////////////////////////////////////////////////////////////////////
@test
def sameElements01(): Bool =
    let a = []: Array[Int32];
    let b = []: Array[Int32];
    Array.sameElements(a,b) == true

@test
def sameElements02(): Bool =
    let a = [1]: Array[Int32];
    let b = []: Array[Int32];
    Array.sameElements(a,b) == false

@test
def sameElements03(): Bool =
    let a = []: Array[Int32];
    let b = [1]: Array[Int32];
    Array.sameElements(a,b) == false

@test
def sameElements04(): Bool =
    let a = [1]: Array[Int32];
    let b = [1]: Array[Int32];
    Array.sameElements(a,b) == true

@test
def sameElements05(): Bool =
    let a = [1]: Array[Int32];
    let b = [0]: Array[Int32];
    Array.sameElements(a,b) == false

@test
def sameElements06(): Bool =
    let a = [1,2]: Array[Int32];
    let b = [1,2]: Array[Int32];
    Array.sameElements(a,b) == true

@test
def sameElements07(): Bool =
    let a = [1,2]: Array[Int32];
    let b = [2,1]: Array[Int32];
    Array.sameElements(a,b) == false

/////////////////////////////////////////////////////////////////////////////
// init                                                                    //
/////////////////////////////////////////////////////////////////////////////
@test
def init01(): Bool =
    let a = Array.init(x -> x, 0);
    Array.sameElements(a, []: Array[Int32])

@test
def init02(): Bool =
    let a = Array.init(x -> x, -1);
    Array.sameElements(a, []: Array[Int32])

@test
def init03(): Bool =
    let a = Array.init(x -> x, 1);
    Array.sameElements(a, [0])

@test
def init04(): Bool =
    let a = Array.init(x -> x, 2);
    Array.sameElements(a, [0,1])

@test
def init05(): Bool =
    let a = Array.init(x -> x, 50);
    Array.length(a) == 50

/////////////////////////////////////////////////////////////////////////////
// copy                                                                    //
/////////////////////////////////////////////////////////////////////////////
@test
def copy01(): Bool =
    let a: Array[Int32] = [];
    let b: Array[Int32] = Array.copy(a);
    Array.sameElements(a, b)

@test
def copy02(): Bool =
    let a: Array[Int32] = [0];
    let b: Array[Int32] = Array.copy(a);
    Array.sameElements(a, b)

@test
def copy03(): Bool =
    let a: Array[Int32] = [0,1];
    let b: Array[Int32] = Array.copy(a);
    Array.sameElements(a, b)

@test
def copy04(): Bool =
    let a: Array[Int32] = [0,1,2];
    let b: Array[Int32] = Array.copy(a);
    Array.sameElements(a, b)

@test
def copy05(): Bool =
    let a: Array[Char] = ['a', 'b', 'c'];
    let b: Array[Char] = Array.copy(a);
    Array.sameElements(a, b)

/////////////////////////////////////////////////////////////////////////////
// iter                                                                    //
/////////////////////////////////////////////////////////////////////////////
@test
def iter01(): Bool =
    let a = []: Array[Int32];
    let sb = StringBuilder.new();
    let fn = i -> if (i > 0) StringBuilder.appendChar(sb, 'T') else StringBuilder.appendChar(sb, 'F');
    Array.iter(fn, a);
    StringBuilder.toString(sb) == ""

@test
def iter02(): Bool =
    let a = [0];
    let sb = StringBuilder.new();
    let fn = i -> if (i > 0) StringBuilder.appendChar(sb, 'T') else StringBuilder.appendChar(sb, 'F');
    Array.iter(fn, a);
    StringBuilder.toString(sb) == "F"

@test
def iter03(): Bool =
    let a = [1];
    let sb = StringBuilder.new();
    let fn = i -> if (i > 0) StringBuilder.appendChar(sb, 'T') else StringBuilder.appendChar(sb, 'F');
    Array.iter(fn, a);
    StringBuilder.toString(sb) == "T"

@test
def iter04(): Bool =
    let a = [0,1];
    let sb = StringBuilder.new();
    let fn = i -> if (i > 0) StringBuilder.appendChar(sb, 'T') else StringBuilder.appendChar(sb, 'F');
    Array.iter(fn, a);
    StringBuilder.toString(sb) == "FT"

/////////////////////////////////////////////////////////////////////////////
// range                                                                   //
/////////////////////////////////////////////////////////////////////////////
@test
def range01(): Bool =
    let a = Array.range(1, 0);
    Array.sameElements(a, []: Array[Int32])

@test
def range02(): Bool =
    let a = Array.range(1, 1);
    Array.sameElements(a, []: Array[Int32])

@test
def range03(): Bool =
    let a = Array.range(1, 2);
    Array.sameElements(a, [1])

@test
def range04(): Bool =
    let a = Array.range(1, 3);
    Array.sameElements(a, [1,2])

@test
def range05(): Bool =
    let a = Array.range(1, 4);
    Array.sameElements(a, [1,2,3])

@test
def range06(): Bool =
    let a = Array.range(-1, 3);
    Array.sameElements(a, [-1,0,1,2])

/////////////////////////////////////////////////////////////////////////////
// append                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def append01(): Bool =
    let a = Array.append([]: Array[Int32], []: Array[Int32]);
    Array.sameElements(a, []: Array[Int32])

@test
def append02(): Bool =
    let a = Array.append([]: Array[Int32], [1]);
    Array.sameElements(a, [1])

@test
def append03(): Bool =
    let a = Array.append([]: Array[Int32], [1,2]);
    Array.sameElements(a, [1,2])

@test
def append04(): Bool =
    let a = Array.append([1], []: Array[Int32]);
    Array.sameElements(a, [1])

@test
def append05(): Bool =
    let a = Array.append([1,2], []: Array[Int32]);
    Array.sameElements(a, [1,2])

@test
def append06(): Bool =
    let a = Array.append([1], [2]);
    Array.sameElements(a, [1,2])

@test
def append07(): Bool =
    let a = Array.append([1,2], [3]);
    Array.sameElements(a, [1,2,3])

@test
def append08(): Bool =
    let a = Array.append([1], [2,3]);
    Array.sameElements(a, [1,2,3])

/////////////////////////////////////////////////////////////////////////////
// flatten                                                                 //
/////////////////////////////////////////////////////////////////////////////
@test
def flatten01(): Bool =
    let a = Array.flatten([]: Array[Array[Int32]]);
    Array.sameElements(a, []: Array[Int32])

@test
def flatten02(): Bool =
    let a = Array.flatten([[]]: Array[Array[Int32]]);
    Array.sameElements(a, []: Array[Int32])

@test
def flatten03(): Bool =
    let a = Array.flatten([[1]]);
    Array.sameElements(a, [1])

@test
def flatten04(): Bool =
    let a= Array.flatten([[1,2]]);
    Array.sameElements(a, [1,2])

@test
def flatten05(): Bool =
    let a = Array.flatten([[],[]]: Array[Array[Int32]]);
    Array.sameElements(a, []: Array[Int32])

@test
def flatten06(): Bool =
    let a = Array.flatten([[1],[]]);
    Array.sameElements(a, [1])

@test
def flatten07(): Bool =
    let a = Array.flatten([[],[1]]);
    Array.sameElements(a, [1])

@test
def flatten08(): Bool =
    let a = Array.flatten([[1],[2]]);
    Array.sameElements(a, [1,2])

@test
def flatten09(): Bool =
    let a = Array.flatten([[1,2],[3,4,5]]);
    Array.sameElements(a, [1,2,3,4,5])


@test
def flatten10(): Bool =
    let a = Array.flatten([[1],[2,3],[4]]);
    Array.sameElements(a, [1,2,3,4])

/////////////////////////////////////////////////////////////////////////////
// map                                                                     //
/////////////////////////////////////////////////////////////////////////////
@test
def map01(): Bool =
    let a = Array.map(i -> i > 2, []);
    Array.sameElements(a, []: Array[Bool])

@test
def map02(): Bool =
    let a = Array.map(i -> i > 2, [1]);
    Array.sameElements(a, [false])

@test
def map03(): Bool =
    let a = Array.map(i -> i > 2, [3]);
    Array.sameElements(a, [true])

@test
def map04(): Bool =
    let a = Array.map(i -> i > 2, [1,2]);
    Array.sameElements(a, [false,false])

@test
def map05(): Bool =
    let a = Array.map(i -> i > 2, [1,8]);
    Array.sameElements(a, [false, true])

@test
def map06(): Bool =
    let a = Array.map(i -> i > 2, [8,1]);
    Array.sameElements(a, [true,false])

@test
def map07(): Bool =
    let a = Array.map(i -> i > 2, [7,8]);
    Array.sameElements(a, [true,true])

/////////////////////////////////////////////////////////////////////////////
// map2                                                                    //
/////////////////////////////////////////////////////////////////////////////
@test
def map201(): Bool =
    let arr = Array.map2((a, b) -> if (b) a+1 else a, [], []);
    Array.sameElements(arr, []: Array[Int32])

@test
def map202(): Bool = // List.map2((a, b) -> if (b) a+1 else a, 1 :: Nil, Nil) == Nil
    let arr = Array.map2((a, b) -> if (b) a+1 else a, [1], []);
    Array.sameElements(arr, []: Array[Int32])

@test
def map203(): Bool = // List.map2((a, b) -> if (b) a+1 else a, Nil, true :: Nil) == Nil
    let arr = Array.map2((a, b) -> if (b) a+1 else a, [], [true]);
    Array.sameElements(arr, []: Array[Int32])

@test
def map204(): Bool = // List.map2((a, b) -> if (b) a+1 else a, 1 :: Nil, true :: Nil) == 2 :: Nil
    let arr = Array.map2((a, b) -> if (b) a+1 else a, [1], [true]);
    Array.sameElements(arr, [2])

@test
def map205(): Bool = // List.map2((a, b) -> if (b) a+1 else a, 1 :: Nil, false :: Nil) == 1 :: Nil
    let arr = Array.map2((a, b) -> if (b) a+1 else a, [1], [false]);
    Array.sameElements(arr, [1])

@test
def map206(): Bool =
    let arr = Array.map2((a, b) -> if (b) a+1 else a, [1,2,3,4,5,6,7,8], [false,true,true,false,false,true,true,true]);
    Array.sameElements(arr, [1,3,4,4,5,7,8,9])

/////////////////////////////////////////////////////////////////////////////
// mapWithIndex                                                            //
/////////////////////////////////////////////////////////////////////////////
@test
def mapWithIndex01(): Bool =
    let a = Array.mapWithIndex((e, i) -> if (i < 1) e > 2 else e <= 2, []);
    Array.sameElements(a, []: Array[Bool])

@test
def mapWithIndex02(): Bool =
    let a = Array.mapWithIndex((e, i) -> if (i < 1) e > 2 else e <= 2, [1]);
    Array.sameElements(a, [false])

@test
def mapWithIndex03(): Bool =
    let a = Array.mapWithIndex((e, i) -> if (i < 1) e > 2 else e <= 2, [3]);
    Array.sameElements(a, [true])

@test
def mapWithIndex04(): Bool =
    let a = Array.mapWithIndex((e, i) -> if (i < 1) e > 2 else e <= 2, [1,2]);
    Array.sameElements(a, [false,true])

@test
def mapWithIndex05(): Bool =
    let a = Array.mapWithIndex((e, i) -> if (i < 1) e > 2 else e <= 2, [1,8]);
    Array.sameElements(a, [false,false])

@test
def mapWithIndex06(): Bool =
    let a = Array.mapWithIndex((e, i) -> if (i < 1) e > 2 else e <= 2, [8,1]);
    Array.sameElements(a, [true,true])

@test
def mapWithIndex07(): Bool =
    let a = Array.mapWithIndex((e, i) -> if (i < 1) e > 2 else e <= 2, [7,8]);
    Array.sameElements(a, [true,false])

/////////////////////////////////////////////////////////////////////////////
// flatMap                                                                 //
/////////////////////////////////////////////////////////////////////////////
@test
def flatMap01(): Bool =
    let a = Array.flatMap(i -> Array.repeat(i, i), []);
    Array.sameElements(a, []: Array[Int32])

@test
def flatMap02(): Bool =
    let a = Array.flatMap(i -> Array.repeat(i, i), [0]);
    Array.sameElements(a, []: Array[Int32])

@test
def flatMap03(): Bool =
    let a = Array.flatMap(i -> Array.repeat(i, i), [1]);
    Array.sameElements(a, [1])

@test
def flatMap04(): Bool =
    let a = Array.flatMap(i -> Array.repeat(i, i), [2]);
    Array.sameElements(a, [2,2])

@test
def flatMap05(): Bool =
    let a = Array.flatMap(i -> Array.repeat(i, i), [1,2]);
    Array.sameElements(a, [1,2,2])


@test
def flatMap06(): Bool =
    let a = Array.flatMap(i -> Array.repeat(i, i), [2,3]);
    Array.sameElements(a, [2,2,3,3,3])

/////////////////////////////////////////////////////////////////////////////
// flatMap2                                                                //
/////////////////////////////////////////////////////////////////////////////
@test
def flatMap201(): Bool =
    let arr = Array.flatMap2((a, b) -> if (b) Array.repeat(a, a) else Array.repeat(a+1, a+1), [], []);
    Array.sameElements(arr, []: Array[Int32])

@test
def flatMap202(): Bool =
    let arr = Array.flatMap2((a, b) -> if (b) Array.repeat(a, a) else Array.repeat(a+1, a+1), [1], []);
    Array.sameElements(arr, []: Array[Int32])

@test
def flatMap203(): Bool =
    let arr = Array.flatMap2((a, b) -> if (b) Array.repeat(a, a) else Array.repeat(a+1, a+1), [], [true]);
    Array.sameElements(arr, []: Array[Int32])

@test
def flatMap204(): Bool =
    let arr = Array.flatMap2((a, b) -> if (b) Array.repeat(a, a) else Array.repeat(a+1, a+1), [2], [true]);
    Array.sameElements(arr, [2,2])

@test
def flatMap205(): Bool =
    let arr = Array.flatMap2((a, b) -> if (b) Array.repeat(a, a) else Array.repeat(a+1, a+1), [2], [false]);
    Array.sameElements(arr, [3,3,3])

@test
def flatMap206(): Bool =
    let arr = Array.flatMap2((a, b) -> if (b) Array.repeat(a, a) else Array.repeat(a+1, a+1), [1,2,3,4,5],
                   [false,true,true,false,false]);
    Array.sameElements(arr, [2, 2, 2, 2, 3, 3, 3, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6])

/////////////////////////////////////////////////////////////////////////////
// count                                                                   //
/////////////////////////////////////////////////////////////////////////////
@test
def count01(): Bool = Array.count(i -> i > 3, []) == 0

@test
def count02(): Bool = Array.count(i -> i > 3, [1]) == 0

@test
def count03(): Bool = Array.count(i -> i > 3, [4]) == 1

@test
def count04(): Bool = Array.count(i -> i > 3, [1,2]) == 0

@test
def count05(): Bool = Array.count(i -> i > 3, [1,8]) == 1

@test
def count06(): Bool = Array.count(i -> i > 3, [8,1]) == 1

@test
def count07(): Bool = Array.count(i -> i > 3, [6,7]) == 2

/////////////////////////////////////////////////////////////////////////////
// repeat                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def repeat01(): Bool =
    let a = Array.repeat(1, -1);
    Array.sameElements(a, []: Array[Int32])

@test
def repeat02(): Bool =
    let a = Array.repeat(1, 0);
    Array.sameElements(a, []: Array[Int32])

@test
def repeat03(): Bool =
    let a = Array.repeat(1, 1);
    Array.sameElements(a, [1])

@test
def repeat04(): Bool =
    let a = Array.repeat(1, 2);
    Array.sameElements(a, [1,1])

@test
def repeat05(): Bool =
    let a = Array.repeat(1, 3);
    Array.sameElements(a, [1,1,1])

/////////////////////////////////////////////////////////////////////////////
// reverse                                                                 //
/////////////////////////////////////////////////////////////////////////////
@test
def reverse01(): Bool =
    let a = Array.reverse([]: Array[Int32]);
    Array.sameElements(a, []: Array[Int32])

@test
def reverse02(): Bool =
    let a = Array.reverse([1]);
    Array.sameElements(a, [1])

@test
def reverse03(): Bool =
    let a = Array.reverse([1,2]);
    Array.sameElements(a, [2,1])

@test
def reverse04(): Bool =
    let a = Array.reverse([1,1]);
    Array.sameElements(a, [1,1])

@test
def reverse05(): Bool =
    let a = Array.reverse([1,2,3]);
    Array.sameElements(a, [3,2,1])

@test
def reverse06(): Bool =
    let a = Array.reverse([1,2,3,4]);
    Array.sameElements(a, [4,3,2,1])

/////////////////////////////////////////////////////////////////////////////
// scan                                                                    //
/////////////////////////////////////////////////////////////////////////////
@test
def scan01(): Bool =
    let a = Array.scan((i, b) -> if (b) i+1 else i+2, 1, []);
    Array.sameElements(a, [1])

@test
def scan02(): Bool =
    let a = Array.scan((i, b) -> if (b) i+1 else i+2, 1, [false]);
    Array.sameElements(a, [1,3])

@test
def scan03(): Bool =
    let a = Array.scan((i, b) -> if (b) i+1 else i+2, 1, [true]);
    Array.sameElements(a, [1,2])

@test
def scan04(): Bool =
    let a = Array.scan((i, b) -> if (b) i+1 else i+2, 1, [false,false]);
    Array.sameElements(a, [1,3,5])

@test
def scan05(): Bool =
    let a = Array.scan((i, b) -> if (b) i+1 else i+2, 1, [false,true]);
    Array.sameElements(a, [1,3,4])

@test
def scan06(): Bool =
    let a = Array.scan((i, b) -> if (b) i+1 else i+2, 1, [true,false]);
    Array.sameElements(a, [1,2,4])

@test
def scan07(): Bool =
    let a = Array.scan((i, b) -> if (b) i+1 else i+2, 1, [true,true]);
    Array.sameElements(a, [1,2,3])

/////////////////////////////////////////////////////////////////////////////
// scanLeft                                                                //
/////////////////////////////////////////////////////////////////////////////
@test
def scanLeft01(): Bool =
    let a = Array.scanLeft((i, b) -> if (b) i+1 else i+2, 1, []);
    Array.sameElements(a, [1])

@test
def scanLeft02(): Bool =
    let a = Array.scanLeft((i, b) -> if (b) i+1 else i+2, 1, [false]);
    Array.sameElements(a, [1,3])

@test
def scanLeft03(): Bool =
    let a = Array.scanLeft((i, b) -> if (b) i+1 else i+2, 1, [true]);
    Array.sameElements(a, [1,2])

@test
def scanLeft04(): Bool =
    let a = Array.scanLeft((i, b) -> if (b) i+1 else i+2, 1, [false,false]);
    Array.sameElements(a, [1,3,5])

@test
def scanLeft05(): Bool =
    let a = Array.scanLeft((i, b) -> if (b) i+1 else i+2, 1, [false,true]);
    Array.sameElements(a, [1,3,4])

@test
def scanLeft06(): Bool =
    let a = Array.scanLeft((i, b) -> if (b) i+1 else i+2, 1, [true,false]);
    Array.sameElements(a, [1,2,4])

@test
def scanLeft07(): Bool =
    let a = Array.scanLeft((i, b) -> if (b) i+1 else i+2, 1, [true,true]);
    Array.sameElements(a, [1,2,3])


/////////////////////////////////////////////////////////////////////////////
// scanRight                                                               //
/////////////////////////////////////////////////////////////////////////////
@test
def scanRight01(): Bool =
    let a = Array.scanRight((b, i) -> if (b) i+1 else i+2, 1, []);
    Array.sameElements(a, [1])

@test
def scanRight02(): Bool =
    let a = Array.scanRight((b, i) -> if (b) i+1 else i+2, 1, [false]);
    Array.sameElements(a, [3,1])

@test
def scanRight03(): Bool =
    let a = Array.scanRight((b, i) -> if (b) i+1 else i+2, 1, [true]);
    Array.sameElements(a, [2,1])

@test
def scanRight04(): Bool =
    let a = Array.scanRight((b, i) -> if (b) i+1 else i+2, 1, [false,false]);
    Array.sameElements(a, [5,3,1])

@test
def scanRight05(): Bool =
    let a = Array.scanRight((b, i) -> if (b) i+1 else i+2, 1, [false,true]);
    Array.sameElements(a, [4,2,1])

@test
def scanRight06(): Bool =
    let a = Array.scanRight((b, i) -> if (b) i+1 else i+2, 1, [true,false]);
    Array.sameElements(a, [4,3,1])

@test
def scanRight07(): Bool =
    let a = Array.scanRight((b, i) -> if (b) i+1 else i+2, 1, [true,true]);
    Array.sameElements(a, [3,2,1])

/////////////////////////////////////////////////////////////////////////////
// fold                                                                    //
/////////////////////////////////////////////////////////////////////////////
@test
def fold01(): Bool = Array.fold((i, e) -> (i - e)*(e % 2 + 1), 100, []) == 100

@test
def fold02(): Bool = Array.fold((i, e) -> (i - e)*(e % 2 + 1), 100, [1]) == 198

@test
def fold03(): Bool = Array.fold((i, e) -> (i - e)*(e % 2 + 1), 100, [1,2]) == 196

@test
def fold04(): Bool = Array.fold((i, e) -> (i - e)*(e % 2 + 1), 100, [1,2,3]) == 386

/////////////////////////////////////////////////////////////////////////////
// foldLeft                                                                //
/////////////////////////////////////////////////////////////////////////////
@test
def foldLeft01(): Bool = Array.foldLeft((i, e) -> (i - e)*(e % 2 + 1), 100, []) == 100

@test
def foldLeft02(): Bool = Array.foldLeft((i, e) -> (i - e)*(e % 2 + 1), 100, [1]) == 198

@test
def foldLeft03(): Bool = Array.foldLeft((i, e) -> (i - e)*(e % 2 + 1), 100, [1,2]) == 196

@test
def foldLeft04(): Bool = Array.foldLeft((i, e) -> (i - e)*(e % 2 + 1), 100, [1,2,3]) == 386

/////////////////////////////////////////////////////////////////////////////
// foldRight                                                               //
/////////////////////////////////////////////////////////////////////////////
@test
def foldRight01(): Bool = Array.foldRight((e, i) -> (i - e)*(e % 2 + 1), 100, []) == 100

@test
def foldRight02(): Bool = Array.foldRight((e, i) -> (i - e)*(e % 2 + 1), 100, [1]) == 198

@test
def foldRight03(): Bool = Array.foldRight((e, i) -> (i - e)*(e % 2 + 1), 100, [1,2]) == 194

@test
def foldRight04(): Bool = Array.foldRight((e, i) -> (i - e)*(e % 2 + 1), 100, [1,2,3]) == 382

/////////////////////////////////////////////////////////////////////////////
// fold2                                                                   //
/////////////////////////////////////////////////////////////////////////////
@test
def fold201(): Bool = Array.fold2((c, a, b) -> if (b) a+c else a*c, 4, [], []) == 4

@test
def fold202(): Bool = Array.fold2((c, a, b) -> if (b) a+c else a*c, 4, [1], []) == 4

@test
def fold203(): Bool = Array.fold2((c, a, b) -> if (b) a+c else a*c, 4, [], [true]) == 4

@test
def fold204(): Bool = Array.fold2((c, a, b) -> if (b) a+c else a*c, 4, [2], [true]) == 6

@test
def fold205(): Bool = Array.fold2((c, a, b) -> if (b) a+c else a*c, 4, [2], [false]) == 8

@test
def fold206(): Bool = Array.fold2((c, a, b) -> if (b) a+c else a*c, 4, [3,2], [true,true]) == 9

@test
def fold207(): Bool = Array.fold2((c, a, b) -> if (b) a+c else a*c, 4, [3,2], [true,false]) == 14

@test
def fold208(): Bool = Array.fold2((c, a, b) -> if (b) a+c else a*c, 4, [3,2], [false,true]) == 14

@test
def fold209(): Bool = Array.fold2((c, a, b) -> if (b) a+c else a*c, 4, [3,2], [false,false]) == 24

@test
def fold210(): Bool = Array.fold2((c, a, b) -> if (b) a+c else a*c, 4, [6,-4,3,2],
                    [true,false,false,true]) == -118

/////////////////////////////////////////////////////////////////////////////
// foldLeft2                                                               //
/////////////////////////////////////////////////////////////////////////////
@test
def foldLeft201(): Bool = Array.foldLeft2((c, a, b) -> if (b) a+c else a*c, 4, [], []) == 4

@test
def foldLeft202(): Bool = Array.foldLeft2((c, a, b) -> if (b) a+c else a*c, 4, [1], []) == 4

@test
def foldLeft203(): Bool = Array.foldLeft2((c, a, b) -> if (b) a+c else a*c, 4, [], [true]) == 4

@test
def foldLeft204(): Bool = Array.foldLeft2((c, a, b) -> if (b) a+c else a*c, 4, [2], [true]) == 6

@test
def foldLeft205(): Bool = Array.foldLeft2((c, a, b) -> if (b) a+c else a*c, 4, [2], [false]) == 8

@test
def foldLeft206(): Bool = Array.foldLeft2((c, a, b) -> if (b) a+c else a*c, 4, [3,2], [true,true]) == 9

@test
def foldLeft207(): Bool = Array.foldLeft2((c, a, b) -> if (b) a+c else a*c, 4, [3,2], [true,false]) == 14

@test
def foldLeft208(): Bool = Array.foldLeft2((c, a, b) -> if (b) a+c else a*c, 4, [3,2], [false,true]) == 14

@test
def foldLeft209(): Bool = Array.foldLeft2((c, a, b) -> if (b) a+c else a*c, 4, [3,2], [false,false]) == 24

@test
def foldLeft210(): Bool = Array.foldLeft2((c, a, b) -> if (b) a+c else a*c, 4, [6,-4,3,2],
                        [true,false,false,true]) == -118

/////////////////////////////////////////////////////////////////////////////
// foldRight2                                                              //
/////////////////////////////////////////////////////////////////////////////
@test
def foldRight201(): Bool = Array.foldRight2((a, b, c) -> if (b) a+c else a*c, 4, [], []) == 4

@test
def foldRight202(): Bool = Array.foldRight2((a, b, c) -> if (b) a+c else a*c, 4, [1], []) == 4

@test
def foldRight203(): Bool = Array.foldRight2((a, b, c) -> if (b) a+c else a*c, 4, [], [true]) == 4

@test
def foldRight204(): Bool = Array.foldRight2((a, b, c) -> if (b) a+c else a*c, 4, [2], [true]) == 6

@test
def foldRight205(): Bool = Array.foldRight2((a, b, c) -> if (b) a+c else a*c, 4, [2], [false]) == 8

@test
def foldRight206(): Bool = Array.foldRight2((a, b, c) -> if (b) a+c else a*c, 4, [3,2], [true,true]) == 9

@test
def foldRight207(): Bool = Array.foldRight2((a, b, c) -> if (b) a+c else a*c, 4, [3,2], [true,false]) == 11

@test
def foldRight208(): Bool = Array.foldRight2((a, b, c) -> if (b) a+c else a*c, 4, [3,2], [false,true]) == 18

@test
def foldRight209(): Bool = Array.foldRight2((a, b, c) -> if (b) a+c else a*c, 4, [3,2], [false,false]) == 24

@test
def foldRight210(): Bool = Array.foldRight2((a, b, c) -> if (b) a+c else a*c, 4, [6,-4,3,2],
                         [true,false,false,true]) == -66

/////////////////////////////////////////////////////////////////////////////
// reduce                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def reduce01(): Bool = Array.reduce((a, b) -> a-b, []: Array[Int]) == None

@test
def reduce02(): Bool = Array.reduce((a, b) -> a-b, [1]) == Some(1)

@test
def reduce03(): Bool = Array.reduce((a, b) -> a-b, [1,2]) == Some(-1)

@test
def reduce04(): Bool = Array.reduce((a, b) -> a-b, [1,2,3]) == Some(-4)

@test
def reduce05(): Bool = Array.reduce((a, b) -> a-b, [1,2,3,4]) == Some(-8)

/////////////////////////////////////////////////////////////////////////////
// reduceLeft                                                              //
/////////////////////////////////////////////////////////////////////////////
@test
def reduceLeft01(): Bool = Array.reduceLeft((a, b) -> a-b, []: Array[Int]) == None

@test
def reduceLeft02(): Bool = Array.reduceLeft((a, b) -> a-b, [1]) == Some(1)

@test
def reduceLeft03(): Bool = Array.reduceLeft((a, b) -> a-b, [1,2]) == Some(-1)

@test
def reduceLeft04(): Bool = Array.reduceLeft((a, b) -> a-b, [1,2,3]) == Some(-4)

@test
def reduceLeft05(): Bool = Array.reduceLeft((a, b) -> a-b, [1,2,3,4]) == Some(-8)

/////////////////////////////////////////////////////////////////////////////
// reduceRight                                                             //
/////////////////////////////////////////////////////////////////////////////
@test
def reduceRight01(): Bool = Array.reduceRight((a, b) -> a-b, []: Array[Int]) == None

@test
def reduceRight02(): Bool = Array.reduceRight((a, b) -> a-b, [1]) == Some(1)

@test
def reduceRight03(): Bool = Array.reduceRight((a, b) -> a-b, [1,2]) == Some(-1)

@test
def reduceRight04(): Bool = Array.reduceRight((a, b) -> a-b, [1,2,3]) == Some(2)

@test
def reduceRight05(): Bool = Array.reduceRight((a, b) -> a-b, [1,2,3,4]) == Some(-2)

/////////////////////////////////////////////////////////////////////////////
// drop                                                                    //
/////////////////////////////////////////////////////////////////////////////
@test
def drop01(): Bool =
    let a = Array.drop(-1, []: Array[Int32]);
    Array.sameElements(a, []: Array[Int32])

@test
def drop02(): Bool =
    let a = Array.drop(0, []: Array[Int32]);
    Array.sameElements(a, []: Array[Int32])

@test
def drop03(): Bool =
    let a = Array.drop(1, []: Array[Int32]);
    Array.sameElements(a, []: Array[Int32])

@test
def drop04(): Bool =
    let a = Array.drop(-1, [1]);
    Array.sameElements(a, [1])

@test
def drop05(): Bool =
    let a = Array.drop(0, [1]);
    Array.sameElements(a, [1])

@test
def drop06(): Bool =
    let a = Array.drop(1, [1]);
    Array.sameElements(a, []: Array[Int32])

@test
def drop07(): Bool =
    let a = Array.drop(2, [1]);
    Array.sameElements(a, []: Array[Int32])

@test
def drop08(): Bool =
    let a = Array.drop(0, [1,2]);
    Array.sameElements(a, [1,2])

@test
def drop09(): Bool =
    let a = Array.drop(1, [1,2]);
    Array.sameElements(a, [2])

@test
def drop10(): Bool =
    let a = Array.drop(2, [1,2]);
    Array.sameElements(a, []: Array[Int32])

@test
def drop11(): Bool =
    let a = Array.drop(2, [1,2,3,4,5,6]);
    Array.sameElements(a, [3,4,5,6])

@test
def drop12(): Bool =
    let a = Array.drop(4, [1,2,3,4,5,6]);
    Array.sameElements(a, [5,6])

/////////////////////////////////////////////////////////////////////////////
// dropWhile                                                               //
/////////////////////////////////////////////////////////////////////////////
@test
def dropWhile01(): Bool =
    let a = Array.dropWhile(i -> i > 3, []);
    Array.sameElements(a, []: Array[Int32])

@test
def dropWhile02(): Bool =
    let a = Array.dropWhile(i -> i > 3, [1]);
    Array.sameElements(a, [1])

@test
def dropWhile03(): Bool =
    let a = Array.dropWhile(i -> i > 3, [4]);
    Array.sameElements(a, []: Array[Int32])

@test
def dropWhile04(): Bool =
    let a = Array.dropWhile(i -> i > 3, [1,2]);
    Array.sameElements(a, [1,2])

@test
def dropWhile05(): Bool =
    let a = Array.dropWhile(i -> i > 3, [1,5]);
    Array.sameElements(a, [1,5])

@test
def dropWhile06(): Bool =
    let a = Array.dropWhile(i -> i > 3, [5,1]);
    Array.sameElements(a, [1])

@test
def dropWhile07(): Bool =
    let a = Array.dropWhile(i -> i > 3, [5,8]);
    Array.sameElements(a, []: Array[Int32])

@test
def dropWhile08(): Bool =
    let a = Array.dropWhile(i -> i > 3, [4, 6, -3, 11, -5, 1, 2, 16, 7, 1, 7]);
    Array.sameElements(a, [-3, 11, -5, 1, 2, 16, 7, 1, 7])

/////////////////////////////////////////////////////////////////////////////
// take                                                                    //
/////////////////////////////////////////////////////////////////////////////
@test
def take01(): Bool =
    let a = Array.take(-1, []: Array[Int32]);
    Array.sameElements(a, []: Array[Int32])

@test
def take02(): Bool =
    let a = Array.take(0, []: Array[Int32]);
    Array.sameElements(a, []: Array[Int32])

@test
def take03(): Bool =
    let a = Array.take(1, []: Array[Int32]);
    Array.sameElements(a, []: Array[Int32])

@test
def take04(): Bool =
    let a = Array.take(-1, [1]);
    Array.sameElements(a, []: Array[Int32])

@test
def take05(): Bool =
    let a = Array.take(0, [1]);
    Array.sameElements(a, []: Array[Int32])

@test
def take06(): Bool =
    let a = Array.take(1, [1]);
    Array.sameElements(a, [1])

@test
def take07(): Bool =
    let a = Array.take(2, [1]);
    Array.sameElements(a, [1])

@test
def take08(): Bool =
    let a = Array.take(0, [1,2]);
    Array.sameElements(a, []: Array[Int32])

@test
def take09(): Bool =
    let a = Array.take(1, [1,2]);
    Array.sameElements(a, [1])

@test
def take10(): Bool =
    let a = Array.take(2, [1,2]);
    Array.sameElements(a, [1,2])

@test
def take11(): Bool =
    let a = Array.take(2, [1,2,3,4,5,6]);
    Array.sameElements(a, [1,2])

@test
def take12(): Bool =
    let a = Array.take(4, [1,2,3,4,5,6]);
    Array.sameElements(a, [1,2,3,4])

/////////////////////////////////////////////////////////////////////////////
// takeWhile                                                               //
/////////////////////////////////////////////////////////////////////////////
@test
def takeWhile01(): Bool =
    let a = Array.takeWhile(i -> i > 3, []: Array[Int32]);
    Array.sameElements(a, []: Array[Int32])

@test
def takeWhile02(): Bool =
    let a = Array.takeWhile(i -> i > 3, [1]);
    Array.sameElements(a, []: Array[Int32])

@test
def takeWhile03(): Bool =
    let a = Array.takeWhile(i -> i > 3, [4]);
    Array.sameElements(a, [4])

@test
def takeWhile04(): Bool =
    let a = Array.takeWhile(i -> i > 3, [1,2]);
    Array.sameElements(a, []: Array[Int32])

@test
def takeWhile05(): Bool =
    let a = Array.takeWhile(i -> i > 3, [1,5]);
    Array.sameElements(a, []: Array[Int32])

@test
def takeWhile06(): Bool =
    let a = Array.takeWhile(i -> i > 3, [5,1]);
    Array.sameElements(a, [5])

@test
def takeWhile07(): Bool =
    let a = Array.takeWhile(i -> i > 3, [5,8]);
    Array.sameElements(a, [5,8])

@test
def takeWhile08(): Bool =
    let a = Array.takeWhile(i -> i > 3, [4, 6, -3, 11, -5, 1, 2, 16, 7, 1, 7]);
    Array.sameElements(a, [4, 6])

/////////////////////////////////////////////////////////////////////////////
// exists                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def exists01(): Bool = Array.exists(i -> i > 3, [] : Array[Int32]) == false

@test
def exists02(): Bool = Array.exists(i -> i > 3, [1]) == false

@test
def exists03(): Bool = Array.exists(i -> i > 3, [5]) == true

@test
def exists04(): Bool = Array.exists(i -> i > 3, [1,2]) == false

@test
def exists05(): Bool = Array.exists(i -> i > 3, [1,6]) == true

@test
def exists06(): Bool = Array.exists(i -> i > 3, [6,1]) == true

@test
def exists07(): Bool = Array.exists(i -> i > 3, [16,6]) == true

@test
def exists08(): Bool = Array.exists(i -> i > 3, [1,-9,3]) == false

@test
def exists09(): Bool = Array.exists(i -> i > 3, [1,9,3]) == true

/////////////////////////////////////////////////////////////////////////////
// forall                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def forall01(): Bool = Array.forall(i -> i > 3, []: Array[Int32]) == true

@test
def forall02(): Bool = Array.forall(i -> i > 3, [1]) == false

@test
def forall03(): Bool = Array.forall(i -> i > 3, [5]) == true

@test
def forall04(): Bool = Array.forall(i -> i > 3, [1,2]) == false

@test
def forall05(): Bool = Array.forall(i -> i > 3, [1,6]) == false

@test
def forall06(): Bool = Array.forall(i -> i > 3, [6,1]) == false

@test
def forall07(): Bool = Array.forall(i -> i > 3, [16,6]) == true

@test
def forall08(): Bool = Array.forall(i -> i > 3, [1,-9,3]) == false

@test
def forall09(): Bool = Array.forall(i -> i > 3, [1,9,3]) == false

@test
def forall10(): Bool = Array.forall(i -> i > 3, [11,9,31]) == true

/////////////////////////////////////////////////////////////////////////////
// memberOf                                                                //
/////////////////////////////////////////////////////////////////////////////
@test
def memberOf01(): Bool = Array.memberOf(0, []: Array[Int32]) == false

@test
def memberOf02(): Bool = Array.memberOf(0, [1]) == false

@test
def memberOf03(): Bool = Array.memberOf(0, [0]) == true

@test
def memberOf04(): Bool = Array.memberOf(0, [1,2]) == false

@test
def memberOf05(): Bool = Array.memberOf(1, [1,2]) == true

@test
def memberOf06(): Bool = Array.memberOf(2, [1,2]) == true

@test
def memberOf07(): Bool = Array.memberOf(3, [1,2]) == false

/////////////////////////////////////////////////////////////////////////////
// indexOf                                                                 //
/////////////////////////////////////////////////////////////////////////////
@test
def indexOf01(): Bool = Array.indexOf(0, []: Array[Int32]) == None

@test
def indexOf02(): Bool = Array.indexOf(0, [1]) == None

@test
def indexOf03(): Bool = Array.indexOf(1, [1]) == Some(0)

@test
def indexOf04(): Bool = Array.indexOf(0, [1,2]) == None

@test
def indexOf05(): Bool = Array.indexOf(1, [1,2]) == Some(0)

@test
def indexOf06(): Bool = Array.indexOf(2, [1,2]) == Some(1)

/////////////////////////////////////////////////////////////////////////////
// indexOfLeft                                                             //
/////////////////////////////////////////////////////////////////////////////
@test
def indexOfLeft01(): Bool = Array.indexOfLeft(0, []: Array[Int32]) == None

@test
def indexOfLeft02(): Bool = Array.indexOfLeft(0, [1]) == None

@test
def indexOfLeft03(): Bool = Array.indexOfLeft(1, [1]) == Some(0)

@test
def indexOfLeft04(): Bool = Array.indexOfLeft(0, [1,2]) == None

@test
def indexOfLeft05(): Bool = Array.indexOfLeft(1, [1,2]) == Some(0)

@test
def indexOfLeft06(): Bool = Array.indexOfLeft(2, [1,2]) == Some(1)

@test
def indexOfLeft07(): Bool = Array.indexOfLeft(1, [1,1]) == Some(0)

/////////////////////////////////////////////////////////////////////////////
// indexOfRight                                                            //
/////////////////////////////////////////////////////////////////////////////
@test
def indexOfRight01(): Bool = Array.indexOfRight(0, []: Array[Int32]) == None

@test
def indexOfRight02(): Bool = Array.indexOfRight(0, [1]) == None

@test
def indexOfRight03(): Bool = Array.indexOfRight(1, [1]) == Some(0)

@test
def indexOfRight04(): Bool = Array.indexOfRight(0, [1,2]) == None

@test
def indexOfRight05(): Bool = Array.indexOfRight(1, [1,2]) == Some(0)

@test
def indexOfRight06(): Bool = Array.indexOfRight(2, [1,2]) == Some(1)

@test
def indexOfRight07(): Bool = Array.indexOfRight(1, [1,1]) == Some(1)

/////////////////////////////////////////////////////////////////////////////
// find                                                                    //
/////////////////////////////////////////////////////////////////////////////
@test
def find01(): Bool = Array.find(i -> i > 2, []: Array[Int32]) == None

@test
def find02(): Bool = Array.find(i -> i > 2, [1]) == None

@test
def find03(): Bool = Array.find(i -> i > 2, [3]) == Some(3)

@test
def find04(): Bool = Array.find(i -> i > 2, [1,2]) == None

@test
def find05(): Bool = Array.find(i -> i > 2, [6,-6]) == Some(6)

@test
def find06(): Bool = Array.find(i -> i > 2, [-6,6]) == Some(6)

@test
def find07(): Bool = Array.find(i -> i > 2, [6,7]) == Some(6)

/////////////////////////////////////////////////////////////////////////////
// findLeft                                                                //
/////////////////////////////////////////////////////////////////////////////
@test
def findLeft01(): Bool = Array.findLeft(i -> i > 2, []) == None

@test
def findLeft02(): Bool = Array.findLeft(i -> i > 2, [1]) == None

@test
def findLeft03(): Bool = Array.findLeft(i -> i > 2, [3]) == Some(3)

@test
def findLeft04(): Bool = Array.findLeft(i -> i > 2, [1,2]) == None

@test
def findLeft05(): Bool = Array.findLeft(i -> i > 2, [6,-6]) == Some(6)

@test
def findLeft06(): Bool = Array.findLeft(i -> i > 2, [-6,6]) == Some(6)

@test
def findLeft07(): Bool = Array.findLeft(i -> i > 2, [6,7]) == Some(6)

/////////////////////////////////////////////////////////////////////////////
// findRight                                                               //
/////////////////////////////////////////////////////////////////////////////
@test
def findRight01(): Bool = Array.findRight(i -> i > 2, []: Array[Int32]) == None

@test
def findRight02(): Bool = Array.findRight(i -> i > 2, [1]) == None

@test
def findRight03(): Bool = Array.findRight(i -> i > 2, [3]) == Some(3)

@test
def findRight04(): Bool = Array.findRight(i -> i > 2, [1,2]) == None

@test
def findRight05(): Bool = Array.findRight(i -> i > 2, [6,-6]) == Some(6)

@test
def findRight06(): Bool = Array.findRight(i -> i > 2, [-6,6]) == Some(6)

@test
def findRight07(): Bool = Array.findRight(i -> i > 2, [6,7]) == Some(7)

/////////////////////////////////////////////////////////////////////////////
// findIndexOf                                                             //
/////////////////////////////////////////////////////////////////////////////
@test
def findIndexOf01(): Bool = Array.findIndexOf(i -> i > 2, []: Array[Int32]) == None

@test
def findIndexOf02(): Bool = Array.findIndexOf(i -> i > 2, [1]) == None

@test
def findIndexOf03(): Bool = Array.findIndexOf(i -> i > 2, [3]) == Some(0)

@test
def findIndexOf04(): Bool = Array.findIndexOf(i -> i > 2, [1,2]) == None

@test
def findIndexOf05(): Bool = Array.findIndexOf(i -> i > 2, [6,-6]) == Some(0)

@test
def findIndexOf06(): Bool = Array.findIndexOf(i -> i > 2, [-6,6]) == Some(1)

@test
def findIndexOf07(): Bool = Array.findIndexOf(i -> i > 2, [6,7]) == Some(0)

/////////////////////////////////////////////////////////////////////////////
// findIndexOfLeft                                                         //
/////////////////////////////////////////////////////////////////////////////
@test
def findIndexOfLeft01(): Bool = Array.findIndexOfLeft(i -> i > 2, []: Array[Int32]) == None

@test
def findIndexOfLeft02(): Bool = Array.findIndexOfLeft(i -> i > 2, [1]) == None

@test
def findIndexOfLeft03(): Bool = Array.findIndexOfLeft(i -> i > 2, [3]) == Some(0)

@test
def findIndexOfLeft04(): Bool = Array.findIndexOfLeft(i -> i > 2, [1,2]) == None

@test
def findIndexOfLeft05(): Bool = Array.findIndexOfLeft(i -> i > 2, [6,-6]) == Some(0)

@test
def findIndexOfLeft06(): Bool = Array.findIndexOfLeft(i -> i > 2, [-6,6]) == Some(1)

@test
def findIndexOfLeft07(): Bool = Array.findIndexOfLeft(i -> i > 2, [6,7]) == Some(0)

/////////////////////////////////////////////////////////////////////////////
// findIndexOfRight                                                        //
/////////////////////////////////////////////////////////////////////////////
@test
def findIndexOfRight01(): Bool = Array.findIndexOfRight(i -> i > 2, []: Array[Int32]) == None

@test
def findIndexOfRight02(): Bool = Array.findIndexOfRight(i -> i > 2, [1]) == None

@test
def findIndexOfRight03(): Bool = Array.findIndexOfRight(i -> i > 2, [3]) == Some(0)

@test
def findIndexOfRight04(): Bool = Array.findIndexOfRight(i -> i > 2, [1,2]) == None

@test
def findIndexOfRight05(): Bool = Array.findIndexOfRight(i -> i > 2, [6,-6]) == Some(0)

@test
def findIndexOfRight06(): Bool = Array.findIndexOfRight(i -> i > 2, [-6,6]) == Some(1)

@test
def findIndexOfRight07(): Bool = Array.findIndexOfRight(i -> i > 2, [6,7]) == Some(1)

/////////////////////////////////////////////////////////////////////////////
// filter                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def filter01(): Bool =
    let a = Array.filter(i -> i > 3, []: Array[Int32]);
    Array.sameElements(a, []: Array[Int32])

@test
def filter02(): Bool =
    let a = Array.filter(i -> i > 3, [2]);
    Array.sameElements(a, []: Array[Int32])

@test
def filter03(): Bool =
    let a = Array.filter(i -> i > 3, [4]);
    Array.sameElements(a, [4])

@test
def filter04(): Bool =
    let a = Array.filter(i -> i > 3, [1,3]);
    Array.sameElements(a, []: Array[Int32])

@test
def filter05(): Bool =
    let a = Array.filter(i -> i > 3, [1,8]);
    Array.sameElements(a, [8])

@test
def filter06(): Bool =
    let a = Array.filter(i -> i > 3, [8,1]);
    Array.sameElements(a, [8])

@test
def filter07(): Bool =
    let a = Array.filter(i -> i > 3, [8,9]);
    Array.sameElements(a, [8,9])

@test
def filter08(): Bool =
    let a = Array.filter(i -> i > 3, [1,4,11,2,-22,17]);
    Array.sameElements(a, [4,11,17])

/////////////////////////////////////////////////////////////////////////////
// filterMap                                                               //
/////////////////////////////////////////////////////////////////////////////
@test
def filterMap01(): Bool =
    let a = Array.filterMap(i -> if (i % 2 == 0) Some(i/2) else None, []: Array[Int32]);
    Array.sameElements(a, []: Array[Int32])

@test
def filterMap02(): Bool =
    let a = Array.filterMap(i -> if (i % 2 == 0) Some(i/2) else None, [1]);
    Array.sameElements(a, []: Array[Int32])

@test
def filterMap03(): Bool =
    let a = Array.filterMap(i -> if (i % 2 == 0) Some(i/2) else None, [2]);
    Array.sameElements(a, [1])

@test
def filterMap04(): Bool =
    let a = Array.filterMap(i -> if (i % 2 == 0) Some(i/2) else None, [1,3]);
    Array.sameElements(a, []: Array[Int32])

@test
def filterMap05(): Bool =
    let a = Array.filterMap(i -> if (i % 2 == 0) Some(i/2) else None, [1,4]);
    Array.sameElements(a, [2])

@test
def filterMap06(): Bool =
    let a = Array.filterMap(i -> if (i % 2 == 0) Some(i/2) else None, [6,-1]);
    Array.sameElements(a, [3])

@test
def filterMap07(): Bool =
    let a = Array.filterMap(i -> if (i % 2 == 0) Some(i/2) else None, [8,6]);
    Array.sameElements(a, [4,3])

@test
def filterMap08(): Bool =
    let a = Array.filterMap(i -> if (i % 2 == 0) Some(i/2) else None, [0,1,2,3,4,5,10]);
    Array.sameElements(a, [0,1,2,5])

/////////////////////////////////////////////////////////////////////////////
// findMap                                                                 //
/////////////////////////////////////////////////////////////////////////////
@test
def findMap01(): Bool = Array.findMap(i -> if (i % 2 == 0) Some(i/2) else None, []: Array[Int32]) == None

@test
def findMap02(): Bool = Array.findMap(i -> if (i % 2 == 0) Some(i/2) else None, [1]) == None

@test
def findMap03(): Bool = Array.findMap(i -> if (i % 2 == 0) Some(i/2) else None, [2]) == Some(1)

@test
def findMap04(): Bool = Array.findMap(i -> if (i % 2 == 0) Some(i/2) else None, [1,3]) == None

@test
def findMap05(): Bool = Array.findMap(i -> if (i % 2 == 0) Some(i/2) else None, [1,4]) == Some(2)

@test
def findMap06(): Bool = Array.findMap(i -> if (i % 2 == 0) Some(i/2) else None, [6,-1]) == Some(3)

@test
def findMap07(): Bool = Array.findMap(i -> if (i % 2 == 0) Some(i/2) else None, [8,6]) == Some(4)

@test
def findMap08(): Bool = Array.findMap(i -> if (i % 2 == 0) Some(i/2) else None, [0,1,2,3,4,5,10]) == Some(0)

/////////////////////////////////////////////////////////////////////////////
// rotateLeft                                                              //
/////////////////////////////////////////////////////////////////////////////
@test
def rotateLeft01(): Bool =
    let a = Array.rotateLeft(0, []: Array[Int32]);
    Array.sameElements(a, []: Array[Int32])

@test
def rotateLeft02(): Bool =
    let a = Array.rotateLeft(1, []: Array[Int32]);
    Array.sameElements(a, []: Array[Int32])

@test
def rotateLeft03(): Bool =
    let a = Array.rotateLeft(0, [1]);
    Array.sameElements(a, [1])

@test
def rotateLeft04(): Bool =
    let a = Array.rotateLeft(0, [1,2]);
    Array.sameElements(a, [1,2])

@test
def rotateLeft05(): Bool =
    let a = Array.rotateLeft(1, [1,2]);
    Array.sameElements(a, [2,1])

@test
def rotateLeft06(): Bool =
    let a = Array.rotateLeft(2, [1,2]);
    Array.sameElements(a, [1,2])

@test
def rotateLeft07(): Bool =
    let a = Array.rotateLeft(3, [1,2]);
    Array.sameElements(a, [2,1])

@test
def rotateLeft08(): Bool =
    let a = Array.rotateLeft(-1, [1,2]);
    Array.sameElements(a, [2,1])

@test
def rotateLeft09(): Bool =
    let a = Array.rotateLeft(0, [1,2,3]);
    Array.sameElements(a, [1,2,3])

@test
def rotateLeft10(): Bool =
    let a = Array.rotateLeft(1, [1,2,3]);
    Array.sameElements(a, [2,3,1])

@test
def rotateLeft11(): Bool =
    let a = Array.rotateLeft(2, [1,2,3]);
    Array.sameElements(a, [3,1,2])

/////////////////////////////////////////////////////////////////////////////
// rotateRight                                                             //
/////////////////////////////////////////////////////////////////////////////
@test
def rotateRight01(): Bool =
    let a = Array.rotateRight(0, []: Array[Int32]);
    Array.sameElements(a, []: Array[Int32])

@test
def rotateRight02(): Bool =
    let a = Array.rotateRight(1, []: Array[Int32]);
    Array.sameElements(a, []: Array[Int32])

@test
def rotateRight03(): Bool =
    let a = Array.rotateRight(0, [1]);
    Array.sameElements(a, [1])

@test
def rotateRight04(): Bool =
    let a = Array.rotateRight(0, [1,2]);
    Array.sameElements(a, [1,2])

@test
def rotateRight05(): Bool =
    let a = Array.rotateRight(1, [1,2]);
    Array.sameElements(a, [2,1])

@test
def rotateRight06(): Bool =
    let a = Array.rotateRight(2, [1,2]);
    Array.sameElements(a, [1,2])

@test
def rotateRight07(): Bool =
    let a = Array.rotateRight(3, [1,2]);
    Array.sameElements(a, [2,1])

@test
def rotateRight08(): Bool =
    let a = Array.rotateRight(-1, [1,2]);
    Array.sameElements(a, [2,1])

@test
def rotateRight09(): Bool =
    let a = Array.rotateRight(0, [1,2,3]);
    Array.sameElements(a, [1,2,3])

@test
def rotateRight10(): Bool =
    let a = Array.rotateRight(1, [1,2,3]);
    Array.sameElements(a, [3,1,2])

@test
def rotateRight11(): Bool =
    let a = Array.rotateRight(2, [1,2,3]);
    Array.sameElements(a, [2,3,1])

/////////////////////////////////////////////////////////////////////////////
// replace                                                                 //
/////////////////////////////////////////////////////////////////////////////
@test
def replace01(): Bool =
    let a = Array.replace(3, 4, []);
    Array.sameElements(a, []: Array[Int32])

@test
def replace02(): Bool =
    let a = Array.replace(3, 4, [1]);
    Array.sameElements(a, [1])

@test
def replace03(): Bool =
    let a = Array.replace(3, 4, [3]);
    Array.sameElements(a, [4])

@test
def replace04(): Bool =
    let a = Array.replace(3, 4, [4]);
    Array.sameElements(a, [4])

@test
def replace05(): Bool =
    let a = Array.replace(3, 4, [1, 2]);
    Array.sameElements(a, [1,2])

@test
def replace06(): Bool =
    let a = Array.replace(3, 4, [1,3]);
    Array.sameElements(a, [1,4])

@test
def replace07(): Bool =
    let a = Array.replace(3, 4, [3,4]);
    Array.sameElements(a, [4,4])

@test
def replace08(): Bool =
    let a = Array.replace(3, 4, [3,3]);
    Array.sameElements(a, [4,4])

/////////////////////////////////////////////////////////////////////////////
// zip                                                                     //
/////////////////////////////////////////////////////////////////////////////
@test
def zip01(): Bool =
    let a = Array.zip([]: Array[Int32], []: Array[Int32]);
    Array.sameElements(a, []: Array[(Int32,Int32)])

@test
def zip02(): Bool =
    let a = Array.zip([1], []: Array[Int32]);
    Array.sameElements(a, []: Array[(Int32,Int32)])

@test
def zip03(): Bool =
    let a = Array.zip([]: Array[Int32], [2]);
    Array.sameElements(a, []: Array[(Int32,Int32)])

@test
def zip04(): Bool =
    let a = Array.zip([1], [2]);
    Array.sameElements(a, [(1, 2)])

@test
def zip05(): Bool =
    let a = Array.zip([1,3], [2,4]);
    Array.sameElements(a, [(1, 2), (3, 4)])

@test
def zip06(): Bool =
    let a = Array.zip([1,3,5], [2,4,6]);
    Array.sameElements(a, [(1, 2), (3, 4), (5, 6)])

@test
def zip07(): Bool =
    let a = Array.zip([1,3,5,7], [2,4,6,8]);
    Array.sameElements(a, [(1, 2), (3, 4), (5, 6), (7, 8)])

/////////////////////////////////////////////////////////////////////////////
// zipWith                                                                 //
/////////////////////////////////////////////////////////////////////////////
@test
def zipWith01(): Bool =
    let arr = Array.zipWith((a, b) -> if (b) a+1 else a, []: Array[Int32], []: Array[Bool]);
    Array.sameElements(arr, []: Array[Int32])

@test
def zipWith02(): Bool =
    let arr = Array.zipWith((a, b) -> if (b) a+1 else a, [1], []: Array[Bool]);
    Array.sameElements(arr, []: Array[Int32])

@test
def zipWith03(): Bool =
    let arr = Array.zipWith((a, b) -> if (b) a+1 else a, []: Array[Int32], [true]);
    Array.sameElements(arr, []: Array[Int32])

@test
def zipWith04(): Bool =
    let arr = Array.zipWith((a, b) -> if (b) a+1 else a, [1], [true]);
    Array.sameElements(arr, [2])

@test
def zipWith05(): Bool =
    let arr = Array.zipWith((a, b) -> if (b) a+1 else a, [1], [false]);
    Array.sameElements(arr, [1])

@test
def zipWith06(): Bool =
    let arr = Array.zipWith((a, b) -> if (b) a+1 else a, [1, 2, 3, 4, 5, 6, 7, 8],
                      [false, true, true, false, false, true, true, true]);
    Array.sameElements(arr, [1, 3, 4, 4, 5, 7, 8, 9])

/////////////////////////////////////////////////////////////////////////////
// transpose                                                               //
/////////////////////////////////////////////////////////////////////////////
@test
def transpose01(): Bool =
    let a = Array.transpose([]);
    let b = Array.toList(a) |> List.map(Array.toList);
    b == Nil

@test
def transpose02(): Bool =
    let a: Array[Array[Int32]] = Array.transpose([[]]);
    let b = Array.toList(a) |> List.map(Array.toList);
    b == Nil :: Nil

@test
def transpose03(): Bool =
    let a: Array[Array[Int32]] = Array.transpose([[], []]);
    let b = Array.toList(a) |> List.map(Array.toList);
    b == Nil :: Nil :: Nil

@test
def transpose04(): Bool =
    let a: Array[Array[Int32]] = Array.transpose([[], [], []]);
    let b = Array.toList(a) |> List.map(Array.toList);
    b == Nil :: Nil :: Nil :: Nil

@test
def transpose05(): Bool =
    let a = Array.transpose([ [1] ]);
    let b = Array.toList(a) |> List.map(Array.toList);
    b == (1 :: Nil) :: Nil

@test
def transpose06(): Bool =
    let a = Array.transpose([[1,2]]);
    let b = Array.toList(a) |> List.map(Array.toList);
    b == (1 :: Nil) :: (2 :: Nil) :: Nil

@test
def transpose07(): Bool =
    let a = Array.transpose([[1, 2, 3]]);
    let b = Array.toList(a) |> List.map(Array.toList);
    b == (1 :: Nil) :: (2 :: Nil) :: (3 :: Nil) :: Nil

@test
def transpose08(): Bool =
    let a = Array.transpose([[1, 2, 3, 4]]);
    let b = Array.toList(a) |> List.map(Array.toList);
    b == (1 :: Nil) :: (2 :: Nil) :: (3 :: Nil) :: (4 :: Nil) :: Nil

@test
def transpose09(): Bool =
    let a = Array.transpose([[1], [2]]);
    let b = Array.toList(a) |> List.map(Array.toList);
    b ==   (1 :: 2 :: Nil) :: Nil

@test
def transpose10(): Bool =
    let a = Array.transpose([[1], [2], [3]]);
    let b = Array.toList(a) |> List.map(Array.toList);
    b ==  (1 :: 2 :: 3 :: Nil) :: Nil

@test
def transpose11(): Bool =
    let a = Array.transpose([[1], [2], [3], [4]]);
    let b = Array.toList(a) |> List.map(Array.toList);
    b == (1 :: 2 :: 3 :: 4 :: Nil) :: Nil

@test
def transpose12(): Bool =
    let a = Array.transpose([[1, 2], [3, 4]]);
    let b = Array.toList(a) |> List.map(Array.toList);
    b == (1 :: 3 :: Nil) :: (2 :: 4 :: Nil) :: Nil

@test
def transpose13(): Bool =
    let a = Array.transpose([[1, 2, 3], [4, 5, 6]]);
    let b = Array.toList(a) |> List.map(Array.toList);
    b == (1 :: 4 :: Nil) :: (2 :: 5 :: Nil) :: (3 :: 6 :: Nil) :: Nil

@test
def transpose14(): Bool =
    let a = Array.transpose([[1, 2, 3, 4], [5, 6, 7, 8]]);
    let b = Array.toList(a) |> List.map(Array.toList);
    b == (1 :: 5 :: Nil) :: (2 :: 6 :: Nil) :: (3 :: 7 :: Nil) :: (4 :: 8 :: Nil) :: Nil

@test
def transpose15(): Bool =
    let a = Array.transpose([[1, 2, 3, 4, 5], [6, 7, 8, 9, 10]]);
    let b = Array.toList(a) |> List.map(Array.toList);
    b == (1 :: 6 :: Nil) :: (2 :: 7 :: Nil) :: (3 :: 8 :: Nil) :: (4 :: 9 :: Nil) :: (5 :: 10 :: Nil) :: Nil

@test
def transpose16(): Bool =
    let a = Array.transpose([[1, 2], [3, 4], [5, 6]]);
    let b = Array.toList(a) |> List.map(Array.toList);
    b == (1 :: 3 :: 5 :: Nil) :: (2 :: 4 :: 6 :: Nil) :: Nil

@test
def transpose17(): Bool =
    let a = Array.transpose([[1, 2], [3, 4], [5, 6], [7,8]]);
    let b = Array.toList(a) |> List.map(Array.toList);
    b == (1 :: 3 :: 5 :: 7 :: Nil) :: (2 :: 4 :: 6 :: 8 :: Nil) :: Nil

@test
def transpose18(): Bool =
    let a = Array.transpose([[1, 2], [3, 4], [5, 6], [7, 8], [9, 10]]);
    let b = Array.toList(a) |> List.map(Array.toList);
    b == (1 :: 3 :: 5 :: 7 :: 9 :: Nil) :: (2 :: 4 :: 6 :: 8 :: 10 :: Nil) :: Nil

@test
def transpose19(): Bool =
    let a = Array.transpose([[1, 2, 3], [4, 5, 6], [7, 8, 9]]);
    let b = Array.toList(a) |> List.map(Array.toList);
    b == (1 :: 4 :: 7 :: Nil) :: (2 :: 5 :: 8 :: Nil) :: (3 :: 6 :: 9 :: Nil) :: Nil

@test
def transpose20(): Bool =
    let a = Array.transpose([[1, 2, 3], [4, 5], [7, 8, 9]]);
    let b = Array.toList(a) |> List.map(Array.toList);
    b == (1 :: 2 :: 3 :: Nil) :: (4 :: 5 :: Nil) :: (7 :: 8 :: 9 :: Nil) :: Nil

@test
def transpose21(): Bool =
    let a = Array.transpose([[1, 2, 3], [], [7, 8, 9]]);
    let b = Array.toList(a) |> List.map(Array.toList);
    b == (1 :: 2 :: 3 :: Nil) :: Nil :: (7 :: 8 :: 9 :: Nil) :: Nil

@test
def transpose22(): Bool =
    let a = Array.transpose([[1, 2, 3], [4, 5, 6], [7, 8, 9, 10]]);
    let b = Array.toList(a) |> List.map(Array.toList);
    b == (1 :: 2 :: 3 :: Nil) :: (4 :: 5 :: 6 :: Nil) :: (7 :: 8 :: 9 :: 10 :: Nil) :: Nil

/*
 * Copyright 2017 Liam Palmer
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

/////////////////////////////////////////////////////////////////////////////
// isEmpty                                                                 //
/////////////////////////////////////////////////////////////////////////////
@test
def isEmpty01: Bool = assert!(List.isEmpty(Nil))

@test
def isEmpty02: Bool = assertNot!(List.isEmpty(1 :: Nil))

@test
def isEmpty03: Bool = assertNot!(List.isEmpty(1 :: 2 :: Nil))

/////////////////////////////////////////////////////////////////////////////
// headOpt                                                                 //
/////////////////////////////////////////////////////////////////////////////
@test
def headOpt01: Bool = List.headOpt(Nil) `assertEq!` None

@test
def headOpt02: Bool = List.headOpt(1 :: Nil) `assertEq!` Some(1)

@test
def headOpt03: Bool = List.headOpt(2 :: 1 :: Nil) `assertEq!` Some(2)

@test
def headOpt04: Bool = List.headOpt(3 :: 2 :: 1 :: Nil) `assertEq!` Some(3)

/////////////////////////////////////////////////////////////////////////////
// lastOpt                                                                 //
/////////////////////////////////////////////////////////////////////////////
@test
def lastOpt01: Bool = List.lastOpt(Nil) `assertEq!` None

@test
def lastOpt02: Bool = List.lastOpt(1 :: Nil) `assertEq!` Some(1)

@test
def lastOpt03: Bool = List.lastOpt(1 :: 2 :: Nil) `assertEq!` Some(2)

@test
def lastOpt04: Bool = List.lastOpt(1 :: 2 :: 3 :: Nil) `assertEq!` Some(3)

/////////////////////////////////////////////////////////////////////////////
// length                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def length01: Bool = List.length(Nil) `assertEq!` 0

@test
def length02: Bool = List.length(1 :: Nil) `assertEq!` 1

@test
def length03: Bool = List.length(1 :: 2 :: Nil) `assertEq!` 2

@test
def length04: Bool = List.length(1 :: 2 :: 3 :: Nil) `assertEq!` 3

/////////////////////////////////////////////////////////////////////////////
// append                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def append01: Bool = List.append(Nil, Nil) `assertEq!` Nil

@test
def append02: Bool = List.append(Nil, 1 :: Nil) `assertEq!` 1 :: Nil

@test
def append03: Bool = List.append(Nil, 1 :: 2 :: Nil) `assertEq!` 1 :: 2 :: Nil

@test
def append04: Bool = List.append(1 :: Nil, Nil) `assertEq!` 1 :: Nil

@test
def append05: Bool = List.append(1 :: 2 :: Nil, Nil) `assertEq!` 1 :: 2 :: Nil

@test
def append06: Bool = List.append(1 :: Nil, 2 :: Nil) `assertEq!` 1 :: 2 :: Nil

@test
def append07: Bool = List.append(1 :: 2 :: Nil, 3 :: Nil) `assertEq!` 1 :: 2 :: 3 :: Nil

@test
def append08: Bool = List.append(1 :: Nil, 2 :: 3 :: Nil) `assertEq!` 1 :: 2 :: 3 :: Nil

/////////////////////////////////////////////////////////////////////////////
// at                                                                      //
/////////////////////////////////////////////////////////////////////////////
@test
def at01: Bool = List.at(0, 1 :: Nil) `assertEq!` 1

@test
def at02: Bool = List.at(0, 1 :: 2 :: 3 :: Nil) `assertEq!` 1

@test
def at03: Bool = List.at(1, 1 :: 2 :: 3 :: Nil) `assertEq!` 2

@test
def at04: Bool = List.at(2, 1 :: 2 :: 3 :: Nil) `assertEq!` 3

/////////////////////////////////////////////////////////////////////////////
// remove                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def remove01: Bool = List.remove(1, Nil) `assertEq!` Nil

@test
def remove02: Bool = List.remove(1, 1 :: Nil) `assertEq!` Nil

@test
def remove03: Bool = List.remove(1, 2 :: Nil) `assertEq!` 2 :: Nil

@test
def remove04: Bool = List.remove(1, 1 :: 2 :: Nil) `assertEq!` 2 :: Nil

@test
def remove05: Bool = List.remove(2, 1 :: 2 :: Nil) `assertEq!` 1 :: Nil

@test
def remove06: Bool = List.remove(3, 1 :: 2 :: Nil) `assertEq!` 1 :: 2 :: Nil

@test
def remove07: Bool = List.remove(1, 1 :: 2 :: 3 :: Nil) `assertEq!` 2 :: 3 :: Nil

@test
def remove08: Bool = List.remove(1, 1 :: 2 :: 1 :: Nil) `assertEq!` 2 :: 1 :: Nil

@test
def remove09: Bool = List.remove(1, 1 :: 1 :: 2 :: Nil) `assertEq!` 1 :: 2 :: Nil

/////////////////////////////////////////////////////////////////////////////
// removeIndex                                                             //
/////////////////////////////////////////////////////////////////////////////
@test
def removeIndex01: Bool = List.removeIndex(0, Nil) `assertEq!` Nil

@test
def removeIndex02: Bool = List.removeIndex(1, Nil) `assertEq!` Nil

@test
def removeIndex03: Bool = List.removeIndex(-1, 1 :: Nil) `assertEq!` 1 :: Nil

@test
def removeIndex04: Bool = List.removeIndex(0, 1 :: Nil) `assertEq!` Nil

@test
def removeIndex05: Bool = List.removeIndex(1, 1 :: Nil) `assertEq!` 1 :: Nil

@test
def removeIndex06: Bool = List.removeIndex(-1, 1 :: 2 :: Nil) `assertEq!` 1 :: 2 :: Nil

@test
def removeIndex07: Bool = List.removeIndex(0, 1 :: 2 :: Nil) `assertEq!` 2 :: Nil

@test
def removeIndex08: Bool = List.removeIndex(1, 1 :: 2 :: Nil) `assertEq!` 1 :: Nil

@test
def removeIndex09: Bool = List.removeIndex(2, 1 :: 2 :: Nil) `assertEq!` 1 :: 2 :: Nil

/////////////////////////////////////////////////////////////////////////////
// memberOf                                                                //
/////////////////////////////////////////////////////////////////////////////
@test
def memberOf01: Bool = assertNot!(List.memberOf(0, Nil))

@test
def memberOf02: Bool = assertNot!(List.memberOf(0, 1 :: Nil))

@test
def memberOf03: Bool = assert!(List.memberOf(0, 0 :: Nil))

@test
def memberOf04: Bool = assertNot!(List.memberOf(0, 1 :: 2 :: Nil))

@test
def memberOf05: Bool = assert!(List.memberOf(1, 1 :: 2 :: Nil))

@test
def memberOf06: Bool = assert!(List.memberOf(2, 1 :: 2 :: Nil))

@test
def memberOf07: Bool = assertNot!(List.memberOf(3, 1 :: 2 :: Nil))

/////////////////////////////////////////////////////////////////////////////
// indexOf                                                                 //
/////////////////////////////////////////////////////////////////////////////
@test
def indexOf01: Bool = List.indexOf(0, Nil) `assertEq!` -1

@test
def indexOf02: Bool = List.indexOf(0, 1 :: Nil) `assertEq!` -1

@test
def indexOf03: Bool = List.indexOf(1, 1 :: Nil) `assertEq!` 0

@test
def indexOf04: Bool = List.indexOf(0, 1 :: 2 :: Nil) `assertEq!` -1

@test
def indexOf05: Bool = List.indexOf(1, 1 :: 2 :: Nil) `assertEq!` 0

@test
def indexOf06: Bool = List.indexOf(2, 1 :: 2 :: Nil) `assertEq!` 1

/////////////////////////////////////////////////////////////////////////////
// find                                                                    //
/////////////////////////////////////////////////////////////////////////////
@test
def find01: Bool = List.find(i -> i > 2, Nil) `assertEq!` None

@test
def find02: Bool = List.find(i -> i > 2, 1 :: Nil) `assertEq!` None

@test
def find03: Bool = List.find(i -> i > 2, 3 :: Nil) `assertEq!` Some(3)

@test
def find04: Bool = List.find(i -> i > 2, 1 :: 2 :: Nil) `assertEq!` None

@test
def find05: Bool = List.find(i -> i > 2, 6 :: -6 :: Nil) `assertEq!` Some(6)

@test
def find06: Bool = List.find(i -> i > 2, -6 :: 6 :: Nil) `assertEq!` Some(6)

@test
def find07: Bool = List.find(i -> i > 2, 6 :: 7 :: Nil) `assertEq!` Some(6)

/////////////////////////////////////////////////////////////////////////////
// findLeft                                                                //
/////////////////////////////////////////////////////////////////////////////
@test
def findLeft01: Bool = List.findLeft(i -> i > 2, Nil) `assertEq!` None

@test
def findLeft02: Bool = List.findLeft(i -> i > 2, 1 :: Nil) `assertEq!` None

@test
def findLeft03: Bool = List.findLeft(i -> i > 2, 3 :: Nil) `assertEq!` Some(3)

@test
def findLeft04: Bool = List.findLeft(i -> i > 2, 1 :: 2 :: Nil) `assertEq!` None

@test
def findLeft05: Bool = List.findLeft(i -> i > 2, 6 :: -6 :: Nil) `assertEq!` Some(6)

@test
def findLeft06: Bool = List.findLeft(i -> i > 2, -6 :: 6 :: Nil) `assertEq!` Some(6)

@test
def findLeft07: Bool = List.findLeft(i -> i > 2, 6 :: 7 :: Nil) `assertEq!` Some(6)

/////////////////////////////////////////////////////////////////////////////
// findRight                                                               //
/////////////////////////////////////////////////////////////////////////////
@test
def findRight01: Bool = List.findRight(i -> i > 2, Nil) `assertEq!` None

@test
def findRight02: Bool = List.findRight(i -> i > 2, 1 :: Nil) `assertEq!` None

@test
def findRight03: Bool = List.findRight(i -> i > 2, 3 :: Nil) `assertEq!` Some(3)

@test
def findRight04: Bool = List.findRight(i -> i > 2, 1 :: 2 :: Nil) `assertEq!` None

@test
def findRight05: Bool = List.findRight(i -> i > 2, 6 :: -6 :: Nil) `assertEq!` Some(6)

@test
def findRight06: Bool = List.findRight(i -> i > 2, -6 :: 6 :: Nil) `assertEq!` Some(6)

@test
def findRight07: Bool = List.findRight(i -> i > 2, 6 :: 7 :: Nil) `assertEq!` Some(7)

/////////////////////////////////////////////////////////////////////////////
// range                                                                   //
/////////////////////////////////////////////////////////////////////////////
@test
def range01: Bool = List.range(1, 0) `assertEq!` Nil

@test
def range02: Bool = List.range(1, 1) `assertEq!` Nil

@test
def range03: Bool = List.range(1, 2) `assertEq!` 1 :: Nil

@test
def range04: Bool = List.range(1, 3) `assertEq!` 1 :: 2 :: Nil

@test
def range05: Bool = List.range(1, 4) `assertEq!` 1 :: 2 :: 3 :: Nil

/////////////////////////////////////////////////////////////////////////////
// repeat                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def repeat01: Bool = List.repeat(1, -1) `assertEq!` Nil

@test
def repeat02: Bool = List.repeat(1, 0) `assertEq!` Nil

@test
def repeat03: Bool = List.repeat(1, 1) `assertEq!` 1 :: Nil

@test
def repeat04: Bool = List.repeat(1, 2) `assertEq!` 1 :: 1 :: Nil

@test
def repeat05: Bool = List.repeat(1, 3) `assertEq!` 1 :: 1 :: 1 :: Nil

/////////////////////////////////////////////////////////////////////////////
// scan                                                                    //
/////////////////////////////////////////////////////////////////////////////
@test
def scan01: Bool = List.scan((i, b) -> if (b) i+1 else i+2, 1, Nil) `assertEq!` 1 :: Nil

@test
def scan02: Bool = List.scan((i, b) -> if (b) i+1 else i+2, 1, false :: Nil) `assertEq!` 1 :: 3 :: Nil

@test
def scan03: Bool = List.scan((i, b) -> if (b) i+1 else i+2, 1, true :: Nil) `assertEq!` 1 :: 2 :: Nil

@test
def scan04: Bool = List.scan((i, b) -> if (b) i+1 else i+2, 1, false :: false :: Nil) `assertEq!` 1 :: 3 :: 5 :: Nil

@test
def scan05: Bool = List.scan((i, b) -> if (b) i+1 else i+2, 1, false :: true :: Nil) `assertEq!` 1 :: 3 :: 4 :: Nil

@test
def scan06: Bool = List.scan((i, b) -> if (b) i+1 else i+2, 1, true :: false :: Nil) `assertEq!` 1 :: 2 :: 4 :: Nil

@test
def scan07: Bool = List.scan((i, b) -> if (b) i+1 else i+2, 1, true :: true :: Nil) `assertEq!` 1 :: 2 :: 3 :: Nil

/////////////////////////////////////////////////////////////////////////////
// scanLeft                                                                //
/////////////////////////////////////////////////////////////////////////////
@test
def scanLeft01: Bool = List.scanLeft((i, b) -> if (b) i+1 else i+2, 1, Nil) `assertEq!` 1 :: Nil

@test
def scanLeft02: Bool = List.scanLeft((i, b) -> if (b) i+1 else i+2, 1, false :: Nil) `assertEq!` 1 :: 3 :: Nil

@test
def scanLeft03: Bool = List.scanLeft((i, b) -> if (b) i+1 else i+2, 1, true :: Nil) `assertEq!` 1 :: 2 :: Nil

@test
def scanLeft04: Bool = List.scanLeft((i, b) -> if (b) i+1 else i+2, 1, false :: false :: Nil) `assertEq!` 1 :: 3 :: 5 :: Nil

@test
def scanLeft05: Bool = List.scanLeft((i, b) -> if (b) i+1 else i+2, 1, false :: true :: Nil) `assertEq!` 1 :: 3 :: 4 :: Nil

@test
def scanLeft06: Bool = List.scanLeft((i, b) -> if (b) i+1 else i+2, 1, true :: false :: Nil) `assertEq!` 1 :: 2 :: 4 :: Nil

@test
def scanLeft07: Bool = List.scanLeft((i, b) -> if (b) i+1 else i+2, 1, true :: true :: Nil) `assertEq!` 1 :: 2 :: 3 :: Nil

/////////////////////////////////////////////////////////////////////////////
// scanRight                                                               //
/////////////////////////////////////////////////////////////////////////////
@test
def scanRight01: Bool = List.scanRight((b, i) -> if (b) i+1 else i+2, 1, Nil) `assertEq!` 1 :: Nil

@test
def scanRight02: Bool = List.scanRight((b, i) -> if (b) i+1 else i+2, 1, false :: Nil) `assertEq!` 3 :: 1 :: Nil

@test
def scanRight03: Bool = List.scanRight((b, i) -> if (b) i+1 else i+2, 1, true :: Nil) `assertEq!` 2 :: 1 :: Nil

@test
def scanRight04: Bool = List.scanRight((b, i) -> if (b) i+1 else i+2, 1, false :: false :: Nil) `assertEq!` 5 :: 3 :: 1 :: Nil

@test
def scanRight05: Bool = List.scanRight((b, i) -> if (b) i+1 else i+2, 1, false :: true :: Nil) `assertEq!` 4 :: 2 :: 1 :: Nil

@test
def scanRight06: Bool = List.scanRight((b, i) -> if (b) i+1 else i+2, 1, true :: false :: Nil) `assertEq!` 4 :: 3 :: 1 :: Nil

@test
def scanRight07: Bool = List.scanRight((b, i) -> if (b) i+1 else i+2, 1, true :: true :: Nil) `assertEq!` 3 :: 2 :: 1 :: Nil

/////////////////////////////////////////////////////////////////////////////
// map                                                                     //
/////////////////////////////////////////////////////////////////////////////
@test
def map01: Bool = List.map(i -> i > 2, Nil) `assertEq!` Nil

@test
def map02: Bool = List.map(i -> i > 2, 1 :: Nil) `assertEq!` false :: Nil

@test
def map03: Bool = List.map(i -> i > 2, 3 :: Nil) `assertEq!` true :: Nil

@test
def map04: Bool = List.map(i -> i > 2, 1 :: 2 :: Nil) `assertEq!` false :: false :: Nil

@test
def map05: Bool = List.map(i -> i > 2, 1 :: 8 :: Nil) `assertEq!` false :: true :: Nil

@test
def map06: Bool = List.map(i -> i > 2, 8 :: 1 :: Nil) `assertEq!` true :: false :: Nil

@test
def map07: Bool = List.map(i -> i > 2, 7 :: 8 :: Nil) `assertEq!` true :: true :: Nil

/////////////////////////////////////////////////////////////////////////////
// mapWithIndex                                                            //
/////////////////////////////////////////////////////////////////////////////
@test
def mapWithIndex01: Bool = List.mapWithIndex((e, i) -> if (i < 1) e > 2 else e <= 2, Nil) `assertEq!` Nil

@test
def mapWithIndex02: Bool = List.mapWithIndex((e, i) -> if (i < 1) e > 2 else e <= 2, 1 :: Nil) `assertEq!` false :: Nil

@test
def mapWithIndex03: Bool = List.mapWithIndex((e, i) -> if (i < 1) e > 2 else e <= 2, 3 :: Nil) `assertEq!` true :: Nil

@test
def mapWithIndex04: Bool = List.mapWithIndex((e, i) -> if (i < 1) e > 2 else e <= 2, 1 :: 2 :: Nil) `assertEq!` false :: true :: Nil

@test
def mapWithIndex05: Bool = List.mapWithIndex((e, i) -> if (i < 1) e > 2 else e <= 2, 1 :: 8 :: Nil) `assertEq!` false :: false :: Nil

@test
def mapWithIndex06: Bool = List.mapWithIndex((e, i) -> if (i < 1) e > 2 else e <= 2, 8 :: 1 :: Nil) `assertEq!` true :: true :: Nil

@test
def mapWithIndex07: Bool = List.mapWithIndex((e, i) -> if (i < 1) e > 2 else e <= 2, 7 :: 8 :: Nil) `assertEq!` true :: false :: Nil

/////////////////////////////////////////////////////////////////////////////
// flatMap                                                                 //
/////////////////////////////////////////////////////////////////////////////
@test
def flatMap01: Bool = List.flatMap(i -> List.repeat(i, i), Nil) `assertEq!` Nil

@test
def flatMap02: Bool = List.flatMap(i -> List.repeat(i, i), 0 :: Nil) `assertEq!` Nil

@test
def flatMap03: Bool = List.flatMap(i -> List.repeat(i, i), 1 :: Nil) `assertEq!` 1 :: Nil

@test
def flatMap04: Bool = List.flatMap(i -> List.repeat(i, i), 2 :: Nil) `assertEq!` 2 :: 2 :: Nil

@test
def flatMap05: Bool = List.flatMap(i -> List.repeat(i, i), 1 :: 2 :: Nil) `assertEq!` 1 :: 2 :: 2 :: Nil

@test
def flatMap06: Bool = List.flatMap(i -> List.repeat(i, i), 2 :: 3 :: Nil) `assertEq!` 2 :: 2 :: 3 :: 3 :: 3 :: Nil

/////////////////////////////////////////////////////////////////////////////
// reverse                                                                 //
/////////////////////////////////////////////////////////////////////////////
@test
def reverse01: Bool = List.reverse(Nil) `assertEq!` Nil

@test
def reverse02: Bool = List.reverse(1 :: Nil) `assertEq!` 1 :: Nil

@test
def reverse03: Bool = List.reverse(1 :: 2 :: Nil) `assertEq!` 2 :: 1 :: Nil

@test
def reverse04: Bool = List.reverse(1 :: 1 :: Nil) `assertEq!` 1 :: 1 :: Nil

@test
def reverse05: Bool = List.reverse(1 :: 2 :: 3 :: Nil) `assertEq!` 3 :: 2 :: 1 :: Nil

@test
def reverse06: Bool = List.reverse(1 :: 2 :: 3 :: 4 :: Nil) `assertEq!` 4 :: 3 :: 2 :: 1 :: Nil

/////////////////////////////////////////////////////////////////////////////
// rotateLeft                                                              //
/////////////////////////////////////////////////////////////////////////////
@test
def rotateLeft01: Bool = List.rotateLeft(0, Nil) `assertEq!` Nil

@test
def rotateLeft02: Bool = List.rotateLeft(1, Nil) `assertEq!` Nil

@test
def rotateLeft03: Bool = List.rotateLeft(0, 1 :: Nil) `assertEq!` 1 :: Nil

@test
def rotateLeft04: Bool = List.rotateLeft(0, 1 :: 2 :: Nil) `assertEq!` 1 :: 2 :: Nil

@test
def rotateLeft05: Bool = List.rotateLeft(1, 1 :: 2 :: Nil) `assertEq!` 2 :: 1 :: Nil

@test
def rotateLeft06: Bool = List.rotateLeft(2, 1 :: 2 :: Nil) `assertEq!` 1 :: 2 :: Nil

@test
def rotateLeft07: Bool = List.rotateLeft(3, 1 :: 2 :: Nil) `assertEq!` 2 :: 1 :: Nil

@test
def rotateLeft08: Bool = List.rotateLeft(-1, 1 :: 2 :: Nil) `assertEq!` 2 :: 1 :: Nil

@test
def rotateLeft09: Bool = List.rotateLeft(0, 1 :: 2 :: 3 :: Nil) `assertEq!` 1 :: 2 :: 3 :: Nil

@test
def rotateLeft10: Bool = List.rotateLeft(1, 1 :: 2 :: 3 :: Nil) `assertEq!` 2 :: 3 :: 1 :: Nil

@test
def rotateLeft11: Bool = List.rotateLeft(2, 1 :: 2 :: 3 :: Nil) `assertEq!` 3 :: 1 :: 2 :: Nil

/////////////////////////////////////////////////////////////////////////////
// rotateRight                                                             //
/////////////////////////////////////////////////////////////////////////////
@test
def rotateRight01: Bool = List.rotateRight(0, Nil) `assertEq!` Nil

@test
def rotateRight02: Bool = List.rotateRight(1, Nil) `assertEq!` Nil

@test
def rotateRight03: Bool = List.rotateRight(0, 1 :: Nil) `assertEq!` 1 :: Nil

@test
def rotateRight04: Bool = List.rotateRight(0, 1 :: 2 :: Nil) `assertEq!` 1 :: 2 :: Nil

@test
def rotateRight05: Bool = List.rotateRight(1, 1 :: 2 :: Nil) `assertEq!` 2 :: 1 :: Nil

@test
def rotateRight06: Bool = List.rotateRight(2, 1 :: 2 :: Nil) `assertEq!` 1 :: 2 :: Nil

@test
def rotateRight07: Bool = List.rotateRight(3, 1 :: 2 :: Nil) `assertEq!` 2 :: 1 :: Nil

@test
def rotateRight08: Bool = List.rotateRight(-1, 1 :: 2 :: Nil) `assertEq!` 2 :: 1 :: Nil

@test
def rotateRight09: Bool = List.rotateRight(0, 1 :: 2 :: 3 :: Nil) `assertEq!` 1 :: 2 :: 3 :: Nil

@test
def rotateRight10: Bool = List.rotateRight(1, 1 :: 2 :: 3 :: Nil) `assertEq!` 3 :: 1 :: 2 :: Nil

@test
def rotateRight11: Bool = List.rotateRight(2, 1 :: 2 :: 3 :: Nil) `assertEq!` 2 :: 3 :: 1 :: Nil

/////////////////////////////////////////////////////////////////////////////
// replace                                                                 //
/////////////////////////////////////////////////////////////////////////////
@test
def replace01: Bool = List.replace(0, 2, Nil) `assertEq!` Nil

@test
def replace02: Bool = List.replace(-1, 2, 1 :: Nil) `assertEq!` 1 :: Nil

@test
def replace03: Bool = List.replace(0, 2, 1 :: Nil) `assertEq!` 2 :: Nil

@test
def replace04: Bool = List.replace(1, 2, 1 :: Nil) `assertEq!` 1 :: Nil

@test
def replace05: Bool = List.replace(0, 5, 1 :: 2 :: Nil) `assertEq!` 5 :: 2 :: Nil

@test
def replace06: Bool = List.replace(1, 5, 1 :: 2 :: Nil) `assertEq!` 1 :: 5 :: Nil

@test
def replace07: Bool = List.replace(2, 5, 1 :: 2 :: Nil) `assertEq!` 1 :: 2 :: Nil

/////////////////////////////////////////////////////////////////////////////
// patch                                                                   //
/////////////////////////////////////////////////////////////////////////////
@test
def patch01: Bool = List.patch(0, 0, Nil, Nil) `assertEq!` Nil

@test
def patch02: Bool = List.patch(0, 2, 1 :: 2 :: Nil, Nil) `assertEq!` Nil

@test
def patch03: Bool = List.patch(0, 2, Nil, 1 :: 2 :: Nil) `assertEq!` 1 :: 2 :: Nil

@test
def patch04: Bool = List.patch(-3, 3, 1 :: 2 :: 4 :: Nil, 1 :: 2 :: Nil) `assertEq!` 1 :: 2 :: Nil

@test
def patch05: Bool = List.patch(2, 3, 1 :: 2 :: 4 :: Nil, 1 :: 2 :: Nil) `assertEq!` 1 :: 2 :: Nil

@test
def patch06: Bool = List.patch(0, 0, Nil, 1 :: Nil) `assertEq!` 1 :: Nil

@test
def patch07: Bool = List.patch(1, 0, 2 :: Nil, 1 :: Nil) `assertEq!` 1 :: Nil

@test
def patch08: Bool = List.patch(0, 1, 2 :: Nil, 1 :: Nil) `assertEq!` 2 :: Nil

@test
def patch09: Bool = List.patch(0, 2, 2 :: 4 :: Nil, 1 :: Nil) `assertEq!` 2 :: Nil

@test
def patch10: Bool = List.patch(-1, 2, 2 :: 4 :: Nil, 1 :: Nil) `assertEq!` 4 :: Nil

@test
def patch11: Bool = List.patch(-1, 2, 3 :: 4 :: Nil, 1 :: 2 :: Nil) `assertEq!` 4 :: 2 :: Nil

@test
def patch12: Bool = List.patch(1, 2, 3 :: 4 :: Nil, 1 :: 2 :: Nil) `assertEq!` 1 :: 3 :: Nil

@test
def patch13: Bool = List.patch(-2, 2, 3 :: 4 :: Nil, 1 :: 2 :: Nil) `assertEq!` 1 :: 2 :: Nil

@test
def patch14: Bool = List.patch(2, 2, 3 :: 4 :: Nil, 1 :: 2 :: Nil) `assertEq!` 1 :: 2 :: Nil

@test
def patch15: Bool = List.patch(1, 1, 3 :: Nil, 1 :: 2 :: Nil) `assertEq!` 1 :: 3 :: Nil

@test
def patch16: Bool = List.patch(0, 2, 3 :: 4 :: Nil, 1 :: 2 :: Nil) `assertEq!` 3 :: 4 :: Nil

@test
def patch17: Bool = List.patch(0, 1, 4 :: Nil, 1 :: 2 :: 3 :: Nil) `assertEq!` 4 :: 2 :: 3 :: Nil

@test
def patch18: Bool = List.patch(1, 1, 4 :: Nil, 1 :: 2 :: 3 :: Nil) `assertEq!` 1 :: 4 :: 3 :: Nil

@test
def patch19: Bool = List.patch(2, 1, 4 :: Nil, 1 :: 2 :: 3 :: Nil) `assertEq!` 1 :: 2 :: 4 :: Nil

@test
def patch20: Bool = List.patch(0, 2, 4 :: 5 :: Nil, 1 :: 2 :: 3 :: Nil) `assertEq!` 4 :: 5 :: 3 :: Nil

@test
def patch21: Bool = List.patch(1, 2, 4 :: 5 :: Nil, 1 :: 2 :: 3 :: Nil) `assertEq!` 1 :: 4 :: 5 :: Nil

@test
def patch22: Bool = List.patch(0, 2, 4 :: 5 :: 6 :: Nil, 1 :: 2 :: 3 :: Nil) `assertEq!` 4 :: 5 :: 3 :: Nil

@test
def patch23: Bool = List.patch(0, 3, 4 :: 5 :: 6 :: Nil, 1 :: 2 :: 3 :: Nil) `assertEq!` 4 :: 5 :: 6 :: Nil

@test
def patch24: Bool = List.patch(2, 4, 14 :: 15 :: 16 :: 17 :: Nil, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: Nil) `assertEq!`
                    1 :: 2 :: 14 :: 15 :: 16 :: 17 :: 7 :: Nil

@test
def patch25: Bool = List.patch(-2, 4, 14 :: 15 :: 16 :: 17 :: Nil, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: Nil) `assertEq!`
                    16 :: 17 :: 3 :: 4 :: 5 :: 6 :: 7 :: Nil

@test
def patch26: Bool = List.patch(4, 5, 14 :: 15 :: 16 :: 17 :: Nil, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: Nil) `assertEq!`
                    1 :: 2 :: 3 :: 4 :: 14 :: 15 :: 16 :: Nil

@test
def patch27: Bool = List.patch(4, 2, 14 :: 15 :: 16 :: 17 :: Nil, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: Nil) `assertEq!`
                    1 :: 2 :: 3 :: 4 :: 14 :: 15 :: 7 :: Nil

@test
def patch28: Bool = List.patch(-1, 10, -1 :: -2 :: -3 :: -4 :: -5 :: -6 :: -7 :: -8 :: Nil, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: Nil) `assertEq!`
                    -2 :: -3 :: -4 :: -5 :: -6 :: -7 :: -8 :: Nil

/////////////////////////////////////////////////////////////////////////////
// permutations                                                            //
/////////////////////////////////////////////////////////////////////////////
@test
def permutations01: Bool = List.permutations(Nil) `assertEq!` Nil :: Nil

@test
def permutations02: Bool = List.permutations(1 :: Nil) `assertEq!` (1 :: Nil) :: Nil

@test
def permutations03: Bool = List.permutations(1 :: 2 :: Nil) `assertEq!` (1 :: 2 :: Nil) :: (2 :: 1 :: Nil) :: Nil

@test
def permutations04: Bool = List.permutations(1 :: 2 :: 3 :: Nil) `assertEq!` (1 :: 2 :: 3 :: Nil) :: (1 :: 3 :: 2 :: Nil) ::
                                                                             (2 :: 1 :: 3 :: Nil) :: (2 :: 3 :: 1 :: Nil) ::
                                                                             (3 :: 1 :: 2 :: Nil) :: (3 :: 2 :: 1 :: Nil) :: Nil

/////////////////////////////////////////////////////////////////////////////
// subsequences                                                            //
/////////////////////////////////////////////////////////////////////////////
@test
def subsequences01: Bool = List.subsequences(Nil) `assertEq!` Nil :: Nil

@test
def subsequences02: Bool = List.subsequences(1 :: Nil) `assertEq!` (1 :: Nil) :: Nil :: Nil

@test
def subsequences03: Bool = List.subsequences(1 :: 2 :: Nil) `assertEq!` (1 :: 2 :: Nil) :: (1 :: Nil) ::
                                                                        (2 :: Nil) :: Nil :: Nil

@test
def subsequences04: Bool = List.subsequences(1 :: 2 :: 3 :: Nil) `assertEq!` (1 :: 2 :: 3 :: Nil) :: (1 :: 2 :: Nil) ::
                                                                             (1 :: 3 :: Nil) :: (1 :: Nil) ::
                                                                             (2 :: 3 :: Nil) :: (2 :: Nil) ::
                                                                             (3 :: Nil) :: Nil :: Nil

/////////////////////////////////////////////////////////////////////////////
// intersperse                                                             //
/////////////////////////////////////////////////////////////////////////////
@test
def intersperse01: Bool = List.intersperse(11, Nil) `assertEq!` Nil

@test
def intersperse02: Bool = List.intersperse(11, 1 :: Nil) `assertEq!` 1 :: Nil

@test
def intersperse03: Bool = List.intersperse(11, 1 :: 2 :: Nil) `assertEq!` 1 :: 11 :: 2 :: Nil

@test
def intersperse04: Bool = List.intersperse(11, 1 :: 2 :: 3 :: Nil) `assertEq!` 1 :: 11 :: 2 :: 11 :: 3 :: Nil

@test
def intersperse05: Bool = List.intersperse(11, 1 :: 2 :: 3 :: 4 :: Nil) `assertEq!` 1 :: 11 :: 2 :: 11 :: 3 :: 11 :: 4 :: Nil

/////////////////////////////////////////////////////////////////////////////
// transpose                                                               //
/////////////////////////////////////////////////////////////////////////////
@test
def transpose01: Bool = List.transpose(Nil) `assertEq!` Nil

@test
def transpose02: Bool = List.transpose(Nil :: Nil) `assertEq!` Nil :: Nil

@test
def transpose03: Bool = List.transpose(Nil :: Nil :: Nil) `assertEq!` Nil :: Nil :: Nil

@test
def transpose04: Bool = List.transpose(Nil :: Nil :: Nil :: Nil) `assertEq!` Nil :: Nil :: Nil :: Nil

@test
def transpose05: Bool = List.transpose((1 :: Nil) :: Nil) `assertEq!` (1 :: Nil) :: Nil

@test
def transpose06: Bool = List.transpose((1 :: 2 :: Nil) :: Nil) `assertEq!` (1 :: Nil) :: (2 :: Nil) :: Nil

@test
def transpose07: Bool = List.transpose((1 :: 2 :: 3 :: Nil) :: Nil) `assertEq!`
                        (1 :: Nil) :: (2 :: Nil) :: (3 :: Nil) :: Nil

@test
def transpose08: Bool = List.transpose((1 :: 2 :: 3 :: 4 :: Nil) :: Nil) `assertEq!`
                        (1 :: Nil) :: (2 :: Nil) :: (3 :: Nil) :: (4 :: Nil) :: Nil

@test
def transpose09: Bool = List.transpose((1 :: Nil) :: (2 :: Nil) :: Nil) `assertEq!` (1 :: 2 :: Nil) :: Nil

@test
def transpose10: Bool = List.transpose((1 :: Nil) :: (2 :: Nil) :: (3 :: Nil) :: Nil) `assertEq!`
                        (1 :: 2 :: 3 :: Nil) :: Nil

@test
def transpose11: Bool = List.transpose((1 :: Nil) :: (2 :: Nil) :: (3 :: Nil) :: (4 :: Nil) :: Nil) `assertEq!`
                        (1 :: 2 :: 3 :: 4 :: Nil) :: Nil

@test
def transpose12: Bool = List.transpose((1 :: 2 :: Nil) :: (3 :: 4 :: Nil) :: Nil) `assertEq!`
                        (1 :: 3 :: Nil) :: (2 :: 4 :: Nil) :: Nil

@test
def transpose13: Bool = List.transpose((1 :: 2 :: 3 :: Nil) :: (4 :: 5 :: 6 :: Nil) :: Nil) `assertEq!`
                        (1 :: 4 :: Nil) :: (2 :: 5 :: Nil) :: (3 :: 6 :: Nil) :: Nil

@test
def transpose14: Bool = List.transpose((1 :: 2 :: 3 :: 4 :: Nil) :: (5 :: 6 :: 7 :: 8 :: Nil) :: Nil) `assertEq!`
                        (1 :: 5 :: Nil) :: (2 :: 6 :: Nil) :: (3 :: 7 :: Nil) :: (4 :: 8 :: Nil) :: Nil

@test
def transpose15: Bool = List.transpose((1 :: 2 :: 3 :: 4 :: 5 :: Nil) :: (6 :: 7 :: 8 :: 9 :: 10 :: Nil) :: Nil) `assertEq!`
                        (1 :: 6 :: Nil) :: (2 :: 7 :: Nil) :: (3 :: 8 :: Nil) :: (4 :: 9 :: Nil) :: (5 :: 10 :: Nil) :: Nil

@test
def transpose16: Bool = List.transpose((1 :: 2 :: Nil) :: (3 :: 4 :: Nil) :: (5 :: 6 :: Nil) :: Nil) `assertEq!`
                        (1 :: 3 :: 5 :: Nil) :: (2 :: 4 :: 6 :: Nil) :: Nil

@test
def transpose17: Bool = List.transpose((1 :: 2 :: Nil) :: (3 :: 4 :: Nil) :: (5 :: 6 :: Nil) :: (7 :: 8 :: Nil) :: Nil) `assertEq!`
                        (1 :: 3 :: 5 :: 7 :: Nil) :: (2 :: 4 :: 6 :: 8 :: Nil) :: Nil

@test
def transpose18: Bool = List.transpose((1 :: 2 :: Nil) :: (3 :: 4 :: Nil) :: (5 :: 6 :: Nil) :: (7 :: 8 :: Nil) :: (9 :: 10 :: Nil) :: Nil) `assertEq!`
                        (1 :: 3 :: 5 :: 7 :: 9 :: Nil) :: (2 :: 4 :: 6 :: 8 :: 10 :: Nil) :: Nil

@test
def transpose19: Bool = List.transpose((1 :: 2 :: 3 :: Nil) :: (4 :: 5 :: 6 :: Nil) :: (7 :: 8 :: 9 :: Nil) :: Nil) `assertEq!`
                        (1 :: 4 :: 7 :: Nil) :: (2 :: 5 :: 8 :: Nil) :: (3 :: 6 :: 9 :: Nil) :: Nil

@test
def transpose20: Bool = List.transpose((1 :: 2 :: 3 :: Nil) :: (4 :: 5 :: Nil) :: (7 :: 8 :: 9 :: Nil) :: Nil) `assertEq!`
                        (1 :: 2 :: 3 :: Nil) :: (4 :: 5 :: Nil) :: (7 :: 8 :: 9 :: Nil) :: Nil

@test
def transpose21: Bool = List.transpose((1 :: 2 :: 3 :: Nil) :: Nil :: (7 :: 8 :: 9 :: Nil) :: Nil) `assertEq!`
                        (1 :: 2 :: 3 :: Nil) :: Nil :: (7 :: 8 :: 9 :: Nil) :: Nil

@test
def transpose22: Bool = List.transpose((1 :: 2 :: 3 :: Nil) :: (4 :: 5 :: 6 :: Nil) :: (7 :: 8 :: 9 :: 10 :: Nil) :: Nil) `assertEq!`
                        (1 :: 2 :: 3 :: Nil) :: (4 :: 5 :: 6 :: Nil) :: (7 :: 8 :: 9 :: 10 :: Nil) :: Nil

/////////////////////////////////////////////////////////////////////////////
// isPrefixOf                                                              //
/////////////////////////////////////////////////////////////////////////////
@test
def isPrefixOf01: Bool = assert!(List.isPrefixOf(Nil, Nil))

@test
def isPrefixOf02: Bool = assertNot!(List.isPrefixOf(1 :: Nil, Nil))

@test
def isPrefixOf03: Bool = assert!(List.isPrefixOf(Nil, 1 :: Nil))

@test
def isPrefixOf04: Bool = assert!(List.isPrefixOf(1 :: Nil, 1 :: Nil))

@test
def isPrefixOf05: Bool = assertNot!(List.isPrefixOf(2 :: Nil, 1 :: Nil))

@test
def isPrefixOf06: Bool = assertNot!(List.isPrefixOf(1 :: 2 :: Nil, 1 :: Nil))

@test
def isPrefixOf07: Bool = assert!(List.isPrefixOf(Nil, 1 :: 2 :: Nil))

@test
def isPrefixOf08: Bool = assert!(List.isPrefixOf(1 :: Nil, 1 :: 2 :: Nil))

@test
def isPrefixOf09: Bool = assertNot!(List.isPrefixOf(2 :: Nil, 1 :: 2 :: Nil))

@test
def isPrefixOf10: Bool = assert!(List.isPrefixOf(1 :: 2 :: Nil, 1 :: 2 :: Nil))

@test
def isPrefixOf11: Bool = assertNot!(List.isPrefixOf(1 :: 3 :: Nil, 1 :: 2 :: Nil))

@test
def isPrefixOf12: Bool = assertNot!(List.isPrefixOf(1 :: 2 :: 3 :: Nil, 1 :: 2 :: Nil))

@test
def isPrefixOf13: Bool = assertNot!(List.isPrefixOf(1 :: 2 :: 3 :: Nil, 89 :: 11 :: 1 :: 2 :: 3 :: 4 :: Nil))

/////////////////////////////////////////////////////////////////////////////
// isInfixOf                                                               //
/////////////////////////////////////////////////////////////////////////////
@test
def isInfixOf01: Bool = assert!(List.isInfixOf(Nil, Nil))

@test
def isInfixOf02: Bool = assertNot!(List.isInfixOf(1 :: Nil, Nil))

@test
def isInfixOf03: Bool = assert!(List.isInfixOf(Nil, 1 :: Nil))

@test
def isInfixOf04: Bool = assert!(List.isInfixOf(1 :: Nil, 1 :: Nil))

@test
def isInfixOf05: Bool = assertNot!(List.isInfixOf(2 :: Nil, 1 :: Nil))

@test
def isInfixOf06: Bool = assertNot!(List.isInfixOf(1 :: 2 :: Nil, 1 :: Nil))

@test
def isInfixOf07: Bool = assert!(List.isInfixOf(Nil, 1 :: 2 :: Nil))

@test
def isInfixOf08: Bool = assert!(List.isInfixOf(1 :: Nil, 1 :: 2 :: Nil))

@test
def isInfixOf09: Bool = assert!(List.isInfixOf(2 :: Nil, 1 :: 2 :: Nil))

@test
def isInfixOf10: Bool = assert!(List.isInfixOf(1 :: 2 :: Nil, 1 :: 2 :: Nil))

@test
def isInfixOf11: Bool = assertNot!(List.isInfixOf(1 :: 3 :: Nil, 1 :: 2 :: Nil))

@test
def isInfixOf12: Bool = assertNot!(List.isInfixOf(1 :: 2 :: 3 :: Nil, 1 :: 2 :: Nil))

@test
def isInfixOf13: Bool = assert!(List.isInfixOf(1 :: 2 :: 3 :: Nil, 89 :: 11 :: 1 :: 2 :: 3 :: 4 :: Nil))

/////////////////////////////////////////////////////////////////////////////
// isSuffixOf                                                              //
/////////////////////////////////////////////////////////////////////////////
@test
def isSuffixOf01: Bool = assert!(List.isSuffixOf(Nil, Nil))

@test
def isSuffixOf02: Bool = assertNot!(List.isSuffixOf(1 :: Nil, Nil))

@test
def isSuffixOf03: Bool = assert!(List.isSuffixOf(Nil, 1 :: Nil))

@test
def isSuffixOf04: Bool = assert!(List.isSuffixOf(1 :: Nil, 1 :: Nil))

@test
def isSuffixOf05: Bool = assertNot!(List.isSuffixOf(2 :: Nil, 1 :: Nil))

@test
def isSuffixOf06: Bool = assertNot!(List.isSuffixOf(1 :: 2 :: Nil, 1 :: Nil))

@test
def isSuffixOf07: Bool = assert!(List.isSuffixOf(Nil, 1 :: 2 :: Nil))

@test
def isSuffixOf08: Bool = assertNot!(List.isSuffixOf(1 :: Nil, 1 :: 2 :: Nil))

@test
def isSuffixOf09: Bool = assert!(List.isSuffixOf(2 :: Nil, 1 :: 2 :: Nil))

@test
def isSuffixOf10: Bool = assert!(List.isSuffixOf(1 :: 2 :: Nil, 1 :: 2 :: Nil))

@test
def isSuffixOf11: Bool = assertNot!(List.isSuffixOf(1 :: 3 :: Nil, 1 :: 2 :: Nil))

@test
def isSuffixOf12: Bool = assertNot!(List.isSuffixOf(1 :: 2 :: 3 :: Nil, 1 :: 2 :: Nil))

@test
def isSuffixOf13: Bool = assert!(List.isSuffixOf(1 :: 2 :: 3 :: Nil, 89 :: 11 :: 1 :: 2 :: 3 :: Nil))

/////////////////////////////////////////////////////////////////////////////
// fold                                                                    //
/////////////////////////////////////////////////////////////////////////////
@test
def fold01: Bool = List.fold((i, e) -> (i - e)*(e % 2 + 1), 100, Nil) `assertEq!` 100

@test
def fold02: Bool = List.fold((i, e) -> (i - e)*(e % 2 + 1), 100, 1 :: Nil) `assertEq!` 198

@test
def fold03: Bool = List.fold((i, e) -> (i - e)*(e % 2 + 1), 100, 1 :: 2 :: Nil) `assertEq!` 196

@test
def fold04: Bool = List.fold((i, e) -> (i - e)*(e % 2 + 1), 100, 1 :: 2 :: 3 :: Nil) `assertEq!` 386

/////////////////////////////////////////////////////////////////////////////
// foldLeft                                                                //
/////////////////////////////////////////////////////////////////////////////
@test
def foldLeft01: Bool = List.foldLeft((i, e) -> (i - e)*(e % 2 + 1), 100, Nil) `assertEq!` 100

@test
def foldLeft02: Bool = List.foldLeft((i, e) -> (i - e)*(e % 2 + 1), 100, 1 :: Nil) `assertEq!` 198

@test
def foldLeft03: Bool = List.foldLeft((i, e) -> (i - e)*(e % 2 + 1), 100, 1 :: 2 :: Nil) `assertEq!` 196

@test
def foldLeft04: Bool = List.foldLeft((i, e) -> (i - e)*(e % 2 + 1), 100, 1 :: 2 :: 3 :: Nil) `assertEq!` 386

/////////////////////////////////////////////////////////////////////////////
// foldRight                                                               //
/////////////////////////////////////////////////////////////////////////////
@test
def foldRight01: Bool = List.foldRight((e, i) -> (i - e)*(e % 2 + 1), 100, Nil) `assertEq!` 100

@test
def foldRight02: Bool = List.foldRight((e, i) -> (i - e)*(e % 2 + 1), 100, 1 :: Nil) `assertEq!` 198

@test
def foldRight03: Bool = List.foldRight((e, i) -> (i - e)*(e % 2 + 1), 100, 1 :: 2 :: Nil) `assertEq!` 194

@test
def foldRight04: Bool = List.foldRight((e, i) -> (i - e)*(e % 2 + 1), 100, 1 :: 2 :: 3 :: Nil) `assertEq!` 382

/////////////////////////////////////////////////////////////////////////////
// reduceOpt                                                               //
/////////////////////////////////////////////////////////////////////////////
//@test
//def reduceOpt01: Bool = List.reduceOpt((a, b) -> a-b, Nil) `assertEq!` None

@test
def reduceOpt02: Bool = List.reduceOpt((a, b) -> a-b, 1 :: Nil) `assertEq!` Some(1)

@test
def reduceOpt03: Bool = List.reduceOpt((a, b) -> a-b, 1 :: 2 :: Nil) `assertEq!` Some(-1)

@test
def reduceOpt04: Bool = List.reduceOpt((a, b) -> a-b, 1 :: 2 :: 3 :: Nil) `assertEq!` Some(-4)

@test
def reduceOpt05: Bool = List.reduceOpt((a, b) -> a-b, 1 :: 2 :: 3 :: 4 :: Nil) `assertEq!` Some(-8)

/////////////////////////////////////////////////////////////////////////////
// reduceLeftOpt                                                           //
/////////////////////////////////////////////////////////////////////////////
//@test
//def reduceLeftOpt01: Bool = List.reduceLeftOpt((a, b) -> a-b, Nil) `assertEq!` None

@test
def reduceLeftOpt02: Bool = List.reduceLeftOpt((a, b) -> a-b, 1 :: Nil) `assertEq!` Some(1)

@test
def reduceLeftOpt03: Bool = List.reduceLeftOpt((a, b) -> a-b, 1 :: 2 :: Nil) `assertEq!` Some(-1)

@test
def reduceLeftOpt04: Bool = List.reduceLeftOpt((a, b) -> a-b, 1 :: 2 :: 3 :: Nil) `assertEq!` Some(-4)

@test
def reduceLeftOpt05: Bool = List.reduceLeftOpt((a, b) -> a-b, 1 :: 2 :: 3 :: 4 :: Nil) `assertEq!` Some(-8)

/////////////////////////////////////////////////////////////////////////////
// reduceRightOpt                                                          //
/////////////////////////////////////////////////////////////////////////////
//@test
//def reduceRightOpt01: Bool = List.reduceRightOpt((a, b) -> a-b, Nil) `assertEq!` None

@test
def reduceRightOpt02: Bool = List.reduceRightOpt((a, b) -> a-b, 1 :: Nil) `assertEq!` Some(1)

@test
def reduceRightOpt03: Bool = List.reduceRightOpt((a, b) -> a-b, 1 :: 2 :: Nil) `assertEq!` Some(-1)

@test
def reduceRightOpt04: Bool = List.reduceRightOpt((a, b) -> a-b, 1 :: 2 :: 3 :: Nil) `assertEq!` Some(2)

@test
def reduceRightOpt05: Bool = List.reduceRightOpt((a, b) -> a-b, 1 :: 2 :: 3 :: 4 :: Nil) `assertEq!` Some(-2)

/////////////////////////////////////////////////////////////////////////////
// count                                                                   //
/////////////////////////////////////////////////////////////////////////////
@test
def count01: Bool = List.count(i -> i > 3, Nil) `assertEq!` 0

@test
def count02: Bool = List.count(i -> i > 3, 1 :: Nil) `assertEq!` 0

@test
def count03: Bool = List.count(i -> i > 3, 4 :: Nil) `assertEq!` 1

@test
def count04: Bool = List.count(i -> i > 3, 1 :: 2 :: Nil) `assertEq!` 0

@test
def count05: Bool = List.count(i -> i > 3, 1 :: 8 :: Nil) `assertEq!` 1

@test
def count06: Bool = List.count(i -> i > 3, 8 :: 1 :: Nil) `assertEq!` 1

@test
def count07: Bool = List.count(i -> i > 3, 6 :: 7 :: Nil) `assertEq!` 2

/////////////////////////////////////////////////////////////////////////////
// concat                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def concat01: Bool = List.concat(Nil) `assertEq!` Nil

@test
def concat02: Bool = List.concat(Nil :: Nil) `assertEq!` Nil

@test
def concat03: Bool = List.concat((1 :: Nil) :: Nil) `assertEq!` 1 :: Nil

@test
def concat04: Bool = List.concat((1 :: 2 :: Nil) :: Nil) `assertEq!` 1 :: 2 :: Nil

@test
def concat05: Bool = List.concat(Nil :: Nil :: Nil) `assertEq!` Nil

@test
def concat06: Bool = List.concat((1 :: Nil) :: Nil :: Nil) `assertEq!` 1 :: Nil

@test
def concat07: Bool = List.concat(Nil :: (1 :: Nil) :: Nil) `assertEq!` 1 :: Nil

@test
def concat08: Bool = List.concat((1 :: Nil) :: (2 :: Nil) :: Nil) `assertEq!` 1 :: 2 :: Nil

@test
def concat09: Bool = List.concat((1 :: 2 :: Nil) :: (3 :: 4 :: 5 :: Nil) :: Nil) `assertEq!` 1 :: 2 :: 3 :: 4 :: 5 :: Nil

@test
def concat10: Bool = List.concat((1 :: Nil) :: (2 :: 3 :: Nil) :: (4 :: Nil) :: Nil) `assertEq!` 1 :: 2 :: 3 :: 4 :: Nil

/////////////////////////////////////////////////////////////////////////////
// exists                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def exists01: Bool = assertNot!(List.exists(i -> i > 3, Nil))

@test
def exists02: Bool = assertNot!(List.exists(i -> i > 3, 1 :: Nil))

@test
def exists03: Bool = assert!(List.exists(i -> i > 3, 5 :: Nil))

@test
def exists04: Bool = assertNot!(List.exists(i -> i > 3, 1 :: 2 :: Nil))

@test
def exists05: Bool = assert!(List.exists(i -> i > 3, 1 :: 6 :: Nil))

@test
def exists06: Bool = assert!(List.exists(i -> i > 3, 6 :: 1 :: Nil))

@test
def exists07: Bool = assert!(List.exists(i -> i > 3, 16 :: 6 :: Nil))

@test
def exists08: Bool = assertNot!(List.exists(i -> i > 3, 1 :: -9 :: 3 :: Nil))

@test
def exists09: Bool = assert!(List.exists(i -> i > 3, 1 :: 9 :: 3 :: Nil))

/////////////////////////////////////////////////////////////////////////////
// forall                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def forall01: Bool = assert!(List.forall(i -> i > 3, Nil))

@test
def forall02: Bool = assertNot!(List.forall(i -> i > 3, 1 :: Nil))

@test
def forall03: Bool = assert!(List.forall(i -> i > 3, 5 :: Nil))

@test
def forall04: Bool = assertNot!(List.forall(i -> i > 3, 1 :: 2 :: Nil))

@test
def forall05: Bool = assertNot!(List.forall(i -> i > 3, 1 :: 6 :: Nil))

@test
def forall06: Bool = assertNot!(List.forall(i -> i > 3, 6 :: 1 :: Nil))

@test
def forall07: Bool = assert!(List.forall(i -> i > 3, 16 :: 6 :: Nil))

@test
def forall08: Bool = assertNot!(List.forall(i -> i > 3, 1 :: -9 :: 3 :: Nil))

@test
def forall09: Bool = assertNot!(List.forall(i -> i > 3, 1 :: 9 :: 3 :: Nil))

@test
def forall10: Bool = assert!(List.forall(i -> i > 3, 11 :: 9 :: 31 :: Nil))

/////////////////////////////////////////////////////////////////////////////
// and                                                                     //
/////////////////////////////////////////////////////////////////////////////
@test
def and01: Bool = assert!(List.and(Nil))

@test
def and02: Bool = assert!(List.and(true :: Nil))

@test
def and03: Bool = assertNot!(List.and(false :: Nil))

@test
def and04: Bool = assert!(List.and(true :: true :: Nil))

@test
def and05: Bool = assertNot!(List.and(true :: false :: Nil))

@test
def and06: Bool = assertNot!(List.and(false :: true :: Nil))

@test
def and07: Bool = assertNot!(List.and(false :: false :: Nil))

@test
def and08: Bool = assert!(List.and(true :: true :: true :: Nil))

@test
def and09: Bool = assertNot!(List.and(false :: true :: true :: Nil))

@test
def and10: Bool = assertNot!(List.and(true :: false :: true :: Nil))

@test
def and11: Bool = assertNot!(List.and(true :: true :: false :: Nil))

/////////////////////////////////////////////////////////////////////////////
// or                                                                      //
/////////////////////////////////////////////////////////////////////////////
@test
def or01: Bool = assertNot!(List.or(Nil))

@test
def or02: Bool = assert!(List.or(true :: Nil))

@test
def or03: Bool = assertNot!(List.or(false :: Nil))

@test
def or04: Bool = assert!(List.or(true :: true :: Nil))

@test
def or05: Bool = assert!(List.or(true :: false :: Nil))

@test
def or06: Bool = assert!(List.or(false :: true :: Nil))

@test
def or07: Bool = assertNot!(List.or(false :: false :: Nil))

@test
def or08: Bool = assert!(List.or(true :: true :: true :: Nil))

@test
def or09: Bool = assert!(List.or(false :: true :: true :: Nil))

@test
def or10: Bool = assert!(List.or(true :: false :: true :: Nil))

@test
def or11: Bool = assert!(List.or(true :: true :: false :: Nil))

/////////////////////////////////////////////////////////////////////////////
// filter                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def filter01: Bool = List.filter(i -> i > 3, Nil) `assertEq!` Nil

@test
def filter02: Bool = List.filter(i -> i > 3, 2 :: Nil) `assertEq!` Nil

@test
def filter03: Bool = List.filter(i -> i > 3, 4 :: Nil) `assertEq!` 4 :: Nil

@test
def filter04: Bool = List.filter(i -> i > 3, 1 :: 3 :: Nil) `assertEq!` Nil

@test
def filter05: Bool = List.filter(i -> i > 3, 1 :: 8 :: Nil) `assertEq!` 8 :: Nil

@test
def filter06: Bool = List.filter(i -> i > 3, 8 :: 1 :: Nil) `assertEq!` 8 :: Nil

@test
def filter07: Bool = List.filter(i -> i > 3, 8 :: 9 :: Nil) `assertEq!` 8 :: 9 :: Nil

@test
def filter08: Bool = List.filter(i -> i > 3, 1 :: 4 :: 11 :: 2 :: -22 :: 17 :: Nil) `assertEq!` 4 :: 11 :: 17 :: Nil

/////////////////////////////////////////////////////////////////////////////
// slice                                                                   //
/////////////////////////////////////////////////////////////////////////////
@test
def slice01: Bool = List.slice(0, 0, Nil) `assertEq!` Nil

@test
def slice02: Bool = List.slice(-1, 1, Nil) `assertEq!` Nil

@test
def slice03: Bool = List.slice(0, 0, 1 :: Nil) `assertEq!` Nil

@test
def slice04: Bool = List.slice(0, 1, 1 :: Nil) `assertEq!` 1 :: Nil

@test
def slice05: Bool = List.slice(0, 2, 1 :: Nil) `assertEq!` 1 :: Nil

@test
def slice06: Bool = List.slice(2, 5, 1 :: Nil) `assertEq!` Nil

@test
def slice07: Bool = List.slice(-1, 1, 1 :: Nil) `assertEq!` 1 :: Nil

@test
def slice08: Bool = List.slice(0, 1, 1 :: 2 :: Nil) `assertEq!` 1 :: Nil

@test
def slice09: Bool = List.slice(0, 2, 1 :: 2 :: Nil) `assertEq!` 1 :: 2 :: Nil

@test
def slice10: Bool = List.slice(1, 2, 1 :: 2 :: Nil) `assertEq!` 2 :: Nil

@test
def slice11: Bool = List.slice(0, 3, 1 :: 2 :: 3 :: Nil) `assertEq!` 1 :: 2 :: 3 :: Nil

@test
def slice12: Bool = List.slice(0, 2, 1 :: 2 :: 3 :: Nil) `assertEq!` 1 :: 2 :: Nil

@test
def slice13: Bool = List.slice(1, 3, 1 :: 2 :: 3 :: Nil) `assertEq!` 2 :: 3 :: Nil

@test
def slice14: Bool = List.slice(1, 2, 1 :: 2 :: 3 :: Nil) `assertEq!` 2 :: Nil

/*
  test("partition.01") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: (List[Int32], List[Int32]) = List.partition(f, Nil)
      """.stripMargin
    runAnyTest(input, mkAnyTuple(mkNil, mkNil))
  }

  test("partition.02") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: (List[Int32], List[Int32]) = List.partition(f, 1 :: Nil)
      """.stripMargin
    runAnyTest(input, mkAnyTuple(mkNil, mkList(List(1))))
  }

  test("partition.03") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: (List[Int32], List[Int32]) = List.partition(f, 4 :: Nil)
      """.stripMargin
    runAnyTest(input, mkAnyTuple(mkList(List(4)), mkNil))
  }

  test("partition.04") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: (List[Int32], List[Int32]) = List.partition(f, 1 :: 2 :: Nil)
      """.stripMargin
    runAnyTest(input, mkAnyTuple(mkNil, mkList(List(1, 2))))
  }

  test("partition.05") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: (List[Int32], List[Int32]) = List.partition(f, 1 :: 6 :: Nil)
      """.stripMargin
    runAnyTest(input, mkAnyTuple(mkList(List(6)), mkList(List(1))))
  }

  test("partition.06") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: (List[Int32], List[Int32]) = List.partition(f, 5 :: 3 :: Nil)
      """.stripMargin
    runAnyTest(input, mkAnyTuple(mkList(List(5)), mkList(List(3))))
  }

  test("partition.07") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: (List[Int32], List[Int32]) = List.partition(f, 99 :: 6 :: Nil)
      """.stripMargin
    runAnyTest(input, mkAnyTuple(mkList(List(99, 6)), mkNil))
  }

  test("partition.08") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: (List[Int32], List[Int32]) = List.partition(f, 99 :: 6 :: 4 :: 1 :: -99 :: 99 :: 88 :: 5 :: 1 :: 0 :: Nil)
      """.stripMargin
    runAnyTest(input, mkAnyTuple(mkList(List(99, 6, 4, 99, 88, 5)), mkList(List(1, -99, 1, 0))))
  }

  test("partition.09") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: (List[Int32], List[Int32]) = List.partition(f, 1 :: 11 :: 24 :: -1 :: 3 :: 14 :: 5 :: Nil)
      """.stripMargin
    runAnyTest(input, mkAnyTuple(mkList(List(11, 24, 14, 5)), mkList(List(1, -1, 3))))
  }

  test("span.01") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: (List[Int32], List[Int32]) = List.span(f, Nil)
      """.stripMargin
    runAnyTest(input, mkAnyTuple(mkNil, mkNil))
  }

  test("span.02") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: (List[Int32], List[Int32]) = List.span(f, 1 :: Nil)
      """.stripMargin
    runAnyTest(input, mkAnyTuple(mkNil, mkList(List(1))))
  }

  test("span.03") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: (List[Int32], List[Int32]) = List.span(f, 4 :: Nil)
      """.stripMargin
    runAnyTest(input, mkAnyTuple(mkList(List(4)), mkNil))
  }

  test("span.04") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: (List[Int32], List[Int32]) = List.span(f, 1 :: 4 :: Nil)
      """.stripMargin
    runAnyTest(input, mkAnyTuple(mkNil, mkList(List(1, 4))))
  }

  test("span.05") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: (List[Int32], List[Int32]) = List.span(f, 4 :: 1 :: Nil)
      """.stripMargin
    runAnyTest(input, mkAnyTuple(mkList(List(4)), mkList(List(1))))
  }

  test("span.06") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: (List[Int32], List[Int32]) = List.span(f, 4 :: 8 :: Nil)
      """.stripMargin
    runAnyTest(input, mkAnyTuple(mkList(List(4, 8)), mkNil))
  }

  test("span.07") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: (List[Int32], List[Int32]) = List.span(f, 1 :: -1 :: Nil)
      """.stripMargin
    runAnyTest(input, mkAnyTuple(mkNil, mkList(List(1, -1))))
  }

  test("span.08") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: (List[Int32], List[Int32]) = List.span(f, -1 :: 11 :: 88 :: Nil)
      """.stripMargin
    runAnyTest(input, mkAnyTuple(mkNil, mkList(List(-1, 11, 88))))
  }

  test("span.09") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: (List[Int32], List[Int32]) = List.span(f, 4 :: -1 :: 88 :: Nil)
      """.stripMargin
    runAnyTest(input, mkAnyTuple(mkList(List(4)), mkList(List(-1, 88))))
  }

  test("span.10") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: (List[Int32], List[Int32]) = List.span(f, 4 :: 9 :: -2 :: Nil)
      """.stripMargin
    runAnyTest(input, mkAnyTuple(mkList(List(4, 9)), mkList(List(-2))))
  }

  test("span.11") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: (List[Int32], List[Int32]) = List.span(f, 4 :: 9 :: 9 :: Nil)
      """.stripMargin
    runAnyTest(input, mkAnyTuple(mkList(List(4, 9, 9)), mkNil))
  }

  test("span.12") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: (List[Int32], List[Int32]) = List.span(f, 8 :: 11 :: 89 :: -1 :: 34 :: Nil)
      """.stripMargin
    runAnyTest(input, mkAnyTuple(mkList(List(8, 11, 89)), mkList(List(-1, 34))))
  }

  test("span.13") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: (List[Int32], List[Int32]) = List.span(f, 3 :: 8 :: 11 :: 89 :: -1 :: 34 :: Nil)
      """.stripMargin
    runAnyTest(input, mkAnyTuple(mkNil, mkList(List(3, 8, 11, 89, -1, 34))))
  }

  test("drop.01") {
    val input = "def r: List[Int32] = List.drop(0, Nil)"
    runAnyTest(input, mkNil)
  }

  test("drop.02") {
    val input = "def r: List[Int32] = List.drop(-1, 1 :: Nil)"
    runAnyTest(input, mkList(List(1)))
  }

  test("drop.03") {
    val input = "def r: List[Int32] = List.drop(0, 1 :: Nil)"
    runAnyTest(input, mkList(List(1)))
  }

  test("drop.04") {
    val input = "def r: List[Int32] = List.drop(1, 1 :: Nil)"
    runAnyTest(input, mkNil)
  }

  test("drop.05") {
    val input = "def r: List[Int32] = List.drop(2, 1 :: Nil)"
    runAnyTest(input, mkNil)
  }

  test("drop.06") {
    val input = "def r: List[Int32] = List.drop(0, 1 :: 2 :: Nil)"
    runAnyTest(input, mkList(List(1, 2)))
  }

  test("drop.07") {
    val input = "def r: List[Int32] = List.drop(1, 1 :: 2 :: Nil)"
    runAnyTest(input, mkList(List(2)))
  }

  test("drop.08") {
    val input = "def r: List[Int32] = List.drop(2, 1 :: 2 :: Nil)"
    runAnyTest(input, mkNil)
  }

  test("drop.09") {
    val input = "def r: List[Int32] = List.drop(3, 1 :: 2 :: Nil)"
    runAnyTest(input, mkNil)
  }

  test("drop.10") {
    val input = "def r: List[Int32] = List.drop(3, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: Nil)"
    runAnyTest(input, mkList(List(4, 5, 6, 7)))
  }

  test("drop.11") {
    val input = "def r: List[Int32] = List.drop(6, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: Nil)"
    runAnyTest(input, mkList(List(7)))
  }

  test("drop.12") {
    val input = "def r: List[Int32] = List.drop(99, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: Nil)"
    runAnyTest(input, mkNil)
  }

  test("dropWhile.01") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: List[Int32] = List.dropWhile(f, Nil)
      """.stripMargin
    runAnyTest(input, mkNil)
  }

  test("dropWhile.02") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: List[Int32] = List.dropWhile(f, 5 :: Nil)
      """.stripMargin
    runAnyTest(input, mkNil)
  }

  test("dropWhile.03") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: List[Int32] = List.dropWhile(f, 1 :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(1)))
  }

  test("dropWhile.04") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: List[Int32] = List.dropWhile(f, 1 :: 2 :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(1, 2)))
  }

  test("dropWhile.05") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: List[Int32] = List.dropWhile(f, 1 :: 6 :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(1, 6)))
  }

  test("dropWhile.06") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: List[Int32] = List.dropWhile(f, 6 :: 1 :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(1)))
  }

  test("dropWhile.07") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: List[Int32] = List.dropWhile(f, 6 :: 11 :: Nil)
      """.stripMargin
    runAnyTest(input, mkNil)
  }

  test("dropWhile.08") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: List[Int32] = List.dropWhile(f, 1 :: 2 :: 3 :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(1, 2, 3)))
  }

  test("dropWhile.09") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: List[Int32] = List.dropWhile(f, 6 :: 2 :: 5 :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(2, 5)))
  }

  test("dropWhile.10") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: List[Int32] = List.dropWhile(f, 6 :: 12 :: 3 :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(3)))
  }

  test("dropWhile.11") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: List[Int32] = List.dropWhile(f, 6 :: 12 :: 32 :: Nil)
      """.stripMargin
    runAnyTest(input, mkNil)
  }

  test("take.01") {
    val input = "def r: List[Int32] = List.take(0, Nil)"
    runAnyTest(input, mkNil)
  }

  test("take.02") {
    val input = "def r: List[Int32] = List.take(-1, 1 :: Nil)"
    runAnyTest(input, mkNil)
  }

  test("take.03") {
    val input = "def r: List[Int32] = List.take(0, 1 :: Nil)"
    runAnyTest(input, mkNil)
  }

  test("take.04") {
    val input = "def r: List[Int32] = List.take(1, 1 :: Nil)"
    runAnyTest(input, mkList(List(1)))
  }

  test("take.05") {
    val input = "def r: List[Int32] = List.take(2, 1 :: Nil)"
    runAnyTest(input, mkList(List(1)))
  }

  test("take.06") {
    val input = "def r: List[Int32] = List.take(-1, 1 :: 2 :: Nil)"
    runAnyTest(input, mkNil)
  }

  test("take.07") {
    val input = "def r: List[Int32] = List.take(0, 1 :: 2 :: Nil)"
    runAnyTest(input, mkNil)
  }

  test("take.08") {
    val input = "def r: List[Int32] = List.take(1, 1 :: 2 :: Nil)"
    runAnyTest(input, mkList(List(1)))
  }

  test("take.09") {
    val input = "def r: List[Int32] = List.take(2, 1 :: 2 :: Nil)"
    runAnyTest(input, mkList(List(1, 2)))
  }

  test("take.10") {
    val input = "def r: List[Int32] = List.take(3, 1 :: 2 :: Nil)"
    runAnyTest(input, mkList(List(1, 2)))
  }

  test("take.11") {
    val input = "def r: List[Int32] = List.take(3, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: Nil)"
    runAnyTest(input, mkList(List(1, 2, 3)))
  }

  test("take.12") {
    val input = "def r: List[Int32] = List.take(6, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: Nil)"
    runAnyTest(input, mkList(List(1, 2, 3, 4, 5, 6)))
  }

  test("take.13") {
    val input = "def r: List[Int32] = List.take(99, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: Nil)"
    runAnyTest(input, mkList(List(1, 2, 3, 4, 5, 6, 7)))
  }

  test("takeWhile.01") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: List[Int32] = List.takeWhile(f, Nil)
      """.stripMargin
    runAnyTest(input, mkNil)
  }

  test("takeWhile.02") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: List[Int32] = List.takeWhile(f, 1 :: Nil)
      """.stripMargin
    runAnyTest(input, mkNil)
  }

  test("takeWhile.03") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: List[Int32] = List.takeWhile(f, 4 :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(4)))
  }

  test("takeWhile.04") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: List[Int32] = List.takeWhile(f, 1 :: 4 :: Nil)
      """.stripMargin
    runAnyTest(input, mkNil)
  }

  test("takeWhile.05") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: List[Int32] = List.takeWhile(f, 4 :: -4 :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(4)))
  }

  test("takeWhile.06") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: List[Int32] = List.takeWhile(f, 8 :: 4 :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(8, 4)))
  }

  test("takeWhile.07") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: List[Int32] = List.takeWhile(f, 4 :: 1 :: 8 :: 9 :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(4)))
  }

  test("takeWhile.08") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: List[Int32] = List.takeWhile(f, 4 :: 11 :: 8 :: -9 :: 7 :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(4, 11, 8)))
  }

  test("takeWhile.09") {
    val input =
      """def f(i: Int32): Bool = i > 3
        |def r: List[Int32] = List.takeWhile(f, 3 :: 11 :: 8 :: -9 :: 7 :: Nil)
      """.stripMargin
    runAnyTest(input, mkNil)
  }

  test("groupBy.01") {
    val input =
      """def f(a: Int32, b: Int32): Bool = a > 3 || b > 8
        |def r: List[List[Int32]] = List.groupBy(f, Nil)
      """.stripMargin
    runAnyTest(input, mkNil)
  }

  test("groupBy.02") {
    val input =
      """def f(a: Int32, b: Int32): Bool = a > 3 || b > 8
        |def r: List[List[Int32]] = List.groupBy(f, 1 :: Nil)
      """.stripMargin
    runAnyTest(input, mkAnyList(List(mkList(List(1)))))
  }

  test("groupBy.03") {
    val input =
      """def f(a: Int32, b: Int32): Bool = a > 3 || b > 8
        |def r: List[List[Int32]] = List.groupBy(f, 1 :: 4 :: Nil)
      """.stripMargin
    runAnyTest(input, mkAnyList(List(mkList(List(1)), mkList(List(4)))))
  }

  test("groupBy.04") {
    val input =
      """def f(a: Int32, b: Int32): Bool = a > 3 || b > 8
        |def r: List[List[Int32]] = List.groupBy(f, 1 :: 2 :: Nil)
      """.stripMargin
    runAnyTest(input, mkAnyList(List(mkList(List(1)), mkList(List(2)))))
  }

  test("groupBy.05") {
    val input =
      """def f(a: Int32, b: Int32): Bool = a > 3 || b > 8
        |def r: List[List[Int32]] = List.groupBy(f, 1 :: 9 :: Nil)
      """.stripMargin
    runAnyTest(input, mkAnyList(List(mkList(List(1, 9)))))
  }

  test("groupBy.06") {
    val input =
      """def f(a: Int32, b: Int32): Bool = a > 3 || b > 8
        |def r: List[List[Int32]] = List.groupBy(f, 1 :: 4 :: 7 :: 6 :: 9 :: 2 :: 4 :: 4 :: 8 :: 16 :: Nil)
      """.stripMargin
    runAnyTest(input, mkAnyList(List(mkList(List(1, 9, 16)), mkList(List(4, 7, 6, 4, 4, 8)), mkList(List(2)))))
  }

  test("groupBy.07") {
    val input =
      """def f(a: Int32, b: Int32): Bool = a > -6 || a*b >= 0
        |def r: List[List[Int32]] = List.groupBy(f, -1 :: -11 :: 4 :: -11 :: 0 :: 8 :: 2 :: 1 :: -3 :: -24 :: Nil)
      """.stripMargin
    runAnyTest(input, mkAnyList(List(mkList(List(-1, -11, -11, 0, -3, -24)), mkList(List(4, 8, 2, 1)))))
  }

  test("groupBy.08") {
    val input =
      """def f(a: Int32, b: Int32): Bool = a < 0 || (a > 10 || (b > 10 || a == b))
        |def r: List[List[Int32]] = List.groupBy(f, -5 :: 6 :: 11 :: 8 :: 8 :: -11 :: -1 :: 0 :: 4 :: -1 :: Nil)
      """.stripMargin
    runAnyTest(input, mkAnyList(List(mkList(List(-5, 11, -11, -1, -1)), mkList(List(6)),
                                     mkList(List(8, 8)), mkList(List(0)), mkList(List(4)))))
  }

  test("zip.01") {
    val input = "def r: List[(Int32, Int32)] = List.zip(Nil, Nil)"
    runAnyTest(input, mkNil)
  }

  test("zip.02") {
    val input = "def r: List[(Int32, Int32)] = List.zip(1 :: Nil, Nil)"
    runAnyTest(input, mkNil)
  }

  test("zip.03") {
    val input = "def r: List[(Int32, Int32)] = List.zip(Nil, 2 :: Nil)"
    runAnyTest(input, mkNil)
  }

  test("zip.04") {
    val input = "def r: List[(Int32, Int32)] = List.zip(1 :: Nil, 2 :: Nil)"
    runAnyTest(input, mkAnyList(List(mkTuple(1, 2))))
  }

  test("zip.05") {
    val input = "def r: List[(Int32, Int32)] = List.zip(1 :: 3 :: Nil, 2 :: 4 :: Nil)"
    runAnyTest(input, mkAnyList(List(mkTuple(1, 2), mkTuple(3, 4))))
  }

  test("zip.06") {
    val input = "def r: List[(Int32, Int32)] = List.zip(1 :: 3 :: 5 :: Nil, 2 :: 4 :: 6 :: Nil)"
    runAnyTest(input, mkAnyList(List(mkTuple(1, 2), mkTuple(3, 4), mkTuple(5, 6))))
  }

  test("zip.07") {
    val input = "def r: List[(Int32, Int32)] = List.zip(1 :: 3 :: 5 :: 7 :: Nil, 2 :: 4 :: 6 :: Nil)"
    runAnyTest(input, mkAnyList(List(mkTuple(1, 2), mkTuple(3, 4), mkTuple(5, 6))))
  }

  test("zip.08") {
    val input = "def r: List[(Int32, Int32)] = List.zip(1 :: 3 :: 5 :: Nil, 2 :: 4 :: 6 :: 8 :: Nil)"
    runAnyTest(input, mkAnyList(List(mkTuple(1, 2), mkTuple(3, 4), mkTuple(5, 6))))
  }

  test("zipWith.01") {
    val input =
      """def f(a: Int32, b: Bool): Int32 = if (b) a+1 else a
        |def r: List[Int32] = List.zipWith(f, Nil, Nil)
      """.stripMargin
    runAnyTest(input, mkNil)
  }

  test("zipWith.02") {
    val input =
      """def f(a: Int32, b: Bool): Int32 = if (b) a+1 else a
        |def r: List[Int32] = List.zipWith(f, 1 :: Nil, Nil)
      """.stripMargin
    runAnyTest(input, mkNil)
  }

  test("zipWith.03") {
    val input =
      """def f(a: Int32, b: Bool): Int32 = if (b) a+1 else a
        |def r: List[Int32] = List.zipWith(f, Nil, true :: Nil)
      """.stripMargin
    runAnyTest(input, mkNil)
  }

  test("zipWith.04") {
    val input =
      """def f(a: Int32, b: Bool): Int32 = if (b) a+1 else a
        |def r: List[Int32] = List.zipWith(f, 1 :: Nil, true :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(2)))
  }

  test("zipWith.05") {
    val input =
      """def f(a: Int32, b: Bool): Int32 = if (b) a+1 else a
        |def r: List[Int32] = List.zipWith(f, 1 :: Nil, false :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(1)))
  }

  test("zipWith.06") {
    val input =
      """def f(a: Int32, b: Bool): Int32 = if (b) a+1 else a
        |def r: List[Int32] = List.zipWith(f, 1 :: 2 :: Nil, true :: true :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(2, 3)))
  }

  test("zipWith.07") {
    val input =
      """def f(a: Int32, b: Bool): Int32 = if (b) a+1 else a
        |def r: List[Int32] = List.zipWith(f, 1 :: 2 :: Nil, true :: false :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(2, 2)))
  }

  test("zipWith.08") {
    val input =
      """def f(a: Int32, b: Bool): Int32 = if (b) a+1 else a
        |def r: List[Int32] = List.zipWith(f, 1 :: 2 :: Nil, false :: true :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(1, 3)))
  }

  test("zipWith.09") {
    val input =
      """def f(a: Int32, b: Bool): Int32 = if (b) a+1 else a
        |def r: List[Int32] = List.zipWith(f, 1 :: 2 :: Nil, false :: false :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(1, 2)))
  }

  test("zipWith.10") {
    val input =
      """def f(a: Int32, b: Bool): Int32 = if (b) a+1 else a
        |def r: List[Int32] = List.zipWith(f, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil, false :: true :: true :: false :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(1, 3, 4, 4)))
  }

  test("unzip.01") {
    val input = "def r: (List[Int32], List[Bool]) = List.unzip(Nil)"
    runAnyTest(input, mkAnyTuple(mkNil, mkNil))
  }

  test("unzip.02") {
    val input = "def r: (List[Int32], List[Bool]) = List.unzip((1, true) :: Nil)"
    runAnyTest(input, mkAnyTuple(mkList(List(1)), mkBoolList(List(true))))
  }

  test("unzip.03") {
    val input = "def r: (List[Int32], List[Bool]) = List.unzip((1, true) :: (2, false) :: Nil)"
    runAnyTest(input, mkAnyTuple(mkList(List(1, 2)), mkBoolList(List(true, false))))
  }

  test("unzip.04") {
    val input = "def r: (List[Int32], List[Bool]) = List.unzip((1, true) :: (2, false) :: (3, false) :: Nil)"
    runAnyTest(input, mkAnyTuple(mkList(List(1, 2, 3)), mkBoolList(List(true, false, false))))
  }

  test("unzip.05") {
    val input = "def r: (List[BigInt], List[Bool]) = List.unzip((1ii, true) :: (2ii, false) :: (3ii, false) :: Nil)"
    runAnyTest(input, mkAnyTuple(mkBigIntList(List(1, 2, 3)), mkBoolList(List(true, false, false))))
  }

  test("map2.01") {
    val input =
      """def f(a: Int32, b: Bool): Int32 = if (b) a+1 else a
        |def r: List[Int32] = List.map2(f, Nil, Nil)
      """.stripMargin
    runAnyTest(input, mkNil)
  }

  test("map2.02") {
    val input =
      """def f(a: Int32, b: Bool): Int32 = if (b) a+1 else a
        |def r: List[Int32] = List.map2(f, 1 :: Nil, Nil)
      """.stripMargin
    runAnyTest(input, mkNil)
  }

  test("map2.03") {
    val input =
      """def f(a: Int32, b: Bool): Int32 = if (b) a+1 else a
        |def r: List[Int32] = List.map2(f, Nil, true :: Nil)
      """.stripMargin
    runAnyTest(input, mkNil)
  }

  test("map2.04") {
    val input =
      """def f(a: Int32, b: Bool): Int32 = if (b) a+1 else a
        |def r: List[Int32] = List.map2(f, 1 :: Nil, true :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(2)))
  }

  test("map2.05") {
    val input =
      """def f(a: Int32, b: Bool): Int32 = if (b) a+1 else a
        |def r: List[Int32] = List.map2(f, 1 :: Nil, false :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(1)))
  }

  test("map2.06") {
    val input =
      """def f(a: Int32, b: Bool): Int32 = if (b) a+1 else a
        |def r: List[Int32] = List.map2(f, 1 :: 2 :: Nil, true :: true :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(2, 3)))
  }

  test("map2.07") {
    val input =
      """def f(a: Int32, b: Bool): Int32 = if (b) a+1 else a
        |def r: List[Int32] = List.map2(f, 1 :: 2 :: Nil, true :: false :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(2, 2)))
  }

  test("map2.08") {
    val input =
      """def f(a: Int32, b: Bool): Int32 = if (b) a+1 else a
        |def r: List[Int32] = List.map2(f, 1 :: 2 :: Nil, false :: true :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(1, 3)))
  }

  test("map2.09") {
    val input =
      """def f(a: Int32, b: Bool): Int32 = if (b) a+1 else a
        |def r: List[Int32] = List.map2(f, 1 :: 2 :: Nil, false :: false :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(1, 2)))
  }

  test("map2.10") {
    val input =
      """def f(a: Int32, b: Bool): Int32 = if (b) a+1 else a
        |def r: List[Int32] = List.map2(f, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil, false :: true :: true :: false :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(1, 3, 4, 4)))
  }

  test("flatMap2.01") {
    val input =
      """def f(a: Int32, b: Bool): List[Int32] = if (b) List.repeat(a, a) else List.repeat(a+1, a+1)
        |def r: List[Int32] = List.flatMap2(f, Nil, Nil)
      """.stripMargin
    runAnyTest(input, mkNil)
  }

  test("flatMap2.02") {
    val input =
      """def f(a: Int32, b: Bool): List[Int32] = if (b) List.repeat(a, a) else List.repeat(a+1, a+1)
        |def r: List[Int32] = List.flatMap2(f, 1 :: Nil, Nil)
      """.stripMargin
    runAnyTest(input, mkNil)
  }

  test("flatMap2.03") {
    val input =
      """def f(a: Int32, b: Bool): List[Int32] = if (b) List.repeat(a, a) else List.repeat(a+1, a+1)
        |def r: List[Int32] = List.flatMap2(f, Nil, true :: Nil)
      """.stripMargin
    runAnyTest(input, mkNil)
  }

  test("flatMap2.04") {
    val input =
      """def f(a: Int32, b: Bool): List[Int32] = if (b) List.repeat(a, a) else List.repeat(a+1, a+1)
        |def r: List[Int32] = List.flatMap2(f, 1 :: Nil, true :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(1)))
  }

  test("flatMap2.05") {
    val input =
      """def f(a: Int32, b: Bool): List[Int32] = if (b) List.repeat(a, a) else List.repeat(a+1, a+1)
        |def r: List[Int32] = List.flatMap2(f, 1 :: 2 :: Nil, true :: false :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(1, 3, 3, 3)))
  }

  test("flatMap2.06") {
    val input =
      """def f(a: Int32, b: Bool): List[Int32] = if (b) List.repeat(a, a) else List.repeat(a+1, a+1)
        |def r: List[Int32] = List.flatMap2(f, 1 :: 2 :: 2 :: Nil, true :: false :: true :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(1, 3, 3, 3, 2, 2)))
  }

  test("flatMap2.07") {
    val input =
      """def f(a: Int32, b: Bool): List[Int32] = if (b) List.repeat(a, a) else List.repeat(a+1, a+1)
        |def r: List[Int32] = List.flatMap2(f, 1 :: 2 :: 2 :: 4 :: Nil, true :: false :: true :: false :: false :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(1, 3, 3, 3, 2, 2, 5, 5, 5, 5, 5)))
  }

  test("fold2.01") {
    val input =
      """def f(c: Int32, a: Int32, b: Bool): Int32 = if (b) a+c else a*c
        |def r: Int32 = List.fold2(f, 4, Nil, Nil)
      """.stripMargin
    runTest(input, 4)
  }

  test("fold2.02") {
    val input =
      """def f(c: Int32, a: Int32, b: Bool): Int32 = if (b) a+c else a*c
        |def r: Int32 = List.fold2(f, 4, 1 :: Nil, Nil)
      """.stripMargin
    runTest(input, 4)
  }

  test("fold2.03") {
    val input =
      """def f(c: Int32, a: Int32, b: Bool): Int32 = if (b) a+c else a*c
        |def r: Int32 = List.fold2(f, 4, Nil, true :: Nil)
      """.stripMargin
    runTest(input, 4)
  }

  test("fold2.04") {
    val input =
      """def f(c: Int32, a: Int32, b: Bool): Int32 = if (b) a+c else a*c
        |def r: Int32 = List.fold2(f, 4, 1 :: Nil, true :: Nil)
      """.stripMargin
    runTest(input, 5)
  }

  test("fold2.05") {
    val input =
      """def f(c: Int32, a: Int32, b: Bool): Int32 = if (b) a+c else a*c
        |def r: Int32 = List.fold2(f, 4, 2 :: Nil, false :: Nil)
      """.stripMargin
    runTest(input, 8)
  }

  test("fold2.06") {
    val input =
      """def f(c: Int32, a: Int32, b: Bool): Int32 = if (b) a+c else a*c
        |def r: Int32 = List.fold2(f, 4, 2 :: 7 :: Nil, false :: true :: Nil)
      """.stripMargin
    runTest(input, 15)
  }

  test("fold2.07") {
    val input =
      """def f(c: Int32, a: Int32, b: Bool): Int32 = if (b) a+c else a*c
        |def r: Int32 = List.fold2(f, 4, 2 :: 7 :: 4 :: Nil, false :: true :: false :: Nil)
      """.stripMargin
    runTest(input, 60)
  }

  test("fold2.08") {
    val input =
      """def f(c: Int32, a: Int32, b: Bool): Int32 = if (b) a+c else a*c
        |def r: Int32 = List.fold2(f, 4, 2 :: 7 :: 4 :: -9 :: Nil, false :: true :: false :: false :: Nil)
      """.stripMargin
    runTest(input, -540)
  }

  test("foldLeft2.01") {
    val input =
      """def f(c: Int32, a: Int32, b: Bool): Int32 = if (b) a+c else a*c
        |def r: Int32 = List.foldLeft2(f, 4, Nil, Nil)
      """.stripMargin
    runTest(input, 4)
  }

  test("foldLeft2.02") {
    val input =
      """def f(c: Int32, a: Int32, b: Bool): Int32 = if (b) a+c else a*c
        |def r: Int32 = List.foldLeft2(f, 4, 1 :: Nil, Nil)
      """.stripMargin
    runTest(input, 4)
  }

  test("foldLeft2.03") {
    val input =
      """def f(c: Int32, a: Int32, b: Bool): Int32 = if (b) a+c else a*c
        |def r: Int32 = List.foldLeft2(f, 4, Nil, true :: Nil)
      """.stripMargin
    runTest(input, 4)
  }

  test("foldLeft2.04") {
    val input =
      """def f(c: Int32, a: Int32, b: Bool): Int32 = if (b) a+c else a*c
        |def r: Int32 = List.foldLeft2(f, 4, 1 :: Nil, true :: Nil)
      """.stripMargin
    runTest(input, 5)
  }

  test("foldLeft2.05") {
    val input =
      """def f(c: Int32, a: Int32, b: Bool): Int32 = if (b) a+c else a*c
        |def r: Int32 = List.foldLeft2(f, 4, 2 :: Nil, false :: Nil)
      """.stripMargin
    runTest(input, 8)
  }

  test("foldLeft2.06") {
    val input =
      """def f(c: Int32, a: Int32, b: Bool): Int32 = if (b) a+c else a*c
        |def r: Int32 = List.foldLeft2(f, 4, 2 :: 7 :: Nil, false :: true :: Nil)
      """.stripMargin
    runTest(input, 15)
  }

  test("foldLeft2.07") {
    val input =
      """def f(c: Int32, a: Int32, b: Bool): Int32 = if (b) a+c else a*c
        |def r: Int32 = List.foldLeft2(f, 4, 2 :: 7 :: 4 :: Nil, false :: true :: false :: Nil)
      """.stripMargin
    runTest(input, 60)
  }

  test("foldLeft2.08") {
    val input =
      """def f(c: Int32, a: Int32, b: Bool): Int32 = if (b) a+c else a*c
        |def r: Int32 = List.foldLeft2(f, 4, 2 :: 7 :: 4 :: -9 :: Nil, false :: true :: false :: false :: Nil)
      """.stripMargin
    runTest(input, -540)
  }

  test("foldRight2.01") {
    val input =
      """def f(a: Int32, b: Bool, c: Int32): Int32 = if (b) a+c else a*c
        |def r: Int32 = List.foldRight2(f, 4, Nil, Nil)
      """.stripMargin
    runTest(input, 4)
  }

  test("foldRight2.02") {
    val input =
      """def f(a: Int32, b: Bool, c: Int32): Int32 = if (b) a+c else a*c
        |def r: Int32 = List.foldRight2(f, 4, 1 :: Nil, Nil)
      """.stripMargin
    runTest(input, 4)
  }

  test("foldRight2.03") {
    val input =
      """def f(a: Int32, b: Bool, c: Int32): Int32 = if (b) a+c else a*c
        |def r: Int32 = List.foldRight2(f, 4, Nil, true :: Nil)
      """.stripMargin
    runTest(input, 4)
  }

  test("foldRight2.04") {
    val input =
      """def f(a: Int32, b: Bool, c: Int32): Int32 = if (b) a+c else a*c
        |def r: Int32 = List.foldRight2(f, 4, 1 :: Nil, true :: Nil)
      """.stripMargin
    runTest(input, 5)
  }

  test("foldRight2.05") {
    val input =
      """def f(a: Int32, b: Bool, c: Int32): Int32 = if (b) a+c else a*c
        |def r: Int32 = List.foldRight2(f, 4, 2 :: Nil, false :: Nil)
      """.stripMargin
    runTest(input, 8)
  }

  test("foldRight2.06") {
    val input =
      """def f(a: Int32, b: Bool, c: Int32): Int32 = if (b) a+c else a*c
        |def r: Int32 = List.foldRight2(f, 4, 2 :: 7 :: Nil, false :: true :: Nil)
      """.stripMargin
    runTest(input, 22)
  }

  test("foldRight2.07") {
    val input =
      """def f(a: Int32, b: Bool, c: Int32): Int32 = if (b) a+c else a*c
        |def r: Int32 = List.foldRight2(f, 4, 5 :: 88 :: 2 :: 7 :: 4 :: Nil, false :: true :: false :: Nil)
      """.stripMargin
    runTest(input, 46)
  }

  test("foldRight2.08") {
    val input =
      """def f(a: Int32, b: Bool, c: Int32): Int32 = if (b) a+c else a*c
        |def r: Int32 = List.foldRight2(f, 4, 2 :: 7 :: 4 :: -9 :: Nil, false :: false :: true :: false :: false :: Nil)
      """.stripMargin
    runTest(input, -274)
  }

  test("foldRight2.09") {
    val input =
      """def f(a: Int32, b: Bool, c: Int32): Int32 = if (b) a+c else a*c
        |def r: Int32 = List.foldRight2(f, 4, 1 :: 2 :: 7 :: 4 :: -9 :: Nil, true :: false :: true :: false :: false :: Nil)
      """.stripMargin
    runTest(input, -273)
  }

  test("concatMap.01") {
    val input =
      """def f(i: Int32): List[Int32] = List.repeat(i, i)
        |def r: List[Int32] = List.concatMap(f, Nil)
      """.stripMargin
    runAnyTest(input, mkNil)
  }

  test("concatMap.02") {
    val input =
      """def f(i: Int32): List[Int32] = List.repeat(i, i)
        |def r: List[Int32] = List.concatMap(f, 1 :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(1)))
  }

  test("concatMap.03") {
    val input =
      """def f(i: Int32): List[Int32] = List.repeat(i, i)
        |def r: List[Int32] = List.concatMap(f, 3 :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(3, 3, 3)))
  }

  test("concatMap.04") {
    val input =
      """def f(i: Int32): List[Int32] = List.repeat(i, i)
        |def r: List[Int32] = List.concatMap(f, 2 :: 1 :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(2, 2, 1)))
  }

  test("concatMap.05") {
    val input =
      """def f(i: Int32): List[Int32] = List.repeat(i, i)
        |def r: List[Int32] = List.concatMap(f, 2 :: 1 :: 3 :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(2, 2, 1, 3, 3, 3)))
  }

  test("concatMap.06") {
    val input =
      """def f(i: Int32): List[Int32] = List.repeat(i, i)
        |def r: List[Int32] = List.concatMap(f, 2 :: 1 :: 3 :: 4 :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(2, 2, 1, 3, 3, 3, 4, 4, 4, 4)))
  }

  test("filterMap.01") {
    val input =
      """def f(i: Int32): Option[Int32] = if (i % 2 == 0) Some(i/2) else None
        |def r: List[Int32] = List.filterMap(f, Nil)
      """.stripMargin
    runAnyTest(input, mkNil)
  }

  test("filterMap.02") {
    val input =
      """def f(i: Int32): Option[Int32] = if (i % 2 == 0) Some(i/2) else None
        |def r: List[Int32] = List.filterMap(f, 1 :: Nil)
      """.stripMargin
    runAnyTest(input, mkNil)
  }

  test("filterMap.03") {
    val input =
      """def f(i: Int32): Option[Int32] = if (i % 2 == 0) Some(i/2) else None
        |def r: List[Int32] = List.filterMap(f, 2 :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(1)))
  }

  test("filterMap.04") {
    val input =
      """def f(i: Int32): Option[Int32] = if (i % 2 == 0) Some(i/2) else None
        |def r: List[Int32] = List.filterMap(f, 1 :: 2 :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(1)))
  }

  test("filterMap.05") {
    val input =
      """def f(i: Int32): Option[Int32] = if (i % 2 == 0) Some(i/2) else None
        |def r: List[Int32] = List.filterMap(f, 4 :: 1 :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(2)))
  }

  test("filterMap.06") {
    val input =
      """def f(i: Int32): Option[Int32] = if (i % 2 == 0) Some(i/2) else None
        |def r: List[Int32] = List.filterMap(f, -9 :: 1 :: Nil)
      """.stripMargin
    runAnyTest(input, mkNil)
  }

  test("filterMap.07") {
    val input =
      """def f(i: Int32): Option[Int32] = if (i % 2 == 0) Some(i/2) else None
        |def r: List[Int32] = List.filterMap(f, -8 :: 44 :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(-4, 22)))
  }

  test("filterMap.08") {
    val input =
      """def f(i: Int32): Option[Int32] = if (i % 2 == 0) Some(i/2) else None
        |def r: List[Int32] = List.filterMap(f, -8 :: 44 :: 11 :: 0 :: 4 :: 87 :: 1 :: 4 :: 3 :: -18 :: Nil)
      """.stripMargin
    runAnyTest(input, mkList(List(-4, 22, 0, 2, 2, -9)))
  }

  test("findMap.01") {
    val input =
      """def f(i: Int32): Option[Int32] = if (i % 2 == 0) Some(i/2) else None
        |def r: Option[Int32] = List.findMap(f, Nil)
      """.stripMargin
    runAnyTest(input, mkNone)
  }

  test("findMap.02") {
    val input =
      """def f(i: Int32): Option[Int32] = if (i % 2 == 0) Some(i/2) else None
        |def r: Option[Int32] = List.findMap(f, 1 :: Nil)
      """.stripMargin
    runAnyTest(input, mkNone)
  }

  test("findMap.03") {
    val input =
      """def f(i: Int32): Option[Int32] = if (i % 2 == 0) Some(i/2) else None
        |def r: Option[Int32] = List.findMap(f, 2 :: Nil)
      """.stripMargin
    runAnyTest(input, mkSome(1))
  }

  test("findMap.04") {
    val input =
      """def f(i: Int32): Option[Int32] = if (i % 2 == 0) Some(i/2) else None
        |def r: Option[Int32] = List.findMap(f, 1 :: 3 :: Nil)
      """.stripMargin
    runAnyTest(input, mkNone)
  }

  test("findMap.05") {
    val input =
      """def f(i: Int32): Option[Int32] = if (i % 2 == 0) Some(i/2) else None
        |def r: Option[Int32] = List.findMap(f, 12 :: 3 :: Nil)
      """.stripMargin
    runAnyTest(input, mkSome(6))
  }

  test("findMap.06") {
    val input =
      """def f(i: Int32): Option[Int32] = if (i % 2 == 0) Some(i/2) else None
        |def r: Option[Int32] = List.findMap(f, 11 :: 38 :: Nil)
      """.stripMargin
    runAnyTest(input, mkSome(19))
  }

  test("findMap.07") {
    val input =
      """def f(i: Int32): Option[Int32] = if (i % 2 == 0) Some(i/2) else None
        |def r: Option[Int32] = List.findMap(f, 112 :: 38 :: Nil)
      """.stripMargin
    runAnyTest(input, mkSome(56))
  }

  test("findMap.08") {
    val input =
      """def f(i: Int32): Option[Int32] = if (i % 2 == 0) Some(i/2) else None
        |def r: Option[Int32] = List.findMap(f, 1 :: 3 :: 5 :: 7 :: 87 :: 112 :: 38 :: 37 :: Nil)
      """.stripMargin
    runAnyTest(input, mkSome(56))
  }

  test("findMap.09") {
    val input =
      """def f(i: Int32): Option[Int32] = if (i % 2 == 0) Some(i/2) else None
        |def r: Option[Int32] = List.findMap(f, 12 :: 3 :: 5 :: 7 :: 87 :: 112 :: 38 :: 37 :: Nil)
      """.stripMargin
    runAnyTest(input, mkSome(6))
  }

  test("toSet.01") {
    val input = "def r: Set[Int32] = List.toSet(Nil)"
    runAnyTest(input, mkSet(List()))
  }

  test("toSet.02") {
    val input = "def r: Set[Int32] = List.toSet(1 :: Nil)"
    runAnyTest(input, mkSet(List(1)))
  }

  test("toSet.03") {
    val input = "def r: Set[Int32] = List.toSet(1 :: 2 :: Nil)"
    runAnyTest(input, mkSet(List(1, 2)))
  }

  test("toSet.04") {
    val input = "def r: Set[Int32] = List.toSet(2 :: 1 :: Nil)"
    runAnyTest(input, mkSet(List(2, 1)))
  }

  test("toSet.05") {
    val input = "def r: Set[Int32] = List.toSet(2 :: 2 :: Nil)"
    runAnyTest(input, mkSet(List(2)))
  }

  test("toSet.06") {
    val input = "def r: Set[Int32] = List.toSet(1 :: 2 :: 3 :: Nil)"
    runAnyTest(input, mkSet(List(1, 2, 3)))
  }

  test("toSet.07") {
    val input = "def r: Set[Int32] = List.toSet(2 :: 1 :: 2 :: 3 :: Nil)"
    runAnyTest(input, mkSet(List(1, 2, 3)))
  }

  test("toSet.08") {
    val input = "def r: Set[Int32] = List.toSet(11 :: 9 :: -1 :: 3 :: 2 :: 1 :: 2 :: 3 :: 11 :: Nil)"
    runAnyTest(input, mkSet(List(9, -1, 1, 2, 3, 11)))
  }

  test("toSet.09") {
    val input = "def r: Set[Int32] = List.toSet(4 :: 11 :: 9 :: 3 :: 4 :: 1 :: 9 :: -9 :: Nil)"
    runAnyTest(input, mkSet(List(11, 3, 4, 1, 9, -9)))
  }
*/
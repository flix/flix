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

namespace TestList {

    use ToString.toString;

/////////////////////////////////////////////////////////////////////////////
// isEmpty                                                                 //
/////////////////////////////////////////////////////////////////////////////
@test
def isEmpty01(): Bool = List.isEmpty(Nil) == true

@test
def isEmpty02(): Bool = List.isEmpty(1 :: Nil) == false

@test
def isEmpty03(): Bool = List.isEmpty(1 :: 2 :: Nil) == false

/////////////////////////////////////////////////////////////////////////////
// head                                                                    //
/////////////////////////////////////////////////////////////////////////////
@test
def head01(): Bool = List.head(Nil) == None

@test
def head02(): Bool = List.head(1 :: Nil) == Some(1)

@test
def head03(): Bool = List.head(2 :: 1 :: Nil) == Some(2)

@test
def head04(): Bool = List.head(3 :: 2 :: 1 :: Nil) == Some(3)

/////////////////////////////////////////////////////////////////////////////
// last                                                                    //
/////////////////////////////////////////////////////////////////////////////
@test
def last01(): Bool = List.last(Nil) == None

@test
def last02(): Bool = List.last(1 :: Nil) == Some(1)

@test
def last03(): Bool = List.last(1 :: 2 :: Nil) == Some(2)

@test
def last04(): Bool = List.last(1 :: 2 :: 3 :: Nil) == Some(3)

/////////////////////////////////////////////////////////////////////////////
// length                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def length01(): Bool = List.length(Nil) == 0

@test
def length02(): Bool = List.length(1 :: Nil) == 1

@test
def length03(): Bool = List.length(1 :: 2 :: Nil) == 2

@test
def length04(): Bool = List.length(1 :: 2 :: 3 :: Nil) == 3

/////////////////////////////////////////////////////////////////////////////
// append                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def append01(): Bool = List.append(Nil, Nil) == Nil

@test
def append02(): Bool = List.append(Nil, 1 :: Nil) == 1 :: Nil

@test
def append03(): Bool = List.append(Nil, 1 :: 2 :: Nil) == 1 :: 2 :: Nil

@test
def append04(): Bool = List.append(1 :: Nil, Nil) == 1 :: Nil

@test
def append05(): Bool = List.append(1 :: 2 :: Nil, Nil) == 1 :: 2 :: Nil

@test
def append06(): Bool = List.append(1 :: Nil, 2 :: Nil) == 1 :: 2 :: Nil

@test
def append07(): Bool = List.append(1 :: 2 :: Nil, 3 :: Nil) == 1 :: 2 :: 3 :: Nil

@test
def append08(): Bool = List.append(1 :: Nil, 2 :: 3 :: Nil) == 1 :: 2 :: 3 :: Nil

/////////////////////////////////////////////////////////////////////////////
// memberOf                                                                //
/////////////////////////////////////////////////////////////////////////////
@test
def memberOf01(): Bool = List.memberOf(0, Nil) == false

@test
def memberOf02(): Bool = List.memberOf(0, 1 :: Nil) == false

@test
def memberOf03(): Bool = List.memberOf(0, 0 :: Nil) == true

@test
def memberOf04(): Bool = List.memberOf(0, 1 :: 2 :: Nil) == false

@test
def memberOf05(): Bool = List.memberOf(1, 1 :: 2 :: Nil) == true

@test
def memberOf06(): Bool = List.memberOf(2, 1 :: 2 :: Nil) == true

@test
def memberOf07(): Bool = List.memberOf(3, 1 :: 2 :: Nil) == false

/////////////////////////////////////////////////////////////////////////////
// indexOf                                                                 //
/////////////////////////////////////////////////////////////////////////////
@test
def indexOf01(): Bool = List.indexOf(0, Nil) == -1

@test
def indexOf02(): Bool = List.indexOf(0, 1 :: Nil) == -1

@test
def indexOf03(): Bool = List.indexOf(1, 1 :: Nil) == 0

@test
def indexOf04(): Bool = List.indexOf(0, 1 :: 2 :: Nil) == -1

@test
def indexOf05(): Bool = List.indexOf(1, 1 :: 2 :: Nil) == 0

@test
def indexOf06(): Bool = List.indexOf(2, 1 :: 2 :: Nil) == 1

/////////////////////////////////////////////////////////////////////////////
// find                                                                    //
/////////////////////////////////////////////////////////////////////////////
@test
def find01(): Bool = List.find(i -> i > 2, Nil) == None

@test
def find02(): Bool = List.find(i -> i > 2, 1 :: Nil) == None

@test
def find03(): Bool = List.find(i -> i > 2, 3 :: Nil) == Some(3)

@test
def find04(): Bool = List.find(i -> i > 2, 1 :: 2 :: Nil) == None

@test
def find05(): Bool = List.find(i -> i > 2, 6 :: -6 :: Nil) == Some(6)

@test
def find06(): Bool = List.find(i -> i > 2, -6 :: 6 :: Nil) == Some(6)

@test
def find07(): Bool = List.find(i -> i > 2, 6 :: 7 :: Nil) == Some(6)

/////////////////////////////////////////////////////////////////////////////
// findLeft                                                                //
/////////////////////////////////////////////////////////////////////////////
@test
def findLeft01(): Bool = List.findLeft(i -> i > 2, Nil) == None

@test
def findLeft02(): Bool = List.findLeft(i -> i > 2, 1 :: Nil) == None

@test
def findLeft03(): Bool = List.findLeft(i -> i > 2, 3 :: Nil) == Some(3)

@test
def findLeft04(): Bool = List.findLeft(i -> i > 2, 1 :: 2 :: Nil) == None

@test
def findLeft05(): Bool = List.findLeft(i -> i > 2, 6 :: -6 :: Nil) == Some(6)

@test
def findLeft06(): Bool = List.findLeft(i -> i > 2, -6 :: 6 :: Nil) == Some(6)

@test
def findLeft07(): Bool = List.findLeft(i -> i > 2, 6 :: 7 :: Nil) == Some(6)

/////////////////////////////////////////////////////////////////////////////
// findRight                                                               //
/////////////////////////////////////////////////////////////////////////////
@test
def findRight01(): Bool = List.findRight(i -> i > 2, Nil) == None

@test
def findRight02(): Bool = List.findRight(i -> i > 2, 1 :: Nil) == None

@test
def findRight03(): Bool = List.findRight(i -> i > 2, 3 :: Nil) == Some(3)

@test
def findRight04(): Bool = List.findRight(i -> i > 2, 1 :: 2 :: Nil) == None

@test
def findRight05(): Bool = List.findRight(i -> i > 2, 6 :: -6 :: Nil) == Some(6)

@test
def findRight06(): Bool = List.findRight(i -> i > 2, -6 :: 6 :: Nil) == Some(6)

@test
def findRight07(): Bool = List.findRight(i -> i > 2, 6 :: 7 :: Nil) == Some(7)

/////////////////////////////////////////////////////////////////////////////
// range                                                                   //
/////////////////////////////////////////////////////////////////////////////
@test
def range01(): Bool = List.range(1, 0) == Nil

@test
def range02(): Bool = List.range(1, 1) == Nil

@test
def range03(): Bool = List.range(1, 2) == 1 :: Nil

@test
def range04(): Bool = List.range(1, 3) == 1 :: 2 :: Nil

@test
def range05(): Bool = List.range(1, 4) == 1 :: 2 :: 3 :: Nil

/////////////////////////////////////////////////////////////////////////////
// repeat                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def repeat01(): Bool = List.repeat(1, -1) == Nil

@test
def repeat02(): Bool = List.repeat(1, 0) == Nil

@test
def repeat03(): Bool = List.repeat(1, 1) == 1 :: Nil

@test
def repeat04(): Bool = List.repeat(1, 2) == 1 :: 1 :: Nil

@test
def repeat05(): Bool = List.repeat(1, 3) == 1 :: 1 :: 1 :: Nil

/////////////////////////////////////////////////////////////////////////////
// scan                                                                    //
/////////////////////////////////////////////////////////////////////////////
@test
def scan01(): Bool = List.scan((i, b) -> if (b) i+1 else i+2, 1, Nil) == 1 :: Nil

@test
def scan02(): Bool = List.scan((i, b) -> if (b) i+1 else i+2, 1, false :: Nil) == 1 :: 3 :: Nil

@test
def scan03(): Bool = List.scan((i, b) -> if (b) i+1 else i+2, 1, true :: Nil) == 1 :: 2 :: Nil

@test
def scan04(): Bool = List.scan((i, b) -> if (b) i+1 else i+2, 1, false :: false :: Nil) == 1 :: 3 :: 5 :: Nil

@test
def scan05(): Bool = List.scan((i, b) -> if (b) i+1 else i+2, 1, false :: true :: Nil) == 1 :: 3 :: 4 :: Nil

@test
def scan06(): Bool = List.scan((i, b) -> if (b) i+1 else i+2, 1, true :: false :: Nil) == 1 :: 2 :: 4 :: Nil

@test
def scan07(): Bool = List.scan((i, b) -> if (b) i+1 else i+2, 1, true :: true :: Nil) == 1 :: 2 :: 3 :: Nil

/////////////////////////////////////////////////////////////////////////////
// scanLeft                                                                //
/////////////////////////////////////////////////////////////////////////////
@test
def scanLeft01(): Bool = List.scanLeft((i, b) -> if (b) i+1 else i+2, 1, Nil) == 1 :: Nil

@test
def scanLeft02(): Bool = List.scanLeft((i, b) -> if (b) i+1 else i+2, 1, false :: Nil) == 1 :: 3 :: Nil

@test
def scanLeft03(): Bool = List.scanLeft((i, b) -> if (b) i+1 else i+2, 1, true :: Nil) == 1 :: 2 :: Nil

@test
def scanLeft04(): Bool = List.scanLeft((i, b) -> if (b) i+1 else i+2, 1, false :: false :: Nil) == 1 :: 3 :: 5 :: Nil

@test
def scanLeft05(): Bool = List.scanLeft((i, b) -> if (b) i+1 else i+2, 1, false :: true :: Nil) == 1 :: 3 :: 4 :: Nil

@test
def scanLeft06(): Bool = List.scanLeft((i, b) -> if (b) i+1 else i+2, 1, true :: false :: Nil) == 1 :: 2 :: 4 :: Nil

@test
def scanLeft07(): Bool = List.scanLeft((i, b) -> if (b) i+1 else i+2, 1, true :: true :: Nil) == 1 :: 2 :: 3 :: Nil

/////////////////////////////////////////////////////////////////////////////
// scanRight                                                               //
/////////////////////////////////////////////////////////////////////////////
@test
def scanRight01(): Bool = List.scanRight((b, i) -> if (b) i+1 else i+2, 1, Nil) == 1 :: Nil

@test
def scanRight02(): Bool = List.scanRight((b, i) -> if (b) i+1 else i+2, 1, false :: Nil) == 3 :: 1 :: Nil

@test
def scanRight03(): Bool = List.scanRight((b, i) -> if (b) i+1 else i+2, 1, true :: Nil) == 2 :: 1 :: Nil

@test
def scanRight04(): Bool = List.scanRight((b, i) -> if (b) i+1 else i+2, 1, false :: false :: Nil) == 5 :: 3 :: 1 :: Nil

@test
def scanRight05(): Bool = List.scanRight((b, i) -> if (b) i+1 else i+2, 1, false :: true :: Nil) == 4 :: 2 :: 1 :: Nil

@test
def scanRight06(): Bool = List.scanRight((b, i) -> if (b) i+1 else i+2, 1, true :: false :: Nil) == 4 :: 3 :: 1 :: Nil

@test
def scanRight07(): Bool = List.scanRight((b, i) -> if (b) i+1 else i+2, 1, true :: true :: Nil) == 3 :: 2 :: 1 :: Nil

/////////////////////////////////////////////////////////////////////////////
// map                                                                     //
/////////////////////////////////////////////////////////////////////////////
@test
def map01(): Bool = List.map(i -> i > 2, Nil) == Nil

@test
def map02(): Bool = List.map(i -> i > 2, 1 :: Nil) == false :: Nil

@test
def map03(): Bool = List.map(i -> i > 2, 3 :: Nil) == true :: Nil

@test
def map04(): Bool = List.map(i -> i > 2, 1 :: 2 :: Nil) == false :: false :: Nil

@test
def map05(): Bool = List.map(i -> i > 2, 1 :: 8 :: Nil) == false :: true :: Nil

@test
def map06(): Bool = List.map(i -> i > 2, 8 :: 1 :: Nil) == true :: false :: Nil

@test
def map07(): Bool = List.map(i -> i > 2, 7 :: 8 :: Nil) == true :: true :: Nil

/////////////////////////////////////////////////////////////////////////////
// mapWithIndex                                                            //
/////////////////////////////////////////////////////////////////////////////
@test
def mapWithIndex01(): Bool = List.mapWithIndex((e, i) -> if (i < 1) e > 2 else e <= 2, Nil) == Nil

@test
def mapWithIndex02(): Bool = List.mapWithIndex((e, i) -> if (i < 1) e > 2 else e <= 2, 1 :: Nil) == false :: Nil

@test
def mapWithIndex03(): Bool = List.mapWithIndex((e, i) -> if (i < 1) e > 2 else e <= 2, 3 :: Nil) == true :: Nil

@test
def mapWithIndex04(): Bool = List.mapWithIndex((e, i) -> if (i < 1) e > 2 else e <= 2, 1 :: 2 :: Nil) == false :: true :: Nil

@test
def mapWithIndex05(): Bool = List.mapWithIndex((e, i) -> if (i < 1) e > 2 else e <= 2, 1 :: 8 :: Nil) == false :: false :: Nil

@test
def mapWithIndex06(): Bool = List.mapWithIndex((e, i) -> if (i < 1) e > 2 else e <= 2, 8 :: 1 :: Nil) == true :: true :: Nil

@test
def mapWithIndex07(): Bool = List.mapWithIndex((e, i) -> if (i < 1) e > 2 else e <= 2, 7 :: 8 :: Nil) == true :: false :: Nil

/////////////////////////////////////////////////////////////////////////////
// flatMap                                                                 //
/////////////////////////////////////////////////////////////////////////////
@test
def flatMap01(): Bool = List.flatMap(i -> List.repeat(i, i), Nil) == Nil

@test
def flatMap02(): Bool = List.flatMap(i -> List.repeat(i, i), 0 :: Nil) == Nil

@test
def flatMap03(): Bool = List.flatMap(i -> List.repeat(i, i), 1 :: Nil) == 1 :: Nil

@test
def flatMap04(): Bool = List.flatMap(i -> List.repeat(i, i), 2 :: Nil) == 2 :: 2 :: Nil

@test
def flatMap05(): Bool = List.flatMap(i -> List.repeat(i, i), 1 :: 2 :: Nil) == 1 :: 2 :: 2 :: Nil

@test
def flatMap06(): Bool = List.flatMap(i -> List.repeat(i, i), 2 :: 3 :: Nil) == 2 :: 2 :: 3 :: 3 :: 3 :: Nil

/////////////////////////////////////////////////////////////////////////////
// reverse                                                                 //
/////////////////////////////////////////////////////////////////////////////
@test
def reverse01(): Bool = List.reverse(Nil) == Nil

@test
def reverse02(): Bool = List.reverse(1 :: Nil) == 1 :: Nil

@test
def reverse03(): Bool = List.reverse(1 :: 2 :: Nil) == 2 :: 1 :: Nil

@test
def reverse04(): Bool = List.reverse(1 :: 1 :: Nil) == 1 :: 1 :: Nil

@test
def reverse05(): Bool = List.reverse(1 :: 2 :: 3 :: Nil) == 3 :: 2 :: 1 :: Nil

@test
def reverse06(): Bool = List.reverse(1 :: 2 :: 3 :: 4 :: Nil) == 4 :: 3 :: 2 :: 1 :: Nil

/////////////////////////////////////////////////////////////////////////////
// rotateLeft                                                              //
/////////////////////////////////////////////////////////////////////////////
@test
def rotateLeft01(): Bool = List.rotateLeft(0, Nil) == Nil

@test
def rotateLeft02(): Bool = List.rotateLeft(1, Nil) == Nil

@test
def rotateLeft03(): Bool = List.rotateLeft(0, 1 :: Nil) == 1 :: Nil

@test
def rotateLeft04(): Bool = List.rotateLeft(0, 1 :: 2 :: Nil) == 1 :: 2 :: Nil

@test
def rotateLeft05(): Bool = List.rotateLeft(1, 1 :: 2 :: Nil) == 2 :: 1 :: Nil

@test
def rotateLeft06(): Bool = List.rotateLeft(2, 1 :: 2 :: Nil) == 1 :: 2 :: Nil

@test
def rotateLeft07(): Bool = List.rotateLeft(3, 1 :: 2 :: Nil) == 2 :: 1 :: Nil

@test
def rotateLeft08(): Bool = List.rotateLeft(-1, 1 :: 2 :: Nil) == 2 :: 1 :: Nil

@test
def rotateLeft09(): Bool = List.rotateLeft(0, 1 :: 2 :: 3 :: Nil) == 1 :: 2 :: 3 :: Nil

@test
def rotateLeft10(): Bool = List.rotateLeft(1, 1 :: 2 :: 3 :: Nil) == 2 :: 3 :: 1 :: Nil

@test
def rotateLeft11(): Bool = List.rotateLeft(2, 1 :: 2 :: 3 :: Nil) == 3 :: 1 :: 2 :: Nil

/////////////////////////////////////////////////////////////////////////////
// rotateRight                                                             //
/////////////////////////////////////////////////////////////////////////////
@test
def rotateRight01(): Bool = List.rotateRight(0, Nil) == Nil

@test
def rotateRight02(): Bool = List.rotateRight(1, Nil) == Nil

@test
def rotateRight03(): Bool = List.rotateRight(0, 1 :: Nil) == 1 :: Nil

@test
def rotateRight04(): Bool = List.rotateRight(0, 1 :: 2 :: Nil) == 1 :: 2 :: Nil

@test
def rotateRight05(): Bool = List.rotateRight(1, 1 :: 2 :: Nil) == 2 :: 1 :: Nil

@test
def rotateRight06(): Bool = List.rotateRight(2, 1 :: 2 :: Nil) == 1 :: 2 :: Nil

@test
def rotateRight07(): Bool = List.rotateRight(3, 1 :: 2 :: Nil) == 2 :: 1 :: Nil

@test
def rotateRight08(): Bool = List.rotateRight(-1, 1 :: 2 :: Nil) == 2 :: 1 :: Nil

@test
def rotateRight09(): Bool = List.rotateRight(0, 1 :: 2 :: 3 :: Nil) == 1 :: 2 :: 3 :: Nil

@test
def rotateRight10(): Bool = List.rotateRight(1, 1 :: 2 :: 3 :: Nil) == 3 :: 1 :: 2 :: Nil

@test
def rotateRight11(): Bool = List.rotateRight(2, 1 :: 2 :: 3 :: Nil) == 2 :: 3 :: 1 :: Nil

/////////////////////////////////////////////////////////////////////////////
// update                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def update01(): Bool = List.update(0, 2, Nil) == Nil

@test
def update02(): Bool = List.update(-1, 2, 1 :: Nil) == 1 :: Nil

@test
def update03(): Bool = List.update(0, 2, 1 :: Nil) == 2 :: Nil

@test
def update04(): Bool = List.update(1, 2, 1 :: Nil) == 1 :: Nil

@test
def update05(): Bool = List.update(0, 5, 1 :: 2 :: Nil) == 5 :: 2 :: Nil

@test
def update06(): Bool = List.update(1, 5, 1 :: 2 :: Nil) == 1 :: 5 :: Nil

@test
def update07(): Bool = List.update(2, 5, 1 :: 2 :: Nil) == 1 :: 2 :: Nil

/////////////////////////////////////////////////////////////////////////////
// replace                                                                 //
/////////////////////////////////////////////////////////////////////////////
@test
def replace01(): Bool = List.replace(3, 4, Nil) == Nil

@test
def replace02(): Bool = List.replace(3, 4, 1 :: Nil) == 1 :: Nil

@test
def replace03(): Bool = List.replace(3, 4, 3 :: Nil) == 4 :: Nil

@test
def replace04(): Bool = List.replace(3, 4, 4 :: Nil) == 4 :: Nil

@test
def replace05(): Bool = List.replace(3, 4, 1 :: 2 :: Nil) == 1 :: 2 :: Nil

@test
def replace06(): Bool = List.replace(3, 4, 1 :: 3 :: Nil) == 1 :: 4 :: Nil

@test
def replace07(): Bool = List.replace(3, 4, 3 :: 4 :: Nil) == 4 :: 4 :: Nil

@test
def replace08(): Bool = List.replace(3, 4, 3 :: 3 :: Nil) == 4 :: 4 :: Nil

/////////////////////////////////////////////////////////////////////////////
// patch                                                                   //
/////////////////////////////////////////////////////////////////////////////
@test
def patch01(): Bool = List.patch(0, 0, Nil, Nil) == Nil

@test
def patch02(): Bool = List.patch(0, 2, 1 :: 2 :: Nil, Nil) == Nil

@test
def patch03(): Bool = List.patch(0, 2, Nil, 1 :: 2 :: Nil) == 1 :: 2 :: Nil

@test
def patch04(): Bool = List.patch(-3, 3, 1 :: 2 :: 4 :: Nil, 1 :: 2 :: Nil) == 1 :: 2 :: Nil

@test
def patch05(): Bool = List.patch(2, 3, 1 :: 2 :: 4 :: Nil, 1 :: 2 :: Nil) == 1 :: 2 :: Nil

@test
def patch06(): Bool = List.patch(0, 0, Nil, 1 :: Nil) == 1 :: Nil

@test
def patch07(): Bool = List.patch(1, 0, 2 :: Nil, 1 :: Nil) == 1 :: Nil

@test
def patch08(): Bool = List.patch(0, 1, 2 :: Nil, 1 :: Nil) == 2 :: Nil

@test
def patch09(): Bool = List.patch(0, 2, 2 :: 4 :: Nil, 1 :: Nil) == 2 :: Nil

@test
def patch10(): Bool = List.patch(-1, 2, 2 :: 4 :: Nil, 1 :: Nil) == 4 :: Nil

@test
def patch11(): Bool = List.patch(-1, 2, 3 :: 4 :: Nil, 1 :: 2 :: Nil) == 4 :: 2 :: Nil

@test
def patch12(): Bool = List.patch(1, 2, 3 :: 4 :: Nil, 1 :: 2 :: Nil) == 1 :: 3 :: Nil

@test
def patch13(): Bool = List.patch(-2, 2, 3 :: 4 :: Nil, 1 :: 2 :: Nil) == 1 :: 2 :: Nil

@test
def patch14(): Bool = List.patch(2, 2, 3 :: 4 :: Nil, 1 :: 2 :: Nil) == 1 :: 2 :: Nil

@test
def patch15(): Bool = List.patch(1, 1, 3 :: Nil, 1 :: 2 :: Nil) == 1 :: 3 :: Nil

@test
def patch16(): Bool = List.patch(0, 2, 3 :: 4 :: Nil, 1 :: 2 :: Nil) == 3 :: 4 :: Nil

@test
def patch17(): Bool = List.patch(0, 1, 4 :: Nil, 1 :: 2 :: 3 :: Nil) == 4 :: 2 :: 3 :: Nil

@test
def patch18(): Bool = List.patch(1, 1, 4 :: Nil, 1 :: 2 :: 3 :: Nil) == 1 :: 4 :: 3 :: Nil

@test
def patch19(): Bool = List.patch(2, 1, 4 :: Nil, 1 :: 2 :: 3 :: Nil) == 1 :: 2 :: 4 :: Nil

@test
def patch20(): Bool = List.patch(0, 2, 4 :: 5 :: Nil, 1 :: 2 :: 3 :: Nil) == 4 :: 5 :: 3 :: Nil

@test
def patch21(): Bool = List.patch(1, 2, 4 :: 5 :: Nil, 1 :: 2 :: 3 :: Nil) == 1 :: 4 :: 5 :: Nil

@test
def patch22(): Bool = List.patch(0, 2, 4 :: 5 :: 6 :: Nil, 1 :: 2 :: 3 :: Nil) == 4 :: 5 :: 3 :: Nil

@test
def patch23(): Bool = List.patch(0, 3, 4 :: 5 :: 6 :: Nil, 1 :: 2 :: 3 :: Nil) == 4 :: 5 :: 6 :: Nil

@test
def patch24(): Bool = List.patch(2, 4, 14 :: 15 :: 16 :: 17 :: Nil, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: Nil) ==
                    1 :: 2 :: 14 :: 15 :: 16 :: 17 :: 7 :: Nil

@test
def patch25(): Bool = List.patch(-2, 4, 14 :: 15 :: 16 :: 17 :: Nil, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: Nil) ==
                    16 :: 17 :: 3 :: 4 :: 5 :: 6 :: 7 :: Nil

@test
def patch26(): Bool = List.patch(4, 5, 14 :: 15 :: 16 :: 17 :: Nil, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: Nil) ==
                    1 :: 2 :: 3 :: 4 :: 14 :: 15 :: 16 :: Nil

@test
def patch27(): Bool = List.patch(4, 2, 14 :: 15 :: 16 :: 17 :: Nil, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: Nil) ==
                    1 :: 2 :: 3 :: 4 :: 14 :: 15 :: 7 :: Nil

@test
def patch28(): Bool = List.patch(-1, 10, -1 :: -2 :: -3 :: -4 :: -5 :: -6 :: -7 :: -8 :: Nil, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: Nil) ==
                    -2 :: -3 :: -4 :: -5 :: -6 :: -7 :: -8 :: Nil

/////////////////////////////////////////////////////////////////////////////
// permutations                                                            //
/////////////////////////////////////////////////////////////////////////////
@test
def permutations01(): Bool = List.permutations(Nil) == Nil :: Nil

@test
def permutations02(): Bool = List.permutations(1 :: Nil) == (1 :: Nil) :: Nil

@test
def permutations03(): Bool = List.permutations(1 :: 2 :: Nil) == (1 :: 2 :: Nil) :: (2 :: 1 :: Nil) :: Nil

@test
def permutations04(): Bool = List.permutations(1 :: 2 :: 3 :: Nil) == (1 :: 2 :: 3 :: Nil) :: (1 :: 3 :: 2 :: Nil) ::
                                                                             (2 :: 1 :: 3 :: Nil) :: (2 :: 3 :: 1 :: Nil) ::
                                                                             (3 :: 1 :: 2 :: Nil) :: (3 :: 2 :: 1 :: Nil) :: Nil

/////////////////////////////////////////////////////////////////////////////
// subsequences                                                            //
/////////////////////////////////////////////////////////////////////////////
@test
def subsequences01(): Bool = List.subsequences(Nil) == Nil :: Nil

@test
def subsequences02(): Bool = List.subsequences(1 :: Nil) == (1 :: Nil) :: Nil :: Nil

@test
def subsequences03(): Bool = List.subsequences(1 :: 2 :: Nil) == (1 :: 2 :: Nil) :: (1 :: Nil) ::
                                                                        (2 :: Nil) :: Nil :: Nil

@test
def subsequences04(): Bool = List.subsequences(1 :: 2 :: 3 :: Nil) == (1 :: 2 :: 3 :: Nil) :: (1 :: 2 :: Nil) ::
                                                                             (1 :: 3 :: Nil) :: (1 :: Nil) ::
                                                                             (2 :: 3 :: Nil) :: (2 :: Nil) ::
                                                                             (3 :: Nil) :: Nil :: Nil

/////////////////////////////////////////////////////////////////////////////
// intersperse                                                             //
/////////////////////////////////////////////////////////////////////////////
@test
def intersperse01(): Bool = List.intersperse(11, Nil) == Nil

@test
def intersperse02(): Bool = List.intersperse(11, 1 :: Nil) == 1 :: Nil

@test
def intersperse03(): Bool = List.intersperse(11, 1 :: 2 :: Nil) == 1 :: 11 :: 2 :: Nil

@test
def intersperse04(): Bool = List.intersperse(11, 1 :: 2 :: 3 :: Nil) == 1 :: 11 :: 2 :: 11 :: 3 :: Nil

@test
def intersperse05(): Bool = List.intersperse(11, 1 :: 2 :: 3 :: 4 :: Nil) == 1 :: 11 :: 2 :: 11 :: 3 :: 11 :: 4 :: Nil

/////////////////////////////////////////////////////////////////////////////
// transpose                                                               //
/////////////////////////////////////////////////////////////////////////////
@test
def transpose01(): Bool = List.transpose(Nil) == Nil

@test
def transpose02(): Bool = List.transpose(Nil :: Nil) == Nil :: Nil

@test
def transpose03(): Bool = List.transpose(Nil :: Nil :: Nil) == Nil :: Nil :: Nil

@test
def transpose04(): Bool = List.transpose(Nil :: Nil :: Nil :: Nil) == Nil :: Nil :: Nil :: Nil

@test
def transpose05(): Bool = List.transpose((1 :: Nil) :: Nil) == (1 :: Nil) :: Nil

@test
def transpose06(): Bool = List.transpose((1 :: 2 :: Nil) :: Nil) == (1 :: Nil) :: (2 :: Nil) :: Nil

@test
def transpose07(): Bool = List.transpose((1 :: 2 :: 3 :: Nil) :: Nil) ==
                        (1 :: Nil) :: (2 :: Nil) :: (3 :: Nil) :: Nil

@test
def transpose08(): Bool = List.transpose((1 :: 2 :: 3 :: 4 :: Nil) :: Nil) ==
                        (1 :: Nil) :: (2 :: Nil) :: (3 :: Nil) :: (4 :: Nil) :: Nil

@test
def transpose09(): Bool = List.transpose((1 :: Nil) :: (2 :: Nil) :: Nil) == (1 :: 2 :: Nil) :: Nil

@test
def transpose10(): Bool = List.transpose((1 :: Nil) :: (2 :: Nil) :: (3 :: Nil) :: Nil) ==
                        (1 :: 2 :: 3 :: Nil) :: Nil

@test
def transpose11(): Bool = List.transpose((1 :: Nil) :: (2 :: Nil) :: (3 :: Nil) :: (4 :: Nil) :: Nil) ==
                        (1 :: 2 :: 3 :: 4 :: Nil) :: Nil

@test
def transpose12(): Bool = List.transpose((1 :: 2 :: Nil) :: (3 :: 4 :: Nil) :: Nil) ==
                        (1 :: 3 :: Nil) :: (2 :: 4 :: Nil) :: Nil

@test
def transpose13(): Bool = List.transpose((1 :: 2 :: 3 :: Nil) :: (4 :: 5 :: 6 :: Nil) :: Nil) ==
                        (1 :: 4 :: Nil) :: (2 :: 5 :: Nil) :: (3 :: 6 :: Nil) :: Nil

@test
def transpose14(): Bool = List.transpose((1 :: 2 :: 3 :: 4 :: Nil) :: (5 :: 6 :: 7 :: 8 :: Nil) :: Nil) ==
                        (1 :: 5 :: Nil) :: (2 :: 6 :: Nil) :: (3 :: 7 :: Nil) :: (4 :: 8 :: Nil) :: Nil

@test
def transpose15(): Bool = List.transpose((1 :: 2 :: 3 :: 4 :: 5 :: Nil) :: (6 :: 7 :: 8 :: 9 :: 10 :: Nil) :: Nil) ==
                        (1 :: 6 :: Nil) :: (2 :: 7 :: Nil) :: (3 :: 8 :: Nil) :: (4 :: 9 :: Nil) :: (5 :: 10 :: Nil) :: Nil

@test
def transpose16(): Bool = List.transpose((1 :: 2 :: Nil) :: (3 :: 4 :: Nil) :: (5 :: 6 :: Nil) :: Nil) ==
                        (1 :: 3 :: 5 :: Nil) :: (2 :: 4 :: 6 :: Nil) :: Nil

@test
def transpose17(): Bool = List.transpose((1 :: 2 :: Nil) :: (3 :: 4 :: Nil) :: (5 :: 6 :: Nil) :: (7 :: 8 :: Nil) :: Nil) ==
                        (1 :: 3 :: 5 :: 7 :: Nil) :: (2 :: 4 :: 6 :: 8 :: Nil) :: Nil

@test
def transpose18(): Bool = List.transpose((1 :: 2 :: Nil) :: (3 :: 4 :: Nil) :: (5 :: 6 :: Nil) :: (7 :: 8 :: Nil) :: (9 :: 10 :: Nil) :: Nil) ==
                        (1 :: 3 :: 5 :: 7 :: 9 :: Nil) :: (2 :: 4 :: 6 :: 8 :: 10 :: Nil) :: Nil

@test
def transpose19(): Bool = List.transpose((1 :: 2 :: 3 :: Nil) :: (4 :: 5 :: 6 :: Nil) :: (7 :: 8 :: 9 :: Nil) :: Nil) ==
                        (1 :: 4 :: 7 :: Nil) :: (2 :: 5 :: 8 :: Nil) :: (3 :: 6 :: 9 :: Nil) :: Nil

@test
def transpose20(): Bool = List.transpose((1 :: 2 :: 3 :: Nil) :: (4 :: 5 :: Nil) :: (7 :: 8 :: 9 :: Nil) :: Nil) ==
                        (1 :: 2 :: 3 :: Nil) :: (4 :: 5 :: Nil) :: (7 :: 8 :: 9 :: Nil) :: Nil

@test
def transpose21(): Bool = List.transpose((1 :: 2 :: 3 :: Nil) :: Nil :: (7 :: 8 :: 9 :: Nil) :: Nil) ==
                        (1 :: 2 :: 3 :: Nil) :: Nil :: (7 :: 8 :: 9 :: Nil) :: Nil

@test
def transpose22(): Bool = List.transpose((1 :: 2 :: 3 :: Nil) :: (4 :: 5 :: 6 :: Nil) :: (7 :: 8 :: 9 :: 10 :: Nil) :: Nil) ==
                        (1 :: 2 :: 3 :: Nil) :: (4 :: 5 :: 6 :: Nil) :: (7 :: 8 :: 9 :: 10 :: Nil) :: Nil

/////////////////////////////////////////////////////////////////////////////
// isPrefixOf                                                              //
/////////////////////////////////////////////////////////////////////////////
@test
def isPrefixOf01(): Bool = List.isPrefixOf(Nil, Nil) == true

@test
def isPrefixOf02(): Bool = List.isPrefixOf(1 :: Nil, Nil) == false

@test
def isPrefixOf03(): Bool = List.isPrefixOf(Nil, 1 :: Nil) == true

@test
def isPrefixOf04(): Bool = List.isPrefixOf(1 :: Nil, 1 :: Nil) == true

@test
def isPrefixOf05(): Bool = List.isPrefixOf(2 :: Nil, 1 :: Nil) == false

@test
def isPrefixOf06(): Bool = List.isPrefixOf(1 :: 2 :: Nil, 1 :: Nil) == false

@test
def isPrefixOf07(): Bool = List.isPrefixOf(Nil, 1 :: 2 :: Nil) == true

@test
def isPrefixOf08(): Bool = List.isPrefixOf(1 :: Nil, 1 :: 2 :: Nil) == true

@test
def isPrefixOf09(): Bool = List.isPrefixOf(2 :: Nil, 1 :: 2 :: Nil) == false

@test
def isPrefixOf10(): Bool = List.isPrefixOf(1 :: 2 :: Nil, 1 :: 2 :: Nil) == true

@test
def isPrefixOf11(): Bool = List.isPrefixOf(1 :: 3 :: Nil, 1 :: 2 :: Nil) == false

@test
def isPrefixOf12(): Bool = List.isPrefixOf(1 :: 2 :: 3 :: Nil, 1 :: 2 :: Nil) == false

@test
def isPrefixOf13(): Bool = List.isPrefixOf(1 :: 2 :: 3 :: Nil, 89 :: 11 :: 1 :: 2 :: 3 :: 4 :: Nil) == false

/////////////////////////////////////////////////////////////////////////////
// isInfixOf                                                               //
/////////////////////////////////////////////////////////////////////////////
@test
def isInfixOf01(): Bool = List.isInfixOf(Nil, Nil) == true

@test
def isInfixOf02(): Bool = List.isInfixOf(1 :: Nil, Nil) == false

@test
def isInfixOf03(): Bool = List.isInfixOf(Nil, 1 :: Nil) == true

@test
def isInfixOf04(): Bool = List.isInfixOf(1 :: Nil, 1 :: Nil) == true

@test
def isInfixOf05(): Bool = List.isInfixOf(2 :: Nil, 1 :: Nil) == false

@test
def isInfixOf06(): Bool = List.isInfixOf(1 :: 2 :: Nil, 1 :: Nil) == false

@test
def isInfixOf07(): Bool = List.isInfixOf(Nil, 1 :: 2 :: Nil) == true

@test
def isInfixOf08(): Bool = List.isInfixOf(1 :: Nil, 1 :: 2 :: Nil) == true

@test
def isInfixOf09(): Bool = List.isInfixOf(2 :: Nil, 1 :: 2 :: Nil) == true

@test
def isInfixOf10(): Bool = List.isInfixOf(1 :: 2 :: Nil, 1 :: 2 :: Nil) == true

@test
def isInfixOf11(): Bool = List.isInfixOf(1 :: 3 :: Nil, 1 :: 2 :: Nil) == false

@test
def isInfixOf12(): Bool = List.isInfixOf(1 :: 2 :: 3 :: Nil, 1 :: 2 :: Nil) == false

@test
def isInfixOf13(): Bool = List.isInfixOf(1 :: 2 :: 3 :: Nil, 89 :: 11 :: 1 :: 2 :: 3 :: 4 :: Nil) == true

/////////////////////////////////////////////////////////////////////////////
// isSuffixOf                                                              //
/////////////////////////////////////////////////////////////////////////////
@test
def isSuffixOf01(): Bool = List.isSuffixOf(Nil, Nil) == true

@test
def isSuffixOf02(): Bool = List.isSuffixOf(1 :: Nil, Nil) == false

@test
def isSuffixOf03(): Bool = List.isSuffixOf(Nil, 1 :: Nil)  == true

@test
def isSuffixOf04(): Bool = List.isSuffixOf(1 :: Nil, 1 :: Nil) == true

@test
def isSuffixOf05(): Bool = List.isSuffixOf(2 :: Nil, 1 :: Nil) == false

@test
def isSuffixOf06(): Bool = List.isSuffixOf(1 :: 2 :: Nil, 1 :: Nil) == false

@test
def isSuffixOf07(): Bool = List.isSuffixOf(Nil, 1 :: 2 :: Nil) == true

@test
def isSuffixOf08(): Bool = List.isSuffixOf(1 :: Nil, 1 :: 2 :: Nil) == false

@test
def isSuffixOf09(): Bool = List.isSuffixOf(2 :: Nil, 1 :: 2 :: Nil) == true

@test
def isSuffixOf10(): Bool = List.isSuffixOf(1 :: 2 :: Nil, 1 :: 2 :: Nil) == true

@test
def isSuffixOf11(): Bool = List.isSuffixOf(1 :: 3 :: Nil, 1 :: 2 :: Nil) == false

@test
def isSuffixOf12(): Bool = List.isSuffixOf(1 :: 2 :: 3 :: Nil, 1 :: 2 :: Nil) == false

@test
def isSuffixOf13(): Bool = List.isSuffixOf(1 :: 2 :: 3 :: Nil, 89 :: 11 :: 1 :: 2 :: 3 :: Nil) == true

/////////////////////////////////////////////////////////////////////////////
// fold                                                                    //
/////////////////////////////////////////////////////////////////////////////
@test
def fold01(): Bool = List.fold((i, e) -> (i - e)*(e % 2 + 1), 100, Nil) == 100

@test
def fold02(): Bool = List.fold((i, e) -> (i - e)*(e % 2 + 1), 100, 1 :: Nil) == 198

@test
def fold03(): Bool = List.fold((i, e) -> (i - e)*(e % 2 + 1), 100, 1 :: 2 :: Nil) == 196

@test
def fold04(): Bool = List.fold((i, e) -> (i - e)*(e % 2 + 1), 100, 1 :: 2 :: 3 :: Nil) == 386

/////////////////////////////////////////////////////////////////////////////
// foldLeft                                                                //
/////////////////////////////////////////////////////////////////////////////
@test
def foldLeft01(): Bool = List.foldLeft((i, e) -> (i - e)*(e % 2 + 1), 100, Nil) == 100

@test
def foldLeft02(): Bool = List.foldLeft((i, e) -> (i - e)*(e % 2 + 1), 100, 1 :: Nil) == 198

@test
def foldLeft03(): Bool = List.foldLeft((i, e) -> (i - e)*(e % 2 + 1), 100, 1 :: 2 :: Nil) == 196

@test
def foldLeft04(): Bool = List.foldLeft((i, e) -> (i - e)*(e % 2 + 1), 100, 1 :: 2 :: 3 :: Nil) == 386

/////////////////////////////////////////////////////////////////////////////
// foldRight                                                               //
/////////////////////////////////////////////////////////////////////////////
@test
def foldRight01(): Bool = List.foldRight((e, i) -> (i - e)*(e % 2 + 1), 100, Nil) == 100

@test
def foldRight02(): Bool = List.foldRight((e, i) -> (i - e)*(e % 2 + 1), 100, 1 :: Nil) == 198

@test
def foldRight03(): Bool = List.foldRight((e, i) -> (i - e)*(e % 2 + 1), 100, 1 :: 2 :: Nil) == 194

@test
def foldRight04(): Bool = List.foldRight((e, i) -> (i - e)*(e % 2 + 1), 100, 1 :: 2 :: 3 :: Nil) == 382

/////////////////////////////////////////////////////////////////////////////
// reduce                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def reduce01(): Bool = List.reduce((a, b) -> a-b, Nil: List[Int]) == None

@test
def reduce02(): Bool = List.reduce((a, b) -> a-b, 1 :: Nil) == Some(1)

@test
def reduce03(): Bool = List.reduce((a, b) -> a-b, 1 :: 2 :: Nil) == Some(-1)

@test
def reduce04(): Bool = List.reduce((a, b) -> a-b, 1 :: 2 :: 3 :: Nil) == Some(-4)

@test
def reduce05(): Bool = List.reduce((a, b) -> a-b, 1 :: 2 :: 3 :: 4 :: Nil) == Some(-8)

/////////////////////////////////////////////////////////////////////////////
// reduceLeft                                                              //
/////////////////////////////////////////////////////////////////////////////
@test
def reduceLeft01(): Bool = List.reduceLeft((a, b) -> a-b, Nil: List[Int]) == None

@test
def reduceLeft02(): Bool = List.reduceLeft((a, b) -> a-b, 1 :: Nil) == Some(1)

@test
def reduceLeft03(): Bool = List.reduceLeft((a, b) -> a-b, 1 :: 2 :: Nil) == Some(-1)

@test
def reduceLeft04(): Bool = List.reduceLeft((a, b) -> a-b, 1 :: 2 :: 3 :: Nil) == Some(-4)

@test
def reduceLeft05(): Bool = List.reduceLeft((a, b) -> a-b, 1 :: 2 :: 3 :: 4 :: Nil) == Some(-8)

/////////////////////////////////////////////////////////////////////////////
// reduceRight                                                             //
/////////////////////////////////////////////////////////////////////////////
@test
def reduceRight01(): Bool = List.reduceRight((a, b) -> a-b, Nil: List[Int]) == None

@test
def reduceRight02(): Bool = List.reduceRight((a, b) -> a-b, 1 :: Nil) == Some(1)

@test
def reduceRight03(): Bool = List.reduceRight((a, b) -> a-b, 1 :: 2 :: Nil) == Some(-1)

@test
def reduceRight04(): Bool = List.reduceRight((a, b) -> a-b, 1 :: 2 :: 3 :: Nil) == Some(2)

@test
def reduceRight05(): Bool = List.reduceRight((a, b) -> a-b, 1 :: 2 :: 3 :: 4 :: Nil) == Some(-2)

/////////////////////////////////////////////////////////////////////////////
// count                                                                   //
/////////////////////////////////////////////////////////////////////////////
@test
def count01(): Bool = List.count(i -> i > 3, Nil) == 0

@test
def count02(): Bool = List.count(i -> i > 3, 1 :: Nil) == 0

@test
def count03(): Bool = List.count(i -> i > 3, 4 :: Nil) == 1

@test
def count04(): Bool = List.count(i -> i > 3, 1 :: 2 :: Nil) == 0

@test
def count05(): Bool = List.count(i -> i > 3, 1 :: 8 :: Nil) == 1

@test
def count06(): Bool = List.count(i -> i > 3, 8 :: 1 :: Nil) == 1

@test
def count07(): Bool = List.count(i -> i > 3, 6 :: 7 :: Nil) == 2

/////////////////////////////////////////////////////////////////////////////
// flatten                                                                 //
/////////////////////////////////////////////////////////////////////////////
@test
def flatten01(): Bool = List.flatten(Nil) == Nil

@test
def flatten02(): Bool = List.flatten(Nil :: Nil) == Nil

@test
def flatten03(): Bool = List.flatten((1 :: Nil) :: Nil) == 1 :: Nil

@test
def flatten04(): Bool = List.flatten((1 :: 2 :: Nil) :: Nil) == 1 :: 2 :: Nil

@test
def flatten05(): Bool = List.flatten(Nil :: Nil :: Nil) == Nil

@test
def flatten06(): Bool = List.flatten((1 :: Nil) :: Nil :: Nil) == 1 :: Nil

@test
def flatten07(): Bool = List.flatten(Nil :: (1 :: Nil) :: Nil) == 1 :: Nil

@test
def flatten08(): Bool = List.flatten((1 :: Nil) :: (2 :: Nil) :: Nil) == 1 :: 2 :: Nil

@test
def flatten09(): Bool = List.flatten((1 :: 2 :: Nil) :: (3 :: 4 :: 5 :: Nil) :: Nil) == 1 :: 2 :: 3 :: 4 :: 5 :: Nil

@test
def flatten10(): Bool = List.flatten((1 :: Nil) :: (2 :: 3 :: Nil) :: (4 :: Nil) :: Nil) == 1 :: 2 :: 3 :: 4 :: Nil

/////////////////////////////////////////////////////////////////////////////
// exists                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def exists01(): Bool = List.exists(i -> i > 3, Nil) == false

@test
def exists02(): Bool = List.exists(i -> i > 3, 1 :: Nil) == false

@test
def exists03(): Bool = List.exists(i -> i > 3, 5 :: Nil) == true

@test
def exists04(): Bool = List.exists(i -> i > 3, 1 :: 2 :: Nil) == false

@test
def exists05(): Bool = List.exists(i -> i > 3, 1 :: 6 :: Nil) == true

@test
def exists06(): Bool = List.exists(i -> i > 3, 6 :: 1 :: Nil) == true

@test
def exists07(): Bool = List.exists(i -> i > 3, 16 :: 6 :: Nil) == true

@test
def exists08(): Bool = List.exists(i -> i > 3, 1 :: -9 :: 3 :: Nil) == false

@test
def exists09(): Bool = List.exists(i -> i > 3, 1 :: 9 :: 3 :: Nil) == true

/////////////////////////////////////////////////////////////////////////////
// forall                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def forall01(): Bool = List.forall(i -> i > 3, Nil) == true

@test
def forall02(): Bool = List.forall(i -> i > 3, 1 :: Nil) == false

@test
def forall03(): Bool = List.forall(i -> i > 3, 5 :: Nil) == true

@test
def forall04(): Bool = List.forall(i -> i > 3, 1 :: 2 :: Nil) == false

@test
def forall05(): Bool = List.forall(i -> i > 3, 1 :: 6 :: Nil) == false

@test
def forall06(): Bool = List.forall(i -> i > 3, 6 :: 1 :: Nil) == false

@test
def forall07(): Bool = List.forall(i -> i > 3, 16 :: 6 :: Nil) == true

@test
def forall08(): Bool = List.forall(i -> i > 3, 1 :: -9 :: 3 :: Nil) == false

@test
def forall09(): Bool = List.forall(i -> i > 3, 1 :: 9 :: 3 :: Nil) == false

@test
def forall10(): Bool = List.forall(i -> i > 3, 11 :: 9 :: 31 :: Nil) == true

/////////////////////////////////////////////////////////////////////////////
// filter                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def filter01(): Bool = List.filter(i -> i > 3, Nil) == Nil

@test
def filter02(): Bool = List.filter(i -> i > 3, 2 :: Nil) == Nil

@test
def filter03(): Bool = List.filter(i -> i > 3, 4 :: Nil) == 4 :: Nil

@test
def filter04(): Bool = List.filter(i -> i > 3, 1 :: 3 :: Nil) == Nil

@test
def filter05(): Bool = List.filter(i -> i > 3, 1 :: 8 :: Nil) == 8 :: Nil

@test
def filter06(): Bool = List.filter(i -> i > 3, 8 :: 1 :: Nil) == 8 :: Nil

@test
def filter07(): Bool = List.filter(i -> i > 3, 8 :: 9 :: Nil) == 8 :: 9 :: Nil

@test
def filter08(): Bool = List.filter(i -> i > 3, 1 :: 4 :: 11 :: 2 :: -22 :: 17 :: Nil) == 4 :: 11 :: 17 :: Nil

/////////////////////////////////////////////////////////////////////////////
// init                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def init01(): Bool = List.init(Nil) == None

@test
def init02(): Bool = List.init(0 :: Nil) == Some(Nil)

@test
def init03(): Bool = List.init(0 :: 1 :: Nil) == Some(0 :: Nil)

@test
def init04(): Bool = List.init(2 :: 1 :: 0 :: Nil) == Some(2 :: 1 :: Nil)

@test
def init05(): Bool = List.init('a' :: 'b' :: 'c' :: 'd' :: Nil) == Some('a' :: 'b' :: 'c' :: Nil)

@test
def init06(): Bool = List.init("test1" :: "test2" :: "test3" :: Nil) == Some("test1" :: "test2" :: Nil)

@test
def init07(): Bool = List.init(2 :: 1 :: 0 :: -1 :: Nil) == Some(2 :: 1 :: 0 :: Nil)

/////////////////////////////////////////////////////////////////////////////
// slice                                                                   //
/////////////////////////////////////////////////////////////////////////////
@test
def slice01(): Bool = List.slice(0, 0, Nil) == Nil

@test
def slice02(): Bool = List.slice(-1, 1, Nil) == Nil

@test
def slice03(): Bool = List.slice(0, 0, 1 :: Nil) == Nil

@test
def slice04(): Bool = List.slice(0, 1, 1 :: Nil) == 1 :: Nil

@test
def slice05(): Bool = List.slice(0, 2, 1 :: Nil) == 1 :: Nil

@test
def slice06(): Bool = List.slice(2, 5, 1 :: Nil) == Nil

@test
def slice07(): Bool = List.slice(-1, 1, 1 :: Nil) == 1 :: Nil

@test
def slice08(): Bool = List.slice(0, 1, 1 :: 2 :: Nil) == 1 :: Nil

@test
def slice09(): Bool = List.slice(0, 2, 1 :: 2 :: Nil) == 1 :: 2 :: Nil

@test
def slice10(): Bool = List.slice(1, 2, 1 :: 2 :: Nil) == 2 :: Nil

@test
def slice11(): Bool = List.slice(0, 3, 1 :: 2 :: 3 :: Nil) == 1 :: 2 :: 3 :: Nil

@test
def slice12(): Bool = List.slice(0, 2, 1 :: 2 :: 3 :: Nil) == 1 :: 2 :: Nil

@test
def slice13(): Bool = List.slice(1, 3, 1 :: 2 :: 3 :: Nil) == 2 :: 3 :: Nil

@test
def slice14(): Bool = List.slice(1, 2, 1 :: 2 :: 3 :: Nil) == 2 :: Nil

/////////////////////////////////////////////////////////////////////////////
// partition                                                               //
/////////////////////////////////////////////////////////////////////////////
@test
def partition01(): Bool = List.partition(i -> i > 3, Nil) == (Nil, Nil)

@test
def partition02(): Bool = List.partition(i -> i > 3, 1 :: Nil) == (Nil, 1 :: Nil)

@test
def partition03(): Bool = List.partition(i -> i > 3, 4 :: Nil) == (4 :: Nil, Nil)

@test
def partition04(): Bool = List.partition(i -> i > 3, 1 :: 2 :: Nil) == (Nil, 1 :: 2 :: Nil)

@test
def partition05(): Bool = List.partition(i -> i > 3, 1 :: 5 :: Nil) == (5 :: Nil, 1 :: Nil)

@test
def partition06(): Bool = List.partition(i -> i > 3, 5 :: 1 :: Nil) == (5 :: Nil, 1 :: Nil)

@test
def partition07(): Bool = List.partition(i -> i > 3, 5 :: 8 :: Nil) == (5 :: 8 :: Nil, Nil)

@test
def partition08(): Bool = List.partition(i -> i > 3, 4 :: -3 :: -5 :: 1 :: 2 :: 16 :: 7 :: 1 :: 7 :: Nil) ==
                        (4 :: 16 :: 7 :: 7 :: Nil, -3 :: -5 :: 1 :: 2 :: 1 :: Nil)

/////////////////////////////////////////////////////////////////////////////
// span                                                                    //
/////////////////////////////////////////////////////////////////////////////
@test
def span01(): Bool = List.span(i -> i > 3, Nil) == (Nil, Nil)

@test
def span02(): Bool = List.span(i -> i > 3, 1 :: Nil) == (Nil, 1 :: Nil)

@test
def span03(): Bool = List.span(i -> i > 3, 4 :: Nil) == (4 :: Nil, Nil)

@test
def span04(): Bool = List.span(i -> i > 3, 1 :: 2 :: Nil) == (Nil, 1 :: 2 :: Nil)

@test
def span05(): Bool = List.span(i -> i > 3, 1 :: 5 :: Nil) == (Nil, 1 :: 5 :: Nil)

@test
def span06(): Bool = List.span(i -> i > 3, 5 :: 1 :: Nil) == (5 :: Nil, 1 :: Nil)

@test
def span07(): Bool = List.span(i -> i > 3, 5 :: 8 :: Nil) == (5 :: 8 :: Nil, Nil)

@test
def span08(): Bool = List.span(i -> i > 3, 4 :: 6 :: -3 :: 11 :: -5 :: 1 :: 2 :: 16 :: 7 :: 1 :: 7 :: Nil) ==
                        (4 :: 6 :: Nil, -3 :: 11 :: -5 :: 1 :: 2 :: 16 :: 7 :: 1 :: 7 :: Nil)

/////////////////////////////////////////////////////////////////////////////
// drop                                                                    //
/////////////////////////////////////////////////////////////////////////////
@test
def drop01(): Bool = List.drop(-1, Nil) == Nil

@test
def drop02(): Bool = List.drop(0, Nil) == Nil

@test
def drop03(): Bool = List.drop(1, Nil) == Nil

@test
def drop04(): Bool = List.drop(-1, 1 :: Nil) == 1 :: Nil

@test
def drop05(): Bool = List.drop(0, 1 :: Nil) == 1 :: Nil

@test
def drop06(): Bool = List.drop(1, 1 :: Nil) == Nil

@test
def drop07(): Bool = List.drop(2, 1 :: Nil) == Nil

@test
def drop08(): Bool = List.drop(0, 1 :: 2 :: Nil) == 1 :: 2 :: Nil

@test
def drop09(): Bool = List.drop(1, 1 :: 2 :: Nil) == 2 :: Nil

@test
def drop10(): Bool = List.drop(2, 1 :: 2 :: Nil) == Nil

@test
def drop11(): Bool = List.drop(2, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil) == 3 :: 4 :: 5 :: 6 :: Nil

@test
def drop12(): Bool = List.drop(4, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil) == 5 :: 6 :: Nil

/////////////////////////////////////////////////////////////////////////////
// dropWhile                                                               //
/////////////////////////////////////////////////////////////////////////////
@test
def dropWhile01(): Bool = List.dropWhile(i -> i > 3, Nil) == Nil

@test
def dropWhile02(): Bool = List.dropWhile(i -> i > 3, 1 :: Nil) == 1 :: Nil

@test
def dropWhile03(): Bool = List.dropWhile(i -> i > 3, 4 :: Nil) == Nil

@test
def dropWhile04(): Bool = List.dropWhile(i -> i > 3, 1 :: 2 :: Nil) == 1 :: 2 :: Nil

@test
def dropWhile05(): Bool = List.dropWhile(i -> i > 3, 1 :: 5 :: Nil) == 1 :: 5 :: Nil

@test
def dropWhile06(): Bool = List.dropWhile(i -> i > 3, 5 :: 1 :: Nil) == 1 :: Nil

@test
def dropWhile07(): Bool = List.dropWhile(i -> i > 3, 5 :: 8 :: Nil) == Nil

@test
def dropWhile08(): Bool = List.dropWhile(i -> i > 3, 4 :: 6 :: -3 :: 11 :: -5 :: 1 :: 2 :: 16 :: 7 :: 1 :: 7 :: Nil) ==
                        -3 :: 11 :: -5 :: 1 :: 2 :: 16 :: 7 :: 1 :: 7 :: Nil

/////////////////////////////////////////////////////////////////////////////
// take                                                                    //
/////////////////////////////////////////////////////////////////////////////
@test
def take01(): Bool = List.take(-1, Nil) == Nil

@test
def take02(): Bool = List.take(0, Nil) == Nil

@test
def take03(): Bool = List.take(1, Nil) == Nil

@test
def take04(): Bool = List.take(-1, 1 :: Nil) == Nil

@test
def take05(): Bool = List.take(0, 1 :: Nil) == Nil

@test
def take06(): Bool = List.take(1, 1 :: Nil) == 1 :: Nil

@test
def take07(): Bool = List.take(2, 1 :: Nil) == 1 :: Nil

@test
def take08(): Bool = List.take(0, 1 :: 2 :: Nil) == Nil

@test
def take09(): Bool = List.take(1, 1 :: 2 :: Nil) == 1 :: Nil

@test
def take10(): Bool = List.take(2, 1 :: 2 :: Nil) == 1 :: 2 :: Nil

@test
def take11(): Bool = List.take(2, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil) == 1 :: 2 :: Nil

@test
def take12(): Bool = List.take(4, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil) == 1 :: 2 :: 3 :: 4 :: Nil

/////////////////////////////////////////////////////////////////////////////
// takeWhile                                                               //
/////////////////////////////////////////////////////////////////////////////
@test
def takeWhile01(): Bool = List.takeWhile(i -> i > 3, Nil) == Nil

@test
def takeWhile02(): Bool = List.takeWhile(i -> i > 3, 1 :: Nil) == Nil

@test
def takeWhile03(): Bool = List.takeWhile(i -> i > 3, 4 :: Nil) == 4 :: Nil

@test
def takeWhile04(): Bool = List.takeWhile(i -> i > 3, 1 :: 2 :: Nil) == Nil

@test
def takeWhile05(): Bool = List.takeWhile(i -> i > 3, 1 :: 5 :: Nil) == Nil

@test
def takeWhile06(): Bool = List.takeWhile(i -> i > 3, 5 :: 1 :: Nil) == 5 :: Nil

@test
def takeWhile07(): Bool = List.takeWhile(i -> i > 3, 5 :: 8 :: Nil) == 5 :: 8 :: Nil

@test
def takeWhile08(): Bool = List.takeWhile(i -> i > 3, 4 :: 6 :: -3 :: 11 :: -5 :: 1 :: 2 :: 16 :: 7 :: 1 :: 7 :: Nil) ==
                        4 :: 6 :: Nil

/////////////////////////////////////////////////////////////////////////////
// groupBy                                                                 //
/////////////////////////////////////////////////////////////////////////////
@test
def groupBy01(): Bool = List.groupBy((a, b) -> a > 3 or b > 8, Nil) == Nil

@test
def groupBy02(): Bool = List.groupBy((a, b) -> a > 3 or b > 8, 1 :: Nil) == (1 :: Nil) :: Nil

@test
def groupBy03(): Bool = List.groupBy((a, b) -> a > 3 or b > 8, 1 :: 4 :: Nil) == (1 :: Nil) :: (4 :: Nil) :: Nil

@test
def groupBy04(): Bool = List.groupBy((a, b) -> a > 3 or b > 8, 1 :: 9 :: Nil) == (1 :: 9 :: Nil) :: Nil

@test
def groupBy05(): Bool = List.groupBy((a, b) -> a > 3 or b > 8, 1 :: 4 :: 7 :: 6 :: 9 :: 2 :: 4 :: 4 :: 8 :: 16 :: Nil) ==
                      (1 :: 9 :: 16 :: Nil) :: (4 :: 7 :: 6 :: 4 :: 4 :: 8 :: Nil) :: (2 :: Nil) :: Nil

@test
def groupBy06(): Bool = List.groupBy((a, b) -> a > -6 or a*b >= 0, -1 :: -11 :: 4 :: -11 :: 0 :: 8 :: 2 :: 1 :: -3 :: -24 :: Nil) ==
                      (-1 :: -11 :: -11 :: 0 :: -3 :: -24 :: Nil) :: (4 :: 8 :: 2 :: 1 :: Nil) :: Nil

@test
def groupBy07(): Bool = List.groupBy((a, b) -> a < 0 or (a > 10 or (b > 10 or a == b)), -5 :: 6 :: 11 :: 8 :: 8 :: -11 :: -1 :: 0 :: 4 :: -1 :: Nil) ==
                      (-5 :: 11 :: -11 :: -1 :: -1 :: Nil) :: (6 :: Nil) :: (8 :: 8 :: Nil) :: (0 :: Nil) :: (4 :: Nil) :: Nil

/////////////////////////////////////////////////////////////////////////////
// zip                                                                     //
/////////////////////////////////////////////////////////////////////////////
@test
def zip01(): Bool = List.zip(Nil, Nil) == Nil

@test
def zip02(): Bool = List.zip(1 :: Nil, Nil) == Nil

@test
def zip03(): Bool = List.zip(Nil, 2 :: Nil) == Nil

@test
def zip04(): Bool = List.zip(1 :: Nil, 2 :: Nil) == (1, 2) :: Nil

@test
def zip05(): Bool = List.zip(1 :: 3 :: Nil, 2 :: 4 :: Nil) == (1, 2) :: (3, 4) :: Nil

@test
def zip06(): Bool = List.zip(1 :: 3 :: 5 :: Nil, 2 :: 4 :: 6 :: Nil) == (1, 2) :: (3, 4) :: (5, 6) :: Nil

@test
def zip07(): Bool = List.zip(1 :: 3 :: 5 :: 7 :: Nil, 2 :: 4 :: 6 :: 8 :: Nil) == (1, 2) :: (3, 4) :: (5, 6) :: (7, 8) :: Nil

/////////////////////////////////////////////////////////////////////////////
// zipWith                                                                 //
/////////////////////////////////////////////////////////////////////////////
@test
def zipWith01(): Bool = List.zipWith((a, b) -> if (b) a+1 else a, Nil, Nil) == Nil

@test
def zipWith02(): Bool = List.zipWith((a, b) -> if (b) a+1 else a, 1 :: Nil, Nil) == Nil

@test
def zipWith03(): Bool = List.zipWith((a, b) -> if (b) a+1 else a, Nil, true :: Nil) == Nil

@test
def zipWith04(): Bool = List.zipWith((a, b) -> if (b) a+1 else a, 1 :: Nil, true :: Nil) == 2 :: Nil

@test
def zipWith05(): Bool = List.zipWith((a, b) -> if (b) a+1 else a, 1 :: Nil, false :: Nil) == 1 :: Nil

@test
def zipWith06(): Bool = List.zipWith((a, b) -> if (b) a+1 else a, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: Nil,
                      false :: true :: true :: false :: false :: true :: true :: true ::Nil) ==
                      1 :: 3 :: 4 :: 4 :: 5 :: 7 :: 8 :: 9 :: Nil

/////////////////////////////////////////////////////////////////////////////
// unzip                                                                   //
/////////////////////////////////////////////////////////////////////////////
@test
def unzip01(): Bool = List.unzip(Nil) == (Nil, Nil)

@test
def unzip02(): Bool = List.unzip((1, true) :: Nil) == (1 :: Nil, true :: Nil)

@test
def unzip03(): Bool = List.unzip((1, true) :: (2, true) :: Nil) == (1 :: 2 :: Nil, true :: true :: Nil)

@test
def unzip04(): Bool = List.unzip((1, true) :: (2, true) :: (3, false) :: Nil) ==
                    (1 :: 2 :: 3 :: Nil, true :: true :: false :: Nil)

/////////////////////////////////////////////////////////////////////////////
// map2                                                                    //
/////////////////////////////////////////////////////////////////////////////
@test
def map201(): Bool = List.map2((a, b) -> if (b) a+1 else a, Nil, Nil) == Nil

@test
def map202(): Bool = List.map2((a, b) -> if (b) a+1 else a, 1 :: Nil, Nil) == Nil

@test
def map203(): Bool = List.map2((a, b) -> if (b) a+1 else a, Nil, true :: Nil) == Nil

@test
def map204(): Bool = List.map2((a, b) -> if (b) a+1 else a, 1 :: Nil, true :: Nil) == 2 :: Nil

@test
def map205(): Bool = List.map2((a, b) -> if (b) a+1 else a, 1 :: Nil, false :: Nil) == 1 :: Nil

@test
def map206(): Bool = List.map2((a, b) -> if (b) a+1 else a, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: Nil,
                   false :: true :: true :: false :: false :: true :: true :: true ::Nil) ==
                   1 :: 3 :: 4 :: 4 :: 5 :: 7 :: 8 :: 9 :: Nil

/////////////////////////////////////////////////////////////////////////////
// flatMap2                                                                //
/////////////////////////////////////////////////////////////////////////////
@test
def flatMap201(): Bool = List.flatMap2((a, b) -> if (b) List.repeat(a, a) else List.repeat(a+1, a+1), Nil, Nil) == Nil

@test
def flatMap202(): Bool = List.flatMap2((a, b) -> if (b) List.repeat(a, a) else List.repeat(a+1, a+1), 1 :: Nil, Nil) == Nil

@test
def flatMap203(): Bool = List.flatMap2((a, b) -> if (b) List.repeat(a, a) else List.repeat(a+1, a+1), Nil, true :: Nil) == Nil

@test
def flatMap204(): Bool = List.flatMap2((a, b) -> if (b) List.repeat(a, a) else List.repeat(a+1, a+1), 2 :: Nil, true :: Nil) == 2 :: 2 :: Nil

@test
def flatMap205(): Bool = List.flatMap2((a, b) -> if (b) List.repeat(a, a) else List.repeat(a+1, a+1), 2 :: Nil, false :: Nil) == 3 :: 3 :: 3 :: Nil

@test
def flatMap206(): Bool = List.flatMap2((a, b) -> if (b) List.repeat(a, a) else List.repeat(a+1, a+1), 1 :: 2 :: 3 :: 4 :: 5 :: Nil,
                   false :: true :: true :: false :: false :: Nil) ==
                   2 :: 2 :: 2 :: 2 :: 3 :: 3 :: 3 :: 5 :: 5 :: 5 :: 5 :: 5 :: 6 :: 6 :: 6 :: 6 :: 6 :: 6 :: Nil

/////////////////////////////////////////////////////////////////////////////
// fold2                                                                   //
/////////////////////////////////////////////////////////////////////////////
@test
def fold201(): Bool = List.fold2((c, a, b) -> if (b) a+c else a*c, 4, Nil, Nil) == 4

@test
def fold202(): Bool = List.fold2((c, a, b) -> if (b) a+c else a*c, 4, 1 :: Nil, Nil) == 4

@test
def fold203(): Bool = List.fold2((c, a, b) -> if (b) a+c else a*c, 4, Nil, true :: Nil) == 4

@test
def fold204(): Bool = List.fold2((c, a, b) -> if (b) a+c else a*c, 4, 2 :: Nil, true :: Nil) == 6

@test
def fold205(): Bool = List.fold2((c, a, b) -> if (b) a+c else a*c, 4, 2 :: Nil, false :: Nil) == 8

@test
def fold206(): Bool = List.fold2((c, a, b) -> if (b) a+c else a*c, 4, 3 :: 2 :: Nil, true :: true :: Nil) == 9

@test
def fold207(): Bool = List.fold2((c, a, b) -> if (b) a+c else a*c, 4, 3 :: 2 :: Nil, true :: false :: Nil) == 14

@test
def fold208(): Bool = List.fold2((c, a, b) -> if (b) a+c else a*c, 4, 3 :: 2 :: Nil, false :: true :: Nil) == 14

@test
def fold209(): Bool = List.fold2((c, a, b) -> if (b) a+c else a*c, 4, 3 :: 2 :: Nil, false :: false :: Nil) == 24

@test
def fold210(): Bool = List.fold2((c, a, b) -> if (b) a+c else a*c, 4, 6 :: -4 :: 3 :: 2 :: Nil,
                    true :: false :: false :: true :: Nil) == -118

/////////////////////////////////////////////////////////////////////////////
// foldLeft2                                                               //
/////////////////////////////////////////////////////////////////////////////
@test
def foldLeft201(): Bool = List.foldLeft2((c, a, b) -> if (b) a+c else a*c, 4, Nil, Nil) == 4

@test
def foldLeft202(): Bool = List.foldLeft2((c, a, b) -> if (b) a+c else a*c, 4, 1 :: Nil, Nil) == 4

@test
def foldLeft203(): Bool = List.foldLeft2((c, a, b) -> if (b) a+c else a*c, 4, Nil, true :: Nil) == 4

@test
def foldLeft204(): Bool = List.foldLeft2((c, a, b) -> if (b) a+c else a*c, 4, 2 :: Nil, true :: Nil) == 6

@test
def foldLeft205(): Bool = List.foldLeft2((c, a, b) -> if (b) a+c else a*c, 4, 2 :: Nil, false :: Nil) == 8

@test
def foldLeft206(): Bool = List.foldLeft2((c, a, b) -> if (b) a+c else a*c, 4, 3 :: 2 :: Nil, true :: true :: Nil) == 9

@test
def foldLeft207(): Bool = List.foldLeft2((c, a, b) -> if (b) a+c else a*c, 4, 3 :: 2 :: Nil, true :: false :: Nil) == 14

@test
def foldLeft208(): Bool = List.foldLeft2((c, a, b) -> if (b) a+c else a*c, 4, 3 :: 2 :: Nil, false :: true :: Nil) == 14

@test
def foldLeft209(): Bool = List.foldLeft2((c, a, b) -> if (b) a+c else a*c, 4, 3 :: 2 :: Nil, false :: false :: Nil) == 24

@test
def foldLeft210(): Bool = List.foldLeft2((c, a, b) -> if (b) a+c else a*c, 4, 6 :: -4 :: 3 :: 2 :: Nil,
                        true :: false :: false :: true :: Nil) == -118

/////////////////////////////////////////////////////////////////////////////
// foldRight2                                                              //
/////////////////////////////////////////////////////////////////////////////
@test
def foldRight201(): Bool = List.foldRight2((a, b, c) -> if (b) a+c else a*c, 4, Nil, Nil) == 4

@test
def foldRight202(): Bool = List.foldRight2((a, b, c) -> if (b) a+c else a*c, 4, 1 :: Nil, Nil) == 4

@test
def foldRight203(): Bool = List.foldRight2((a, b, c) -> if (b) a+c else a*c, 4, Nil, true :: Nil) == 4

@test
def foldRight204(): Bool = List.foldRight2((a, b, c) -> if (b) a+c else a*c, 4, 2 :: Nil, true :: Nil) == 6

@test
def foldRight205(): Bool = List.foldRight2((a, b, c) -> if (b) a+c else a*c, 4, 2 :: Nil, false :: Nil) == 8

@test
def foldRight206(): Bool = List.foldRight2((a, b, c) -> if (b) a+c else a*c, 4, 3 :: 2 :: Nil, true :: true :: Nil) == 9

@test
def foldRight207(): Bool = List.foldRight2((a, b, c) -> if (b) a+c else a*c, 4, 3 :: 2 :: Nil, true :: false :: Nil) == 11

@test
def foldRight208(): Bool = List.foldRight2((a, b, c) -> if (b) a+c else a*c, 4, 3 :: 2 :: Nil, false :: true :: Nil) == 18

@test
def foldRight209(): Bool = List.foldRight2((a, b, c) -> if (b) a+c else a*c, 4, 3 :: 2 :: Nil, false :: false :: Nil) == 24

@test
def foldRight210(): Bool = List.foldRight2((a, b, c) -> if (b) a+c else a*c, 4, 6 :: -4 :: 3 :: 2 :: Nil,
                         true :: false :: false :: true :: Nil) == -66

/////////////////////////////////////////////////////////////////////////////
// filterMap                                                               //
/////////////////////////////////////////////////////////////////////////////
@test
def filterMap01(): Bool = List.filterMap(i -> if (i % 2 == 0) Some(i/2) else None, Nil) == Nil

@test
def filterMap02(): Bool = List.filterMap(i -> if (i % 2 == 0) Some(i/2) else None, 1 :: Nil) == Nil

@test
def filterMap03(): Bool = List.filterMap(i -> if (i % 2 == 0) Some(i/2) else None, 2 :: Nil) == 1 :: Nil

@test
def filterMap04(): Bool = List.filterMap(i -> if (i % 2 == 0) Some(i/2) else None, 1 :: 3 :: Nil) == Nil

@test
def filterMap05(): Bool = List.filterMap(i -> if (i % 2 == 0) Some(i/2) else None, 1 :: 4 :: Nil) == 2 :: Nil

@test
def filterMap06(): Bool = List.filterMap(i -> if (i % 2 == 0) Some(i/2) else None, 6 :: -1 :: Nil) == 3 :: Nil

@test
def filterMap07(): Bool = List.filterMap(i -> if (i % 2 == 0) Some(i/2) else None, 8 :: 6 :: Nil) == 4 :: 3 :: Nil

@test
def filterMap08(): Bool = List.filterMap(i -> if (i % 2 == 0) Some(i/2) else None, 0 :: 1 :: 2 :: 3 :: 4 :: 5 :: 10 :: Nil) ==
                        0 :: 1 :: 2 :: 5 :: Nil

/////////////////////////////////////////////////////////////////////////////
// findMap                                                                 //
/////////////////////////////////////////////////////////////////////////////
@test
def findMap01(): Bool = List.findMap(i -> if (i % 2 == 0) Some(i/2) else None, Nil) == None

@test
def findMap02(): Bool = List.findMap(i -> if (i % 2 == 0) Some(i/2) else None, 1 :: Nil) == None

@test
def findMap03(): Bool = List.findMap(i -> if (i % 2 == 0) Some(i/2) else None, 2 :: Nil) == Some(1)

@test
def findMap04(): Bool = List.findMap(i -> if (i % 2 == 0) Some(i/2) else None, 1 :: 3 :: Nil) == None

@test
def findMap05(): Bool = List.findMap(i -> if (i % 2 == 0) Some(i/2) else None, 1 :: 4 :: Nil) == Some(2)

@test
def findMap06(): Bool = List.findMap(i -> if (i % 2 == 0) Some(i/2) else None, 6 :: -1 :: Nil) == Some(3)

@test
def findMap07(): Bool = List.findMap(i -> if (i % 2 == 0) Some(i/2) else None, 8 :: 6 :: Nil) == Some(4)

@test
def findMap08(): Bool = List.findMap(i -> if (i % 2 == 0) Some(i/2) else None, 0 :: 1 :: 2 :: 3 :: 4 :: 5 :: 10 :: Nil) == Some(0)

/////////////////////////////////////////////////////////////////////////////
// toSet                                                                   //
/////////////////////////////////////////////////////////////////////////////
@test
def toSet01(): Bool = List.toSet(Nil) == Set#{}

@test
def toSet02(): Bool = List.toSet(1 :: Nil) == Set#{1}

@test
def toSet03(): Bool = List.toSet(1 :: 2 :: Nil) == Set#{1, 2}

@test
def toSet04(): Bool = List.toSet(1 :: 1 :: Nil) == Set#{1}

@test
def toSet05(): Bool = List.toSet(1 :: 2 :: 3 :: Nil) == Set#{1, 2, 3}

@test
def toSet06(): Bool = List.toSet(1 :: 2 :: 1 :: Nil) == Set#{1, 2}

@test
def toSet07(): Bool = List.toSet(1 :: 1 :: 2 :: Nil) == Set#{1, 2}

@test
def toSet08(): Bool = List.toSet(2 :: 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 3 :: 3 :: Nil) == Set#{1, 2, 3, 4, 5, 6}

/////////////////////////////////////////////////////////////////////////////
// toMap                                                                   //
/////////////////////////////////////////////////////////////////////////////
@test
def toMap01(): Bool = List.toMap(Nil) == Map#{}

@test
def toMap02(): Bool = List.toMap((1, true) :: Nil) == Map#{1 -> true}

@test
def toMap03(): Bool = List.toMap((1, true) :: (2, false) :: Nil) == Map#{1 -> true, 2 -> false}

@test
def toMap04(): Bool = List.toMap((1, true) :: (1, false) :: Nil) == Map#{1 -> true}

/////////////////////////////////////////////////////////////////////////////
// foreach                                                                 //
/////////////////////////////////////////////////////////////////////////////

@test
def foreach01(): Bool & Impure =
    let r = ref 21;
    List.foreach(x -> r := x, Nil);
    21 == deref r

@test
def foreach02(): Bool & Impure =
    let r = ref 21;
    List.foreach(x -> r := x, 1 :: 2 :: 3 :: Nil);
    3 == deref r

/////////////////////////////////////////////////////////////////////////////
// toArray                                                                 //
/////////////////////////////////////////////////////////////////////////////
@test
def toArray01(): Bool & Impure =
    let xs: List[Int32] = Nil;
    let a: Array[Int32] = List.toArray(xs);
    Array.sameElements(a, []: Array[Int32])


@test
def toArray02(): Bool & Impure =
    let a = List.toArray(1 :: Nil);
    Array.sameElements(a, [1])

@test
def toArray03(): Bool & Impure =
    let a = List.toArray(1 :: 2 :: Nil);
    Array.sameElements(a, [1,2])

@test
def toArray04(): Bool & Impure =
    let a = List.toArray(1 :: 2 :: 3 :: Nil);
    Array.sameElements(a, [1,2,3])

/////////////////////////////////////////////////////////////////////////////
// sortWith                                                                //
/////////////////////////////////////////////////////////////////////////////

@test
def sortWith01(): Bool =
    List.sortWith((x,y) -> if (x < y) -1 else 1, Nil: List[Int32]) == Nil

@test
def sortWith02(): Bool =
    List.sortWith((x,y) -> if (x < y) -1 else 1, 0 :: Nil) == 0 :: Nil

@test
def sortWith03(): Bool =
    List.sortWith((x,y) -> if (x < y) -1 else 1, 0 :: 1 :: Nil) == 0 :: 1 :: Nil

@test
def sortWith04(): Bool =
    List.sortWith((x,y) -> if (x < y) -1 else 1, 1 :: 0 :: Nil) == 0 :: 1 :: Nil

@test
def sortWith05(): Bool =
    List.sortWith((x,y) -> if (x < y) -1 else 1, 1 :: 1 :: Nil) == 1 :: 1 :: Nil

@test
def sortWith06(): Bool =
    List.sortWith((x,y) -> if (x < y) -1 else 1, 0 :: 1 :: 2 :: 3 :: 4 :: 5 :: Nil) == 0 :: 1 :: 2 :: 3 :: 4 :: 5 :: Nil

@test
def sortWith07(): Bool =
    List.sortWith((x,y) -> if (x < y) -1 else 1, 5 :: 4 :: 3 :: 2 :: 1 :: 0 :: Nil) == 0 :: 1 :: 2 :: 3 :: 4 :: 5 :: Nil

@test
def sortWith08(): Bool =
    List.sortWith((x,y) -> if (x < y) -1 else 1, 5 :: 3 :: 0 :: 4 :: 1 :: 2 :: Nil) == 0 :: 1 :: 2 :: 3 :: 4 :: 5 :: Nil

@test
def sortWith09(): Bool =
    List.sortWith((x,y) -> if (x < y) -1 else 1, 2 :: 3 :: 0 :: 4 :: 1 :: 2 :: Nil) == 0 :: 1 :: 2 :: 2 :: 3 :: 4 :: Nil

@test
def sortWith10(): Bool =
    List.sortWith((x,y) -> if (x > y) -1 else 1, 0 :: 1 :: 2 :: 3 :: 4 :: 5 :: Nil) == 5 :: 4 :: 3 :: 2 :: 1 :: 0 :: Nil

@test
def sortWith11(): Bool =
    List.sortWith((x,y) -> if (x > y) -1 else 1, 5 :: 4 :: 3 :: 2 :: 1 :: 0 :: Nil) == 5 :: 4 :: 3 :: 2 :: 1 :: 0 :: Nil

@test
def sortWith12(): Bool =
    List.sortWith((x,y) -> if (x > y) -1 else 1, 5 :: 3 :: 0 :: 4 :: 1 :: 2 :: Nil) == 5 :: 4 :: 3 :: 2 :: 1 :: 0 :: Nil

@test
def sortWith13(): Bool =
    List.sortWith((x,y) -> if (x > y) -1 else 1, 2 :: 3 :: 0 :: 4 :: 1 :: 2 :: Nil) == 4 :: 3 :: 2 :: 2 :: 1 :: 0 :: Nil

/////////////////////////////////////////////////////////////////////////////
// unfold                                                                  //
/////////////////////////////////////////////////////////////////////////////

@test
def unfold01(): Bool =
    List.unfold(s -> if (true) None else Some(Char.fromInt32(s + 48), s + 1), 0) == Nil

@test
def unfold02(): Bool =
    List.unfold(s -> if (s > 0) None else Some(Char.fromInt32(s + 48), s + 1), 0) == '0' :: Nil

@test
def unfold03(): Bool =
    List.unfold(s -> if (s > 1) None else Some(Char.fromInt32(s + 48), s + 1), 0) == '0' :: '1' :: Nil

@test
def unfold04(): Bool =
    List.unfold(s -> if (s >= 10) None else Some(Char.fromInt32(s + 48), s + 1), 0) == '0' :: '1' :: '2' :: '3' :: '4' :: '5' :: '6' :: '7' :: '8' :: '9' :: Nil

@test
def unfold05(): Bool =
    List.unfold(s -> if (s >= 10) None else Some(Char.fromInt32(s + 48), s + 1), 5) == '5' :: '6' :: '7' :: '8' :: '9' :: Nil

@test
def unfold06(): Bool =
    List.unfold(s -> if (s >= 10) None else Some(Char.fromInt32(s + 48), s + 2), 0) == '0' :: '2' :: '4' :: '6' :: '8' :: Nil

/////////////////////////////////////////////////////////////////////////////
// unfoldWithIter                                                          //
/////////////////////////////////////////////////////////////////////////////

@test
def unfoldWithIter01(): Bool & Impure =
    let x = ref 0;
    let step = () ->
        if (true)
            None
        else {
            let c = Char.fromInt32(deref x + 48);
            x := deref x + 1;
            Some(c)
        };
    List.unfoldWithIter(step) == Nil

@test
def unfoldWithIter02(): Bool & Impure =
    let x = ref 0;
    let step = () ->
        if (deref x > 0)
            None
        else {
            let c = Char.fromInt32(deref x + 48);
            x := deref x + 1;
            Some(c)
        };
    List.unfoldWithIter(step) == '0' :: Nil

@test
def unfoldWithIter03(): Bool & Impure =
    let x = ref 0;
    let step = () ->
        if (deref x > 1)
            None
        else {
            let c = Char.fromInt32(deref x + 48);
            x := deref x + 1;
            Some(c)
        };
    List.unfoldWithIter(step) == '0' :: '1' :: Nil

@test
def unfoldWithIter04(): Bool & Impure =
    let x = ref 0;
    let step = () ->
        if (deref x >= 10)
            None
        else {
            let c = Char.fromInt32(deref x + 48);
            x := deref x + 1;
            Some(c)
        };
    List.unfoldWithIter(step) == '0' :: '1' :: '2' :: '3' :: '4' :: '5' :: '6' :: '7' :: '8' :: '9' :: Nil

@test
def unfoldWithIter05(): Bool & Impure =
    let x = ref 5;
    let step = () ->
        if (deref x >= 10)
            None
        else {
            let c = Char.fromInt32(deref x + 48);
            x := deref x + 1;
            Some(c)
        };
    List.unfoldWithIter(step) == '5' :: '6' :: '7' :: '8' :: '9' :: Nil

@test
def unfoldWithIter06(): Bool & Impure =
    let x = ref 0;
    let step = () ->
        if (deref x >= 10)
            None
        else {
            let c = Char.fromInt32(deref x + 48);
            x := deref x + 2;
            Some(c)
        };
    List.unfoldWithIter(step) == '0' :: '2' :: '4' :: '6' :: '8' :: Nil


/////////////////////////////////////////////////////////////////////////////
// unfoldWithOkIter                                                        //
/////////////////////////////////////////////////////////////////////////////

@test
def unfoldWithOkIter01(): Bool & Impure =
    let x = ref 0;
    let step = () ->
        if (true) {
            Ok(None)
        } else {
            Ok(Some(deref x))
        };
    List.unfoldWithOkIter(step) == Ok(Nil)

@test
def unfoldWithOkIter02(): Bool & Impure =
    let x = ref 0;
    let step = () ->
        if (true) {
            Err(deref x)
        } else {
            Ok(Some(deref x))
        };
    List.unfoldWithOkIter(step) == Err(0)

@test
def unfoldWithOkIter03(): Bool & Impure =
    let x = ref 1;
    let step = () ->
        if (deref x % 5 > 0) {
            let c = deref x;
            x := deref x + 1;
            Ok(Some(c))
        } else {
            Ok(None)
        };
    List.unfoldWithOkIter(step) == Ok(1 :: 2 :: 3 :: 4 :: Nil)

/////////////////////////////////////////////////////////////////////////////
// distinct                                                                //
/////////////////////////////////////////////////////////////////////////////

@test
def distinct01(): Bool =
    List.distinct(Nil) == Nil

@test
def distinct02(): Bool =
    List.distinct(1 :: Nil) == 1 :: Nil

@test
def distinct03(): Bool =
    List.distinct(1 :: 2 :: Nil) == 1 :: 2 :: Nil

@test
def distinct04(): Bool =
    List.distinct(1 :: 1 :: 2 :: Nil) == 1 :: 2 :: Nil

@test
def distinct05(): Bool =
    List.distinct(1 :: 2 :: 1 :: Nil) == 1 :: 2 :: Nil

@test
def distinct06(): Bool =
    List.distinct(1 :: 2 :: 2 :: Nil) == 1 :: 2 :: Nil

@test
def distinct07(): Bool =
    List.distinct(1 :: 2 :: 2 :: 3 :: Nil) == 1 :: 2 :: 3 :: Nil

@test
def distinct08(): Bool =
    List.distinct(1 :: 2 :: 2 :: 3 :: 1 :: 4 :: Nil) == 1 :: 2 :: 3 :: 4 :: Nil

@test
def distinct09(): Bool =
    List.distinct(4 :: 4 :: 3  :: 4 :: 2 :: 1 :: 1 :: Nil) == 4 :: 3 :: 2 :: 1 :: Nil

/////////////////////////////////////////////////////////////////////////////
// distinctWith                                                            //
/////////////////////////////////////////////////////////////////////////////

@test
def distinctWith01(): Bool =
    List.distinctWith((x,y) -> x == y, Nil) == Nil

@test
def distinctWith02(): Bool =
    List.distinctWith((x,y) -> x == y, 1 :: Nil) == 1 :: Nil

@test
def distinctWith03(): Bool =
    List.distinctWith((x,y) -> x == y, 1 :: 2 :: Nil) == 1 :: 2 :: Nil

@test
def distinctWith04(): Bool =
    List.distinctWith((x,y) -> x == y, 1 :: 1 :: 2 :: Nil) == 1 :: 2 :: Nil

@test
def distinctWith05(): Bool =
    List.distinctWith((x,y) -> x == y, 1 :: 2 :: 1 :: Nil) == 1 :: 2 :: Nil

@test
def distinctWith06(): Bool =
    List.distinctWith((x,y) -> x == y, 1 :: 2 :: 2 :: Nil) == 1 :: 2 :: Nil

@test
def distinctWith07(): Bool =
    List.distinctWith((x,y) -> x == y, 1 :: 2 :: 2 :: 3 :: Nil) == 1 :: 2 :: 3 :: Nil

@test
def distinctWith08(): Bool =
    List.distinctWith((x,y) -> x == y, 1 :: 2 :: 2 :: 3 :: 1 :: 4 :: Nil) == 1 :: 2 :: 3 :: 4 :: Nil

@test
def distinctWith09(): Bool =
    List.distinctWith((x,y) -> x == y, 4 :: 4 :: 3  :: 4 :: 2 :: 1 :: 1 :: Nil) == 4 :: 3 :: 2 :: 1 :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // toString                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toString01(): Bool =
        toString(1 :: Nil) == "1 :: Nil"

    @test
    def toString02(): Bool =
        toString(1 :: 2 :: Nil) == "1 :: 2 :: Nil"

    @test
    def toString03(): Bool =
        toString(93 :: 3 :: 4 :: Nil) == "93 :: 3 :: 4 :: Nil"

    @test
    def toString04(): Bool =
        toString('a' :: 'b' :: 'c' :: Nil) == "a :: b :: c :: Nil"

    @test
    def toString05(): Bool =
        toString(true :: false :: true :: true :: Nil) == "true :: false :: true :: true :: Nil"

    @test
    def toString06(): Bool =
        toString((1 :: 2 :: Nil) :: (2 :: 3 :: Nil) :: (4 :: 7 :: Nil) :: Nil) == "1 :: 2 :: Nil :: 2 :: 3 :: Nil :: 4 :: 7 :: Nil :: Nil"

}
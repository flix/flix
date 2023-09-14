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

mod TestList {

    use ToString.toString
    use Hash.hash

/////////////////////////////////////////////////////////////////////////////
// Collectable.collect                                                     //
/////////////////////////////////////////////////////////////////////////////
@Test
def collect01(): Bool = region rc {
    Collectable.collect(Iterator.empty(rc): Iterator[Int32, rc, rc]) == Nil
}

@Test
def collect02(): Bool = region rc {
    Collectable.collect(Iterator.singleton(rc, 1)) == 1 :: Nil
}

@Test
def collect03(): Bool = region rc {
    Collectable.collect(Iterator.range(rc, 1, 3)) == 1 :: 2 :: Nil
}

/////////////////////////////////////////////////////////////////////////////
// isEmpty                                                                 //
/////////////////////////////////////////////////////////////////////////////
@Test
def isEmpty01(): Bool = List.isEmpty(Nil) == true

@Test
def isEmpty02(): Bool = List.isEmpty(1 :: Nil) == false

@Test
def isEmpty03(): Bool = List.isEmpty(1 :: 2 :: Nil) == false

/////////////////////////////////////////////////////////////////////////////
// head                                                                    //
/////////////////////////////////////////////////////////////////////////////
@Test
def head01(): Bool = List.head(Nil: List[Unit]) == None

@Test
def head02(): Bool = List.head(1 :: Nil) == Some(1)

@Test
def head03(): Bool = List.head(2 :: 1 :: Nil) == Some(2)

@Test
def head04(): Bool = List.head(3 :: 2 :: 1 :: Nil) == Some(3)

/////////////////////////////////////////////////////////////////////////////
// last                                                                    //
/////////////////////////////////////////////////////////////////////////////
@Test
def last01(): Bool = List.last(Nil: List[Unit]) == None

@Test
def last02(): Bool = List.last(1 :: Nil) == Some(1)

@Test
def last03(): Bool = List.last(1 :: 2 :: Nil) == Some(2)

@Test
def last04(): Bool = List.last(1 :: 2 :: 3 :: Nil) == Some(3)

/////////////////////////////////////////////////////////////////////////////
// length                                                                  //
/////////////////////////////////////////////////////////////////////////////
@Test
def length01(): Bool = List.length(Nil) == 0

@Test
def length02(): Bool = List.length(1 :: Nil) == 1

@Test
def length03(): Bool = List.length(1 :: 2 :: Nil) == 2

@Test
def length04(): Bool = List.length(1 :: 2 :: 3 :: Nil) == 3

/////////////////////////////////////////////////////////////////////////////
// append                                                                  //
/////////////////////////////////////////////////////////////////////////////
@Test
def append01(): Bool = List.append(Nil: List[Unit], Nil: List[Unit]) == Nil

@Test
def append02(): Bool = List.append(Nil, 1 :: Nil) == 1 :: Nil

@Test
def append03(): Bool = List.append(Nil, 1 :: 2 :: Nil) == 1 :: 2 :: Nil

@Test
def append04(): Bool = List.append(1 :: Nil, Nil) == 1 :: Nil

@Test
def append05(): Bool = List.append(1 :: 2 :: Nil, Nil) == 1 :: 2 :: Nil

@Test
def append06(): Bool = List.append(1 :: Nil, 2 :: Nil) == 1 :: 2 :: Nil

@Test
def append07(): Bool = List.append(1 :: 2 :: Nil, 3 :: Nil) == 1 :: 2 :: 3 :: Nil

@Test
def append08(): Bool = List.append(1 :: Nil, 2 :: 3 :: Nil) == 1 :: 2 :: 3 :: Nil

/////////////////////////////////////////////////////////////////////////////
// memberOf                                                                //
/////////////////////////////////////////////////////////////////////////////
@Test
def memberOf01(): Bool = List.memberOf(0, Nil) == false

@Test
def memberOf02(): Bool = List.memberOf(0, 1 :: Nil) == false

@Test
def memberOf03(): Bool = List.memberOf(0, 0 :: Nil) == true

@Test
def memberOf04(): Bool = List.memberOf(0, 1 :: 2 :: Nil) == false

@Test
def memberOf05(): Bool = List.memberOf(1, 1 :: 2 :: Nil) == true

@Test
def memberOf06(): Bool = List.memberOf(2, 1 :: 2 :: Nil) == true

@Test
def memberOf07(): Bool = List.memberOf(3, 1 :: 2 :: Nil) == false

/////////////////////////////////////////////////////////////////////////////
// indexOf                                                                 //
/////////////////////////////////////////////////////////////////////////////
@Test
def indexOf01(): Bool = List.indexOf(0, Nil) == None

@Test
def indexOf02(): Bool = List.indexOf(0, 1 :: Nil) == None

@Test
def indexOf03(): Bool = List.indexOf(1, 1 :: Nil) == Some(0)

@Test
def indexOf04(): Bool = List.indexOf(0, 1 :: 2 :: Nil) == None

@Test
def indexOf05(): Bool = List.indexOf(1, 1 :: 2 :: Nil) == Some(0)

@Test
def indexOf06(): Bool = List.indexOf(2, 1 :: 2 :: Nil) == Some(1)

/////////////////////////////////////////////////////////////////////////////
// find                                                                    //
/////////////////////////////////////////////////////////////////////////////
@Test
def find01(): Bool = List.find(i -> i > 2, Nil) == None

@Test
def find02(): Bool = List.find(i -> i > 2, 1 :: Nil) == None

@Test
def find03(): Bool = List.find(i -> i > 2, 3 :: Nil) == Some(3)

@Test
def find04(): Bool = List.find(i -> i > 2, 1 :: 2 :: Nil) == None

@Test
def find05(): Bool = List.find(i -> i > 2, 6 :: -6 :: Nil) == Some(6)

@Test
def find06(): Bool = List.find(i -> i > 2, -6 :: 6 :: Nil) == Some(6)

@Test
def find07(): Bool = List.find(i -> i > 2, 6 :: 7 :: Nil) == Some(6)

/////////////////////////////////////////////////////////////////////////////
// findLeft                                                                //
/////////////////////////////////////////////////////////////////////////////
@Test
def findLeft01(): Bool = List.findLeft(i -> i > 2, Nil) == None

@Test
def findLeft02(): Bool = List.findLeft(i -> i > 2, 1 :: Nil) == None

@Test
def findLeft03(): Bool = List.findLeft(i -> i > 2, 3 :: Nil) == Some(3)

@Test
def findLeft04(): Bool = List.findLeft(i -> i > 2, 1 :: 2 :: Nil) == None

@Test
def findLeft05(): Bool = List.findLeft(i -> i > 2, 6 :: -6 :: Nil) == Some(6)

@Test
def findLeft06(): Bool = List.findLeft(i -> i > 2, -6 :: 6 :: Nil) == Some(6)

@Test
def findLeft07(): Bool = List.findLeft(i -> i > 2, 6 :: 7 :: Nil) == Some(6)

/////////////////////////////////////////////////////////////////////////////
// findRight                                                               //
/////////////////////////////////////////////////////////////////////////////
@Test
def findRight01(): Bool = List.findRight(i -> i > 2, Nil) == None

@Test
def findRight02(): Bool = List.findRight(i -> i > 2, 1 :: Nil) == None

@Test
def findRight03(): Bool = List.findRight(i -> i > 2, 3 :: Nil) == Some(3)

@Test
def findRight04(): Bool = List.findRight(i -> i > 2, 1 :: 2 :: Nil) == None

@Test
def findRight05(): Bool = List.findRight(i -> i > 2, 6 :: -6 :: Nil) == Some(6)

@Test
def findRight06(): Bool = List.findRight(i -> i > 2, -6 :: 6 :: Nil) == Some(6)

@Test
def findRight07(): Bool = List.findRight(i -> i > 2, 6 :: 7 :: Nil) == Some(7)

/////////////////////////////////////////////////////////////////////////////
// range                                                                   //
/////////////////////////////////////////////////////////////////////////////
@Test
def range01(): Bool = List.range(1, 0) == Nil

@Test
def range02(): Bool = List.range(1, 1) == Nil

@Test
def range03(): Bool = List.range(1, 2) == 1 :: Nil

@Test
def range04(): Bool = List.range(1, 3) == 1 :: 2 :: Nil

@Test
def range05(): Bool = List.range(1, 4) == 1 :: 2 :: 3 :: Nil

/////////////////////////////////////////////////////////////////////////////
// repeat                                                                  //
/////////////////////////////////////////////////////////////////////////////
@Test
def repeat01(): Bool = List.repeat(-1, 1) == Nil

@Test
def repeat02(): Bool = List.repeat(0, 1) == Nil

@Test
def repeat03(): Bool = List.repeat(1, 1) == 1 :: Nil

@Test
def repeat04(): Bool = List.repeat(2, 1) == 1 :: 1 :: Nil

@Test
def repeat05(): Bool = List.repeat(3, 1) == 1 :: 1 :: 1 :: Nil

/////////////////////////////////////////////////////////////////////////////
// scan                                                                    //
/////////////////////////////////////////////////////////////////////////////
@Test
def scan01(): Bool = List.scan((i, b) -> if (b) i+1 else i+2, 1, Nil) == 1 :: Nil

@Test
def scan02(): Bool = List.scan((i, b) -> if (b) i+1 else i+2, 1, false :: Nil) == 1 :: 3 :: Nil

@Test
def scan03(): Bool = List.scan((i, b) -> if (b) i+1 else i+2, 1, true :: Nil) == 1 :: 2 :: Nil

@Test
def scan04(): Bool = List.scan((i, b) -> if (b) i+1 else i+2, 1, false :: false :: Nil) == 1 :: 3 :: 5 :: Nil

@Test
def scan05(): Bool = List.scan((i, b) -> if (b) i+1 else i+2, 1, false :: true :: Nil) == 1 :: 3 :: 4 :: Nil

@Test
def scan06(): Bool = List.scan((i, b) -> if (b) i+1 else i+2, 1, true :: false :: Nil) == 1 :: 2 :: 4 :: Nil

@Test
def scan07(): Bool = List.scan((i, b) -> if (b) i+1 else i+2, 1, true :: true :: Nil) == 1 :: 2 :: 3 :: Nil

/////////////////////////////////////////////////////////////////////////////
// scanLeft                                                                //
/////////////////////////////////////////////////////////////////////////////
@Test
def scanLeft01(): Bool = List.scanLeft((i, b) -> if (b) i+1 else i+2, 1, Nil) == 1 :: Nil

@Test
def scanLeft02(): Bool = List.scanLeft((i, b) -> if (b) i+1 else i+2, 1, false :: Nil) == 1 :: 3 :: Nil

@Test
def scanLeft03(): Bool = List.scanLeft((i, b) -> if (b) i+1 else i+2, 1, true :: Nil) == 1 :: 2 :: Nil

@Test
def scanLeft04(): Bool = List.scanLeft((i, b) -> if (b) i+1 else i+2, 1, false :: false :: Nil) == 1 :: 3 :: 5 :: Nil

@Test
def scanLeft05(): Bool = List.scanLeft((i, b) -> if (b) i+1 else i+2, 1, false :: true :: Nil) == 1 :: 3 :: 4 :: Nil

@Test
def scanLeft06(): Bool = List.scanLeft((i, b) -> if (b) i+1 else i+2, 1, true :: false :: Nil) == 1 :: 2 :: 4 :: Nil

@Test
def scanLeft07(): Bool = List.scanLeft((i, b) -> if (b) i+1 else i+2, 1, true :: true :: Nil) == 1 :: 2 :: 3 :: Nil

/////////////////////////////////////////////////////////////////////////////
// scanRight                                                               //
/////////////////////////////////////////////////////////////////////////////
@Test
def scanRight01(): Bool = List.scanRight((b, i) -> if (b) i+1 else i+2, 1, Nil) == 1 :: Nil

@Test
def scanRight02(): Bool = List.scanRight((b, i) -> if (b) i+1 else i+2, 1, false :: Nil) == 3 :: 1 :: Nil

@Test
def scanRight03(): Bool = List.scanRight((b, i) -> if (b) i+1 else i+2, 1, true :: Nil) == 2 :: 1 :: Nil

@Test
def scanRight04(): Bool = List.scanRight((b, i) -> if (b) i+1 else i+2, 1, false :: false :: Nil) == 5 :: 3 :: 1 :: Nil

@Test
def scanRight05(): Bool = List.scanRight((b, i) -> if (b) i+1 else i+2, 1, false :: true :: Nil) == 4 :: 2 :: 1 :: Nil

@Test
def scanRight06(): Bool = List.scanRight((b, i) -> if (b) i+1 else i+2, 1, true :: false :: Nil) == 4 :: 3 :: 1 :: Nil

@Test
def scanRight07(): Bool = List.scanRight((b, i) -> if (b) i+1 else i+2, 1, true :: true :: Nil) == 3 :: 2 :: 1 :: Nil

/////////////////////////////////////////////////////////////////////////////
// map                                                                     //
/////////////////////////////////////////////////////////////////////////////
@Test
def map01(): Bool = List.map(i -> i > 2, Nil) == Nil

@Test
def map02(): Bool = List.map(i -> i > 2, 1 :: Nil) == false :: Nil

@Test
def map03(): Bool = List.map(i -> i > 2, 3 :: Nil) == true :: Nil

@Test
def map04(): Bool = List.map(i -> i > 2, 1 :: 2 :: Nil) == false :: false :: Nil

@Test
def map05(): Bool = List.map(i -> i > 2, 1 :: 8 :: Nil) == false :: true :: Nil

@Test
def map06(): Bool = List.map(i -> i > 2, 8 :: 1 :: Nil) == true :: false :: Nil

@Test
def map07(): Bool = List.map(i -> i > 2, 7 :: 8 :: Nil) == true :: true :: Nil

/////////////////////////////////////////////////////////////////////////////
// point                                                                   //
/////////////////////////////////////////////////////////////////////////////
@Test
def point01(): Bool = List.point(0) == 0 :: Nil

@Test
def point02(): Bool = List.point(123) == 123 :: Nil

@Test
def point03(): Bool = List.point(true) == true :: Nil

@Test
def point04(): Bool = List.point(Some(0)) == Some(0) :: Nil

@Test
def point05(): Bool = List.point(Nil : List[Bool]) == Nil :: Nil

@Test
def point06(): Bool = List.point(42::Nil) == (42 :: Nil) :: Nil

@Test
def point07(): Bool = List.point(List.point(1)) == (1:: Nil) :: Nil

/////////////////////////////////////////////////////////////////////////////
// ap                                                                      //
/////////////////////////////////////////////////////////////////////////////
@Test
def ap01(): Bool = List.ap(Nil, Nil) == Nil : List[Int32]

@Test
def ap02(): Bool = List.ap((x -> x+1) :: Nil, Nil) == Nil

@Test
def ap03(): Bool = List.ap(Nil, 5 :: Nil) == Nil : List[Int32]

@Test
def ap04(): Bool = List.ap((x -> x+1) :: Nil, 5 :: Nil) == 6 :: Nil

@Test
def ap05(): Bool = List.ap((x -> x+1) :: Nil, 0 :: 5 :: Nil) == 1 :: 6 :: Nil

@Test
def ap06(): Bool = List.ap((x -> x+1) :: (x -> x*2) :: Nil, 0 :: 4 :: Nil) == 1 :: 5 :: 0 :: 8 :: Nil

/////////////////////////////////////////////////////////////////////////////
// map2                                                                    //
/////////////////////////////////////////////////////////////////////////////

@Test
def map201(): Bool = List.map2((x,y) -> x+y, Nil, Nil) == Nil : List[Int32]

@Test
def map202(): Bool = List.map2((x,y) -> x+y, 10 :: Nil, Nil) == Nil

@Test
def map203(): Bool = List.map2((x,y) -> x+y, Nil, 1 :: Nil) == Nil

@Test
def map204(): Bool = List.map2((x,y) -> x+y, 10 :: Nil, 1 :: Nil) == 11 :: Nil

@Test
def map205(): Bool = List.map2((x,y) -> x+y, 10 :: 20 :: Nil, 1 :: Nil) == 11 :: 21 :: Nil

@Test
def map206(): Bool = List.map2((x,y) -> x+y, 10 :: 20 :: Nil, 1 :: 2 :: Nil) == 11 :: 12 :: 21 :: 22 :: Nil

@Test
def map207(): Bool = List.map2((x,y) -> (x+y) :: Nil, 10 :: Nil, 1 :: 2 :: Nil) == (11 :: Nil) :: (12 :: Nil) :: Nil

/////////////////////////////////////////////////////////////////////////////
// map3                                                                    //
/////////////////////////////////////////////////////////////////////////////

@Test
def map301(): Bool = List.map3((x,y,z) -> (x,y,z), Nil, Nil, Nil) == Nil : List[(Int32, String, String)]

@Test
def map302(): Bool = List.map3((x,y,z) -> (x,y,z), 1 :: 2 :: 3 :: Nil, "a" :: "b" :: "c" :: Nil, "A" :: "B" :: "C" :: Nil) == (1, "a", "A") :: (1, "a", "B") :: (1, "a", "C") :: (1, "b", "A") :: (1, "b", "B") :: (1, "b", "C") :: (1, "c", "A") :: (1, "c", "B") :: (1, "c", "C") :: (2, "a", "A") :: (2, "a", "B") :: (2, "a", "C") :: (2, "b", "A") :: (2, "b", "B") :: (2, "b", "C") :: (2, "c", "A") :: (2, "c", "B") :: (2, "c", "C") :: (3, "a", "A") :: (3, "a", "B") :: (3, "a", "C") :: (3, "b", "A") :: (3, "b", "B") :: (3, "b", "C") :: (3, "c", "A") :: (3, "c", "B") :: (3, "c", "C") :: Nil


/////////////////////////////////////////////////////////////////////////////
// mapWithIndex                                                            //
/////////////////////////////////////////////////////////////////////////////
@Test
def mapWithIndex01(): Bool = List.mapWithIndex((i, e) -> if (i < 1) e > 2 else e <= 2, Nil) == Nil

@Test
def mapWithIndex02(): Bool = List.mapWithIndex((i, e) -> if (i < 1) e > 2 else e <= 2, 1 :: Nil) == false :: Nil

@Test
def mapWithIndex03(): Bool = List.mapWithIndex((i, e) -> if (i < 1) e > 2 else e <= 2, 3 :: Nil) == true :: Nil

@Test
def mapWithIndex04(): Bool = List.mapWithIndex((i, e) -> if (i < 1) e > 2 else e <= 2, 1 :: 2 :: Nil) == false :: true :: Nil

@Test
def mapWithIndex05(): Bool = List.mapWithIndex((i, e) -> if (i < 1) e > 2 else e <= 2, 1 :: 8 :: Nil) == false :: false :: Nil

@Test
def mapWithIndex06(): Bool = List.mapWithIndex((i, e) -> if (i < 1) e > 2 else e <= 2, 8 :: 1 :: Nil) == true :: true :: Nil

@Test
def mapWithIndex07(): Bool = List.mapWithIndex((i, e) -> if (i < 1) e > 2 else e <= 2, 7 :: 8 :: Nil) == true :: false :: Nil

/////////////////////////////////////////////////////////////////////////////
// flatMap                                                                 //
/////////////////////////////////////////////////////////////////////////////
@Test
def flatMap01(): Bool = List.flatMap(i -> List.repeat(i, i), Nil) == Nil

@Test
def flatMap02(): Bool = List.flatMap(i -> List.repeat(i, i), 0 :: Nil) == Nil

@Test
def flatMap03(): Bool = List.flatMap(i -> List.repeat(i, i), 1 :: Nil) == 1 :: Nil

@Test
def flatMap04(): Bool = List.flatMap(i -> List.repeat(i, i), 2 :: Nil) == 2 :: 2 :: Nil

@Test
def flatMap05(): Bool = List.flatMap(i -> List.repeat(i, i), 1 :: 2 :: Nil) == 1 :: 2 :: 2 :: Nil

@Test
def flatMap06(): Bool = List.flatMap(i -> List.repeat(i, i), 2 :: 3 :: Nil) == 2 :: 2 :: 3 :: 3 :: 3 :: Nil

/////////////////////////////////////////////////////////////////////////////
// reverse                                                                 //
/////////////////////////////////////////////////////////////////////////////
@Test
def reverse01(): Bool = List.reverse(Nil: List[Unit]) == Nil

@Test
def reverse02(): Bool = List.reverse(1 :: Nil) == 1 :: Nil

@Test
def reverse03(): Bool = List.reverse(1 :: 2 :: Nil) == 2 :: 1 :: Nil

@Test
def reverse04(): Bool = List.reverse(1 :: 1 :: Nil) == 1 :: 1 :: Nil

@Test
def reverse05(): Bool = List.reverse(1 :: 2 :: 3 :: Nil) == 3 :: 2 :: 1 :: Nil

@Test
def reverse06(): Bool = List.reverse(1 :: 2 :: 3 :: 4 :: Nil) == 4 :: 3 :: 2 :: 1 :: Nil

/////////////////////////////////////////////////////////////////////////////
// rotateLeft                                                              //
/////////////////////////////////////////////////////////////////////////////
@Test
def rotateLeft01(): Bool = List.rotateLeft(0, Nil: List[Unit]) == Nil

@Test
def rotateLeft02(): Bool = List.rotateLeft(1, Nil: List[Unit]) == Nil

@Test
def rotateLeft03(): Bool = List.rotateLeft(0, 1 :: Nil) == 1 :: Nil

@Test
def rotateLeft04(): Bool = List.rotateLeft(0, 1 :: 2 :: Nil) == 1 :: 2 :: Nil

@Test
def rotateLeft05(): Bool = List.rotateLeft(1, 1 :: 2 :: Nil) == 2 :: 1 :: Nil

@Test
def rotateLeft06(): Bool = List.rotateLeft(2, 1 :: 2 :: Nil) == 1 :: 2 :: Nil

@Test
def rotateLeft07(): Bool = List.rotateLeft(3, 1 :: 2 :: Nil) == 2 :: 1 :: Nil

@Test
def rotateLeft08(): Bool = List.rotateLeft(-1, 1 :: 2 :: Nil) == 2 :: 1 :: Nil

@Test
def rotateLeft09(): Bool = List.rotateLeft(0, 1 :: 2 :: 3 :: Nil) == 1 :: 2 :: 3 :: Nil

@Test
def rotateLeft10(): Bool = List.rotateLeft(1, 1 :: 2 :: 3 :: Nil) == 2 :: 3 :: 1 :: Nil

@Test
def rotateLeft11(): Bool = List.rotateLeft(2, 1 :: 2 :: 3 :: Nil) == 3 :: 1 :: 2 :: Nil

/////////////////////////////////////////////////////////////////////////////
// rotateRight                                                             //
/////////////////////////////////////////////////////////////////////////////
@Test
def rotateRight01(): Bool = List.rotateRight(0, Nil: List[Unit]) == Nil

@Test
def rotateRight02(): Bool = List.rotateRight(1, Nil: List[Unit]) == Nil

@Test
def rotateRight03(): Bool = List.rotateRight(0, 1 :: Nil) == 1 :: Nil

@Test
def rotateRight04(): Bool = List.rotateRight(0, 1 :: 2 :: Nil) == 1 :: 2 :: Nil

@Test
def rotateRight05(): Bool = List.rotateRight(1, 1 :: 2 :: Nil) == 2 :: 1 :: Nil

@Test
def rotateRight06(): Bool = List.rotateRight(2, 1 :: 2 :: Nil) == 1 :: 2 :: Nil

@Test
def rotateRight07(): Bool = List.rotateRight(3, 1 :: 2 :: Nil) == 2 :: 1 :: Nil

@Test
def rotateRight08(): Bool = List.rotateRight(-1, 1 :: 2 :: Nil) == 2 :: 1 :: Nil

@Test
def rotateRight09(): Bool = List.rotateRight(0, 1 :: 2 :: 3 :: Nil) == 1 :: 2 :: 3 :: Nil

@Test
def rotateRight10(): Bool = List.rotateRight(1, 1 :: 2 :: 3 :: Nil) == 3 :: 1 :: 2 :: Nil

@Test
def rotateRight11(): Bool = List.rotateRight(2, 1 :: 2 :: 3 :: Nil) == 2 :: 3 :: 1 :: Nil

/////////////////////////////////////////////////////////////////////////////
// update                                                                  //
/////////////////////////////////////////////////////////////////////////////
@Test
def update01(): Bool = List.update(0, 2, Nil) == Nil

@Test
def update02(): Bool = List.update(-1, 2, 1 :: Nil) == 1 :: Nil

@Test
def update03(): Bool = List.update(0, 2, 1 :: Nil) == 2 :: Nil

@Test
def update04(): Bool = List.update(1, 2, 1 :: Nil) == 1 :: Nil

@Test
def update05(): Bool = List.update(0, 5, 1 :: 2 :: Nil) == 5 :: 2 :: Nil

@Test
def update06(): Bool = List.update(1, 5, 1 :: 2 :: Nil) == 1 :: 5 :: Nil

@Test
def update07(): Bool = List.update(2, 5, 1 :: 2 :: Nil) == 1 :: 2 :: Nil

/////////////////////////////////////////////////////////////////////////////
// replace                                                                 //
/////////////////////////////////////////////////////////////////////////////
@Test
def replace01(): Bool = List.replace(from = 3, to = 4, Nil) == Nil

@Test
def replace02(): Bool = List.replace(from = 3, to = 4, 1 :: Nil) == 1 :: Nil

@Test
def replace03(): Bool = List.replace(from = 3, to = 4, 3 :: Nil) == 4 :: Nil

@Test
def replace04(): Bool = List.replace(from = 3, to = 4, 4 :: Nil) == 4 :: Nil

@Test
def replace05(): Bool = List.replace(from = 3, to = 4, 1 :: 2 :: Nil) == 1 :: 2 :: Nil

@Test
def replace06(): Bool = List.replace(from = 3, to = 4, 1 :: 3 :: Nil) == 1 :: 4 :: Nil

@Test
def replace07(): Bool = List.replace(from = 3, to = 4, 3 :: 4 :: Nil) == 4 :: 4 :: Nil

@Test
def replace08(): Bool = List.replace(from = 3, to = 4, 3 :: 3 :: Nil) == 4 :: 4 :: Nil

/////////////////////////////////////////////////////////////////////////////
// patch                                                                   //
/////////////////////////////////////////////////////////////////////////////
@Test
def patch01(): Bool = List.patch(0, 0, Nil: List[Int32], Nil) == Nil

@Test
def patch02(): Bool = List.patch(0, 2, 1 :: 2 :: Nil, Nil) == Nil

@Test
def patch03(): Bool = List.patch(0, 2, Nil, 1 :: 2 :: Nil) == 1 :: 2 :: Nil

@Test
def patch04(): Bool = List.patch(-3, 3, 1 :: 2 :: 4 :: Nil, 1 :: 2 :: Nil) == 1 :: 2 :: Nil

@Test
def patch05(): Bool = List.patch(2, 3, 1 :: 2 :: 4 :: Nil, 1 :: 2 :: Nil) == 1 :: 2 :: Nil

@Test
def patch06(): Bool = List.patch(0, 0, Nil, 1 :: Nil) == 1 :: Nil

@Test
def patch07(): Bool = List.patch(1, 0, 2 :: Nil, 1 :: Nil) == 1 :: Nil

@Test
def patch08(): Bool = List.patch(0, 1, 2 :: Nil, 1 :: Nil) == 2 :: Nil

@Test
def patch09(): Bool = List.patch(0, 2, 2 :: 4 :: Nil, 1 :: Nil) == 2 :: Nil

@Test
def patch10(): Bool = List.patch(-1, 2, 2 :: 4 :: Nil, 1 :: Nil) == 4 :: Nil

@Test
def patch11(): Bool = List.patch(-1, 2, 3 :: 4 :: Nil, 1 :: 2 :: Nil) == 4 :: 2 :: Nil

@Test
def patch12(): Bool = List.patch(1, 2, 3 :: 4 :: Nil, 1 :: 2 :: Nil) == 1 :: 3 :: Nil

@Test
def patch13(): Bool = List.patch(-2, 2, 3 :: 4 :: Nil, 1 :: 2 :: Nil) == 1 :: 2 :: Nil

@Test
def patch14(): Bool = List.patch(2, 2, 3 :: 4 :: Nil, 1 :: 2 :: Nil) == 1 :: 2 :: Nil

@Test
def patch15(): Bool = List.patch(1, 1, 3 :: Nil, 1 :: 2 :: Nil) == 1 :: 3 :: Nil

@Test
def patch16(): Bool = List.patch(0, 2, 3 :: 4 :: Nil, 1 :: 2 :: Nil) == 3 :: 4 :: Nil

@Test
def patch17(): Bool = List.patch(0, 1, 4 :: Nil, 1 :: 2 :: 3 :: Nil) == 4 :: 2 :: 3 :: Nil

@Test
def patch18(): Bool = List.patch(1, 1, 4 :: Nil, 1 :: 2 :: 3 :: Nil) == 1 :: 4 :: 3 :: Nil

@Test
def patch19(): Bool = List.patch(2, 1, 4 :: Nil, 1 :: 2 :: 3 :: Nil) == 1 :: 2 :: 4 :: Nil

@Test
def patch20(): Bool = List.patch(0, 2, 4 :: 5 :: Nil, 1 :: 2 :: 3 :: Nil) == 4 :: 5 :: 3 :: Nil

@Test
def patch21(): Bool = List.patch(1, 2, 4 :: 5 :: Nil, 1 :: 2 :: 3 :: Nil) == 1 :: 4 :: 5 :: Nil

@Test
def patch22(): Bool = List.patch(0, 2, 4 :: 5 :: 6 :: Nil, 1 :: 2 :: 3 :: Nil) == 4 :: 5 :: 3 :: Nil

@Test
def patch23(): Bool = List.patch(0, 3, 4 :: 5 :: 6 :: Nil, 1 :: 2 :: 3 :: Nil) == 4 :: 5 :: 6 :: Nil

@Test
def patch24(): Bool = List.patch(2, 4, 14 :: 15 :: 16 :: 17 :: Nil, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: Nil) ==
                    1 :: 2 :: 14 :: 15 :: 16 :: 17 :: 7 :: Nil

@Test
def patch25(): Bool = List.patch(-2, 4, 14 :: 15 :: 16 :: 17 :: Nil, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: Nil) ==
                    16 :: 17 :: 3 :: 4 :: 5 :: 6 :: 7 :: Nil

@Test
def patch26(): Bool = List.patch(4, 5, 14 :: 15 :: 16 :: 17 :: Nil, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: Nil) ==
                    1 :: 2 :: 3 :: 4 :: 14 :: 15 :: 16 :: Nil

@Test
def patch27(): Bool = List.patch(4, 2, 14 :: 15 :: 16 :: 17 :: Nil, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: Nil) ==
                    1 :: 2 :: 3 :: 4 :: 14 :: 15 :: 7 :: Nil

@Test
def patch28(): Bool = List.patch(-1, 10, -1 :: -2 :: -3 :: -4 :: -5 :: -6 :: -7 :: -8 :: Nil, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: Nil) ==
                    -2 :: -3 :: -4 :: -5 :: -6 :: -7 :: -8 :: Nil

/////////////////////////////////////////////////////////////////////////////
// permutations                                                            //
/////////////////////////////////////////////////////////////////////////////
@Test
def permutations01(): Bool = List.permutations(Nil: List[Unit]) == Nil :: Nil

@Test
def permutations02(): Bool = List.permutations(1 :: Nil) == (1 :: Nil) :: Nil

@Test
def permutations03(): Bool = List.permutations(1 :: 2 :: Nil) == (1 :: 2 :: Nil) :: (2 :: 1 :: Nil) :: Nil

@Test
def permutations04(): Bool = List.permutations(1 :: 2 :: 3 :: Nil) == (1 :: 2 :: 3 :: Nil) :: (1 :: 3 :: 2 :: Nil) ::
                                                                             (2 :: 1 :: 3 :: Nil) :: (2 :: 3 :: 1 :: Nil) ::
                                                                             (3 :: 1 :: 2 :: Nil) :: (3 :: 2 :: 1 :: Nil) :: Nil

/////////////////////////////////////////////////////////////////////////////
// subsequences                                                            //
/////////////////////////////////////////////////////////////////////////////
@Test
def subsequences01(): Bool = List.subsequences(Nil: List[Unit]) == Nil :: Nil

@Test
def subsequences02(): Bool = List.subsequences(1 :: Nil) == (1 :: Nil) :: Nil :: Nil

@Test
def subsequences03(): Bool = List.subsequences(1 :: 2 :: Nil) == (1 :: 2 :: Nil) :: (1 :: Nil) ::
                                                                        (2 :: Nil) :: Nil :: Nil

@Test
def subsequences04(): Bool = List.subsequences(1 :: 2 :: 3 :: Nil) == (1 :: 2 :: 3 :: Nil) :: (1 :: 2 :: Nil) ::
                                                                             (1 :: 3 :: Nil) :: (1 :: Nil) ::
                                                                             (2 :: 3 :: Nil) :: (2 :: Nil) ::
                                                                             (3 :: Nil) :: Nil :: Nil

/////////////////////////////////////////////////////////////////////////////
// intersperse                                                             //
/////////////////////////////////////////////////////////////////////////////
@Test
def intersperse01(): Bool = List.intersperse(11, Nil) == Nil

@Test
def intersperse02(): Bool = List.intersperse(11, 1 :: Nil) == 1 :: Nil

@Test
def intersperse03(): Bool = List.intersperse(11, 1 :: 2 :: Nil) == 1 :: 11 :: 2 :: Nil

@Test
def intersperse04(): Bool = List.intersperse(11, 1 :: 2 :: 3 :: Nil) == 1 :: 11 :: 2 :: 11 :: 3 :: Nil

@Test
def intersperse05(): Bool = List.intersperse(11, 1 :: 2 :: 3 :: 4 :: Nil) == 1 :: 11 :: 2 :: 11 :: 3 :: 11 :: 4 :: Nil

/////////////////////////////////////////////////////////////////////////////
// transpose                                                               //
/////////////////////////////////////////////////////////////////////////////
@Test
def transpose01(): Bool = List.transpose(Nil: List[List[Unit]]) == Nil

@Test
def transpose02(): Bool = List.transpose(Nil :: Nil): List[List[Unit]] == Nil :: Nil

@Test
def transpose03(): Bool = List.transpose(Nil :: Nil :: Nil): List[List[Unit]] == Nil :: Nil :: Nil

@Test
def transpose04(): Bool = List.transpose(Nil :: Nil :: Nil :: Nil): List[List[Unit]] == Nil :: Nil :: Nil :: Nil

@Test
def transpose05(): Bool = List.transpose((1 :: Nil) :: Nil) == (1 :: Nil) :: Nil

@Test
def transpose06(): Bool = List.transpose((1 :: 2 :: Nil) :: Nil) == (1 :: Nil) :: (2 :: Nil) :: Nil

@Test
def transpose07(): Bool = List.transpose((1 :: 2 :: 3 :: Nil) :: Nil) ==
                        (1 :: Nil) :: (2 :: Nil) :: (3 :: Nil) :: Nil

@Test
def transpose08(): Bool = List.transpose((1 :: 2 :: 3 :: 4 :: Nil) :: Nil) ==
                        (1 :: Nil) :: (2 :: Nil) :: (3 :: Nil) :: (4 :: Nil) :: Nil

@Test
def transpose09(): Bool = List.transpose((1 :: Nil) :: (2 :: Nil) :: Nil) == (1 :: 2 :: Nil) :: Nil

@Test
def transpose10(): Bool = List.transpose((1 :: Nil) :: (2 :: Nil) :: (3 :: Nil) :: Nil) ==
                        (1 :: 2 :: 3 :: Nil) :: Nil

@Test
def transpose11(): Bool = List.transpose((1 :: Nil) :: (2 :: Nil) :: (3 :: Nil) :: (4 :: Nil) :: Nil) ==
                        (1 :: 2 :: 3 :: 4 :: Nil) :: Nil

@Test
def transpose12(): Bool = List.transpose((1 :: 2 :: Nil) :: (3 :: 4 :: Nil) :: Nil) ==
                        (1 :: 3 :: Nil) :: (2 :: 4 :: Nil) :: Nil

@Test
def transpose13(): Bool = List.transpose((1 :: 2 :: 3 :: Nil) :: (4 :: 5 :: 6 :: Nil) :: Nil) ==
                        (1 :: 4 :: Nil) :: (2 :: 5 :: Nil) :: (3 :: 6 :: Nil) :: Nil

@Test
def transpose14(): Bool = List.transpose((1 :: 2 :: 3 :: 4 :: Nil) :: (5 :: 6 :: 7 :: 8 :: Nil) :: Nil) ==
                        (1 :: 5 :: Nil) :: (2 :: 6 :: Nil) :: (3 :: 7 :: Nil) :: (4 :: 8 :: Nil) :: Nil

@Test
def transpose15(): Bool = List.transpose((1 :: 2 :: 3 :: 4 :: 5 :: Nil) :: (6 :: 7 :: 8 :: 9 :: 10 :: Nil) :: Nil) ==
                        (1 :: 6 :: Nil) :: (2 :: 7 :: Nil) :: (3 :: 8 :: Nil) :: (4 :: 9 :: Nil) :: (5 :: 10 :: Nil) :: Nil

@Test
def transpose16(): Bool = List.transpose((1 :: 2 :: Nil) :: (3 :: 4 :: Nil) :: (5 :: 6 :: Nil) :: Nil) ==
                        (1 :: 3 :: 5 :: Nil) :: (2 :: 4 :: 6 :: Nil) :: Nil

@Test
def transpose17(): Bool = List.transpose((1 :: 2 :: Nil) :: (3 :: 4 :: Nil) :: (5 :: 6 :: Nil) :: (7 :: 8 :: Nil) :: Nil) ==
                        (1 :: 3 :: 5 :: 7 :: Nil) :: (2 :: 4 :: 6 :: 8 :: Nil) :: Nil

@Test
def transpose18(): Bool = List.transpose((1 :: 2 :: Nil) :: (3 :: 4 :: Nil) :: (5 :: 6 :: Nil) :: (7 :: 8 :: Nil) :: (9 :: 10 :: Nil) :: Nil) ==
                        (1 :: 3 :: 5 :: 7 :: 9 :: Nil) :: (2 :: 4 :: 6 :: 8 :: 10 :: Nil) :: Nil

@Test
def transpose19(): Bool = List.transpose((1 :: 2 :: 3 :: Nil) :: (4 :: 5 :: 6 :: Nil) :: (7 :: 8 :: 9 :: Nil) :: Nil) ==
                        (1 :: 4 :: 7 :: Nil) :: (2 :: 5 :: 8 :: Nil) :: (3 :: 6 :: 9 :: Nil) :: Nil

@Test
def transpose20(): Bool = List.transpose((1 :: 2 :: 3 :: Nil) :: (4 :: 5 :: Nil) :: (7 :: 8 :: 9 :: Nil) :: Nil) ==
                        (1 :: 2 :: 3 :: Nil) :: (4 :: 5 :: Nil) :: (7 :: 8 :: 9 :: Nil) :: Nil

@Test
def transpose21(): Bool = List.transpose((1 :: 2 :: 3 :: Nil) :: Nil :: (7 :: 8 :: 9 :: Nil) :: Nil) ==
                        (1 :: 2 :: 3 :: Nil) :: Nil :: (7 :: 8 :: 9 :: Nil) :: Nil

@Test
def transpose22(): Bool = List.transpose((1 :: 2 :: 3 :: Nil) :: (4 :: 5 :: 6 :: Nil) :: (7 :: 8 :: 9 :: 10 :: Nil) :: Nil) ==
                        (1 :: 2 :: 3 :: Nil) :: (4 :: 5 :: 6 :: Nil) :: (7 :: 8 :: 9 :: 10 :: Nil) :: Nil

/////////////////////////////////////////////////////////////////////////////
// isPrefixOf                                                              //
/////////////////////////////////////////////////////////////////////////////
@Test
def isPrefixOf01(): Bool = List.isPrefixOf(Nil: List[Unit], Nil) == true

@Test
def isPrefixOf02(): Bool = List.isPrefixOf(1 :: Nil, Nil) == false

@Test
def isPrefixOf03(): Bool = List.isPrefixOf(Nil, 1 :: Nil) == true

@Test
def isPrefixOf04(): Bool = List.isPrefixOf(1 :: Nil, 1 :: Nil) == true

@Test
def isPrefixOf05(): Bool = List.isPrefixOf(2 :: Nil, 1 :: Nil) == false

@Test
def isPrefixOf06(): Bool = List.isPrefixOf(1 :: 2 :: Nil, 1 :: Nil) == false

@Test
def isPrefixOf07(): Bool = List.isPrefixOf(Nil, 1 :: 2 :: Nil) == true

@Test
def isPrefixOf08(): Bool = List.isPrefixOf(1 :: Nil, 1 :: 2 :: Nil) == true

@Test
def isPrefixOf09(): Bool = List.isPrefixOf(2 :: Nil, 1 :: 2 :: Nil) == false

@Test
def isPrefixOf10(): Bool = List.isPrefixOf(1 :: 2 :: Nil, 1 :: 2 :: Nil) == true

@Test
def isPrefixOf11(): Bool = List.isPrefixOf(1 :: 3 :: Nil, 1 :: 2 :: Nil) == false

@Test
def isPrefixOf12(): Bool = List.isPrefixOf(1 :: 2 :: 3 :: Nil, 1 :: 2 :: Nil) == false

@Test
def isPrefixOf13(): Bool = List.isPrefixOf(1 :: 2 :: 3 :: Nil, 89 :: 11 :: 1 :: 2 :: 3 :: 4 :: Nil) == false

/////////////////////////////////////////////////////////////////////////////
// isInfixOf                                                               //
/////////////////////////////////////////////////////////////////////////////
@Test
def isInfixOf01(): Bool = List.isInfixOf(Nil: List[Unit], Nil) == true

@Test
def isInfixOf02(): Bool = List.isInfixOf(1 :: Nil, Nil) == false

@Test
def isInfixOf03(): Bool = List.isInfixOf(Nil, 1 :: Nil) == true

@Test
def isInfixOf04(): Bool = List.isInfixOf(1 :: Nil, 1 :: Nil) == true

@Test
def isInfixOf05(): Bool = List.isInfixOf(2 :: Nil, 1 :: Nil) == false

@Test
def isInfixOf06(): Bool = List.isInfixOf(1 :: 2 :: Nil, 1 :: Nil) == false

@Test
def isInfixOf07(): Bool = List.isInfixOf(Nil, 1 :: 2 :: Nil) == true

@Test
def isInfixOf08(): Bool = List.isInfixOf(1 :: Nil, 1 :: 2 :: Nil) == true

@Test
def isInfixOf09(): Bool = List.isInfixOf(2 :: Nil, 1 :: 2 :: Nil) == true

@Test
def isInfixOf10(): Bool = List.isInfixOf(1 :: 2 :: Nil, 1 :: 2 :: Nil) == true

@Test
def isInfixOf11(): Bool = List.isInfixOf(1 :: 3 :: Nil, 1 :: 2 :: Nil) == false

@Test
def isInfixOf12(): Bool = List.isInfixOf(1 :: 2 :: 3 :: Nil, 1 :: 2 :: Nil) == false

@Test
def isInfixOf13(): Bool = List.isInfixOf(1 :: 2 :: 3 :: Nil, 89 :: 11 :: 1 :: 2 :: 3 :: 4 :: Nil) == true

/////////////////////////////////////////////////////////////////////////////
// isSuffixOf                                                              //
/////////////////////////////////////////////////////////////////////////////
@Test
def isSuffixOf01(): Bool = List.isSuffixOf(Nil: List[Unit], Nil: List[Unit]) == true

@Test
def isSuffixOf02(): Bool = List.isSuffixOf(1 :: Nil, Nil) == false

@Test
def isSuffixOf03(): Bool = List.isSuffixOf(Nil, 1 :: Nil)  == true

@Test
def isSuffixOf04(): Bool = List.isSuffixOf(1 :: Nil, 1 :: Nil) == true

@Test
def isSuffixOf05(): Bool = List.isSuffixOf(2 :: Nil, 1 :: Nil) == false

@Test
def isSuffixOf06(): Bool = List.isSuffixOf(1 :: 2 :: Nil, 1 :: Nil) == false

@Test
def isSuffixOf07(): Bool = List.isSuffixOf(Nil, 1 :: 2 :: Nil) == true

@Test
def isSuffixOf08(): Bool = List.isSuffixOf(1 :: Nil, 1 :: 2 :: Nil) == false

@Test
def isSuffixOf09(): Bool = List.isSuffixOf(2 :: Nil, 1 :: 2 :: Nil) == true

@Test
def isSuffixOf10(): Bool = List.isSuffixOf(1 :: 2 :: Nil, 1 :: 2 :: Nil) == true

@Test
def isSuffixOf11(): Bool = List.isSuffixOf(1 :: 3 :: Nil, 1 :: 2 :: Nil) == false

@Test
def isSuffixOf12(): Bool = List.isSuffixOf(1 :: 2 :: 3 :: Nil, 1 :: 2 :: Nil) == false

@Test
def isSuffixOf13(): Bool = List.isSuffixOf(1 :: 2 :: 3 :: Nil, 89 :: 11 :: 1 :: 2 :: 3 :: Nil) == true

/////////////////////////////////////////////////////////////////////////////
// fold                                                                    //
/////////////////////////////////////////////////////////////////////////////
@Test
def fold01(): Bool = List.fold(Nil: List[String]) == ""

@Test
def fold02(): Bool = List.fold("a" :: Nil) == "a"

@Test
def fold03(): Bool = List.fold("a" :: "b" :: "c" :: Nil) == "abc"

@Test
def fold04(): Bool = List.fold(("a", "b") :: ("c", "d") :: ("e", "f") :: Nil) == ("ace", "bdf")

/////////////////////////////////////////////////////////////////////////////
// foldLeft                                                                //
/////////////////////////////////////////////////////////////////////////////
@Test
def foldLeft01(): Bool = List.foldLeft((i, e) -> (i - e)*(e `Int32.rem` 2 + 1), 100, Nil) == 100

@Test
def foldLeft02(): Bool = List.foldLeft((i, e) -> (i - e)*(e `Int32.rem` 2 + 1), 100, 1 :: Nil) == 198

@Test
def foldLeft03(): Bool = List.foldLeft((i, e) -> (i - e)*(e `Int32.rem` 2 + 1), 100, 1 :: 2 :: Nil) == 196

@Test
def foldLeft04(): Bool = List.foldLeft((i, e) -> (i - e)*(e `Int32.rem` 2 + 1), 100, 1 :: 2 :: 3 :: Nil) == 386

/////////////////////////////////////////////////////////////////////////////
// foldRight                                                               //
/////////////////////////////////////////////////////////////////////////////
@Test
def foldRight01(): Bool = List.foldRight((e, acc) -> (acc - e) * (e `Int32.rem` 2 + 1), 100, Nil) == 100

@Test
def foldRight02(): Bool = List.foldRight((e, acc) -> (acc - e) * (e `Int32.rem` 2 + 1), 100, 1 :: Nil) == 198

@Test
def foldRight03(): Bool = List.foldRight((e, acc) -> (acc - e) * (e `Int32.rem` 2 + 1), 100, 1 :: 2 :: Nil) == 194

@Test
def foldRight04(): Bool = List.foldRight((e, acc) -> (acc - e) * (e `Int32.rem` 2 + 1), 100, 1 :: 2 :: 3 :: Nil) == 382

@Test
def foldRight05(): Bool = List.foldRight((e, acc) -> (acc - e) * (e `Int32.rem` 2 + 1), 100, Nil) == 100

@Test
def foldRight06(): Bool = List.foldRight((e, acc) -> (acc - e) * (e `Int32.rem` 2 + 1), 100, 1 :: Nil) == 198

@Test
def foldRight07(): Bool = List.foldRight((e, acc) -> (acc - e) * (e `Int32.rem` 2 + 1), 100, 1 :: 2 :: Nil) == 194

@Test
def foldRight08(): Bool = List.foldRight((e, acc) -> (acc - e) * (e `Int32.rem` 2 + 1), 100, 1 :: 2 :: 3 :: Nil) == 382

/////////////////////////////////////////////////////////////////////////////
// foldRightWithCont                                                       //
/////////////////////////////////////////////////////////////////////////////
@Test
def foldRightWithCont01(): Bool = List.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.rem` 2 + 1), 100, Nil) == 100

@Test
def foldRightWithCont02(): Bool = List.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.rem` 2 + 1), 100, 1 :: Nil) == 198

@Test
def foldRightWithCont03(): Bool = List.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.rem` 2 + 1), 100, 1 :: 2 :: Nil) == 194

@Test
def foldRightWithCont04(): Bool = List.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.rem` 2 + 1), 100, 1 :: 2 :: 3 :: Nil) == 382

@Test
def foldRightWithCont05(): Bool = List.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.rem` 2 + 1), 100, Nil) == 100

@Test
def foldRightWithCont06(): Bool = List.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.rem` 2 + 1), 100, 1 :: Nil) == 198

@Test
def foldRightWithCont07(): Bool = List.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.rem` 2 + 1), 100, 1 :: 2 :: Nil) == 194

@Test
def foldRightWithCont08(): Bool = List.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.rem` 2 + 1), 100, 1 :: 2 :: 3 :: Nil) == 382

/////////////////////////////////////////////////////////////////////////////
// reduceLeft                                                              //
/////////////////////////////////////////////////////////////////////////////
@Test
def reduceLeft01(): Bool = List.reduceLeft((a, b) -> a-b, Nil: List[Int32]) == None

@Test
def reduceLeft02(): Bool = List.reduceLeft((a, b) -> a-b, 1 :: Nil) == Some(1)

@Test
def reduceLeft03(): Bool = List.reduceLeft((a, b) -> a-b, 1 :: 2 :: Nil) == Some(-1)

@Test
def reduceLeft04(): Bool = List.reduceLeft((a, b) -> a-b, 1 :: 2 :: 3 :: Nil) == Some(-4)

@Test
def reduceLeft05(): Bool = List.reduceLeft((a, b) -> a-b, 1 :: 2 :: 3 :: 4 :: Nil) == Some(-8)

/////////////////////////////////////////////////////////////////////////////
// reduceRight                                                             //
/////////////////////////////////////////////////////////////////////////////
@Test
def reduceRight01(): Bool = List.reduceRight((a, b) -> a-b, Nil: List[Int32]) == None

@Test
def reduceRight02(): Bool = List.reduceRight((a, b) -> a-b, 1 :: Nil) == Some(1)

@Test
def reduceRight03(): Bool = List.reduceRight((a, b) -> a-b, 1 :: 2 :: Nil) == Some(-1)

@Test
def reduceRight04(): Bool = List.reduceRight((a, b) -> a-b, 1 :: 2 :: 3 :: Nil) == Some(2)

@Test
def reduceRight05(): Bool = List.reduceRight((a, b) -> a-b, 1 :: 2 :: 3 :: 4 :: Nil) == Some(-2)

/////////////////////////////////////////////////////////////////////////////
// count                                                                   //
/////////////////////////////////////////////////////////////////////////////
@Test
def count01(): Bool = List.count(i -> i > 3, Nil) == 0

@Test
def count02(): Bool = List.count(i -> i > 3, 1 :: Nil) == 0

@Test
def count03(): Bool = List.count(i -> i > 3, 4 :: Nil) == 1

@Test
def count04(): Bool = List.count(i -> i > 3, 1 :: 2 :: Nil) == 0

@Test
def count05(): Bool = List.count(i -> i > 3, 1 :: 8 :: Nil) == 1

@Test
def count06(): Bool = List.count(i -> i > 3, 8 :: 1 :: Nil) == 1

@Test
def count07(): Bool = List.count(i -> i > 3, 6 :: 7 :: Nil) == 2

/////////////////////////////////////////////////////////////////////////////
// flatten                                                                 //
/////////////////////////////////////////////////////////////////////////////
@Test
def flatten01(): Bool = List.flatten(Nil: List[List[Unit]]) == Nil

@Test
def flatten02(): Bool = List.flatten(Nil :: Nil): List[Unit] == Nil

@Test
def flatten03(): Bool = List.flatten((1 :: Nil) :: Nil) == 1 :: Nil

@Test
def flatten04(): Bool = List.flatten((1 :: 2 :: Nil) :: Nil) == 1 :: 2 :: Nil

@Test
def flatten05(): Bool = List.flatten(Nil :: Nil :: Nil): List[Unit] == Nil

@Test
def flatten06(): Bool = List.flatten((1 :: Nil) :: Nil :: Nil) == 1 :: Nil

@Test
def flatten07(): Bool = List.flatten(Nil :: (1 :: Nil) :: Nil) == 1 :: Nil

@Test
def flatten08(): Bool = List.flatten((1 :: Nil) :: (2 :: Nil) :: Nil) == 1 :: 2 :: Nil

@Test
def flatten09(): Bool = List.flatten((1 :: 2 :: Nil) :: (3 :: 4 :: 5 :: Nil) :: Nil) == 1 :: 2 :: 3 :: 4 :: 5 :: Nil

@Test
def flatten10(): Bool = List.flatten((1 :: Nil) :: (2 :: 3 :: Nil) :: (4 :: Nil) :: Nil) == 1 :: 2 :: 3 :: 4 :: Nil

/////////////////////////////////////////////////////////////////////////////
// exists                                                                  //
/////////////////////////////////////////////////////////////////////////////
@Test
def exists01(): Bool = List.exists(i -> i > 3, Nil) == false

@Test
def exists02(): Bool = List.exists(i -> i > 3, 1 :: Nil) == false

@Test
def exists03(): Bool = List.exists(i -> i > 3, 5 :: Nil) == true

@Test
def exists04(): Bool = List.exists(i -> i > 3, 1 :: 2 :: Nil) == false

@Test
def exists05(): Bool = List.exists(i -> i > 3, 1 :: 6 :: Nil) == true

@Test
def exists06(): Bool = List.exists(i -> i > 3, 6 :: 1 :: Nil) == true

@Test
def exists07(): Bool = List.exists(i -> i > 3, 16 :: 6 :: Nil) == true

@Test
def exists08(): Bool = List.exists(i -> i > 3, 1 :: -9 :: 3 :: Nil) == false

@Test
def exists09(): Bool = List.exists(i -> i > 3, 1 :: 9 :: 3 :: Nil) == true

/////////////////////////////////////////////////////////////////////////////
// forAll                                                                  //
/////////////////////////////////////////////////////////////////////////////
@Test
def forAll01(): Bool = List.forAll(i -> i > 3, Nil) == true

@Test
def forAll02(): Bool = List.forAll(i -> i > 3, 1 :: Nil) == false

@Test
def forAll03(): Bool = List.forAll(i -> i > 3, 5 :: Nil) == true

@Test
def forAll04(): Bool = List.forAll(i -> i > 3, 1 :: 2 :: Nil) == false

@Test
def forAll05(): Bool = List.forAll(i -> i > 3, 1 :: 6 :: Nil) == false

@Test
def forAll06(): Bool = List.forAll(i -> i > 3, 6 :: 1 :: Nil) == false

@Test
def forAll07(): Bool = List.forAll(i -> i > 3, 16 :: 6 :: Nil) == true

@Test
def forAll08(): Bool = List.forAll(i -> i > 3, 1 :: -9 :: 3 :: Nil) == false

@Test
def forAll09(): Bool = List.forAll(i -> i > 3, 1 :: 9 :: 3 :: Nil) == false

@Test
def forAll10(): Bool = List.forAll(i -> i > 3, 11 :: 9 :: 31 :: Nil) == true

/////////////////////////////////////////////////////////////////////////////
// filter                                                                  //
/////////////////////////////////////////////////////////////////////////////
@Test
def filter01(): Bool = List.filter(i -> i > 3, Nil) == Nil

@Test
def filter02(): Bool = List.filter(i -> i > 3, 2 :: Nil) == Nil

@Test
def filter03(): Bool = List.filter(i -> i > 3, 4 :: Nil) == 4 :: Nil

@Test
def filter04(): Bool = List.filter(i -> i > 3, 1 :: 3 :: Nil) == Nil

@Test
def filter05(): Bool = List.filter(i -> i > 3, 1 :: 8 :: Nil) == 8 :: Nil

@Test
def filter06(): Bool = List.filter(i -> i > 3, 8 :: 1 :: Nil) == 8 :: Nil

@Test
def filter07(): Bool = List.filter(i -> i > 3, 8 :: 9 :: Nil) == 8 :: 9 :: Nil

@Test
def filter08(): Bool = List.filter(i -> i > 3, 1 :: 4 :: 11 :: 2 :: -22 :: 17 :: Nil) == 4 :: 11 :: 17 :: Nil

/////////////////////////////////////////////////////////////////////////////
// init                                                                  //
/////////////////////////////////////////////////////////////////////////////
@Test
def init01(): Bool = List.init(Nil: List[Unit]) == None

@Test
def init02(): Bool = List.init(0 :: Nil) == Some(Nil)

@Test
def init03(): Bool = List.init(0 :: 1 :: Nil) == Some(0 :: Nil)

@Test
def init04(): Bool = List.init(2 :: 1 :: 0 :: Nil) == Some(2 :: 1 :: Nil)

@Test
def init05(): Bool = List.init('a' :: 'b' :: 'c' :: 'd' :: Nil) == Some('a' :: 'b' :: 'c' :: Nil)

@Test
def init06(): Bool = List.init("test1" :: "test2" :: "test3" :: Nil) == Some("test1" :: "test2" :: Nil)

@Test
def init07(): Bool = List.init(2 :: 1 :: 0 :: -1 :: Nil) == Some(2 :: 1 :: 0 :: Nil)

/////////////////////////////////////////////////////////////////////////////
// slice                                                                   //
/////////////////////////////////////////////////////////////////////////////
@Test
def slice01(): Bool = List.slice(start = 0, end = 0, Nil: List[Unit]) == Nil

@Test
def slice02(): Bool = List.slice(start = -1, end = 1, Nil: List[Unit]) == Nil

@Test
def slice03(): Bool = List.slice(start = 0, end = 0, 1 :: Nil) == Nil

@Test
def slice04(): Bool = List.slice(start = 0, end = 1, 1 :: Nil) == 1 :: Nil

@Test
def slice05(): Bool = List.slice(start = 0, end = 2, 1 :: Nil) == 1 :: Nil

@Test
def slice06(): Bool = List.slice(start = 2, end = 5, 1 :: Nil) == Nil

@Test
def slice07(): Bool = List.slice(start = -1, end = 1, 1 :: Nil) == 1 :: Nil

@Test
def slice08(): Bool = List.slice(start = 0, end = 1, 1 :: 2 :: Nil) == 1 :: Nil

@Test
def slice09(): Bool = List.slice(start = 0, end = 2, 1 :: 2 :: Nil) == 1 :: 2 :: Nil

@Test
def slice10(): Bool = List.slice(start = 1, end = 2, 1 :: 2 :: Nil) == 2 :: Nil

@Test
def slice11(): Bool = List.slice(start = 0, end = 3, 1 :: 2 :: 3 :: Nil) == 1 :: 2 :: 3 :: Nil

@Test
def slice12(): Bool = List.slice(start = 0, end = 2, 1 :: 2 :: 3 :: Nil) == 1 :: 2 :: Nil

@Test
def slice13(): Bool = List.slice(start = 1, end = 3, 1 :: 2 :: 3 :: Nil) == 2 :: 3 :: Nil

@Test
def slice14(): Bool = List.slice(start = 1, end = 2, 1 :: 2 :: 3 :: Nil) == 2 :: Nil

/////////////////////////////////////////////////////////////////////////////
// partition                                                               //
/////////////////////////////////////////////////////////////////////////////
@Test
def partition01(): Bool = List.partition(i -> i > 3, Nil) == (Nil, Nil)

@Test
def partition02(): Bool = List.partition(i -> i > 3, 1 :: Nil) == (Nil, 1 :: Nil)

@Test
def partition03(): Bool = List.partition(i -> i > 3, 4 :: Nil) == (4 :: Nil, Nil)

@Test
def partition04(): Bool = List.partition(i -> i > 3, 1 :: 2 :: Nil) == (Nil, 1 :: 2 :: Nil)

@Test
def partition05(): Bool = List.partition(i -> i > 3, 1 :: 5 :: Nil) == (5 :: Nil, 1 :: Nil)

@Test
def partition06(): Bool = List.partition(i -> i > 3, 5 :: 1 :: Nil) == (5 :: Nil, 1 :: Nil)

@Test
def partition07(): Bool = List.partition(i -> i > 3, 5 :: 8 :: Nil) == (5 :: 8 :: Nil, Nil)

@Test
def partition08(): Bool = List.partition(i -> i > 3, 4 :: -3 :: -5 :: 1 :: 2 :: 16 :: 7 :: 1 :: 7 :: Nil) ==
                        (4 :: 16 :: 7 :: 7 :: Nil, -3 :: -5 :: 1 :: 2 :: 1 :: Nil)

/////////////////////////////////////////////////////////////////////////////
// span                                                                    //
/////////////////////////////////////////////////////////////////////////////
@Test
def span01(): Bool = List.span(i -> i > 3, Nil) == (Nil, Nil)

@Test
def span02(): Bool = List.span(i -> i > 3, 1 :: Nil) == (Nil, 1 :: Nil)

@Test
def span03(): Bool = List.span(i -> i > 3, 4 :: Nil) == (4 :: Nil, Nil)

@Test
def span04(): Bool = List.span(i -> i > 3, 1 :: 2 :: Nil) == (Nil, 1 :: 2 :: Nil)

@Test
def span05(): Bool = List.span(i -> i > 3, 1 :: 5 :: Nil) == (Nil, 1 :: 5 :: Nil)

@Test
def span06(): Bool = List.span(i -> i > 3, 5 :: 1 :: Nil) == (5 :: Nil, 1 :: Nil)

@Test
def span07(): Bool = List.span(i -> i > 3, 5 :: 8 :: Nil) == (5 :: 8 :: Nil, Nil)

@Test
def span08(): Bool = List.span(i -> i > 3, 4 :: 6 :: -3 :: 11 :: -5 :: 1 :: 2 :: 16 :: 7 :: 1 :: 7 :: Nil) ==
                        (4 :: 6 :: Nil, -3 :: 11 :: -5 :: 1 :: 2 :: 16 :: 7 :: 1 :: 7 :: Nil)

/////////////////////////////////////////////////////////////////////////////
// drop                                                                    //
/////////////////////////////////////////////////////////////////////////////
@Test
def drop01(): Bool = List.drop(-1, Nil: List[Unit]) == Nil

@Test
def drop02(): Bool = List.drop(0, Nil: List[Unit]) == Nil

@Test
def drop03(): Bool = List.drop(1, Nil: List[Unit]) == Nil

@Test
def drop04(): Bool = List.drop(-1, 1 :: Nil) == 1 :: Nil

@Test
def drop05(): Bool = List.drop(0, 1 :: Nil) == 1 :: Nil

@Test
def drop06(): Bool = List.drop(1, 1 :: Nil) == Nil

@Test
def drop07(): Bool = List.drop(2, 1 :: Nil) == Nil

@Test
def drop08(): Bool = List.drop(0, 1 :: 2 :: Nil) == 1 :: 2 :: Nil

@Test
def drop09(): Bool = List.drop(1, 1 :: 2 :: Nil) == 2 :: Nil

@Test
def drop10(): Bool = List.drop(2, 1 :: 2 :: Nil) == Nil

@Test
def drop11(): Bool = List.drop(2, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil) == 3 :: 4 :: 5 :: 6 :: Nil

@Test
def drop12(): Bool = List.drop(4, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil) == 5 :: 6 :: Nil

/////////////////////////////////////////////////////////////////////////////
// dropWhile                                                               //
/////////////////////////////////////////////////////////////////////////////
@Test
def dropWhile01(): Bool = List.dropWhile(i -> i > 3, Nil) == Nil

@Test
def dropWhile02(): Bool = List.dropWhile(i -> i > 3, 1 :: Nil) == 1 :: Nil

@Test
def dropWhile03(): Bool = List.dropWhile(i -> i > 3, 4 :: Nil) == Nil

@Test
def dropWhile04(): Bool = List.dropWhile(i -> i > 3, 1 :: 2 :: Nil) == 1 :: 2 :: Nil

@Test
def dropWhile05(): Bool = List.dropWhile(i -> i > 3, 1 :: 5 :: Nil) == 1 :: 5 :: Nil

@Test
def dropWhile06(): Bool = List.dropWhile(i -> i > 3, 5 :: 1 :: Nil) == 1 :: Nil

@Test
def dropWhile07(): Bool = List.dropWhile(i -> i > 3, 5 :: 8 :: Nil) == Nil

@Test
def dropWhile08(): Bool = List.dropWhile(i -> i > 3, 4 :: 6 :: -3 :: 11 :: -5 :: 1 :: 2 :: 16 :: 7 :: 1 :: 7 :: Nil) ==
                        -3 :: 11 :: -5 :: 1 :: 2 :: 16 :: 7 :: 1 :: 7 :: Nil

/////////////////////////////////////////////////////////////////////////////
// take                                                                    //
/////////////////////////////////////////////////////////////////////////////
@Test
def take01(): Bool = List.take(-1, Nil: List[Unit]) == Nil

@Test
def take02(): Bool = List.take(0, Nil: List[Unit]) == Nil

@Test
def take03(): Bool = List.take(1, Nil: List[Unit]) == Nil

@Test
def take04(): Bool = List.take(-1, 1 :: Nil) == Nil

@Test
def take05(): Bool = List.take(0, 1 :: Nil) == Nil

@Test
def take06(): Bool = List.take(1, 1 :: Nil) == 1 :: Nil

@Test
def take07(): Bool = List.take(2, 1 :: Nil) == 1 :: Nil

@Test
def take08(): Bool = List.take(0, 1 :: 2 :: Nil) == Nil

@Test
def take09(): Bool = List.take(1, 1 :: 2 :: Nil) == 1 :: Nil

@Test
def take10(): Bool = List.take(2, 1 :: 2 :: Nil) == 1 :: 2 :: Nil

@Test
def take11(): Bool = List.take(2, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil) == 1 :: 2 :: Nil

@Test
def take12(): Bool = List.take(4, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil) == 1 :: 2 :: 3 :: 4 :: Nil

/////////////////////////////////////////////////////////////////////////////
// takeWhile                                                               //
/////////////////////////////////////////////////////////////////////////////
@Test
def takeWhile01(): Bool = List.takeWhile(i -> i > 3, Nil: List[Int32]) == Nil

@Test
def takeWhile02(): Bool = List.takeWhile(i -> i > 3, 1 :: Nil) == Nil

@Test
def takeWhile03(): Bool = List.takeWhile(i -> i > 3, 4 :: Nil) == 4 :: Nil

@Test
def takeWhile04(): Bool = List.takeWhile(i -> i > 3, 1 :: 2 :: Nil) == Nil

@Test
def takeWhile05(): Bool = List.takeWhile(i -> i > 3, 1 :: 5 :: Nil) == Nil

@Test
def takeWhile06(): Bool = List.takeWhile(i -> i > 3, 5 :: 1 :: Nil) == 5 :: Nil

@Test
def takeWhile07(): Bool = List.takeWhile(i -> i > 3, 5 :: 8 :: Nil) == 5 :: 8 :: Nil

@Test
def takeWhile08(): Bool = List.takeWhile(i -> i > 3, 4 :: 6 :: -3 :: 11 :: -5 :: 1 :: 2 :: 16 :: 7 :: 1 :: 7 :: Nil) ==
                        4 :: 6 :: Nil

/////////////////////////////////////////////////////////////////////////////
// splitAt                                                                 //
/////////////////////////////////////////////////////////////////////////////

@Test
def splitAt01(): Bool =
    let xs = Nil: List[Int32];
    List.splitAt(0, xs) == (Nil, Nil)

@Test
def splitAt02(): Bool =
    let xs = 1 :: 5 :: 19 :: 3 :: Nil;
    List.splitAt(3, xs) == (1 :: 5 :: 19 :: Nil, 3 :: Nil)

@Test
def splitAt03(): Bool =
    let xs = 1 :: 5 :: 19 :: 3 :: Nil;
    List.splitAt(-3, xs) == (Nil, xs)

@Test
def splitAt04(): Bool =
    let xs = 1 :: 5 :: 19 :: 3 :: Nil;
    List.splitAt(5, xs) == (xs, Nil)

/////////////////////////////////////////////////////////////////////////////
// groupBy                                                                 //
/////////////////////////////////////////////////////////////////////////////
@Test
def groupBy01(): Bool = List.groupBy((a, b) -> a > 3 or b > 8, Nil: List[Int32]) == Nil

@Test
def groupBy02(): Bool = List.groupBy((a, b) -> a > 3 or b > 8, 1 :: Nil) == (1 :: Nil) :: Nil

@Test
def groupBy03(): Bool = List.groupBy((a, b) -> a > 3 or b > 8, 1 :: 4 :: Nil) == (1 :: Nil) :: (4 :: Nil) :: Nil

@Test
def groupBy04(): Bool = List.groupBy((a, b) -> a > 3 or b > 8, 1 :: 9 :: Nil) == (1 :: 9 :: Nil) :: Nil

@Test
def groupBy05(): Bool = List.groupBy((a, b) -> a > 3 or b > 8, 1 :: 4 :: 7 :: 6 :: 9 :: 2 :: 4 :: 4 :: 8 :: 16 :: Nil) ==
                      (1 :: 9 :: 16 :: Nil) :: (4 :: 7 :: 6 :: 4 :: 4 :: 8 :: Nil) :: (2 :: Nil) :: Nil

@Test
def groupBy06(): Bool = List.groupBy((a, b) -> a > -6 or a*b >= 0, -1 :: -11 :: 4 :: -11 :: 0 :: 8 :: 2 :: 1 :: -3 :: -24 :: Nil) ==
                      (-1 :: -11 :: -11 :: 0 :: -3 :: -24 :: Nil) :: (4 :: 8 :: 2 :: 1 :: Nil) :: Nil

@Test
def groupBy07(): Bool = List.groupBy((a, b) -> a < 0 or (a > 10 or (b > 10 or a == b)), -5 :: 6 :: 11 :: 8 :: 8 :: -11 :: -1 :: 0 :: 4 :: -1 :: Nil) ==
                      (-5 :: 11 :: -11 :: -1 :: -1 :: Nil) :: (6 :: Nil) :: (8 :: 8 :: Nil) :: (0 :: Nil) :: (4 :: Nil) :: Nil

/////////////////////////////////////////////////////////////////////////////
// zip                                                                     //
/////////////////////////////////////////////////////////////////////////////
@Test
def zip01(): Bool = List.zip(Nil: List[Unit], Nil: List[Unit]) == Nil

@Test
def zip02(): Bool = List.zip(1 :: Nil, Nil: List[Unit]) == Nil

@Test
def zip03(): Bool = List.zip(Nil: List[Unit], 2 :: Nil) == Nil

@Test
def zip04(): Bool = List.zip(1 :: Nil, 2 :: Nil) == (1, 2) :: Nil

@Test
def zip05(): Bool = List.zip(1 :: 3 :: Nil, 2 :: 4 :: Nil) == (1, 2) :: (3, 4) :: Nil

@Test
def zip06(): Bool = List.zip(1 :: 3 :: 5 :: Nil, 2 :: 4 :: 6 :: Nil) == (1, 2) :: (3, 4) :: (5, 6) :: Nil

@Test
def zip07(): Bool = List.zip(1 :: 3 :: 5 :: 7 :: Nil, 2 :: 4 :: 6 :: 8 :: Nil) == (1, 2) :: (3, 4) :: (5, 6) :: (7, 8) :: Nil

/////////////////////////////////////////////////////////////////////////////
// zipWith                                                                 //
/////////////////////////////////////////////////////////////////////////////
@Test
def zipWith01(): Bool = List.zipWith((a, b) -> if (b) a+1 else a, Nil, Nil) == Nil

@Test
def zipWith02(): Bool = List.zipWith((a, b) -> if (b) a+1 else a, 1 :: Nil, Nil) == Nil

@Test
def zipWith03(): Bool = List.zipWith((a, b) -> if (b) a+1 else a, Nil, true :: Nil) == Nil

@Test
def zipWith04(): Bool = List.zipWith((a, b) -> if (b) a+1 else a, 1 :: Nil, true :: Nil) == 2 :: Nil

@Test
def zipWith05(): Bool = List.zipWith((a, b) -> if (b) a+1 else a, 1 :: Nil, false :: Nil) == 1 :: Nil

@Test
def zipWith06(): Bool = List.zipWith((a, b) -> if (b) a+1 else a, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: Nil,
                      false :: true :: true :: false :: false :: true :: true :: true ::Nil) ==
                      1 :: 3 :: 4 :: 4 :: 5 :: 7 :: 8 :: 9 :: Nil

/////////////////////////////////////////////////////////////////////////////
// zipWithIndex                                                            //
/////////////////////////////////////////////////////////////////////////////
@Test
def zipWithIndex01(): Bool = List.zipWithIndex(Nil : List[Char]) == Nil

@Test
def zipWithIndex02(): Bool = List.zipWithIndex(1 :: Nil) == (0, 1) :: Nil

@Test
def zipWithIndex03(): Bool = List.zipWithIndex("a" :: "b" :: Nil) == (0, "a") :: (1, "b") :: Nil

@Test
def zipWithIndex04(): Bool = List.zipWithIndex(List.zipWithIndex(true :: false :: Nil)) == (0, (0, true)) :: (1, (1, false)) :: Nil

/////////////////////////////////////////////////////////////////////////////
// zipWithA                                                                //
/////////////////////////////////////////////////////////////////////////////
@Test
def zipWithA01(): Bool = List.zipWithA((a, b) -> {if (b) a+1 else a} |> Identity.Identity, Nil, Nil) == Identity.Identity(Nil)

@Test
def zipWithA02(): Bool = List.zipWithA((a, b) -> {if (b) a+1 else a} |> Identity.Identity, 1 :: Nil, Nil) == Identity.Identity(Nil)

@Test
def zipWithA03(): Bool = List.zipWithA((a, b) -> {if (b) a+1 else a} |> Identity.Identity, Nil, true :: Nil) == Identity.Identity(Nil)

@Test
def zipWithA04(): Bool = List.zipWithA((a, b) -> {if (b) a+1 else a} |> Identity.Identity, 1 :: Nil, true :: Nil) == Identity.Identity(2 :: Nil)

@Test
def zipWithA05(): Bool = List.zipWithA((a, b) -> {if (b) a+1 else a} |> Identity.Identity, 1 :: Nil, false :: Nil) == Identity.Identity(1 :: Nil)

@Test
def zipWithA06(): Bool = List.zipWithA((a, b) -> {if (b) a+1 else a} |> Identity.Identity, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: Nil,
                      false :: true :: true :: false :: false :: true :: true :: true :: Nil) ==
                      Identity.Identity(1 :: 3 :: 4 :: 4 :: 5 :: 7 :: 8 :: 9 :: Nil)


/////////////////////////////////////////////////////////////////////////////
// unzip                                                                   //
/////////////////////////////////////////////////////////////////////////////
@Test
def unzip01(): Bool = List.unzip(Nil: List[(Unit, Unit)]) == (Nil, Nil)

@Test
def unzip02(): Bool = List.unzip((1, true) :: Nil) == (1 :: Nil, true :: Nil)

@Test
def unzip03(): Bool = List.unzip((1, true) :: (2, true) :: Nil) == (1 :: 2 :: Nil, true :: true :: Nil)

@Test
def unzip04(): Bool = List.unzip((1, true) :: (2, true) :: (3, false) :: Nil) ==
                    (1 :: 2 :: 3 :: Nil, true :: true :: false :: Nil)

/////////////////////////////////////////////////////////////////////////////
// zip3                                                                    //
/////////////////////////////////////////////////////////////////////////////
@Test
def zip301(): Bool = List.zip3(Nil: List[Unit],Nil: List[Unit], Nil: List[Unit]) == Nil

@Test
def zip302(): Bool = List.zip3(1 :: Nil, Nil: List[Unit], Nil: List[Unit]) == Nil

@Test
def zip303(): Bool = List.zip3(Nil: List[Unit], 2 :: Nil, Nil: List[Unit]) == Nil

@Test
def zip304(): Bool = List.zip3(Nil: List[Unit], Nil: List[Unit], 3 :: Nil) == Nil

@Test
def zip305(): Bool = List.zip3(1 :: Nil, 2 :: Nil, 3 :: Nil) == (1, 2, 3) :: Nil

@Test
def zip306(): Bool = List.zip3(1 :: 4 :: Nil, 2 :: 5 :: Nil, 3 :: 6 :: Nil) == (1, 2, 3) :: (4, 5, 6) :: Nil

@Test
def zip307(): Bool = List.zip3(1 :: 4 :: 7 :: Nil, 2 :: 5 :: 8 :: Nil, 3 :: 6 :: 9 :: Nil) == (1, 2, 3) :: (4, 5, 6) :: (7, 8, 9) :: Nil

@Test
def zip308(): Bool = List.zip3(1 :: 4 :: 7 :: 10 :: Nil, 2 :: 5 :: 8 :: 11 :: Nil, 3 :: 6 :: 9 :: 12 :: Nil) == (1, 2, 3) :: (4, 5, 6) :: (7, 8, 9) :: (10, 11, 12) :: Nil

/////////////////////////////////////////////////////////////////////////////
// zipWith3                                                                //
/////////////////////////////////////////////////////////////////////////////
@Test
def zipWith301(): Bool = List.zipWith3((a, b, c) -> if (c) a else b, Nil: List[Int32], Nil, Nil) == Nil

@Test
def zipWith302(): Bool = List.zipWith3((a, b, c) -> if (c) a else b, 1 :: Nil, Nil, Nil) == Nil

@Test
def zipWith303(): Bool = List.zipWith3((a, b, c) -> if (c) a else b, Nil, 10 :: Nil, Nil) == Nil

@Test
def zipWith304(): Bool = List.zipWith3((a, b, c) -> if (c) a else b, Nil: List[Int32], Nil, true :: Nil) == Nil

@Test
def zipWith305(): Bool = List.zipWith3((a, b, c) -> if (c) a else b, 1 :: Nil, 10 :: Nil, true :: Nil) == 1 :: Nil

@Test
def zipWith306(): Bool = List.zipWith3((a, b, c) -> if (c) a else b, 1 :: Nil, 10 :: Nil, false :: Nil) == 10 :: Nil

@Test
def zipWith307(): Bool = List.zipWith3((a, b, c) -> if (c) a else b,
                            1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: Nil,
                            10 :: 11 :: 12 :: 13 :: 14 :: 15 :: 16 :: 17 :: Nil,
                            false :: true :: true :: false :: false :: true :: true :: true ::Nil) ==
                            10 :: 2 :: 3 :: 13 :: 14 :: 6 :: 7 :: 8 :: Nil

/////////////////////////////////////////////////////////////////////////////
// unzip3                                                                  //
/////////////////////////////////////////////////////////////////////////////
@Test
def unzip301(): Bool = List.unzip3(Nil: List[(Unit, Unit, Unit)]) == (Nil, Nil, Nil)

@Test
def unzip302(): Bool = List.unzip3((1, 'a', true) :: Nil) == (1 :: Nil, 'a' :: Nil, true :: Nil)

@Test
def unzip303(): Bool = List.unzip3((1, 'a', true) :: (2, 'b', true) :: Nil) == (1 :: 2 :: Nil, 'a' :: 'b' :: Nil, true :: true :: Nil)

@Test
def unzip304(): Bool = List.unzip3((1, 'a', true) :: (2, 'b', true) :: (3, 'c', false) :: Nil) ==
                    (1 :: 2 :: 3 :: Nil, 'a' :: 'b' :: 'c' :: Nil, true :: true :: false :: Nil)

/////////////////////////////////////////////////////////////////////////////
// fold2                                                                   //
/////////////////////////////////////////////////////////////////////////////
@Test
def fold201(): Bool = List.fold2((c, a, b) -> if (b) a+c else a*c, 4, Nil, Nil) == 4

@Test
def fold202(): Bool = List.fold2((c, a, b) -> if (b) a+c else a*c, 4, 1 :: Nil, Nil) == 4

@Test
def fold203(): Bool = List.fold2((c, a, b) -> if (b) a+c else a*c, 4, Nil, true :: Nil) == 4

@Test
def fold204(): Bool = List.fold2((c, a, b) -> if (b) a+c else a*c, 4, 2 :: Nil, true :: Nil) == 6

@Test
def fold205(): Bool = List.fold2((c, a, b) -> if (b) a+c else a*c, 4, 2 :: Nil, false :: Nil) == 8

@Test
def fold206(): Bool = List.fold2((c, a, b) -> if (b) a+c else a*c, 4, 3 :: 2 :: Nil, true :: true :: Nil) == 9

@Test
def fold207(): Bool = List.fold2((c, a, b) -> if (b) a+c else a*c, 4, 3 :: 2 :: Nil, true :: false :: Nil) == 14

@Test
def fold208(): Bool = List.fold2((c, a, b) -> if (b) a+c else a*c, 4, 3 :: 2 :: Nil, false :: true :: Nil) == 14

@Test
def fold209(): Bool = List.fold2((c, a, b) -> if (b) a+c else a*c, 4, 3 :: 2 :: Nil, false :: false :: Nil) == 24

@Test
def fold210(): Bool = List.fold2((c, a, b) -> if (b) a+c else a*c, 4, 6 :: -4 :: 3 :: 2 :: Nil,
                    true :: false :: false :: true :: Nil) == -118

/////////////////////////////////////////////////////////////////////////////
// foldLeft2                                                               //
/////////////////////////////////////////////////////////////////////////////
@Test
def foldLeft201(): Bool = List.foldLeft2((c, a, b) -> if (b) a+c else a*c, 4, Nil, Nil) == 4

@Test
def foldLeft202(): Bool = List.foldLeft2((c, a, b) -> if (b) a+c else a*c, 4, 1 :: Nil, Nil) == 4

@Test
def foldLeft203(): Bool = List.foldLeft2((c, a, b) -> if (b) a+c else a*c, 4, Nil, true :: Nil) == 4

@Test
def foldLeft204(): Bool = List.foldLeft2((c, a, b) -> if (b) a+c else a*c, 4, 2 :: Nil, true :: Nil) == 6

@Test
def foldLeft205(): Bool = List.foldLeft2((c, a, b) -> if (b) a+c else a*c, 4, 2 :: Nil, false :: Nil) == 8

@Test
def foldLeft206(): Bool = List.foldLeft2((c, a, b) -> if (b) a+c else a*c, 4, 3 :: 2 :: Nil, true :: true :: Nil) == 9

@Test
def foldLeft207(): Bool = List.foldLeft2((c, a, b) -> if (b) a+c else a*c, 4, 3 :: 2 :: Nil, true :: false :: Nil) == 14

@Test
def foldLeft208(): Bool = List.foldLeft2((c, a, b) -> if (b) a+c else a*c, 4, 3 :: 2 :: Nil, false :: true :: Nil) == 14

@Test
def foldLeft209(): Bool = List.foldLeft2((c, a, b) -> if (b) a+c else a*c, 4, 3 :: 2 :: Nil, false :: false :: Nil) == 24

@Test
def foldLeft210(): Bool = List.foldLeft2((c, a, b) -> if (b) a+c else a*c, 4, 6 :: -4 :: 3 :: 2 :: Nil,
                        true :: false :: false :: true :: Nil) == -118

/////////////////////////////////////////////////////////////////////////////
// foldRight2                                                              //
/////////////////////////////////////////////////////////////////////////////
@Test
def foldRight201(): Bool = List.foldRight2((a, b, c) -> if (b) a+c else a*c, 4, Nil, Nil) == 4

@Test
def foldRight202(): Bool = List.foldRight2((a, b, c) -> if (b) a+c else a*c, 4, 1 :: Nil, Nil) == 4

@Test
def foldRight203(): Bool = List.foldRight2((a, b, c) -> if (b) a+c else a*c, 4, Nil, true :: Nil) == 4

@Test
def foldRight204(): Bool = List.foldRight2((a, b, c) -> if (b) a+c else a*c, 4, 2 :: Nil, true :: Nil) == 6

@Test
def foldRight205(): Bool = List.foldRight2((a, b, c) -> if (b) a+c else a*c, 4, 2 :: Nil, false :: Nil) == 8

@Test
def foldRight206(): Bool = List.foldRight2((a, b, c) -> if (b) a+c else a*c, 4, 3 :: 2 :: Nil, true :: true :: Nil) == 9

@Test
def foldRight207(): Bool = List.foldRight2((a, b, c) -> if (b) a+c else a*c, 4, 3 :: 2 :: Nil, true :: false :: Nil) == 11

@Test
def foldRight208(): Bool = List.foldRight2((a, b, c) -> if (b) a+c else a*c, 4, 3 :: 2 :: Nil, false :: true :: Nil) == 18

@Test
def foldRight209(): Bool = List.foldRight2((a, b, c) -> if (b) a+c else a*c, 4, 3 :: 2 :: Nil, false :: false :: Nil) == 24

@Test
def foldRight210(): Bool = List.foldRight2((a, b, c) -> if (b) a+c else a*c, 4, 6 :: -4 :: 3 :: 2 :: Nil,
                         true :: false :: false :: true :: Nil) == -66

/////////////////////////////////////////////////////////////////////////////
// foldMap                                                                 //
/////////////////////////////////////////////////////////////////////////////

@Test
def foldMap01(): Bool =
    List.foldMap(x -> 2 * x, Nil) == 0

@Test
def foldMap02(): Bool =
    List.foldMap(x -> 2 * x, 1 :: 2 :: Nil) == 6

@Test
def foldMap03(): Bool =
    List.foldMap(x -> if (x == "a") "b" else x, "a" :: Nil) == "b"

@Test
def foldMap04(): Bool =
    List.foldMap(x -> if (x == "c") "b" else x, "a" :: "b" :: "c" :: Nil) == "abb"

@Test
def foldMap05(): Bool =
    List.foldMap(Int32.toString, 1 :: 2 :: 3 :: Nil) == "123"

/////////////////////////////////////////////////////////////////////////////
// filterMap                                                               //
/////////////////////////////////////////////////////////////////////////////
@Test
def filterMap01(): Bool = List.filterMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Nil) == Nil

@Test
def filterMap02(): Bool = List.filterMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, 1 :: Nil) == Nil

@Test
def filterMap03(): Bool = List.filterMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, 2 :: Nil) == 1 :: Nil

@Test
def filterMap04(): Bool = List.filterMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, 1 :: 3 :: Nil) == Nil

@Test
def filterMap05(): Bool = List.filterMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, 1 :: 4 :: Nil) == 2 :: Nil

@Test
def filterMap06(): Bool = List.filterMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, 6 :: -1 :: Nil) == 3 :: Nil

@Test
def filterMap07(): Bool = List.filterMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, 8 :: 6 :: Nil) == 4 :: 3 :: Nil

@Test
def filterMap08(): Bool = List.filterMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, 0 :: 1 :: 2 :: 3 :: 4 :: 5 :: 10 :: Nil) ==
                        0 :: 1 :: 2 :: 5 :: Nil

/////////////////////////////////////////////////////////////////////////////
// findMap                                                                 //
/////////////////////////////////////////////////////////////////////////////
@Test
def findMap01(): Bool = List.findMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, Nil) == None

@Test
def findMap02(): Bool = List.findMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, 1 :: Nil) == None

@Test
def findMap03(): Bool = List.findMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, 2 :: Nil) == Some(1)

@Test
def findMap04(): Bool = List.findMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, 1 :: 3 :: Nil) == None

@Test
def findMap05(): Bool = List.findMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, 1 :: 4 :: Nil) == Some(2)

@Test
def findMap06(): Bool = List.findMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, 6 :: -1 :: Nil) == Some(3)

@Test
def findMap07(): Bool = List.findMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, 8 :: 6 :: Nil) == Some(4)

@Test
def findMap08(): Bool = List.findMap(i -> if (i `Int32.rem` 2 == 0) Some(i/2) else None, 0 :: 1 :: 2 :: 3 :: 4 :: 5 :: 10 :: Nil) == Some(0)

/////////////////////////////////////////////////////////////////////////////
// toMutDeque                                                              //
/////////////////////////////////////////////////////////////////////////////

@Test
def toMutDeque01(): Bool = region rc {
    let l = Nil: List[Unit];
    let d = List.toMutDeque(rc, l);

    l == MutDeque.toList(d)
}

@Test
def toMutDeque02(): Bool = region rc {
    let l = 1 :: 2 :: Nil;
    let d = List.toMutDeque(rc, l);

    l == MutDeque.toList(d)
}

@Test
def toMutDeque03(): Bool = region rc {
    let l = 3 :: 1 :: 2 :: Nil;
    let d = List.toMutDeque(rc, l);

    l == MutDeque.toList(d)
}

@Test
def toMutDeque04(): Bool = region rc {
    let l = 'a' :: 'b' :: 'c' :: Nil;
    let d = List.toMutDeque(rc, l);

    l == MutDeque.toList(d)
}

/////////////////////////////////////////////////////////////////////////////
// toSet                                                                   //
/////////////////////////////////////////////////////////////////////////////
@Test
def toSet01(): Bool = List.toSet(Nil: List[Unit]) == Set#{}

@Test
def toSet02(): Bool = List.toSet(1 :: Nil) == Set#{1}

@Test
def toSet03(): Bool = List.toSet(1 :: 2 :: Nil) == Set#{1, 2}

@Test
def toSet04(): Bool = List.toSet(1 :: 1 :: Nil) == Set#{1}

@Test
def toSet05(): Bool = List.toSet(1 :: 2 :: 3 :: Nil) == Set#{1, 2, 3}

@Test
def toSet06(): Bool = List.toSet(1 :: 2 :: 1 :: Nil) == Set#{1, 2}

@Test
def toSet07(): Bool = List.toSet(1 :: 1 :: 2 :: Nil) == Set#{1, 2}

@Test
def toSet08(): Bool = List.toSet(2 :: 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 3 :: 3 :: Nil) == Set#{1, 2, 3, 4, 5, 6}

/////////////////////////////////////////////////////////////////////////////
// toMap                                                                   //
/////////////////////////////////////////////////////////////////////////////
@Test
def toMap01(): Bool = List.toMap(Nil: List[(Unit, Unit)]) == Map#{}

@Test
def toMap02(): Bool = List.toMap((1, true) :: Nil) == Map#{1 => true}

@Test
def toMap03(): Bool = List.toMap((1, true) :: (2, false) :: Nil) == Map#{1 => true, 2 => false}

@Test
def toMap04(): Bool = List.toMap((1, true) :: (1, false) :: Nil) == Map#{1 => true}

/////////////////////////////////////////////////////////////////////////////
// toMapWith                                                               //
/////////////////////////////////////////////////////////////////////////////

@Test
def toMapWith01(): Bool =
    List.toMapWith(x -> 2 * x, Nil) == Map#{}

@Test
def toMapWith02(): Bool =
    List.toMapWith(x -> 0 * x, 1 :: 3 :: 6 :: Nil) == Map#{1 => 0, 3 => 0, 6 => 0}

@Test
def toMapWith03(): Bool =
    List.toMapWith(x -> 2 * x, 1 :: 3 :: 6 :: Nil) == Map#{1 => 2, 3 => 6, 6 => 12}

@Test
def toMapWith04(): Bool =
    List.toMapWith(x -> x + 10, 1 :: 3 :: 6 :: Nil) == Map#{1 => 11, 3 => 13, 6 => 16}

@Test
def toMapWith05(): Bool =
    List.toMapWith(x -> x + 10, 3 :: 6 :: 1 :: Nil) == Map#{1 => 11, 3 => 13, 6 => 16}

/////////////////////////////////////////////////////////////////////////////
// forEach                                                                 //
/////////////////////////////////////////////////////////////////////////////

@Test
def forEach01(): Bool = region rc {
    let ri = ref 21 @ rc;
    List.forEach(x -> ri := x, Nil);
    21 == deref ri
}

@Test
def forEach02(): Bool = region rc {
    let ri = ref 21 @ rc;
    List.forEach(x -> ri := x, 1 :: 2 :: 3 :: Nil);
    3 == deref ri
}

/////////////////////////////////////////////////////////////////////////////
// forEachWithIndex                                                        //
/////////////////////////////////////////////////////////////////////////////

@Test
def forEachWithIndex01(): Bool = region rc {
    let ri = ref 21 @ rc;
    List.forEachWithIndex((i, _) -> ri := i, Nil);
    21 == deref ri
}

@Test
def forEachWithIndex02(): Bool = region rc {
    let ri = ref 21 @ rc;
    List.forEachWithIndex((i, _) -> ri := i, 1 :: 2 :: 3 :: Nil);
    2 == deref ri
}

/////////////////////////////////////////////////////////////////////////////
// toArray                                                                 //
/////////////////////////////////////////////////////////////////////////////

@Test
def toArray01(): Bool = region rc {
    let a = List.toArray(rc, Nil): Array[Int32, rc];
    Array.sameElements(a, Array#{} @ rc)
}

@Test
def toArray02(): Bool = region rc {
    let a = List.toArray(rc, 1 :: Nil);
    Array.sameElements(a, Array#{1} @ rc)
}

@Test
def toArray03(): Bool = region rc {
    let a = List.toArray(rc, 1 :: 2 :: Nil);
    Array.sameElements(a, Array#{1, 2} @ rc)
}

@Test
def toArray04(): Bool = region rc {
    let a = List.toArray(rc, 1 :: 2 :: 3 :: Nil);
    Array.sameElements(a, Array#{1, 2, 3} @ rc)
}


/////////////////////////////////////////////////////////////////////////////
// toVector                                                                //
/////////////////////////////////////////////////////////////////////////////

@Test
def toVector01(): Bool =
    (List.toVector(Nil) : Vector[Int32]) `Vector.equals` Vector.empty()

@Test
def toVector02(): Bool =
    List.toVector(0 :: Nil) `Vector.equals` Vector.singleton(0)

@Test
def toVector03(): Bool =
    List.toVector(1 :: 2 :: 3 :: Nil) `Vector.equals` Vector.range(1, 4)


/////////////////////////////////////////////////////////////////////////////
// toNel                                                                   //
/////////////////////////////////////////////////////////////////////////////

@Test
def toNel01(): Bool = List.toNel(Nil: List[Unit]) == None

@Test
def toNel02(): Bool = List.toNel(1 :: Nil) == Some(Nel.Nel(1, Nil))

@Test
def toNel03(): Bool = List.toNel(1 :: 2 :: Nil) == Some(Nel.Nel(1, 2 :: Nil))

@Test
def toNel04(): Bool = List.toNel(1 :: 1 :: Nil) == Some(Nel.Nel(1, 1 :: Nil))

@Test
def toNel05(): Bool = List.toNel(2 :: 1 :: Nil) == Some(Nel.Nel(2, 1 :: Nil))

/////////////////////////////////////////////////////////////////////////////
// toNec                                                                   //
/////////////////////////////////////////////////////////////////////////////

@Test
def toNec01(): Bool = List.toNec(Nil: List[Unit]) == None

@Test
def toNec02(): Bool = List.toNec(1 :: Nil) == Some(Nec.singleton(1))

@Test
def toNec03(): Bool = List.toNec(1 :: 2 :: Nil) == Some(Nec.cons(1, Nec.singleton(2)))

@Test
def toNec04(): Bool = List.toNec(1 :: 1 :: Nil) == Some(Nec.cons(1, Nec.singleton(1)))

@Test
def toNec05(): Bool = List.toNec(2 :: 1 :: Nil) == Some(Nec.cons(2, Nec.singleton(1)))

/////////////////////////////////////////////////////////////////////////////
// sortWith                                                                //
/////////////////////////////////////////////////////////////////////////////

def cmp(x: Int32, y: Int32): Comparison =
    if (x < y) Comparison.LessThan
    else if (x == y) Comparison.EqualTo
    else Comparison.GreaterThan

@Test
def sortWith01(): Bool =
    List.sortWith(cmp, Nil: List[Int32]) == Nil

@Test
def sortWith02(): Bool =
    List.sortWith(cmp, 0 :: Nil) == 0 :: Nil

@Test
def sortWith03(): Bool =
    List.sortWith(cmp, 0 :: 1 :: Nil) == 0 :: 1 :: Nil

@Test
def sortWith04(): Bool =
    List.sortWith(cmp, 1 :: 0 :: Nil) == 0 :: 1 :: Nil

@Test
def sortWith05(): Bool =
    List.sortWith(cmp, 1 :: 1 :: Nil) == 1 :: 1 :: Nil

@Test
def sortWith06(): Bool =
    List.sortWith(cmp, 0 :: 1 :: 2 :: 3 :: 4 :: 5 :: Nil) == 0 :: 1 :: 2 :: 3 :: 4 :: 5 :: Nil

@Test
def sortWith07(): Bool =
    List.sortWith(cmp, 5 :: 4 :: 3 :: 2 :: 1 :: 0 :: Nil) == 0 :: 1 :: 2 :: 3 :: 4 :: 5 :: Nil

@Test
def sortWith08(): Bool =
    List.sortWith(cmp, 5 :: 3 :: 0 :: 4 :: 1 :: 2 :: Nil) == 0 :: 1 :: 2 :: 3 :: 4 :: 5 :: Nil

@Test
def sortWith09(): Bool =
    List.sortWith(cmp, 2 :: 3 :: 0 :: 4 :: 1 :: 2 :: Nil) == 0 :: 1 :: 2 :: 2 :: 3 :: 4 :: Nil

@Test
def sortWith10(): Bool =
    List.sortWith(flip(cmp), 0 :: 1 :: 2 :: 3 :: 4 :: 5 :: Nil) == 5 :: 4 :: 3 :: 2 :: 1 :: 0 :: Nil

@Test
def sortWith11(): Bool =
    List.sortWith(flip(cmp), 5 :: 4 :: 3 :: 2 :: 1 :: 0 :: Nil) == 5 :: 4 :: 3 :: 2 :: 1 :: 0 :: Nil

@Test
def sortWith12(): Bool =
    List.sortWith(flip(cmp), 5 :: 3 :: 0 :: 4 :: 1 :: 2 :: Nil) == 5 :: 4 :: 3 :: 2 :: 1 :: 0 :: Nil

@Test
def sortWith13(): Bool =
    List.sortWith(flip(cmp), 2 :: 3 :: 0 :: 4 :: 1 :: 2 :: Nil) == 4 :: 3 :: 2 :: 2 :: 1 :: 0 :: Nil

/////////////////////////////////////////////////////////////////////////////
// sort                                                                    //
/////////////////////////////////////////////////////////////////////////////

def testSortVsSortWith(xs: List[Int32]) : Bool =
    List.sort(xs) == List.sortWith(cmp, xs)

@Test
def sort01(): Bool = testSortVsSortWith(Nil: List[Int32])

@Test
def sort02(): Bool = testSortVsSortWith(0 :: Nil)

@Test
def sort03(): Bool = testSortVsSortWith(0 :: 1 :: Nil)

@Test
def sort04(): Bool = testSortVsSortWith(1 :: 0 :: Nil)

@Test
def sort05(): Bool = testSortVsSortWith(1 :: 1 :: Nil)

@Test
def sort06(): Bool = testSortVsSortWith(0 :: 1 :: 2 :: 3 :: 4 :: 5 :: Nil)

@Test
def sort07(): Bool = testSortVsSortWith(5 :: 4 :: 3 :: 2 :: 1 :: 0 :: Nil)

@Test
def sort08(): Bool = testSortVsSortWith(5 :: 3 :: 0 :: 4 :: 1 :: 2 :: Nil)

@Test
def sort09(): Bool = testSortVsSortWith(2 :: 3 :: 0 :: 4 :: 1 :: 2 :: Nil)

/////////////////////////////////////////////////////////////////////////////
// sortBy                                                                  //
/////////////////////////////////////////////////////////////////////////////

def testSortByVsSort(a: List[Int32]) : Bool =
    (List.sortBy(identity, a) == List.sort(a)) and
    (List.sortBy(x -> 4*x+7, a) == List.sort(a)) and
    (List.sortBy(x -> -x, a) == List.sortWith(flip(cmp),a))

@Test
def sortBy01(): Bool = testSortByVsSort(Nil: List[Int32])

@Test
def sortBy02(): Bool = testSortByVsSort(0 :: Nil)

@Test
def sortBy03(): Bool = testSortByVsSort(0 :: 1 :: Nil)

@Test
def sortBy04(): Bool = testSortByVsSort(1 :: 0 :: Nil)

@Test
def sortBy05(): Bool = testSortByVsSort(1 :: 1 :: Nil)

@Test
def sortBy06(): Bool = testSortByVsSort(0 :: 1 :: 2 :: 3 :: 4 :: 5 :: Nil)

@Test
def sortBy07(): Bool = testSortByVsSort(5 :: 4 :: 3 :: 2 :: 1 :: 0 :: Nil)

@Test
def sortBy08(): Bool = testSortByVsSort(5 :: 3 :: 0 :: 4 :: 1 :: 2 :: Nil)

@Test
def sortBy09(): Bool = testSortByVsSort(2 :: 3 :: 0 :: 4 :: 1 :: 2 :: Nil)


/////////////////////////////////////////////////////////////////////////////
// unfold                                                                  //
/////////////////////////////////////////////////////////////////////////////

@Test
def unfold01(): Bool =
    List.unfold(s -> if (true) None else Some(s + 48, s + 1), 0) == Nil

@Test
def unfold02(): Bool =
    List.unfold(s -> if (s > 0) None else Some(s + 48, s + 1), 0) == 48 :: Nil

@Test
def unfold03(): Bool =
    List.unfold(s -> if (s > 1) None else Some(s + 48, s + 1), 0) == 48 :: 49 :: Nil

@Test
def unfold04(): Bool =
    List.unfold(s -> if (s >= 10) None else Some(s + 48, s + 1), 0) == 48 :: 49 :: 50 :: 51 :: 52 :: 53 :: 54 :: 55 :: 56 :: 57 :: Nil

@Test
def unfold05(): Bool =
    List.unfold(s -> if (s >= 10) None else Some(s + 48, s + 1), 5) == 53 :: 54 :: 55 :: 56 :: 57 :: Nil

@Test
def unfold06(): Bool =
    List.unfold(s -> if (s >= 10) None else Some(s + 48, s + 2), 0) == 48 :: 50 :: 52 :: 54 :: 56 :: Nil

/////////////////////////////////////////////////////////////////////////////
// unfoldWithIter                                                          //
/////////////////////////////////////////////////////////////////////////////

@Test
def unfoldWithIter01(): Bool = region rc {
    let x = ref 0 @ rc;
    let step = () ->
        if (true)
            None
        else {
            let c = Some(deref x + 48);
            x := deref x + 1;
            c
        };
    List.unfoldWithIter(step) == Nil
}

@Test
def unfoldWithIter02(): Bool = region rc {
    let x = ref 0 @ rc;
    let step = () ->
        if (deref x > 0)
            None
        else {
            let c = Some(deref x + 48);
            x := deref x + 1;
            c
        };
    List.unfoldWithIter(step) == 48 :: Nil
}

@Test
def unfoldWithIter03(): Bool = region rc {
    let x = ref 0 @ rc;
    let step = () ->
        if (deref x > 1)
            None
        else {
            let c = Some(deref x + 48);
            x := deref x + 1;
            c
        };
    List.unfoldWithIter(step) == 48 :: 49 :: Nil
}

@Test
def unfoldWithIter04(): Bool = region rc {
    let x = ref 0 @ rc;
    let step = () ->
        if (deref x >= 10)
            None
        else {
            let c = Some(deref x + 48);
            x := deref x + 1;
            c
        };
    List.unfoldWithIter(step) == 48 :: 49 :: 50 :: 51 :: 52 :: 53 :: 54 :: 55 :: 56 :: 57 :: Nil
}

@Test
def unfoldWithIter05(): Bool = region rc {
    let x = ref 5 @ rc;
    let step = () ->
        if (deref x >= 10)
            None
        else {
            let c = Some(deref x + 48);
            x := deref x + 1;
            c
        };
    List.unfoldWithIter(step) == 53 :: 54 :: 55 :: 56 :: 57 :: Nil
}

@Test
def unfoldWithIter06(): Bool = region rc {
    let x = ref 0 @ rc;
    let step = () ->
        if (deref x >= 10)
            None
        else {
            let c = Some(deref x + 48);
            x := deref x + 2;
            c
        };
    List.unfoldWithIter(step) == 48 :: 50 :: 52 :: 54 :: 56 :: Nil
}

/////////////////////////////////////////////////////////////////////////////
// unfoldWithOkIter                                                        //
/////////////////////////////////////////////////////////////////////////////

@Test
def unfoldWithOkIter01(): Bool = region rc {
    let x = ref 0 @ rc;
    let step = () ->
        if (true) {
            Ok(None)
        } else {
            Ok(Some(deref x))
        };
    List.unfoldWithOkIter(step): Result[Unit, _] == Ok(Nil)
}

@Test
def unfoldWithOkIter02(): Bool = region rc {
    let x = ref 0 @ rc;
    let step = () ->
        if (true) {
            Err(deref x)
        } else {
            Ok(Some(deref x))
        };
    List.unfoldWithOkIter(step) == Err(0)
}

@Test
def unfoldWithOkIter03(): Bool = region rc {
    let x = ref 1 @ rc;
    let step = () ->
        if (deref x `Int32.rem` 5 > 0) {
            let c = deref x;
            x := deref x + 1;
            Ok(Some(c))
        } else {
            Ok(None)
        };
    List.unfoldWithOkIter(step): Result[Unit, _] == Ok(1 :: 2 :: 3 :: 4 :: Nil)
}

/////////////////////////////////////////////////////////////////////////////
// distinct                                                                //
/////////////////////////////////////////////////////////////////////////////

@Test
def distinct01(): Bool =
    List.distinct(Nil: List[Unit]) == Nil

@Test
def distinct02(): Bool =
    List.distinct(1 :: Nil) == 1 :: Nil

@Test
def distinct03(): Bool =
    List.distinct(1 :: 2 :: Nil) == 1 :: 2 :: Nil

@Test
def distinct04(): Bool =
    List.distinct(1 :: 1 :: 2 :: Nil) == 1 :: 2 :: Nil

@Test
def distinct05(): Bool =
    List.distinct(1 :: 2 :: 1 :: Nil) == 1 :: 2 :: Nil

@Test
def distinct06(): Bool =
    List.distinct(1 :: 2 :: 2 :: Nil) == 1 :: 2 :: Nil

@Test
def distinct07(): Bool =
    List.distinct(1 :: 2 :: 2 :: 3 :: Nil) == 1 :: 2 :: 3 :: Nil

@Test
def distinct08(): Bool =
    List.distinct(1 :: 2 :: 2 :: 3 :: 1 :: 4 :: Nil) == 1 :: 2 :: 3 :: 4 :: Nil

@Test
def distinct09(): Bool =
    List.distinct(4 :: 4 :: 3  :: 4 :: 2 :: 1 :: 1 :: Nil) == 4 :: 3 :: 2 :: 1 :: Nil

/////////////////////////////////////////////////////////////////////////////
// distinctWith                                                            //
/////////////////////////////////////////////////////////////////////////////

@Test
def distinctWith01(): Bool =
    List.distinctWith((x,y) -> x == y, Nil: List[Unit]) == Nil

@Test
def distinctWith02(): Bool =
    List.distinctWith((x,y) -> x == y, 1 :: Nil) == 1 :: Nil

@Test
def distinctWith03(): Bool =
    List.distinctWith((x,y) -> x == y, 1 :: 2 :: Nil) == 1 :: 2 :: Nil

@Test
def distinctWith04(): Bool =
    List.distinctWith((x,y) -> x == y, 1 :: 1 :: 2 :: Nil) == 1 :: 2 :: Nil

@Test
def distinctWith05(): Bool =
    List.distinctWith((x,y) -> x == y, 1 :: 2 :: 1 :: Nil) == 1 :: 2 :: Nil

@Test
def distinctWith06(): Bool =
    List.distinctWith((x,y) -> x == y, 1 :: 2 :: 2 :: Nil) == 1 :: 2 :: Nil

@Test
def distinctWith07(): Bool =
    List.distinctWith((x,y) -> x == y, 1 :: 2 :: 2 :: 3 :: Nil) == 1 :: 2 :: 3 :: Nil

@Test
def distinctWith08(): Bool =
    List.distinctWith((x,y) -> x == y, 1 :: 2 :: 2 :: 3 :: 1 :: 4 :: Nil) == 1 :: 2 :: 3 :: 4 :: Nil

@Test
def distinctWith09(): Bool =
    List.distinctWith((x,y) -> x == y, 4 :: 4 :: 3  :: 4 :: 2 :: 1 :: 1 :: Nil) == 4 :: 3 :: 2 :: 1 :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // toString                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toString01(): Bool =
        toString(1 :: Nil) == "1 :: Nil"

    @Test
    def toString02(): Bool =
        toString(1 :: 2 :: Nil) == "1 :: 2 :: Nil"

    @Test
    def toString03(): Bool =
        toString(93 :: 3 :: 4 :: Nil) == "93 :: 3 :: 4 :: Nil"

    @Test
    def toString04(): Bool =
        toString('a' :: 'b' :: 'c' :: Nil) == "a :: b :: c :: Nil"

    @Test
    def toString05(): Bool =
        toString(true :: false :: true :: true :: Nil) == "true :: false :: true :: true :: Nil"

    @Test
    def toString06(): Bool =
        toString((1 :: 2 :: Nil) :: (2 :: 3 :: Nil) :: (4 :: 7 :: Nil) :: Nil) == "1 :: 2 :: Nil :: 2 :: 3 :: Nil :: 4 :: 7 :: Nil :: Nil"

    /////////////////////////////////////////////////////////////////////////////
    // hash                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def hash01(): Bool = hash(Nil: List[Unit]) == hash(Nil: List[Unit])

    @Test
    def hash02(): Bool = hash(1 :: Nil) == hash(1 :: Nil)

    @Test
    def hash03(): Bool = hash(1 :: 2 :: Nil) == hash(1 :: 2 :: Nil)

    @Test
    def hash04(): Bool = hash(2 :: 1 :: Nil) != hash(1 :: 2 :: Nil)

    @Test
    def hash05(): Bool = hash("c" :: "b" :: "a" :: Nil) != hash("c" :: "a" :: "b" :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // mimimumBy                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def minimumBy01(): Bool = List.minimumBy((x, y) -> x <=> y, Nil: List[Int32]) == None

    @Test
    def minimumBy02(): Bool = List.minimumBy((x, y) -> x <=> y, 1 :: Nil) == Some(1)

    @Test
    def minimumBy03(): Bool = List.minimumBy((x, y) -> x <=> y, 3 :: 2 :: 1 :: Nil) == Some(1)

    @Test
    def minimumBy04(): Bool = List.minimumBy((x, y) -> x <=> y, 0 :: 3 :: 2 :: 1 :: Nil) == Some(0)

    @Test
    def minimumBy05(): Bool = List.minimumBy((_, _) -> Comparison.LessThan, List.range(0, 99)) == Some(0)

    /////////////////////////////////////////////////////////////////////////////
    // maximumBy                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def maximumBy01(): Bool = List.maximumBy((x, y) -> x <=> y, Nil: List[Int32]) == None

    @Test
    def maximumBy02(): Bool = List.maximumBy((x, y) -> x <=> y, 1 :: Nil) == Some(1)

    @Test
    def maximumBy03(): Bool = List.maximumBy((x, y) -> x <=> y, 3 :: 2 :: 1 :: Nil) == Some(3)

    @Test
    def maximumBy04(): Bool = List.maximumBy((x, y) -> x <=> y, 0 :: 3 :: 2 :: 1 :: Nil) == Some(3)

    @Test
    def maximumBy05(): Bool = List.maximumBy((_, _) -> Comparison.LessThan, List.range(0, 99)) == Some(98)


    /////////////////////////////////////////////////////////////////////////////
    // toDelayList                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toDelayList01(): Bool = Nil: List[Unit] |> List.toDelayList == DelayList.empty()

    @Test
    def toDelayList02(): Bool = List.range(-1000, 1000) |> List.toDelayList == DelayList.range(-1000, 1000)

    @Test
    def toDelayList03(): Bool = List.range(-1000, 1000) |> List.toDelayList |> DelayList.toList == List.range(-1000, 1000)

    /////////////////////////////////////////////////////////////////////////////
    // toChain                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toChain01(): Bool =
        List.toChain(Nil: List[Int32]) == Chain.empty(): Chain[Int32]

    @Test
    def toChain02(): Bool =
        List.toChain(1 :: Nil) == Chain.singleton(1)

    @Test
    def toChain03(): Bool =
        List.toChain(1 :: 2 :: Nil) == (1 `Chain.cons` (2 `Chain.cons` Chain.empty()))

    @Test
    def toChain04(): Bool =
        List.toChain(1 :: 2 :: 3 :: Nil) == (1 `Chain.cons` (2 `Chain.cons` (3 `Chain.cons` Chain.empty())))


    /////////////////////////////////////////////////////////////////////////////
    // sum                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sum01(): Bool =
        List.sum(Nil) == 0

    @Test
    def sum02(): Bool =
        List.sum(1 :: Nil) == 1

    @Test
    def sum03(): Bool =
        List.sum(1 :: 2 :: 3 :: Nil) == 6

    @Test
    def sum04(): Bool =
        List.sum(1 :: 2 :: 3 :: -3 :: Nil) == 3

    @Test
    def sum05(): Bool =
        List.sum(-1 :: -2 :: -3 :: -4 :: Nil) == -10

    @Test
    def sum06(): Bool =
        List.sum(10 :: -10 :: Nil) == 0

    @Test
    def sum07(): Bool =
        List.range(1, 101) |> List.sum == 5050


    /////////////////////////////////////////////////////////////////////////////
    // sumWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sumWith01(): Bool =
        List.sumWith(x -> x + 1, Nil: List[Int32]) == 0

    @Test
    def sumWith02(): Bool =
        List.sumWith(x -> x + 1, 1 :: Nil) == 2

    @Test
    def sumWith03(): Bool =
        List.sumWith(x -> x + 1, 1 :: 2 :: 3 :: Nil) == 9

    @Test
    def sumWith04(): Bool =
        List.sumWith(x -> x + 1, 1 :: 2 :: 3 :: -3 :: Nil) == 7

    @Test
    def sumWith05(): Bool =
        List.sumWith(x -> x + 1, -1 :: -2 :: -3 :: -4 :: Nil) == -6

    @Test
    def sumWith06(): Bool =
        List.sumWith(x -> x + 1, 10 :: -10 :: Nil) == 2


    /////////////////////////////////////////////////////////////////////////////
    // iterator                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def iterator01(): Bool = region rc {
        Nil: List[Int32] |> List.iterator(rc) |> Iterator.toList == Nil
    }

    @Test
    def iterator02(): Bool = region rc {
        (1 :: 2 :: 3 :: Nil) |> List.iterator(rc) |> Iterator.toList == 1 :: 2 :: 3 :: Nil
    }

    @Test
    def iterator03(): Bool = region rc {
        List.range(-100, 100) |> List.iterator(rc) |> Iterator.toList == List.range(-100, 100)
    }


    /////////////////////////////////////////////////////////////////////////////
    // enumerate                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def enumerate01(): Bool = region rc {
        let z = ref Nil @ rc;
        let l = Nil;
        Iterator.forEach(match (i, x) ->  z := (i, x) :: deref z, List.enumerator(rc, l));
        List.reverse(deref z) == Nil: List[(Int32, Int32)]
    }

    @Test
    def enumerate02(): Bool = region rc {
        let z = ref Nil @ rc;
        let l = 1 :: 2 :: 3 :: Nil;
        Iterator.forEach(match (i, x) ->  z := (i, x) :: deref z, List.enumerator(rc, l));
        List.reverse(deref z) == (0, 1) :: (1, 2) :: (2, 3) :: Nil
    }

    @Test
    def enumerate03(): Bool = region rc {
        let z = ref Nil @ rc;
        let l = List.range(100, 200);
        Iterator.forEach(match (i, x) -> z := (i, x) :: deref z, List.enumerator(rc, l));
        List.reverse(deref z) == List.zip(List.range(0, 100), l)
    }

    /////////////////////////////////////////////////////////////////////////////
    // sequence                                                                //
    /////////////////////////////////////////////////////////////////////////////

    /// TODO - this does not test order of effects.
    /// We need a more "effective" monad than Identity.Identity to show that effects are
    /// sequenced in left-to-right order.
    /// This applies to the `sequence` tests on the other container types as well.

    @Test
    def sequence01(): Bool =
        let l: List[Identity[Int32]] = Nil;
        List.sequence(l) == Identity.Identity(Nil)

    @Test
    def sequence02(): Bool =
        let l = Identity.Identity(1) :: Nil;
        List.sequence(l) == Identity.Identity(1 :: Nil)

    @Test
    def sequence03(): Bool =
        let l = Identity.Identity(1) :: Identity.Identity(2) :: Nil;
        List.sequence(l) == Identity.Identity(1 :: 2 :: Nil)

    @Test
    def sequence04(): Bool =
        let l = Identity.Identity(1) :: Identity.Identity(2) :: Identity.Identity(3) :: Nil;
        List.sequence(l) == Identity.Identity(1 :: 2 :: 3 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // traverse                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def traverse01(): Bool = region rc {
        let st = ref 0 @ rc;
        let l = Nil;
        let ans = List.traverse(x -> {st := x; Identity.Identity(x)}, l);
        ans == Identity.Identity(Nil) and deref st == 0
    }

    @Test
    def traverse02(): Bool = region rc {
        let st = ref 0 @ rc;
        let l = 1 :: Nil;
        let ans = List.traverse(x -> {st := x; Identity.Identity(x)}, l);
        ans == Identity.Identity(1 :: Nil) and deref st == 1
    }

    @Test
    def traverse03(): Bool = region rc {
        let st = ref 0 @ rc;
        let l = 1 :: 2 :: Nil;
        let ans = List.traverse(x -> {st := x; Identity.Identity(x)}, l);
        ans == Identity.Identity(1 :: 2 :: Nil) and deref st == 2
    }

    @Test
    def traverse04(): Bool = region rc {
        let st = ref 0 @ rc;
        let l = 1 :: 2 :: 3 :: Nil;
        let ans = List.traverse(x -> {st := x; Identity.Identity(x)}, l);
        ans == Identity.Identity(1 :: 2 :: 3 :: Nil) and deref st == 3
    }

    /////////////////////////////////////////////////////////////////////////////
    // merge                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def merge01(): Bool =
        let l1 = Nil: List[Int32];
        let l2 = Nil: List[Int32];
        List.merge(l1, l2) == Nil

    @Test
    def merge02(): Bool =
        let l1 = 1 :: Nil;
        let l2 = Nil: List[Int32];
        List.merge(l1, l2) == 1 :: Nil

    @Test
    def merge03(): Bool =
        let l1 = 1 :: Nil;
        let l2 = 2 :: Nil;
        List.merge(l1, l2) == 1 :: 2 :: Nil

    @Test
    def merge04(): Bool =
        let l1 = 2 :: Nil;
        let l2 = 1 :: Nil;
        List.merge(l1, l2) == 1 :: 2 :: Nil

    @Test
    def merge05(): Bool =
        let l1 = 1 :: 4 :: 8 :: 10 :: Nil;
        let l2 = 2 :: 3 :: 8 :: 9 :: Nil;
        List.merge(l1, l2) == 1 :: 2 :: 3 :: 4 :: 8 :: 8 :: 9 :: 10 :: Nil

    @Test
    def merge06(): Bool =
        let l1 = 4 :: 5 :: Nil;
        let l2 = 2 :: 3 :: 6 :: 7 :: Nil;
        List.merge(l1, l2) == 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // shuffle                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def shuffle01(): Bool \ NonDet =
        let l1: List[Int32] = Nil;
        let l2 = List.shuffle(Random.new(), l1);

        List.length(l2) == 0 and List.toSet(l2) == Set#{}

    @Test
    def shuffle02(): Bool \ NonDet =
        let l1 = 1 :: Nil;
        let l2 = List.shuffle(Random.new(), l1);

        List.length(l2) == 1 and List.toSet(l2) == Set#{1}

    @Test
    def shuffle03(): Bool \ NonDet =
        let l1 = 1 :: 2 :: 3 :: Nil;
        let l2 = List.shuffle(Random.new(), l1);

        List.length(l2) == 3 and List.toSet(l2) == Set#{1, 2, 3}

    @Test
    def shuffle04(): Bool \ NonDet =
        let l1 = 0 :: 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: 9 :: Nil;
        let l2 = List.shuffle(Random.new(), l1);

        List.length(l2) == 10 and List.toSet(l2) == Set#{0, 1, 2, 3, 4, 5, 6, 7, 8, 9}

/////////////////////////////////////////////////////////////////////////////
// Literals                                                                //
/////////////////////////////////////////////////////////////////////////////

@Test
def literal01(): Bool =
    List#{} == (Nil: List[String])

@Test
def literal02(): Bool =
    List#{1} == 1 :: Nil

@Test
def literal03(): Bool =
    List#{1, 2, 3} == 1 :: 2 :: 3 :: Nil

@Test
def literal04(): Bool =
    List#{'a', 'b', 'c'} == 'a' :: 'b' :: 'c' :: Nil

@Test
def literal05(): Bool =
    List#{List#{1, 2}, List#{3}, List#{4, 5}} == (1 :: 2 :: Nil) :: (3 :: Nil) :: (4 :: 5 :: Nil) :: Nil

@Test
def literal06(): Bool =
    not List.isEmpty(List#{{}})

}

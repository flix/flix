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

mod TestOption {

    use Hash.hash

/////////////////////////////////////////////////////////////////////////////
// isEmpty                                                                 //
/////////////////////////////////////////////////////////////////////////////
@Test
def isEmpty01(): Bool = Option.isEmpty(None) == true

@Test
def isEmpty02(): Bool = Option.isEmpty(Some(0)) == false

@Test
def isEmpty03(): Bool = Option.isEmpty(Some(22ii)) == false

/////////////////////////////////////////////////////////////////////////////
// getWithDefault                                                          //
/////////////////////////////////////////////////////////////////////////////
@Test
def getWithDefault01(): Bool = Option.getWithDefault(32, None) == 32

@Test
def getWithDefault02(): Bool = Option.getWithDefault(32, Some(30)) == 30

@Test
def getWithDefault03(): Bool = Option.getWithDefault(32ii, None) == 32ii

@Test
def getWithDefault04(): Bool = Option.getWithDefault(32ii, Some(30ii)) == 30ii

/////////////////////////////////////////////////////////////////////////////
// exists                                                                  //
/////////////////////////////////////////////////////////////////////////////
@Test
def exists01(): Bool = Option.exists(i -> i == 2, None) == false

@Test
def exists02(): Bool = Option.exists(i -> i == 2, Some(1)) == false

@Test
def exists03(): Bool = Option.exists(i -> i == 2, Some(2)) == true

/////////////////////////////////////////////////////////////////////////////
// forAll                                                                  //
/////////////////////////////////////////////////////////////////////////////
@Test
def forAll01(): Bool = Option.forAll(i -> i == 2, None) == true

@Test
def forAll02(): Bool = Option.forAll(i -> i == 2, Some(1)) == false

@Test
def forAll03(): Bool = Option.forAll(i -> i == 2, Some(2)) == true

/////////////////////////////////////////////////////////////////////////////
// filter                                                                  //
/////////////////////////////////////////////////////////////////////////////
@Test
def filter01(): Bool = Option.filter(i -> i == 2, None) == None

@Test
def filter02(): Bool = Option.filter(i -> i == 2, Some(1)) == None

@Test
def filter03(): Bool = Option.filter(i -> i == 2, Some(2)) == Some(2)

/////////////////////////////////////////////////////////////////////////////
// map                                                                     //
/////////////////////////////////////////////////////////////////////////////
@Test
def map01(): Bool = Option.map(i -> 2*i, None) == None

@Test
def map02(): Bool = Option.map(i -> 2*i, Some(1)) == Some(2)

@Test
def map03(): Bool = Option.map(i -> 2*i, Some(2)) == Some(4)

@Test
def map04(): Bool = region rc {
    let n = ref 2 @ rc;
    discard Option.map(i -> n := i, Some(4));
    4 == deref n
}

/////////////////////////////////////////////////////////////////////////////
// toList                                                                  //
/////////////////////////////////////////////////////////////////////////////
@Test
def toList01(): Bool = Option.toList(None: Option[Unit]) == Nil

@Test
def toList02(): Bool = Option.toList(Some(1)) == 1 :: Nil

/////////////////////////////////////////////////////////////////////////////
// toSet                                                                   //
/////////////////////////////////////////////////////////////////////////////
@Test
def toSet01(): Bool = Option.toSet(None: Option[Unit]) == Set#{}

@Test
def toSet02(): Bool = Option.toSet(Some(1)) == Set#{1}

/////////////////////////////////////////////////////////////////////////////
// toMap                                                                   //
/////////////////////////////////////////////////////////////////////////////
@Test
def toMap01(): Bool = Option.toMap(None: Option[(Unit, Unit)]) == Map#{}

@Test
def toMap02(): Bool = Option.toMap(Some((1, true))) == Map#{1 => true}

/////////////////////////////////////////////////////////////////////////////
// toMapWith                                                               //
/////////////////////////////////////////////////////////////////////////////

@Test
def toMapWith01(): Bool =
    Option.toMapWith(x -> 0 * x, None) == Map#{}

@Test
def toMapWith02(): Bool =
    Option.toMapWith(x -> 0 * x, Some(1)) == Map#{1 => 0}

@Test
def toMapWith03(): Bool =
    Option.toMapWith(x -> 2 * x, Some(1)) == Map#{1 => 2}

@Test
def toMapWith04(): Bool =
    Option.toMapWith(x -> x + 10, Some(1)) == Map#{1 => 11}

@Test
def toMapWith05(): Bool =
    Option.toMapWith(x -> x + "b", Some("a")) == Map#{"a" => "ab"}

/////////////////////////////////////////////////////////////////////////////
// toOk                                                                   //
/////////////////////////////////////////////////////////////////////////////
@Test
def toOk01(): Bool = Option.toOk("err", None: Option[Unit]) == Err("err")

@Test
def toOk02(): Bool = Option.toOk("err", Some(1)) == Ok(1)

/////////////////////////////////////////////////////////////////////////////
// toErr                                                                   //
/////////////////////////////////////////////////////////////////////////////
@Test
def toErr01(): Bool = Option.toErr("default", None: Option[Unit]) == Ok("default")

@Test
def toErr02(): Bool = Option.toErr("default", Some("err")) == Err("err")

/////////////////////////////////////////////////////////////////////////////
// toSuccess                                                               //
/////////////////////////////////////////////////////////////////////////////
@Test
def toSuccess01(): Bool = Option.toSuccess("err", None: Option[Unit]) == Validation.Failure(Nec.singleton("err"))

@Test
def toSuccess02(): Bool = Option.toSuccess("err", Some(1)) == Validation.Success(1)

/////////////////////////////////////////////////////////////////////////////
// toFailure                                                               //
/////////////////////////////////////////////////////////////////////////////
@Test
def toFailure01(): Bool = Option.toFailure("default", None: Option[Unit]) == Validation.Success("default")

@Test
def toFailure02(): Bool = Option.toFailure("default", Some("err")) == Validation.Failure(Nec.singleton("err"))

/////////////////////////////////////////////////////////////////////////////
// withDefault                                                             //
/////////////////////////////////////////////////////////////////////////////
@Test
def withDefault01(): Bool = Option.withDefault(default = None, None) == None: Option[Int32]

@Test
def withDefault02(): Bool = Option.withDefault(default = Some(1), None) == Some(1)

@Test
def withDefault03(): Bool = Option.withDefault(default = None, Some(1)) == Some(1)

@Test
def withDefault04(): Bool = Option.withDefault(default = Some(2), Some(1)) == Some(1)

/////////////////////////////////////////////////////////////////////////////
// replace                                                                 //
/////////////////////////////////////////////////////////////////////////////
@Test
def replace01(): Bool = Option.replace(from = 3, to = 4, None) == None

@Test
def replace02(): Bool = Option.replace(from = 3, to = 4, Some(2)) == Some(2)

@Test
def replace03(): Bool = Option.replace(from = 3, to = 4, Some(3)) == Some(4)

@Test
def replace04(): Bool = Option.replace(from = 3, to = 4, Some(4)) == Some(4)

/////////////////////////////////////////////////////////////////////////////
// count                                                                   //
/////////////////////////////////////////////////////////////////////////////
@Test
def count01(): Bool = Option.count(i -> i == 2, None) == 0

@Test
def count02(): Bool = Option.count(i -> i == 2, Some(1)) == 0

@Test
def count03(): Bool = Option.count(i -> i == 2, Some(2)) == 1


    /////////////////////////////////////////////////////////////////////////////
    // sum                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sum01(): Bool = Option.sum(None) == 0

    @Test
    def sum02(): Bool = Option.sum(Some(1)) == 1

    @Test
    def sum03(): Bool = Option.sum(Some(-2)) == -2


    /////////////////////////////////////////////////////////////////////////////
    // sumWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sumWith01(): Bool = Option.sumWith(x -> x + 1, None) == 0

    @Test
    def sumWith02(): Bool = Option.sumWith(x -> x + 1, Some(1)) == 2

    @Test
    def sumWith03(): Bool = Option.sumWith(x -> x + 1, Some(-2)) == -1


/////////////////////////////////////////////////////////////////////////////
// find                                                                    //
/////////////////////////////////////////////////////////////////////////////
@Test
def find01(): Bool = Option.find(i -> i == 2, None) == None

@Test
def find02(): Bool = Option.find(i -> i == 2, Some(1)) == None

@Test
def find03(): Bool = Option.find(i -> i == 2, Some(2)) == Some(2)

/////////////////////////////////////////////////////////////////////////////
// flatten                                                                 //
/////////////////////////////////////////////////////////////////////////////
@Test
def flatten01(): Bool = Option.flatten(None): Option[Unit] == None

@Test
def flatten02(): Bool = Option.flatten(Some(None: Option[Unit])) == None

@Test
def flatten03(): Bool = Option.flatten(Some(Some(2))) == Some(2)

    /////////////////////////////////////////////////////////////////////////////
    // fold                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def fold01(): Bool = Option.fold(None: Option[Unit]) == ()

    @Test
    def fold02(): Bool = Option.fold(Some("a")) == "a"

    @Test
    def fold03(): Bool = Option.fold(Some(())) == ()

    @Test
    def fold04(): Bool = Option.fold(Some(("a", "b", "c"))) == ("a", "b", "c")

/////////////////////////////////////////////////////////////////////////////
// foldLeft                                                                //
/////////////////////////////////////////////////////////////////////////////
@Test
def foldLeft01(): Bool = Option.foldLeft((b, i) -> if (i == 2 and b) true else false, false, None) == false

@Test
def foldLeft02(): Bool = Option.foldLeft((b, i) -> if (i == 2 and b) true else false, false, Some(1)) == false

@Test
def foldLeft03(): Bool = Option.foldLeft((b, i) -> if (i == 2 and b) true else false, false, Some(2)) == false

@Test
def foldLeft04(): Bool = Option.foldLeft((b, i) -> if (i == 2 and b) true else false, true, Some(1)) == false

@Test
def foldLeft05(): Bool = Option.foldLeft((b, i) -> if (i == 2 and b) true else false, true, Some(2)) == true

/////////////////////////////////////////////////////////////////////////////
// foldRight                                                               //
/////////////////////////////////////////////////////////////////////////////
@Test
def foldRight01(): Bool = Option.foldRight((i, acc) -> if (i == 2 and acc) true else false, None, false) == false

@Test
def foldRight02(): Bool = Option.foldRight((i, acc) -> if (i == 2 and acc) true else false, Some(1), false) == false

@Test
def foldRight03(): Bool = Option.foldRight((i, acc) -> if (i == 2 and acc) true else false, Some(2), false) == false

@Test
def foldRight04(): Bool = Option.foldRight((i, acc) -> if (i == 2 and acc) true else false, Some(1), true) == false

@Test
def foldRight05(): Bool = Option.foldRight((i, acc) -> if (i == 2 and acc) true else false, Some(2), true) == true

/////////////////////////////////////////////////////////////////////////////
// foldRightWithCont                                                       //
/////////////////////////////////////////////////////////////////////////////
@Test
def foldRightWithCont01(): Bool = Option.foldRightWithCont((i, k) -> if (i == 2 and k()) true else false, None, false) == false

@Test
def foldRightWithCont02(): Bool = Option.foldRightWithCont((i, k) -> if (i == 2 and k()) true else false, Some(1), false) == false

@Test
def foldRightWithCont03(): Bool = Option.foldRightWithCont((i, k) -> if (i == 2 and k()) true else false, Some(2), false) == false

@Test
def foldRightWithCont04(): Bool = Option.foldRightWithCont((i, k) -> if (i == 2 and k()) true else false, Some(1), true) == false

@Test
def foldRightWithCont05(): Bool = Option.foldRightWithCont((i, k) -> if (i == 2 and k()) true else false, Some(2), true) == true

/////////////////////////////////////////////////////////////////////////////
// sequence                                                                //
/////////////////////////////////////////////////////////////////////////////
@Test
def sequence01(): Bool = Option.sequence(Nil): Option[List[Unit]] == Some(Nil)

@Test
def sequence02(): Bool = Option.sequence(Some(1) :: Nil) == Some(1 :: Nil)

@Test
def sequence03(): Bool = Option.sequence(Some(1) :: Some(2) :: Nil) == Some(1 :: 2 :: Nil)

@Test
def sequence04(): Bool = Option.sequence(Some(1) :: Some(2) :: Some(3) :: Nil) == Some(1 :: 2 :: 3 :: Nil)

@Test
def sequence05(): Bool = Option.sequence(None :: Some(2) :: Some(3) :: Nil) == None

@Test
def sequence06(): Bool = Option.sequence(Some(1) :: Some(2) :: None :: Nil) == None

@Test
def sequence07(): Bool = Option.sequence(Some(1) :: None :: Some(3) :: Nil) == None

/////////////////////////////////////////////////////////////////////////////
// traverse                                                                //
/////////////////////////////////////////////////////////////////////////////
@Test
def traverse01(): Bool = Option.traverse(x -> Some(x + 1), Nil) == Some(Nil)

@Test
def traverse02(): Bool = Option.traverse(x -> Some(x + 1), 1 :: Nil) == Some(2 :: Nil)

@Test
def traverse03(): Bool = Option.traverse(x -> Some(x + 1), 1 :: 2 :: Nil) == Some(2 :: 3 :: Nil)

@Test
def traverse04(): Bool = Option.traverse(x -> Some(x + 1), 1 :: 2 :: 3 :: Nil) == Some(2 :: 3 :: 4 :: Nil)

@Test
def traverse05(): Bool = Option.traverse(x -> if (x == 1) None else Some(x), 1 :: 2 :: 3 :: Nil) == None

@Test
def traverse06(): Bool = Option.traverse(x -> if (x == 3) None else Some(x), 1 :: 2 :: 3 :: Nil) == None

@Test
def traverse07(): Bool = Option.traverse(x -> if (x == 2) None else Some(x), 1 :: 2 :: 3 :: Nil) == None

@Test
def traverse08(): Bool = Option.traverse(_ -> Some(42), 1 :: 2 :: 3 :: Nil) == Some(42 :: 42 :: 42 :: Nil)

/////////////////////////////////////////////////////////////////////////////
// traverseX                                                               //
/////////////////////////////////////////////////////////////////////////////
@Test
def traverseX01(): Bool = Option.traverseX(x -> Some(x + 1), Nil) == Some()

@Test
def traverseX02(): Bool = Option.traverseX(x -> Some(x + 1), 1 :: Nil) == Some()

@Test
def traverseX03(): Bool = Option.traverseX(x -> Some(x + 1), 1 :: 2 :: Nil) == Some()

@Test
def traverseX04(): Bool = Option.traverseX(x -> Some(x + 1), 1 :: 2 :: 3 :: Nil) == Some()

@Test
def traverseX05(): Bool = Option.traverseX(x -> if (x == 1) None else Some(x), 1 :: 2 :: 3 :: Nil) == None

@Test
def traverseX06(): Bool = Option.traverseX(x -> if (x == 3) None else Some(x), 1 :: 2 :: 3 :: Nil) == None

@Test
def traverseX07(): Bool = Option.traverseX(x -> if (x == 2) None else Some(x), 1 :: 2 :: 3 :: Nil) == None

@Test
def traverseX08(): Bool = Option.traverseX(_ -> Some(42), 1 :: 2 :: 3 :: Nil) == Some()

/////////////////////////////////////////////////////////////////////////////
// foldLeftM                                                               //
/////////////////////////////////////////////////////////////////////////////
@Test
def foldLeftM01(): Bool = Option.foldLeftM((ac, i) -> if (i < 0) None else Some(Int32.max(i, ac)), 0, Nil) == Some(0)

@Test
def foldLeftM02(): Bool = Option.foldLeftM((ac, i) -> if (i < 0) None else Some(Int32.max(i, ac)), 0, 1 :: Nil) == Some(1)

@Test
def foldLeftM03(): Bool = Option.foldLeftM((ac, i) -> if (i < 0) None else Some(Int32.max(i, ac)), 0, 1 :: 2 :: Nil) == Some(2)

@Test
def foldLeftM04(): Bool = Option.foldLeftM((ac, i) -> if (i < 0) None else Some(Int32.max(i, ac)), 0, 1 :: 2 :: 3 :: Nil) == Some(3)

@Test
def foldLeftM05(): Bool = Option.foldLeftM((ac, i) -> if (i < 0) None else Some(Int32.max(i, ac)), 0, -1 :: 2 :: 3 :: Nil) == None

@Test
def foldLeftM06(): Bool = Option.foldLeftM((ac, i) -> if (i < 0) None else Some(Int32.max(i, ac)), 0, 1 :: -2 :: 3 :: Nil) == None

@Test
def foldLeftM07(): Bool = Option.foldLeftM((ac, i) -> if (i < 0) None else Some(Int32.max(i, ac)), 0, 1 :: 2 :: -3 :: Nil) == None

/////////////////////////////////////////////////////////////////////////////
// foldRightM                                                              //
/////////////////////////////////////////////////////////////////////////////
@Test
def foldRightM01(): Bool = Option.foldRightM((i, ac) -> if (i < 0) None else Some(Int32.max(i, ac)), 0, Nil) == Some(0)

@Test
def foldRightM02(): Bool = Option.foldRightM((i, ac) -> if (i < 0) None else Some(Int32.max(i, ac)), 0, 1 :: Nil) == Some(1)

@Test
def foldRightM03(): Bool = Option.foldRightM((i, ac) -> if (i < 0) None else Some(Int32.max(i, ac)), 0, 1 :: 2 :: Nil) == Some(2)

@Test
def foldRightM04(): Bool = Option.foldRightM((i, ac) -> if (i < 0) None else Some(Int32.max(i, ac)), 0, 1 :: 2 :: 3 :: Nil) == Some(3)

@Test
def foldRightM05(): Bool = Option.foldRightM((i, ac) -> if (i < 0) None else Some(Int32.max(i, ac)), 0, -1 :: 2 :: 3 :: Nil) == None

@Test
def foldRightM06(): Bool = Option.foldRightM((i, ac) -> if (i < 0) None else Some(Int32.max(i, ac)), 0, 1 :: -2 :: 3 :: Nil) == None

@Test
def foldRightM07(): Bool = Option.foldRightM((i, ac) -> if (i < 0) None else Some(Int32.max(i, ac)), 0, 1 :: 2 :: -3 :: Nil) == None

/////////////////////////////////////////////////////////////////////////////
// zip                                                                     //
/////////////////////////////////////////////////////////////////////////////
@Test
def zip01(): Bool = Option.zip(None: Option[Unit], None: Option[Unit]) == None

@Test
def zip02(): Bool = Option.zip(Some(1), None: Option[Unit]) == None

@Test
def zip03(): Bool = Option.zip(None: Option[Unit], Some(true)) == None

@Test
def zip04(): Bool = Option.zip(Some(1), Some(true)) == Some((1, true))

/////////////////////////////////////////////////////////////////////////////
// unzip                                                                   //
/////////////////////////////////////////////////////////////////////////////
@Test
def unzip01(): Bool = Option.unzip(None: Option[(Unit, Unit)]) == (None, None)

@Test
def unzip02(): Bool = Option.unzip(Some((1, true))) == (Some(1), Some(true))

/////////////////////////////////////////////////////////////////////////////
// forEach                                                                 //
/////////////////////////////////////////////////////////////////////////////

@Test
def forEach01(): Bool = region rc {
    let ri = ref 21 @ rc;
    Option.forEach(x -> ri := x, None);
    21 == deref ri
}

@Test
def forEach02(): Bool = region rc {
    let ri = ref 21 @ rc;
    Option.forEach(x -> ri := x, Some(42));
    42 == deref ri
}

/////////////////////////////////////////////////////////////////////////////
// point                                                                   //
/////////////////////////////////////////////////////////////////////////////

@Test
def point01(): Bool = Option.point(5) == Some(5)

/////////////////////////////////////////////////////////////////////////////
// ap                                                                      //
/////////////////////////////////////////////////////////////////////////////

@Test
def ap01(): Bool = Option.ap(Some(x -> x + 1), Some(123)) == Some(124)

@Test
def ap02(): Bool = Option.ap(Some(x -> x + 1), None: Option[Int32]) == None

@Test
def ap03(): Bool = Option.ap(None, Some(123)): Option[Int32] == None

@Test
def ap04(): Bool = Option.ap(None, None): Option[Unit] == None

/////////////////////////////////////////////////////////////////////////////
// flatMap                                                                 //
/////////////////////////////////////////////////////////////////////////////

@Test
def flatMap01(): Bool = Option.flatMap(x -> Some(x + 1), Some(123)) == Some(124)

@Test
def flatMap02(): Bool = Option.flatMap(x -> Some(x + 1), None: Option[Int32]) == None

@Test
def flatMap03(): Bool = Option.flatMap(_x -> None, Some(123)): Option[Int32] == None

@Test
def flatMap04(): Bool = Option.flatMap(_x -> None, None): Option[Unit] == None

@Test
def flatMap05(): Bool = Option.flatMap(i -> if (i == 1) None else Some(2*i), None) == None

@Test
def flatMap06(): Bool = Option.flatMap(i -> if (i == 1) None else Some(2*i), Some(1)) == None

@Test
def flatMap07(): Bool = Option.flatMap(i -> if (i == 1) None else Some(2*i), Some(2)) == Some(4)

/////////////////////////////////////////////////////////////////////////////
// map2                                                                    //
/////////////////////////////////////////////////////////////////////////////

@Test
def map201(): Bool = Option.map2((x1,x2) -> x1 + x2, Some(123), Some(1)) == Some(124)

@Test
def map202(): Bool = Option.map2((x1,x2) -> x1 + x2, Some(123), None) == None

@Test
def map203(): Bool = Option.map2((x1,x2) -> x1 + x2, None, Some(1)) == None

@Test
def map204(): Bool = Option.map2((x1,x2) -> x1 + x2, None, None: Option[Int32]) == None

/////////////////////////////////////////////////////////////////////////////
// map3                                                                    //
/////////////////////////////////////////////////////////////////////////////

@Test
def map301(): Bool = Option.map3((x1,x2,x3) -> x1 + x2 + x3, Some(123), Some(1), Some(2)) == Some(126)

@Test
def map302(): Bool = Option.map3((x1,x2,x3) -> x1 + x2 + x3, Some(123), Some(1), None) == None

@Test
def map303(): Bool = Option.map3((x1,x2,x3) -> x1 + x2 + x3, Some(123), None, Some(2)) == None

@Test
def map304(): Bool = Option.map3((x1,x2,x3) -> x1 + x2 + x3, None, Some(1), Some(2)) == None

@Test
def map305(): Bool = Option.map3((x1,x2,x3) -> x1 + x2 + x3, Some(123), None, None) == None

@Test
def map306(): Bool = Option.map3((x1,x2,x3) -> x1 + x2 + x3, None, Some(1), None) == None

@Test
def map307(): Bool = Option.map3((x1,x2,x3) -> x1 + x2 + x3, None, None, None: Option[Int32]) == None

/////////////////////////////////////////////////////////////////////////////
// map4                                                                    //
/////////////////////////////////////////////////////////////////////////////

@Test
def map401(): Bool = Option.map4((x1,x2,x3,x4) -> x1 + x2 + x3 + x4, Some(123), Some(1), Some(2), Some(3)) == Some(129)

@Test
def map402(): Bool = Option.map4((x1,x2,x3,x4) -> x1 + x2 + x3 + x4, Some(123), Some(1), Some(2), None) == None

@Test
def map403(): Bool = Option.map4((x1,x2,x3,x4) -> x1 + x2 + x3 + x4, Some(123), Some(1), None, Some(3)) == None

@Test
def map404(): Bool = Option.map4((x1,x2,x3,x4) -> x1 + x2 + x3 + x4, Some(123), None, Some(2), Some(3)) == None

@Test
def map405(): Bool = Option.map4((x1,x2,x3,x4) -> x1 + x2 + x3 + x4, None, Some(1), Some(2), Some(3)) == None

@Test
def map406(): Bool = Option.map4((x1,x2,x3,x4) -> x1 + x2 + x3 + x4, None, None, None, None: Option[Int32]) == None

/////////////////////////////////////////////////////////////////////////////
// map5                                                                    //
/////////////////////////////////////////////////////////////////////////////

@Test
def map501(): Bool = Option.map5((x1,x2,x3,x4,x5) -> x1 + x2 + x3 + x4 + x5, Some(123), Some(1), Some(2), Some(3), Some(4)) == Some(133)

@Test
def map502(): Bool = Option.map5((x1,x2,x3,x4,x5) -> x1 + x2 + x3 + x4 + x5, Some(123), Some(1), Some(2), Some(3), None) == None

@Test
def map503(): Bool = Option.map5((x1,x2,x3,x4,x5) -> x1 + x2 + x3 + x4 + x5, Some(123), Some(1), Some(2), None, Some(4)) == None

@Test
def map504(): Bool = Option.map5((x1,x2,x3,x4,x5) -> x1 + x2 + x3 + x4 + x5, Some(123), Some(1), None, Some(3), Some(4)) == None

@Test
def map505(): Bool = Option.map5((x1,x2,x3,x4,x5) -> x1 + x2 + x3 + x4 + x5, Some(123), None, Some(2), Some(3), Some(4)) == None

@Test
def map506(): Bool = Option.map5((x1,x2,x3,x4,x5) -> x1 + x2 + x3 + x4 + x5, None, Some(1), Some(2), Some(3), Some(4)) == None

@Test
def map507(): Bool = Option.map5((x1,x2,x3,x4,x5) -> x1 + x2 + x3 + x4 + x5, None, None, None, None, None: Option[Int32]) == None

/////////////////////////////////////////////////////////////////////////////
// map6                                                                   //
/////////////////////////////////////////////////////////////////////////////

@Test
def map601(): Bool = Option.map6((x1,x2,x3,x4,x5,x6) -> x1 + x2 + x3 + x4 + x5 + x6, Some(123), Some(1), Some(2), Some(3), Some(4), Some(5)) == Some(138)

@Test
def map602(): Bool = Option.map6((x1,x2,x3,x4,x5,x6) -> x1 + x2 + x3 + x4 + x5 + x6, Some(123), Some(1), Some(2), Some(3), Some(4), None) == None

@Test
def map603(): Bool = Option.map6((x1,x2,x3,x4,x5,x6) -> x1 + x2 + x3 + x4 + x5 + x6, Some(123), Some(1), Some(2), Some(3), None, Some(5)) == None

@Test
def map604(): Bool = Option.map6((x1,x2,x3,x4,x5,x6) -> x1 + x2 + x3 + x4 + x5 + x6, Some(123), Some(1), Some(2), None, Some(4), Some(5)) == None

@Test
def map605(): Bool = Option.map6((x1,x2,x3,x4,x5,x6) -> x1 + x2 + x3 + x4 + x5 + x6, Some(123), Some(1), None, Some(3), Some(4), Some(5)) == None

@Test
def map606(): Bool = Option.map6((x1,x2,x3,x4,x5,x6) -> x1 + x2 + x3 + x4 + x5 + x6, Some(123), None, Some(2), Some(3), Some(4), Some(5)) == None

@Test
def map607(): Bool = Option.map6((x1,x2,x3,x4,x5,x6) -> x1 + x2 + x3 + x4 + x5 + x6, None, Some(1), Some(2), Some(3), Some(4), Some(5)) == None

@Test
def map608(): Bool = Option.map6((x1,x2,x3,x4,x5,x6) -> x1 + x2 + x3 + x4 + x5 + x6, None, None, None, None, None, None: Option[Int32]) == None

/////////////////////////////////////////////////////////////////////////////
// map7                                                                    //
/////////////////////////////////////////////////////////////////////////////

@Test
def map701(): Bool = Option.map7((x1,x2,x3,x4,x5,x6,x7) -> x1 + x2 + x3 + x4 + x5 + x6 + x7, Some(123), Some(1), Some(2), Some(3), Some(4), Some(5), Some(6)) == Some(144)

@Test
def map702(): Bool = Option.map7((x1,x2,x3,x4,x5,x6,x7) -> x1 + x2 + x3 + x4 + x5 + x6 + x7, Some(123), Some(1), Some(2), Some(3), Some(4), Some(5), None) == None

@Test
def map703(): Bool = Option.map7((x1,x2,x3,x4,x5,x6,x7) -> x1 + x2 + x3 + x4 + x5 + x6 + x7, Some(123), Some(1), Some(2), Some(3), Some(4), None, Some(6)) == None

@Test
def map704(): Bool = Option.map7((x1,x2,x3,x4,x5,x6,x7) -> x1 + x2 + x3 + x4 + x5 + x6 + x7, Some(123), Some(1), Some(2), Some(3), None, Some(5), Some(6)) == None

@Test
def map705(): Bool = Option.map7((x1,x2,x3,x4,x5,x6,x7) -> x1 + x2 + x3 + x4 + x5 + x6 + x7, Some(123), Some(1), Some(2), None, Some(4), Some(5), Some(6)) == None

@Test
def map706(): Bool = Option.map7((x1,x2,x3,x4,x5,x6,x7) -> x1 + x2 + x3 + x4 + x5 + x6 + x7, Some(123), Some(1), None, Some(3), Some(4), Some(5), Some(6)) == None

@Test
def map707(): Bool = Option.map7((x1,x2,x3,x4,x5,x6,x7) -> x1 + x2 + x3 + x4 + x5 + x6 + x7, Some(123), None, Some(2), Some(3), Some(4), Some(5), Some(6)) == None

@Test
def map708(): Bool = Option.map7((x1,x2,x3,x4,x5,x6,x7) -> x1 + x2 + x3 + x4 + x5 + x6 + x7, None, Some(1), Some(2), Some(3), Some(4), Some(5), Some(6)) == None

@Test
def map709(): Bool = Option.map7((x1,x2,x3,x4,x5,x6,x7) -> x1 + x2 + x3 + x4 + x5 + x6 + x7, None, None, None, None, None, None, None: Option[Int32]) == None

/////////////////////////////////////////////////////////////////////////////
// map8                                                                    //
/////////////////////////////////////////////////////////////////////////////

@Test
def map801(): Bool = Option.map8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, Some(123), Some(1), Some(2), Some(3), Some(4), Some(5), Some(6), Some(7)) == Some(151)

@Test
def map802(): Bool = Option.map8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, Some(123), Some(1), Some(2), Some(3), Some(4), Some(5), Some(6), None) == None

@Test
def map803(): Bool = Option.map8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, Some(123), Some(1), Some(2), Some(3), Some(4), Some(5), None, Some(7)) == None

@Test
def map804(): Bool = Option.map8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, Some(123), Some(1), Some(2), Some(3), Some(4), None, Some(6), Some(7)) == None

@Test
def map805(): Bool = Option.map8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, Some(123), Some(1), Some(2), Some(3), None, Some(5), Some(6), Some(7)) == None

@Test
def map806(): Bool = Option.map8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, Some(123), Some(1), Some(2), None, Some(4), Some(5), Some(6), Some(7)) == None

@Test
def map807(): Bool = Option.map8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, Some(123), Some(1), None, Some(3), Some(4), Some(5), Some(6), Some(7)) == None

@Test
def map808(): Bool = Option.map8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, Some(123), None, Some(2), Some(3), Some(4), Some(5), Some(6), Some(7)) == None

@Test
def map809(): Bool = Option.map8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, None, Some(1), Some(2), Some(3), Some(4), Some(5), Some(6), Some(7)) == None

@Test
def map810(): Bool = Option.map8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, None, None, None, None, None, None, None, None: Option[Int32]) == None

/////////////////////////////////////////////////////////////////////////////
// map9                                                                    //
/////////////////////////////////////////////////////////////////////////////

@Test
def map901(): Bool = Option.map9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Some(123), Some(1), Some(2), Some(3), Some(4), Some(5), Some(6), Some(7), Some(8)) == Some(159)

@Test
def map902(): Bool = Option.map9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Some(123), Some(1), Some(2), Some(3), Some(4), Some(5), Some(6), Some(7), None) == None

@Test
def map903(): Bool = Option.map9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Some(123), Some(1), Some(2), Some(3), Some(4), Some(5), Some(6), None, Some(8)) == None

@Test
def map904(): Bool = Option.map9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Some(123), Some(1), Some(2), Some(3), Some(4), Some(5), None, Some(7), Some(8)) == None

@Test
def map905(): Bool = Option.map9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Some(123), Some(1), Some(2), Some(3), Some(4), None, Some(6), Some(7), Some(8)) == None

@Test
def map906(): Bool = Option.map9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Some(123), Some(1), Some(2), Some(3), None, Some(5), Some(6), Some(7), Some(8)) == None

@Test
def map907(): Bool = Option.map9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Some(123), Some(1), Some(2), None, Some(4), Some(5), Some(6), Some(7), Some(8)) == None

@Test
def map908(): Bool = Option.map9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Some(123), Some(1), None, Some(3), Some(4), Some(5), Some(6), Some(7), Some(8)) == None

@Test
def map909(): Bool = Option.map9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Some(123), None, Some(2), Some(3), Some(4), Some(5), Some(6), Some(7), Some(8)) == None

@Test
def map910(): Bool = Option.map9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, None, Some(1), Some(2), Some(3), Some(4), Some(5), Some(6), Some(7), Some(8)) == None

@Test
def map911(): Bool = Option.map9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, None, None, None, None, None, None, None, None, None: Option[Int32]) == None

/////////////////////////////////////////////////////////////////////////////
// map10                                                                   //
/////////////////////////////////////////////////////////////////////////////

@Test
def map1001(): Bool = Option.map10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Some(123), Some(1), Some(2), Some(3), Some(4), Some(5), Some(6), Some(7), Some(8), Some(9)) == Some(168)

@Test
def map1002(): Bool = Option.map10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Some(123), Some(1), Some(2), Some(3), Some(4), Some(5), Some(6), Some(7), Some(8), None) == None

@Test
def map1003(): Bool = Option.map10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Some(123), Some(1), Some(2), Some(3), Some(4), Some(5), Some(6), Some(7), None, Some(9)) == None

@Test
def map1004(): Bool = Option.map10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Some(123), Some(1), Some(2), Some(3), Some(4), Some(5), Some(6), None, Some(8), Some(9)) == None

@Test
def map1005(): Bool = Option.map10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Some(123), Some(1), Some(2), Some(3), Some(4), Some(5), None, Some(7), Some(8), Some(9)) == None

@Test
def map1006(): Bool = Option.map10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Some(123), Some(1), Some(2), Some(3), Some(4), None, Some(6), Some(7), Some(8), Some(9)) == None

@Test
def map1007(): Bool = Option.map10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Some(123), Some(1), Some(2), Some(3), None, Some(5), Some(6), Some(7), Some(8), Some(9)) == None

@Test
def map1008(): Bool = Option.map10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Some(123), Some(1), Some(2), None, Some(4), Some(5), Some(6), Some(7), Some(8), Some(9)) == None

@Test
def map1009(): Bool = Option.map10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Some(123), Some(1), None, Some(3), Some(4), Some(5), Some(6), Some(7), Some(8), Some(9)) == None

@Test
def map1010(): Bool = Option.map10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Some(123), None, Some(2), Some(3), Some(4), Some(5), Some(6), Some(7), Some(8), Some(9)) == None

@Test
def map1011(): Bool = Option.map10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, None, Some(1), Some(2), Some(3), Some(4), Some(5), Some(6), Some(7), Some(8), Some(9)) == None

@Test
def map1012(): Bool = Option.map10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, None, None, None, None, None, None, None, None, None, None: Option[Int32]) == None

    /////////////////////////////////////////////////////////////////////////////
    // hash                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def hash01(): Bool = hash(None: Option[Unit]) == hash(None: Option[Unit])

    @Test
    def hash02(): Bool = hash(Some(1)) == hash(Some(1))

    @Test
    def hash03(): Bool = hash(Some((1, 1))) == hash(Some((1, 1)))

    @Test
    def hash04(): Bool = hash(Some((1, 2))) != hash(Some((2, 1)))

    @Test
    def hash05(): Bool = hash(None: Option[Unit]) != hash(Some((())))


    /////////////////////////////////////////////////////////////////////////////
    // iterator                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def iterator01(): Bool = region rc {
        None: Option[Int32] |> Option.iterator(rc) |> Iterator.toList == Nil
    }

    @Test
    def iterator02(): Bool = region rc {
        Some(1) |> Option.iterator(rc) |> Iterator.toList == 1 :: Nil
    }

    @Test
    def iterator03(): Bool = region rc {
        Some(1 :: 2 :: 3 :: Nil) |> Option.iterator(rc) |> Iterator.toList == (1 :: 2 :: 3 :: Nil) :: Nil
    }

}

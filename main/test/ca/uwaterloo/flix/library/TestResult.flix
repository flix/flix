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

namespace TestResult {

/////////////////////////////////////////////////////////////////////////////
// isOk                                                                    //
/////////////////////////////////////////////////////////////////////////////
@test
def isOk01(): Bool = assert!(Result.isOk(Ok(1)))

@test
def isOk02(): Bool = assertNot!(Result.isOk(Err(1)))

/////////////////////////////////////////////////////////////////////////////
// isErr                                                                   //
/////////////////////////////////////////////////////////////////////////////
@test
def isErr01(): Bool = assertNot!(Result.isErr(Ok(1)))

@test
def isErr02(): Bool = assert!(Result.isErr(Err(1)))

/////////////////////////////////////////////////////////////////////////////
// getWithDefault                                                          //
/////////////////////////////////////////////////////////////////////////////
@test
def getWithDefault01(): Bool = Result.getWithDefault(Ok(1), 2) `assertEq!` 1

@test
def getWithDefault02(): Bool = Result.getWithDefault(Err(false), 2) `assertEq!` 2

/////////////////////////////////////////////////////////////////////////////
// withDefault                                                             //
/////////////////////////////////////////////////////////////////////////////
@test
def withDefault01(): Bool = Result.withDefault(Ok(1), Ok(2)) `assertEq!` Ok(1)

@test
def withDefault02(): Bool = Result.withDefault(Ok(1), Err(false)) `assertEq!` Ok(1)

@test
def withDefault03(): Bool = Result.withDefault(Err(false), Ok(2)) `assertEq!` Ok(2)

@test
def withDefault04(): Bool = Result.withDefault(Err(false), Err(true)) `assertEq!` Err(true)

/////////////////////////////////////////////////////////////////////////////
// replace                                                                 //
/////////////////////////////////////////////////////////////////////////////
@test
def replace01(): Bool = Result.replace(3, 4, Err(false)) `assertEq!` Err(false)

@test
def replace02(): Bool = Result.replace(3, 4, Ok(2)) `assertEq!` Ok(2)

@test
def replace03(): Bool = Result.replace(3, 4, Ok(3)) `assertEq!` Ok(4)

@test
def replace04(): Bool = Result.replace(3, 4, Ok(4)) `assertEq!` Ok(4)

/////////////////////////////////////////////////////////////////////////////
// exists                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def exists01(): Bool = assertNot!(Result.exists(i -> i == 2, Err(false)))

@test
def exists02(): Bool = assertNot!(Result.exists(i -> i == 2, Ok(1)))

@test
def exists03(): Bool = assert!(Result.exists(i -> i == 2, Ok(2)))

/////////////////////////////////////////////////////////////////////////////
// forall                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def forall01(): Bool = assert!(Result.forall(i -> i == 2, Err(false)))

@test
def forall02(): Bool = assertNot!(Result.forall(i -> i == 2, Ok(1)))

@test
def forall03(): Bool = assert!(Result.forall(i -> i == 2, Ok(2)))

/////////////////////////////////////////////////////////////////////////////
// map                                                                     //
/////////////////////////////////////////////////////////////////////////////
@test
def map01(): Bool = Result.map(i -> i == 2, Err(-1ii)) `assertEq!` Err(-1ii)

@test
def map02(): Bool = Result.map(i -> i == 2, Ok(1)) `assertEq!` Ok(false)

@test
def map03(): Bool = Result.map(i -> i == 2, Ok(2)) `assertEq!` Ok(true)

/////////////////////////////////////////////////////////////////////////////
// mapErr                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def mapErr01(): Bool = Result.mapErr(i -> i == 2, Ok(1)) `assertEq!` Ok(1)

@test
def mapErr02(): Bool = Result.mapErr(i -> i == 2, Err(1)) `assertEq!` Err(false)

@test
def mapErr03(): Bool = Result.mapErr(i -> i == 2, Err(2)) `assertEq!` Err(true)

/////////////////////////////////////////////////////////////////////////////
// flatMap                                                                 //
/////////////////////////////////////////////////////////////////////////////
@test
def flatMap01(): Bool = Result.flatMap(i -> if (i == 2) Ok(i) else Err(false), Err(true)) `assertEq!` Err(true)

@test
def flatMap02(): Bool = Result.flatMap(i -> if (i == 2) Ok(i) else Err(false), Ok(1)) `assertEq!` Err(false)

@test
def flatMap03(): Bool = Result.flatMap(i -> if (i == 2) Ok(2*i) else Err(false), Ok(2)) `assertEq!` Ok(4)

/////////////////////////////////////////////////////////////////////////////
// count                                                                   //
/////////////////////////////////////////////////////////////////////////////
@test
def count01(): Bool = Result.count(i -> i == 2, Err(false)) `assertEq!` 0

@test
def count02(): Bool = Result.count(i -> i == 2, Ok(1)) `assertEq!` 0

@test
def count03(): Bool = Result.count(i -> i == 2, Ok(2)) `assertEq!` 1

/////////////////////////////////////////////////////////////////////////////
// find                                                                    //
/////////////////////////////////////////////////////////////////////////////
@test
def find01(): Bool = Result.find(i -> i == 2, Err(false)) `assertEq!` None

@test
def find02(): Bool = Result.find(i -> i == 2, Ok(1)) `assertEq!` None

@test
def find03(): Bool = Result.find(i -> i == 2, Ok(2)) `assertEq!` Some(2)

/////////////////////////////////////////////////////////////////////////////
// foldLeft                                                                //
/////////////////////////////////////////////////////////////////////////////
@test
def foldLeft01(): Bool = Result.foldLeft((b, i) -> if (i == 2 && b) true else false, false, Err(-1ii)) `assertEq!` false

@test
def foldLeft02(): Bool = Result.foldLeft((b, i) -> if (i == 2 && b) true else false, false, Ok(1)) `assertEq!` false

@test
def foldLeft03(): Bool = Result.foldLeft((b, i) -> if (i == 2 && b) true else false, true, Ok(1)) `assertEq!` false

@test
def foldLeft04(): Bool = Result.foldLeft((b, i) -> if (i == 2 && b) true else false, false, Ok(2)) `assertEq!` false

@test
def foldLeft05(): Bool = Result.foldLeft((b, i) -> if (i == 2 && b) true else false, true, Ok(2)) `assertEq!` true

/////////////////////////////////////////////////////////////////////////////
// foldRight                                                               //
/////////////////////////////////////////////////////////////////////////////
@test
def foldRight01(): Bool = Result.foldRight((i, b) -> if (i == 2 && b) true else false, Err(-1ii), false) `assertEq!` false

@test
def foldRight02(): Bool = Result.foldRight((i, b) -> if (i == 2 && b) true else false, Ok(1), false) `assertEq!` false

@test
def foldRight03(): Bool = Result.foldRight((i, b) -> if (i == 2 && b) true else false, Ok(1), true) `assertEq!` false

@test
def foldRight04(): Bool = Result.foldRight((i, b) -> if (i == 2 && b) true else false, Ok(2), false) `assertEq!` false

@test
def foldRight05(): Bool = Result.foldRight((i, b) -> if (i == 2 && b) true else false, Ok(2), true) `assertEq!` true

/////////////////////////////////////////////////////////////////////////////
// toList                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def toList01(): Bool = Result.toList(Err(-1ii)) `assertEq!` Nil

@test
def toList02(): Bool = Result.toList(Ok(1)) `assertEq!` 1 :: Nil

/////////////////////////////////////////////////////////////////////////////
// toSet                                                                   //
/////////////////////////////////////////////////////////////////////////////
@test
def toSet01(): Bool = Result.toSet(Err(-1ii)) `assertEq!` Set#{}

@test
def toSet02(): Bool = Result.toSet(Ok(1)) `assertEq!` Set#{1}

/////////////////////////////////////////////////////////////////////////////
// toMap                                                                   //
/////////////////////////////////////////////////////////////////////////////
@test
def toMap01(): Bool = Result.toMap(Err(-1ii)) `assertEq!` Map#{}

@test
def toMap02(): Bool = Result.toMap(Ok((1, true))) `assertEq!` Map#{1 -> true}

/////////////////////////////////////////////////////////////////////////////
// toOption                                                                //
/////////////////////////////////////////////////////////////////////////////
@test
def toOption01(): Bool = Result.toOption(Err(-1ii)) `assertEq!` None

@test
def toOption02(): Bool = Result.toOption(Ok(1)) `assertEq!` Some(1)

/////////////////////////////////////////////////////////////////////////////
// foreach                                                                 //
/////////////////////////////////////////////////////////////////////////////

@test
def foreach01(): Bool & Impure =
    let r = ref 21;
    Result.foreach(x -> r := x, Ok(42): Result[Int, Int]);
    42 == deref r

@test
def foreach02(): Bool & Impure =
    let r = ref 21;
    Result.foreach(x -> r := x, Err(42): Result[Int, Int]);
    21 == deref r

/////////////////////////////////////////////////////////////////////////////
// ap                                                                      //
/////////////////////////////////////////////////////////////////////////////
@test
def ap01(): Bool = Result.ap(Ok(x -> x + 1), Ok(123)) == Ok(124)

@test
def ap02(): Bool = Result.ap(Ok(x -> x + 1), Err("b")) == Err("b")

@test
def ap03(): Bool = Result.ap(Err("a"), Ok(123)) == Err("a")

@test
def ap04(): Bool = Result.ap(Err("a"), Err("b")) == Err("a")

/////////////////////////////////////////////////////////////////////////////
// lift2                                                                   //
/////////////////////////////////////////////////////////////////////////////

@test
def lift201(): Bool = Result.lift2((x1,x2) -> x1 + x2, Ok(123), Ok(1)) == Ok(124)

@test
def lift202(): Bool = Result.lift2((x1,x2) -> x1 + x2, Ok(123), Err("e1")) == Err("e1")

@test
def lift203(): Bool = Result.lift2((x1,x2) -> x1 + x2, Err("e1"), Ok(1)) == Err("e1")

@test
def lift204(): Bool = Result.lift2((x1,x2) -> x1 + x2, Err("e1"), Err("e2"): Result[Int32, String]) == Err("e1")

/////////////////////////////////////////////////////////////////////////////
// lift3                                                                   //
/////////////////////////////////////////////////////////////////////////////

@test
def lift301(): Bool = Result.lift3((x1,x2,x3) -> x1 + x2 + x3, Ok(123), Ok(1), Ok(2)) == Ok(126)

@test
def lift302(): Bool = Result.lift3((x1,x2,x3) -> x1 + x2 + x3, Ok(123), Ok(1), Err("e1")) == Err("e1")

@test
def lift303(): Bool = Result.lift3((x1,x2,x3) -> x1 + x2 + x3, Ok(123), Err("e1"), Ok(2)) == Err("e1")

@test
def lift304(): Bool = Result.lift3((x1,x2,x3) -> x1 + x2 + x3, Err("e1"), Ok(1), Ok(2)) == Err("e1")

@test
def lift305(): Bool = Result.lift3((x1,x2,x3) -> x1 + x2 + x3, Ok(123), Err("e1"), Err("e2")) == Err("e1")

@test
def lift306(): Bool = Result.lift3((x1,x2,x3) -> x1 + x2 + x3, Err("e1"), Ok(1), Err("e2")) == Err("e1")

@test
def lift307(): Bool = Result.lift3((x1,x2,x3) -> x1 + x2 + x3, Err("e1"), Err("e2"), Err("e3"): Result[Int32, String]) == Err("e1")

/////////////////////////////////////////////////////////////////////////////
// lift4                                                                   //
/////////////////////////////////////////////////////////////////////////////

@test
def lift401(): Bool = Result.lift4((x1,x2,x3,x4) -> x1 + x2 + x3 + x4, Ok(123), Ok(1), Ok(2), Ok(3)) == Ok(129)

@test
def lift402(): Bool = Result.lift4((x1,x2,x3,x4) -> x1 + x2 + x3 + x4, Ok(123), Ok(1), Ok(2), Err("e1")) == Err("e1")

@test
def lift403(): Bool = Result.lift4((x1,x2,x3,x4) -> x1 + x2 + x3 + x4, Ok(123), Ok(1), Err("e1"), Ok(3)) == Err("e1")

@test
def lift404(): Bool = Result.lift4((x1,x2,x3,x4) -> x1 + x2 + x3 + x4, Ok(123), Err("e1"), Ok(2), Ok(3)) == Err("e1")

@test
def lift405(): Bool = Result.lift4((x1,x2,x3,x4) -> x1 + x2 + x3 + x4, Err("e1"), Ok(1), Ok(2), Ok(3)) == Err("e1")

@test
def lift406(): Bool = Result.lift4((x1,x2,x3,x4) -> x1 + x2 + x3 + x4, Err("e1"), Err("e2"), Err("e3"), Err("e4"): Result[Int32, String]) == Err("e1")

/////////////////////////////////////////////////////////////////////////////
// lift5                                                                   //
/////////////////////////////////////////////////////////////////////////////

@test
def lift501(): Bool = Result.lift5((x1,x2,x3,x4,x5) -> x1 + x2 + x3 + x4 + x5, Ok(123), Ok(1), Ok(2), Ok(3), Ok(4)) == Ok(133)

@test
def lift502(): Bool = Result.lift5((x1,x2,x3,x4,x5) -> x1 + x2 + x3 + x4 + x5, Ok(123), Ok(1), Ok(2), Ok(3), Err("e1")) == Err("e1")

@test
def lift503(): Bool = Result.lift5((x1,x2,x3,x4,x5) -> x1 + x2 + x3 + x4 + x5, Ok(123), Ok(1), Ok(2), Err("e1"), Ok(4)) == Err("e1")

@test
def lift504(): Bool = Result.lift5((x1,x2,x3,x4,x5) -> x1 + x2 + x3 + x4 + x5, Ok(123), Ok(1), Err("e1"), Ok(3), Ok(4)) == Err("e1")

@test
def lift505(): Bool = Result.lift5((x1,x2,x3,x4,x5) -> x1 + x2 + x3 + x4 + x5, Ok(123), Err("e1"), Ok(2), Ok(3), Ok(4)) == Err("e1")

@test
def lift506(): Bool = Result.lift5((x1,x2,x3,x4,x5) -> x1 + x2 + x3 + x4 + x5, Err("e1"), Ok(1), Ok(2), Ok(3), Ok(4)) == Err("e1")

@test
def lift507(): Bool = Result.lift5((x1,x2,x3,x4,x5) -> x1 + x2 + x3 + x4 + x5, Err("e1"), Err("e2"), Err("e3"), Err("e4"), Err("e5"): Result[Int32, String]) == Err("e1")

/////////////////////////////////////////////////////////////////////////////
// lift6                                                                   //
/////////////////////////////////////////////////////////////////////////////

@test
def lift601(): Bool = Result.lift6((x1,x2,x3,x4,x5,x6) -> x1 + x2 + x3 + x4 + x5 + x6, Ok(123), Ok(1), Ok(2), Ok(3), Ok(4), Ok(5)) == Ok(138)

@test
def lift602(): Bool = Result.lift6((x1,x2,x3,x4,x5,x6) -> x1 + x2 + x3 + x4 + x5 + x6, Ok(123), Ok(1), Ok(2), Ok(3), Ok(4), Err("e1")) == Err("e1")

@test
def lift603(): Bool = Result.lift6((x1,x2,x3,x4,x5,x6) -> x1 + x2 + x3 + x4 + x5 + x6, Ok(123), Ok(1), Ok(2), Ok(3), Err("e1"), Ok(5)) == Err("e1")

@test
def lift604(): Bool = Result.lift6((x1,x2,x3,x4,x5,x6) -> x1 + x2 + x3 + x4 + x5 + x6, Ok(123), Ok(1), Ok(2), Err("e1"), Ok(4), Ok(5)) == Err("e1")

@test
def lift605(): Bool = Result.lift6((x1,x2,x3,x4,x5,x6) -> x1 + x2 + x3 + x4 + x5 + x6, Ok(123), Ok(1), Err("e1"), Ok(3), Ok(4), Ok(5)) == Err("e1")

@test
def lift606(): Bool = Result.lift6((x1,x2,x3,x4,x5,x6) -> x1 + x2 + x3 + x4 + x5 + x6, Ok(123), Err("e1"), Ok(2), Ok(3), Ok(4), Ok(5)) == Err("e1")

@test
def lift607(): Bool = Result.lift6((x1,x2,x3,x4,x5,x6) -> x1 + x2 + x3 + x4 + x5 + x6, Err("e1"), Ok(1), Ok(2), Ok(3), Ok(4), Ok(5)) == Err("e1")

@test
def lift608(): Bool = Result.lift6((x1,x2,x3,x4,x5,x6) -> x1 + x2 + x3 + x4 + x5 + x6, Err("e1"), Err("e2"), Err("e3"), Err("e4"), Err("e5"), Err("e6"): Result[Int32, String]) == Err("e1")

/////////////////////////////////////////////////////////////////////////////
// lift7                                                                   //
/////////////////////////////////////////////////////////////////////////////

@test
def lift701(): Bool = Result.lift7((x1,x2,x3,x4,x5,x6,x7) -> x1 + x2 + x3 + x4 + x5 + x6 + x7, Ok(123), Ok(1), Ok(2), Ok(3), Ok(4), Ok(5), Ok(6)) == Ok(144)

@test
def lift702(): Bool = Result.lift7((x1,x2,x3,x4,x5,x6,x7) -> x1 + x2 + x3 + x4 + x5 + x6 + x7, Ok(123), Ok(1), Ok(2), Ok(3), Ok(4), Ok(5), Err("e1")) == Err("e1")

@test
def lift703(): Bool = Result.lift7((x1,x2,x3,x4,x5,x6,x7) -> x1 + x2 + x3 + x4 + x5 + x6 + x7, Ok(123), Ok(1), Ok(2), Ok(3), Ok(4), Err("e1"), Ok(6)) == Err("e1")

@test
def lift704(): Bool = Result.lift7((x1,x2,x3,x4,x5,x6,x7) -> x1 + x2 + x3 + x4 + x5 + x6 + x7, Ok(123), Ok(1), Ok(2), Ok(3), Err("e1"), Ok(5), Ok(6)) == Err("e1")

@test
def lift705(): Bool = Result.lift7((x1,x2,x3,x4,x5,x6,x7) -> x1 + x2 + x3 + x4 + x5 + x6 + x7, Ok(123), Ok(1), Ok(2), Err("e1"), Ok(4), Ok(5), Ok(6)) == Err("e1")

@test
def lift706(): Bool = Result.lift7((x1,x2,x3,x4,x5,x6,x7) -> x1 + x2 + x3 + x4 + x5 + x6 + x7, Ok(123), Ok(1), Err("e1"), Ok(3), Ok(4), Ok(5), Ok(6)) == Err("e1")

@test
def lift707(): Bool = Result.lift7((x1,x2,x3,x4,x5,x6,x7) -> x1 + x2 + x3 + x4 + x5 + x6 + x7, Ok(123), Err("e1"), Ok(2), Ok(3), Ok(4), Ok(5), Ok(6)) == Err("e1")

@test
def lift708(): Bool = Result.lift7((x1,x2,x3,x4,x5,x6,x7) -> x1 + x2 + x3 + x4 + x5 + x6 + x7, Err("e1"), Ok(1), Ok(2), Ok(3), Ok(4), Ok(5), Ok(6)) == Err("e1")

@test
def lift709(): Bool = Result.lift7((x1,x2,x3,x4,x5,x6,x7) -> x1 + x2 + x3 + x4 + x5 + x6 + x7, Err("e1"), Err("e2"), Err("e3"), Err("e4"), Err("e5"), Err("e6"), Err("e7"): Result[Int32, String]) == Err("e1")

/////////////////////////////////////////////////////////////////////////////
// lift8                                                                   //
/////////////////////////////////////////////////////////////////////////////

@test
def lift801(): Bool = Result.lift8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, Ok(123), Ok(1), Ok(2), Ok(3), Ok(4), Ok(5), Ok(6), Ok(7)) == Ok(151)

@test
def lift802(): Bool = Result.lift8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, Ok(123), Ok(1), Ok(2), Ok(3), Ok(4), Ok(5), Ok(6), Err("e1")) == Err("e1")

@test
def lift803(): Bool = Result.lift8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, Ok(123), Ok(1), Ok(2), Ok(3), Ok(4), Ok(5), Err("e1"), Ok(7)) == Err("e1")

@test
def lift804(): Bool = Result.lift8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, Ok(123), Ok(1), Ok(2), Ok(3), Ok(4), Err("e1"), Ok(6), Ok(7)) == Err("e1")

@test
def lift805(): Bool = Result.lift8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, Ok(123), Ok(1), Ok(2), Ok(3), Err("e1"), Ok(5), Ok(6), Ok(7)) == Err("e1")

@test
def lift806(): Bool = Result.lift8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, Ok(123), Ok(1), Ok(2), Err("e1"), Ok(4), Ok(5), Ok(6), Ok(7)) == Err("e1")

@test
def lift807(): Bool = Result.lift8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, Ok(123), Ok(1), Err("e1"), Ok(3), Ok(4), Ok(5), Ok(6), Ok(7)) == Err("e1")

@test
def lift808(): Bool = Result.lift8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, Ok(123), Err("e1"), Ok(2), Ok(3), Ok(4), Ok(5), Ok(6), Ok(7)) == Err("e1")

@test
def lift809(): Bool = Result.lift8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, Err("e1"), Ok(1), Ok(2), Ok(3), Ok(4), Ok(5), Ok(6), Ok(7)) == Err("e1")

@test
def lift810(): Bool = Result.lift8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, Err("e1"), Err("e2"), Err("e3"), Err("e4"), Err("e5"), Err("e6"), Err("e7"), Err("e8"): Result[Int32, String]) == Err("e1")

/////////////////////////////////////////////////////////////////////////////
// lift9                                                                   //
/////////////////////////////////////////////////////////////////////////////

@test
def lift901(): Bool = Result.lift9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Ok(123), Ok(1), Ok(2), Ok(3), Ok(4), Ok(5), Ok(6), Ok(7), Ok(8)) == Ok(159)

@test
def lift902(): Bool = Result.lift9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Ok(123), Ok(1), Ok(2), Ok(3), Ok(4), Ok(5), Ok(6), Ok(7), Err("e1")) == Err("e1")

@test
def lift903(): Bool = Result.lift9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Ok(123), Ok(1), Ok(2), Ok(3), Ok(4), Ok(5), Ok(6), Err("e1"), Ok(8)) == Err("e1")

@test
def lift904(): Bool = Result.lift9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Ok(123), Ok(1), Ok(2), Ok(3), Ok(4), Ok(5), Err("e1"), Ok(7), Ok(8)) == Err("e1")

@test
def lift905(): Bool = Result.lift9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Ok(123), Ok(1), Ok(2), Ok(3), Ok(4), Err("e1"), Ok(6), Ok(7), Ok(8)) == Err("e1")

@test
def lift906(): Bool = Result.lift9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Ok(123), Ok(1), Ok(2), Ok(3), Err("e1"), Ok(5), Ok(6), Ok(7), Ok(8)) == Err("e1")

@test
def lift907(): Bool = Result.lift9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Ok(123), Ok(1), Ok(2), Err("e1"), Ok(4), Ok(5), Ok(6), Ok(7), Ok(8)) == Err("e1")

@test
def lift908(): Bool = Result.lift9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Ok(123), Ok(1), Err("e1"), Ok(3), Ok(4), Ok(5), Ok(6), Ok(7), Ok(8)) == Err("e1")

@test
def lift909(): Bool = Result.lift9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Ok(123), Err("e1"), Ok(2), Ok(3), Ok(4), Ok(5), Ok(6), Ok(7), Ok(8)) == Err("e1")

@test
def lift910(): Bool = Result.lift9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Err("e1"), Ok(1), Ok(2), Ok(3), Ok(4), Ok(5), Ok(6), Ok(7), Ok(8)) == Err("e1")

@test
def lift911(): Bool = Result.lift9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Err("e1"), Err("e2"), Err("e3"), Err("e4"), Err("e5"), Err("e6"), Err("e7"), Err("e8"), Err("e9"): Result[Int32, String]) == Err("e1")

/////////////////////////////////////////////////////////////////////////////
// lift10                                                                  //
/////////////////////////////////////////////////////////////////////////////

@test
def lift1001(): Bool = Result.lift10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Ok(123), Ok(1), Ok(2), Ok(3), Ok(4), Ok(5), Ok(6), Ok(7), Ok(8), Ok(9)) == Ok(168)

@test
def lift1002(): Bool = Result.lift10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Ok(123), Ok(1), Ok(2), Ok(3), Ok(4), Ok(5), Ok(6), Ok(7), Ok(8), Err("e1")) == Err("e1")

@test
def lift1003(): Bool = Result.lift10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Ok(123), Ok(1), Ok(2), Ok(3), Ok(4), Ok(5), Ok(6), Ok(7), Err("e1"), Ok(9)) == Err("e1")

@test
def lift1004(): Bool = Result.lift10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Ok(123), Ok(1), Ok(2), Ok(3), Ok(4), Ok(5), Ok(6), Err("e1"), Ok(8), Ok(9)) == Err("e1")

@test
def lift1005(): Bool = Result.lift10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Ok(123), Ok(1), Ok(2), Ok(3), Ok(4), Ok(5), Err("e1"), Ok(7), Ok(8), Ok(9)) == Err("e1")

@test
def lift1006(): Bool = Result.lift10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Ok(123), Ok(1), Ok(2), Ok(3), Ok(4), Err("e1"), Ok(6), Ok(7), Ok(8), Ok(9)) == Err("e1")

@test
def lift1007(): Bool = Result.lift10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Ok(123), Ok(1), Ok(2), Ok(3), Err("e1"), Ok(5), Ok(6), Ok(7), Ok(8), Ok(9)) == Err("e1")

@test
def lift1008(): Bool = Result.lift10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Ok(123), Ok(1), Ok(2), Err("e1"), Ok(4), Ok(5), Ok(6), Ok(7), Ok(8), Ok(9)) == Err("e1")

@test
def lift1009(): Bool = Result.lift10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Ok(123), Ok(1), Err("e1"), Ok(3), Ok(4), Ok(5), Ok(6), Ok(7), Ok(8), Ok(9)) == Err("e1")

@test
def lift1010(): Bool = Result.lift10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Ok(123), Err("e1"), Ok(2), Ok(3), Ok(4), Ok(5), Ok(6), Ok(7), Ok(8), Ok(9)) == Err("e1")

@test
def lift1011(): Bool = Result.lift10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Err("e1"), Ok(1), Ok(2), Ok(3), Ok(4), Ok(5), Ok(6), Ok(7), Ok(8), Ok(9)) == Err("e1")

@test
def lift1012(): Bool = Result.lift10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Err("e1"), Err("e2"), Err("e3"), Err("e4"), Err("e5"), Err("e6"), Err("e7"), Err("e8"), Err("e9"), Err("e10"): Result[Int32, String]) == Err("e1")

}
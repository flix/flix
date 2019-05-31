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
def toSet01(): Bool = Result.toSet(Err(-1ii)) `assertEq!` #{}

@test
def toSet02(): Bool = Result.toSet(Ok(1)) `assertEq!` #{1}

/////////////////////////////////////////////////////////////////////////////
// toMap                                                                   //
/////////////////////////////////////////////////////////////////////////////
@test
def toMap01(): Bool = Result.toMap(Err(-1ii)) `assertEq!` @{}

@test
def toMap02(): Bool = Result.toMap(Ok((1, true))) `assertEq!` @{1 -> true}

/////////////////////////////////////////////////////////////////////////////
// toOption                                                                //
/////////////////////////////////////////////////////////////////////////////
@test
def toOption01(): Bool = Result.toOption(Err(-1ii)) `assertEq!` None

@test
def toOption02(): Bool = Result.toOption(Ok(1)) `assertEq!` Some(1)

}
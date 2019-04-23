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

namespace TestOption {

/////////////////////////////////////////////////////////////////////////////
// isEmpty                                                                 //
/////////////////////////////////////////////////////////////////////////////
@test
def isEmpty01(): Bool = assert!(Option.isEmpty(None))

@test
def isEmpty02(): Bool = assertNot!(Option.isEmpty(Some(0)))

@test
def isEmpty03(): Bool = assertNot!(Option.isEmpty(Some(22ii)))

/////////////////////////////////////////////////////////////////////////////
// getWithDefault                                                          //
/////////////////////////////////////////////////////////////////////////////
@test
def getWithDefault01(): Bool = Option.getWithDefault(None, 32) `assertEq!` 32

@test
def getWithDefault02(): Bool = Option.getWithDefault(Some(30), 32) `assertEq!` 30

@test
def getWithDefault03(): Bool = Option.getWithDefault(None, 32ii) `assertEq!` 32ii

@test
def getWithDefault04(): Bool = Option.getWithDefault(Some(30ii), 32ii) `assertEq!` 30ii

/////////////////////////////////////////////////////////////////////////////
// exists                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def exists01(): Bool = assertNot!(Option.exists(i -> i == 2, None))

@test
def exists02(): Bool = assertNot!(Option.exists(i -> i == 2, Some(1)))

@test
def exists03(): Bool = assert!(Option.exists(i -> i == 2, Some(2)))

/////////////////////////////////////////////////////////////////////////////
// forall                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def forall01(): Bool = assert!(Option.forall(i -> i == 2, None))

@test
def forall02(): Bool = assertNot!(Option.forall(i -> i == 2, Some(1)))

@test
def forall03(): Bool = assert!(Option.forall(i -> i == 2, Some(2)))

/////////////////////////////////////////////////////////////////////////////
// filter                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def filter01(): Bool = Option.filter(i -> i == 2, None) `assertEq!` None

@test
def filter02(): Bool = Option.filter(i -> i == 2, Some(1)) `assertEq!` None

@test
def filter03(): Bool = Option.filter(i -> i == 2, Some(2)) `assertEq!` Some(2)

/////////////////////////////////////////////////////////////////////////////
// map                                                                     //
/////////////////////////////////////////////////////////////////////////////
@test
def map01(): Bool = Option.map(i -> 2*i, None) `assertEq!` None

@test
def map02(): Bool = Option.map(i -> 2*i, Some(1)) `assertEq!` Some(2)

@test
def map03(): Bool = Option.map(i -> 2*i, Some(2)) `assertEq!` Some(4)

/////////////////////////////////////////////////////////////////////////////
// map2                                                                    //
/////////////////////////////////////////////////////////////////////////////
@test
def map201(): Bool = Option.map2((i, b) -> if (b) 2*i else 3*i, None, None) `assertEq!` None

@test
def map202(): Bool = Option.map2((i, b) -> if (b) 2*i else 3*i, Some(1), None) `assertEq!` None

@test
def map203(): Bool = Option.map2((i, b) -> if (b) 2*i else 3*i, None, Some(true)) `assertEq!` None

@test
def map204(): Bool = Option.map2((i, b) -> if (b) 2*i else 3*i, Some(1), Some(true)) `assertEq!` Some(2)

@test
def map205(): Bool = Option.map2((i, b) -> if (b) 2*i else 3*i, Some(1), Some(false)) `assertEq!` Some(3)

/////////////////////////////////////////////////////////////////////////////
// flatMap                                                                 //
/////////////////////////////////////////////////////////////////////////////
@test
def flatMap01(): Bool = Option.flatMap(i -> if (i == 1) None else Some(2*i), None) `assertEq!` None

@test
def flatMap02(): Bool = Option.flatMap(i -> if (i == 1) None else Some(2*i), Some(1)) `assertEq!` None

@test
def flatMap03(): Bool = Option.flatMap(i -> if (i == 1) None else Some(2*i), Some(2)) `assertEq!` Some(4)

/////////////////////////////////////////////////////////////////////////////
// flatMap2                                                                //
/////////////////////////////////////////////////////////////////////////////
@test
def flatMap201(): Bool = Option.flatMap2((i, b) -> if (b) Some(2*i) else None, None, None) `assertEq!` None

@test
def flatMap202(): Bool = Option.flatMap2((i, b) -> if (b) Some(2*i) else None, Some(5), None) `assertEq!` None

@test
def flatMap203(): Bool = Option.flatMap2((i, b) -> if (b) Some(2*i) else None, None, Some(true)) `assertEq!` None

@test
def flatMap204(): Bool = Option.flatMap2((i, b) -> if (b) Some(2*i) else None, Some(1), Some(false)) `assertEq!` None

@test
def flatMap205(): Bool = Option.flatMap2((i, b) -> if (b) Some(2*i) else None, Some(1), Some(true)) `assertEq!` Some(2)

/////////////////////////////////////////////////////////////////////////////
// toList                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def toList01(): Bool = Option.toList(None) `assertEq!` Nil

@test
def toList02(): Bool = Option.toList(Some(1)) `assertEq!` 1 :: Nil

/////////////////////////////////////////////////////////////////////////////
// toSet                                                                   //
/////////////////////////////////////////////////////////////////////////////
@test
def toSet01(): Bool = Option.toSet(None) `assertEq!` #{}

@test
def toSet02(): Bool = Option.toSet(Some(1)) `assertEq!` #{1}

/////////////////////////////////////////////////////////////////////////////
// toMap                                                                   //
/////////////////////////////////////////////////////////////////////////////
@test
def toMap01(): Bool = Option.toMap(None) `assertEq!` @{}

@test
def toMap02(): Bool = Option.toMap(Some((1, true))) `assertEq!` @{1 -> true}

/////////////////////////////////////////////////////////////////////////////
// withDefault                                                             //
/////////////////////////////////////////////////////////////////////////////
@test
def withDefault01(): Bool = Option.withDefault(None, None) `assertEq!` None

@test
def withDefault02(): Bool = Option.withDefault(None, Some(1)) `assertEq!` Some(1)

@test
def withDefault03(): Bool = Option.withDefault(Some(1), None) `assertEq!` Some(1)

@test
def withDefault04(): Bool = Option.withDefault(Some(1), Some(2)) `assertEq!` Some(1)

/////////////////////////////////////////////////////////////////////////////
// replace                                                                 //
/////////////////////////////////////////////////////////////////////////////
@test
def replace01(): Bool = Option.replace(3, 4, None) `assertEq!` None

@test
def replace02(): Bool = Option.replace(3, 4, Some(2)) `assertEq!` Some(2)

@test
def replace03(): Bool = Option.replace(3, 4, Some(3)) `assertEq!` Some(4)

@test
def replace04(): Bool = Option.replace(3, 4, Some(4)) `assertEq!` Some(4)

/////////////////////////////////////////////////////////////////////////////
// count                                                                   //
/////////////////////////////////////////////////////////////////////////////
@test
def count01(): Bool = Option.count(i -> i == 2, None) `assertEq!` 0

@test
def count02(): Bool = Option.count(i -> i == 2, Some(1)) `assertEq!` 0

@test
def count03(): Bool = Option.count(i -> i == 2, Some(2)) `assertEq!` 1

/////////////////////////////////////////////////////////////////////////////
// find                                                                    //
/////////////////////////////////////////////////////////////////////////////
@test
def find01(): Bool = Option.find(i -> i == 2, None) `assertEq!` None

@test
def find02(): Bool = Option.find(i -> i == 2, Some(1)) `assertEq!` None

@test
def find03(): Bool = Option.find(i -> i == 2, Some(2)) `assertEq!` Some(2)

/////////////////////////////////////////////////////////////////////////////
// flatten                                                                 //
/////////////////////////////////////////////////////////////////////////////
@test
def flatten01(): Bool = Option.flatten(None) `assertEq!` None

@test
def flatten02(): Bool = Option.flatten(Some(None)) `assertEq!` None

@test
def flatten03(): Bool = Option.flatten(Some(Some(2))) `assertEq!` Some(2)


/////////////////////////////////////////////////////////////////////////////
// foldLeft                                                                //
/////////////////////////////////////////////////////////////////////////////
@test
def foldLeft01(): Bool = assertNot!(Option.foldLeft((b, i) -> if (i == 2 && b) true else false, false, None))

@test
def foldLeft02(): Bool = assertNot!(Option.foldLeft((b, i) -> if (i == 2 && b) true else false, false, Some(1)))

@test
def foldLeft03(): Bool = assertNot!(Option.foldLeft((b, i) -> if (i == 2 && b) true else false, false, Some(2)))

@test
def foldLeft04(): Bool = assertNot!(Option.foldLeft((b, i) -> if (i == 2 && b) true else false, true, Some(1)))

@test
def foldLeft05(): Bool = assert!(Option.foldLeft((b, i) -> if (i == 2 && b) true else false, true, Some(2)))

/////////////////////////////////////////////////////////////////////////////
// foldRight                                                               //
/////////////////////////////////////////////////////////////////////////////
@test
def foldRight01(): Bool = assertNot!(Option.foldRight((i, b) -> if (i == 2 && b) true else false, None, false))

@test
def foldRight02(): Bool = assertNot!(Option.foldRight((i, b) -> if (i == 2 && b) true else false, Some(1), false))

@test
def foldRight03(): Bool = assertNot!(Option.foldRight((i, b) -> if (i == 2 && b) true else false, Some(2), false))

@test
def foldRight04(): Bool = assertNot!(Option.foldRight((i, b) -> if (i == 2 && b) true else false, Some(1), true))

@test
def foldRight05(): Bool = assert!(Option.foldRight((i, b) -> if (i == 2 && b) true else false, Some(2), true))

/////////////////////////////////////////////////////////////////////////////
// isNone                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def isNone01(): Bool = assert!(Option.isNone(None))

@test
def isNone02(): Bool = assertNot!(Option.isNone(Some(1)))

/////////////////////////////////////////////////////////////////////////////
// isSome                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def isSome01(): Bool = assertNot!(Option.isSome(None))

@test
def isSome02(): Bool = assert!(Option.isSome(Some(1)))

/////////////////////////////////////////////////////////////////////////////
// zip                                                                     //
/////////////////////////////////////////////////////////////////////////////
@test
def zip01(): Bool = Option.zip(None, None) `assertEq!` None

@test
def zip02(): Bool = Option.zip(Some(1), None) `assertEq!` None

@test
def zip03(): Bool = Option.zip(None, Some(true)) `assertEq!` None

@test
def zip04(): Bool = Option.zip(Some(1), Some(true)) `assertEq!` Some((1, true))

/////////////////////////////////////////////////////////////////////////////
// unzip                                                                   //
/////////////////////////////////////////////////////////////////////////////
@test
def unzip01(): Bool = Option.unzip(None) `assertEq!` (None, None)

@test
def unzip02(): Bool = Option.unzip(Some((1, true))) `assertEq!` (Some(1), Some(true))

}

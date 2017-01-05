/*
 * Copyright 2016 Liam Palmer
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
// size                                                                    //
/////////////////////////////////////////////////////////////////////////////
@test
def size01: Bool = Set/size(#{}) `assertEq!` 0

@test
def size02: Bool = Set/size(#{1}) `assertEq!` 1

/////////////////////////////////////////////////////////////////////////////
// empty                                                                   //
/////////////////////////////////////////////////////////////////////////////
@test
def empty01: Bool = Set/empty() `assertEq!` #{}

/////////////////////////////////////////////////////////////////////////////
// singleton                                                               //
/////////////////////////////////////////////////////////////////////////////
@test
def singleton01: Bool = Set/singleton(1) `assertEq!` #{1}

@test
def singleton02: Bool = Set/singleton(2) `assertEq!` #{2}

@test
def singleton03: Bool = Set/singleton(true) `assertEq!` #{true}

/////////////////////////////////////////////////////////////////////////////
// insert                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def insert01: Bool = Set/insert(1, #{}) `assertEq!` #{1}

@test
def insert02: Bool = Set/insert(1, #{1}) `assertEq!` #{1}
/*
@test
def insert03: Bool = Set/insert(1, #{2}) `assertEq!` #{1, 2}

@test
def insert04: Bool = Set/insert(1, #{2, 3}) `assertEq!` #{1, 2, 3}

@test
def insert05: Bool = Set/insert(1, #{1, 2, 3}) `assertEq!` #{1, 2, 3}

@test
def insert06: Bool = Set/insert(1, #{2, 1, 3}) `assertEq!` #{2, 1, 3}
*/

/////////////////////////////////////////////////////////////////////////////
// delete                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def delete01: Bool = Set/delete(1, #{}) `assertEq!` #{}

@test
def delete02: Bool = Set/delete(1, #{1}) `assertEq!` #{}

@test
def delete03: Bool = Set/delete(1, #{2}) `assertEq!` #{2}

/////////////////////////////////////////////////////////////////////////////
// null                                                                    //
/////////////////////////////////////////////////////////////////////////////
@test
def null01: Bool = Set/null(#{}) `assertEq!` true

@test
def null02: Bool = Set/null(#{1}) `assertEq!` false

/////////////////////////////////////////////////////////////////////////////
// memberOf                                                                //
/////////////////////////////////////////////////////////////////////////////
@test
def memberOf01: Bool = Set/memberOf(1, #{}) `assertEq!` false

@test
def memberOf02: Bool = Set/memberOf(1, #{2}) `assertEq!` false

@test
def memberOf03: Bool = Set/memberOf(1, #{1}) `assertEq!` true

/////////////////////////////////////////////////////////////////////////////
// isSubsetOf                                                              //
/////////////////////////////////////////////////////////////////////////////
@test
def isSubsetOf01: Bool = Set/isSubsetOf(#{}, #{}) `assertEq!` true

@test
def isSubsetOf02: Bool = Set/isSubsetOf(#{}, #{1}) `assertEq!` true

@test
def isSubsetOf03: Bool = Set/isSubsetOf(#{1}, #{1}) `assertEq!` true

@test
def isSubsetOf04: Bool = Set/isSubsetOf(#{1}, #{2}) `assertEq!` false

/////////////////////////////////////////////////////////////////////////////
// isProperSubsetOf                                                        //
/////////////////////////////////////////////////////////////////////////////
@test
def isProperSubsetOf01: Bool = Set/isProperSubsetOf(#{}, #{}) `assertEq!` false

@test
def isProperSubsetOf02: Bool = Set/isProperSubsetOf(#{}, #{1}) `assertEq!` true

@test
def isProperSubsetOf03: Bool = Set/isProperSubsetOf(#{1}, #{1}) `assertEq!` false

@test
def isProperSubsetOf04: Bool = Set/isProperSubsetOf(#{1}, #{2}) `assertEq!` false

/////////////////////////////////////////////////////////////////////////////
// union                                                                   //
/////////////////////////////////////////////////////////////////////////////
@test
def union01: Bool = Set/union(#{}, #{}) `assertEq!` #{}

@test
def union02: Bool = Set/union(#{1}, #{}) `assertEq!` #{1}

@test
def union03: Bool = Set/union(#{}, #{2}) `assertEq!` #{2}

/////////////////////////////////////////////////////////////////////////////
// intersection                                                            //
/////////////////////////////////////////////////////////////////////////////
@test
def intersection01: Bool = Set/intersection(#{}, #{}) `assertEq!` #{}

@test
def intersection02: Bool = Set/intersection(#{1}, #{}) `assertEq!` #{}

@test
def intersection03: Bool = Set/intersection(#{}, #{2}) `assertEq!` #{}

@test
def intersection04: Bool = Set/intersection(#{1}, #{2}) `assertEq!` #{}

@test
def intersection05: Bool = Set/intersection(#{1}, #{1}) `assertEq!` #{1}

/////////////////////////////////////////////////////////////////////////////
// difference                                                              //
/////////////////////////////////////////////////////////////////////////////
@test
def difference01: Bool = Set/difference(#{}, #{}) `assertEq!` #{}

@test
def difference02: Bool = Set/difference(#{}, #{2}) `assertEq!` #{}

@test
def difference03: Bool = Set/difference(#{1}, #{}) `assertEq!` #{1}

@test
def difference04: Bool = Set/difference(#{1}, #{2}) `assertEq!` #{1}

//@test
//def difference05: Bool = Set/difference(#{1}, #{1}) `assertEq!` #{1}

/////////////////////////////////////////////////////////////////////////////
// subsets                                                                 //
/////////////////////////////////////////////////////////////////////////////
/*
@test
def subsets01: Bool = Set/subsets(#{}) `assertEq!` #{#{}}

@test
def subsets02: Bool = Set/subsets(#{1}) `assertEq!` #{#{}, #{1}}
*/

/////////////////////////////////////////////////////////////////////////////
// filter                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def filter01: Bool = Set/filter(x -> x % 2 == 0, #{}) `assertEq!` #{}

@test
def filter02: Bool = Set/filter(x -> x % 2 == 0, #{1}) `assertEq!` #{}

@test
def filter03: Bool = Set/filter(x -> x % 2 == 0, #{2}) `assertEq!` #{2}

/////////////////////////////////////////////////////////////////////////////
// map                                                                     //
/////////////////////////////////////////////////////////////////////////////
@test
def map01: Bool = Set/map(x -> x % 2 == 0, #{}) `assertEq!` #{}

@test
def map02: Bool = Set/map(x -> x % 2 == 0, #{1}) `assertEq!` #{false}

@test
def map03: Bool = Set/map(x -> x % 2 == 0, #{2}) `assertEq!` #{true}

/////////////////////////////////////////////////////////////////////////////
// flatMap                                                                     //
/////////////////////////////////////////////////////////////////////////////
@test
def flatMap01: Bool = Set/flatMap(x -> if (x % 2 == 0) #{} else #{x}, #{}) `assertEq!` #{}

@test
def flatMap02: Bool = Set/flatMap(x -> if (x % 2 == 0) #{} else #{x}, #{2}) `assertEq!` #{}

@test
def flatMap03: Bool = Set/flatMap(x -> if (x % 2 == 0) #{} else #{x}, #{1}) `assertEq!` #{1}
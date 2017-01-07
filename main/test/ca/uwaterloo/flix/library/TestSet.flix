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
// size                                                                    //
/////////////////////////////////////////////////////////////////////////////
@test
def size01: Bool = Set/size(#{}) `assertEq!` 0

@test
def size02: Bool = Set/size(#{1}) `assertEq!` 1

@test
def size03: Bool = Set/size(#{1, 2}) `assertEq!` 2

@test
def size04: Bool = Set/size(#{1, 2, 3}) `assertEq!` 3

@test
def size05: Bool = Set/size(#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 14}) `assertEq!` 13

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

@test
def singleton04: Bool = Set/singleton(false) `assertEq!` #{false}

/////////////////////////////////////////////////////////////////////////////
// insert                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def insert01: Bool = Set/insert(1, #{}) `assertEq!` #{1}

@test
def insert02: Bool = Set/insert(1, #{1}) `assertEq!` #{1}

@test
def insert03: Bool = Set/insert(1, #{2}) `assertEq!` #{1, 2}

@test
def insert04: Bool = Set/insert(1, #{2, 3}) `assertEq!` #{1, 2, 3}

@test
def insert05: Bool = Set/insert(1, #{1, 2, 3}) `assertEq!` #{1, 2, 3}

@test
def insert06: Bool = Set/insert(1, #{2, 1, 3}) `assertEq!` #{2, 1, 3}

@test
def insert07: Bool = Set/insert(1, #{2, 1, 3, 7, 8, 9, 10}) `assertEq!` #{2, 1, 3, 7, 8, 9, 10}

@test
def insert08: Bool = Set/insert(11, #{2, 1, 3, 7, 8, 9, 10, 35}) `assertEq!` #{11, 2, 1, 3, 7, 8, 9, 10, 35}

/////////////////////////////////////////////////////////////////////////////
// delete                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def delete01: Bool = Set/delete(1, #{}) `assertEq!` #{}

@test
def delete02: Bool = Set/delete(1, #{1}) `assertEq!` #{}

@test
def delete03: Bool = Set/delete(1, #{2}) `assertEq!` #{2}

@test
def delete04: Bool = Set/delete(2, #{2, 3}) `assertEq!` #{3}

@test
def delete05: Bool = Set/delete(3, #{2, 3}) `assertEq!` #{2}

@test
def delete06: Bool = Set/delete(1, #{2, 3}) `assertEq!` #{2, 3}

@test
def delete07: Bool = Set/delete(0, #{1, 2, 3, 4, 5, 6, 7}) `assertEq!` #{1, 2, 3, 4, 5, 6, 7}

@test
def delete08: Bool = Set/delete(1, #{1, 2, 3, 4, 5, 6, 7}) `assertEq!` #{2, 3, 4, 5, 6, 7}

@test
def delete09: Bool = Set/delete(2, #{1, 2, 3, 4, 5, 6, 7}) `assertEq!` #{1, 3, 4, 5, 6, 7}

@test
def delete10: Bool = Set/delete(6, #{1, 2, 3, 4, 5, 6, 7}) `assertEq!` #{1, 2, 3, 4, 5, 7}

@test
def delete11: Bool = Set/delete(7, #{1, 2, 3, 4, 5, 6, 7}) `assertEq!` #{1, 2, 3, 4, 5, 6}

@test
def delete12: Bool = Set/delete(8, #{1, 2, 3, 4, 5, 6, 7}) `assertEq!` #{1, 2, 3, 4, 5, 6, 7}

/////////////////////////////////////////////////////////////////////////////
// null                                                                    //
/////////////////////////////////////////////////////////////////////////////
@test
def null01: Bool = assert!(Set/null(#{}))

@test
def null02: Bool = assertNot!(Set/null(#{1}))

@test
def null03: Bool = assertNot!(Set/null(#{1, 2}))

@test
def null04: Bool = assertNot!(Set/null(#{1, 2, 3}))

@test
def null05: Bool = assertNot!(Set/null(#{1, 2, 3, 4, 5, 6, 7, 8}))

/////////////////////////////////////////////////////////////////////////////
// memberOf                                                                //
/////////////////////////////////////////////////////////////////////////////
@test
def memberOf01: Bool = assertNot!(Set/memberOf(1, #{}))

@test
def memberOf02: Bool = assertNot!(Set/memberOf(1, #{2}))

@test
def memberOf03: Bool = assert!(Set/memberOf(1, #{1}))

@test
def memberOf04: Bool = assert!(Set/memberOf(1, #{1, 2}))

@test
def memberOf05: Bool = assert!(Set/memberOf(2, #{1, 2}))

@test
def memberOf06: Bool = assertNot!(Set/memberOf(0, #{1, 2}))

@test
def memberOf07: Bool = assertNot!(Set/memberOf(3, #{1, 2}))

@test
def memberOf08: Bool = assertNot!(Set/memberOf(0, #{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}))

@test
def memberOf09: Bool = assert!(Set/memberOf(1, #{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}))

@test
def memberOf10: Bool = assert!(Set/memberOf(2, #{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}))

@test
def memberOf11: Bool = assert!(Set/memberOf(10, #{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}))

@test
def memberOf12: Bool = assertNot!(Set/memberOf(12, #{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}))

/////////////////////////////////////////////////////////////////////////////
// isSubsetOf                                                              //
/////////////////////////////////////////////////////////////////////////////
@test
def isSubsetOf01: Bool = assert!(Set/isSubsetOf(#{}, #{}))

@test
def isSubsetOf02: Bool = assert!(Set/isSubsetOf(#{}, #{1}))

@test
def isSubsetOf03: Bool = assert!(Set/isSubsetOf(#{1}, #{1}))

@test
def isSubsetOf04: Bool = assertNot!(Set/isSubsetOf(#{1}, #{2}))

@test
def isSubsetOf05: Bool = assert!(Set/isSubsetOf(#{}, #{1, 2}))

@test
def isSubsetOf06: Bool = assert!(Set/isSubsetOf(#{1}, #{1, 2}))

@test
def isSubsetOf07: Bool = assert!(Set/isSubsetOf(#{2}, #{1, 2}))

@test
def isSubsetOf08: Bool = assertNot!(Set/isSubsetOf(#{3}, #{1, 2}))

@test
def isSubsetOf09: Bool = assert!(Set/isSubsetOf(#{1, 2}, #{1, 2}))

@test
def isSubsetOf10: Bool = assert!(Set/isSubsetOf(#{2, 1}, #{1, 2}))

@test
def isSubsetOf11: Bool = assertNot!(Set/isSubsetOf(#{3, 1}, #{1, 2}))

@test
def isSubsetOf12: Bool = assertNot!(Set/isSubsetOf(#{1, 2, 3}, #{1, 2}))

@test
def isSubsetOf13: Bool = assert!(Set/isSubsetOf(#{10}, #{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}))

@test
def isSubsetOf14: Bool = assert!(Set/isSubsetOf(#{9, 1}, #{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}))

@test
def isSubsetOf15: Bool = assert!(Set/isSubsetOf(#{6, 5, 8}, #{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}))

@test
def isSubsetOf16: Bool = assertNot!(Set/isSubsetOf(#{10, 2, 3, 6, -1, 4}, #{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}))

/////////////////////////////////////////////////////////////////////////////
// isProperSubsetOf                                                        //
/////////////////////////////////////////////////////////////////////////////
@test
def isProperSubsetOf01: Bool = assertNot!(Set/isProperSubsetOf(#{}, #{}))

@test
def isProperSubsetOf02: Bool = assert!(Set/isProperSubsetOf(#{}, #{1}))

@test
def isProperSubsetOf03: Bool = assertNot!(Set/isProperSubsetOf(#{1}, #{1}))

@test
def isProperSubsetOf04: Bool = assertNot!(Set/isProperSubsetOf(#{1}, #{2}))

@test
def isProperSubsetOf05: Bool = assert!(Set/isProperSubsetOf(#{}, #{1, 2}))

@test
def isProperSubsetOf06: Bool = assert!(Set/isProperSubsetOf(#{1}, #{1, 2}))

@test
def isProperSubsetOf07: Bool = assert!(Set/isProperSubsetOf(#{2}, #{1, 2}))

@test
def isProperSubsetOf08: Bool = assertNot!(Set/isProperSubsetOf(#{3}, #{1, 2}))

@test
def isProperSubsetOf09: Bool = assertNot!(Set/isProperSubsetOf(#{1, 2}, #{1, 2}))

@test
def isProperSubsetOf10: Bool = assertNot!(Set/isProperSubsetOf(#{2, 1}, #{1, 2}))

@test
def isProperSubsetOf11: Bool = assertNot!(Set/isProperSubsetOf(#{3, 1}, #{1, 2}))

@test
def isProperSubsetOf12: Bool = assertNot!(Set/isProperSubsetOf(#{1, 2, 3}, #{1, 2}))

@test
def isProperSubsetOf13: Bool = assert!(Set/isProperSubsetOf(#{10}, #{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}))

@test
def isProperSubsetOf14: Bool = assert!(Set/isProperSubsetOf(#{9, 1}, #{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}))

@test
def isProperSubsetOf15: Bool = assert!(Set/isProperSubsetOf(#{6, 5, 8}, #{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}))

@test
def isProperSubsetOf16: Bool = assertNot!(Set/isProperSubsetOf(#{10, 2, 3, 6, -1, 4}, #{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}))

@test
def isProperSubsetOf17: Bool = assertNot!(Set/isProperSubsetOf(#{10, 2, 3, 6, 1, 4, 5, 9, 8, 7}, #{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}))

/////////////////////////////////////////////////////////////////////////////
// exists                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def exists01: Bool = assertNot!(Set/exists(x -> x % 8 == 7, #{}))

@test
def exists02: Bool = assertNot!(Set/exists(x -> x % 8 == 7, #{5}))

@test
def exists03: Bool = assert!(Set/exists(x -> x % 8 == 7, #{7}))

@test
def exists04: Bool = assert!(Set/exists(x -> x % 8 == 7, #{15}))

@test
def exists05: Bool = assertNot!(Set/exists(x -> x % 8 == 7, #{1, 44}))

@test
def exists06: Bool = assert!(Set/exists(x -> x % 8 == 7, #{11, 71}))

@test
def exists07: Bool = assert!(Set/exists(x -> x % 8 == 7, #{71, 12}))

@test
def exists08: Bool = assert!(Set/exists(x -> x % 8 == 7, #{71, 79}))

@test
def exists09: Bool = assertNot!(Set/exists(x -> x % 8 == 7, #{11, -1, -14, -2, 84, 113}))

@test
def exists10: Bool = assert!(Set/exists(x -> x % 8 == 7, #{11, -1, 31, -14, -2, 84, 111}))

@test
def exists11: Bool = assert!(Set/exists(x -> x % 8 == 7, #{11, -1, -14, -2, 84, 111, 38}))

/////////////////////////////////////////////////////////////////////////////
// forall                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def forall01: Bool = assert!(Set/forall(x -> x % 8 == 7, #{}))

@test
def forall02: Bool = assertNot!(Set/forall(x -> x % 8 == 7, #{5}))

@test
def forall03: Bool = assert!(Set/forall(x -> x % 8 == 7, #{7}))

@test
def forall04: Bool = assert!(Set/forall(x -> x % 8 == 7, #{15}))

@test
def forall05: Bool = assertNot!(Set/forall(x -> x % 8 == 7, #{1, 44}))

@test
def forall06: Bool = assertNot!(Set/forall(x -> x % 8 == 7, #{11, 71}))

@test
def forall07: Bool = assertNot!(Set/forall(x -> x % 8 == 7, #{71, 12}))

@test
def forall08: Bool = assert!(Set/forall(x -> x % 8 == 7, #{71, 79}))

@test
def forall09: Bool = assertNot!(Set/forall(x -> x % 8 == 7, #{7, 15, 23, 84, 111}))

@test
def forall10: Bool = assertNot!(Set/forall(x -> x % 8 == 7, #{7, 15, 23, 111, 3}))

@test
def forall11: Bool = assert!(Set/forall(x -> x % 8 == 7, #{7, 15, 23, 119, 111}))

/////////////////////////////////////////////////////////////////////////////
// union                                                                   //
/////////////////////////////////////////////////////////////////////////////
@test
def union01: Bool = Set/union(#{}, #{}) `assertEq!` #{}

@test
def union02: Bool = Set/union(#{1}, #{}) `assertEq!` #{1}

@test
def union03: Bool = Set/union(#{}, #{2}) `assertEq!` #{2}

@test
def union04: Bool = Set/union(#{1}, #{1}) `assertEq!` #{1}

@test
def union05: Bool = Set/union(#{1}, #{-1}) `assertEq!` #{1, -1}

@test
def union06: Bool = Set/union(#{}, #{-1, 9}) `assertEq!` #{-1, 9}

@test
def union07: Bool = Set/union(#{9}, #{-1, 9}) `assertEq!` #{-1, 9}

@test
def union08: Bool = Set/union(#{4}, #{-1, 9}) `assertEq!` #{4, -1, 9}

@test
def union09: Bool = Set/union(#{9, -1}, #{-1, 9}) `assertEq!` #{-1, 9}

@test
def union10: Bool = Set/union(#{9, 5}, #{-1, 9}) `assertEq!` #{5, -1, 9}

@test
def union11: Bool = Set/union(#{6, 5}, #{-1, 9}) `assertEq!` #{6, 5, -1, 9}

@test
def union12: Bool = Set/union(#{6, -99}, #{6, 5, -1, 9, 43, 7, 8, -99}) `assertEq!` #{6, 5, -1, 9, 43, 7, 8, -99}

@test
def union13: Bool = Set/union(#{6, -99, -1, 5, 22}, #{6, 5, -1, 9, 43, 7, 8, -99}) `assertEq!` #{22, 6, 5, -1, 9, 43, 7, 8, -99}

@test
def union14: Bool = Set/union(#{-2, -3, -4}, #{6, 5, -1, 9, 43, 7, 8, -99}) `assertEq!` #{-2, -3, -4, 6, 5, -1, 9, 43, 7, 8, -99}

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

@test
def intersection06: Bool = Set/intersection(#{}, #{1, 2}) `assertEq!` #{}

@test
def intersection07: Bool = Set/intersection(#{1, 2}, #{}) `assertEq!` #{}

@test
def intersection08: Bool = Set/intersection(#{2}, #{1, 2}) `assertEq!` #{2}

@test
def intersection09: Bool = Set/intersection(#{1}, #{1, 2}) `assertEq!` #{1}

@test
def intersection10: Bool = Set/intersection(#{2, 1}, #{1, 2}) `assertEq!` #{2, 1}

@test
def intersection11: Bool = Set/intersection(#{1, 2}, #{1, 2}) `assertEq!` #{1, 2}

@test
def intersection12: Bool = Set/intersection(#{3, 2}, #{1, 2}) `assertEq!` #{2}

@test
def intersection13: Bool = Set/intersection(#{3, 55}, #{1, 2}) `assertEq!` #{}

@test
def intersection14: Bool = Set/intersection(#{3, 55, 11, 87, 22, 34, -87, 23}, #{1, 2, 84, -87, 87, 3, 44}) `assertEq!` #{3, 87, -87}

@test
def intersection15: Bool = Set/intersection(#{3, 55, 11, 87, 22, 34, -87, 23}, #{23, 1, 2, 84, 87, 3}) `assertEq!` #{3, 87, 23}

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

@test
def difference05: Bool = Set/difference(#{1}, #{1}) `assertEq!` #{}

@test
def difference06: Bool = Set/difference(#{1, 2}, #{}) `assertEq!` #{1, 2}

@test
def difference07: Bool = Set/difference(#{1, 2}, #{1}) `assertEq!` #{2}

@test
def difference08: Bool = Set/difference(#{1, 2}, #{2}) `assertEq!` #{1}

@test
def difference09: Bool = Set/difference(#{1, 2}, #{8, 2, 4}) `assertEq!` #{1}

@test
def difference10: Bool = Set/difference(#{1, 2}, #{3, 1, 2, 4}) `assertEq!` #{}

@test
def difference11: Bool = Set/difference(#{1, 2}, #{3, 11, 21, 4}) `assertEq!` #{1, 2}

@test
def difference12: Bool = Set/difference(#{1, 2, 87, 4, 5, 6, 86, 92, 111, -1}, #{-1, 92, 4, 5, 1, 2, 86}) `assertEq!` #{87, 6, 111}

@test
def difference13: Bool = Set/difference(#{1, 2, 87, 4, 5, 6, 86, 92, 111, -1}, #{-1, 92, 4, 5, 1, 2, 86, 99, 6}) `assertEq!` #{87, 111}

/////////////////////////////////////////////////////////////////////////////
// subsets                                                                 //
/////////////////////////////////////////////////////////////////////////////
@test
def subsets01: Bool = Set/subsets(#{}) `assertEq!` #{#{}}

@test
def subsets02: Bool = Set/subsets(#{1}) `assertEq!` #{#{1}, #{}}

@test
def subsets03: Bool = Set/subsets(#{1, 2}) `assertEq!` #{#{1, 2}, #{1}, #{2}, #{}}

@test
def subsets04: Bool = Set/subsets(#{1, 2, 3}) `assertEq!` #{#{1, 2, 3}, #{1, 2}, #{1, 3}, #{1}, #{2, 3}, #{2}, #{3}, #{}}

/////////////////////////////////////////////////////////////////////////////
// filter                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def filter01: Bool = Set/filter(x -> x % 2 == 0, #{}) `assertEq!` #{}

@test
def filter02: Bool = Set/filter(x -> x % 2 == 0, #{1}) `assertEq!` #{}

@test
def filter03: Bool = Set/filter(x -> x % 2 == 0, #{2}) `assertEq!` #{2}

@test
def filter04: Bool = Set/filter(x -> x % 2 == 0, #{1, 3}) `assertEq!` #{}

@test
def filter05: Bool = Set/filter(x -> x % 2 == 0, #{8, 3}) `assertEq!` #{8}

@test
def filter06: Bool = Set/filter(x -> x % 2 == 0, #{-1, 32}) `assertEq!` #{32}

@test
def filter07: Bool = Set/filter(x -> x % 2 == 0, #{12, 34}) `assertEq!` #{12, 34}

@test
def filter08: Bool = Set/filter(x -> x % 2 == 0, #{-33, -1, 12, 1, 34, 88, 7, 77, 31}) `assertEq!` #{12, 34, 88}

@test
def filter09: Bool = Set/filter(x -> x % 2 == 0, #{-33, -1, 12, 1, 34, 88, 7, 77, 31, 7, -92, 841}) `assertEq!` #{12, 34, 88, -92}

/////////////////////////////////////////////////////////////////////////////
// map                                                                     //
/////////////////////////////////////////////////////////////////////////////
@test
def map01: Bool = Set/map(x -> x % 2 == 0, #{}) `assertEq!` #{}

@test
def map02: Bool = Set/map(x -> x % 2 == 0, #{1}) `assertEq!` #{false}

@test
def map03: Bool = Set/map(x -> x % 2 == 0, #{2}) `assertEq!` #{true}

@test
def map04: Bool = Set/map(x -> x % 2 == 0, #{1, -1}) `assertEq!` #{false}

@test
def map05: Bool = Set/map(x -> x % 2 == 0, #{1, -12}) `assertEq!` #{false, true}

@test
def map06: Bool = Set/map(x -> x % 2 == 0, #{16, -1}) `assertEq!` #{true, false}

@test
def map07: Bool = Set/map(x -> x % 2 == 0, #{12, -12}) `assertEq!` #{true}

@test
def map08: Bool = Set/map(x -> x % 2 == 0, #{12, -12}) `assertEq!` #{true}

@test
def map09: Bool = Set/map(x -> x % 2 == 0, #{12, -12, 1, 14}) `assertEq!` #{false, true}

@test
def map10: Bool = Set/map(x -> x % 2 == 0, #{12, -12, 1, 14, 7, 88, -91}) `assertEq!` #{true, false}

@test
def map11: Bool = Set/map(x -> x % 2 == 0, #{12, -12, 122, 14}) `assertEq!` #{true}

@test
def map12: Bool = Set/map(x -> x % 2 == 0, #{123, -123, 1223, 141}) `assertEq!` #{false}

@test
def map13: Bool = Set/map(x -> x % 9, #{11, 5, 16, 4}) `assertEq!` #{2, 5, 7, 4}

@test
def map14: Bool = Set/map(x -> x % 9, #{0, 5, 1, -9, -8}) `assertEq!` #{5, 1, 0, -8}

@test
def map15: Bool = Set/map(x -> x % 9, #{0, 5, 1, 10, 7, 19, 28, 2}) `assertEq!` #{0, 5, 7, 1, 2}

/////////////////////////////////////////////////////////////////////////////
// flatMap                                                                 //
/////////////////////////////////////////////////////////////////////////////
@test
def flatMap01: Bool = Set/flatMap(x -> if (x % 2 == 0) #{} else #{x}, #{}) `assertEq!` #{}

@test
def flatMap02: Bool = Set/flatMap(x -> if (x % 2 == 0) #{} else #{x}, #{2}) `assertEq!` #{}

@test
def flatMap03: Bool = Set/flatMap(x -> if (x % 2 == 0) #{} else #{x}, #{1}) `assertEq!` #{1}

@test
def flatMap04: Bool = Set/flatMap(x -> if (x % 2 == 0) #{} else #{x}, #{2, 4}) `assertEq!` #{}

@test
def flatMap05: Bool = Set/flatMap(x -> if (x % 2 == 0) #{} else #{x}, #{2, 13}) `assertEq!` #{13}

@test
def flatMap06: Bool = Set/flatMap(x -> if (x % 2 == 0) #{} else #{x}, #{15, -8}) `assertEq!` #{15}

@test
def flatMap07: Bool = Set/flatMap(x -> if (x % 2 == 0) #{} else #{x}, #{1, 11}) `assertEq!` #{1, 11}

@test
def flatMap08: Bool = Set/flatMap(x -> #{x, 2*x}, #{1, 4, 8, 2}) `assertEq!` #{1, 8, 16, 2, 4}

@test
def flatMap09: Bool = Set/flatMap(x -> #{x, 3*x}, #{1, 8, 3, 2, 9, -5, -1}) `assertEq!` #{1, 8, 24, 3, 2, 6, 9, 27, -5, -15, -1, -3}

@test
def flatMap10: Bool = Set/flatMap(x -> #{x, 2*x, 3*x}, #{1, 4, 3, 2}) `assertEq!` #{1, 8, 12, 3, 9, 2, 4, 6}

/////////////////////////////////////////////////////////////////////////////
// partition                                                               //
/////////////////////////////////////////////////////////////////////////////
@test
def partition01: Bool = Set/partition(x -> x % 2 == 0, #{}) `assertEq!` (#{}, #{})

@test
def partition02: Bool = Set/partition(x -> x % 2 == 0, #{1}) `assertEq!` (#{}, #{1})

@test
def partition03: Bool = Set/partition(x -> x % 2 == 0, #{2}) `assertEq!` (#{2}, #{})

@test
def partition04: Bool = Set/partition(x -> x % 2 == 0, #{1, 3}) `assertEq!` (#{}, #{1, 3})

@test
def partition05: Bool = Set/partition(x -> x % 2 == 0, #{1, 2}) `assertEq!` (#{2}, #{1})

@test
def partition06: Bool = Set/partition(x -> x % 2 == 0, #{2, 1}) `assertEq!` (#{2}, #{1})

@test
def partition07: Bool = Set/partition(x -> x % 2 == 0, #{2, -4}) `assertEq!` (#{2, -4}, #{})

@test
def partition08: Bool = Set/partition(x -> x % 2 == 0, #{2, -11, 89, -4, 11, -6, 84}) `assertEq!` (#{2, -4, -6, 84}, #{-11, 89, 11})

@test
def partition09: Bool = Set/partition(x -> x % 2 == 0, #{84, -6, 11, -4, 89, -11, 2}) `assertEq!` (#{84, -6, -4, 2}, #{11, 89, -11})

@test
def partition10: Bool = Set/partition(x -> x % 2 == 0, #{1, 2, 3, 4, 5, 6, 7, 8}) `assertEq!` (#{2, 4, 6, 8}, #{1, 3, 5, 7})
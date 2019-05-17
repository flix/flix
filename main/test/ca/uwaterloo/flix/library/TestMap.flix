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

namespace TestMap {

/////////////////////////////////////////////////////////////////////////////
// size                                                                    //
/////////////////////////////////////////////////////////////////////////////
@test
def size01(): Bool = Map.size(@{}) `assertEq!` 0

@test
def size02(): Bool = Map.size(@{1 -> 2}) `assertEq!` 1

@test
def size03(): Bool = Map.size(@{1 -> 2, 2 -> 4}) `assertEq!` 2

@test
def size04(): Bool = Map.size(@{1 -> 2, 2 -> 4, 3 -> 6}) `assertEq!` 3

@test
def size05(): Bool = Map.size(@{1 -> 2, 2 -> 4, 3 -> 6, 4 -> 8}) `assertEq!` 4

@test
def size06(): Bool = Map.size(@{1 -> 2, 2 -> 4, 3 -> 6, 4 -> 8, 5 -> 10}) `assertEq!` 5

/////////////////////////////////////////////////////////////////////////////
// empty                                                                   //
/////////////////////////////////////////////////////////////////////////////
@test
def empty01(): Bool = assert!(Map.eq(Map.empty(), @{}))

/////////////////////////////////////////////////////////////////////////////
// singleton                                                               //
/////////////////////////////////////////////////////////////////////////////
@test
def singleton01(): Bool = assert!(Map.eq(Map.singleton(1, 2), @{1 -> 2}))

@test
def singleton02(): Bool = assert!(Map.eq(Map.singleton(3, -1), @{3 -> -1}))

@test
def singleton03(): Bool = assert!(Map.eq(Map.singleton(-99, -11), @{-99 -> -11}))

/////////////////////////////////////////////////////////////////////////////
// isEmpty                                                                 //
/////////////////////////////////////////////////////////////////////////////
@test
def isEmpty01(): Bool = assert!(Map.isEmpty(@{}))

@test
def isEmpty02(): Bool = assertNot!(Map.isEmpty(@{1 -> 2}))

@test
def isEmpty03(): Bool = assertNot!(Map.isEmpty(@{1 -> 2, 2 -> 4}))

@test
def isEmpty04(): Bool = assertNot!(Map.isEmpty(@{1 -> 2, 2 -> 4, 3 -> 6}))

@test
def isEmpty05(): Bool = assertNot!(Map.isEmpty(@{1 -> 2, 2 -> 4, 3 -> 6, 4 -> 8}))

/////////////////////////////////////////////////////////////////////////////
// get                                                                     //
/////////////////////////////////////////////////////////////////////////////
@test
def get01(): Bool = Map.get(2, @{}) `assertEq!` None

@test
def get02(): Bool = Map.get(2, @{1 -> 2}) `assertEq!` None

@test
def get03(): Bool = Map.get(2, @{2 -> 1}) `assertEq!` Some(1)

@test
def get04(): Bool = Map.get(5, @{2 -> 1, 3 -> 17}) `assertEq!` None

@test
def get05(): Bool = Map.get(5, @{2 -> 1, 5 -> 17}) `assertEq!` Some(17)

@test
def get06(): Bool = Map.get(5, @{5 -> 1, 3 -> 17}) `assertEq!` Some(1)

@test
def get07(): Bool = Map.get(-2, @{2 -> 1, 3 -> 17, -1 -> -2}) `assertEq!` None

@test
def get08(): Bool = Map.get(-2, @{-2 -> 1, 3 -> 17, -1 -> -2}) `assertEq!` Some(1)

@test
def get09(): Bool = Map.get(-2, @{2 -> 1, -2 -> 17, -1 -> -2}) `assertEq!` Some(17)

@test
def get10(): Bool = Map.get(-2, @{2 -> 1, 3 -> 17, -2 -> -2}) `assertEq!` Some(-2)

/////////////////////////////////////////////////////////////////////////////
// getWithDefault                                                          //
/////////////////////////////////////////////////////////////////////////////
@test
def getWithDefault01(): Bool = Map.getWithDefault(2, 34, @{}) `assertEq!` 34

@test
def getWithDefault02(): Bool = Map.getWithDefault(2, 34, @{1 -> 2}) `assertEq!` 34

@test
def getWithDefault03(): Bool = Map.getWithDefault(2, 34, @{2 -> 1}) `assertEq!` 1

@test
def getWithDefault04(): Bool = Map.getWithDefault(5, 34, @{2 -> 1, 3 -> 17}) `assertEq!` 34

@test
def getWithDefault05(): Bool = Map.getWithDefault(5, 34, @{2 -> 1, 5 -> 17}) `assertEq!` 17

@test
def getWithDefault06(): Bool = Map.getWithDefault(5, 34, @{5 -> 1, 3 -> 17}) `assertEq!` 1

@test
def getWithDefault07(): Bool = Map.getWithDefault(-2, 34, @{2 -> 1, 3 -> 17, -1 -> -2}) `assertEq!` 34

@test
def getWithDefault08(): Bool = Map.getWithDefault(-2, 34, @{-2 -> 1, 3 -> 17, -1 -> -2}) `assertEq!` 1

@test
def getWithDefault09(): Bool = Map.getWithDefault(-2, 34, @{2 -> 1, -2 -> 17, -1 -> -2}) `assertEq!` 17

@test
def getWithDefault10(): Bool = Map.getWithDefault(-2, 34, @{2 -> 1, 3 -> 17, -2 -> -2}) `assertEq!` -2

/////////////////////////////////////////////////////////////////////////////
// memberOf                                                                //
/////////////////////////////////////////////////////////////////////////////
@test
def memberOf01(): Bool = assertNot!(Map.memberOf(2, @{}))

@test
def memberOf02(): Bool = assertNot!(Map.memberOf(2, @{1 -> 2}))

@test
def memberOf03(): Bool = assert!(Map.memberOf(2, @{2 -> 1}))

@test
def memberOf04(): Bool = assertNot!(Map.memberOf(5, @{2 -> 1, 3 -> 17}))

@test
def memberOf05(): Bool = assert!(Map.memberOf(5, @{2 -> 1, 5 -> 17}))

@test
def memberOf06(): Bool = assert!(Map.memberOf(5, @{5 -> 1, 3 -> 17}))

@test
def memberOf07(): Bool = assertNot!(Map.memberOf(-2, @{2 -> 1, 3 -> 17, -1 -> -2}))

@test
def memberOf08(): Bool = assert!(Map.memberOf(-2, @{-2 -> 1, 3 -> 17, -1 -> -2}))

@test
def memberOf09(): Bool = assert!(Map.memberOf(-2, @{2 -> 1, -2 -> 17, -1 -> -2}))

@test
def memberOf10(): Bool = assert!(Map.memberOf(-2, @{2 -> 1, 3 -> 17, -2 -> -2}))

/////////////////////////////////////////////////////////////////////////////
// keysOf                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def keysOf01(): Bool = assert!(Set.eq(Map.keysOf(@{}), #{}))

@test
def keysOf02(): Bool = assert!(Set.eq(Map.keysOf(@{1 -> 2}), #{1}))

@test
def keysOf03(): Bool = assert!(Set.eq(Map.keysOf(@{1 -> 2, 2 -> 4}), #{1, 2}))

@test
def keysOf04(): Bool = assert!(Set.eq(Map.keysOf(@{1 -> 2, 2 -> 4, 3 -> 6}), #{1, 2, 3}))

@test
def keysOf05(): Bool = assert!(Set.eq(Map.keysOf(@{1 -> 2, 2 -> 4, 3 -> 6, 4 -> 8}), #{1, 2, 3, 4}))

@test
def keysOf06(): Bool = assert!(Set.eq(Map.keysOf(@{1 -> 2, 2 -> 4, 3 -> 6, 4 -> 8, 5 -> 10}), #{1, 2, 3, 4, 5}))

/////////////////////////////////////////////////////////////////////////////
// valuesOf                                                                //
/////////////////////////////////////////////////////////////////////////////
@test
def valuesOf01(): Bool = Map.valuesOf(@{}) `assertEq!` Nil

@test
def valuesOf02(): Bool = Map.valuesOf(@{1 -> 2}) `assertEq!` 2 :: Nil

@test
def valuesOf03(): Bool = Map.valuesOf(@{1 -> 2, 2 -> 4}) `assertEq!` 4 :: 2 :: Nil

@test
def valuesOf04(): Bool = Map.valuesOf(@{1 -> 2, 2 -> 4, 3 -> 6}) `assertEq!` 6 :: 4 :: 2 :: Nil

@test
def valuesOf05(): Bool = Map.valuesOf(@{1 -> 2, 2 -> 4, 3 -> 6, 4 -> 8}) `assertEq!` 8 :: 6 :: 4 :: 2 :: Nil

@test
def valuesOf06(): Bool = Map.valuesOf(@{1 -> 2, 2 -> 4, 3 -> 6, 4 -> 8, 5 -> 10}) `assertEq!` 10 :: 8 :: 6 :: 4 :: 2 :: Nil

@test
def valuesOf07(): Bool = Map.valuesOf(@{1 -> -11, 2 -> 4, 3 -> -5, 4 -> 7, 5 -> -5}) `assertEq!` -5 :: 7 :: -5 :: 4 :: -11 :: Nil

/////////////////////////////////////////////////////////////////////////////
// insert                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def insert01(): Bool = assert!(Map.eq(Map.insert(1, 3, @{}), @{1 -> 3}))

@test
def insert02(): Bool = assert!(Map.eq(Map.insert(1, 3, @{1 -> 4}), @{1 -> 3}))

@test
def insert03(): Bool = assert!(Map.eq(Map.insert(2, 3, @{1 -> 4}), @{1 -> 4, 2 -> 3}))

@test
def insert04(): Bool = assert!(Map.eq(Map.insert(1, 1, @{1 -> 4, 5 -> -2}), @{1 -> 1, 5 -> -2}))

@test
def insert05(): Bool = assert!(Map.eq(Map.insert(5, 1, @{1 -> 4, 5 -> -2}), @{1 -> 4, 5 -> 1}))

@test
def insert06(): Bool = assert!(Map.eq(Map.insert(4, -2, @{1 -> 4, 5 -> -2}), @{1 -> 4, 5 -> -2, 4 -> -2}))

/////////////////////////////////////////////////////////////////////////////
// insertWith                                                              //
/////////////////////////////////////////////////////////////////////////////
@test
def insertWith01(): Bool = assert!(Map.eq(Map.insertWith((v1, v2) -> v1 + v2, 1, 3, @{}), @{1 -> 3}))

@test
def insertWith02(): Bool = assert!(Map.eq(Map.insertWith((v1, v2) -> v1 + v2, 1, 3, @{1 -> 4}), @{1 -> 7}))

@test
def insertWith03(): Bool = assert!(Map.eq(Map.insertWith((v1, v2) -> v1 + v2, 2, 3, @{1 -> 4}), @{1 -> 4, 2 -> 3}))

@test
def insertWith04(): Bool = assert!(Map.eq(Map.insertWith((v1, v2) -> v1 + v2, 1, 1, @{1 -> 4, 5 -> -2}), @{1 -> 5, 5 -> -2}))

@test
def insertWith05(): Bool = assert!(Map.eq(Map.insertWith((v1, v2) -> v1 + v2, 5, 1, @{1 -> 4, 5 -> -2}), @{1 -> 4, 5 -> -1}))

@test
def insertWith06(): Bool = assert!(Map.eq(Map.insertWith((v1, v2) -> v1 + v2, 4, -2, @{1 -> 4, 5 -> -2}), @{1 -> 4, 5 -> -2, 4 -> -2}))

/////////////////////////////////////////////////////////////////////////////
// insertWithKey                                                           //
/////////////////////////////////////////////////////////////////////////////
@test
def insertWithKey01(): Bool = assert!(Map.eq(Map.insertWithKey((k, v1, v2) -> k + v1 + v2, 1, 3, @{}), @{1 -> 3}))

@test
def insertWithKey02(): Bool = assert!(Map.eq(Map.insertWithKey((k, v1, v2) -> k + v1 + v2, 1, 3, @{1 -> 4}), @{1 -> 8}))

@test
def insertWithKey03(): Bool = assert!(Map.eq(Map.insertWithKey((k, v1, v2) -> k + v1 + v2, 2, 3, @{1 -> 4}), @{1 -> 4, 2 -> 3}))

@test
def insertWithKey04(): Bool = assert!(Map.eq(Map.insertWithKey((k, v1, v2) -> k + v1 + v2, 1, 1, @{1 -> 4, 5 -> -2}), @{1 -> 6, 5 -> -2}))

@test
def insertWithKey05(): Bool = assert!(Map.eq(Map.insertWithKey((k, v1, v2) -> k + v1 + v2, 5, 1, @{1 -> 4, 5 -> -2}), @{1 -> 4, 5 -> 4}))

@test
def insertWithKey06(): Bool = assert!(Map.eq(Map.insertWithKey((k, v1, v2) -> k + v1 + v2, 4, -2, @{1 -> 4, 5 -> -2}), @{1 -> 4, 5 -> -2, 4 -> -2}))

/////////////////////////////////////////////////////////////////////////////
// adjust                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def adjust01(): Bool = assert!(Map.eq(Map.adjust(v -> 2*v, 1, @{}), @{}))

@test
def adjust02(): Bool = assert!(Map.eq(Map.adjust(v -> 2*v, 1, @{1 -> 4}), @{1 -> 8}))

@test
def adjust03(): Bool = assert!(Map.eq(Map.adjust(v -> 2*v, 2, @{1 -> 4}), @{1 -> 4}))

@test
def adjust04(): Bool = assert!(Map.eq(Map.adjust(v -> 2*v, 1, @{1 -> -14, 5 -> -2}), @{1 -> -28, 5 -> -2}))

@test
def adjust05(): Bool = assert!(Map.eq(Map.adjust(v -> 2*v, 5, @{1 -> 4, 5 -> -2}), @{1 -> 4, 5 -> -4}))

@test
def adjust06(): Bool = assert!(Map.eq(Map.adjust(v -> 2*v, 4, @{1 -> 4, 5 -> -2}), @{1 -> 4, 5 -> -2}))

/////////////////////////////////////////////////////////////////////////////
// adjustWithKey                                                           //
/////////////////////////////////////////////////////////////////////////////
@test
def adjustWithKey01(): Bool = assert!(Map.eq(Map.adjustWithKey((k, v) -> k+v, 1, @{}), @{}))

@test
def adjustWithKey02(): Bool = assert!(Map.eq(Map.adjustWithKey((k, v) -> k+v, 1, @{1 -> 4}), @{1 -> 5}))

@test
def adjustWithKey03(): Bool = assert!(Map.eq(Map.adjustWithKey((k, v) -> k+v, 2, @{1 -> 4}), @{1 -> 4}))

@test
def adjustWithKey04(): Bool = assert!(Map.eq(Map.adjustWithKey((k, v) -> k+v, 1, @{1 -> -14, 5 -> -2}), @{1 -> -13, 5 -> -2}))

@test
def adjustWithKey05(): Bool = assert!(Map.eq(Map.adjustWithKey((k, v) -> k+v, 5, @{1 -> 4, 5 -> -2}), @{1 -> 4, 5 -> 3}))

@test
def adjustWithKey06(): Bool = assert!(Map.eq(Map.adjustWithKey((k, v) -> k+v, 4, @{1 -> 4, 5 -> -2}), @{1 -> 4, 5 -> -2}))

/////////////////////////////////////////////////////////////////////////////
// update                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def update01(): Bool = assert!(Map.eq(Map.update(v -> if (v % 2 != 0) Some(2*v) else None, 1, @{}), @{}))

@test
def update02(): Bool = assert!(Map.eq(Map.update(v -> if (v % 2 != 0) Some(2*v) else None, 1, @{1 -> 3}), @{1 -> 6}))

@test
def update03(): Bool = assert!(Map.eq(Map.update(v -> if (v % 2 != 0) Some(2*v) else None, 1, @{1 -> 4}), @{1 -> 4}))

@test
def update04(): Bool = assert!(Map.eq(Map.update(v -> if (v % 2 != 0) Some(2*v) else None, 2, @{1 -> 4}), @{1 -> 4}))

@test
def update05(): Bool = assert!(Map.eq(Map.update(v -> if (v % 2 != 0) Some(2*v) else None, 1, @{1 -> -14, 5 -> -2}), @{1 -> -14, 5 -> -2}))

@test
def update06(): Bool = assert!(Map.eq(Map.update(v -> if (v % 2 != 0) Some(2*v) else None, 1, @{1 -> -13, 5 -> -2}), @{1 -> -26, 5 -> -2}))

@test
def update07(): Bool = assert!(Map.eq(Map.update(v -> if (v % 2 != 0) Some(2*v) else None, 5, @{1 -> 4, 5 -> -2}), @{1 -> 4, 5 -> -2}))

@test
def update08(): Bool = assert!(Map.eq(Map.update(v -> if (v % 2 != 0) Some(2*v) else None, 5, @{1 -> 4, 5 -> -1}), @{1 -> 4, 5 -> -2}))

@test
def update09(): Bool = assert!(Map.eq(Map.update(v -> if (v % 2 != 0) Some(2*v) else None, 4, @{1 -> 4, 5 -> -2}), @{1 -> 4, 5 -> -2}))

/////////////////////////////////////////////////////////////////////////////
// updateWithKey                                                           //
/////////////////////////////////////////////////////////////////////////////
@test
def updateWithKey01(): Bool = assert!(Map.eq(Map.updateWithKey((k, v) -> if (v % 2 != 0) Some(k+2*v) else None, 1, @{}), @{}))

@test
def updateWithKey02(): Bool = assert!(Map.eq(Map.updateWithKey((k, v) -> if (v % 2 != 0) Some(k+2*v) else None, 1, @{1 -> 3}), @{1 -> 7}))

@test
def updateWithKey03(): Bool = assert!(Map.eq(Map.updateWithKey((k, v) -> if (v % 2 != 0) Some(k+2*v) else None, 1, @{1 -> 4}), @{1 -> 4}))

@test
def updateWithKey04(): Bool = assert!(Map.eq(Map.updateWithKey((k, v) -> if (v % 2 != 0) Some(k+2*v) else None, 2, @{1 -> 4}), @{1 -> 4}))

@test
def updateWithKey05(): Bool = assert!(Map.eq(Map.updateWithKey((k, v) -> if (v % 2 != 0) Some(k+2*v) else None, 1, @{1 -> -14, 5 -> -2}), @{1 -> -14, 5 -> -2}))

@test
def updateWithKey06(): Bool = assert!(Map.eq(Map.updateWithKey((k, v) -> if (v % 2 != 0) Some(k+2*v) else None, 1, @{1 -> -13, 5 -> -2}), @{1 -> -25, 5 -> -2}))

@test
def updateWithKey07(): Bool = assert!(Map.eq(Map.updateWithKey((k, v) -> if (v % 2 != 0) Some(k+2*v) else None, 5, @{1 -> 4, 5 -> -2}), @{1 -> 4, 5 -> -2}))

@test
def updateWithKey08(): Bool = assert!(Map.eq(Map.updateWithKey((k, v) -> if (v % 2 != 0) Some(k+2*v) else None, 5, @{1 -> 4, 5 -> -1}), @{1 -> 4, 5 -> 3}))

@test
def updateWithKey09(): Bool = assert!(Map.eq(Map.updateWithKey((k, v) -> if (v % 2 != 0) Some(k+2*v) else None, 4, @{1 -> 4, 5 -> -2}), @{1 -> 4, 5 -> -2}))

/////////////////////////////////////////////////////////////////////////////
// replace                                                                 //
/////////////////////////////////////////////////////////////////////////////
@test
def replace01(): Bool = assert!(Map.eq(Map.replace((1, true), (2, false), @{}), @{}))

@test
def replace02(): Bool = assert!(Map.eq(Map.replace((1, true), (2, false), @{1 -> false}), @{1 -> false}))

@test
def replace03(): Bool = assert!(Map.eq(Map.replace((1, true), (2, false), @{1 -> true}), @{2 -> false}))

@test
def replace04(): Bool = assert!(Map.eq(Map.replace((1, true), (1, false), @{1 -> true}), @{1 -> false}))

@test
def replace05(): Bool = assert!(Map.eq(Map.replace((1, true), (2, false), @{1 -> false, 3 -> true}), @{1 -> false, 3 -> true}))

@test
def replace06(): Bool = assert!(Map.eq(Map.replace((1, true), (2, false), @{1 -> true, 3 -> true}), @{2 -> false, 3 -> true}))

@test
def replace07(): Bool = assert!(Map.eq(Map.replace((1, true), (2, false), @{3 -> false, 1 -> true}), @{3 -> false, 2 -> false}))

@test
def replace08(): Bool = assert!(Map.eq(Map.replace((1, true), (2, false), @{1 -> true, 2 -> true}), @{2 -> false}))

/////////////////////////////////////////////////////////////////////////////
// delete                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def delete01(): Bool = assert!(Map.eq(Map.delete(1, @{}), @{}))

@test
def delete02(): Bool = assert!(Map.eq(Map.delete(1, @{2 -> 4}), @{2 -> 4}))

@test
def delete03(): Bool = assert!(Map.eq(Map.delete(1, @{1 -> 4}), @{}))

@test
def delete04(): Bool = assert!(Map.eq(Map.delete(1, @{6 -> 1, 2 -> 4}), @{6 -> 1, 2 -> 4}))

@test
def delete05(): Bool = assert!(Map.eq(Map.delete(6, @{6 -> 1, 2 -> 4}), @{2 -> 4}))

@test
def delete06(): Bool = assert!(Map.eq(Map.delete(2, @{6 -> 1, 2 -> 4}), @{6 -> 1}))

@test
def delete07(): Bool = assert!(Map.eq(Map.delete(1, @{6 -> 1, 2 -> 4, 8 -> 89}), @{6 -> 1, 2 -> 4, 8 -> 89}))

@test
def delete08(): Bool = assert!(Map.eq(Map.delete(6, @{6 -> 1, 2 -> 4, 8 -> 89}), @{2 -> 4, 8 -> 89}))

@test
def delete09(): Bool = assert!(Map.eq(Map.delete(2, @{6 -> 1, 2 -> 4, 8 -> 89}), @{6 -> 1, 8 -> 89}))

@test
def delete10(): Bool = assert!(Map.eq(Map.delete(8, @{6 -> 1, 2 -> 4, 8 -> 89}), @{6 -> 1, 2 -> 4}))

/////////////////////////////////////////////////////////////////////////////
// isSubmapOf                                                              //
/////////////////////////////////////////////////////////////////////////////
@test
def isSubmapOf01(): Bool = assert!(Map.isSubmapOf(@{}, @{}))

@test
def isSubmapOf02(): Bool = assertNot!(Map.isSubmapOf(@{1 -> 2}, @{}))

@test
def isSubmapOf03(): Bool = assertNot!(Map.isSubmapOf(@{3 -> 4}, @{}))

@test
def isSubmapOf04(): Bool = assert!(Map.isSubmapOf(@{1 -> 3}, @{1 -> 3}))

@test
def isSubmapOf05(): Bool = assertNot!(Map.isSubmapOf(@{2 -> 3}, @{1 -> 3}))

@test
def isSubmapOf06(): Bool = assertNot!(Map.isSubmapOf(@{1 -> 4}, @{1 -> 3}))

@test
def isSubmapOf07(): Bool = assertNot!(Map.isSubmapOf(@{1 -> 3, 2 -> 3}, @{1 -> 3}))

@test
def isSubmapOf08(): Bool = assertNot!(Map.isSubmapOf(@{1 -> 3, 2 -> 3}, @{2 -> 3}))

@test
def isSubmapOf09(): Bool = assert!(Map.isSubmapOf(@{}, @{2 -> 4, 1 -> 3}))

@test
def isSubmapOf10(): Bool = assert!(Map.isSubmapOf(@{2 -> 4}, @{2 -> 4, 1 -> 3}))

@test
def isSubmapOf11(): Bool = assert!(Map.isSubmapOf(@{1 -> 3}, @{2 -> 4, 1 -> 3}))

@test
def isSubmapOf12(): Bool = assert!(Map.isSubmapOf(@{1 -> 3, 2 -> 4}, @{1 -> 3, 2 -> 4}))

@test
def isSubmapOf13(): Bool = assert!(Map.isSubmapOf(@{2 -> 4, 1 -> 3}, @{1 -> 3, 2 -> 4}))

@test
def isSubmapOf14(): Bool = assertNot!(Map.isSubmapOf(@{2 -> 5, 1 -> 3}, @{1 -> 3, 2 -> 4}))

@test
def isSubmapOf15(): Bool = assertNot!(Map.isSubmapOf(@{2 -> 4, 1 -> -1}, @{1 -> 3, 2 -> 4}))

@test
def isSubmapOf16(): Bool = assertNot!(Map.isSubmapOf(@{3 -> 4, 1 -> 3}, @{1 -> 3, 2 -> 4}))

@test
def isSubmapOf17(): Bool = assertNot!(Map.isSubmapOf(@{2 -> 4, 3 -> 3}, @{1 -> 3, 2 -> 4}))

@test
def isSubmapOf18(): Bool = assertNot!(Map.isSubmapOf(@{2 -> 4, 1 -> 3, 8 -> 9}, @{1 -> 3, 2 -> 4}))

/////////////////////////////////////////////////////////////////////////////
// isProperSubmapOf                                                        //
/////////////////////////////////////////////////////////////////////////////
@test
def isProperSubmapOf01(): Bool = assertNot!(Map.isProperSubmapOf(@{}, @{}))

@test
def isProperSubmapOf02(): Bool = assertNot!(Map.isProperSubmapOf(@{1 -> 2}, @{}))

@test
def isProperSubmapOf03(): Bool = assertNot!(Map.isProperSubmapOf(@{3 -> 4}, @{}))

@test
def isProperSubmapOf04(): Bool = assertNot!(Map.isProperSubmapOf(@{1 -> 3}, @{1 -> 3}))

@test
def isProperSubmapOf05(): Bool = assertNot!(Map.isProperSubmapOf(@{2 -> 3}, @{1 -> 3}))

@test
def isProperSubmapOf06(): Bool = assertNot!(Map.isProperSubmapOf(@{1 -> 4}, @{1 -> 3}))

@test
def isProperSubmapOf07(): Bool = assertNot!(Map.isProperSubmapOf(@{1 -> 3, 2 -> 3}, @{1 -> 3}))

@test
def isProperSubmapOf08(): Bool = assertNot!(Map.isProperSubmapOf(@{1 -> 3, 2 -> 3}, @{2 -> 3}))

@test
def isProperSubmapOf09(): Bool = assert!(Map.isProperSubmapOf(@{}, @{2 -> 4, 1 -> 3}))

@test
def isProperSubmapOf10(): Bool = assert!(Map.isProperSubmapOf(@{2 -> 4}, @{2 -> 4, 1 -> 3}))

@test
def isProperSubmapOf11(): Bool = assert!(Map.isProperSubmapOf(@{1 -> 3}, @{2 -> 4, 1 -> 3}))

@test
def isProperSubmapOf12(): Bool = assertNot!(Map.isProperSubmapOf(@{1 -> 3, 2 -> 4}, @{1 -> 3, 2 -> 4}))

@test
def isProperSubmapOf13(): Bool = assertNot!(Map.isProperSubmapOf(@{2 -> 4, 1 -> 3}, @{1 -> 3, 2 -> 4}))

@test
def isProperSubmapOf14(): Bool = assertNot!(Map.isProperSubmapOf(@{2 -> 5, 1 -> 3}, @{1 -> 3, 2 -> 4}))

@test
def isProperSubmapOf15(): Bool = assertNot!(Map.isProperSubmapOf(@{2 -> 4, 1 -> -1}, @{1 -> 3, 2 -> 4}))

@test
def isProperSubmapOf16(): Bool = assertNot!(Map.isProperSubmapOf(@{3 -> 4, 1 -> 3}, @{1 -> 3, 2 -> 4}))

@test
def isProperSubmapOf17(): Bool = assertNot!(Map.isProperSubmapOf(@{2 -> 4, 3 -> 3}, @{1 -> 3, 2 -> 4}))

@test
def isProperSubmapOf18(): Bool = assertNot!(Map.isProperSubmapOf(@{2 -> 4, 1 -> 3, 8 -> 9}, @{1 -> 3, 2 -> 4}))

/////////////////////////////////////////////////////////////////////////////
// find                                                                    //
/////////////////////////////////////////////////////////////////////////////
@test
def find01(): Bool = Map.find((k, v) -> k == v, @{}) `assertEq!` None

@test
def find02(): Bool = Map.find((k, v) -> k == v, @{1 -> 2}) `assertEq!` None

@test
def find03(): Bool = Map.find((k, v) -> k == v, @{1 -> 1}) `assertEq!` Some((1, 1))

@test
def find04(): Bool = Map.find((k, v) -> k == v, @{1 -> 2, 2 -> 3}) `assertEq!` None

@test
def find05(): Bool = Map.find((k, v) -> k == v, @{1 -> 1, 2 -> 3}) `assertEq!` Some((1, 1))

@test
def find06(): Bool = Map.find((k, v) -> k == v, @{1 -> 2, 2 -> 2}) `assertEq!` Some((2, 2))

@test
def find07(): Bool = Map.find((k, v) -> k == v, @{1 -> 1, 2 -> 2}) `assertEq!` Some((2, 2))

/////////////////////////////////////////////////////////////////////////////
// findLeft                                                                //
/////////////////////////////////////////////////////////////////////////////
@test
def findLeft01(): Bool = Map.findLeft((k, v) -> k == v, @{}) `assertEq!` None

@test
def findLeft02(): Bool = Map.findLeft((k, v) -> k == v, @{1 -> 2}) `assertEq!` None

@test
def findLeft03(): Bool = Map.findLeft((k, v) -> k == v, @{1 -> 1}) `assertEq!` Some((1, 1))

@test
def findLeft04(): Bool = Map.findLeft((k, v) -> k == v, @{1 -> 2, 2 -> 3}) `assertEq!` None

@test
def findLeft05(): Bool = Map.findLeft((k, v) -> k == v, @{1 -> 1, 2 -> 3}) `assertEq!` Some((1, 1))

@test
def findLeft06(): Bool = Map.findLeft((k, v) -> k == v, @{1 -> 2, 2 -> 2}) `assertEq!` Some((2, 2))

@test
def findLeft07(): Bool = Map.findLeft((k, v) -> k == v, @{1 -> 1, 2 -> 2}) `assertEq!` Some((2, 2))

/////////////////////////////////////////////////////////////////////////////
// findRight                                                               //
/////////////////////////////////////////////////////////////////////////////
@test
def findRight01(): Bool = Map.findRight((k, v) -> k == v, @{}) `assertEq!` None

@test
def findRight02(): Bool = Map.findRight((k, v) -> k == v, @{1 -> 2}) `assertEq!` None

@test
def findRight03(): Bool = Map.findRight((k, v) -> k == v, @{1 -> 1}) `assertEq!` Some((1, 1))

@test
def findRight04(): Bool = Map.findRight((k, v) -> k == v, @{1 -> 2, 2 -> 3}) `assertEq!` None

@test
def findRight05(): Bool = Map.findRight((k, v) -> k == v, @{1 -> 1, 2 -> 3}) `assertEq!` Some((1, 1))

@test
def findRight06(): Bool = Map.findRight((k, v) -> k == v, @{1 -> 2, 2 -> 2}) `assertEq!` Some((2, 2))

@test
def findRight07(): Bool = Map.findRight((k, v) -> k == v, @{1 -> 1, 2 -> 2}) `assertEq!` Some((1, 1))

/////////////////////////////////////////////////////////////////////////////
// filter                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def filter01(): Bool = assert!(Map.eq(Map.filter(v -> v % 2 == 0, @{}), @{}))

@test
def filter02(): Bool = assert!(Map.eq(Map.filter(v -> v % 2 == 0, @{1 -> 2}), @{1 -> 2}))

@test
def filter03(): Bool = assert!(Map.eq(Map.filter(v -> v % 2 == 0, @{1 -> 3}), @{}))

@test
def filter04(): Bool = assert!(Map.eq(Map.filter(v -> v % 2 == 0, @{1 -> 8, 2 -> 4}), @{1 -> 8, 2 -> 4}))

@test
def filter05(): Bool = assert!(Map.eq(Map.filter(v -> v % 2 == 0, @{1 -> 7, 2 -> 4}), @{2 -> 4}))

@test
def filter06(): Bool = assert!(Map.eq(Map.filter(v -> v % 2 == 0, @{1 -> 8, 2 -> 3}), @{1 -> 8}))

@test
def filter07(): Bool = assert!(Map.eq(Map.filter(v -> v % 2 == 0, @{1 -> -1, 2 -> -3}), @{}))

/////////////////////////////////////////////////////////////////////////////
// filterWithKey                                                           //
/////////////////////////////////////////////////////////////////////////////
@test
def filterWithKey01(): Bool = assert!(Map.eq(Map.filterWithKey((k, v) -> k == v, @{}), @{}))

@test
def filterWithKey02(): Bool = assert!(Map.eq(Map.filterWithKey((k, v) -> k == v, @{1 -> 1}), @{1 -> 1}))

@test
def filterWithKey03(): Bool = assert!(Map.eq(Map.filterWithKey((k, v) -> k == v, @{1 -> 3}), @{}))

@test
def filterWithKey04(): Bool = assert!(Map.eq(Map.filterWithKey((k, v) -> k == v, @{1 -> 1, 2 -> 2}), @{1 -> 1, 2 -> 2}))

@test
def filterWithKey05(): Bool = assert!(Map.eq(Map.filterWithKey((k, v) -> k == v, @{1 -> 7, 2 -> 2}), @{2 -> 2}))

@test
def filterWithKey06(): Bool = assert!(Map.eq(Map.filterWithKey((k, v) -> k == v, @{1 -> 1, 2 -> 3}), @{1 -> 1}))

@test
def filterWithKey07(): Bool = assert!(Map.eq(Map.filterWithKey((k, v) -> k == v, @{1 -> -1, 2 -> -3}), @{}))

/////////////////////////////////////////////////////////////////////////////
// map                                                                     //
/////////////////////////////////////////////////////////////////////////////
@test
def map01(): Bool = assert!(Map.eq(Map.map(v -> 3*v, @{}), @{}))

@test
def map02(): Bool = assert!(Map.eq(Map.map(v -> 3*v, @{1 -> 4}), @{1 -> 12}))

@test
def map03(): Bool = assert!(Map.eq(Map.map(v -> 3*v, @{2 -> -1, 0 -> 0}), @{2 -> -3, 0 -> 0}))

@test
def map04(): Bool = assert!(Map.eq(Map.map(v -> 3*v, @{2 -> -1, 5 -> 15, 11 -> -9}), @{2 -> -3, 5 -> 45, 11 -> -27}))

@test
def map05(): Bool = assert!(Map.eq(Map.map(v -> 3*v, @{2 -> -1, 5 -> 15, 11 -> -9, 8 -> 8}), @{2 -> -3, 5 -> 45, 11 -> -27, 8 -> 24}))

/////////////////////////////////////////////////////////////////////////////
// mapWithKey                                                              //
/////////////////////////////////////////////////////////////////////////////
@test
def mapWithKey01(): Bool = assert!(Map.eq(Map.mapWithKey((_, v) -> v, @{}), @{}))

@test
def mapWithKey02(): Bool = assert!(Map.eq(Map.mapWithKey((k, v) -> k + v, @{1 -> 4}), @{1 -> 5}))

@test
def mapWithKey03(): Bool = assert!(Map.eq(Map.mapWithKey((k, v) -> k + v, @{2 -> -1, 0 -> 0}), @{2 -> 1, 0 -> 0}))

@test
def mapWithKey04(): Bool = assert!(Map.eq(Map.mapWithKey((k, v) -> k + v, @{2 -> -1, 5 -> 15, 11 -> -9}), @{2 -> 1, 5 -> 20, 11 -> 2}))

@test
def mapWithKey05(): Bool = assert!(Map.eq(Map.mapWithKey((k, v) -> k + v, @{2 -> -1, 5 -> 15, 11 -> -9, 8 -> 8}),
                                        @{2 -> 1, 5 -> 20, 11 -> 2, 8 -> 16}))

/////////////////////////////////////////////////////////////////////////////
// fold                                                                    //
/////////////////////////////////////////////////////////////////////////////
@test
def fold01(): Bool = Map.fold((s, v) -> s + v, 0, @{}) `assertEq!` 0

@test
def fold02(): Bool = Map.fold((s, v) -> s + v, 0, @{1 -> 2}) `assertEq!` 2

@test
def fold03(): Bool = Map.fold((s, v) -> s + v, 0, @{1 -> 2, 3 -> 4}) `assertEq!` 6

@test
def fold04(): Bool = Map.fold((s, v) -> s + v, 0, @{1 -> 2, 3 -> 4, 5 -> 6}) `assertEq!` 12

/////////////////////////////////////////////////////////////////////////////
// foldWithKey                                                             //
/////////////////////////////////////////////////////////////////////////////
@test
def foldWithKey01(): Bool = Map.foldWithKey((k, s, v) -> k + s + v, 0, @{}) `assertEq!` 0

@test
def foldWithKey02(): Bool = Map.foldWithKey((k, s, v) -> k + s + v, 0, @{1 -> 2}) `assertEq!` 3

@test
def foldWithKey03(): Bool = Map.foldWithKey((k, s, v) -> k + s + v, 0, @{1 -> 2, 3 -> 4}) `assertEq!` 10

@test
def foldWithKey04(): Bool = Map.foldWithKey((k, s, v) -> k + s + v, 0, @{1 -> 2, 3 -> 4, 5 -> 6}) `assertEq!` 21

/////////////////////////////////////////////////////////////////////////////
// foldLeft                                                                //
/////////////////////////////////////////////////////////////////////////////
@test
def foldLeft01(): Bool = Map.foldLeft((s, v) -> s + v, 0, @{}) `assertEq!` 0

@test
def foldLeft02(): Bool = Map.foldLeft((s, v) -> s + v, 0, @{1 -> 2}) `assertEq!` 2

@test
def foldLeft03(): Bool = Map.foldLeft((s, v) -> s + v, 0, @{1 -> 2, 3 -> 4}) `assertEq!` 6

@test
def foldLeft04(): Bool = Map.foldLeft((s, v) -> s + v, 0, @{1 -> 2, 3 -> 4, 5 -> 6}) `assertEq!` 12

/////////////////////////////////////////////////////////////////////////////
// foldLeftWithKey                                                         //
/////////////////////////////////////////////////////////////////////////////
@test
def foldLeftWithKey01(): Bool = Map.foldLeftWithKey((k, s, v) -> k + s + v, 0, @{}) `assertEq!` 0

@test
def foldLeftWithKey02(): Bool = Map.foldLeftWithKey((k, s, v) -> k + s + v, 0, @{1 -> 2}) `assertEq!` 3

@test
def foldLeftWithKey03(): Bool = Map.foldLeftWithKey((k, s, v) -> k + s + v, 0, @{1 -> 2, 3 -> 4}) `assertEq!` 10

@test
def foldLeftWithKey04(): Bool = Map.foldLeftWithKey((k, s, v) -> k + s + v, 0, @{1 -> 2, 3 -> 4, 5 -> 6}) `assertEq!` 21

/////////////////////////////////////////////////////////////////////////////
// foldRight                                                               //
/////////////////////////////////////////////////////////////////////////////
@test
def foldRight01(): Bool = Map.foldRight((v, s) -> s + v, 0, @{}) `assertEq!` 0

@test
def foldRight02(): Bool = Map.foldRight((v, s) -> s + v, 0, @{1 -> 2}) `assertEq!` 2

@test
def foldRight03(): Bool = Map.foldRight((v, s) -> s + v, 0, @{1 -> 2, 3 -> 4}) `assertEq!` 6

@test
def foldRight04(): Bool = Map.foldRight((v, s) -> s + v, 0, @{1 -> 2, 3 -> 4, 5 -> 6}) `assertEq!` 12

/////////////////////////////////////////////////////////////////////////////
// foldRightWithKey                                                        //
/////////////////////////////////////////////////////////////////////////////
@test
def foldRightWithKey01(): Bool = Map.foldRightWithKey((k, v, s) -> k + s + v, 0, @{}) `assertEq!` 0

@test
def foldRightWithKey02(): Bool = Map.foldRightWithKey((k, v, s) -> k + s + v, 0, @{1 -> 2}) `assertEq!` 3

@test
def foldRightWithKey03(): Bool = Map.foldRightWithKey((k, v, s) -> k + s + v, 0, @{1 -> 2, 3 -> 4}) `assertEq!` 10

@test
def foldRightWithKey04(): Bool = Map.foldRightWithKey((k, v, s) -> k + s + v, 0, @{1 -> 2, 3 -> 4, 5 -> 6}) `assertEq!` 21

/////////////////////////////////////////////////////////////////////////////
// reduce                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def reduce01(): Bool = Map.reduce((v1, v2) -> v1 - v2, @{}: Map[Int32, Int32]) `assertEq!` None

@test
def reduce02(): Bool = Map.reduce((v1, v2) -> v1 - v2, @{1 -> 2}) `assertEq!` Some(2)

@test
def reduce03(): Bool = Map.reduce((v1, v2) -> v1 - v2, @{1 -> 2, 2 -> 3}) `assertEq!` Some(1)

@test
def reduce04(): Bool = Map.reduce((v1, v2) -> v1 - v2, @{1 -> 2, 2 -> 3, 3 -> 4}) `assertEq!` Some(-1)

/////////////////////////////////////////////////////////////////////////////
// reduceWithKey                                                           //
/////////////////////////////////////////////////////////////////////////////
@test
def reduceWithKey01(): Bool = Map.reduceWithKey((k1, v1, k2, v2) -> (k1 - k2, v1 - v2), @{}: Map[Int32, Int32]) `assertEq!` None

@test
def reduceWithKey02(): Bool = Map.reduceWithKey((k1, v1, k2, v2) -> (k1 - k2, v1 - v2), @{1 -> 2}) `assertEq!` Some((1, 2))

@test
def reduceWithKey03(): Bool = Map.reduceWithKey((k1, v1, k2, v2) -> (k1 - k2, v1 - v2), @{1 -> 2, 2 -> 3}) `assertEq!` Some((1, 1))

@test
def reduceWithKey04(): Bool = Map.reduceWithKey((k1, v1, k2, v2) -> (k1 - k2, v1 - v2), @{1 -> 2, 2 -> 3, 3 -> 4}) `assertEq!` Some((0, -1))

/////////////////////////////////////////////////////////////////////////////
// reduceLeft                                                              //
/////////////////////////////////////////////////////////////////////////////
@test
def reduceLeft01(): Bool = Map.reduceLeft((v1, v2) -> v1 - v2, @{}: Map[Int32, Int32]) `assertEq!` None

@test
def reduceLeft02(): Bool = Map.reduceLeft((v1, v2) -> v1 - v2, @{1 -> 2}) `assertEq!` Some(2)

@test
def reduceLeft03(): Bool = Map.reduceLeft((v1, v2) -> v1 - v2, @{1 -> 2, 2 -> 3}) `assertEq!` Some(1)

@test
def reduceLeft04(): Bool = Map.reduceLeft((v1, v2) -> v1 - v2, @{1 -> 2, 2 -> 3, 3 -> 4}) `assertEq!` Some(-1)

/////////////////////////////////////////////////////////////////////////////
// reduceLeftWithKey                                                       //
/////////////////////////////////////////////////////////////////////////////
@test
def reduceLeftWithKey01(): Bool = Map.reduceLeftWithKey((k1, v1, k2, v2) -> (k1 - k2, v1 - v2), @{}: Map[Int32, Int32]) `assertEq!` None

@test
def reduceLeftWithKey02(): Bool = Map.reduceLeftWithKey((k1, v1, k2, v2) -> (k1 - k2, v1 - v2), @{1 -> 2}) `assertEq!` Some((1, 2))

@test
def reduceLeftWithKey03(): Bool = Map.reduceLeftWithKey((k1, v1, k2, v2) -> (k1 - k2, v1 - v2), @{1 -> 2, 2 -> 3}) `assertEq!` Some((1, 1))

@test
def reduceLeftWithKey04(): Bool = Map.reduceLeftWithKey((k1, v1, k2, v2) -> (k1 - k2, v1 - v2), @{1 -> 2, 2 -> 3, 3 -> 4}) `assertEq!` Some((0, -1))

/////////////////////////////////////////////////////////////////////////////
// reduceRight                                                             //
/////////////////////////////////////////////////////////////////////////////
@test
def reduceRight01(): Bool = Map.reduceRight((v1, v2) -> v1 - v2, @{}: Map[Int32, Int32]) `assertEq!` None

@test
def reduceRight02(): Bool = Map.reduceRight((v1, v2) -> v1 - v2, @{1 -> 2}) `assertEq!` Some(2)

@test
def reduceRight03(): Bool = Map.reduceRight((v1, v2) -> v1 - v2, @{1 -> 2, 2 -> 3}) `assertEq!` Some(1)

@test
def reduceRight04(): Bool = Map.reduceRight((v1, v2) -> v1 - v2, @{1 -> 2, 2 -> 3, 3 -> 4}) `assertEq!` Some(3)

/////////////////////////////////////////////////////////////////////////////
// reduceRightWithKey                                                      //
/////////////////////////////////////////////////////////////////////////////
@test
def reduceRightWithKey01(): Bool = Map.reduceRightWithKey((k1, v1, k2, v2) -> (k1 - k2, v1 - v2), @{}: Map[Int32, Int32]) `assertEq!` None

@test
def reduceRightWithKey02(): Bool = Map.reduceRightWithKey((k1, v1, k2, v2) -> (k1 - k2, v1 - v2), @{1 -> 2}) `assertEq!` Some((1, 2))

@test
def reduceRightWithKey03(): Bool = Map.reduceRightWithKey((k1, v1, k2, v2) -> (k1 - k2, v1 - v2), @{1 -> 2, 2 -> 3}) `assertEq!` Some((1, 1))

@test
def reduceRightWithKey04(): Bool = Map.reduceRightWithKey((k1, v1, k2, v2) -> (k1 - k2, v1 - v2), @{1 -> 2, 2 -> 3, 3 -> 4}) `assertEq!` Some((2 , 3))

/////////////////////////////////////////////////////////////////////////////
// count                                                                   //
/////////////////////////////////////////////////////////////////////////////
@test
def count01(): Bool = Map.count((k, v) -> k == v, @{}) `assertEq!` 0

@test
def count02(): Bool = Map.count((k, v) -> k == v, @{1 -> 2}) `assertEq!` 0

@test
def count03(): Bool = Map.count((k, v) -> k == v, @{1 -> 1}) `assertEq!` 1

@test
def count04(): Bool = Map.count((k, v) -> k == v, @{1 -> 2, 2 -> 3}) `assertEq!` 0

@test
def count05(): Bool = Map.count((k, v) -> k == v, @{1 -> 1, 2 -> 3}) `assertEq!` 1

@test
def count06(): Bool = Map.count((k, v) -> k == v, @{1 -> 2, 2 -> 2}) `assertEq!` 1

@test
def count07(): Bool = Map.count((k, v) -> k == v, @{1 -> 1, 2 -> 2}) `assertEq!` 2

/////////////////////////////////////////////////////////////////////////////
// exists                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def exists01(): Bool = assertNot!(Map.exists((k, v) -> k == v, @{}))

@test
def exists02(): Bool = assertNot!(Map.exists((k, v) -> k == v, @{1 -> 2}))

@test
def exists03(): Bool = assert!(Map.exists((k, v) -> k == v, @{1 -> 1}))

@test
def exists04(): Bool = assertNot!(Map.exists((k, v) -> k == v, @{1 -> 2, 2 -> 3}))

@test
def exists05(): Bool = assert!(Map.exists((k, v) -> k == v, @{1 -> 1, 2 -> 3}))

@test
def exists06(): Bool = assert!(Map.exists((k, v) -> k == v, @{1 -> 2, 2 -> 2}))

@test
def exists07(): Bool = assert!(Map.exists((k, v) -> k == v, @{1 -> 1, 2 -> 2}))

/////////////////////////////////////////////////////////////////////////////
// forall                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def forall01(): Bool = assert!(Map.forall((k, v) -> k == v, @{}))

@test
def forall02(): Bool = assertNot!(Map.forall((k, v) -> k == v, @{1 -> 2}))

@test
def forall03(): Bool = assert!(Map.forall((k, v) -> k == v, @{1 -> 1}))

@test
def forall04(): Bool = assertNot!(Map.forall((k, v) -> k == v, @{1 -> 2, 2 -> 3}))

@test
def forall05(): Bool = assertNot!(Map.forall((k, v) -> k == v, @{1 -> 1, 2 -> 3}))

@test
def forall06(): Bool = assertNot!(Map.forall((k, v) -> k == v, @{1 -> 2, 2 -> 2}))

@test
def forall07(): Bool = assert!(Map.forall((k, v) -> k == v, @{1 -> 1, 2 -> 2}))

/////////////////////////////////////////////////////////////////////////////
// union                                                                   //
/////////////////////////////////////////////////////////////////////////////
@test
def union01(): Bool = assert!(Map.eq(Map.union(@{}, @{}), @{}))

@test
def union02(): Bool = assert!(Map.eq(Map.union(@{1 -> 2}, @{}), @{1 -> 2}))

@test
def union03(): Bool = assert!(Map.eq(Map.union(@{}, @{1 -> 2}), @{1 -> 2}))

@test
def union04(): Bool = assert!(Map.eq(Map.union(@{}, @{1 -> 2, 3 -> 4}), @{1 -> 2, 3 -> 4}))

@test
def union05(): Bool = assert!(Map.eq(Map.union(@{1 -> 2, 3 -> 4}, @{}), @{1 -> 2, 3 -> 4}))

@test
def union06(): Bool = assert!(Map.eq(Map.union(@{1 -> 2}, @{3 -> 4}), @{1 -> 2, 3 -> 4}))

@test
def union07(): Bool = assert!(Map.eq(Map.union(@{1 -> 2}, @{1 -> 5}), @{1 -> 2}))

@test
def union08(): Bool = assert!(Map.eq(Map.union(@{}, @{1 -> 2, 2 -> 3, 3 -> 4}), @{1 -> 2, 2 -> 3, 3 -> 4}))

@test
def union09(): Bool = assert!(Map.eq(Map.union(@{1 -> 2, 2 -> 3, 3 -> 4}, @{}), @{1 -> 2, 2 -> 3, 3 -> 4}))

@test
def union10(): Bool = assert!(Map.eq(Map.union(@{1 -> 2, 2 -> 3}, @{3 -> 4}), @{1 -> 2, 2 -> 3, 3 -> 4}))

@test
def union11(): Bool = assert!(Map.eq(Map.union(@{3 -> 4}, @{1 -> 2, 2 -> 3}), @{1 -> 2, 2 -> 3, 3 -> 4}))

@test
def union12(): Bool = assert!(Map.eq(Map.union(@{1 -> 2, 2 -> 3}, @{2 -> 8}), @{1 -> 2, 2 -> 3}))

@test
def union13(): Bool = assert!(Map.eq(Map.union(@{1 -> 2, 2 -> 3}, @{1 -> 8}), @{1 -> 2, 2 -> 3}))

@test
def union14(): Bool = assert!(Map.eq(Map.union(@{1 -> 4}, @{1 -> 2, 2 -> 3}), @{1 -> 4, 2 -> 3}))

@test
def union15(): Bool = assert!(Map.eq(Map.union(@{2 -> 7}, @{1 -> 2, 2 -> 3}), @{1 -> 2, 2 -> 7}))

@test
def union16(): Bool = assert!(Map.eq(Map.union(@{2 -> 7, 11 -> 14, 9 -> 8, 15 -> 22}, @{15 -> 21, 1 -> 2, 2 -> 8, 44 -> 33}),
                                   @{2 -> 7, 11 -> 14, 9 -> 8, 15 -> 22, 1 -> 2, 44 -> 33}))

/////////////////////////////////////////////////////////////////////////////
// unionWith                                                               //
/////////////////////////////////////////////////////////////////////////////
@test
def unionWith01(): Bool = assert!(Map.eq(Map.unionWith((v1, v2) -> v1 - v2, @{}, @{}), @{}: Map[Int, Int]))

@test
def unionWith02(): Bool = assert!(Map.eq(Map.unionWith((v1, v2) -> v1 - v2, @{1 -> 2}, @{}), @{1 -> 2}))

@test
def unionWith03(): Bool = assert!(Map.eq(Map.unionWith((v1, v2) -> v1 - v2, @{}, @{1 -> 2}), @{1 -> 2}))

@test
def unionWith04(): Bool = assert!(Map.eq(Map.unionWith((v1, v2) -> v1 - v2, @{}, @{1 -> 2, 3 -> 4}), @{1 -> 2, 3 -> 4}))

@test
def unionWith05(): Bool = assert!(Map.eq(Map.unionWith((v1, v2) -> v1 - v2, @{1 -> 2, 3 -> 4}, @{}), @{1 -> 2, 3 -> 4}))

@test
def unionWith06(): Bool = assert!(Map.eq(Map.unionWith((v1, v2) -> v1 - v2, @{1 -> 2}, @{3 -> 4}), @{1 -> 2, 3 -> 4}))

@test
def unionWith07(): Bool = assert!(Map.eq(Map.unionWith((v1, v2) -> v1 - v2, @{1 -> 2}, @{1 -> 5}), @{1 -> -3}))

@test
def unionWith08(): Bool = assert!(Map.eq(Map.unionWith((v1, v2) -> v1 - v2, @{}, @{1 -> 2, 2 -> 3, 3 -> 4}), @{1 -> 2, 2 -> 3, 3 -> 4}))

@test
def unionWith09(): Bool = assert!(Map.eq(Map.unionWith((v1, v2) -> v1 - v2, @{1 -> 2, 2 -> 3, 3 -> 4}, @{}), @{1 -> 2, 2 -> 3, 3 -> 4}))

@test
def unionWith10(): Bool = assert!(Map.eq(Map.unionWith((v1, v2) -> v1 - v2, @{1 -> 2, 2 -> 3}, @{3 -> 4}), @{1 -> 2, 2 -> 3, 3 -> 4}))

@test
def unionWith11(): Bool = assert!(Map.eq(Map.unionWith((v1, v2) -> v1 - v2, @{3 -> 4}, @{1 -> 2, 2 -> 3}), @{1 -> 2, 2 -> 3, 3 -> 4}))

@test
def unionWith12(): Bool = assert!(Map.eq(Map.unionWith((v1, v2) -> v1 - v2, @{1 -> 2, 2 -> 3}, @{2 -> 8}), @{1 -> 2, 2 -> -5}))

@test
def unionWith13(): Bool = assert!(Map.eq(Map.unionWith((v1, v2) -> v1 - v2, @{1 -> 2, 2 -> 3}, @{1 -> 8}), @{1 -> -6, 2 -> 3}))

@test
def unionWith14(): Bool = assert!(Map.eq(Map.unionWith((v1, v2) -> v1 - v2, @{1 -> 4}, @{1 -> 2, 2 -> 3}), @{1 -> 2, 2 -> 3}))

@test
def unionWith15(): Bool = assert!(Map.eq(Map.unionWith((v1, v2) -> v1 - v2, @{2 -> 7}, @{1 -> 2, 2 -> 3}), @{1 -> 2, 2 -> 4}))

@test
def unionWith16(): Bool = assert!(Map.eq(Map.unionWith((v1, v2) -> v1 - v2, @{2 -> 7, 11 -> 14, 9 -> 8, 15 -> 22}, @{15 -> 21, 1 -> 2, 2 -> 8, 44 -> 33}),
                                   @{2 -> -1, 11 -> 14, 9 -> 8, 15 -> 1, 1 -> 2, 44 -> 33}))

/////////////////////////////////////////////////////////////////////////////
// unionWithKey                                                            //
/////////////////////////////////////////////////////////////////////////////
@test
def unionWithKey01(): Bool = assert!(Map.eq(Map.unionWithKey((k, v1, v2) -> k + v1 - v2, @{}, @{}), @{}: Map[Int, Int]))

@test
def unionWithKey02(): Bool = assert!(Map.eq(Map.unionWithKey((k, v1, v2) -> k + v1 - v2, @{1 -> 2}, @{}), @{1 -> 2}))

@test
def unionWithKey03(): Bool = assert!(Map.eq(Map.unionWithKey((k, v1, v2) -> k + v1 - v2, @{}, @{1 -> 2}), @{1 -> 2}))

@test
def unionWithKey04(): Bool = assert!(Map.eq(Map.unionWithKey((k, v1, v2) -> k + v1 - v2, @{}, @{1 -> 2, 3 -> 4}), @{1 -> 2, 3 -> 4}))

@test
def unionWithKey05(): Bool = assert!(Map.eq(Map.unionWithKey((k, v1, v2) -> k + v1 - v2, @{1 -> 2, 3 -> 4}, @{}), @{1 -> 2, 3 -> 4}))

@test
def unionWithKey06(): Bool = assert!(Map.eq(Map.unionWithKey((k, v1, v2) -> k + v1 - v2, @{1 -> 2}, @{3 -> 4}), @{1 -> 2, 3 -> 4}))

@test
def unionWithKey07(): Bool = assert!(Map.eq(Map.unionWithKey((k, v1, v2) -> k + v1 - v2, @{1 -> 2}, @{1 -> 5}), @{1 -> -2}))

@test
def unionWithKey08(): Bool = assert!(Map.eq(Map.unionWithKey((k, v1, v2) -> k + v1 - v2, @{}, @{1 -> 2, 2 -> 3, 3 -> 4}), @{1 -> 2, 2 -> 3, 3 -> 4}))

@test
def unionWithKey09(): Bool = assert!(Map.eq(Map.unionWithKey((k, v1, v2) -> k + v1 - v2, @{1 -> 2, 2 -> 3, 3 -> 4}, @{}), @{1 -> 2, 2 -> 3, 3 -> 4}))

@test
def unionWithKey10(): Bool = assert!(Map.eq(Map.unionWithKey((k, v1, v2) -> k + v1 - v2, @{1 -> 2, 2 -> 3}, @{3 -> 4}), @{1 -> 2, 2 -> 3, 3 -> 4}))

@test
def unionWithKey11(): Bool = assert!(Map.eq(Map.unionWithKey((k, v1, v2) -> k + v1 - v2, @{3 -> 4}, @{1 -> 2, 2 -> 3}), @{1 -> 2, 2 -> 3, 3 -> 4}))

@test
def unionWithKey12(): Bool = assert!(Map.eq(Map.unionWithKey((k, v1, v2) -> k + v1 - v2, @{1 -> 2, 2 -> 3}, @{2 -> 8}), @{1 -> 2, 2 -> -3}))

@test
def unionWithKey13(): Bool = assert!(Map.eq(Map.unionWithKey((k, v1, v2) -> k + v1 - v2, @{1 -> 2, 2 -> 3}, @{1 -> 8}), @{1 -> -5, 2 -> 3}))

@test
def unionWithKey14(): Bool = assert!(Map.eq(Map.unionWithKey((k, v1, v2) -> k + v1 - v2, @{1 -> 4}, @{1 -> 2, 2 -> 3}), @{1 -> 3, 2 -> 3}))

@test
def unionWithKey15(): Bool = assert!(Map.eq(Map.unionWithKey((k, v1, v2) -> k + v1 - v2, @{2 -> 7}, @{1 -> 2, 2 -> 3}), @{1 -> 2, 2 -> 6}))

@test
def unionWithKey16(): Bool = assert!(Map.eq(Map.unionWithKey((k, v1, v2) -> k + v1 - v2, @{2 -> 7, 11 -> 14, 9 -> 8, 15 -> 22}, @{15 -> 21, 1 -> 2, 2 -> 8, 44 -> 33}),
                                   @{2 -> 1, 11 -> 14, 9 -> 8, 15 -> 16, 1 -> 2, 44 -> 33}))

/////////////////////////////////////////////////////////////////////////////
// intersection                                                            //
/////////////////////////////////////////////////////////////////////////////
@test
def intersection01(): Bool = assert!(Map.eq(Map.intersection(@{}, @{}), @{}))

@test
def intersection02(): Bool = assert!(Map.eq(Map.intersection(@{1 -> 2}, @{}), @{}))

@test
def intersection03(): Bool = assert!(Map.eq(Map.intersection(@{}, @{1 -> 2}), @{}))

@test
def intersection04(): Bool = assert!(Map.eq(Map.intersection(@{}, @{1 -> 2, 3 -> 4}), @{}))

@test
def intersection05(): Bool = assert!(Map.eq(Map.intersection(@{1 -> 2, 3 -> 4}, @{}), @{}))

@test
def intersection06(): Bool = assert!(Map.eq(Map.intersection(@{1 -> 2}, @{3 -> 4}), @{}))

@test
def intersection07(): Bool = assert!(Map.eq(Map.intersection(@{1 -> 2}, @{1 -> 5}), @{1 -> 2}))

@test
def intersection08(): Bool = assert!(Map.eq(Map.intersection(@{}, @{1 -> 2, 2 -> 3, 3 -> 4}), @{}))

@test
def intersection09(): Bool = assert!(Map.eq(Map.intersection(@{1 -> 2, 2 -> 3, 3 -> 4}, @{}), @{}))

@test
def intersection10(): Bool = assert!(Map.eq(Map.intersection(@{1 -> 2, 2 -> 3}, @{3 -> 4}), @{}))

@test
def intersection11(): Bool = assert!(Map.eq(Map.intersection(@{3 -> 4}, @{1 -> 2, 2 -> 3}), @{}))

@test
def intersection12(): Bool = assert!(Map.eq(Map.intersection(@{1 -> 2, 2 -> 3}, @{2 -> 8}), @{2 -> 3}))

@test
def intersection13(): Bool = assert!(Map.eq(Map.intersection(@{1 -> 2, 2 -> 3}, @{1 -> 8}), @{1 -> 2}))

@test
def intersection14(): Bool = assert!(Map.eq(Map.intersection(@{1 -> 4}, @{1 -> 2, 2 -> 3}), @{1 -> 4}))

@test
def intersection15(): Bool = assert!(Map.eq(Map.intersection(@{2 -> 7}, @{1 -> 2, 2 -> 3}), @{2 -> 7}))

@test
def intersection16(): Bool = assert!(Map.eq(Map.intersection(@{2 -> 7, 11 -> 14, 9 -> 8, 15 -> 22}, @{15 -> 21, 1 -> 2, 2 -> 8, 44 -> 33, 11 -> 1}),
                                   @{2 -> 7, 15 -> 22, 11 -> 14}))

/////////////////////////////////////////////////////////////////////////////
// intersectionWith                                                        //
/////////////////////////////////////////////////////////////////////////////
@test
def intersectionWith01(): Bool = assert!(Map.eq(Map.intersectionWith((v1, v2) -> v1 - v2, @{}, @{}), @{}: Map[Int, Int]))

@test
def intersectionWith02(): Bool = assert!(Map.eq(Map.intersectionWith((v1, v2) -> v1 - v2, @{1 -> 2}, @{}), @{}))

@test
def intersectionWith03(): Bool = assert!(Map.eq(Map.intersectionWith((v1, v2) -> v1 - v2, @{}, @{1 -> 2}), @{}))

@test
def intersectionWith04(): Bool = assert!(Map.eq(Map.intersectionWith((v1, v2) -> v1 - v2, @{}, @{1 -> 2, 3 -> 4}), @{}))

@test
def intersectionWith05(): Bool = assert!(Map.eq(Map.intersectionWith((v1, v2) -> v1 - v2, @{1 -> 2, 3 -> 4}, @{}), @{}))

@test
def intersectionWith06(): Bool = assert!(Map.eq(Map.intersectionWith((v1, v2) -> v1 - v2, @{1 -> 2}, @{3 -> 4}), @{}))

@test
def intersectionWith07(): Bool = assert!(Map.eq(Map.intersectionWith((v1, v2) -> v1 - v2, @{1 -> 2}, @{1 -> 5}), @{1 -> -3}))

@test
def intersectionWith08(): Bool = assert!(Map.eq(Map.intersectionWith((v1, v2) -> v1 - v2, @{}, @{1 -> 2, 2 -> 3, 3 -> 4}), @{}))

@test
def intersectionWith09(): Bool = assert!(Map.eq(Map.intersectionWith((v1, v2) -> v1 - v2, @{1 -> 2, 2 -> 3, 3 -> 4}, @{}), @{}))

@test
def intersectionWith10(): Bool = assert!(Map.eq(Map.intersectionWith((v1, v2) -> v1 - v2, @{1 -> 2, 2 -> 3}, @{3 -> 4}), @{}))

@test
def intersectionWith11(): Bool = assert!(Map.eq(Map.intersectionWith((v1, v2) -> v1 - v2, @{3 -> 4}, @{1 -> 2, 2 -> 3}), @{}))

@test
def intersectionWith12(): Bool = assert!(Map.eq(Map.intersectionWith((v1, v2) -> v1 - v2, @{1 -> 2, 2 -> 3}, @{2 -> 8}), @{2 -> -5}))

@test
def intersectionWith13(): Bool = assert!(Map.eq(Map.intersectionWith((v1, v2) -> v1 - v2, @{1 -> 2, 2 -> 3}, @{1 -> 8}), @{1 -> -6}))

@test
def intersectionWith14(): Bool = assert!(Map.eq(Map.intersectionWith((v1, v2) -> v1 - v2, @{1 -> 4}, @{1 -> 2, 2 -> 3}), @{1 -> 2}))

@test
def intersectionWith15(): Bool = assert!(Map.eq(Map.intersectionWith((v1, v2) -> v1 - v2, @{2 -> 7}, @{1 -> 2, 2 -> 3}), @{2 -> 4}))

@test
def intersectionWith16(): Bool = assert!(Map.eq(Map.intersectionWith((v1, v2) -> v1 - v2, @{2 -> 7, 11 -> 14, 9 -> 8, 15 -> 22}, @{15 -> 21, 1 -> 2, 2 -> 8, 44 -> 33, 11 -> 11}),
                                   @{2 -> -1, 15 -> 1, 11 -> 3}))

/////////////////////////////////////////////////////////////////////////////
// intersectionWithKey                                                     //
/////////////////////////////////////////////////////////////////////////////
@test
def intersectionWithKey01(): Bool = assert!(Map.eq(Map.intersectionWithKey((k, v1, v2) -> k + v1 - v2, @{}, @{}), @{}: Map[Int, Int]))

@test
def intersectionWithKey02(): Bool = assert!(Map.eq(Map.intersectionWithKey((k, v1, v2) -> k + v1 - v2, @{1 -> 2}, @{}), @{}))

@test
def intersectionWithKey03(): Bool = assert!(Map.eq(Map.intersectionWithKey((k, v1, v2) -> k + v1 - v2, @{}, @{1 -> 2}), @{}))

@test
def intersectionWithKey04(): Bool = assert!(Map.eq(Map.intersectionWithKey((k, v1, v2) -> k + v1 - v2, @{}, @{1 -> 2, 3 -> 4}), @{}))

@test
def intersectionWithKey05(): Bool = assert!(Map.eq(Map.intersectionWithKey((k, v1, v2) -> k + v1 - v2, @{1 -> 2, 3 -> 4}, @{}), @{}))

@test
def intersectionWithKey06(): Bool = assert!(Map.eq(Map.intersectionWithKey((k, v1, v2) -> k + v1 - v2, @{1 -> 2}, @{3 -> 4}), @{}))

@test
def intersectionWithKey07(): Bool = assert!(Map.eq(Map.intersectionWithKey((k, v1, v2) -> k + v1 - v2, @{1 -> 2}, @{1 -> 5}), @{1 -> -2}))

@test
def intersectionWithKey08(): Bool = assert!(Map.eq(Map.intersectionWithKey((k, v1, v2) -> k + v1 - v2, @{}, @{1 -> 2, 2 -> 3, 3 -> 4}), @{}))

@test
def intersectionWithKey09(): Bool = assert!(Map.eq(Map.intersectionWithKey((k, v1, v2) -> k + v1 - v2, @{1 -> 2, 2 -> 3, 3 -> 4}, @{}), @{}))

@test
def intersectionWithKey10(): Bool = assert!(Map.eq(Map.intersectionWithKey((k, v1, v2) -> k + v1 - v2, @{1 -> 2, 2 -> 3}, @{3 -> 4}), @{}))

@test
def intersectionWithKey11(): Bool = assert!(Map.eq(Map.intersectionWithKey((k, v1, v2) -> k + v1 - v2, @{3 -> 4}, @{1 -> 2, 2 -> 3}), @{}))

@test
def intersectionWithKey12(): Bool = assert!(Map.eq(Map.intersectionWithKey((k, v1, v2) -> k + v1 - v2, @{1 -> 2, 2 -> 3}, @{2 -> 8}), @{2 -> -3}))

@test
def intersectionWithKey13(): Bool = assert!(Map.eq(Map.intersectionWithKey((k, v1, v2) -> k + v1 - v2, @{1 -> 2, 2 -> 3}, @{1 -> 8}), @{1 -> -5}))

@test
def intersectionWithKey14(): Bool = assert!(Map.eq(Map.intersectionWithKey((k, v1, v2) -> k + v1 - v2, @{1 -> 4}, @{1 -> 2, 2 -> 3}), @{1 -> 3}))

@test
def intersectionWithKey15(): Bool = assert!(Map.eq(Map.intersectionWithKey((k, v1, v2) -> k + v1 - v2, @{2 -> 7}, @{1 -> 2, 2 -> 3}), @{2 -> 6}))

@test
def intersectionWithKey16(): Bool = assert!(Map.eq(Map.intersectionWithKey((k, v1, v2) -> k + v1 - v2, @{2 -> 7, 11 -> 14, 9 -> 8, 15 -> 22}, @{15 -> 21, 1 -> 2, 2 -> 8, 44 -> 33, 11 -> 17}),
                                   @{2 -> 1, 15 -> 16, 11 -> 8}))

/////////////////////////////////////////////////////////////////////////////
// difference                                                              //
/////////////////////////////////////////////////////////////////////////////
@test
def difference01(): Bool = assert!(Map.eq(Map.difference(@{}, @{}), @{}))

@test
def difference02(): Bool = assert!(Map.eq(Map.difference(@{1 -> 2}, @{}), @{1 -> 2}))

@test
def difference03(): Bool = assert!(Map.eq(Map.difference(@{}, @{1 -> 2}), @{}))

@test
def difference04(): Bool = assert!(Map.eq(Map.difference(@{}, @{1 -> 2, 3 -> 4}), @{}))

@test
def difference05(): Bool = assert!(Map.eq(Map.difference(@{1 -> 2, 3 -> 4}, @{}), @{1 -> 2, 3 -> 4}))

@test
def difference06(): Bool = assert!(Map.eq(Map.difference(@{1 -> 2}, @{3 -> 4}), @{1 -> 2}))

@test
def difference07(): Bool = assert!(Map.eq(Map.difference(@{1 -> 2}, @{1 -> 5}), @{}))

@test
def difference08(): Bool = assert!(Map.eq(Map.difference(@{}, @{1 -> 2, 2 -> 3, 3 -> 4}), @{}))

@test
def difference09(): Bool = assert!(Map.eq(Map.difference(@{1 -> 2, 2 -> 3, 3 -> 4}, @{}), @{1 -> 2, 2 -> 3, 3 -> 4}))

@test
def difference10(): Bool = assert!(Map.eq(Map.difference(@{1 -> 2, 2 -> 3}, @{3 -> 4}), @{1 -> 2, 2 -> 3}))

@test
def difference11(): Bool = assert!(Map.eq(Map.difference(@{3 -> 4}, @{1 -> 2, 2 -> 3}), @{3 -> 4}))

@test
def difference12(): Bool = assert!(Map.eq(Map.difference(@{1 -> 2, 2 -> 3}, @{2 -> 8}), @{1 -> 2}))

@test
def difference13(): Bool = assert!(Map.eq(Map.difference(@{1 -> 2, 2 -> 3}, @{1 -> 8}), @{2 -> 3}))

@test
def difference14(): Bool = assert!(Map.eq(Map.difference(@{1 -> 4}, @{1 -> 2, 2 -> 3}), @{}))

@test
def difference15(): Bool = assert!(Map.eq(Map.difference(@{2 -> 7}, @{1 -> 2, 2 -> 3}), @{}))

@test
def difference16(): Bool = assert!(Map.eq(Map.difference(@{2 -> 7, 11 -> 14, 9 -> 8, 15 -> 22}, @{15 -> 21, 1 -> 2, 2 -> 8, 44 -> 33}),
                                   @{11 -> 14, 9 -> 8}))

/////////////////////////////////////////////////////////////////////////////
// differenceWith                                                          //
/////////////////////////////////////////////////////////////////////////////
@test
def differenceWith01(): Bool = assert!(Map.eq(Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, @{}, @{}), @{}: Map[Int, Int]))

@test
def differenceWith02(): Bool = assert!(Map.eq(Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, @{1 -> 2}, @{}), @{1 -> 2}))

@test
def differenceWith03(): Bool = assert!(Map.eq(Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, @{}, @{1 -> 2}), @{}))

@test
def differenceWith04(): Bool = assert!(Map.eq(Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, @{}, @{1 -> 2, 3 -> 4}), @{}))

@test
def differenceWith05(): Bool = assert!(Map.eq(Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, @{1 -> 2, 3 -> 4}, @{}), @{1 -> 2, 3 -> 4}))

@test
def differenceWith06(): Bool = assert!(Map.eq(Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, @{1 -> 2}, @{3 -> 4}), @{1 -> 2}))

@test
def differenceWith07(): Bool = assert!(Map.eq(Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, @{1 -> 2}, @{1 -> 5}), @{}))

@test
def differenceWith08(): Bool = assert!(Map.eq(Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, @{1 -> 5}, @{1 -> 4}), @{1 -> 5}))

@test
def differenceWith09(): Bool = assert!(Map.eq(Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, @{}, @{1 -> 2, 2 -> 3, 3 -> 4}), @{}))

@test
def differenceWith10(): Bool = assert!(Map.eq(Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, @{1 -> 2, 2 -> 3, 3 -> 4}, @{}), @{1 -> 2, 2 -> 3, 3 -> 4}))

@test
def differenceWith11(): Bool = assert!(Map.eq(Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, @{1 -> 2, 2 -> 3}, @{3 -> 4}), @{1 -> 2, 2 -> 3}))

@test
def differenceWith12(): Bool = assert!(Map.eq(Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, @{3 -> 4}, @{1 -> 2, 2 -> 3}), @{3 -> 4}))

@test
def differenceWith13(): Bool = assert!(Map.eq(Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, @{1 -> 2, 2 -> 3}, @{2 -> 8}), @{1 -> 2}))

@test
def differenceWith14(): Bool = assert!(Map.eq(Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, @{1 -> 2, 2 -> 9}, @{2 -> 8}), @{1 -> 2, 2 -> 9}))

@test
def differenceWith15(): Bool = assert!(Map.eq(Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, @{1 -> 2, 2 -> 3}, @{1 -> 8}), @{2 -> 3}))

@test
def differenceWith16(): Bool = assert!(Map.eq(Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, @{1 -> 9, 2 -> 3}, @{1 -> 8}), @{1 -> 9, 2 -> 3}))

@test
def differenceWith17(): Bool = assert!(Map.eq(Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, @{1 -> 1}, @{1 -> 2, 2 -> 3}), @{}))

@test
def differenceWith18(): Bool = assert!(Map.eq(Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, @{1 -> 4}, @{1 -> 2, 2 -> 3}), @{1 -> 4}))

@test
def differenceWith19(): Bool = assert!(Map.eq(Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, @{2 -> -4}, @{1 -> 2, 2 -> 3}), @{}))

@test
def differenceWith20(): Bool = assert!(Map.eq(Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, @{2 -> 7}, @{1 -> 2, 2 -> 3}), @{2 -> 7}))

@test
def differenceWith21(): Bool = assert!(Map.eq(Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, @{2 -> 7, 11 -> 14, 9 -> 8, 15 -> 22}, @{15 -> 21, 1 -> 2, 2 -> 8, 44 -> 33}),
                                   @{11 -> 14, 9 -> 8, 15 -> 22}))

/////////////////////////////////////////////////////////////////////////////
// differenceWithKey                                                       //
/////////////////////////////////////////////////////////////////////////////
@test
def differenceWithKey01(): Bool = assert!(Map.eq(Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, @{}, @{}), @{}: Map[Int, Int]))

@test
def differenceWithKey02(): Bool = assert!(Map.eq(Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, @{1 -> 2}, @{}), @{1 -> 2}))

@test
def differenceWithKey03(): Bool = assert!(Map.eq(Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, @{}, @{1 -> 2}), @{}))

@test
def differenceWithKey04(): Bool = assert!(Map.eq(Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, @{}, @{1 -> 2, 3 -> 4}), @{}))

@test
def differenceWithKey05(): Bool = assert!(Map.eq(Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, @{1 -> 2, 3 -> 4}, @{}), @{1 -> 2, 3 -> 4}))

@test
def differenceWithKey06(): Bool = assert!(Map.eq(Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, @{1 -> 2}, @{3 -> 4}), @{1 -> 2}))

@test
def differenceWithKey07(): Bool = assert!(Map.eq(Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, @{1 -> 2}, @{1 -> 5}), @{}))

@test
def differenceWithKey08(): Bool = assert!(Map.eq(Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, @{1 -> 5}, @{1 -> 4}), @{1 -> 6}))

@test
def differenceWithKey09(): Bool = assert!(Map.eq(Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, @{}, @{1 -> 2, 2 -> 3, 3 -> 4}), @{}))

@test
def differenceWithKey10(): Bool = assert!(Map.eq(Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, @{1 -> 2, 2 -> 3, 3 -> 4}, @{}), @{1 -> 2, 2 -> 3, 3 -> 4}))

@test
def differenceWithKey11(): Bool = assert!(Map.eq(Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, @{1 -> 2, 2 -> 3}, @{3 -> 4}), @{1 -> 2, 2 -> 3}))

@test
def differenceWithKey12(): Bool = assert!(Map.eq(Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, @{3 -> 4}, @{1 -> 2, 2 -> 3}), @{3 -> 4}))

@test
def differenceWithKey13(): Bool = assert!(Map.eq(Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, @{1 -> 2, 2 -> 3}, @{2 -> 8}), @{1 -> 2}))

@test
def differenceWithKey14(): Bool = assert!(Map.eq(Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, @{1 -> 2, 2 -> 9}, @{2 -> 8}), @{1 -> 2, 2 -> 11}))

@test
def differenceWithKey15(): Bool = assert!(Map.eq(Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, @{1 -> 2, 2 -> 3}, @{1 -> 8}), @{2 -> 3}))

@test
def differenceWithKey16(): Bool = assert!(Map.eq(Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, @{1 -> 9, 2 -> 3}, @{1 -> 8}), @{1 -> 10, 2 -> 3}))

@test
def differenceWithKey17(): Bool = assert!(Map.eq(Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, @{1 -> 1}, @{1 -> 2, 2 -> 3}), @{}))

@test
def differenceWithKey18(): Bool = assert!(Map.eq(Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, @{1 -> 4}, @{1 -> 2, 2 -> 3}), @{1 -> 5}))

@test
def differenceWithKey19(): Bool = assert!(Map.eq(Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, @{2 -> -4}, @{1 -> 2, 2 -> 3}), @{}))

@test
def differenceWithKey20(): Bool = assert!(Map.eq(Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, @{2 -> 7}, @{1 -> 2, 2 -> 3}), @{2 -> 9}))

@test
def differenceWithKey21(): Bool = assert!(Map.eq(Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, @{2 -> 7, 11 -> 14, 9 -> 8, 15 -> 22}, @{15 -> 21, 1 -> 2, 2 -> 8, 44 -> 33}),
                                   @{11 -> 14, 9 -> 8, 15 -> 37}))

/////////////////////////////////////////////////////////////////////////////
// toSet                                                                   //
/////////////////////////////////////////////////////////////////////////////
@test
def toSet01(): Bool = assert!(Set.eq(Map.toSet(@{}), #{}))

@test
def toSet02(): Bool = assert!(Set.eq(Map.toSet(@{1 -> 2}), #{(1, 2)}))

@test
def toSet03(): Bool = assert!(Set.eq(Map.toSet(@{1 -> 2, 3 -> 4}), #{(1, 2), (3, 4)}))

@test
def toSet04(): Bool = assert!(Set.eq(Map.toSet(@{1 -> 2, 2 -> 2, 3 -> 4}), #{(1, 2), (2, 2), (3, 4)}))

/////////////////////////////////////////////////////////////////////////////
// toList                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def toList01(): Bool = Map.toList(@{}) `assertEq!` Nil

@test
def toList02(): Bool = Map.toList(@{1 -> 2}) `assertEq!` (1, 2) :: Nil

@test
def toList03(): Bool = Map.toList(@{1 -> 2, 3 -> 4}) `assertEq!` (3, 4) :: (1, 2) :: Nil

@test
def toList04(): Bool = Map.toList(@{1 -> 2, 2 -> 2, 3 -> 4}) `assertEq!` (3, 4) :: (2, 2) :: (1, 2) :: Nil

/////////////////////////////////////////////////////////////////////////////
// eq                                                                      //
/////////////////////////////////////////////////////////////////////////////
@test
def eq01(): Bool = assert!(Map.eq(@{}, @{}))

@test
def eq02(): Bool = assertNot!(Map.eq(@{1 -> 2}, @{}))

@test
def eq03(): Bool = assertNot!(Map.eq(@{}, @{1 -> 2}))

@test
def eq04(): Bool = assertNot!(Map.eq(@{1 -> 2, 3 -> 4}, @{}))

@test
def eq05(): Bool = assertNot!(Map.eq(@{}, @{1 -> 2, 3 -> 4}))

@test
def eq06(): Bool = assert!(Map.eq(@{1 -> 2}, @{1 -> 2}))

@test
def eq07(): Bool = assertNot!(Map.eq(@{1 -> 3}, @{1 -> 2}))

@test
def eq08(): Bool = assertNot!(Map.eq(@{1 -> 2}, @{1 -> 3}))

@test
def eq09(): Bool = assertNot!(Map.eq(@{2 -> 2}, @{1 -> 2}))

@test
def eq10(): Bool = assertNot!(Map.eq(@{1 -> 2}, @{4 -> 2}))

@test
def eq11(): Bool = assertNot!(Map.eq(@{1 -> 2, 3 -> 4}, @{1 -> 2}))

@test
def eq12(): Bool = assertNot!(Map.eq(@{1 -> 2}, @{1 -> 2, 3 -> 4}))

@test
def eq13(): Bool = assert!(Map.eq(@{1 -> 2, 3 -> 4}, @{1 -> 2, 3 -> 4}))

@test
def eq14(): Bool = assert!(Map.eq(@{1 -> 2, 3 -> 4}, @{3 -> 4, 1 -> 2}))

@test
def eq15(): Bool = assertNot!(Map.eq(@{1 -> 1, 3 -> 4}, @{1 -> 2, 3 -> 4}))

@test
def eq16(): Bool = assertNot!(Map.eq(@{4 -> 2, 3 -> 4}, @{1 -> 2, 3 -> 4}))

@test
def eq17(): Bool = assertNot!(Map.eq(@{1 -> 2, 3 -> 4}, @{1 -> 2, 2 -> 4}))

@test
def eq18(): Bool = assertNot!(Map.eq(@{1 -> 2, 3 -> 4}, @{1 -> 2, 3 -> 8}))

@test
def eq19(): Bool = assertNot!(Map.eq(@{1 -> 2, 3 -> 4, 5 -> 6}, @{1 -> 2, 3 -> 4}))

@test
def eq20(): Bool = assertNot!(Map.eq(@{1 -> 2, 3 -> 4}, @{1 -> 2, 3 -> 4, 5 -> 6}))

@test
def eq21(): Bool = assert!(Map.eq(@{1 -> 2, 3 -> 4, 5 -> 6}, @{1 -> 2, 3 -> 4, 5 -> 6}))

@test
def eq22(): Bool = assert!(Map.eq(@{1 -> 2, 3 -> 4, 5 -> 6}, @{1 -> 2, 5 -> 6, 3 -> 4}))

@test
def eq23(): Bool = assert!(Map.eq(@{1 -> 2, 3 -> 4, 5 -> 6}, @{3 -> 4, 5 -> 6, 1 -> 2}))

@test
def eq24(): Bool = assert!(Map.eq(@{1 -> 2, 3 -> 4, 5 -> 6}, @{5 -> 6, 3 -> 4, 1 -> 2}))

@test
def eq25(): Bool = assert!(Map.eq(@{1 -> 2, 3 -> 4, 5 -> 6, 7 -> 8, 9 -> 10}, @{7 -> 8, 1 -> 2, 3 -> 4, 9 -> 10, 5 -> 6}))

}
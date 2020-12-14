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

use Core/Cmp.Ordering.{LessThan, Equal, GreaterThan};

namespace TestMap {

    use ToString.toString;

/////////////////////////////////////////////////////////////////////////////
// size                                                                    //
/////////////////////////////////////////////////////////////////////////////
@test
def size01(): Bool = Map.size(Map#{}) == 0

@test
def size02(): Bool = Map.size(Map#{1 -> 2}) == 1

@test
def size03(): Bool = Map.size(Map#{1 -> 2, 2 -> 4}) == 2

@test
def size04(): Bool = Map.size(Map#{1 -> 2, 2 -> 4, 3 -> 6}) == 3

@test
def size05(): Bool = Map.size(Map#{1 -> 2, 2 -> 4, 3 -> 6, 4 -> 8}) == 4

@test
def size06(): Bool = Map.size(Map#{1 -> 2, 2 -> 4, 3 -> 6, 4 -> 8, 5 -> 10}) == 5

/////////////////////////////////////////////////////////////////////////////
// empty                                                                   //
/////////////////////////////////////////////////////////////////////////////
@test
def empty01(): Bool = Map.__eq(Map.empty(), Map#{}) == true

/////////////////////////////////////////////////////////////////////////////
// singleton                                                               //
/////////////////////////////////////////////////////////////////////////////
@test
def singleton01(): Bool = Map.__eq(Map.singleton(1, 2), Map#{1 -> 2}) == true

@test
def singleton02(): Bool = Map.__eq(Map.singleton(3, -1), Map#{3 -> -1}) == true

@test
def singleton03(): Bool = Map.__eq(Map.singleton(-99, -11), Map#{-99 -> -11}) == true

/////////////////////////////////////////////////////////////////////////////
// isEmpty                                                                 //
/////////////////////////////////////////////////////////////////////////////
@test
def isEmpty01(): Bool = Map.isEmpty(Map#{}) == true

@test
def isEmpty02(): Bool = Map.isEmpty(Map#{1 -> 2}) == false

@test
def isEmpty03(): Bool = Map.isEmpty(Map#{1 -> 2, 2 -> 4}) == false

@test
def isEmpty04(): Bool = Map.isEmpty(Map#{1 -> 2, 2 -> 4, 3 -> 6}) == false

@test
def isEmpty05(): Bool = Map.isEmpty(Map#{1 -> 2, 2 -> 4, 3 -> 6, 4 -> 8}) == false

/////////////////////////////////////////////////////////////////////////////
// get                                                                     //
/////////////////////////////////////////////////////////////////////////////
@test
def get01(): Bool = Map.get(2, Map#{}) == None

@test
def get02(): Bool = Map.get(2, Map#{1 -> 2}) == None

@test
def get03(): Bool = Map.get(2, Map#{2 -> 1}) == Some(1)

@test
def get04(): Bool = Map.get(5, Map#{2 -> 1, 3 -> 17}) == None

@test
def get05(): Bool = Map.get(5, Map#{2 -> 1, 5 -> 17}) == Some(17)

@test
def get06(): Bool = Map.get(5, Map#{5 -> 1, 3 -> 17}) == Some(1)

@test
def get07(): Bool = Map.get(-2, Map#{2 -> 1, 3 -> 17, -1 -> -2}) == None

@test
def get08(): Bool = Map.get(-2, Map#{-2 -> 1, 3 -> 17, -1 -> -2}) == Some(1)

@test
def get09(): Bool = Map.get(-2, Map#{2 -> 1, -2 -> 17, -1 -> -2}) == Some(17)

@test
def get10(): Bool = Map.get(-2, Map#{2 -> 1, 3 -> 17, -2 -> -2}) == Some(-2)

/////////////////////////////////////////////////////////////////////////////
// getWithDefault                                                          //
/////////////////////////////////////////////////////////////////////////////
@test
def getWithDefault01(): Bool = Map.getWithDefault(2, 34, Map#{}) == 34

@test
def getWithDefault02(): Bool = Map.getWithDefault(2, 34, Map#{1 -> 2}) == 34

@test
def getWithDefault03(): Bool = Map.getWithDefault(2, 34, Map#{2 -> 1}) == 1

@test
def getWithDefault04(): Bool = Map.getWithDefault(5, 34, Map#{2 -> 1, 3 -> 17}) == 34

@test
def getWithDefault05(): Bool = Map.getWithDefault(5, 34, Map#{2 -> 1, 5 -> 17}) == 17

@test
def getWithDefault06(): Bool = Map.getWithDefault(5, 34, Map#{5 -> 1, 3 -> 17}) == 1

@test
def getWithDefault07(): Bool = Map.getWithDefault(-2, 34, Map#{2 -> 1, 3 -> 17, -1 -> -2}) == 34

@test
def getWithDefault08(): Bool = Map.getWithDefault(-2, 34, Map#{-2 -> 1, 3 -> 17, -1 -> -2}) == 1

@test
def getWithDefault09(): Bool = Map.getWithDefault(-2, 34, Map#{2 -> 1, -2 -> 17, -1 -> -2}) == 17

@test
def getWithDefault10(): Bool = Map.getWithDefault(-2, 34, Map#{2 -> 1, 3 -> 17, -2 -> -2}) == -2

/////////////////////////////////////////////////////////////////////////////
// memberOf                                                                //
/////////////////////////////////////////////////////////////////////////////
@test
def memberOf01(): Bool = Map.memberOf(2, Map#{}) == false

@test
def memberOf02(): Bool = Map.memberOf(2, Map#{1 -> 2}) == false

@test
def memberOf03(): Bool = Map.memberOf(2, Map#{2 -> 1}) == true

@test
def memberOf04(): Bool = Map.memberOf(5, Map#{2 -> 1, 3 -> 17}) == false

@test
def memberOf05(): Bool = Map.memberOf(5, Map#{2 -> 1, 5 -> 17}) == true

@test
def memberOf06(): Bool = Map.memberOf(5, Map#{5 -> 1, 3 -> 17}) == true

@test
def memberOf07(): Bool = Map.memberOf(-2, Map#{2 -> 1, 3 -> 17, -1 -> -2}) == false

@test
def memberOf08(): Bool = Map.memberOf(-2, Map#{-2 -> 1, 3 -> 17, -1 -> -2}) == true

@test
def memberOf09(): Bool = Map.memberOf(-2, Map#{2 -> 1, -2 -> 17, -1 -> -2}) == true

@test
def memberOf10(): Bool = Map.memberOf(-2, Map#{2 -> 1, 3 -> 17, -2 -> -2}) == true

/////////////////////////////////////////////////////////////////////////////
// keysOf                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def keysOf01(): Bool = Set.__eq(Map.keysOf(Map#{}), Set#{}) == true

@test
def keysOf02(): Bool = Set.__eq(Map.keysOf(Map#{1 -> 2}), Set#{1}) == true

@test
def keysOf03(): Bool = Set.__eq(Map.keysOf(Map#{1 -> 2, 2 -> 4}), Set#{1, 2}) == true

@test
def keysOf04(): Bool = Set.__eq(Map.keysOf(Map#{1 -> 2, 2 -> 4, 3 -> 6}), Set#{1, 2, 3}) == true

@test
def keysOf05(): Bool = Set.__eq(Map.keysOf(Map#{1 -> 2, 2 -> 4, 3 -> 6, 4 -> 8}), Set#{1, 2, 3, 4}) == true

@test
def keysOf06(): Bool = Set.__eq(Map.keysOf(Map#{1 -> 2, 2 -> 4, 3 -> 6, 4 -> 8, 5 -> 10}), Set#{1, 2, 3, 4, 5}) == true

/////////////////////////////////////////////////////////////////////////////
// valuesOf                                                                //
/////////////////////////////////////////////////////////////////////////////
@test
def valuesOf01(): Bool = Map.valuesOf(Map#{}) == Nil

@test
def valuesOf02(): Bool = Map.valuesOf(Map#{1 -> 2}) == 2 :: Nil

@test
def valuesOf03(): Bool = Map.valuesOf(Map#{1 -> 2, 2 -> 4}) == 2 :: 4 :: Nil

@test
def valuesOf04(): Bool = Map.valuesOf(Map#{1 -> 2, 2 -> 4, 3 -> 6}) == 2 :: 4 :: 6 :: Nil

@test
def valuesOf05(): Bool = Map.valuesOf(Map#{1 -> 2, 2 -> 4, 3 -> 6, 4 -> 8}) == 2 :: 4 :: 6 :: 8 :: Nil

@test
def valuesOf06(): Bool = Map.valuesOf(Map#{1 -> 2, 2 -> 4, 3 -> 6, 4 -> 8, 5 -> 10}) == 2 :: 4 :: 6 :: 8 :: 10 :: Nil

@test
def valuesOf07(): Bool = Map.valuesOf(Map#{1 -> -11, 2 -> 4, 3 -> -5, 4 -> 7, 5 -> -5}) == -11 :: 4 :: -5 :: 7 :: -5 :: Nil

/////////////////////////////////////////////////////////////////////////////
// insert                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def insert01(): Bool = Map.__eq(Map.insert(1, 3, Map#{}), Map#{1 -> 3}) == true

@test
def insert02(): Bool = Map.__eq(Map.insert(1, 3, Map#{1 -> 4}), Map#{1 -> 3}) == true

@test
def insert03(): Bool = Map.__eq(Map.insert(2, 3, Map#{1 -> 4}), Map#{1 -> 4, 2 -> 3}) == true

@test
def insert04(): Bool = Map.__eq(Map.insert(1, 1, Map#{1 -> 4, 5 -> -2}), Map#{1 -> 1, 5 -> -2}) == true

@test
def insert05(): Bool = Map.__eq(Map.insert(5, 1, Map#{1 -> 4, 5 -> -2}), Map#{1 -> 4, 5 -> 1}) == true

@test
def insert06(): Bool = Map.__eq(Map.insert(4, -2, Map#{1 -> 4, 5 -> -2}), Map#{1 -> 4, 5 -> -2, 4 -> -2}) == true

/////////////////////////////////////////////////////////////////////////////
// insertWith                                                              //
/////////////////////////////////////////////////////////////////////////////
@test
def insertWith01(): Bool = Map.__eq(Map.insertWith((v1, v2) -> v1 + v2, 1, 3, Map#{}), Map#{1 -> 3}) == true

@test
def insertWith02(): Bool = Map.__eq(Map.insertWith((v1, v2) -> v1 + v2, 1, 3, Map#{1 -> 4}), Map#{1 -> 7}) == true

@test
def insertWith03(): Bool = Map.__eq(Map.insertWith((v1, v2) -> v1 + v2, 2, 3, Map#{1 -> 4}), Map#{1 -> 4, 2 -> 3}) == true

@test
def insertWith04(): Bool = Map.__eq(Map.insertWith((v1, v2) -> v1 + v2, 1, 1, Map#{1 -> 4, 5 -> -2}), Map#{1 -> 5, 5 -> -2}) == true

@test
def insertWith05(): Bool = Map.__eq(Map.insertWith((v1, v2) -> v1 + v2, 5, 1, Map#{1 -> 4, 5 -> -2}), Map#{1 -> 4, 5 -> -1}) == true

@test
def insertWith06(): Bool = Map.__eq(Map.insertWith((v1, v2) -> v1 + v2, 4, -2, Map#{1 -> 4, 5 -> -2}), Map#{1 -> 4, 5 -> -2, 4 -> -2}) == true

/////////////////////////////////////////////////////////////////////////////
// insertWithKey                                                           //
/////////////////////////////////////////////////////////////////////////////
@test
def insertWithKey01(): Bool = Map.__eq(Map.insertWithKey((k, v1, v2) -> k + v1 + v2, 1, 3, Map#{}), Map#{1 -> 3}) == true

@test
def insertWithKey02(): Bool = Map.__eq(Map.insertWithKey((k, v1, v2) -> k + v1 + v2, 1, 3, Map#{1 -> 4}), Map#{1 -> 8}) == true

@test
def insertWithKey03(): Bool = Map.__eq(Map.insertWithKey((k, v1, v2) -> k + v1 + v2, 2, 3, Map#{1 -> 4}), Map#{1 -> 4, 2 -> 3}) == true

@test
def insertWithKey04(): Bool = Map.__eq(Map.insertWithKey((k, v1, v2) -> k + v1 + v2, 1, 1, Map#{1 -> 4, 5 -> -2}), Map#{1 -> 6, 5 -> -2}) == true

@test
def insertWithKey05(): Bool = Map.__eq(Map.insertWithKey((k, v1, v2) -> k + v1 + v2, 5, 1, Map#{1 -> 4, 5 -> -2}), Map#{1 -> 4, 5 -> 4}) == true

@test
def insertWithKey06(): Bool = Map.__eq(Map.insertWithKey((k, v1, v2) -> k + v1 + v2, 4, -2, Map#{1 -> 4, 5 -> -2}), Map#{1 -> 4, 5 -> -2, 4 -> -2}) == true

/////////////////////////////////////////////////////////////////////////////
// adjust                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def adjust01(): Bool = Map.__eq(Map.adjust(v -> 2*v, 1, Map#{}), Map#{}) == true

@test
def adjust02(): Bool = Map.__eq(Map.adjust(v -> 2*v, 1, Map#{1 -> 4}), Map#{1 -> 8}) == true

@test
def adjust03(): Bool = Map.__eq(Map.adjust(v -> 2*v, 2, Map#{1 -> 4}), Map#{1 -> 4}) == true

@test
def adjust04(): Bool = Map.__eq(Map.adjust(v -> 2*v, 1, Map#{1 -> -14, 5 -> -2}), Map#{1 -> -28, 5 -> -2}) == true

@test
def adjust05(): Bool = Map.__eq(Map.adjust(v -> 2*v, 5, Map#{1 -> 4, 5 -> -2}), Map#{1 -> 4, 5 -> -4}) == true

@test
def adjust06(): Bool = Map.__eq(Map.adjust(v -> 2*v, 4, Map#{1 -> 4, 5 -> -2}), Map#{1 -> 4, 5 -> -2}) == true

/////////////////////////////////////////////////////////////////////////////
// adjustWithKey                                                           //
/////////////////////////////////////////////////////////////////////////////
@test
def adjustWithKey01(): Bool = Map.__eq(Map.adjustWithKey((k, v) -> k+v, 1, Map#{}), Map#{}) == true

@test
def adjustWithKey02(): Bool = Map.__eq(Map.adjustWithKey((k, v) -> k+v, 1, Map#{1 -> 4}), Map#{1 -> 5}) == true

@test
def adjustWithKey03(): Bool = Map.__eq(Map.adjustWithKey((k, v) -> k+v, 2, Map#{1 -> 4}), Map#{1 -> 4}) == true

@test
def adjustWithKey04(): Bool = Map.__eq(Map.adjustWithKey((k, v) -> k+v, 1, Map#{1 -> -14, 5 -> -2}), Map#{1 -> -13, 5 -> -2}) == true

@test
def adjustWithKey05(): Bool = Map.__eq(Map.adjustWithKey((k, v) -> k+v, 5, Map#{1 -> 4, 5 -> -2}), Map#{1 -> 4, 5 -> 3}) == true

@test
def adjustWithKey06(): Bool = Map.__eq(Map.adjustWithKey((k, v) -> k+v, 4, Map#{1 -> 4, 5 -> -2}), Map#{1 -> 4, 5 -> -2}) == true

/////////////////////////////////////////////////////////////////////////////
// update                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def update01(): Bool = Map.__eq(Map.update(v -> if (v % 2 != 0) Some(2*v) else None, 1, Map#{}), Map#{}) == true

@test
def update02(): Bool = Map.__eq(Map.update(v -> if (v % 2 != 0) Some(2*v) else None, 1, Map#{1 -> 3}), Map#{1 -> 6}) == true

@test
def update03(): Bool = Map.__eq(Map.update(v -> if (v % 2 != 0) Some(2*v) else None, 1, Map#{1 -> 4}), Map#{1 -> 4}) == true

@test
def update04(): Bool = Map.__eq(Map.update(v -> if (v % 2 != 0) Some(2*v) else None, 2, Map#{1 -> 4}), Map#{1 -> 4}) == true

@test
def update05(): Bool = Map.__eq(Map.update(v -> if (v % 2 != 0) Some(2*v) else None, 1, Map#{1 -> -14, 5 -> -2}), Map#{1 -> -14, 5 -> -2}) == true

@test
def update06(): Bool = Map.__eq(Map.update(v -> if (v % 2 != 0) Some(2*v) else None, 1, Map#{1 -> -13, 5 -> -2}), Map#{1 -> -26, 5 -> -2}) == true

@test
def update07(): Bool = Map.__eq(Map.update(v -> if (v % 2 != 0) Some(2*v) else None, 5, Map#{1 -> 4, 5 -> -2}), Map#{1 -> 4, 5 -> -2}) == true

@test
def update08(): Bool = Map.__eq(Map.update(v -> if (v % 2 != 0) Some(2*v) else None, 5, Map#{1 -> 4, 5 -> -1}), Map#{1 -> 4, 5 -> -2}) == true

@test
def update09(): Bool = Map.__eq(Map.update(v -> if (v % 2 != 0) Some(2*v) else None, 4, Map#{1 -> 4, 5 -> -2}), Map#{1 -> 4, 5 -> -2}) == true

/////////////////////////////////////////////////////////////////////////////
// updateWithKey                                                           //
/////////////////////////////////////////////////////////////////////////////
@test
def updateWithKey01(): Bool = Map.__eq(Map.updateWithKey((k, v) -> if (v % 2 != 0) Some(k+2*v) else None, 1, Map#{}), Map#{}) == true

@test
def updateWithKey02(): Bool = Map.__eq(Map.updateWithKey((k, v) -> if (v % 2 != 0) Some(k+2*v) else None, 1, Map#{1 -> 3}), Map#{1 -> 7}) == true

@test
def updateWithKey03(): Bool = Map.__eq(Map.updateWithKey((k, v) -> if (v % 2 != 0) Some(k+2*v) else None, 1, Map#{1 -> 4}), Map#{1 -> 4}) == true

@test
def updateWithKey04(): Bool = Map.__eq(Map.updateWithKey((k, v) -> if (v % 2 != 0) Some(k+2*v) else None, 2, Map#{1 -> 4}), Map#{1 -> 4}) == true

@test
def updateWithKey05(): Bool = Map.__eq(Map.updateWithKey((k, v) -> if (v % 2 != 0) Some(k+2*v) else None, 1, Map#{1 -> -14, 5 -> -2}), Map#{1 -> -14, 5 -> -2}) == true

@test
def updateWithKey06(): Bool = Map.__eq(Map.updateWithKey((k, v) -> if (v % 2 != 0) Some(k+2*v) else None, 1, Map#{1 -> -13, 5 -> -2}), Map#{1 -> -25, 5 -> -2}) == true

@test
def updateWithKey07(): Bool = Map.__eq(Map.updateWithKey((k, v) -> if (v % 2 != 0) Some(k+2*v) else None, 5, Map#{1 -> 4, 5 -> -2}), Map#{1 -> 4, 5 -> -2}) == true

@test
def updateWithKey08(): Bool = Map.__eq(Map.updateWithKey((k, v) -> if (v % 2 != 0) Some(k+2*v) else None, 5, Map#{1 -> 4, 5 -> -1}), Map#{1 -> 4, 5 -> 3}) == true

@test
def updateWithKey09(): Bool = Map.__eq(Map.updateWithKey((k, v) -> if (v % 2 != 0) Some(k+2*v) else None, 4, Map#{1 -> 4, 5 -> -2}), Map#{1 -> 4, 5 -> -2}) == true

/////////////////////////////////////////////////////////////////////////////
// delete                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def delete01(): Bool = Map.__eq(Map.delete(1, Map#{}), Map#{}) == true

@test
def delete02(): Bool = Map.__eq(Map.delete(1, Map#{2 -> 4}), Map#{2 -> 4}) == true

@test
def delete03(): Bool = Map.__eq(Map.delete(1, Map#{1 -> 4}), Map#{}) == true

@test
def delete04(): Bool = Map.__eq(Map.delete(1, Map#{6 -> 1, 2 -> 4}), Map#{6 -> 1, 2 -> 4}) == true

@test
def delete05(): Bool = Map.__eq(Map.delete(6, Map#{6 -> 1, 2 -> 4}), Map#{2 -> 4}) == true

@test
def delete06(): Bool = Map.__eq(Map.delete(2, Map#{6 -> 1, 2 -> 4}), Map#{6 -> 1}) == true

@test
def delete07(): Bool = Map.__eq(Map.delete(1, Map#{6 -> 1, 2 -> 4, 8 -> 89}), Map#{6 -> 1, 2 -> 4, 8 -> 89}) == true

@test
def delete08(): Bool = Map.__eq(Map.delete(6, Map#{6 -> 1, 2 -> 4, 8 -> 89}), Map#{2 -> 4, 8 -> 89}) == true

@test
def delete09(): Bool = Map.__eq(Map.delete(2, Map#{6 -> 1, 2 -> 4, 8 -> 89}), Map#{6 -> 1, 8 -> 89}) == true

@test
def delete10(): Bool = Map.__eq(Map.delete(8, Map#{6 -> 1, 2 -> 4, 8 -> 89}), Map#{6 -> 1, 2 -> 4}) == true

/////////////////////////////////////////////////////////////////////////////
// isSubmapOf                                                              //
/////////////////////////////////////////////////////////////////////////////
@test
def isSubmapOf01(): Bool = Map.isSubmapOf(Map#{}, Map#{}) == true

@test
def isSubmapOf02(): Bool = Map.isSubmapOf(Map#{1 -> 2}, Map#{}) == false

@test
def isSubmapOf03(): Bool = Map.isSubmapOf(Map#{3 -> 4}, Map#{}) == false

@test
def isSubmapOf04(): Bool = Map.isSubmapOf(Map#{1 -> 3}, Map#{1 -> 3}) == true

@test
def isSubmapOf05(): Bool = Map.isSubmapOf(Map#{2 -> 3}, Map#{1 -> 3}) == false

@test
def isSubmapOf06(): Bool = Map.isSubmapOf(Map#{1 -> 4}, Map#{1 -> 3}) == false

@test
def isSubmapOf07(): Bool = Map.isSubmapOf(Map#{1 -> 3, 2 -> 3}, Map#{1 -> 3}) == false

@test
def isSubmapOf08(): Bool = Map.isSubmapOf(Map#{1 -> 3, 2 -> 3}, Map#{2 -> 3}) == false

@test
def isSubmapOf09(): Bool = Map.isSubmapOf(Map#{}, Map#{2 -> 4, 1 -> 3}) == true

@test
def isSubmapOf10(): Bool = Map.isSubmapOf(Map#{2 -> 4}, Map#{2 -> 4, 1 -> 3}) == true

@test
def isSubmapOf11(): Bool = Map.isSubmapOf(Map#{1 -> 3}, Map#{2 -> 4, 1 -> 3}) == true

@test
def isSubmapOf12(): Bool = Map.isSubmapOf(Map#{1 -> 3, 2 -> 4}, Map#{1 -> 3, 2 -> 4}) == true

@test
def isSubmapOf13(): Bool = Map.isSubmapOf(Map#{2 -> 4, 1 -> 3}, Map#{1 -> 3, 2 -> 4}) == true

@test
def isSubmapOf14(): Bool = Map.isSubmapOf(Map#{2 -> 5, 1 -> 3}, Map#{1 -> 3, 2 -> 4}) == false

@test
def isSubmapOf15(): Bool = Map.isSubmapOf(Map#{2 -> 4, 1 -> -1}, Map#{1 -> 3, 2 -> 4}) == false

@test
def isSubmapOf16(): Bool = Map.isSubmapOf(Map#{3 -> 4, 1 -> 3}, Map#{1 -> 3, 2 -> 4}) == false

@test
def isSubmapOf17(): Bool = Map.isSubmapOf(Map#{2 -> 4, 3 -> 3}, Map#{1 -> 3, 2 -> 4}) == false

@test
def isSubmapOf18(): Bool = Map.isSubmapOf(Map#{2 -> 4, 1 -> 3, 8 -> 9}, Map#{1 -> 3, 2 -> 4}) == false

/////////////////////////////////////////////////////////////////////////////
// isProperSubmapOf                                                        //
/////////////////////////////////////////////////////////////////////////////
@test
def isProperSubmapOf01(): Bool = Map.isProperSubmapOf(Map#{}, Map#{}) == false

@test
def isProperSubmapOf02(): Bool = Map.isProperSubmapOf(Map#{1 -> 2}, Map#{}) == false

@test
def isProperSubmapOf03(): Bool = Map.isProperSubmapOf(Map#{3 -> 4}, Map#{}) == false

@test
def isProperSubmapOf04(): Bool = Map.isProperSubmapOf(Map#{1 -> 3}, Map#{1 -> 3}) == false

@test
def isProperSubmapOf05(): Bool = Map.isProperSubmapOf(Map#{2 -> 3}, Map#{1 -> 3}) == false

@test
def isProperSubmapOf06(): Bool = Map.isProperSubmapOf(Map#{1 -> 4}, Map#{1 -> 3}) == false

@test
def isProperSubmapOf07(): Bool = Map.isProperSubmapOf(Map#{1 -> 3, 2 -> 3}, Map#{1 -> 3}) == false

@test
def isProperSubmapOf08(): Bool = Map.isProperSubmapOf(Map#{1 -> 3, 2 -> 3}, Map#{2 -> 3}) == false

@test
def isProperSubmapOf09(): Bool = Map.isProperSubmapOf(Map#{}, Map#{2 -> 4, 1 -> 3}) == true

@test
def isProperSubmapOf10(): Bool = Map.isProperSubmapOf(Map#{2 -> 4}, Map#{2 -> 4, 1 -> 3}) == true

@test
def isProperSubmapOf11(): Bool = Map.isProperSubmapOf(Map#{1 -> 3}, Map#{2 -> 4, 1 -> 3}) == true

@test
def isProperSubmapOf12(): Bool = Map.isProperSubmapOf(Map#{1 -> 3, 2 -> 4}, Map#{1 -> 3, 2 -> 4}) == false

@test
def isProperSubmapOf13(): Bool = Map.isProperSubmapOf(Map#{2 -> 4, 1 -> 3}, Map#{1 -> 3, 2 -> 4}) == false

@test
def isProperSubmapOf14(): Bool = Map.isProperSubmapOf(Map#{2 -> 5, 1 -> 3}, Map#{1 -> 3, 2 -> 4}) == false

@test
def isProperSubmapOf15(): Bool = Map.isProperSubmapOf(Map#{2 -> 4, 1 -> -1}, Map#{1 -> 3, 2 -> 4}) == false

@test
def isProperSubmapOf16(): Bool = Map.isProperSubmapOf(Map#{3 -> 4, 1 -> 3}, Map#{1 -> 3, 2 -> 4}) == false

@test
def isProperSubmapOf17(): Bool = Map.isProperSubmapOf(Map#{2 -> 4, 3 -> 3}, Map#{1 -> 3, 2 -> 4}) == false

@test
def isProperSubmapOf18(): Bool = Map.isProperSubmapOf(Map#{2 -> 4, 1 -> 3, 8 -> 9}, Map#{1 -> 3, 2 -> 4}) == false

/////////////////////////////////////////////////////////////////////////////
// find                                                                    //
/////////////////////////////////////////////////////////////////////////////
@test
def find01(): Bool = Map.find((k, v) -> k == v, Map#{}) == None

@test
def find02(): Bool = Map.find((k, v) -> k == v, Map#{1 -> 2}) == None

@test
def find03(): Bool = Map.find((k, v) -> k == v, Map#{1 -> 1}) == Some((1, 1))

@test
def find04(): Bool = Map.find((k, v) -> k == v, Map#{1 -> 2, 2 -> 3}) == None

@test
def find05(): Bool = Map.find((k, v) -> k == v, Map#{1 -> 1, 2 -> 3}) == Some((1, 1))

@test
def find06(): Bool = Map.find((k, v) -> k == v, Map#{1 -> 2, 2 -> 2}) == Some((2, 2))

@test
def find07(): Bool = Map.find((k, v) -> k == v, Map#{1 -> 1, 2 -> 2}) == Some((1, 1))

/////////////////////////////////////////////////////////////////////////////
// findLeft                                                                //
/////////////////////////////////////////////////////////////////////////////
@test
def findLeft01(): Bool = Map.findLeft((k, v) -> k == v, Map#{}) == None

@test
def findLeft02(): Bool = Map.findLeft((k, v) -> k == v, Map#{1 -> 2}) == None

@test
def findLeft03(): Bool = Map.findLeft((k, v) -> k == v, Map#{1 -> 1}) == Some((1, 1))

@test
def findLeft04(): Bool = Map.findLeft((k, v) -> k == v, Map#{1 -> 2, 2 -> 3}) == None

@test
def findLeft05(): Bool = Map.findLeft((k, v) -> k == v, Map#{1 -> 1, 2 -> 3}) == Some((1, 1))

@test
def findLeft06(): Bool = Map.findLeft((k, v) -> k == v, Map#{1 -> 2, 2 -> 2}) == Some((2, 2))

@test
def findLeft07(): Bool = Map.findLeft((k, v) -> k == v, Map#{1 -> 1, 2 -> 2}) == Some((1, 1))

/////////////////////////////////////////////////////////////////////////////
// findRight                                                               //
/////////////////////////////////////////////////////////////////////////////
@test
def findRight01(): Bool = Map.findRight((k, v) -> k == v, Map#{}) == None

@test
def findRight02(): Bool = Map.findRight((k, v) -> k == v, Map#{1 -> 2}) == None

@test
def findRight03(): Bool = Map.findRight((k, v) -> k == v, Map#{1 -> 1}) == Some((1, 1))

@test
def findRight04(): Bool = Map.findRight((k, v) -> k == v, Map#{1 -> 2, 2 -> 3}) == None

@test
def findRight05(): Bool = Map.findRight((k, v) -> k == v, Map#{1 -> 1, 2 -> 3}) == Some((1, 1))

@test
def findRight06(): Bool = Map.findRight((k, v) -> k == v, Map#{1 -> 2, 2 -> 2}) == Some((2, 2))

@test
def findRight07(): Bool = Map.findRight((k, v) -> k == v, Map#{1 -> 1, 2 -> 2}) == Some((2, 2))

/////////////////////////////////////////////////////////////////////////////
// filter                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def filter01(): Bool = Map.__eq(Map.filter(v -> v % 2 == 0, Map#{}), Map#{}) == true

@test
def filter02(): Bool = Map.__eq(Map.filter(v -> v % 2 == 0, Map#{1 -> 2}), Map#{1 -> 2}) == true

@test
def filter03(): Bool = Map.__eq(Map.filter(v -> v % 2 == 0, Map#{1 -> 3}), Map#{}) == true

@test
def filter04(): Bool = Map.__eq(Map.filter(v -> v % 2 == 0, Map#{1 -> 8, 2 -> 4}), Map#{1 -> 8, 2 -> 4}) == true

@test
def filter05(): Bool = Map.__eq(Map.filter(v -> v % 2 == 0, Map#{1 -> 7, 2 -> 4}), Map#{2 -> 4}) == true

@test
def filter06(): Bool = Map.__eq(Map.filter(v -> v % 2 == 0, Map#{1 -> 8, 2 -> 3}), Map#{1 -> 8}) == true

@test
def filter07(): Bool = Map.__eq(Map.filter(v -> v % 2 == 0, Map#{1 -> -1, 2 -> -3}), Map#{}) == true

/////////////////////////////////////////////////////////////////////////////
// filterWithKey                                                           //
/////////////////////////////////////////////////////////////////////////////
@test
def filterWithKey01(): Bool = Map.__eq(Map.filterWithKey((k, v) -> k == v, Map#{}), Map#{}) == true

@test
def filterWithKey02(): Bool = Map.__eq(Map.filterWithKey((k, v) -> k == v, Map#{1 -> 1}), Map#{1 -> 1}) == true

@test
def filterWithKey03(): Bool = Map.__eq(Map.filterWithKey((k, v) -> k == v, Map#{1 -> 3}), Map#{}) == true

@test
def filterWithKey04(): Bool = Map.__eq(Map.filterWithKey((k, v) -> k == v, Map#{1 -> 1, 2 -> 2}), Map#{1 -> 1, 2 -> 2}) == true

@test
def filterWithKey05(): Bool = Map.__eq(Map.filterWithKey((k, v) -> k == v, Map#{1 -> 7, 2 -> 2}), Map#{2 -> 2}) == true

@test
def filterWithKey06(): Bool = Map.__eq(Map.filterWithKey((k, v) -> k == v, Map#{1 -> 1, 2 -> 3}), Map#{1 -> 1}) == true

@test
def filterWithKey07(): Bool = Map.__eq(Map.filterWithKey((k, v) -> k == v, Map#{1 -> -1, 2 -> -3}), Map#{}) == true

/////////////////////////////////////////////////////////////////////////////
// map                                                                     //
/////////////////////////////////////////////////////////////////////////////
@test
def map01(): Bool = Map.__eq(Map.map(v -> 3*v, Map#{}), Map#{}) == true

@test
def map02(): Bool = Map.__eq(Map.map(v -> 3*v, Map#{1 -> 4}), Map#{1 -> 12}) == true

@test
def map03(): Bool = Map.__eq(Map.map(v -> 3*v, Map#{2 -> -1, 0 -> 0}), Map#{2 -> -3, 0 -> 0}) == true

@test
def map04(): Bool = Map.__eq(Map.map(v -> 3*v, Map#{2 -> -1, 5 -> 15, 11 -> -9}), Map#{2 -> -3, 5 -> 45, 11 -> -27}) == true

@test
def map05(): Bool = Map.__eq(Map.map(v -> 3*v, Map#{2 -> -1, 5 -> 15, 11 -> -9, 8 -> 8}), Map#{2 -> -3, 5 -> 45, 11 -> -27, 8 -> 24}) == true

/////////////////////////////////////////////////////////////////////////////
// mapWithKey                                                              //
/////////////////////////////////////////////////////////////////////////////
@test
def mapWithKey01(): Bool = Map.__eq(Map.mapWithKey((_, v) -> v, Map#{}), Map#{}) == true

@test
def mapWithKey02(): Bool = Map.__eq(Map.mapWithKey((k, v) -> k + v, Map#{1 -> 4}), Map#{1 -> 5}) == true

@test
def mapWithKey03(): Bool = Map.__eq(Map.mapWithKey((k, v) -> k + v, Map#{2 -> -1, 0 -> 0}), Map#{2 -> 1, 0 -> 0}) == true

@test
def mapWithKey04(): Bool = Map.__eq(Map.mapWithKey((k, v) -> k + v, Map#{2 -> -1, 5 -> 15, 11 -> -9}), Map#{2 -> 1, 5 -> 20, 11 -> 2}) == true

@test
def mapWithKey05(): Bool = Map.__eq(Map.mapWithKey((k, v) -> k + v, Map#{2 -> -1, 5 -> 15, 11 -> -9, 8 -> 8}),
                                        Map#{2 -> 1, 5 -> 20, 11 -> 2, 8 -> 16}) == true

/////////////////////////////////////////////////////////////////////////////
// fold                                                                    //
/////////////////////////////////////////////////////////////////////////////
@test
def fold01(): Bool = Map.fold((s, v) -> s + v, 0, Map#{}) == 0

@test
def fold02(): Bool = Map.fold((s, v) -> s + v, 0, Map#{1 -> 2}) == 2

@test
def fold03(): Bool = Map.fold((s, v) -> s + v, 0, Map#{1 -> 2, 3 -> 4}) == 6

@test
def fold04(): Bool = Map.fold((s, v) -> s + v, 0, Map#{1 -> 2, 3 -> 4, 5 -> 6}) == 12

/////////////////////////////////////////////////////////////////////////////
// foldWithKey                                                             //
/////////////////////////////////////////////////////////////////////////////
@test
def foldWithKey01(): Bool = Map.foldWithKey((k, s, v) -> k + s + v, 0, Map#{}) == 0

@test
def foldWithKey02(): Bool = Map.foldWithKey((k, s, v) -> k + s + v, 0, Map#{1 -> 2}) == 3

@test
def foldWithKey03(): Bool = Map.foldWithKey((k, s, v) -> k + s + v, 0, Map#{1 -> 2, 3 -> 4}) == 10

@test
def foldWithKey04(): Bool = Map.foldWithKey((k, s, v) -> k + s + v, 0, Map#{1 -> 2, 3 -> 4, 5 -> 6}) == 21

/////////////////////////////////////////////////////////////////////////////
// foldLeft                                                                //
/////////////////////////////////////////////////////////////////////////////
@test
def foldLeft01(): Bool = Map.foldLeft((s, v) -> s + v, 0, Map#{}) == 0

@test
def foldLeft02(): Bool = Map.foldLeft((s, v) -> s + v, 0, Map#{1 -> 2}) == 2

@test
def foldLeft03(): Bool = Map.foldLeft((s, v) -> s + v, 0, Map#{1 -> 2, 3 -> 4}) == 6

@test
def foldLeft04(): Bool = Map.foldLeft((s, v) -> s + v, 0, Map#{1 -> 2, 3 -> 4, 5 -> 6}) == 12

/////////////////////////////////////////////////////////////////////////////
// foldLeftWithKey                                                         //
/////////////////////////////////////////////////////////////////////////////
@test
def foldLeftWithKey01(): Bool = Map.foldLeftWithKey((s, k, v) -> k + s + v, 0, Map#{}) == 0

@test
def foldLeftWithKey02(): Bool = Map.foldLeftWithKey((s, k, v) -> k + s + v, 0, Map#{1 -> 2}) == 3

@test
def foldLeftWithKey03(): Bool = Map.foldLeftWithKey((s, k, v) -> k + s + v, 0, Map#{1 -> 2, 3 -> 4}) == 10

@test
def foldLeftWithKey04(): Bool = Map.foldLeftWithKey((s, k, v) -> k + s + v, 0, Map#{1 -> 2, 3 -> 4, 5 -> 6}) == 21

/////////////////////////////////////////////////////////////////////////////
// foldRight                                                               //
/////////////////////////////////////////////////////////////////////////////
@test
def foldRight01(): Bool = Map.foldRight((v, s) -> s + v, 0, Map#{}) == 0

@test
def foldRight02(): Bool = Map.foldRight((v, s) -> s + v, 0, Map#{1 -> 2}) == 2

@test
def foldRight03(): Bool = Map.foldRight((v, s) -> s + v, 0, Map#{1 -> 2, 3 -> 4}) == 6

@test
def foldRight04(): Bool = Map.foldRight((v, s) -> s + v, 0, Map#{1 -> 2, 3 -> 4, 5 -> 6}) == 12

/////////////////////////////////////////////////////////////////////////////
// foldRightWithKey                                                        //
/////////////////////////////////////////////////////////////////////////////
@test
def foldRightWithKey01(): Bool = Map.foldRightWithKey((k, v, s) -> k + s + v, 0, Map#{}) == 0

@test
def foldRightWithKey02(): Bool = Map.foldRightWithKey((k, v, s) -> k + s + v, 0, Map#{1 -> 2}) == 3

@test
def foldRightWithKey03(): Bool = Map.foldRightWithKey((k, v, s) -> k + s + v, 0, Map#{1 -> 2, 3 -> 4}) == 10

@test
def foldRightWithKey04(): Bool = Map.foldRightWithKey((k, v, s) -> k + s + v, 0, Map#{1 -> 2, 3 -> 4, 5 -> 6}) == 21

/////////////////////////////////////////////////////////////////////////////
// reduce                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def reduce01(): Bool = Map.reduce((v1, v2) -> v1 - v2, Map#{}: Map[Int32, Int32]) == None

@test
def reduce02(): Bool = Map.reduce((v1, v2) -> v1 - v2, Map#{1 -> 2}) == Some(2)

@test
def reduce03(): Bool = Map.reduce((v1, v2) -> v1 - v2, Map#{1 -> 2, 2 -> 3}) == Some(-1)

@test
def reduce04(): Bool = Map.reduce((v1, v2) -> v1 - v2, Map#{1 -> 2, 2 -> 3, 3 -> 4}) == Some(-5)

/////////////////////////////////////////////////////////////////////////////
// reduceWithKey                                                           //
/////////////////////////////////////////////////////////////////////////////
@test
def reduceWithKey01(): Bool = Map.reduceWithKey((k1, v1, k2, v2) -> (k1 - k2, v1 - v2), Map#{}: Map[Int32, Int32]) == None

@test
def reduceWithKey02(): Bool = Map.reduceWithKey((k1, v1, k2, v2) -> (k1 - k2, v1 - v2), Map#{1 -> 2}) == Some((1, 2))

@test
def reduceWithKey03(): Bool = Map.reduceWithKey((k1, v1, k2, v2) -> (k1 - k2, v1 - v2), Map#{1 -> 2, 2 -> 3}) == Some((-1, -1))

@test
def reduceWithKey04(): Bool = Map.reduceWithKey((k1, v1, k2, v2) -> (k1 - k2, v1 - v2), Map#{1 -> 2, 2 -> 3, 3 -> 4}) == Some((-4, -5))

/////////////////////////////////////////////////////////////////////////////
// reduceLeft                                                              //
/////////////////////////////////////////////////////////////////////////////
@test
def reduceLeft01(): Bool = Map.reduceLeft((v1, v2) -> v1 - v2, Map#{}: Map[Int32, Int32]) == None

@test
def reduceLeft02(): Bool = Map.reduceLeft((v1, v2) -> v1 - v2, Map#{1 -> 2}) == Some(2)

@test
def reduceLeft03(): Bool = Map.reduceLeft((v1, v2) -> v1 - v2, Map#{1 -> 2, 2 -> 3}) == Some(-1)

@test
def reduceLeft04(): Bool = Map.reduceLeft((v1, v2) -> v1 - v2, Map#{1 -> 2, 2 -> 3, 3 -> 4}) == Some(-5)

/////////////////////////////////////////////////////////////////////////////
// reduceLeftWithKey                                                       //
/////////////////////////////////////////////////////////////////////////////
@test
def reduceLeftWithKey01(): Bool = Map.reduceLeftWithKey((k1, v1, k2, v2) -> (k1 - k2, v1 - v2), Map#{}: Map[Int32, Int32]) == None

@test
def reduceLeftWithKey02(): Bool = Map.reduceLeftWithKey((k1, v1, k2, v2) -> (k1 - k2, v1 - v2), Map#{1 -> 2}) == Some((1, 2))

@test
def reduceLeftWithKey03(): Bool = Map.reduceLeftWithKey((k1, v1, k2, v2) -> (k1 - k2, v1 - v2), Map#{1 -> 2, 2 -> 3}) == Some((-1, -1))

@test
def reduceLeftWithKey04(): Bool = Map.reduceLeftWithKey((k1, v1, k2, v2) -> (k1 - k2, v1 - v2), Map#{1 -> 2, 2 -> 3, 3 -> 4}) == Some((-4, -5))

/////////////////////////////////////////////////////////////////////////////
// reduceRight                                                             //
/////////////////////////////////////////////////////////////////////////////
@test
def reduceRight01(): Bool = Map.reduceRight((v1, v2) -> v1 - v2, Map#{}: Map[Int32, Int32]) == None

@test
def reduceRight02(): Bool = Map.reduceRight((v1, v2) -> v1 - v2, Map#{1 -> 2}) == Some(2)

@test
def reduceRight03(): Bool = Map.reduceRight((v1, v2) -> v1 - v2, Map#{1 -> 2, 2 -> 3}) == Some(-1)

@test
def reduceRight04(): Bool = Map.reduceRight((v1, v2) -> v1 - v2, Map#{1 -> 2, 2 -> 3, 3 -> 4}) == Some(3)

/////////////////////////////////////////////////////////////////////////////
// reduceRightWithKey                                                      //
/////////////////////////////////////////////////////////////////////////////
@test
def reduceRightWithKey01(): Bool = Map.reduceRightWithKey((k1, v1, k2, v2) -> (k1 - k2, v1 - v2), Map#{}: Map[Int32, Int32]) == None

@test
def reduceRightWithKey02(): Bool = Map.reduceRightWithKey((k1, v1, k2, v2) -> (k1 - k2, v1 - v2), Map#{1 -> 2}) == Some((1, 2))

@test
def reduceRightWithKey03(): Bool = Map.reduceRightWithKey((k1, v1, k2, v2) -> (k1 - k2, v1 - v2), Map#{1 -> 2, 2 -> 3}) == Some((-1, -1))

@test
def reduceRightWithKey04(): Bool = Map.reduceRightWithKey((k1, v1, k2, v2) -> (k1 - k2, v1 - v2), Map#{1 -> 2, 2 -> 3, 3 -> 4}) == Some((2 , 3))

/////////////////////////////////////////////////////////////////////////////
// count                                                                   //
/////////////////////////////////////////////////////////////////////////////
@test
def count01(): Bool = Map.count((k, v) -> k == v, Map#{}) == 0

@test
def count02(): Bool = Map.count((k, v) -> k == v, Map#{1 -> 2}) == 0

@test
def count03(): Bool = Map.count((k, v) -> k == v, Map#{1 -> 1}) == 1

@test
def count04(): Bool = Map.count((k, v) -> k == v, Map#{1 -> 2, 2 -> 3}) == 0

@test
def count05(): Bool = Map.count((k, v) -> k == v, Map#{1 -> 1, 2 -> 3}) == 1

@test
def count06(): Bool = Map.count((k, v) -> k == v, Map#{1 -> 2, 2 -> 2}) == 1

@test
def count07(): Bool = Map.count((k, v) -> k == v, Map#{1 -> 1, 2 -> 2}) == 2

/////////////////////////////////////////////////////////////////////////////
// exists                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def exists01(): Bool = Map.exists((k, v) -> k == v, Map#{}) == false

@test
def exists02(): Bool = Map.exists((k, v) -> k == v, Map#{1 -> 2}) == false

@test
def exists03(): Bool = Map.exists((k, v) -> k == v, Map#{1 -> 1}) == true

@test
def exists04(): Bool = Map.exists((k, v) -> k == v, Map#{1 -> 2, 2 -> 3}) == false

@test
def exists05(): Bool = Map.exists((k, v) -> k == v, Map#{1 -> 1, 2 -> 3}) == true

@test
def exists06(): Bool = Map.exists((k, v) -> k == v, Map#{1 -> 2, 2 -> 2}) == true

@test
def exists07(): Bool = Map.exists((k, v) -> k == v, Map#{1 -> 1, 2 -> 2}) == true

/////////////////////////////////////////////////////////////////////////////
// forall                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def forall01(): Bool = Map.forall((k, v) -> k == v, Map#{}) == true

@test
def forall02(): Bool = Map.forall((k, v) -> k == v, Map#{1 -> 2}) == false

@test
def forall03(): Bool = Map.forall((k, v) -> k == v, Map#{1 -> 1}) == true

@test
def forall04(): Bool = Map.forall((k, v) -> k == v, Map#{1 -> 2, 2 -> 3}) == false

@test
def forall05(): Bool = Map.forall((k, v) -> k == v, Map#{1 -> 1, 2 -> 3}) == false

@test
def forall06(): Bool = Map.forall((k, v) -> k == v, Map#{1 -> 2, 2 -> 2}) == false

@test
def forall07(): Bool = Map.forall((k, v) -> k == v, Map#{1 -> 1, 2 -> 2}) == true

/////////////////////////////////////////////////////////////////////////////
// union                                                                   //
/////////////////////////////////////////////////////////////////////////////
@test
def union01(): Bool = Map.__eq(Map.union(Map#{}, Map#{}), Map#{}) == true

@test
def union02(): Bool = Map.__eq(Map.union(Map#{1 -> 2}, Map#{}), Map#{1 -> 2}) == true

@test
def union03(): Bool = Map.__eq(Map.union(Map#{}, Map#{1 -> 2}), Map#{1 -> 2}) == true

@test
def union04(): Bool = Map.__eq(Map.union(Map#{}, Map#{1 -> 2, 3 -> 4}), Map#{1 -> 2, 3 -> 4}) == true

@test
def union05(): Bool = Map.__eq(Map.union(Map#{1 -> 2, 3 -> 4}, Map#{}), Map#{1 -> 2, 3 -> 4}) == true

@test
def union06(): Bool = Map.__eq(Map.union(Map#{1 -> 2}, Map#{3 -> 4}), Map#{1 -> 2, 3 -> 4}) == true

@test
def union07(): Bool = Map.__eq(Map.union(Map#{1 -> 2}, Map#{1 -> 5}), Map#{1 -> 2}) == true

@test
def union08(): Bool = Map.__eq(Map.union(Map#{}, Map#{1 -> 2, 2 -> 3, 3 -> 4}), Map#{1 -> 2, 2 -> 3, 3 -> 4}) == true

@test
def union09(): Bool = Map.__eq(Map.union(Map#{1 -> 2, 2 -> 3, 3 -> 4}, Map#{}), Map#{1 -> 2, 2 -> 3, 3 -> 4}) == true

@test
def union10(): Bool = Map.__eq(Map.union(Map#{1 -> 2, 2 -> 3}, Map#{3 -> 4}), Map#{1 -> 2, 2 -> 3, 3 -> 4}) == true

@test
def union11(): Bool = Map.__eq(Map.union(Map#{3 -> 4}, Map#{1 -> 2, 2 -> 3}), Map#{1 -> 2, 2 -> 3, 3 -> 4}) == true

@test
def union12(): Bool = Map.__eq(Map.union(Map#{1 -> 2, 2 -> 3}, Map#{2 -> 8}), Map#{1 -> 2, 2 -> 3}) == true

@test
def union13(): Bool = Map.__eq(Map.union(Map#{1 -> 2, 2 -> 3}, Map#{1 -> 8}), Map#{1 -> 2, 2 -> 3}) == true

@test
def union14(): Bool = Map.__eq(Map.union(Map#{1 -> 4}, Map#{1 -> 2, 2 -> 3}), Map#{1 -> 4, 2 -> 3}) == true

@test
def union15(): Bool = Map.__eq(Map.union(Map#{2 -> 7}, Map#{1 -> 2, 2 -> 3}), Map#{1 -> 2, 2 -> 7}) == true

@test
def union16(): Bool = Map.__eq(Map.union(Map#{2 -> 7, 11 -> 14, 9 -> 8, 15 -> 22}, Map#{15 -> 21, 1 -> 2, 2 -> 8, 44 -> 33}),
                                   Map#{2 -> 7, 11 -> 14, 9 -> 8, 15 -> 22, 1 -> 2, 44 -> 33}) == true

/////////////////////////////////////////////////////////////////////////////
// unionWith                                                               //
/////////////////////////////////////////////////////////////////////////////
@test
def unionWith01(): Bool = Map.__eq(Map.unionWith((v1, v2) -> v1 - v2, Map#{}, Map#{}), Map#{}: Map[Int, Int]) == true

@test
def unionWith02(): Bool = Map.__eq(Map.unionWith((v1, v2) -> v1 - v2, Map#{1 -> 2}, Map#{}), Map#{1 -> 2}) == true

@test
def unionWith03(): Bool = Map.__eq(Map.unionWith((v1, v2) -> v1 - v2, Map#{}, Map#{1 -> 2}), Map#{1 -> 2}) == true

@test
def unionWith04(): Bool = Map.__eq(Map.unionWith((v1, v2) -> v1 - v2, Map#{}, Map#{1 -> 2, 3 -> 4}), Map#{1 -> 2, 3 -> 4}) == true

@test
def unionWith05(): Bool = Map.__eq(Map.unionWith((v1, v2) -> v1 - v2, Map#{1 -> 2, 3 -> 4}, Map#{}), Map#{1 -> 2, 3 -> 4}) == true

@test
def unionWith06(): Bool = Map.__eq(Map.unionWith((v1, v2) -> v1 - v2, Map#{1 -> 2}, Map#{3 -> 4}), Map#{1 -> 2, 3 -> 4}) == true

@test
def unionWith07(): Bool = Map.__eq(Map.unionWith((v1, v2) -> v1 - v2, Map#{1 -> 2}, Map#{1 -> 5}), Map#{1 -> -3}) == true

@test
def unionWith08(): Bool = Map.__eq(Map.unionWith((v1, v2) -> v1 - v2, Map#{}, Map#{1 -> 2, 2 -> 3, 3 -> 4}), Map#{1 -> 2, 2 -> 3, 3 -> 4}) == true

@test
def unionWith09(): Bool = Map.__eq(Map.unionWith((v1, v2) -> v1 - v2, Map#{1 -> 2, 2 -> 3, 3 -> 4}, Map#{}), Map#{1 -> 2, 2 -> 3, 3 -> 4}) == true

@test
def unionWith10(): Bool = Map.__eq(Map.unionWith((v1, v2) -> v1 - v2, Map#{1 -> 2, 2 -> 3}, Map#{3 -> 4}), Map#{1 -> 2, 2 -> 3, 3 -> 4}) == true

@test
def unionWith11(): Bool = Map.__eq(Map.unionWith((v1, v2) -> v1 - v2, Map#{3 -> 4}, Map#{1 -> 2, 2 -> 3}), Map#{1 -> 2, 2 -> 3, 3 -> 4}) == true

@test
def unionWith12(): Bool = Map.__eq(Map.unionWith((v1, v2) -> v1 - v2, Map#{1 -> 2, 2 -> 3}, Map#{2 -> 8}), Map#{1 -> 2, 2 -> -5}) == true

@test
def unionWith13(): Bool = Map.__eq(Map.unionWith((v1, v2) -> v1 - v2, Map#{1 -> 2, 2 -> 3}, Map#{1 -> 8}), Map#{1 -> -6, 2 -> 3}) == true

@test
def unionWith14(): Bool = Map.__eq(Map.unionWith((v1, v2) -> v1 - v2, Map#{1 -> 4}, Map#{1 -> 2, 2 -> 3}), Map#{1 -> 2, 2 -> 3}) == true

@test
def unionWith15(): Bool = Map.__eq(Map.unionWith((v1, v2) -> v1 - v2, Map#{2 -> 7}, Map#{1 -> 2, 2 -> 3}), Map#{1 -> 2, 2 -> 4}) == true

@test
def unionWith16(): Bool = Map.__eq(Map.unionWith((v1, v2) -> v1 - v2, Map#{2 -> 7, 11 -> 14, 9 -> 8, 15 -> 22}, Map#{15 -> 21, 1 -> 2, 2 -> 8, 44 -> 33}),
                                   Map#{2 -> -1, 11 -> 14, 9 -> 8, 15 -> 1, 1 -> 2, 44 -> 33}) == true

/////////////////////////////////////////////////////////////////////////////
// unionWithKey                                                            //
/////////////////////////////////////////////////////////////////////////////
@test
def unionWithKey01(): Bool = Map.__eq(Map.unionWithKey((k, v1, v2) -> k + v1 - v2, Map#{}, Map#{}), Map#{}: Map[Int, Int]) == true

@test
def unionWithKey02(): Bool = Map.__eq(Map.unionWithKey((k, v1, v2) -> k + v1 - v2, Map#{1 -> 2}, Map#{}), Map#{1 -> 2}) == true

@test
def unionWithKey03(): Bool = Map.__eq(Map.unionWithKey((k, v1, v2) -> k + v1 - v2, Map#{}, Map#{1 -> 2}), Map#{1 -> 2}) == true

@test
def unionWithKey04(): Bool = Map.__eq(Map.unionWithKey((k, v1, v2) -> k + v1 - v2, Map#{}, Map#{1 -> 2, 3 -> 4}), Map#{1 -> 2, 3 -> 4}) == true

@test
def unionWithKey05(): Bool = Map.__eq(Map.unionWithKey((k, v1, v2) -> k + v1 - v2, Map#{1 -> 2, 3 -> 4}, Map#{}), Map#{1 -> 2, 3 -> 4}) == true

@test
def unionWithKey06(): Bool = Map.__eq(Map.unionWithKey((k, v1, v2) -> k + v1 - v2, Map#{1 -> 2}, Map#{3 -> 4}), Map#{1 -> 2, 3 -> 4}) == true

@test
def unionWithKey07(): Bool = Map.__eq(Map.unionWithKey((k, v1, v2) -> k + v1 - v2, Map#{1 -> 2}, Map#{1 -> 5}), Map#{1 -> -2}) == true

@test
def unionWithKey08(): Bool = Map.__eq(Map.unionWithKey((k, v1, v2) -> k + v1 - v2, Map#{}, Map#{1 -> 2, 2 -> 3, 3 -> 4}), Map#{1 -> 2, 2 -> 3, 3 -> 4}) == true

@test
def unionWithKey09(): Bool = Map.__eq(Map.unionWithKey((k, v1, v2) -> k + v1 - v2, Map#{1 -> 2, 2 -> 3, 3 -> 4}, Map#{}), Map#{1 -> 2, 2 -> 3, 3 -> 4}) == true

@test
def unionWithKey10(): Bool = Map.__eq(Map.unionWithKey((k, v1, v2) -> k + v1 - v2, Map#{1 -> 2, 2 -> 3}, Map#{3 -> 4}), Map#{1 -> 2, 2 -> 3, 3 -> 4}) == true

@test
def unionWithKey11(): Bool = Map.__eq(Map.unionWithKey((k, v1, v2) -> k + v1 - v2, Map#{3 -> 4}, Map#{1 -> 2, 2 -> 3}), Map#{1 -> 2, 2 -> 3, 3 -> 4}) == true

@test
def unionWithKey12(): Bool = Map.__eq(Map.unionWithKey((k, v1, v2) -> k + v1 - v2, Map#{1 -> 2, 2 -> 3}, Map#{2 -> 8}), Map#{1 -> 2, 2 -> -3}) == true

@test
def unionWithKey13(): Bool = Map.__eq(Map.unionWithKey((k, v1, v2) -> k + v1 - v2, Map#{1 -> 2, 2 -> 3}, Map#{1 -> 8}), Map#{1 -> -5, 2 -> 3}) == true

@test
def unionWithKey14(): Bool = Map.__eq(Map.unionWithKey((k, v1, v2) -> k + v1 - v2, Map#{1 -> 4}, Map#{1 -> 2, 2 -> 3}), Map#{1 -> 3, 2 -> 3}) == true

@test
def unionWithKey15(): Bool = Map.__eq(Map.unionWithKey((k, v1, v2) -> k + v1 - v2, Map#{2 -> 7}, Map#{1 -> 2, 2 -> 3}), Map#{1 -> 2, 2 -> 6}) == true

@test
def unionWithKey16(): Bool = Map.__eq(Map.unionWithKey((k, v1, v2) -> k + v1 - v2, Map#{2 -> 7, 11 -> 14, 9 -> 8, 15 -> 22}, Map#{15 -> 21, 1 -> 2, 2 -> 8, 44 -> 33}),
                                   Map#{2 -> 1, 11 -> 14, 9 -> 8, 15 -> 16, 1 -> 2, 44 -> 33})== true

/////////////////////////////////////////////////////////////////////////////
// intersection                                                            //
/////////////////////////////////////////////////////////////////////////////
@test
def intersection01(): Bool = Map.__eq(Map.intersection(Map#{}, Map#{}), Map#{}) == true

@test
def intersection02(): Bool = Map.__eq(Map.intersection(Map#{1 -> 2}, Map#{}), Map#{}) == true

@test
def intersection03(): Bool = Map.__eq(Map.intersection(Map#{}, Map#{1 -> 2}), Map#{}) == true

@test
def intersection04(): Bool = Map.__eq(Map.intersection(Map#{}, Map#{1 -> 2, 3 -> 4}), Map#{}) == true

@test
def intersection05(): Bool = Map.__eq(Map.intersection(Map#{1 -> 2, 3 -> 4}, Map#{}), Map#{}) == true

@test
def intersection06(): Bool = Map.__eq(Map.intersection(Map#{1 -> 2}, Map#{3 -> 4}), Map#{}) == true

@test
def intersection07(): Bool = Map.__eq(Map.intersection(Map#{1 -> 2}, Map#{1 -> 5}), Map#{1 -> 2}) == true

@test
def intersection08(): Bool = Map.__eq(Map.intersection(Map#{}, Map#{1 -> 2, 2 -> 3, 3 -> 4}), Map#{}) == true

@test
def intersection09(): Bool = Map.__eq(Map.intersection(Map#{1 -> 2, 2 -> 3, 3 -> 4}, Map#{}), Map#{}) == true

@test
def intersection10(): Bool = Map.__eq(Map.intersection(Map#{1 -> 2, 2 -> 3}, Map#{3 -> 4}), Map#{}) == true

@test
def intersection11(): Bool = Map.__eq(Map.intersection(Map#{3 -> 4}, Map#{1 -> 2, 2 -> 3}), Map#{}) == true

@test
def intersection12(): Bool = Map.__eq(Map.intersection(Map#{1 -> 2, 2 -> 3}, Map#{2 -> 8}), Map#{2 -> 3}) == true

@test
def intersection13(): Bool = Map.__eq(Map.intersection(Map#{1 -> 2, 2 -> 3}, Map#{1 -> 8}), Map#{1 -> 2}) == true

@test
def intersection14(): Bool = Map.__eq(Map.intersection(Map#{1 -> 4}, Map#{1 -> 2, 2 -> 3}), Map#{1 -> 4}) == true

@test
def intersection15(): Bool = Map.__eq(Map.intersection(Map#{2 -> 7}, Map#{1 -> 2, 2 -> 3}), Map#{2 -> 7}) == true

@test
def intersection16(): Bool = Map.__eq(Map.intersection(Map#{2 -> 7, 11 -> 14, 9 -> 8, 15 -> 22}, Map#{15 -> 21, 1 -> 2, 2 -> 8, 44 -> 33, 11 -> 1}),
                                   Map#{2 -> 7, 15 -> 22, 11 -> 14}) == true

/////////////////////////////////////////////////////////////////////////////
// intersectionWith                                                        //
/////////////////////////////////////////////////////////////////////////////
@test
def intersectionWith01(): Bool = Map.__eq(Map.intersectionWith((v1, v2) -> v1 - v2, Map#{}, Map#{}), Map#{}: Map[Int, Int]) == true

@test
def intersectionWith02(): Bool = Map.__eq(Map.intersectionWith((v1, v2) -> v1 - v2, Map#{1 -> 2}, Map#{}), Map#{}) == true

@test
def intersectionWith03(): Bool = Map.__eq(Map.intersectionWith((v1, v2) -> v1 - v2, Map#{}, Map#{1 -> 2}), Map#{}) == true

@test
def intersectionWith04(): Bool = Map.__eq(Map.intersectionWith((v1, v2) -> v1 - v2, Map#{}, Map#{1 -> 2, 3 -> 4}), Map#{}) == true

@test
def intersectionWith05(): Bool = Map.__eq(Map.intersectionWith((v1, v2) -> v1 - v2, Map#{1 -> 2, 3 -> 4}, Map#{}), Map#{}) == true

@test
def intersectionWith06(): Bool = Map.__eq(Map.intersectionWith((v1, v2) -> v1 - v2, Map#{1 -> 2}, Map#{3 -> 4}), Map#{}) == true

@test
def intersectionWith07(): Bool = Map.__eq(Map.intersectionWith((v1, v2) -> v1 - v2, Map#{1 -> 2}, Map#{1 -> 5}), Map#{1 -> -3}) == true

@test
def intersectionWith08(): Bool = Map.__eq(Map.intersectionWith((v1, v2) -> v1 - v2, Map#{}, Map#{1 -> 2, 2 -> 3, 3 -> 4}), Map#{}) == true

@test
def intersectionWith09(): Bool = Map.__eq(Map.intersectionWith((v1, v2) -> v1 - v2, Map#{1 -> 2, 2 -> 3, 3 -> 4}, Map#{}), Map#{}) == true

@test
def intersectionWith10(): Bool = Map.__eq(Map.intersectionWith((v1, v2) -> v1 - v2, Map#{1 -> 2, 2 -> 3}, Map#{3 -> 4}), Map#{}) == true

@test
def intersectionWith11(): Bool = Map.__eq(Map.intersectionWith((v1, v2) -> v1 - v2, Map#{3 -> 4}, Map#{1 -> 2, 2 -> 3}), Map#{}) == true

@test
def intersectionWith12(): Bool = Map.__eq(Map.intersectionWith((v1, v2) -> v1 - v2, Map#{1 -> 2, 2 -> 3}, Map#{2 -> 8}), Map#{2 -> -5}) == true

@test
def intersectionWith13(): Bool = Map.__eq(Map.intersectionWith((v1, v2) -> v1 - v2, Map#{1 -> 2, 2 -> 3}, Map#{1 -> 8}), Map#{1 -> -6}) == true

@test
def intersectionWith14(): Bool = Map.__eq(Map.intersectionWith((v1, v2) -> v1 - v2, Map#{1 -> 4}, Map#{1 -> 2, 2 -> 3}), Map#{1 -> 2}) == true

@test
def intersectionWith15(): Bool = Map.__eq(Map.intersectionWith((v1, v2) -> v1 - v2, Map#{2 -> 7}, Map#{1 -> 2, 2 -> 3}), Map#{2 -> 4}) == true

@test
def intersectionWith16(): Bool = Map.__eq(Map.intersectionWith((v1, v2) -> v1 - v2, Map#{2 -> 7, 11 -> 14, 9 -> 8, 15 -> 22}, Map#{15 -> 21, 1 -> 2, 2 -> 8, 44 -> 33, 11 -> 11}),
                                   Map#{2 -> -1, 15 -> 1, 11 -> 3}) == true

/////////////////////////////////////////////////////////////////////////////
// intersectionWithKey                                                     //
/////////////////////////////////////////////////////////////////////////////
@test
def intersectionWithKey01(): Bool = Map.__eq(Map.intersectionWithKey((k, v1, v2) -> k + v1 - v2, Map#{}, Map#{}), Map#{}: Map[Int, Int]) == true

@test
def intersectionWithKey02(): Bool = Map.__eq(Map.intersectionWithKey((k, v1, v2) -> k + v1 - v2, Map#{1 -> 2}, Map#{}), Map#{}) == true

@test
def intersectionWithKey03(): Bool = Map.__eq(Map.intersectionWithKey((k, v1, v2) -> k + v1 - v2, Map#{}, Map#{1 -> 2}), Map#{}) == true

@test
def intersectionWithKey04(): Bool = Map.__eq(Map.intersectionWithKey((k, v1, v2) -> k + v1 - v2, Map#{}, Map#{1 -> 2, 3 -> 4}), Map#{}) == true

@test
def intersectionWithKey05(): Bool = Map.__eq(Map.intersectionWithKey((k, v1, v2) -> k + v1 - v2, Map#{1 -> 2, 3 -> 4}, Map#{}), Map#{}) == true

@test
def intersectionWithKey06(): Bool = Map.__eq(Map.intersectionWithKey((k, v1, v2) -> k + v1 - v2, Map#{1 -> 2}, Map#{3 -> 4}), Map#{}) == true

@test
def intersectionWithKey07(): Bool = Map.__eq(Map.intersectionWithKey((k, v1, v2) -> k + v1 - v2, Map#{1 -> 2}, Map#{1 -> 5}), Map#{1 -> -2}) == true

@test
def intersectionWithKey08(): Bool = Map.__eq(Map.intersectionWithKey((k, v1, v2) -> k + v1 - v2, Map#{}, Map#{1 -> 2, 2 -> 3, 3 -> 4}), Map#{}) == true

@test
def intersectionWithKey09(): Bool = Map.__eq(Map.intersectionWithKey((k, v1, v2) -> k + v1 - v2, Map#{1 -> 2, 2 -> 3, 3 -> 4}, Map#{}), Map#{}) == true

@test
def intersectionWithKey10(): Bool = Map.__eq(Map.intersectionWithKey((k, v1, v2) -> k + v1 - v2, Map#{1 -> 2, 2 -> 3}, Map#{3 -> 4}), Map#{}) == true

@test
def intersectionWithKey11(): Bool = Map.__eq(Map.intersectionWithKey((k, v1, v2) -> k + v1 - v2, Map#{3 -> 4}, Map#{1 -> 2, 2 -> 3}), Map#{}) == true

@test
def intersectionWithKey12(): Bool = Map.__eq(Map.intersectionWithKey((k, v1, v2) -> k + v1 - v2, Map#{1 -> 2, 2 -> 3}, Map#{2 -> 8}), Map#{2 -> -3}) == true

@test
def intersectionWithKey13(): Bool = Map.__eq(Map.intersectionWithKey((k, v1, v2) -> k + v1 - v2, Map#{1 -> 2, 2 -> 3}, Map#{1 -> 8}), Map#{1 -> -5}) == true

@test
def intersectionWithKey14(): Bool = Map.__eq(Map.intersectionWithKey((k, v1, v2) -> k + v1 - v2, Map#{1 -> 4}, Map#{1 -> 2, 2 -> 3}), Map#{1 -> 3}) == true

@test
def intersectionWithKey15(): Bool = Map.__eq(Map.intersectionWithKey((k, v1, v2) -> k + v1 - v2, Map#{2 -> 7}, Map#{1 -> 2, 2 -> 3}), Map#{2 -> 6}) == true

@test
def intersectionWithKey16(): Bool = Map.__eq(Map.intersectionWithKey((k, v1, v2) -> k + v1 - v2, Map#{2 -> 7, 11 -> 14, 9 -> 8, 15 -> 22}, Map#{15 -> 21, 1 -> 2, 2 -> 8, 44 -> 33, 11 -> 17}),
                                   Map#{2 -> 1, 15 -> 16, 11 -> 8}) == true

/////////////////////////////////////////////////////////////////////////////
// difference                                                              //
/////////////////////////////////////////////////////////////////////////////
@test
def difference01(): Bool = Map.__eq(Map.difference(Map#{}, Map#{}), Map#{}) == true

@test
def difference02(): Bool = Map.__eq(Map.difference(Map#{1 -> 2}, Map#{}), Map#{1 -> 2}) == true

@test
def difference03(): Bool = Map.__eq(Map.difference(Map#{}, Map#{1 -> 2}), Map#{}) == true

@test
def difference04(): Bool = Map.__eq(Map.difference(Map#{}, Map#{1 -> 2, 3 -> 4}), Map#{}) == true

@test
def difference05(): Bool = Map.__eq(Map.difference(Map#{1 -> 2, 3 -> 4}, Map#{}), Map#{1 -> 2, 3 -> 4}) == true

@test
def difference06(): Bool = Map.__eq(Map.difference(Map#{1 -> 2}, Map#{3 -> 4}), Map#{1 -> 2}) == true

@test
def difference07(): Bool = Map.__eq(Map.difference(Map#{1 -> 2}, Map#{1 -> 5}), Map#{}) == true

@test
def difference08(): Bool = Map.__eq(Map.difference(Map#{}, Map#{1 -> 2, 2 -> 3, 3 -> 4}), Map#{}) == true

@test
def difference09(): Bool = Map.__eq(Map.difference(Map#{1 -> 2, 2 -> 3, 3 -> 4}, Map#{}), Map#{1 -> 2, 2 -> 3, 3 -> 4}) == true

@test
def difference10(): Bool = Map.__eq(Map.difference(Map#{1 -> 2, 2 -> 3}, Map#{3 -> 4}), Map#{1 -> 2, 2 -> 3}) == true

@test
def difference11(): Bool = Map.__eq(Map.difference(Map#{3 -> 4}, Map#{1 -> 2, 2 -> 3}), Map#{3 -> 4}) == true

@test
def difference12(): Bool = Map.__eq(Map.difference(Map#{1 -> 2, 2 -> 3}, Map#{2 -> 8}), Map#{1 -> 2}) == true

@test
def difference13(): Bool = Map.__eq(Map.difference(Map#{1 -> 2, 2 -> 3}, Map#{1 -> 8}), Map#{2 -> 3}) == true

@test
def difference14(): Bool = Map.__eq(Map.difference(Map#{1 -> 4}, Map#{1 -> 2, 2 -> 3}), Map#{}) == true

@test
def difference15(): Bool = Map.__eq(Map.difference(Map#{2 -> 7}, Map#{1 -> 2, 2 -> 3}), Map#{}) == true

@test
def difference16(): Bool = Map.__eq(Map.difference(Map#{2 -> 7, 11 -> 14, 9 -> 8, 15 -> 22}, Map#{15 -> 21, 1 -> 2, 2 -> 8, 44 -> 33}),
                                   Map#{11 -> 14, 9 -> 8}) == true

/////////////////////////////////////////////////////////////////////////////
// differenceWith                                                          //
/////////////////////////////////////////////////////////////////////////////
@test
def differenceWith01(): Bool = Map.__eq(Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, Map#{}, Map#{}), Map#{}: Map[Int, Int]) == true

@test
def differenceWith02(): Bool = Map.__eq(Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, Map#{1 -> 2}, Map#{}), Map#{1 -> 2}) == true

@test
def differenceWith03(): Bool = Map.__eq(Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, Map#{}, Map#{1 -> 2}), Map#{}) == true

@test
def differenceWith04(): Bool = Map.__eq(Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, Map#{}, Map#{1 -> 2, 3 -> 4}), Map#{}) == true

@test
def differenceWith05(): Bool = Map.__eq(Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, Map#{1 -> 2, 3 -> 4}, Map#{}), Map#{1 -> 2, 3 -> 4}) == true

@test
def differenceWith06(): Bool = Map.__eq(Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, Map#{1 -> 2}, Map#{3 -> 4}), Map#{1 -> 2}) == true

@test
def differenceWith07(): Bool = Map.__eq(Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, Map#{1 -> 2}, Map#{1 -> 5}), Map#{}) == true

@test
def differenceWith08(): Bool = Map.__eq(Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, Map#{1 -> 5}, Map#{1 -> 4}), Map#{1 -> 5}) == true

@test
def differenceWith09(): Bool = Map.__eq(Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, Map#{}, Map#{1 -> 2, 2 -> 3, 3 -> 4}), Map#{}) == true

@test
def differenceWith10(): Bool = Map.__eq(Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, Map#{1 -> 2, 2 -> 3, 3 -> 4}, Map#{}), Map#{1 -> 2, 2 -> 3, 3 -> 4}) == true

@test
def differenceWith11(): Bool = Map.__eq(Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, Map#{1 -> 2, 2 -> 3}, Map#{3 -> 4}), Map#{1 -> 2, 2 -> 3}) == true

@test
def differenceWith12(): Bool = Map.__eq(Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, Map#{3 -> 4}, Map#{1 -> 2, 2 -> 3}), Map#{3 -> 4}) == true

@test
def differenceWith13(): Bool = Map.__eq(Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, Map#{1 -> 2, 2 -> 3}, Map#{2 -> 8}), Map#{1 -> 2}) == true

@test
def differenceWith14(): Bool = Map.__eq(Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, Map#{1 -> 2, 2 -> 9}, Map#{2 -> 8}), Map#{1 -> 2, 2 -> 9}) == true

@test
def differenceWith15(): Bool = Map.__eq(Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, Map#{1 -> 2, 2 -> 3}, Map#{1 -> 8}), Map#{2 -> 3}) == true

@test
def differenceWith16(): Bool = Map.__eq(Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, Map#{1 -> 9, 2 -> 3}, Map#{1 -> 8}), Map#{1 -> 9, 2 -> 3}) == true

@test
def differenceWith17(): Bool = Map.__eq(Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, Map#{1 -> 1}, Map#{1 -> 2, 2 -> 3}), Map#{}) == true

@test
def differenceWith18(): Bool = Map.__eq(Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, Map#{1 -> 4}, Map#{1 -> 2, 2 -> 3}), Map#{1 -> 4}) == true

@test
def differenceWith19(): Bool = Map.__eq(Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, Map#{2 -> -4}, Map#{1 -> 2, 2 -> 3}), Map#{}) == true

@test
def differenceWith20(): Bool = Map.__eq(Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, Map#{2 -> 7}, Map#{1 -> 2, 2 -> 3}), Map#{2 -> 7}) == true

@test
def differenceWith21(): Bool = Map.__eq(Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, Map#{2 -> 7, 11 -> 14, 9 -> 8, 15 -> 22}, Map#{15 -> 21, 1 -> 2, 2 -> 8, 44 -> 33}),
                                   Map#{11 -> 14, 9 -> 8, 15 -> 22}) == true

/////////////////////////////////////////////////////////////////////////////
// differenceWithKey                                                       //
/////////////////////////////////////////////////////////////////////////////
@test
def differenceWithKey01(): Bool = Map.__eq(Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, Map#{}, Map#{}), Map#{}: Map[Int, Int]) == true

@test
def differenceWithKey02(): Bool = Map.__eq(Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, Map#{1 -> 2}, Map#{}), Map#{1 -> 2}) == true

@test
def differenceWithKey03(): Bool = Map.__eq(Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, Map#{}, Map#{1 -> 2}), Map#{}) == true

@test
def differenceWithKey04(): Bool = Map.__eq(Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, Map#{}, Map#{1 -> 2, 3 -> 4}), Map#{}) == true

@test
def differenceWithKey05(): Bool = Map.__eq(Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, Map#{1 -> 2, 3 -> 4}, Map#{}), Map#{1 -> 2, 3 -> 4}) == true

@test
def differenceWithKey06(): Bool = Map.__eq(Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, Map#{1 -> 2}, Map#{3 -> 4}), Map#{1 -> 2}) == true

@test
def differenceWithKey07(): Bool = Map.__eq(Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, Map#{1 -> 2}, Map#{1 -> 5}), Map#{}) == true

@test
def differenceWithKey08(): Bool = Map.__eq(Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, Map#{1 -> 5}, Map#{1 -> 4}), Map#{1 -> 6}) == true

@test
def differenceWithKey09(): Bool = Map.__eq(Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, Map#{}, Map#{1 -> 2, 2 -> 3, 3 -> 4}), Map#{}) == true

@test
def differenceWithKey10(): Bool = Map.__eq(Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, Map#{1 -> 2, 2 -> 3, 3 -> 4}, Map#{}), Map#{1 -> 2, 2 -> 3, 3 -> 4}) == true

@test
def differenceWithKey11(): Bool = Map.__eq(Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, Map#{1 -> 2, 2 -> 3}, Map#{3 -> 4}), Map#{1 -> 2, 2 -> 3}) == true

@test
def differenceWithKey12(): Bool = Map.__eq(Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, Map#{3 -> 4}, Map#{1 -> 2, 2 -> 3}), Map#{3 -> 4}) == true

@test
def differenceWithKey13(): Bool = Map.__eq(Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, Map#{1 -> 2, 2 -> 3}, Map#{2 -> 8}), Map#{1 -> 2}) == true

@test
def differenceWithKey14(): Bool = Map.__eq(Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, Map#{1 -> 2, 2 -> 9}, Map#{2 -> 8}), Map#{1 -> 2, 2 -> 11}) == true

@test
def differenceWithKey15(): Bool = Map.__eq(Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, Map#{1 -> 2, 2 -> 3}, Map#{1 -> 8}), Map#{2 -> 3}) == true

@test
def differenceWithKey16(): Bool = Map.__eq(Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, Map#{1 -> 9, 2 -> 3}, Map#{1 -> 8}), Map#{1 -> 10, 2 -> 3}) == true

@test
def differenceWithKey17(): Bool = Map.__eq(Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, Map#{1 -> 1}, Map#{1 -> 2, 2 -> 3}), Map#{}) == true

@test
def differenceWithKey18(): Bool = Map.__eq(Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, Map#{1 -> 4}, Map#{1 -> 2, 2 -> 3}), Map#{1 -> 5}) == true

@test
def differenceWithKey19(): Bool = Map.__eq(Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, Map#{2 -> -4}, Map#{1 -> 2, 2 -> 3}), Map#{}) == true

@test
def differenceWithKey20(): Bool = Map.__eq(Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, Map#{2 -> 7}, Map#{1 -> 2, 2 -> 3}), Map#{2 -> 9}) == true

@test
def differenceWithKey21(): Bool = Map.__eq(Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, Map#{2 -> 7, 11 -> 14, 9 -> 8, 15 -> 22}, Map#{15 -> 21, 1 -> 2, 2 -> 8, 44 -> 33}),
                                   Map#{11 -> 14, 9 -> 8, 15 -> 37}) == true

/////////////////////////////////////////////////////////////////////////////
// toSet                                                                   //
/////////////////////////////////////////////////////////////////////////////
@test
def toSet01(): Bool = Set.__eq(Map.toSet(Map#{}), Set#{}) == true

@test
def toSet02(): Bool = Set.__eq(Map.toSet(Map#{1 -> 2}), Set#{(1, 2)}) == true

@test
def toSet03(): Bool = Set.__eq(Map.toSet(Map#{1 -> 2, 3 -> 4}), Set#{(1, 2), (3, 4)}) == true

@test
def toSet04(): Bool = Set.__eq(Map.toSet(Map#{1 -> 2, 2 -> 2, 3 -> 4}), Set#{(1, 2), (2, 2), (3, 4)}) == true

/////////////////////////////////////////////////////////////////////////////
// toList                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def toList01(): Bool = Map.toList(Map#{}) == Nil

@test
def toList02(): Bool = Map.toList(Map#{1 -> 2}) == (1, 2) :: Nil

@test
def toList03(): Bool = Map.toList(Map#{1 -> 2, 3 -> 4}) == (1, 2) :: (3, 4) :: Nil

@test
def toList04(): Bool = Map.toList(Map#{1 -> 2, 2 -> 2, 3 -> 4}) == (1, 2) :: (2, 2) :: (3, 4) :: Nil

/////////////////////////////////////////////////////////////////////////////
// eq                                                                      //
/////////////////////////////////////////////////////////////////////////////
@test
def eq01(): Bool = Map.__eq(Map#{}, Map#{}) == true

@test
def eq02(): Bool = Map.__eq(Map#{1 -> 2}, Map#{}) == false

@test
def eq03(): Bool = Map.__eq(Map#{}, Map#{1 -> 2}) == false

@test
def eq04(): Bool = Map.__eq(Map#{1 -> 2, 3 -> 4}, Map#{}) == false

@test
def eq05(): Bool = Map.__eq(Map#{}, Map#{1 -> 2, 3 -> 4}) == false

@test
def eq06(): Bool = Map.__eq(Map#{1 -> 2}, Map#{1 -> 2}) == true

@test
def eq07(): Bool = Map.__eq(Map#{1 -> 3}, Map#{1 -> 2}) == false

@test
def eq08(): Bool = Map.__eq(Map#{1 -> 2}, Map#{1 -> 3}) == false

@test
def eq09(): Bool = Map.__eq(Map#{2 -> 2}, Map#{1 -> 2}) == false

@test
def eq10(): Bool = Map.__eq(Map#{1 -> 2}, Map#{4 -> 2}) == false

@test
def eq11(): Bool = Map.__eq(Map#{1 -> 2, 3 -> 4}, Map#{1 -> 2}) == false

@test
def eq12(): Bool = Map.__eq(Map#{1 -> 2}, Map#{1 -> 2, 3 -> 4}) == false

@test
def eq13(): Bool = Map.__eq(Map#{1 -> 2, 3 -> 4}, Map#{1 -> 2, 3 -> 4}) == true

@test
def eq14(): Bool = Map.__eq(Map#{1 -> 2, 3 -> 4}, Map#{3 -> 4, 1 -> 2}) == true

@test
def eq15(): Bool = Map.__eq(Map#{1 -> 1, 3 -> 4}, Map#{1 -> 2, 3 -> 4}) == false

@test
def eq16(): Bool = Map.__eq(Map#{4 -> 2, 3 -> 4}, Map#{1 -> 2, 3 -> 4}) == false

@test
def eq17(): Bool = Map.__eq(Map#{1 -> 2, 3 -> 4}, Map#{1 -> 2, 2 -> 4}) == false

@test
def eq18(): Bool = Map.__eq(Map#{1 -> 2, 3 -> 4}, Map#{1 -> 2, 3 -> 8}) == false

@test
def eq19(): Bool = Map.__eq(Map#{1 -> 2, 3 -> 4, 5 -> 6}, Map#{1 -> 2, 3 -> 4}) == false

@test
def eq20(): Bool = Map.__eq(Map#{1 -> 2, 3 -> 4}, Map#{1 -> 2, 3 -> 4, 5 -> 6}) == false

@test
def eq21(): Bool = Map.__eq(Map#{1 -> 2, 3 -> 4, 5 -> 6}, Map#{1 -> 2, 3 -> 4, 5 -> 6}) == true

@test
def eq22(): Bool = Map.__eq(Map#{1 -> 2, 3 -> 4, 5 -> 6}, Map#{1 -> 2, 5 -> 6, 3 -> 4}) == true

@test
def eq23(): Bool = Map.__eq(Map#{1 -> 2, 3 -> 4, 5 -> 6}, Map#{3 -> 4, 5 -> 6, 1 -> 2}) == true

@test
def eq24(): Bool = Map.__eq(Map#{1 -> 2, 3 -> 4, 5 -> 6}, Map#{5 -> 6, 3 -> 4, 1 -> 2}) == true

@test
def eq25(): Bool = Map.__eq(Map#{1 -> 2, 3 -> 4, 5 -> 6, 7 -> 8, 9 -> 10}, Map#{7 -> 8, 1 -> 2, 3 -> 4, 9 -> 10, 5 -> 6}) == true

/////////////////////////////////////////////////////////////////////////////
// foreach                                                                 //
/////////////////////////////////////////////////////////////////////////////

@test
def foreach01(): Bool & Impure =
    let r = ref 21;
    Map.foreach((k, _) -> r := k, Map#{});
    21 == deref r

@test
def foreach02(): Bool & Impure =
    let r = ref 21;
    Map.foreach((k, _) -> r := k, Map#{1 -> "Hello World!"});
    1 == deref r

/////////////////////////////////////////////////////////////////////////////
// unfold                                                                  //
/////////////////////////////////////////////////////////////////////////////

@test
def unfold01(): Bool =
    Map.unfold(s -> if (true) None else Some(s, Char.fromInt32(s + 48), s + 1), 0) == Map#{}

@test
def unfold02(): Bool =
    Map.unfold(s -> if (s > 0) None else Some(s, Char.fromInt32(s + 48), s + 1), 0) == Map#{0 -> '0'}

@test
def unfold03(): Bool =
    Map.unfold(s -> if (s > 1) None else Some(s, Char.fromInt32(s + 48), s + 1), 0) == Map#{0 -> '0', 1 -> '1'}

@test
def unfold04(): Bool =
    Map.unfold(s -> if (s >= 10) None else Some(s, Char.fromInt32(s + 48), s + 1), 0) == Map#{0 -> '0', 1 -> '1', 2 -> '2', 3 -> '3', 4 -> '4', 5 -> '5', 6 -> '6', 7 -> '7', 8 -> '8', 9 -> '9'}

@test
def unfold05(): Bool =
    Map.unfold(s -> if (s >= 10) None else Some(s, Char.fromInt32(s + 48), s + 1), 5) == Map#{5 -> '5', 6 -> '6', 7 -> '7', 8 -> '8', 9 -> '9'}

@test
def unfold06(): Bool =
    Map.unfold(s -> if (s >= 10) None else Some(s, Char.fromInt32(s + 48), s + 2), 0) == Map#{0 -> '0', 2 -> '2', 4 -> '4', 6 -> '6', 8 -> '8'}

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
            let k = deref x;
            let v = Char.fromInt32(k + 48);
            x := deref x + 1;
            Some(k, v)
        };
    Map.unfoldWithIter(step) == Map#{}

@test
def unfoldWithIter02(): Bool & Impure =
    let x = ref 0;
    let step = () ->
        if (deref x > 0)
            None
        else {
            let k = deref x;
            let v = Char.fromInt32(k + 48);
            x := deref x + 1;
            Some(k, v)
        };
    Map.unfoldWithIter(step) == Map#{0 -> '0'}

@test
def unfoldWithIter03(): Bool & Impure =
    let x = ref 0;
    let step = () ->
        if (deref x > 1)
            None
        else {
            let k = deref x;
            let v = Char.fromInt32(k + 48);
            x := deref x + 1;
            Some(k, v)
        };
    Map.unfoldWithIter(step) == Map#{0 -> '0', 1 -> '1'}

@test
def unfoldWithIter04(): Bool & Impure =
    let x = ref 0;
    let step = () ->
        if (deref x >= 10)
            None
        else {
            let k = deref x;
            let v = Char.fromInt32(k + 48);
            x := deref x + 1;
            Some(k, v)
        };
    Map.unfoldWithIter(step) == Map#{0 -> '0', 1 -> '1', 2 -> '2', 3 -> '3', 4 -> '4', 5 -> '5', 6 -> '6', 7 -> '7', 8 -> '8', 9 -> '9'}

@test
def unfoldWithIter05(): Bool & Impure =
    let x = ref 5;
    let step = () ->
        if (deref x >= 10)
            None
        else {
            let k = deref x;
            let v = Char.fromInt32(k + 48);
            x := deref x + 1;
            Some(k, v)
        };
    Map.unfoldWithIter(step) == Map#{5 -> '5', 6 -> '6', 7 -> '7', 8 -> '8', 9 -> '9'}

@test
def unfoldWithIter06(): Bool & Impure =
    let x = ref 0;
    let step = () ->
        if (deref x >= 10)
            None
        else {
            let k = deref x;
            let v = Char.fromInt32(k + 48);
            x := deref x + 2;
            Some(k, v)
        };
    Map.unfoldWithIter(step) == Map#{0 -> '0', 2 -> '2', 4 -> '4', 6 -> '6', 8 -> '8'}

/////////////////////////////////////////////////////////////////////////////
// query                                                                   //
/////////////////////////////////////////////////////////////////////////////

@test
def query01(): Bool =
    Map.query(_ -> Equal, Map#{}) == Nil

@test
def query02(): Bool =
    Map.query(_ -> Equal, Map#{1 -> 101, 2 -> 42}) == (1, 101) :: (2, 42) :: Nil

@test
def query03(): Bool =
    Map.query(x -> if (x < 2) LessThan else if (x > 2) GreaterThan else Equal, Map#{1 -> 1, 2 -> 2, 3 -> 3}) == (2, 2) :: Nil

@test
def query04(): Bool =
    Map.query(x -> if (x < 'b') LessThan else if (x > 'c') GreaterThan else Equal, Map#{'a' -> 0, 'b' -> 1, 'c' -> 2, 'd' -> 3, 'e' -> 4}) == ('b', 1) :: ('c', 2) :: Nil

@test
def query05(): Bool =
    let m = Map.unfold(s -> if (s > 10) None else Some(s, s, s + 1), 5);
    Map.query(x -> if (x < 5) LessThan else if (x >= 10) GreaterThan else Equal, m) == List.map(x -> (x, x), List.range(5, 10))

@test
def query06(): Bool =
    let m = Map.unfold(s -> if (s > 75) None else Some(s, Char.fromInt32(s + 48), s + 1), 25);
    Map.query(x -> if (x < 25) LessThan else if (x >= 75) GreaterThan else Equal, m) == List.map(x -> (x, Char.fromInt32(x + 48)), List.range(25, 75))

@test
def query07(): Bool =
    Map.query(x -> if (x < 42) LessThan else if (x > 42) GreaterThan else Equal, Map#{42 -> true}) == (42, true) :: Nil

@test
def query08(): Bool =
    Map.query(x -> if (x < 1) LessThan else if (x > 1) GreaterThan else Equal, Map#{1 -> 2, 2 -> 3, 3 -> 4, 4 -> 5}) == (1, 2) :: Nil

@test
def query09(): Bool =
    Map.query(x -> if (x < 2) LessThan else if (x > 3) GreaterThan else Equal, Map#{1 -> 2, 2 -> 3, 3 -> 4, 4 -> 5}) == (2, 3) :: (3, 4) :: Nil

@test
def query10(): Bool =
    Map.query(x -> if (x < 4) LessThan else if (x > 4) GreaterThan else Equal, Map#{1 -> 2, 2 -> 3, 3 -> 4, 4 -> 5}) == (4, 5) :: Nil

@test
def query11(): Bool =
    let m = Map#{(1, 2) -> 0, (1, 3) -> 1, (2, 1) -> 2, (2, 2) -> 3, (2, 3) -> 4, (3, 1) -> 5};
    Map.query(x -> if (fst(x) < 1) LessThan else if (fst(x) > 1) GreaterThan else Equal, m) == ((1, 2), 0) :: ((1, 3), 1) :: Nil

@test
def query12(): Bool =
    let m = Map#{(1, 2) -> 0, (1, 3) -> 1, (2, 1) -> 2, (2, 2) -> 3, (2, 3) -> 4, (3, 1) -> 5};
    Map.query(x -> if (fst(x) < 3) LessThan else if (fst(x) > 3) GreaterThan else Equal, m) == ((3, 1), 5) :: Nil

@test
def query13(): Bool =
    let m = Map#{(1, 2) -> 0, (1, 3) -> 1, (2, 1) -> 2, (2, 2) -> 3, (2, 3) -> 4, (3, 1) -> 5};
    Map.query(x -> if (fst(x) > 2) GreaterThan else Equal, m) == ((1, 2), 0) :: ((1, 3), 1) :: ((2, 1), 2) :: ((2, 2), 3) :: ((2, 3), 4) :: Nil

@test
def query14(): Bool =
    let m = Map#{(1, 2) -> 0, (1, 3) -> 1, (2, 1) -> 2, (2, 2) -> 3, (2, 3) -> 4, (3, 1) -> 5};
    Map.query(x -> if (fst(x) < 2) LessThan else Equal, m) == ((2, 1), 2) :: ((2, 2), 3) :: ((2, 3), 4) :: ((3, 1), 5) :: Nil

@test
def query15(): Bool =
    let m = Map#{(1, 2) -> 0, (1, 3) -> 1, (2, 1) -> 2, (2, 2) -> 3, (2, 3) -> 4, (3, 1) -> 5};
    Map.query(x -> if (x <=> (2, 2) > 0) GreaterThan else Equal, m) == ((1, 2), 0) :: ((1, 3), 1) :: ((2, 1), 2) :: ((2, 2), 3) :: Nil

@test
def query16(): Bool =
    let m = Map#{(1, 2) -> 0, (1, 3) -> 1, (2, 1) -> 2, (2, 2) -> 3, (2, 3) -> 4, (3, 1) -> 5};
    Map.query(x -> if (x <=> (2, 2) < 0) LessThan else Equal, m) == ((2, 2), 3) :: ((2, 3), 4) :: ((3, 1), 5) :: Nil

@test
def query17(): Bool =
    let m = Map#{(1, 2, 2) -> 0, (1, 2, 3) -> 1, (1, 2, 4) -> 2, (2, 2, 4) -> 3};
    Map.query(x -> if (x <=> (1, 2, 3) < 0) LessThan else Equal, m) == ((1, 2, 3), 1) :: ((1, 2, 4), 2) :: ((2, 2, 4), 3) :: Nil

@test
def query18(): Bool =
    let m = Map#{(1, 2, 2) -> 0, (1, 2, 3) -> 1, (1, 2, 4) -> 2, (2, 2, 4) -> 3};
    Map.query(x -> if (x <=> (1, 2, 3) > 0) GreaterThan else Equal, m) == ((1, 2, 2), 0) :: ((1, 2, 3), 1) :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // toString                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toString01(): Bool =
        toString(Map#{1 -> 2}) == "Map#{1 -> 2}"

    @test
    def toString02(): Bool =
        toString(Map#{1 -> 0, 2 -> 1, 3 -> 2}) == "Map#{1 -> 0, 2 -> 1, 3 -> 2}"

    @test
    def toString03(): Bool =
        toString(Map#{1 -> "b", 2 -> "a"}) == "Map#{1 -> b, 2 -> a}"

    @test
    def toString04(): Bool =
        toString(Map#{97 -> false, 2 -> true, 3 -> false, 4 -> true, 0 -> true}) == "Map#{0 -> true, 2 -> true, 3 -> false, 4 -> true, 97 -> false}"

    @test
    def toString05(): Bool =
        toString(Map#{2 -> Map#{1 -> 0, 2 -> 1}, 3 -> Map#{3 -> 2, 4 -> 92}}) == "Map#{2 -> Map#{1 -> 0, 2 -> 1}, 3 -> Map#{3 -> 2, 4 -> 92}}"

}
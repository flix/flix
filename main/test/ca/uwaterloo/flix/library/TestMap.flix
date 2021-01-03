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

    use ToString.toString;
    use Hash.hash;

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
def empty01(): Bool = Map.empty(): Map[Unit, Unit] == Map#{}

/////////////////////////////////////////////////////////////////////////////
// singleton                                                               //
/////////////////////////////////////////////////////////////////////////////
@test
def singleton01(): Bool = Map.singleton(1, 2) == Map#{1 -> 2}

@test
def singleton02(): Bool = Map.singleton(3, -1) == Map#{3 -> -1}

@test
def singleton03(): Bool = Map.singleton(-99, -11) == Map#{-99 -> -11}

/////////////////////////////////////////////////////////////////////////////
// isEmpty                                                                 //
/////////////////////////////////////////////////////////////////////////////
@test
def isEmpty01(): Bool = Map.isEmpty(Map#{}: Map[Unit, Unit])

@test
def isEmpty02(): Bool = not Map.isEmpty(Map#{1 -> 2})

@test
def isEmpty03(): Bool = not Map.isEmpty(Map#{1 -> 2, 2 -> 4})

@test
def isEmpty04(): Bool = not Map.isEmpty(Map#{1 -> 2, 2 -> 4, 3 -> 6})

@test
def isEmpty05(): Bool = not Map.isEmpty(Map#{1 -> 2, 2 -> 4, 3 -> 6, 4 -> 8})

/////////////////////////////////////////////////////////////////////////////
// get                                                                     //
/////////////////////////////////////////////////////////////////////////////
@test
def get01(): Bool = Map.get(2, Map#{}: Map[_, Unit]) == None

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
def memberOf01(): Bool = not Map.memberOf(2, Map#{}: Map[_, Unit])

@test
def memberOf02(): Bool = not Map.memberOf(2, Map#{1 -> 2})

@test
def memberOf03(): Bool = Map.memberOf(2, Map#{2 -> 1})

@test
def memberOf04(): Bool = not Map.memberOf(5, Map#{2 -> 1, 3 -> 17})

@test
def memberOf05(): Bool = Map.memberOf(5, Map#{2 -> 1, 5 -> 17})

@test
def memberOf06(): Bool = Map.memberOf(5, Map#{5 -> 1, 3 -> 17})

@test
def memberOf07(): Bool = not Map.memberOf(-2, Map#{2 -> 1, 3 -> 17, -1 -> -2})

@test
def memberOf08(): Bool = Map.memberOf(-2, Map#{-2 -> 1, 3 -> 17, -1 -> -2})

@test
def memberOf09(): Bool = Map.memberOf(-2, Map#{2 -> 1, -2 -> 17, -1 -> -2})

@test
def memberOf10(): Bool = Map.memberOf(-2, Map#{2 -> 1, 3 -> 17, -2 -> -2})

/////////////////////////////////////////////////////////////////////////////
// keysOf                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def keysOf01(): Bool = Map.keysOf(Map#{}: Map[Unit, Unit]) == Set#{}

@test
def keysOf02(): Bool = Map.keysOf(Map#{1 -> 2}) == Set#{1}

@test
def keysOf03(): Bool = Map.keysOf(Map#{1 -> 2, 2 -> 4}) == Set#{1, 2}

@test
def keysOf04(): Bool = Map.keysOf(Map#{1 -> 2, 2 -> 4, 3 -> 6}) == Set#{1, 2, 3}

@test
def keysOf05(): Bool = Map.keysOf(Map#{1 -> 2, 2 -> 4, 3 -> 6, 4 -> 8}) == Set#{1, 2, 3, 4}

@test
def keysOf06(): Bool = Map.keysOf(Map#{1 -> 2, 2 -> 4, 3 -> 6, 4 -> 8, 5 -> 10}) == Set#{1, 2, 3, 4, 5}

/////////////////////////////////////////////////////////////////////////////
// valuesOf                                                                //
/////////////////////////////////////////////////////////////////////////////
@test
def valuesOf01(): Bool = Map.valuesOf(Map#{}: Map[Unit, Unit]) == Nil

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
def insert01(): Bool = Map.insert(1, 3, Map#{}) == Map#{1 -> 3}

@test
def insert02(): Bool = Map.insert(1, 3, Map#{1 -> 4}) == Map#{1 -> 3}

@test
def insert03(): Bool = Map.insert(2, 3, Map#{1 -> 4}) == Map#{1 -> 4, 2 -> 3}

@test
def insert04(): Bool = Map.insert(1, 1, Map#{1 -> 4, 5 -> -2}) == Map#{1 -> 1, 5 -> -2}

@test
def insert05(): Bool = Map.insert(5, 1, Map#{1 -> 4, 5 -> -2}) == Map#{1 -> 4, 5 -> 1}

@test
def insert06(): Bool = Map.insert(4, -2, Map#{1 -> 4, 5 -> -2}) == Map#{1 -> 4, 5 -> -2, 4 -> -2}

/////////////////////////////////////////////////////////////////////////////
// insertWith                                                              //
/////////////////////////////////////////////////////////////////////////////
@test
def insertWith01(): Bool = Map.insertWith((v1, v2) -> v1 + v2, 1, 3, Map#{}) == Map#{1 -> 3}

@test
def insertWith02(): Bool = Map.insertWith((v1, v2) -> v1 + v2, 1, 3, Map#{1 -> 4}) == Map#{1 -> 7}

@test
def insertWith03(): Bool = Map.insertWith((v1, v2) -> v1 + v2, 2, 3, Map#{1 -> 4}) == Map#{1 -> 4, 2 -> 3}

@test
def insertWith04(): Bool = Map.insertWith((v1, v2) -> v1 + v2, 1, 1, Map#{1 -> 4, 5 -> -2}) == Map#{1 -> 5, 5 -> -2}

@test
def insertWith05(): Bool = Map.insertWith((v1, v2) -> v1 + v2, 5, 1, Map#{1 -> 4, 5 -> -2}) == Map#{1 -> 4, 5 -> -1}

@test
def insertWith06(): Bool = Map.insertWith((v1, v2) -> v1 + v2, 4, -2, Map#{1 -> 4, 5 -> -2}) == Map#{1 -> 4, 5 -> -2, 4 -> -2}

/////////////////////////////////////////////////////////////////////////////
// insertWithKey                                                           //
/////////////////////////////////////////////////////////////////////////////
@test
def insertWithKey01(): Bool = Map.insertWithKey((k, v1, v2) -> k + v1 + v2, 1, 3, Map#{}) == Map#{1 -> 3}

@test
def insertWithKey02(): Bool = Map.insertWithKey((k, v1, v2) -> k + v1 + v2, 1, 3, Map#{1 -> 4}) == Map#{1 -> 8}

@test
def insertWithKey03(): Bool = Map.insertWithKey((k, v1, v2) -> k + v1 + v2, 2, 3, Map#{1 -> 4}) == Map#{1 -> 4, 2 -> 3}

@test
def insertWithKey04(): Bool = Map.insertWithKey((k, v1, v2) -> k + v1 + v2, 1, 1, Map#{1 -> 4, 5 -> -2}) == Map#{1 -> 6, 5 -> -2}

@test
def insertWithKey05(): Bool = Map.insertWithKey((k, v1, v2) -> k + v1 + v2, 5, 1, Map#{1 -> 4, 5 -> -2}) == Map#{1 -> 4, 5 -> 4}

@test
def insertWithKey06(): Bool = Map.insertWithKey((k, v1, v2) -> k + v1 + v2, 4, -2, Map#{1 -> 4, 5 -> -2}) == Map#{1 -> 4, 5 -> -2, 4 -> -2}

/////////////////////////////////////////////////////////////////////////////
// adjust                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def adjust01(): Bool = Map.adjust(v -> 2*v, 1, Map#{}) == Map#{}

@test
def adjust02(): Bool = Map.adjust(v -> 2*v, 1, Map#{1 -> 4}) == Map#{1 -> 8}

@test
def adjust03(): Bool = Map.adjust(v -> 2*v, 2, Map#{1 -> 4}) == Map#{1 -> 4}

@test
def adjust04(): Bool = Map.adjust(v -> 2*v, 1, Map#{1 -> -14, 5 -> -2}) == Map#{1 -> -28, 5 -> -2}

@test
def adjust05(): Bool = Map.adjust(v -> 2*v, 5, Map#{1 -> 4, 5 -> -2}) == Map#{1 -> 4, 5 -> -4}

@test
def adjust06(): Bool = Map.adjust(v -> 2*v, 4, Map#{1 -> 4, 5 -> -2}) == Map#{1 -> 4, 5 -> -2}

/////////////////////////////////////////////////////////////////////////////
// adjustWithKey                                                           //
/////////////////////////////////////////////////////////////////////////////
@test
def adjustWithKey01(): Bool = Map.adjustWithKey((k, v) -> k+v, 1, Map#{}) == Map#{}

@test
def adjustWithKey02(): Bool = Map.adjustWithKey((k, v) -> k+v, 1, Map#{1 -> 4}) == Map#{1 -> 5}

@test
def adjustWithKey03(): Bool = Map.adjustWithKey((k, v) -> k+v, 2, Map#{1 -> 4}) == Map#{1 -> 4}

@test
def adjustWithKey04(): Bool = Map.adjustWithKey((k, v) -> k+v, 1, Map#{1 -> -14, 5 -> -2}) == Map#{1 -> -13, 5 -> -2}

@test
def adjustWithKey05(): Bool = Map.adjustWithKey((k, v) -> k+v, 5, Map#{1 -> 4, 5 -> -2}) == Map#{1 -> 4, 5 -> 3}

@test
def adjustWithKey06(): Bool = Map.adjustWithKey((k, v) -> k+v, 4, Map#{1 -> 4, 5 -> -2}) == Map#{1 -> 4, 5 -> -2}

/////////////////////////////////////////////////////////////////////////////
// update                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def update01(): Bool = Map.update(v -> if (v % 2 != 0) Some(2*v) else None, 1, Map#{}) == Map#{}

@test
def update02(): Bool = Map.update(v -> if (v % 2 != 0) Some(2*v) else None, 1, Map#{1 -> 3}) == Map#{1 -> 6}

@test
def update03(): Bool = Map.update(v -> if (v % 2 != 0) Some(2*v) else None, 1, Map#{1 -> 4}) == Map#{1 -> 4}

@test
def update04(): Bool = Map.update(v -> if (v % 2 != 0) Some(2*v) else None, 2, Map#{1 -> 4}) == Map#{1 -> 4}

@test
def update05(): Bool = Map.update(v -> if (v % 2 != 0) Some(2*v) else None, 1, Map#{1 -> -14, 5 -> -2}) == Map#{1 -> -14, 5 -> -2}

@test
def update06(): Bool = Map.update(v -> if (v % 2 != 0) Some(2*v) else None, 1, Map#{1 -> -13, 5 -> -2}) == Map#{1 -> -26, 5 -> -2}

@test
def update07(): Bool = Map.update(v -> if (v % 2 != 0) Some(2*v) else None, 5, Map#{1 -> 4, 5 -> -2}) == Map#{1 -> 4, 5 -> -2}

@test
def update08(): Bool = Map.update(v -> if (v % 2 != 0) Some(2*v) else None, 5, Map#{1 -> 4, 5 -> -1}) == Map#{1 -> 4, 5 -> -2}

@test
def update09(): Bool = Map.update(v -> if (v % 2 != 0) Some(2*v) else None, 4, Map#{1 -> 4, 5 -> -2}) == Map#{1 -> 4, 5 -> -2}

/////////////////////////////////////////////////////////////////////////////
// updateWithKey                                                           //
/////////////////////////////////////////////////////////////////////////////
@test
def updateWithKey01(): Bool = Map.updateWithKey((k, v) -> if (v % 2 != 0) Some(k+2*v) else None, 1, Map#{}) == Map#{}

@test
def updateWithKey02(): Bool = Map.updateWithKey((k, v) -> if (v % 2 != 0) Some(k+2*v) else None, 1, Map#{1 -> 3}) == Map#{1 -> 7}

@test
def updateWithKey03(): Bool = Map.updateWithKey((k, v) -> if (v % 2 != 0) Some(k+2*v) else None, 1, Map#{1 -> 4}) == Map#{1 -> 4}

@test
def updateWithKey04(): Bool = Map.updateWithKey((k, v) -> if (v % 2 != 0) Some(k+2*v) else None, 2, Map#{1 -> 4}) == Map#{1 -> 4}

@test
def updateWithKey05(): Bool = Map.updateWithKey((k, v) -> if (v % 2 != 0) Some(k+2*v) else None, 1, Map#{1 -> -14, 5 -> -2}) == Map#{1 -> -14, 5 -> -2}

@test
def updateWithKey06(): Bool = Map.updateWithKey((k, v) -> if (v % 2 != 0) Some(k+2*v) else None, 1, Map#{1 -> -13, 5 -> -2}) == Map#{1 -> -25, 5 -> -2}

@test
def updateWithKey07(): Bool = Map.updateWithKey((k, v) -> if (v % 2 != 0) Some(k+2*v) else None, 5, Map#{1 -> 4, 5 -> -2}) == Map#{1 -> 4, 5 -> -2}

@test
def updateWithKey08(): Bool = Map.updateWithKey((k, v) -> if (v % 2 != 0) Some(k+2*v) else None, 5, Map#{1 -> 4, 5 -> -1}) == Map#{1 -> 4, 5 -> 3}

@test
def updateWithKey09(): Bool = Map.updateWithKey((k, v) -> if (v % 2 != 0) Some(k+2*v) else None, 4, Map#{1 -> 4, 5 -> -2}) == Map#{1 -> 4, 5 -> -2}

/////////////////////////////////////////////////////////////////////////////
// delete                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def delete01(): Bool = Map.delete(1, Map#{}: Map[_, Unit]) == Map#{}

@test
def delete02(): Bool = Map.delete(1, Map#{2 -> 4}) == Map#{2 -> 4}

@test
def delete03(): Bool = Map.delete(1, Map#{1 -> 4}) == Map#{}

@test
def delete04(): Bool = Map.delete(1, Map#{6 -> 1, 2 -> 4}) == Map#{6 -> 1, 2 -> 4}

@test
def delete05(): Bool = Map.delete(6, Map#{6 -> 1, 2 -> 4}) == Map#{2 -> 4}

@test
def delete06(): Bool = Map.delete(2, Map#{6 -> 1, 2 -> 4}) == Map#{6 -> 1}

@test
def delete07(): Bool = Map.delete(1, Map#{6 -> 1, 2 -> 4, 8 -> 89}) == Map#{6 -> 1, 2 -> 4, 8 -> 89}

@test
def delete08(): Bool = Map.delete(6, Map#{6 -> 1, 2 -> 4, 8 -> 89}) == Map#{2 -> 4, 8 -> 89}

@test
def delete09(): Bool = Map.delete(2, Map#{6 -> 1, 2 -> 4, 8 -> 89}) == Map#{6 -> 1, 8 -> 89}

@test
def delete10(): Bool = Map.delete(8, Map#{6 -> 1, 2 -> 4, 8 -> 89}) == Map#{6 -> 1, 2 -> 4}

/////////////////////////////////////////////////////////////////////////////
// isSubmapOf                                                              //
/////////////////////////////////////////////////////////////////////////////
@test
def isSubmapOf01(): Bool = Map.isSubmapOf(Map#{}: Map[Unit, Unit], Map#{})

@test
def isSubmapOf02(): Bool = not Map.isSubmapOf(Map#{1 -> 2}, Map#{})

@test
def isSubmapOf03(): Bool = not Map.isSubmapOf(Map#{3 -> 4}, Map#{})

@test
def isSubmapOf04(): Bool = Map.isSubmapOf(Map#{1 -> 3}, Map#{1 -> 3})

@test
def isSubmapOf05(): Bool = not Map.isSubmapOf(Map#{2 -> 3}, Map#{1 -> 3})

@test
def isSubmapOf06(): Bool = not Map.isSubmapOf(Map#{1 -> 4}, Map#{1 -> 3})

@test
def isSubmapOf07(): Bool = not Map.isSubmapOf(Map#{1 -> 3, 2 -> 3}, Map#{1 -> 3})

@test
def isSubmapOf08(): Bool = not Map.isSubmapOf(Map#{1 -> 3, 2 -> 3}, Map#{2 -> 3})

@test
def isSubmapOf09(): Bool = Map.isSubmapOf(Map#{}, Map#{2 -> 4, 1 -> 3})

@test
def isSubmapOf10(): Bool = Map.isSubmapOf(Map#{2 -> 4}, Map#{2 -> 4, 1 -> 3})

@test
def isSubmapOf11(): Bool = Map.isSubmapOf(Map#{1 -> 3}, Map#{2 -> 4, 1 -> 3})

@test
def isSubmapOf12(): Bool = Map.isSubmapOf(Map#{1 -> 3, 2 -> 4}, Map#{1 -> 3, 2 -> 4})

@test
def isSubmapOf13(): Bool = Map.isSubmapOf(Map#{2 -> 4, 1 -> 3}, Map#{1 -> 3, 2 -> 4})

@test
def isSubmapOf14(): Bool = not Map.isSubmapOf(Map#{2 -> 5, 1 -> 3}, Map#{1 -> 3, 2 -> 4})

@test
def isSubmapOf15(): Bool = not Map.isSubmapOf(Map#{2 -> 4, 1 -> -1}, Map#{1 -> 3, 2 -> 4})

@test
def isSubmapOf16(): Bool = not Map.isSubmapOf(Map#{3 -> 4, 1 -> 3}, Map#{1 -> 3, 2 -> 4})

@test
def isSubmapOf17(): Bool = not Map.isSubmapOf(Map#{2 -> 4, 3 -> 3}, Map#{1 -> 3, 2 -> 4})

@test
def isSubmapOf18(): Bool = not Map.isSubmapOf(Map#{2 -> 4, 1 -> 3, 8 -> 9}, Map#{1 -> 3, 2 -> 4})

/////////////////////////////////////////////////////////////////////////////
// isProperSubmapOf                                                        //
/////////////////////////////////////////////////////////////////////////////
@test
def isProperSubmapOf01(): Bool = not Map.isProperSubmapOf(Map#{}: Map[Unit, Unit], Map#{})

@test
def isProperSubmapOf02(): Bool = not Map.isProperSubmapOf(Map#{1 -> 2}, Map#{})

@test
def isProperSubmapOf03(): Bool = not Map.isProperSubmapOf(Map#{3 -> 4}, Map#{})

@test
def isProperSubmapOf04(): Bool = not Map.isProperSubmapOf(Map#{1 -> 3}, Map#{1 -> 3})

@test
def isProperSubmapOf05(): Bool = not Map.isProperSubmapOf(Map#{2 -> 3}, Map#{1 -> 3})

@test
def isProperSubmapOf06(): Bool = not Map.isProperSubmapOf(Map#{1 -> 4}, Map#{1 -> 3})

@test
def isProperSubmapOf07(): Bool = not Map.isProperSubmapOf(Map#{1 -> 3, 2 -> 3}, Map#{1 -> 3})

@test
def isProperSubmapOf08(): Bool = not Map.isProperSubmapOf(Map#{1 -> 3, 2 -> 3}, Map#{2 -> 3})

@test
def isProperSubmapOf09(): Bool = Map.isProperSubmapOf(Map#{}, Map#{2 -> 4, 1 -> 3})

@test
def isProperSubmapOf10(): Bool = Map.isProperSubmapOf(Map#{2 -> 4}, Map#{2 -> 4, 1 -> 3})

@test
def isProperSubmapOf11(): Bool = Map.isProperSubmapOf(Map#{1 -> 3}, Map#{2 -> 4, 1 -> 3})

@test
def isProperSubmapOf12(): Bool = not Map.isProperSubmapOf(Map#{1 -> 3, 2 -> 4}, Map#{1 -> 3, 2 -> 4})

@test
def isProperSubmapOf13(): Bool = not Map.isProperSubmapOf(Map#{2 -> 4, 1 -> 3}, Map#{1 -> 3, 2 -> 4})

@test
def isProperSubmapOf14(): Bool = not Map.isProperSubmapOf(Map#{2 -> 5, 1 -> 3}, Map#{1 -> 3, 2 -> 4})

@test
def isProperSubmapOf15(): Bool = not Map.isProperSubmapOf(Map#{2 -> 4, 1 -> -1}, Map#{1 -> 3, 2 -> 4})

@test
def isProperSubmapOf16(): Bool = not Map.isProperSubmapOf(Map#{3 -> 4, 1 -> 3}, Map#{1 -> 3, 2 -> 4})

@test
def isProperSubmapOf17(): Bool = not Map.isProperSubmapOf(Map#{2 -> 4, 3 -> 3}, Map#{1 -> 3, 2 -> 4})

@test
def isProperSubmapOf18(): Bool = not Map.isProperSubmapOf(Map#{2 -> 4, 1 -> 3, 8 -> 9}, Map#{1 -> 3, 2 -> 4})

/////////////////////////////////////////////////////////////////////////////
// find                                                                    //
/////////////////////////////////////////////////////////////////////////////
@test
def find01(): Bool = Map.find((k, v) -> k == v, Map#{}: Map[Unit, Unit]) == None

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
def findLeft01(): Bool = Map.findLeft((k, v) -> k == v, Map#{}: Map[Unit, Unit]) == None

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
def findRight01(): Bool = Map.findRight((k, v) -> k == v, Map#{}: Map[Unit, Unit]) == None

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
def filter01(): Bool = Map.filter(v -> v % 2 == 0, Map#{}: Map[Unit, _]) == Map#{}

@test
def filter02(): Bool = Map.filter(v -> v % 2 == 0, Map#{1 -> 2}) == Map#{1 -> 2}

@test
def filter03(): Bool = Map.filter(v -> v % 2 == 0, Map#{1 -> 3}) == Map#{}

@test
def filter04(): Bool = Map.filter(v -> v % 2 == 0, Map#{1 -> 8, 2 -> 4}) == Map#{1 -> 8, 2 -> 4}

@test
def filter05(): Bool = Map.filter(v -> v % 2 == 0, Map#{1 -> 7, 2 -> 4}) == Map#{2 -> 4}

@test
def filter06(): Bool = Map.filter(v -> v % 2 == 0, Map#{1 -> 8, 2 -> 3}) == Map#{1 -> 8}

@test
def filter07(): Bool = Map.filter(v -> v % 2 == 0, Map#{1 -> -1, 2 -> -3}) == Map#{}

/////////////////////////////////////////////////////////////////////////////
// filterWithKey                                                           //
/////////////////////////////////////////////////////////////////////////////
@test
def filterWithKey01(): Bool = Map.filterWithKey((k, v) -> k == v, Map#{}: Map[Unit, Unit]) == Map#{}

@test
def filterWithKey02(): Bool = Map.filterWithKey((k, v) -> k == v, Map#{1 -> 1}) == Map#{1 -> 1}

@test
def filterWithKey03(): Bool = Map.filterWithKey((k, v) -> k == v, Map#{1 -> 3}) == Map#{}

@test
def filterWithKey04(): Bool = Map.filterWithKey((k, v) -> k == v, Map#{1 -> 1, 2 -> 2}) == Map#{1 -> 1, 2 -> 2}

@test
def filterWithKey05(): Bool = Map.filterWithKey((k, v) -> k == v, Map#{1 -> 7, 2 -> 2}) == Map#{2 -> 2}

@test
def filterWithKey06(): Bool = Map.filterWithKey((k, v) -> k == v, Map#{1 -> 1, 2 -> 3}) == Map#{1 -> 1}

@test
def filterWithKey07(): Bool = Map.filterWithKey((k, v) -> k == v, Map#{1 -> -1, 2 -> -3}) == Map#{}

/////////////////////////////////////////////////////////////////////////////
// map                                                                     //
/////////////////////////////////////////////////////////////////////////////
@test
def map01(): Bool = Map.map(v -> 3*v, Map#{}: Map[Unit, Int32]) == Map#{}

@test
def map02(): Bool = Map.map(v -> 3*v, Map#{1 -> 4}) == Map#{1 -> 12}

@test
def map03(): Bool = Map.map(v -> 3*v, Map#{2 -> -1, 0 -> 0}) == Map#{2 -> -3, 0 -> 0}

@test
def map04(): Bool = Map.map(v -> 3*v, Map#{2 -> -1, 5 -> 15, 11 -> -9}) == Map#{2 -> -3, 5 -> 45, 11 -> -27}

@test
def map05(): Bool = Map.map(v -> 3*v, Map#{2 -> -1, 5 -> 15, 11 -> -9, 8 -> 8}) == Map#{2 -> -3, 5 -> 45, 11 -> -27, 8 -> 24}

/////////////////////////////////////////////////////////////////////////////
// mapWithKey                                                              //
/////////////////////////////////////////////////////////////////////////////
@test
def mapWithKey01(): Bool = Map.mapWithKey((_, v) -> v, Map#{}: Map[Unit, Unit]) == Map#{}

@test
def mapWithKey02(): Bool = Map.mapWithKey((k, v) -> k + v, Map#{1 -> 4}) == Map#{1 -> 5}

@test
def mapWithKey03(): Bool = Map.mapWithKey((k, v) -> k + v, Map#{2 -> -1, 0 -> 0}) == Map#{2 -> 1, 0 -> 0}

@test
def mapWithKey04(): Bool = Map.mapWithKey((k, v) -> k + v, Map#{2 -> -1, 5 -> 15, 11 -> -9}) == Map#{2 -> 1, 5 -> 20, 11 -> 2}

@test
def mapWithKey05(): Bool = Map.mapWithKey((k, v) -> k + v, Map#{2 -> -1, 5 -> 15, 11 -> -9, 8 -> 8})
                                        == Map#{2 -> 1, 5 -> 20, 11 -> 2, 8 -> 16}

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
def count01(): Bool = Map.count((k, v) -> k == v, Map#{}: Map[Unit, Unit]) == 0

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
def exists01(): Bool = not Map.exists((k, v) -> k == v, Map#{}: Map[Unit, Unit])

@test
def exists02(): Bool = not Map.exists((k, v) -> k == v, Map#{1 -> 2})

@test
def exists03(): Bool = Map.exists((k, v) -> k == v, Map#{1 -> 1})

@test
def exists04(): Bool = not Map.exists((k, v) -> k == v, Map#{1 -> 2, 2 -> 3})

@test
def exists05(): Bool = Map.exists((k, v) -> k == v, Map#{1 -> 1, 2 -> 3})

@test
def exists06(): Bool = Map.exists((k, v) -> k == v, Map#{1 -> 2, 2 -> 2})

@test
def exists07(): Bool = Map.exists((k, v) -> k == v, Map#{1 -> 1, 2 -> 2})

/////////////////////////////////////////////////////////////////////////////
// forall                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def forall01(): Bool = Map.forall((k, v) -> k == v, Map#{}: Map[Unit, Unit])

@test
def forall02(): Bool = not Map.forall((k, v) -> k == v, Map#{1 -> 2})

@test
def forall03(): Bool = Map.forall((k, v) -> k == v, Map#{1 -> 1})

@test
def forall04(): Bool = not Map.forall((k, v) -> k == v, Map#{1 -> 2, 2 -> 3})

@test
def forall05(): Bool = not Map.forall((k, v) -> k == v, Map#{1 -> 1, 2 -> 3})

@test
def forall06(): Bool = not Map.forall((k, v) -> k == v, Map#{1 -> 2, 2 -> 2})

@test
def forall07(): Bool = Map.forall((k, v) -> k == v, Map#{1 -> 1, 2 -> 2})

/////////////////////////////////////////////////////////////////////////////
// union                                                                   //
/////////////////////////////////////////////////////////////////////////////
@test
def union01(): Bool = Map.union(Map#{}: Map[Unit, Unit], Map#{}) == Map#{}

@test
def union02(): Bool = Map.union(Map#{1 -> 2}, Map#{}) == Map#{1 -> 2}

@test
def union03(): Bool = Map.union(Map#{}, Map#{1 -> 2}) == Map#{1 -> 2}

@test
def union04(): Bool = Map.union(Map#{}, Map#{1 -> 2, 3 -> 4}) == Map#{1 -> 2, 3 -> 4}

@test
def union05(): Bool = Map.union(Map#{1 -> 2, 3 -> 4}, Map#{}) == Map#{1 -> 2, 3 -> 4}

@test
def union06(): Bool = Map.union(Map#{1 -> 2}, Map#{3 -> 4}) == Map#{1 -> 2, 3 -> 4}

@test
def union07(): Bool = Map.union(Map#{1 -> 2}, Map#{1 -> 5}) == Map#{1 -> 2}

@test
def union08(): Bool = Map.union(Map#{}, Map#{1 -> 2, 2 -> 3, 3 -> 4}) == Map#{1 -> 2, 2 -> 3, 3 -> 4}

@test
def union09(): Bool = Map.union(Map#{1 -> 2, 2 -> 3, 3 -> 4}, Map#{}) == Map#{1 -> 2, 2 -> 3, 3 -> 4}

@test
def union10(): Bool = Map.union(Map#{1 -> 2, 2 -> 3}, Map#{3 -> 4}) == Map#{1 -> 2, 2 -> 3, 3 -> 4}

@test
def union11(): Bool = Map.union(Map#{3 -> 4}, Map#{1 -> 2, 2 -> 3}) == Map#{1 -> 2, 2 -> 3, 3 -> 4}

@test
def union12(): Bool = Map.union(Map#{1 -> 2, 2 -> 3}, Map#{2 -> 8}) == Map#{1 -> 2, 2 -> 3}

@test
def union13(): Bool = Map.union(Map#{1 -> 2, 2 -> 3}, Map#{1 -> 8}) == Map#{1 -> 2, 2 -> 3}

@test
def union14(): Bool = Map.union(Map#{1 -> 4}, Map#{1 -> 2, 2 -> 3}) == Map#{1 -> 4, 2 -> 3}

@test
def union15(): Bool = Map.union(Map#{2 -> 7}, Map#{1 -> 2, 2 -> 3}) == Map#{1 -> 2, 2 -> 7}

@test
def union16(): Bool = Map.union(Map#{2 -> 7, 11 -> 14, 9 -> 8, 15 -> 22}, Map#{15 -> 21, 1 -> 2, 2 -> 8, 44 -> 33})
                                   == Map#{2 -> 7, 11 -> 14, 9 -> 8, 15 -> 22, 1 -> 2, 44 -> 33}

/////////////////////////////////////////////////////////////////////////////
// unionWith                                                               //
/////////////////////////////////////////////////////////////////////////////
@test
def unionWith01(): Bool = Map.unionWith((v1, v2) -> v1 - v2, Map#{}, Map#{}) == Map#{}: Map[Int, Int]

@test
def unionWith02(): Bool = Map.unionWith((v1, v2) -> v1 - v2, Map#{1 -> 2}, Map#{}) == Map#{1 -> 2}

@test
def unionWith03(): Bool = Map.unionWith((v1, v2) -> v1 - v2, Map#{}, Map#{1 -> 2}) == Map#{1 -> 2}

@test
def unionWith04(): Bool = Map.unionWith((v1, v2) -> v1 - v2, Map#{}, Map#{1 -> 2, 3 -> 4}) == Map#{1 -> 2, 3 -> 4}

@test
def unionWith05(): Bool = Map.unionWith((v1, v2) -> v1 - v2, Map#{1 -> 2, 3 -> 4}, Map#{}) == Map#{1 -> 2, 3 -> 4}

@test
def unionWith06(): Bool = Map.unionWith((v1, v2) -> v1 - v2, Map#{1 -> 2}, Map#{3 -> 4}) == Map#{1 -> 2, 3 -> 4}

@test
def unionWith07(): Bool = Map.unionWith((v1, v2) -> v1 - v2, Map#{1 -> 2}, Map#{1 -> 5}) == Map#{1 -> -3}

@test
def unionWith08(): Bool = Map.unionWith((v1, v2) -> v1 - v2, Map#{}, Map#{1 -> 2, 2 -> 3, 3 -> 4}) == Map#{1 -> 2, 2 -> 3, 3 -> 4}

@test
def unionWith09(): Bool = Map.unionWith((v1, v2) -> v1 - v2, Map#{1 -> 2, 2 -> 3, 3 -> 4}, Map#{}) == Map#{1 -> 2, 2 -> 3, 3 -> 4}

@test
def unionWith10(): Bool = Map.unionWith((v1, v2) -> v1 - v2, Map#{1 -> 2, 2 -> 3}, Map#{3 -> 4}) == Map#{1 -> 2, 2 -> 3, 3 -> 4}

@test
def unionWith11(): Bool = Map.unionWith((v1, v2) -> v1 - v2, Map#{3 -> 4}, Map#{1 -> 2, 2 -> 3}) == Map#{1 -> 2, 2 -> 3, 3 -> 4}

@test
def unionWith12(): Bool = Map.unionWith((v1, v2) -> v1 - v2, Map#{1 -> 2, 2 -> 3}, Map#{2 -> 8}) == Map#{1 -> 2, 2 -> -5}

@test
def unionWith13(): Bool = Map.unionWith((v1, v2) -> v1 - v2, Map#{1 -> 2, 2 -> 3}, Map#{1 -> 8}) == Map#{1 -> -6, 2 -> 3}

@test
def unionWith14(): Bool = Map.unionWith((v1, v2) -> v1 - v2, Map#{1 -> 4}, Map#{1 -> 2, 2 -> 3}) == Map#{1 -> 2, 2 -> 3}

@test
def unionWith15(): Bool = Map.unionWith((v1, v2) -> v1 - v2, Map#{2 -> 7}, Map#{1 -> 2, 2 -> 3}) == Map#{1 -> 2, 2 -> 4}

@test
def unionWith16(): Bool = Map.unionWith((v1, v2) -> v1 - v2, Map#{2 -> 7, 11 -> 14, 9 -> 8, 15 -> 22}, Map#{15 -> 21, 1 -> 2, 2 -> 8, 44 -> 33})
                                   == Map#{2 -> -1, 11 -> 14, 9 -> 8, 15 -> 1, 1 -> 2, 44 -> 33}

/////////////////////////////////////////////////////////////////////////////
// unionWithKey                                                            //
/////////////////////////////////////////////////////////////////////////////
@test
def unionWithKey01(): Bool = Map.unionWithKey((k, v1, v2) -> k + v1 - v2, Map#{}, Map#{}) == Map#{}: Map[Int, Int]

@test
def unionWithKey02(): Bool = Map.unionWithKey((k, v1, v2) -> k + v1 - v2, Map#{1 -> 2}, Map#{}) == Map#{1 -> 2}

@test
def unionWithKey03(): Bool = Map.unionWithKey((k, v1, v2) -> k + v1 - v2, Map#{}, Map#{1 -> 2}) == Map#{1 -> 2}

@test
def unionWithKey04(): Bool = Map.unionWithKey((k, v1, v2) -> k + v1 - v2, Map#{}, Map#{1 -> 2, 3 -> 4}) == Map#{1 -> 2, 3 -> 4}

@test
def unionWithKey05(): Bool = Map.unionWithKey((k, v1, v2) -> k + v1 - v2, Map#{1 -> 2, 3 -> 4}, Map#{}) == Map#{1 -> 2, 3 -> 4}

@test
def unionWithKey06(): Bool = Map.unionWithKey((k, v1, v2) -> k + v1 - v2, Map#{1 -> 2}, Map#{3 -> 4}) == Map#{1 -> 2, 3 -> 4}

@test
def unionWithKey07(): Bool = Map.unionWithKey((k, v1, v2) -> k + v1 - v2, Map#{1 -> 2}, Map#{1 -> 5}) == Map#{1 -> -2}

@test
def unionWithKey08(): Bool = Map.unionWithKey((k, v1, v2) -> k + v1 - v2, Map#{}, Map#{1 -> 2, 2 -> 3, 3 -> 4}) == Map#{1 -> 2, 2 -> 3, 3 -> 4}

@test
def unionWithKey09(): Bool = Map.unionWithKey((k, v1, v2) -> k + v1 - v2, Map#{1 -> 2, 2 -> 3, 3 -> 4}, Map#{}) == Map#{1 -> 2, 2 -> 3, 3 -> 4}

@test
def unionWithKey10(): Bool = Map.unionWithKey((k, v1, v2) -> k + v1 - v2, Map#{1 -> 2, 2 -> 3}, Map#{3 -> 4}) == Map#{1 -> 2, 2 -> 3, 3 -> 4}

@test
def unionWithKey11(): Bool = Map.unionWithKey((k, v1, v2) -> k + v1 - v2, Map#{3 -> 4}, Map#{1 -> 2, 2 -> 3}) == Map#{1 -> 2, 2 -> 3, 3 -> 4}

@test
def unionWithKey12(): Bool = Map.unionWithKey((k, v1, v2) -> k + v1 - v2, Map#{1 -> 2, 2 -> 3}, Map#{2 -> 8}) == Map#{1 -> 2, 2 -> -3}

@test
def unionWithKey13(): Bool = Map.unionWithKey((k, v1, v2) -> k + v1 - v2, Map#{1 -> 2, 2 -> 3}, Map#{1 -> 8}) == Map#{1 -> -5, 2 -> 3}

@test
def unionWithKey14(): Bool = Map.unionWithKey((k, v1, v2) -> k + v1 - v2, Map#{1 -> 4}, Map#{1 -> 2, 2 -> 3}) == Map#{1 -> 3, 2 -> 3}

@test
def unionWithKey15(): Bool = Map.unionWithKey((k, v1, v2) -> k + v1 - v2, Map#{2 -> 7}, Map#{1 -> 2, 2 -> 3}) == Map#{1 -> 2, 2 -> 6}

@test
def unionWithKey16(): Bool = Map.unionWithKey((k, v1, v2) -> k + v1 - v2, Map#{2 -> 7, 11 -> 14, 9 -> 8, 15 -> 22}, Map#{15 -> 21, 1 -> 2, 2 -> 8, 44 -> 33})
                                   == Map#{2 -> 1, 11 -> 14, 9 -> 8, 15 -> 16, 1 -> 2, 44 -> 33}

/////////////////////////////////////////////////////////////////////////////
// intersection                                                            //
/////////////////////////////////////////////////////////////////////////////
@test
def intersection01(): Bool = Map.intersection(Map#{}: Map[Unit, Unit], Map#{}) == Map#{}

@test
def intersection02(): Bool = Map.intersection(Map#{1 -> 2}, Map#{}) == Map#{}

@test
def intersection03(): Bool = Map.intersection(Map#{}, Map#{1 -> 2}) == Map#{}

@test
def intersection04(): Bool = Map.intersection(Map#{}, Map#{1 -> 2, 3 -> 4}) == Map#{}

@test
def intersection05(): Bool = Map.intersection(Map#{1 -> 2, 3 -> 4}, Map#{}) == Map#{}

@test
def intersection06(): Bool = Map.intersection(Map#{1 -> 2}, Map#{3 -> 4}) == Map#{}

@test
def intersection07(): Bool = Map.intersection(Map#{1 -> 2}, Map#{1 -> 5}) == Map#{1 -> 2}

@test
def intersection08(): Bool = Map.intersection(Map#{}, Map#{1 -> 2, 2 -> 3, 3 -> 4}) == Map#{}

@test
def intersection09(): Bool = Map.intersection(Map#{1 -> 2, 2 -> 3, 3 -> 4}, Map#{}) == Map#{}

@test
def intersection10(): Bool = Map.intersection(Map#{1 -> 2, 2 -> 3}, Map#{3 -> 4}) == Map#{}

@test
def intersection11(): Bool = Map.intersection(Map#{3 -> 4}, Map#{1 -> 2, 2 -> 3}) == Map#{}

@test
def intersection12(): Bool = Map.intersection(Map#{1 -> 2, 2 -> 3}, Map#{2 -> 8}) == Map#{2 -> 3}

@test
def intersection13(): Bool = Map.intersection(Map#{1 -> 2, 2 -> 3}, Map#{1 -> 8}) == Map#{1 -> 2}

@test
def intersection14(): Bool = Map.intersection(Map#{1 -> 4}, Map#{1 -> 2, 2 -> 3}) == Map#{1 -> 4}

@test
def intersection15(): Bool = Map.intersection(Map#{2 -> 7}, Map#{1 -> 2, 2 -> 3}) == Map#{2 -> 7}

@test
def intersection16(): Bool = Map.intersection(Map#{2 -> 7, 11 -> 14, 9 -> 8, 15 -> 22}, Map#{15 -> 21, 1 -> 2, 2 -> 8, 44 -> 33, 11 -> 1})
                                   == Map#{2 -> 7, 15 -> 22, 11 -> 14}

/////////////////////////////////////////////////////////////////////////////
// intersectionWith                                                        //
/////////////////////////////////////////////////////////////////////////////
@test
def intersectionWith01(): Bool = Map.intersectionWith((v1, v2) -> v1 - v2, Map#{}, Map#{}) == Map#{}: Map[Int, Int]

@test
def intersectionWith02(): Bool = Map.intersectionWith((v1, v2) -> v1 - v2, Map#{1 -> 2}, Map#{}) == Map#{}

@test
def intersectionWith03(): Bool = Map.intersectionWith((v1, v2) -> v1 - v2, Map#{}, Map#{1 -> 2}) == Map#{}

@test
def intersectionWith04(): Bool = Map.intersectionWith((v1, v2) -> v1 - v2, Map#{}, Map#{1 -> 2, 3 -> 4}) == Map#{}

@test
def intersectionWith05(): Bool = Map.intersectionWith((v1, v2) -> v1 - v2, Map#{1 -> 2, 3 -> 4}, Map#{}) == Map#{}

@test
def intersectionWith06(): Bool = Map.intersectionWith((v1, v2) -> v1 - v2, Map#{1 -> 2}, Map#{3 -> 4}) == Map#{}

@test
def intersectionWith07(): Bool = Map.intersectionWith((v1, v2) -> v1 - v2, Map#{1 -> 2}, Map#{1 -> 5}) == Map#{1 -> -3}

@test
def intersectionWith08(): Bool = Map.intersectionWith((v1, v2) -> v1 - v2, Map#{}, Map#{1 -> 2, 2 -> 3, 3 -> 4}) == Map#{}

@test
def intersectionWith09(): Bool = Map.intersectionWith((v1, v2) -> v1 - v2, Map#{1 -> 2, 2 -> 3, 3 -> 4}, Map#{}) == Map#{}

@test
def intersectionWith10(): Bool = Map.intersectionWith((v1, v2) -> v1 - v2, Map#{1 -> 2, 2 -> 3}, Map#{3 -> 4}) == Map#{}

@test
def intersectionWith11(): Bool = Map.intersectionWith((v1, v2) -> v1 - v2, Map#{3 -> 4}, Map#{1 -> 2, 2 -> 3}) == Map#{}

@test
def intersectionWith12(): Bool = Map.intersectionWith((v1, v2) -> v1 - v2, Map#{1 -> 2, 2 -> 3}, Map#{2 -> 8}) == Map#{2 -> -5}

@test
def intersectionWith13(): Bool = Map.intersectionWith((v1, v2) -> v1 - v2, Map#{1 -> 2, 2 -> 3}, Map#{1 -> 8}) == Map#{1 -> -6}

@test
def intersectionWith14(): Bool = Map.intersectionWith((v1, v2) -> v1 - v2, Map#{1 -> 4}, Map#{1 -> 2, 2 -> 3}) == Map#{1 -> 2}

@test
def intersectionWith15(): Bool = Map.intersectionWith((v1, v2) -> v1 - v2, Map#{2 -> 7}, Map#{1 -> 2, 2 -> 3}) == Map#{2 -> 4}

@test
def intersectionWith16(): Bool = Map.intersectionWith((v1, v2) -> v1 - v2, Map#{2 -> 7, 11 -> 14, 9 -> 8, 15 -> 22}, Map#{15 -> 21, 1 -> 2, 2 -> 8, 44 -> 33, 11 -> 11})
                                   == Map#{2 -> -1, 15 -> 1, 11 -> 3}

/////////////////////////////////////////////////////////////////////////////
// intersectionWithKey                                                     //
/////////////////////////////////////////////////////////////////////////////
@test
def intersectionWithKey01(): Bool = Map.intersectionWithKey((k, v1, v2) -> k + v1 - v2, Map#{}, Map#{}) == Map#{}: Map[Int, Int]

@test
def intersectionWithKey02(): Bool = Map.intersectionWithKey((k, v1, v2) -> k + v1 - v2, Map#{1 -> 2}, Map#{}) == Map#{}

@test
def intersectionWithKey03(): Bool = Map.intersectionWithKey((k, v1, v2) -> k + v1 - v2, Map#{}, Map#{1 -> 2}) == Map#{}

@test
def intersectionWithKey04(): Bool = Map.intersectionWithKey((k, v1, v2) -> k + v1 - v2, Map#{}, Map#{1 -> 2, 3 -> 4}) == Map#{}

@test
def intersectionWithKey05(): Bool = Map.intersectionWithKey((k, v1, v2) -> k + v1 - v2, Map#{1 -> 2, 3 -> 4}, Map#{}) == Map#{}

@test
def intersectionWithKey06(): Bool = Map.intersectionWithKey((k, v1, v2) -> k + v1 - v2, Map#{1 -> 2}, Map#{3 -> 4}) == Map#{}

@test
def intersectionWithKey07(): Bool = Map.intersectionWithKey((k, v1, v2) -> k + v1 - v2, Map#{1 -> 2}, Map#{1 -> 5}) == Map#{1 -> -2}

@test
def intersectionWithKey08(): Bool = Map.intersectionWithKey((k, v1, v2) -> k + v1 - v2, Map#{}, Map#{1 -> 2, 2 -> 3, 3 -> 4}) == Map#{}

@test
def intersectionWithKey09(): Bool = Map.intersectionWithKey((k, v1, v2) -> k + v1 - v2, Map#{1 -> 2, 2 -> 3, 3 -> 4}, Map#{}) == Map#{}

@test
def intersectionWithKey10(): Bool = Map.intersectionWithKey((k, v1, v2) -> k + v1 - v2, Map#{1 -> 2, 2 -> 3}, Map#{3 -> 4}) == Map#{}

@test
def intersectionWithKey11(): Bool = Map.intersectionWithKey((k, v1, v2) -> k + v1 - v2, Map#{3 -> 4}, Map#{1 -> 2, 2 -> 3}) == Map#{}

@test
def intersectionWithKey12(): Bool = Map.intersectionWithKey((k, v1, v2) -> k + v1 - v2, Map#{1 -> 2, 2 -> 3}, Map#{2 -> 8}) == Map#{2 -> -3}

@test
def intersectionWithKey13(): Bool = Map.intersectionWithKey((k, v1, v2) -> k + v1 - v2, Map#{1 -> 2, 2 -> 3}, Map#{1 -> 8}) == Map#{1 -> -5}

@test
def intersectionWithKey14(): Bool = Map.intersectionWithKey((k, v1, v2) -> k + v1 - v2, Map#{1 -> 4}, Map#{1 -> 2, 2 -> 3}) == Map#{1 -> 3}

@test
def intersectionWithKey15(): Bool = Map.intersectionWithKey((k, v1, v2) -> k + v1 - v2, Map#{2 -> 7}, Map#{1 -> 2, 2 -> 3}) == Map#{2 -> 6}

@test
def intersectionWithKey16(): Bool = Map.intersectionWithKey((k, v1, v2) -> k + v1 - v2, Map#{2 -> 7, 11 -> 14, 9 -> 8, 15 -> 22}, Map#{15 -> 21, 1 -> 2, 2 -> 8, 44 -> 33, 11 -> 17})
                                  == Map#{2 -> 1, 15 -> 16, 11 -> 8}

/////////////////////////////////////////////////////////////////////////////
// difference                                                              //
/////////////////////////////////////////////////////////////////////////////
@test
def difference01(): Bool = Map.difference(Map#{}: Map[Unit, Unit], Map#{}: Map[Unit, Unit]) == Map#{}

@test
def difference02(): Bool = Map.difference(Map#{1 -> 2}, Map#{}) == Map#{1 -> 2}

@test
def difference03(): Bool = Map.difference(Map#{}, Map#{1 -> 2}) == Map#{}

@test
def difference04(): Bool = Map.difference(Map#{}, Map#{1 -> 2, 3 -> 4}) == Map#{}

@test
def difference05(): Bool = Map.difference(Map#{1 -> 2, 3 -> 4}, Map#{}) == Map#{1 -> 2, 3 -> 4}

@test
def difference06(): Bool = Map.difference(Map#{1 -> 2}, Map#{3 -> 4}) == Map#{1 -> 2}

@test
def difference07(): Bool = Map.difference(Map#{1 -> 2}, Map#{1 -> 5}) == Map#{}

@test
def difference08(): Bool = Map.difference(Map#{}, Map#{1 -> 2, 2 -> 3, 3 -> 4}) == Map#{}

@test
def difference09(): Bool = Map.difference(Map#{1 -> 2, 2 -> 3, 3 -> 4}, Map#{}) == Map#{1 -> 2, 2 -> 3, 3 -> 4}

@test
def difference10(): Bool = Map.difference(Map#{1 -> 2, 2 -> 3}, Map#{3 -> 4}) == Map#{1 -> 2, 2 -> 3}

@test
def difference11(): Bool = Map.difference(Map#{3 -> 4}, Map#{1 -> 2, 2 -> 3}) == Map#{3 -> 4}

@test
def difference12(): Bool = Map.difference(Map#{1 -> 2, 2 -> 3}, Map#{2 -> 8}) == Map#{1 -> 2}

@test
def difference13(): Bool = Map.difference(Map#{1 -> 2, 2 -> 3}, Map#{1 -> 8}) == Map#{2 -> 3}

@test
def difference14(): Bool = Map.difference(Map#{1 -> 4}, Map#{1 -> 2, 2 -> 3}) == Map#{}

@test
def difference15(): Bool = Map.difference(Map#{2 -> 7}, Map#{1 -> 2, 2 -> 3}) == Map#{}

@test
def difference16(): Bool = Map.difference(Map#{2 -> 7, 11 -> 14, 9 -> 8, 15 -> 22}, Map#{15 -> 21, 1 -> 2, 2 -> 8, 44 -> 33})
                                  == Map#{11 -> 14, 9 -> 8}

/////////////////////////////////////////////////////////////////////////////
// differenceWith                                                          //
/////////////////////////////////////////////////////////////////////////////
@test
def differenceWith01(): Bool = Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, Map#{}, Map#{}) == Map#{}: Map[Int, Int]

@test
def differenceWith02(): Bool = Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, Map#{1 -> 2}, Map#{}) == Map#{1 -> 2}

@test
def differenceWith03(): Bool = Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, Map#{}, Map#{1 -> 2}) == Map#{}

@test
def differenceWith04(): Bool = Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, Map#{}, Map#{1 -> 2, 3 -> 4}) == Map#{}

@test
def differenceWith05(): Bool = Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, Map#{1 -> 2, 3 -> 4}, Map#{}) == Map#{1 -> 2, 3 -> 4}

@test
def differenceWith06(): Bool = Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, Map#{1 -> 2}, Map#{3 -> 4}) == Map#{1 -> 2}

@test
def differenceWith07(): Bool = Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, Map#{1 -> 2}, Map#{1 -> 5}) == Map#{}

@test
def differenceWith08(): Bool = Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, Map#{1 -> 5}, Map#{1 -> 4}) == Map#{1 -> 5}

@test
def differenceWith09(): Bool = Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, Map#{}, Map#{1 -> 2, 2 -> 3, 3 -> 4}) == Map#{}

@test
def differenceWith10(): Bool = Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, Map#{1 -> 2, 2 -> 3, 3 -> 4}, Map#{}) == Map#{1 -> 2, 2 -> 3, 3 -> 4}

@test
def differenceWith11(): Bool = Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, Map#{1 -> 2, 2 -> 3}, Map#{3 -> 4}) == Map#{1 -> 2, 2 -> 3}

@test
def differenceWith12(): Bool = Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, Map#{3 -> 4}, Map#{1 -> 2, 2 -> 3}) == Map#{3 -> 4}

@test
def differenceWith13(): Bool = Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, Map#{1 -> 2, 2 -> 3}, Map#{2 -> 8}) == Map#{1 -> 2}

@test
def differenceWith14(): Bool = Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, Map#{1 -> 2, 2 -> 9}, Map#{2 -> 8}) == Map#{1 -> 2, 2 -> 9}

@test
def differenceWith15(): Bool = Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, Map#{1 -> 2, 2 -> 3}, Map#{1 -> 8}) == Map#{2 -> 3}

@test
def differenceWith16(): Bool = Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, Map#{1 -> 9, 2 -> 3}, Map#{1 -> 8}) == Map#{1 -> 9, 2 -> 3}

@test
def differenceWith17(): Bool = Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, Map#{1 -> 1}, Map#{1 -> 2, 2 -> 3}) == Map#{}

@test
def differenceWith18(): Bool = Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, Map#{1 -> 4}, Map#{1 -> 2, 2 -> 3}) == Map#{1 -> 4}

@test
def differenceWith19(): Bool = Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, Map#{2 -> -4}, Map#{1 -> 2, 2 -> 3}) == Map#{}

@test
def differenceWith20(): Bool = Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, Map#{2 -> 7}, Map#{1 -> 2, 2 -> 3}) == Map#{2 -> 7}

@test
def differenceWith21(): Bool = Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, Map#{2 -> 7, 11 -> 14, 9 -> 8, 15 -> 22}, Map#{15 -> 21, 1 -> 2, 2 -> 8, 44 -> 33})
                                  == Map#{11 -> 14, 9 -> 8, 15 -> 22}

/////////////////////////////////////////////////////////////////////////////
// differenceWithKey                                                       //
/////////////////////////////////////////////////////////////////////////////
@test
def differenceWithKey01(): Bool = Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, Map#{}, Map#{}) == Map#{}: Map[Int, Int]

@test
def differenceWithKey02(): Bool = Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, Map#{1 -> 2}, Map#{}) == Map#{1 -> 2}

@test
def differenceWithKey03(): Bool = Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, Map#{}, Map#{1 -> 2}) == Map#{}

@test
def differenceWithKey04(): Bool = Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, Map#{}, Map#{1 -> 2, 3 -> 4}) == Map#{}

@test
def differenceWithKey05(): Bool = Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, Map#{1 -> 2, 3 -> 4}, Map#{}) == Map#{1 -> 2, 3 -> 4}

@test
def differenceWithKey06(): Bool = Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, Map#{1 -> 2}, Map#{3 -> 4}) == Map#{1 -> 2}

@test
def differenceWithKey07(): Bool = Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, Map#{1 -> 2}, Map#{1 -> 5}) == Map#{}

@test
def differenceWithKey08(): Bool = Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, Map#{1 -> 5}, Map#{1 -> 4}) == Map#{1 -> 6}

@test
def differenceWithKey09(): Bool = Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, Map#{}, Map#{1 -> 2, 2 -> 3, 3 -> 4}) == Map#{}

@test
def differenceWithKey10(): Bool = Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, Map#{1 -> 2, 2 -> 3, 3 -> 4}, Map#{}) == Map#{1 -> 2, 2 -> 3, 3 -> 4}

@test
def differenceWithKey11(): Bool = Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, Map#{1 -> 2, 2 -> 3}, Map#{3 -> 4}) == Map#{1 -> 2, 2 -> 3}

@test
def differenceWithKey12(): Bool = Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, Map#{3 -> 4}, Map#{1 -> 2, 2 -> 3}) == Map#{3 -> 4}

@test
def differenceWithKey13(): Bool = Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, Map#{1 -> 2, 2 -> 3}, Map#{2 -> 8}) == Map#{1 -> 2}

@test
def differenceWithKey14(): Bool = Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, Map#{1 -> 2, 2 -> 9}, Map#{2 -> 8}) == Map#{1 -> 2, 2 -> 11}

@test
def differenceWithKey15(): Bool = Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, Map#{1 -> 2, 2 -> 3}, Map#{1 -> 8}) == Map#{2 -> 3}

@test
def differenceWithKey16(): Bool = Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, Map#{1 -> 9, 2 -> 3}, Map#{1 -> 8}) == Map#{1 -> 10, 2 -> 3}

@test
def differenceWithKey17(): Bool = Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, Map#{1 -> 1}, Map#{1 -> 2, 2 -> 3}) == Map#{}

@test
def differenceWithKey18(): Bool = Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, Map#{1 -> 4}, Map#{1 -> 2, 2 -> 3}) == Map#{1 -> 5}

@test
def differenceWithKey19(): Bool = Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, Map#{2 -> -4}, Map#{1 -> 2, 2 -> 3}) == Map#{}

@test
def differenceWithKey20(): Bool = Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, Map#{2 -> 7}, Map#{1 -> 2, 2 -> 3}) == Map#{2 -> 9}

@test
def differenceWithKey21(): Bool = Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, Map#{2 -> 7, 11 -> 14, 9 -> 8, 15 -> 22}, Map#{15 -> 21, 1 -> 2, 2 -> 8, 44 -> 33})
                                  == Map#{11 -> 14, 9 -> 8, 15 -> 37}

/////////////////////////////////////////////////////////////////////////////
// toSet                                                                   //
/////////////////////////////////////////////////////////////////////////////
@test
def toSet01(): Bool = Map.toSet(Map#{}: Map[Unit, Unit]) == Set#{}

@test
def toSet02(): Bool = Map.toSet(Map#{1 -> 2}) == Set#{(1, 2)}

@test
def toSet03(): Bool = Map.toSet(Map#{1 -> 2, 3 -> 4}) == Set#{(1, 2), (3, 4)}

@test
def toSet04(): Bool = Map.toSet(Map#{1 -> 2, 2 -> 2, 3 -> 4}) == Set#{(1, 2), (2, 2), (3, 4)}

/////////////////////////////////////////////////////////////////////////////
// toList                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def toList01(): Bool = Map.toList(Map#{}: Map[Unit, Unit]) == Nil

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
def eq01(): Bool = Map#{}: Map[Unit, Unit] == Map#{}

@test
def eq02(): Bool = Map#{1 -> 2} != Map#{}

@test
def eq03(): Bool = Map#{} != Map#{1 -> 2}

@test
def eq04(): Bool = Map#{1 -> 2, 3 -> 4} != Map#{}

@test
def eq05(): Bool = Map#{} != Map#{1 -> 2, 3 -> 4}

@test
def eq06(): Bool = Map#{1 -> 2} == Map#{1 -> 2}

@test
def eq07(): Bool = Map#{1 -> 3} != Map#{1 -> 2}

@test
def eq08(): Bool = Map#{1 -> 2} != Map#{1 -> 3}

@test
def eq09(): Bool = Map#{2 -> 2} != Map#{1 -> 2}

@test
def eq10(): Bool = Map#{1 -> 2} != Map#{4 -> 2}

@test
def eq11(): Bool = Map#{1 -> 2, 3 -> 4} != Map#{1 -> 2}

@test
def eq12(): Bool = Map#{1 -> 2} != Map#{1 -> 2, 3 -> 4}

@test
def eq13(): Bool = Map#{1 -> 2, 3 -> 4} == Map#{1 -> 2, 3 -> 4}

@test
def eq14(): Bool = Map#{1 -> 2, 3 -> 4} == Map#{3 -> 4, 1 -> 2}

@test
def eq15(): Bool = Map#{1 -> 1, 3 -> 4} != Map#{1 -> 2, 3 -> 4}

@test
def eq16(): Bool = Map#{4 -> 2, 3 -> 4} != Map#{1 -> 2, 3 -> 4}

@test
def eq17(): Bool = Map#{1 -> 2, 3 -> 4} != Map#{1 -> 2, 2 -> 4}

@test
def eq18(): Bool = Map#{1 -> 2, 3 -> 4} != Map#{1 -> 2, 3 -> 8}

@test
def eq19(): Bool = Map#{1 -> 2, 3 -> 4, 5 -> 6} != Map#{1 -> 2, 3 -> 4}

@test
def eq20(): Bool = Map#{1 -> 2, 3 -> 4} != Map#{1 -> 2, 3 -> 4, 5 -> 6}

@test
def eq21(): Bool = Map#{1 -> 2, 3 -> 4, 5 -> 6} == Map#{1 -> 2, 3 -> 4, 5 -> 6}

@test
def eq22(): Bool = Map#{1 -> 2, 3 -> 4, 5 -> 6} == Map#{1 -> 2, 5 -> 6, 3 -> 4}

@test
def eq23(): Bool = Map#{1 -> 2, 3 -> 4, 5 -> 6} == Map#{3 -> 4, 5 -> 6, 1 -> 2}

@test
def eq24(): Bool = Map#{1 -> 2, 3 -> 4, 5 -> 6} == Map#{5 -> 6, 3 -> 4, 1 -> 2}

@test
def eq25(): Bool = Map#{1 -> 2, 3 -> 4, 5 -> 6, 7 -> 8, 9 -> 10} == Map#{7 -> 8, 1 -> 2, 3 -> 4, 9 -> 10, 5 -> 6}

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
    Map.query(_ -> EqualTo, Map#{}: Map[Unit, Unit]) == Nil

@test
def query02(): Bool =
    Map.query(_ -> EqualTo, Map#{1 -> 101, 2 -> 42}) == (1, 101) :: (2, 42) :: Nil

@test
def query03(): Bool =
    Map.query(x -> if (x < 2) LessThan else if (x > 2) GreaterThan else EqualTo, Map#{1 -> 1, 2 -> 2, 3 -> 3}) == (2, 2) :: Nil

@test
def query04(): Bool =
    Map.query(x -> if (x < 'b') LessThan else if (x > 'c') GreaterThan else EqualTo, Map#{'a' -> 0, 'b' -> 1, 'c' -> 2, 'd' -> 3, 'e' -> 4}) == ('b', 1) :: ('c', 2) :: Nil

@test
def query05(): Bool =
    let m = Map.unfold(s -> if (s > 10) None else Some(s, s, s + 1), 5);
    Map.query(x -> if (x < 5) LessThan else if (x >= 10) GreaterThan else EqualTo, m) == List.map(x -> (x, x), List.range(5, 10))

@test
def query06(): Bool =
    let m = Map.unfold(s -> if (s > 75) None else Some(s, Char.fromInt32(s + 48), s + 1), 25);
    Map.query(x -> if (x < 25) LessThan else if (x >= 75) GreaterThan else EqualTo, m) == List.map(x -> (x, Char.fromInt32(x + 48)), List.range(25, 75))

@test
def query07(): Bool =
    Map.query(x -> if (x < 42) LessThan else if (x > 42) GreaterThan else EqualTo, Map#{42 -> true}) == (42, true) :: Nil

@test
def query08(): Bool =
    Map.query(x -> if (x < 1) LessThan else if (x > 1) GreaterThan else EqualTo, Map#{1 -> 2, 2 -> 3, 3 -> 4, 4 -> 5}) == (1, 2) :: Nil

@test
def query09(): Bool =
    Map.query(x -> if (x < 2) LessThan else if (x > 3) GreaterThan else EqualTo, Map#{1 -> 2, 2 -> 3, 3 -> 4, 4 -> 5}) == (2, 3) :: (3, 4) :: Nil

@test
def query10(): Bool =
    Map.query(x -> if (x < 4) LessThan else if (x > 4) GreaterThan else EqualTo, Map#{1 -> 2, 2 -> 3, 3 -> 4, 4 -> 5}) == (4, 5) :: Nil

@test
def query11(): Bool =
    let m = Map#{(1, 2) -> 0, (1, 3) -> 1, (2, 1) -> 2, (2, 2) -> 3, (2, 3) -> 4, (3, 1) -> 5};
    Map.query(x -> if (fst(x) < 1) LessThan else if (fst(x) > 1) GreaterThan else EqualTo, m) == ((1, 2), 0) :: ((1, 3), 1) :: Nil

@test
def query12(): Bool =
    let m = Map#{(1, 2) -> 0, (1, 3) -> 1, (2, 1) -> 2, (2, 2) -> 3, (2, 3) -> 4, (3, 1) -> 5};
    Map.query(x -> if (fst(x) < 3) LessThan else if (fst(x) > 3) GreaterThan else EqualTo, m) == ((3, 1), 5) :: Nil

@test
def query13(): Bool =
    let m = Map#{(1, 2) -> 0, (1, 3) -> 1, (2, 1) -> 2, (2, 2) -> 3, (2, 3) -> 4, (3, 1) -> 5};
    Map.query(x -> if (fst(x) > 2) GreaterThan else EqualTo, m) == ((1, 2), 0) :: ((1, 3), 1) :: ((2, 1), 2) :: ((2, 2), 3) :: ((2, 3), 4) :: Nil

@test
def query14(): Bool =
    let m = Map#{(1, 2) -> 0, (1, 3) -> 1, (2, 1) -> 2, (2, 2) -> 3, (2, 3) -> 4, (3, 1) -> 5};
    Map.query(x -> if (fst(x) < 2) LessThan else EqualTo, m) == ((2, 1), 2) :: ((2, 2), 3) :: ((2, 3), 4) :: ((3, 1), 5) :: Nil

@test
def query15(): Bool =
    let m = Map#{(1, 2) -> 0, (1, 3) -> 1, (2, 1) -> 2, (2, 2) -> 3, (2, 3) -> 4, (3, 1) -> 5};
    let f = x -> {
        let cmp = x <=> (2, 2);
        if (cmp == GreaterThan)
            cmp
        else
            EqualTo
        };
    Map.query(f, m) == ((1, 2), 0) :: ((1, 3), 1) :: ((2, 1), 2) :: ((2, 2), 3) :: Nil

@test
def query16(): Bool =
    let m = Map#{(1, 2) -> 0, (1, 3) -> 1, (2, 1) -> 2, (2, 2) -> 3, (2, 3) -> 4, (3, 1) -> 5};
    let f = x -> {
        let cmp = x <=> (2, 2);
        if (cmp == LessThan)
            cmp
        else
            EqualTo
        };
    Map.query(f, m) == ((2, 2), 3) :: ((2, 3), 4) :: ((3, 1), 5) :: Nil

@test
def query17(): Bool =
    let m = Map#{(1, 2, 2) -> 0, (1, 2, 3) -> 1, (1, 2, 4) -> 2, (2, 2, 4) -> 3};
    let f = x -> {
        let cmp = x <=> (1, 2, 3);
        if (cmp == LessThan)
            cmp
        else
            EqualTo
        };
    Map.query(f, m) == ((1, 2, 3), 1) :: ((1, 2, 4), 2) :: ((2, 2, 4), 3) :: Nil

@test
def query18(): Bool =
    let m = Map#{(1, 2, 2) -> 0, (1, 2, 3) -> 1, (1, 2, 4) -> 2, (2, 2, 4) -> 3};
    let f = x -> {
        let cmp = x <=> (1, 2, 3);
        if (cmp == GreaterThan)
            cmp
        else
            EqualTo
        };
    Map.query(f, m) == ((1, 2, 2), 0) :: ((1, 2, 3), 1) :: Nil

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

    /////////////////////////////////////////////////////////////////////////////
    // hash                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def hash01(): Bool = hash(Map#{}: Map[Unit, Unit]) == hash(Map#{}: Map[Unit, Unit])

    @test
    def hash02(): Bool = hash(Map#{1 -> true, 2 -> false, 3 -> false}) == hash(Map#{1 -> true, 2 -> false, 3 -> false})

    @test
    def hash03(): Bool = hash(Map#{1 -> 2, 1 -> 2}) != hash(Map#{1 -> 1, 2 -> 2})

    @test
    def hash04(): Bool = hash(Map#{1 -> 'a', 2 -> 'b'}) != hash(Map#{'a' -> 1, 'b' -> 2})

    @test
    def hash05(): Bool = hash(Map#{1 -> 'a', 2 -> 'b', 3 -> 'z', 4 -> 'x'}) == hash(Map#{1 -> 'a', 2 -> 'b', 3 -> 'z', 4 -> 'x'})

}
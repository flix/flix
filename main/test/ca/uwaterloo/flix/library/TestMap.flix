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

mod TestMap {

    use ToString.toString
    use Hash.hash

/////////////////////////////////////////////////////////////////////////////
// size                                                                    //
/////////////////////////////////////////////////////////////////////////////
@Test
def size01(): Bool = Map.size(Map#{}) == 0

@Test
def size02(): Bool = Map.size(Map#{1 => 2}) == 1

@Test
def size03(): Bool = Map.size(Map#{1 => 2, 2 => 4}) == 2

@Test
def size04(): Bool = Map.size(Map#{1 => 2, 2 => 4, 3 => 6}) == 3

@Test
def size05(): Bool = Map.size(Map#{1 => 2, 2 => 4, 3 => 6, 4 => 8}) == 4

@Test
def size06(): Bool = Map.size(Map#{1 => 2, 2 => 4, 3 => 6, 4 => 8, 5 => 10}) == 5

/////////////////////////////////////////////////////////////////////////////
// empty                                                                   //
/////////////////////////////////////////////////////////////////////////////
@Test
def empty01(): Bool = Map.empty(): Map[Unit, Unit] == Map#{}

/////////////////////////////////////////////////////////////////////////////
// singleton                                                               //
/////////////////////////////////////////////////////////////////////////////
@Test
def singleton01(): Bool = Map.singleton(1, 2) == Map#{1 => 2}

@Test
def singleton02(): Bool = Map.singleton(3, -1) == Map#{3 => -1}

@Test
def singleton03(): Bool = Map.singleton(-99, -11) == Map#{-99 => -11}

/////////////////////////////////////////////////////////////////////////////
// isEmpty                                                                 //
/////////////////////////////////////////////////////////////////////////////
@Test
def isEmpty01(): Bool = Map.isEmpty(Map#{}: Map[Unit, Unit])

@Test
def isEmpty02(): Bool = not Map.isEmpty(Map#{1 => 2})

@Test
def isEmpty03(): Bool = not Map.isEmpty(Map#{1 => 2, 2 => 4})

@Test
def isEmpty04(): Bool = not Map.isEmpty(Map#{1 => 2, 2 => 4, 3 => 6})

@Test
def isEmpty05(): Bool = not Map.isEmpty(Map#{1 => 2, 2 => 4, 3 => 6, 4 => 8})

/////////////////////////////////////////////////////////////////////////////
// get                                                                     //
/////////////////////////////////////////////////////////////////////////////
@Test
def get01(): Bool = Map.get(2, Map#{}: Map[_, Unit]) == None

@Test
def get02(): Bool = Map.get(2, Map#{1 => 2}) == None

@Test
def get03(): Bool = Map.get(2, Map#{2 => 1}) == Some(1)

@Test
def get04(): Bool = Map.get(5, Map#{2 => 1, 3 => 17}) == None

@Test
def get05(): Bool = Map.get(5, Map#{2 => 1, 5 => 17}) == Some(17)

@Test
def get06(): Bool = Map.get(5, Map#{5 => 1, 3 => 17}) == Some(1)

@Test
def get07(): Bool = Map.get(-2, Map#{2 => 1, 3 => 17, -1 => -2}) == None

@Test
def get08(): Bool = Map.get(-2, Map#{-2 => 1, 3 => 17, -1 => -2}) == Some(1)

@Test
def get09(): Bool = Map.get(-2, Map#{2 => 1, -2 => 17, -1 => -2}) == Some(17)

@Test
def get10(): Bool = Map.get(-2, Map#{2 => 1, 3 => 17, -2 => -2}) == Some(-2)

/////////////////////////////////////////////////////////////////////////////
// getWithDefault                                                          //
/////////////////////////////////////////////////////////////////////////////
@Test
def getWithDefault01(): Bool = Map.getWithDefault(2, 34, Map#{}) == 34

@Test
def getWithDefault02(): Bool = Map.getWithDefault(2, 34, Map#{1 => 2}) == 34

@Test
def getWithDefault03(): Bool = Map.getWithDefault(2, 34, Map#{2 => 1}) == 1

@Test
def getWithDefault04(): Bool = Map.getWithDefault(5, 34, Map#{2 => 1, 3 => 17}) == 34

@Test
def getWithDefault05(): Bool = Map.getWithDefault(5, 34, Map#{2 => 1, 5 => 17}) == 17

@Test
def getWithDefault06(): Bool = Map.getWithDefault(5, 34, Map#{5 => 1, 3 => 17}) == 1

@Test
def getWithDefault07(): Bool = Map.getWithDefault(-2, 34, Map#{2 => 1, 3 => 17, -1 => -2}) == 34

@Test
def getWithDefault08(): Bool = Map.getWithDefault(-2, 34, Map#{-2 => 1, 3 => 17, -1 => -2}) == 1

@Test
def getWithDefault09(): Bool = Map.getWithDefault(-2, 34, Map#{2 => 1, -2 => 17, -1 => -2}) == 17

@Test
def getWithDefault10(): Bool = Map.getWithDefault(-2, 34, Map#{2 => 1, 3 => 17, -2 => -2}) == -2

/////////////////////////////////////////////////////////////////////////////
// memberOf                                                                //
/////////////////////////////////////////////////////////////////////////////
@Test
def memberOf01(): Bool = not Map.memberOf(2, Map#{}: Map[_, Unit])

@Test
def memberOf02(): Bool = not Map.memberOf(2, Map#{1 => 2})

@Test
def memberOf03(): Bool = Map.memberOf(2, Map#{2 => 1})

@Test
def memberOf04(): Bool = not Map.memberOf(5, Map#{2 => 1, 3 => 17})

@Test
def memberOf05(): Bool = Map.memberOf(5, Map#{2 => 1, 5 => 17})

@Test
def memberOf06(): Bool = Map.memberOf(5, Map#{5 => 1, 3 => 17})

@Test
def memberOf07(): Bool = not Map.memberOf(-2, Map#{2 => 1, 3 => 17, -1 => -2})

@Test
def memberOf08(): Bool = Map.memberOf(-2, Map#{-2 => 1, 3 => 17, -1 => -2})

@Test
def memberOf09(): Bool = Map.memberOf(-2, Map#{2 => 1, -2 => 17, -1 => -2})

@Test
def memberOf10(): Bool = Map.memberOf(-2, Map#{2 => 1, 3 => 17, -2 => -2})

/////////////////////////////////////////////////////////////////////////////
// keysOf                                                                  //
/////////////////////////////////////////////////////////////////////////////
@Test
def keysOf01(): Bool = Map.keysOf(Map#{}: Map[Unit, Unit]) == Set#{}

@Test
def keysOf02(): Bool = Map.keysOf(Map#{1 => 2}) == Set#{1}

@Test
def keysOf03(): Bool = Map.keysOf(Map#{1 => 2, 2 => 4}) == Set#{1, 2}

@Test
def keysOf04(): Bool = Map.keysOf(Map#{1 => 2, 2 => 4, 3 => 6}) == Set#{1, 2, 3}

@Test
def keysOf05(): Bool = Map.keysOf(Map#{1 => 2, 2 => 4, 3 => 6, 4 => 8}) == Set#{1, 2, 3, 4}

@Test
def keysOf06(): Bool = Map.keysOf(Map#{1 => 2, 2 => 4, 3 => 6, 4 => 8, 5 => 10}) == Set#{1, 2, 3, 4, 5}

/////////////////////////////////////////////////////////////////////////////
// valuesOf                                                                //
/////////////////////////////////////////////////////////////////////////////
@Test
def valuesOf01(): Bool = Map.valuesOf(Map#{}: Map[Unit, Unit]) == Nil

@Test
def valuesOf02(): Bool = Map.valuesOf(Map#{1 => 2}) == 2 :: Nil

@Test
def valuesOf03(): Bool = Map.valuesOf(Map#{1 => 2, 2 => 4}) == 2 :: 4 :: Nil

@Test
def valuesOf04(): Bool = Map.valuesOf(Map#{1 => 2, 2 => 4, 3 => 6}) == 2 :: 4 :: 6 :: Nil

@Test
def valuesOf05(): Bool = Map.valuesOf(Map#{1 => 2, 2 => 4, 3 => 6, 4 => 8}) == 2 :: 4 :: 6 :: 8 :: Nil

@Test
def valuesOf06(): Bool = Map.valuesOf(Map#{1 => 2, 2 => 4, 3 => 6, 4 => 8, 5 => 10}) == 2 :: 4 :: 6 :: 8 :: 10 :: Nil

@Test
def valuesOf07(): Bool = Map.valuesOf(Map#{1 => -11, 2 => 4, 3 => -5, 4 => 7, 5 => -5}) == -11 :: 4 :: -5 :: 7 :: -5 :: Nil

/////////////////////////////////////////////////////////////////////////////
// insert                                                                  //
/////////////////////////////////////////////////////////////////////////////
@Test
def insert01(): Bool = Map.insert(1, 3, Map#{}) == Map#{1 => 3}

@Test
def insert02(): Bool = Map.insert(1, 3, Map#{1 => 4}) == Map#{1 => 3}

@Test
def insert03(): Bool = Map.insert(2, 3, Map#{1 => 4}) == Map#{1 => 4, 2 => 3}

@Test
def insert04(): Bool = Map.insert(1, 1, Map#{1 => 4, 5 => -2}) == Map#{1 => 1, 5 => -2}

@Test
def insert05(): Bool = Map.insert(5, 1, Map#{1 => 4, 5 => -2}) == Map#{1 => 4, 5 => 1}

@Test
def insert06(): Bool = Map.insert(4, -2, Map#{1 => 4, 5 => -2}) == Map#{1 => 4, 5 => -2, 4 => -2}

/////////////////////////////////////////////////////////////////////////////
// insertWith                                                              //
/////////////////////////////////////////////////////////////////////////////
@Test
def insertWith01(): Bool = Map.insertWith((v1, v2) -> v1 + v2, 1, 3, Map#{}) == Map#{1 => 3}

@Test
def insertWith02(): Bool = Map.insertWith((v1, v2) -> v1 + v2, 1, 3, Map#{1 => 4}) == Map#{1 => 7}

@Test
def insertWith03(): Bool = Map.insertWith((v1, v2) -> v1 + v2, 2, 3, Map#{1 => 4}) == Map#{1 => 4, 2 => 3}

@Test
def insertWith04(): Bool = Map.insertWith((v1, v2) -> v1 + v2, 1, 1, Map#{1 => 4, 5 => -2}) == Map#{1 => 5, 5 => -2}

@Test
def insertWith05(): Bool = Map.insertWith((v1, v2) -> v1 + v2, 5, 1, Map#{1 => 4, 5 => -2}) == Map#{1 => 4, 5 => -1}

@Test
def insertWith06(): Bool = Map.insertWith((v1, v2) -> v1 + v2, 4, -2, Map#{1 => 4, 5 => -2}) == Map#{1 => 4, 5 => -2, 4 => -2}

/////////////////////////////////////////////////////////////////////////////
// insertWithKey                                                           //
/////////////////////////////////////////////////////////////////////////////
@Test
def insertWithKey01(): Bool = Map.insertWithKey((k, v1, v2) -> k + v1 + v2, 1, 3, Map#{}) == Map#{1 => 3}

@Test
def insertWithKey02(): Bool = Map.insertWithKey((k, v1, v2) -> k + v1 + v2, 1, 3, Map#{1 => 4}) == Map#{1 => 8}

@Test
def insertWithKey03(): Bool = Map.insertWithKey((k, v1, v2) -> k + v1 + v2, 2, 3, Map#{1 => 4}) == Map#{1 => 4, 2 => 3}

@Test
def insertWithKey04(): Bool = Map.insertWithKey((k, v1, v2) -> k + v1 + v2, 1, 1, Map#{1 => 4, 5 => -2}) == Map#{1 => 6, 5 => -2}

@Test
def insertWithKey05(): Bool = Map.insertWithKey((k, v1, v2) -> k + v1 + v2, 5, 1, Map#{1 => 4, 5 => -2}) == Map#{1 => 4, 5 => 4}

@Test
def insertWithKey06(): Bool = Map.insertWithKey((k, v1, v2) -> k + v1 + v2, 4, -2, Map#{1 => 4, 5 => -2}) == Map#{1 => 4, 5 => -2, 4 => -2}

/////////////////////////////////////////////////////////////////////////////
// adjust                                                                  //
/////////////////////////////////////////////////////////////////////////////
@Test
def adjust01(): Bool = Map.adjust(v -> 2*v, 1, Map#{}) == Map#{}

@Test
def adjust02(): Bool = Map.adjust(v -> 2*v, 1, Map#{1 => 4}) == Map#{1 => 8}

@Test
def adjust03(): Bool = Map.adjust(v -> 2*v, 2, Map#{1 => 4}) == Map#{1 => 4}

@Test
def adjust04(): Bool = Map.adjust(v -> 2*v, 1, Map#{1 => -14, 5 => -2}) == Map#{1 => -28, 5 => -2}

@Test
def adjust05(): Bool = Map.adjust(v -> 2*v, 5, Map#{1 => 4, 5 => -2}) == Map#{1 => 4, 5 => -4}

@Test
def adjust06(): Bool = Map.adjust(v -> 2*v, 4, Map#{1 => 4, 5 => -2}) == Map#{1 => 4, 5 => -2}

/////////////////////////////////////////////////////////////////////////////
// adjustWithKey                                                           //
/////////////////////////////////////////////////////////////////////////////
@Test
def adjustWithKey01(): Bool = Map.adjustWithKey((k, v) -> k+v, 1, Map#{}) == Map#{}

@Test
def adjustWithKey02(): Bool = Map.adjustWithKey((k, v) -> k+v, 1, Map#{1 => 4}) == Map#{1 => 5}

@Test
def adjustWithKey03(): Bool = Map.adjustWithKey((k, v) -> k+v, 2, Map#{1 => 4}) == Map#{1 => 4}

@Test
def adjustWithKey04(): Bool = Map.adjustWithKey((k, v) -> k+v, 1, Map#{1 => -14, 5 => -2}) == Map#{1 => -13, 5 => -2}

@Test
def adjustWithKey05(): Bool = Map.adjustWithKey((k, v) -> k+v, 5, Map#{1 => 4, 5 => -2}) == Map#{1 => 4, 5 => 3}

@Test
def adjustWithKey06(): Bool = Map.adjustWithKey((k, v) -> k+v, 4, Map#{1 => 4, 5 => -2}) == Map#{1 => 4, 5 => -2}

/////////////////////////////////////////////////////////////////////////////
// update                                                                  //
/////////////////////////////////////////////////////////////////////////////
@Test
def update01(): Bool = Map.update(v -> if (v `Int32.rem` 2 != 0) Some(2*v) else None, 1, Map#{}) == Map#{}

@Test
def update02(): Bool = Map.update(v -> if (v `Int32.rem` 2 != 0) Some(2*v) else None, 1, Map#{1 => 3}) == Map#{1 => 6}

@Test
def update03(): Bool = Map.update(v -> if (v `Int32.rem` 2 != 0) Some(2*v) else None, 1, Map#{1 => 4}) == Map#{1 => 4}

@Test
def update04(): Bool = Map.update(v -> if (v `Int32.rem` 2 != 0) Some(2*v) else None, 2, Map#{1 => 4}) == Map#{1 => 4}

@Test
def update05(): Bool = Map.update(v -> if (v `Int32.rem` 2 != 0) Some(2*v) else None, 1, Map#{1 => -14, 5 => -2}) == Map#{1 => -14, 5 => -2}

@Test
def update06(): Bool = Map.update(v -> if (v `Int32.rem` 2 != 0) Some(2*v) else None, 1, Map#{1 => -13, 5 => -2}) == Map#{1 => -26, 5 => -2}

@Test
def update07(): Bool = Map.update(v -> if (v `Int32.rem` 2 != 0) Some(2*v) else None, 5, Map#{1 => 4, 5 => -2}) == Map#{1 => 4, 5 => -2}

@Test
def update08(): Bool = Map.update(v -> if (v `Int32.rem` 2 != 0) Some(2*v) else None, 5, Map#{1 => 4, 5 => -1}) == Map#{1 => 4, 5 => -2}

@Test
def update09(): Bool = Map.update(v -> if (v `Int32.rem` 2 != 0) Some(2*v) else None, 4, Map#{1 => 4, 5 => -2}) == Map#{1 => 4, 5 => -2}

/////////////////////////////////////////////////////////////////////////////
// updateWithKey                                                           //
/////////////////////////////////////////////////////////////////////////////
@Test
def updateWithKey01(): Bool = Map.updateWithKey((k, v) -> if (v `Int32.rem` 2 != 0) Some(k+2*v) else None, 1, Map#{}) == Map#{}

@Test
def updateWithKey02(): Bool = Map.updateWithKey((k, v) -> if (v `Int32.rem` 2 != 0) Some(k+2*v) else None, 1, Map#{1 => 3}) == Map#{1 => 7}

@Test
def updateWithKey03(): Bool = Map.updateWithKey((k, v) -> if (v `Int32.rem` 2 != 0) Some(k+2*v) else None, 1, Map#{1 => 4}) == Map#{1 => 4}

@Test
def updateWithKey04(): Bool = Map.updateWithKey((k, v) -> if (v `Int32.rem` 2 != 0) Some(k+2*v) else None, 2, Map#{1 => 4}) == Map#{1 => 4}

@Test
def updateWithKey05(): Bool = Map.updateWithKey((k, v) -> if (v `Int32.rem` 2 != 0) Some(k+2*v) else None, 1, Map#{1 => -14, 5 => -2}) == Map#{1 => -14, 5 => -2}

@Test
def updateWithKey06(): Bool = Map.updateWithKey((k, v) -> if (v `Int32.rem` 2 != 0) Some(k+2*v) else None, 1, Map#{1 => -13, 5 => -2}) == Map#{1 => -25, 5 => -2}

@Test
def updateWithKey07(): Bool = Map.updateWithKey((k, v) -> if (v `Int32.rem` 2 != 0) Some(k+2*v) else None, 5, Map#{1 => 4, 5 => -2}) == Map#{1 => 4, 5 => -2}

@Test
def updateWithKey08(): Bool = Map.updateWithKey((k, v) -> if (v `Int32.rem` 2 != 0) Some(k+2*v) else None, 5, Map#{1 => 4, 5 => -1}) == Map#{1 => 4, 5 => 3}

@Test
def updateWithKey09(): Bool = Map.updateWithKey((k, v) -> if (v `Int32.rem` 2 != 0) Some(k+2*v) else None, 4, Map#{1 => 4, 5 => -2}) == Map#{1 => 4, 5 => -2}

/////////////////////////////////////////////////////////////////////////////
// remove                                                                  //
/////////////////////////////////////////////////////////////////////////////
@Test
def remove01(): Bool = Map.remove(1, Map#{}: Map[_, Unit]) == Map#{}

@Test
def remove02(): Bool = Map.remove(1, Map#{2 => 4}) == Map#{2 => 4}

@Test
def remove03(): Bool = Map.remove(1, Map#{1 => 4}) == Map#{}

@Test
def remove04(): Bool = Map.remove(1, Map#{6 => 1, 2 => 4}) == Map#{6 => 1, 2 => 4}

@Test
def remove05(): Bool = Map.remove(6, Map#{6 => 1, 2 => 4}) == Map#{2 => 4}

@Test
def remove06(): Bool = Map.remove(2, Map#{6 => 1, 2 => 4}) == Map#{6 => 1}

@Test
def remove07(): Bool = Map.remove(1, Map#{6 => 1, 2 => 4, 8 => 89}) == Map#{6 => 1, 2 => 4, 8 => 89}

@Test
def remove08(): Bool = Map.remove(6, Map#{6 => 1, 2 => 4, 8 => 89}) == Map#{2 => 4, 8 => 89}

@Test
def remove09(): Bool = Map.remove(2, Map#{6 => 1, 2 => 4, 8 => 89}) == Map#{6 => 1, 8 => 89}

@Test
def remove10(): Bool = Map.remove(8, Map#{6 => 1, 2 => 4, 8 => 89}) == Map#{6 => 1, 2 => 4}

/////////////////////////////////////////////////////////////////////////////
// isSubmapOf                                                              //
/////////////////////////////////////////////////////////////////////////////
@Test
def isSubmapOf01(): Bool = Map.isSubmapOf(Map#{}: Map[Unit, Unit], Map#{})

@Test
def isSubmapOf02(): Bool = not Map.isSubmapOf(Map#{1 => 2}, Map#{})

@Test
def isSubmapOf03(): Bool = not Map.isSubmapOf(Map#{3 => 4}, Map#{})

@Test
def isSubmapOf04(): Bool = Map.isSubmapOf(Map#{1 => 3}, Map#{1 => 3})

@Test
def isSubmapOf05(): Bool = not Map.isSubmapOf(Map#{2 => 3}, Map#{1 => 3})

@Test
def isSubmapOf06(): Bool = not Map.isSubmapOf(Map#{1 => 4}, Map#{1 => 3})

@Test
def isSubmapOf07(): Bool = not Map.isSubmapOf(Map#{1 => 3, 2 => 3}, Map#{1 => 3})

@Test
def isSubmapOf08(): Bool = not Map.isSubmapOf(Map#{1 => 3, 2 => 3}, Map#{2 => 3})

@Test
def isSubmapOf09(): Bool = Map.isSubmapOf(Map#{}, Map#{2 => 4, 1 => 3})

@Test
def isSubmapOf10(): Bool = Map.isSubmapOf(Map#{2 => 4}, Map#{2 => 4, 1 => 3})

@Test
def isSubmapOf11(): Bool = Map.isSubmapOf(Map#{1 => 3}, Map#{2 => 4, 1 => 3})

@Test
def isSubmapOf12(): Bool = Map.isSubmapOf(Map#{1 => 3, 2 => 4}, Map#{1 => 3, 2 => 4})

@Test
def isSubmapOf13(): Bool = Map.isSubmapOf(Map#{2 => 4, 1 => 3}, Map#{1 => 3, 2 => 4})

@Test
def isSubmapOf14(): Bool = not Map.isSubmapOf(Map#{2 => 5, 1 => 3}, Map#{1 => 3, 2 => 4})

@Test
def isSubmapOf15(): Bool = not Map.isSubmapOf(Map#{2 => 4, 1 => -1}, Map#{1 => 3, 2 => 4})

@Test
def isSubmapOf16(): Bool = not Map.isSubmapOf(Map#{3 => 4, 1 => 3}, Map#{1 => 3, 2 => 4})

@Test
def isSubmapOf17(): Bool = not Map.isSubmapOf(Map#{2 => 4, 3 => 3}, Map#{1 => 3, 2 => 4})

@Test
def isSubmapOf18(): Bool = not Map.isSubmapOf(Map#{2 => 4, 1 => 3, 8 => 9}, Map#{1 => 3, 2 => 4})

/////////////////////////////////////////////////////////////////////////////
// isProperSubmapOf                                                        //
/////////////////////////////////////////////////////////////////////////////
@Test
def isProperSubmapOf01(): Bool = not Map.isProperSubmapOf(Map#{}: Map[Unit, Unit], Map#{})

@Test
def isProperSubmapOf02(): Bool = not Map.isProperSubmapOf(Map#{1 => 2}, Map#{})

@Test
def isProperSubmapOf03(): Bool = not Map.isProperSubmapOf(Map#{3 => 4}, Map#{})

@Test
def isProperSubmapOf04(): Bool = not Map.isProperSubmapOf(Map#{1 => 3}, Map#{1 => 3})

@Test
def isProperSubmapOf05(): Bool = not Map.isProperSubmapOf(Map#{2 => 3}, Map#{1 => 3})

@Test
def isProperSubmapOf06(): Bool = not Map.isProperSubmapOf(Map#{1 => 4}, Map#{1 => 3})

@Test
def isProperSubmapOf07(): Bool = not Map.isProperSubmapOf(Map#{1 => 3, 2 => 3}, Map#{1 => 3})

@Test
def isProperSubmapOf08(): Bool = not Map.isProperSubmapOf(Map#{1 => 3, 2 => 3}, Map#{2 => 3})

@Test
def isProperSubmapOf09(): Bool = Map.isProperSubmapOf(Map#{}, Map#{2 => 4, 1 => 3})

@Test
def isProperSubmapOf10(): Bool = Map.isProperSubmapOf(Map#{2 => 4}, Map#{2 => 4, 1 => 3})

@Test
def isProperSubmapOf11(): Bool = Map.isProperSubmapOf(Map#{1 => 3}, Map#{2 => 4, 1 => 3})

@Test
def isProperSubmapOf12(): Bool = not Map.isProperSubmapOf(Map#{1 => 3, 2 => 4}, Map#{1 => 3, 2 => 4})

@Test
def isProperSubmapOf13(): Bool = not Map.isProperSubmapOf(Map#{2 => 4, 1 => 3}, Map#{1 => 3, 2 => 4})

@Test
def isProperSubmapOf14(): Bool = not Map.isProperSubmapOf(Map#{2 => 5, 1 => 3}, Map#{1 => 3, 2 => 4})

@Test
def isProperSubmapOf15(): Bool = not Map.isProperSubmapOf(Map#{2 => 4, 1 => -1}, Map#{1 => 3, 2 => 4})

@Test
def isProperSubmapOf16(): Bool = not Map.isProperSubmapOf(Map#{3 => 4, 1 => 3}, Map#{1 => 3, 2 => 4})

@Test
def isProperSubmapOf17(): Bool = not Map.isProperSubmapOf(Map#{2 => 4, 3 => 3}, Map#{1 => 3, 2 => 4})

@Test
def isProperSubmapOf18(): Bool = not Map.isProperSubmapOf(Map#{2 => 4, 1 => 3, 8 => 9}, Map#{1 => 3, 2 => 4})

/////////////////////////////////////////////////////////////////////////////
// find                                                                    //
/////////////////////////////////////////////////////////////////////////////
@Test
def find01(): Bool = Map.find((k, v) -> k == v, Map#{}: Map[Unit, Unit]) == None

@Test
def find02(): Bool = Map.find((k, v) -> k == v, Map#{1 => 2}) == None

@Test
def find03(): Bool = Map.find((k, v) -> k == v, Map#{1 => 1}) == Some((1, 1))

@Test
def find04(): Bool = Map.find((k, v) -> k == v, Map#{1 => 2, 2 => 3}) == None

@Test
def find05(): Bool = Map.find((k, v) -> k == v, Map#{1 => 1, 2 => 3}) == Some((1, 1))

@Test
def find06(): Bool = Map.find((k, v) -> k == v, Map#{1 => 2, 2 => 2}) == Some((2, 2))

@Test
def find07(): Bool = Map.find((k, v) -> k == v, Map#{1 => 1, 2 => 2}) == Some((1, 1))

/////////////////////////////////////////////////////////////////////////////
// findLeft                                                                //
/////////////////////////////////////////////////////////////////////////////
@Test
def findLeft01(): Bool = Map.findLeft((k, v) -> k == v, Map#{}: Map[Unit, Unit]) == None

@Test
def findLeft02(): Bool = Map.findLeft((k, v) -> k == v, Map#{1 => 2}) == None

@Test
def findLeft03(): Bool = Map.findLeft((k, v) -> k == v, Map#{1 => 1}) == Some((1, 1))

@Test
def findLeft04(): Bool = Map.findLeft((k, v) -> k == v, Map#{1 => 2, 2 => 3}) == None

@Test
def findLeft05(): Bool = Map.findLeft((k, v) -> k == v, Map#{1 => 1, 2 => 3}) == Some((1, 1))

@Test
def findLeft06(): Bool = Map.findLeft((k, v) -> k == v, Map#{1 => 2, 2 => 2}) == Some((2, 2))

@Test
def findLeft07(): Bool = Map.findLeft((k, v) -> k == v, Map#{1 => 1, 2 => 2}) == Some((1, 1))

/////////////////////////////////////////////////////////////////////////////
// findRight                                                               //
/////////////////////////////////////////////////////////////////////////////
@Test
def findRight01(): Bool = Map.findRight((k, v) -> k == v, Map#{}: Map[Unit, Unit]) == None

@Test
def findRight02(): Bool = Map.findRight((k, v) -> k == v, Map#{1 => 2}) == None

@Test
def findRight03(): Bool = Map.findRight((k, v) -> k == v, Map#{1 => 1}) == Some((1, 1))

@Test
def findRight04(): Bool = Map.findRight((k, v) -> k == v, Map#{1 => 2, 2 => 3}) == None

@Test
def findRight05(): Bool = Map.findRight((k, v) -> k == v, Map#{1 => 1, 2 => 3}) == Some((1, 1))

@Test
def findRight06(): Bool = Map.findRight((k, v) -> k == v, Map#{1 => 2, 2 => 2}) == Some((2, 2))

@Test
def findRight07(): Bool = Map.findRight((k, v) -> k == v, Map#{1 => 1, 2 => 2}) == Some((2, 2))

/////////////////////////////////////////////////////////////////////////////
// filter                                                                  //
/////////////////////////////////////////////////////////////////////////////
@Test
def filter01(): Bool = Map.filter(v -> v `Int32.rem` 2 == 0, Map#{}: Map[Unit, _]) == Map#{}

@Test
def filter02(): Bool = Map.filter(v -> v `Int32.rem` 2 == 0, Map#{1 => 2}) == Map#{1 => 2}

@Test
def filter03(): Bool = Map.filter(v -> v `Int32.rem` 2 == 0, Map#{1 => 3}) == Map#{}

@Test
def filter04(): Bool = Map.filter(v -> v `Int32.rem` 2 == 0, Map#{1 => 8, 2 => 4}) == Map#{1 => 8, 2 => 4}

@Test
def filter05(): Bool = Map.filter(v -> v `Int32.rem` 2 == 0, Map#{1 => 7, 2 => 4}) == Map#{2 => 4}

@Test
def filter06(): Bool = Map.filter(v -> v `Int32.rem` 2 == 0, Map#{1 => 8, 2 => 3}) == Map#{1 => 8}

@Test
def filter07(): Bool = Map.filter(v -> v `Int32.rem` 2 == 0, Map#{1 => -1, 2 => -3}) == Map#{}

/////////////////////////////////////////////////////////////////////////////
// filterWithKey                                                           //
/////////////////////////////////////////////////////////////////////////////
@Test
def filterWithKey01(): Bool = Map.filterWithKey((k, v) -> k == v, Map#{}: Map[Unit, Unit]) == Map#{}

@Test
def filterWithKey02(): Bool = Map.filterWithKey((k, v) -> k == v, Map#{1 => 1}) == Map#{1 => 1}

@Test
def filterWithKey03(): Bool = Map.filterWithKey((k, v) -> k == v, Map#{1 => 3}) == Map#{}

@Test
def filterWithKey04(): Bool = Map.filterWithKey((k, v) -> k == v, Map#{1 => 1, 2 => 2}) == Map#{1 => 1, 2 => 2}

@Test
def filterWithKey05(): Bool = Map.filterWithKey((k, v) -> k == v, Map#{1 => 7, 2 => 2}) == Map#{2 => 2}

@Test
def filterWithKey06(): Bool = Map.filterWithKey((k, v) -> k == v, Map#{1 => 1, 2 => 3}) == Map#{1 => 1}

@Test
def filterWithKey07(): Bool = Map.filterWithKey((k, v) -> k == v, Map#{1 => -1, 2 => -3}) == Map#{}

/////////////////////////////////////////////////////////////////////////////
// filterMap                                                               //
/////////////////////////////////////////////////////////////////////////////

@Test
def filterMap01(): Bool = Map.filterMap(x -> if (x > 1) Some("${x}") else None, Map#{}: Map[Unit, _]) == Map#{}

@Test
def filterMap02(): Bool = Map.filterMap(x -> if (x > 1) Some("${x}") else None, Map#{1 => 1}) == Map#{}

@Test
def filterMap03(): Bool =
    Map.filterMap(x -> if (x > 1) Some("${x}") else None, Map#{1 => 1, 2 => 2, 3 => 3, 4 => 4})
        == Map#{2 => "2", 3 => "3", 4 => "4"}

/////////////////////////////////////////////////////////////////////////////
// filterMapWithKey                                                        //
/////////////////////////////////////////////////////////////////////////////

@Test
def filterMapWithKey01(): Bool = Map.filterMapWithKey((k, v) -> if (k == v and v > 1) Some("${v}") else None, Map#{}: Map[Int32, Int32]) == Map#{}

@Test
def filterMapWithKey02(): Bool = Map.filterMapWithKey((k, v) -> if (k == v and v > 1) Some("${v}") else None, Map#{1 => 1}) == Map#{}

@Test
def filterMapWithKey03(): Bool =
    Map.filterMapWithKey((k, v) -> if (k == v and v > 1) Some("${v}") else None, Map#{1 => 1, 2 => 2, 3 => 3, 4 => 4})
        == Map#{2 => "2", 3 => "3", 4 => "4"}

/////////////////////////////////////////////////////////////////////////////
// map                                                                     //
/////////////////////////////////////////////////////////////////////////////
@Test
def map01(): Bool = Map.map(v -> 3*v, Map#{}: Map[Unit, Int32]) == Map#{}

@Test
def map02(): Bool = Map.map(v -> 3*v, Map#{1 => 4}) == Map#{1 => 12}

@Test
def map03(): Bool = Map.map(v -> 3*v, Map#{2 => -1, 0 => 0}) == Map#{2 => -3, 0 => 0}

@Test
def map04(): Bool = Map.map(v -> 3*v, Map#{2 => -1, 5 => 15, 11 => -9}) == Map#{2 => -3, 5 => 45, 11 => -27}

@Test
def map05(): Bool = Map.map(v -> 3*v, Map#{2 => -1, 5 => 15, 11 => -9, 8 => 8}) == Map#{2 => -3, 5 => 45, 11 => -27, 8 => 24}

/////////////////////////////////////////////////////////////////////////////
// mapWithKey                                                              //
/////////////////////////////////////////////////////////////////////////////
@Test
def mapWithKey01(): Bool = Map.mapWithKey((_, v) -> v, Map#{}: Map[Unit, Unit]) == Map#{}

@Test
def mapWithKey02(): Bool = Map.mapWithKey((k, v) -> k + v, Map#{1 => 4}) == Map#{1 => 5}

@Test
def mapWithKey03(): Bool = Map.mapWithKey((k, v) -> k + v, Map#{2 => -1, 0 => 0}) == Map#{2 => 1, 0 => 0}

@Test
def mapWithKey04(): Bool = Map.mapWithKey((k, v) -> k + v, Map#{2 => -1, 5 => 15, 11 => -9}) == Map#{2 => 1, 5 => 20, 11 => 2}

@Test
def mapWithKey05(): Bool = Map.mapWithKey((k, v) -> k + v, Map#{2 => -1, 5 => 15, 11 => -9, 8 => 8})
                                        == Map#{2 => 1, 5 => 20, 11 => 2, 8 => 16}

@Test
def mapWithKey06(): Bool = region rc {
    let range = List.range(0, 2000);
    let m = List.zip(range, range) |> List.toMap;
    let a = MutList.new(rc);
    discard Map.mapWithKey((k, v) -> { let b = MutList.new(rc); MutList.push!(k, b); MutList.append!(MutList.toList(b), a); v }, m);
    MutList.toList(a) == range
}


/////////////////////////////////////////////////////////////////////////////
// foldWithKey                                                             //
/////////////////////////////////////////////////////////////////////////////
@Test
def foldWithKey01(): Bool = Map.foldWithKey((k, s, v) -> k + s + v, 0, Map#{}) == 0

@Test
def foldWithKey02(): Bool = Map.foldWithKey((k, s, v) -> k + s + v, 0, Map#{1 => 2}) == 3

@Test
def foldWithKey03(): Bool = Map.foldWithKey((k, s, v) -> k + s + v, 0, Map#{1 => 2, 3 => 4}) == 10

@Test
def foldWithKey04(): Bool = Map.foldWithKey((k, s, v) -> k + s + v, 0, Map#{1 => 2, 3 => 4, 5 => 6}) == 21

/////////////////////////////////////////////////////////////////////////////
// foldLeft                                                                //
/////////////////////////////////////////////////////////////////////////////
@Test
def foldLeft01(): Bool = Map.foldLeft((s, v) -> s + v, 0, Map#{}) == 0

@Test
def foldLeft02(): Bool = Map.foldLeft((s, v) -> s + v, 0, Map#{1 => 2}) == 2

@Test
def foldLeft03(): Bool = Map.foldLeft((s, v) -> s + v, 0, Map#{1 => 2, 3 => 4}) == 6

@Test
def foldLeft04(): Bool = Map.foldLeft((s, v) -> s + v, 0, Map#{1 => 2, 3 => 4, 5 => 6}) == 12

/////////////////////////////////////////////////////////////////////////////
// foldLeftWithKey                                                         //
/////////////////////////////////////////////////////////////////////////////
@Test
def foldLeftWithKey01(): Bool = Map.foldLeftWithKey((s, k, v) -> k + s + v, 0, Map#{}) == 0

@Test
def foldLeftWithKey02(): Bool = Map.foldLeftWithKey((s, k, v) -> k + s + v, 0, Map#{1 => 2}) == 3

@Test
def foldLeftWithKey03(): Bool = Map.foldLeftWithKey((s, k, v) -> k + s + v, 0, Map#{1 => 2, 3 => 4}) == 10

@Test
def foldLeftWithKey04(): Bool = Map.foldLeftWithKey((s, k, v) -> k + s + v, 0, Map#{1 => 2, 3 => 4, 5 => 6}) == 21

/////////////////////////////////////////////////////////////////////////////
// foldRight                                                               //
/////////////////////////////////////////////////////////////////////////////
@Test
def foldRight01(): Bool = Map.foldRight((v, acc) -> acc + v, 0, Map#{}) == 0

@Test
def foldRight02(): Bool = Map.foldRight((v, acc) -> acc + v, 0, Map#{1 => 2}) == 2

@Test
def foldRight03(): Bool = Map.foldRight((v, acc) -> acc + v, 0, Map#{1 => 2, 3 => 4}) == 6

@Test
def foldRight04(): Bool = Map.foldRight((v, acc) -> acc + v, 0, Map#{1 => 2, 3 => 4, 5 => 6}) == 12

/////////////////////////////////////////////////////////////////////////////
// foldRightWithKey                                                        //
/////////////////////////////////////////////////////////////////////////////
@Test
def foldRightWithKey01(): Bool = Map.foldRightWithKey((k, v, acc) -> k + acc + v, 0, Map#{}) == 0

@Test
def foldRightWithKey02(): Bool = Map.foldRightWithKey((k, v, acc) -> k + acc + v, 0, Map#{1 => 2}) == 3

@Test
def foldRightWithKey03(): Bool = Map.foldRightWithKey((k, v, acc) -> k + acc + v, 0, Map#{1 => 2, 3 => 4}) == 10

@Test
def foldRightWithKey04(): Bool = Map.foldRightWithKey((k, v, acc) -> k + acc + v, 0, Map#{1 => 2, 3 => 4, 5 => 6}) == 21

/////////////////////////////////////////////////////////////////////////////
// foldRightWithCont                                                       //
/////////////////////////////////////////////////////////////////////////////
@Test
def foldRightWithCont01(): Bool = Map.foldRightWithCont((v, k) -> k() + v, 0, Map#{}) == 0

@Test
def foldRightWithCont02(): Bool = Map.foldRightWithCont((v, k) -> k() + v, 0, Map#{1 => 2}) == 2

@Test
def foldRightWithCont03(): Bool = Map.foldRightWithCont((v, k) -> k() + v, 0, Map#{1 => 2, 3 => 4}) == 6

@Test
def foldRightWithCont04(): Bool = Map.foldRightWithCont((v, k) -> k() + v, 0, Map#{1 => 2, 3 => 4, 5 => 6}) == 12

/////////////////////////////////////////////////////////////////////////////
// foldRightWithKeyCont                                                    //
/////////////////////////////////////////////////////////////////////////////
@Test
def foldRightWithKeyCont01(): Bool = Map.foldRightWithKeyCont((k, v, c) -> k + c() + v, 0, Map#{}) == 0

@Test
def foldRightWithKeyCont02(): Bool = Map.foldRightWithKeyCont((k, v, c) -> k + c() + v, 0, Map#{1 => 2}) == 3

@Test
def foldRightWithKeyCont03(): Bool = Map.foldRightWithKeyCont((k, v, c) -> k + c() + v, 0, Map#{1 => 2, 3 => 4}) == 10

@Test
def foldRightWithKeyCont04(): Bool = Map.foldRightWithKeyCont((k, v, c) -> k + c() + v, 0, Map#{1 => 2, 3 => 4, 5 => 6}) == 21

/////////////////////////////////////////////////////////////////////////////
// foldMapWithKey                                                          //
/////////////////////////////////////////////////////////////////////////////

@Test
def foldMapWithKey01(): Bool = Map.foldMapWithKey((k, v) -> k + v, Map#{}) == 0

@Test
def foldMapWithKey02(): Bool = Map.foldMapWithKey((k, v) -> k + v, Map#{1 => 2, 3 => 5}) == (1 + 2 + 3 + 5)

@Test
def foldMapWithKey03(): Bool =
    Map.foldMapWithKey((k, v) -> 2 * (k + v), Map#{1 => 2, 3 => 5}) == (2 * (1 + 2) + 2 * (3 + 5))

@Test
def foldMapWithKey04(): Bool = Map.foldMapWithKey((k, v) -> "${k}" + v, Map#{1 => "a", 2 => "b"}) == "1a2b"

/////////////////////////////////////////////////////////////////////////////
// foldMap                                                                 //
/////////////////////////////////////////////////////////////////////////////

@Test
def foldMap01(): Bool =
    Map.foldMap(v -> v + 1, Map#{}) == 0

@Test
def foldMap02(): Bool =
    Map.foldMap(v -> v + 1, Map#{1 => 2, 3 => 5}) == (2 + 1) + (5 + 1)

@Test
def foldMap03(): Bool =
    Map.foldMap(v -> 2 * v, Map#{1 => 2, 3 => 5}) == (2 * 2) + (2 * 5)

@Test
def foldMap04(): Bool =
    Map.foldMap(v -> "x" + v, Map#{1 => "a", 2 => "b"}) == "xaxb"

/////////////////////////////////////////////////////////////////////////////
// reduceLeft                                                              //
/////////////////////////////////////////////////////////////////////////////
@Test
def reduceLeft01(): Bool = Map.reduceLeft((v1, v2) -> v1 - v2, Map#{}: Map[Int32, Int32]) == None

@Test
def reduceLeft02(): Bool = Map.reduceLeft((v1, v2) -> v1 - v2, Map#{1 => 2}) == Some(2)

@Test
def reduceLeft03(): Bool = Map.reduceLeft((v1, v2) -> v1 - v2, Map#{1 => 2, 2 => 3}) == Some(-1)

@Test
def reduceLeft04(): Bool = Map.reduceLeft((v1, v2) -> v1 - v2, Map#{1 => 2, 2 => 3, 3 => 4}) == Some(-5)

/////////////////////////////////////////////////////////////////////////////
// reduceLeftWithKey                                                       //
/////////////////////////////////////////////////////////////////////////////
@Test
def reduceLeftWithKey01(): Bool = Map.reduceLeftWithKey((k1, v1, k2, v2) -> (k1 - k2, v1 - v2), Map#{}: Map[Int32, Int32]) == None

@Test
def reduceLeftWithKey02(): Bool = Map.reduceLeftWithKey((k1, v1, k2, v2) -> (k1 - k2, v1 - v2), Map#{1 => 2}) == Some((1, 2))

@Test
def reduceLeftWithKey03(): Bool = Map.reduceLeftWithKey((k1, v1, k2, v2) -> (k1 - k2, v1 - v2), Map#{1 => 2, 2 => 3}) == Some((-1, -1))

@Test
def reduceLeftWithKey04(): Bool = Map.reduceLeftWithKey((k1, v1, k2, v2) -> (k1 - k2, v1 - v2), Map#{1 => 2, 2 => 3, 3 => 4}) == Some((-4, -5))

/////////////////////////////////////////////////////////////////////////////
// reduceRight                                                             //
/////////////////////////////////////////////////////////////////////////////
@Test
def reduceRight01(): Bool = Map.reduceRight((v1, v2) -> v1 - v2, Map#{}: Map[Int32, Int32]) == None

@Test
def reduceRight02(): Bool = Map.reduceRight((v1, v2) -> v1 - v2, Map#{1 => 2}) == Some(2)

@Test
def reduceRight03(): Bool = Map.reduceRight((v1, v2) -> v1 - v2, Map#{1 => 2, 2 => 3}) == Some(-1)

@Test
def reduceRight04(): Bool = Map.reduceRight((v1, v2) -> v1 - v2, Map#{1 => 2, 2 => 3, 3 => 4}) == Some(3)

/////////////////////////////////////////////////////////////////////////////
// reduceRightWithKey                                                      //
/////////////////////////////////////////////////////////////////////////////
@Test
def reduceRightWithKey01(): Bool = Map.reduceRightWithKey((k1, v1, k2, v2) -> (k1 - k2, v1 - v2), Map#{}: Map[Int32, Int32]) == None

@Test
def reduceRightWithKey02(): Bool = Map.reduceRightWithKey((k1, v1, k2, v2) -> (k1 - k2, v1 - v2), Map#{1 => 2}) == Some((1, 2))

@Test
def reduceRightWithKey03(): Bool = Map.reduceRightWithKey((k1, v1, k2, v2) -> (k1 - k2, v1 - v2), Map#{1 => 2, 2 => 3}) == Some((-1, -1))

@Test
def reduceRightWithKey04(): Bool = Map.reduceRightWithKey((k1, v1, k2, v2) -> (k1 - k2, v1 - v2), Map#{1 => 2, 2 => 3, 3 => 4}) == Some((2 , 3))

/////////////////////////////////////////////////////////////////////////////
// count                                                                   //
/////////////////////////////////////////////////////////////////////////////
@Test
def count01(): Bool = Map.count((k, v) -> k == v, Map#{}: Map[Unit, Unit]) == 0

@Test
def count02(): Bool = Map.count((k, v) -> k == v, Map#{1 => 2}) == 0

@Test
def count03(): Bool = Map.count((k, v) -> k == v, Map#{1 => 1}) == 1

@Test
def count04(): Bool = Map.count((k, v) -> k == v, Map#{1 => 2, 2 => 3}) == 0

@Test
def count05(): Bool = Map.count((k, v) -> k == v, Map#{1 => 1, 2 => 3}) == 1

@Test
def count06(): Bool = Map.count((k, v) -> k == v, Map#{1 => 2, 2 => 2}) == 1

@Test
def count07(): Bool = Map.count((k, v) -> k == v, Map#{1 => 1, 2 => 2}) == 2

@Test
def count08(): Bool = region rc {
    let range = List.range(0, 2000);
    let m = List.zip(range, range) |> List.toMap;
    let a = MutList.new(rc);
    discard Map.count((k, _) -> { let b = MutList.new(rc); MutList.push!(k, b); MutList.append!(MutList.toList(b), a); true }, m);
    MutList.toList(a) == range
}


    /////////////////////////////////////////////////////////////////////////////
    // sumValues                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sumValues01(): Bool =
        Map.empty() |> Map.sumValues == 0

    @Test
    def sumValues02(): Bool =
        Map#{1 => 1} |>
            Map.sumValues == 1

    @Test
    def sumValues03(): Bool =
        Map#{1 => 1, 2 => 2, 3 => 3} |>
            Map.sumValues == 6

    @Test
    def sumValues04(): Bool =
        Map#{1 => 1, 2 => 2, 3 => 3, -3 => -3} |>
            Map.sumValues == 3

    @Test
    def sumValues05(): Bool =
        Map#{-1 => -1, -2 => -2, -3 => -3, -4 => -4} |>
            Map.sumValues == -10

    @Test
    def sumValues06(): Bool =
        Map#{10 => 10, -10 => -10} |>
            Map.sumValues == 0

    @Test
    def sumValues07(): Bool =
        List.range(1, 101) |> List.zip(List.range(1, 101)) |> List.toMap |>
            Map.sumValues == 5050


    /////////////////////////////////////////////////////////////////////////////
    // sumKeys                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sumKeys01(): Bool =
        Map.empty() |> Map.sumKeys == 0

    @Test
    def sumKeys02(): Bool =
        Map#{1 => 1} |>
            Map.sumKeys == 1

    @Test
    def sumKeys03(): Bool =
        Map#{1 => 1, 2 => 2, 3 => 3} |>
            Map.sumKeys == 6

    @Test
    def sumKeys04(): Bool =
        Map#{1 => 1, 2 => 2, 3 => 3, -3 => -3} |>
            Map.sumKeys == 3

    @Test
    def sumKeys05(): Bool =
        Map#{-1 => -1, -2 => -2, -5 => -3, -4 => -4} |>
            Map.sumKeys == -12

    @Test
    def sumKeys06(): Bool =
        Map#{10 => 10, -10 => -10} |>
            Map.sumKeys == 0

    @Test
    def sumKeys07(): Bool =
        List.range(1, 101) |> List.zip(List.range(1, 101)) |> List.toMap |>
            Map.sumKeys == 5050


    /////////////////////////////////////////////////////////////////////////////
    // sumWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sumWith01(): Bool =
        Map.empty() |> Map.sumWith((k, v) -> k + v) == 0

    @Test
    def sumWith02(): Bool =
        Map#{1 => 1} |>
            Map.sumWith((k, v) -> k + v) == 2

    @Test
    def sumWith03(): Bool =
        Map#{1 => 1, 2 => 2, 3 => 3} |>
            Map.sumWith((k, v) -> k + v) == 12

    @Test
    def sumWith04(): Bool =
        Map#{1 => 1, 2 => 2, 3 => 3, -3 => -3} |>
            Map.sumWith((k, v) -> k + v) == 6

    @Test
    def sumWith05(): Bool =
        Map#{-1 => -1, -2 => -2, -3 => -3, -4 => -4} |>
            Map.sumWith((k, v) -> k + v) == -20

    @Test
    def sumWith06(): Bool =
        Map#{10 => 10, -10 => -10} |>
            Map.sumWith((k, v) -> k + v) == 0

    @Test
    def sumWith07(): Bool = region rc {
        let range = List.range(0, 2000);
        let m = List.zip(range, range) |> List.toMap;
        let a = MutList.new(rc);
        discard Map.sumWith((k, _) -> { let b = MutList.new(rc); MutList.push!(k, b); MutList.append!(MutList.toList(b), a); 1 }, m);
        MutList.toList(a) == range
    }


/////////////////////////////////////////////////////////////////////////////
// exists                                                                  //
/////////////////////////////////////////////////////////////////////////////
@Test
def exists01(): Bool = not Map.exists((k, v) -> k == v, Map#{}: Map[Unit, Unit])

@Test
def exists02(): Bool = not Map.exists((k, v) -> k == v, Map#{1 => 2})

@Test
def exists03(): Bool = Map.exists((k, v) -> k == v, Map#{1 => 1})

@Test
def exists04(): Bool = not Map.exists((k, v) -> k == v, Map#{1 => 2, 2 => 3})

@Test
def exists05(): Bool = Map.exists((k, v) -> k == v, Map#{1 => 1, 2 => 3})

@Test
def exists06(): Bool = Map.exists((k, v) -> k == v, Map#{1 => 2, 2 => 2})

@Test
def exists07(): Bool = Map.exists((k, v) -> k == v, Map#{1 => 1, 2 => 2})

/////////////////////////////////////////////////////////////////////////////
// forAll                                                                  //
/////////////////////////////////////////////////////////////////////////////
@Test
def forAll01(): Bool = Map.forAll((k, v) -> k == v, Map#{}: Map[Unit, Unit])

@Test
def forAll02(): Bool = not Map.forAll((k, v) -> k == v, Map#{1 => 2})

@Test
def forAll03(): Bool = Map.forAll((k, v) -> k == v, Map#{1 => 1})

@Test
def forAll04(): Bool = not Map.forAll((k, v) -> k == v, Map#{1 => 2, 2 => 3})

@Test
def forAll05(): Bool = not Map.forAll((k, v) -> k == v, Map#{1 => 1, 2 => 3})

@Test
def forAll06(): Bool = not Map.forAll((k, v) -> k == v, Map#{1 => 2, 2 => 2})

@Test
def forAll07(): Bool = Map.forAll((k, v) -> k == v, Map#{1 => 1, 2 => 2})

/////////////////////////////////////////////////////////////////////////////
// union                                                                   //
/////////////////////////////////////////////////////////////////////////////
@Test
def union01(): Bool = Map.union(Map#{}: Map[Unit, Unit], Map#{}) == Map#{}

@Test
def union02(): Bool = Map.union(Map#{1 => 2}, Map#{}) == Map#{1 => 2}

@Test
def union03(): Bool = Map.union(Map#{}, Map#{1 => 2}) == Map#{1 => 2}

@Test
def union04(): Bool = Map.union(Map#{}, Map#{1 => 2, 3 => 4}) == Map#{1 => 2, 3 => 4}

@Test
def union05(): Bool = Map.union(Map#{1 => 2, 3 => 4}, Map#{}) == Map#{1 => 2, 3 => 4}

@Test
def union06(): Bool = Map.union(Map#{1 => 2}, Map#{3 => 4}) == Map#{1 => 2, 3 => 4}

@Test
def union07(): Bool = Map.union(Map#{1 => 2}, Map#{1 => 5}) == Map#{1 => 2}

@Test
def union08(): Bool = Map.union(Map#{}, Map#{1 => 2, 2 => 3, 3 => 4}) == Map#{1 => 2, 2 => 3, 3 => 4}

@Test
def union09(): Bool = Map.union(Map#{1 => 2, 2 => 3, 3 => 4}, Map#{}) == Map#{1 => 2, 2 => 3, 3 => 4}

@Test
def union10(): Bool = Map.union(Map#{1 => 2, 2 => 3}, Map#{3 => 4}) == Map#{1 => 2, 2 => 3, 3 => 4}

@Test
def union11(): Bool = Map.union(Map#{3 => 4}, Map#{1 => 2, 2 => 3}) == Map#{1 => 2, 2 => 3, 3 => 4}

@Test
def union12(): Bool = Map.union(Map#{1 => 2, 2 => 3}, Map#{2 => 8}) == Map#{1 => 2, 2 => 3}

@Test
def union13(): Bool = Map.union(Map#{1 => 2, 2 => 3}, Map#{1 => 8}) == Map#{1 => 2, 2 => 3}

@Test
def union14(): Bool = Map.union(Map#{1 => 4}, Map#{1 => 2, 2 => 3}) == Map#{1 => 4, 2 => 3}

@Test
def union15(): Bool = Map.union(Map#{2 => 7}, Map#{1 => 2, 2 => 3}) == Map#{1 => 2, 2 => 7}

@Test
def union16(): Bool = Map.union(Map#{2 => 7, 11 => 14, 9 => 8, 15 => 22}, Map#{15 => 21, 1 => 2, 2 => 8, 44 => 33})
                                   == Map#{2 => 7, 11 => 14, 9 => 8, 15 => 22, 1 => 2, 44 => 33}

/////////////////////////////////////////////////////////////////////////////
// unionWith                                                               //
/////////////////////////////////////////////////////////////////////////////
@Test
def unionWith01(): Bool = Map.unionWith((v1, v2) -> v1 - v2, Map#{}, Map#{}) == Map#{}: Map[Int32, Int32]

@Test
def unionWith02(): Bool = Map.unionWith((v1, v2) -> v1 - v2, Map#{1 => 2}, Map#{}) == Map#{1 => 2}

@Test
def unionWith03(): Bool = Map.unionWith((v1, v2) -> v1 - v2, Map#{}, Map#{1 => 2}) == Map#{1 => 2}

@Test
def unionWith04(): Bool = Map.unionWith((v1, v2) -> v1 - v2, Map#{}, Map#{1 => 2, 3 => 4}) == Map#{1 => 2, 3 => 4}

@Test
def unionWith05(): Bool = Map.unionWith((v1, v2) -> v1 - v2, Map#{1 => 2, 3 => 4}, Map#{}) == Map#{1 => 2, 3 => 4}

@Test
def unionWith06(): Bool = Map.unionWith((v1, v2) -> v1 - v2, Map#{1 => 2}, Map#{3 => 4}) == Map#{1 => 2, 3 => 4}

@Test
def unionWith07(): Bool = Map.unionWith((v1, v2) -> v1 - v2, Map#{1 => 2}, Map#{1 => 5}) == Map#{1 => -3}

@Test
def unionWith08(): Bool = Map.unionWith((v1, v2) -> v1 - v2, Map#{}, Map#{1 => 2, 2 => 3, 3 => 4}) == Map#{1 => 2, 2 => 3, 3 => 4}

@Test
def unionWith09(): Bool = Map.unionWith((v1, v2) -> v1 - v2, Map#{1 => 2, 2 => 3, 3 => 4}, Map#{}) == Map#{1 => 2, 2 => 3, 3 => 4}

@Test
def unionWith10(): Bool = Map.unionWith((v1, v2) -> v1 - v2, Map#{1 => 2, 2 => 3}, Map#{3 => 4}) == Map#{1 => 2, 2 => 3, 3 => 4}

@Test
def unionWith11(): Bool = Map.unionWith((v1, v2) -> v1 - v2, Map#{3 => 4}, Map#{1 => 2, 2 => 3}) == Map#{1 => 2, 2 => 3, 3 => 4}

@Test
def unionWith12(): Bool = Map.unionWith((v1, v2) -> v1 - v2, Map#{1 => 2, 2 => 3}, Map#{2 => 8}) == Map#{1 => 2, 2 => -5}

@Test
def unionWith13(): Bool = Map.unionWith((v1, v2) -> v1 - v2, Map#{1 => 2, 2 => 3}, Map#{1 => 8}) == Map#{1 => -6, 2 => 3}

@Test
def unionWith14(): Bool = Map.unionWith((v1, v2) -> v1 - v2, Map#{1 => 4}, Map#{1 => 2, 2 => 3}) == Map#{1 => 2, 2 => 3}

@Test
def unionWith15(): Bool = Map.unionWith((v1, v2) -> v1 - v2, Map#{2 => 7}, Map#{1 => 2, 2 => 3}) == Map#{1 => 2, 2 => 4}

@Test
def unionWith16(): Bool = Map.unionWith((v1, v2) -> v1 - v2, Map#{2 => 7, 11 => 14, 9 => 8, 15 => 22}, Map#{15 => 21, 1 => 2, 2 => 8, 44 => 33})
                                   == Map#{2 => -1, 11 => 14, 9 => 8, 15 => 1, 1 => 2, 44 => 33}

/////////////////////////////////////////////////////////////////////////////
// unionWithKey                                                            //
/////////////////////////////////////////////////////////////////////////////
@Test
def unionWithKey01(): Bool = Map.unionWithKey((k, v1, v2) -> k + v1 - v2, Map#{}, Map#{}) == Map#{}: Map[Int32, Int32]

@Test
def unionWithKey02(): Bool = Map.unionWithKey((k, v1, v2) -> k + v1 - v2, Map#{1 => 2}, Map#{}) == Map#{1 => 2}

@Test
def unionWithKey03(): Bool = Map.unionWithKey((k, v1, v2) -> k + v1 - v2, Map#{}, Map#{1 => 2}) == Map#{1 => 2}

@Test
def unionWithKey04(): Bool = Map.unionWithKey((k, v1, v2) -> k + v1 - v2, Map#{}, Map#{1 => 2, 3 => 4}) == Map#{1 => 2, 3 => 4}

@Test
def unionWithKey05(): Bool = Map.unionWithKey((k, v1, v2) -> k + v1 - v2, Map#{1 => 2, 3 => 4}, Map#{}) == Map#{1 => 2, 3 => 4}

@Test
def unionWithKey06(): Bool = Map.unionWithKey((k, v1, v2) -> k + v1 - v2, Map#{1 => 2}, Map#{3 => 4}) == Map#{1 => 2, 3 => 4}

@Test
def unionWithKey07(): Bool = Map.unionWithKey((k, v1, v2) -> k + v1 - v2, Map#{1 => 2}, Map#{1 => 5}) == Map#{1 => -2}

@Test
def unionWithKey08(): Bool = Map.unionWithKey((k, v1, v2) -> k + v1 - v2, Map#{}, Map#{1 => 2, 2 => 3, 3 => 4}) == Map#{1 => 2, 2 => 3, 3 => 4}

@Test
def unionWithKey09(): Bool = Map.unionWithKey((k, v1, v2) -> k + v1 - v2, Map#{1 => 2, 2 => 3, 3 => 4}, Map#{}) == Map#{1 => 2, 2 => 3, 3 => 4}

@Test
def unionWithKey10(): Bool = Map.unionWithKey((k, v1, v2) -> k + v1 - v2, Map#{1 => 2, 2 => 3}, Map#{3 => 4}) == Map#{1 => 2, 2 => 3, 3 => 4}

@Test
def unionWithKey11(): Bool = Map.unionWithKey((k, v1, v2) -> k + v1 - v2, Map#{3 => 4}, Map#{1 => 2, 2 => 3}) == Map#{1 => 2, 2 => 3, 3 => 4}

@Test
def unionWithKey12(): Bool = Map.unionWithKey((k, v1, v2) -> k + v1 - v2, Map#{1 => 2, 2 => 3}, Map#{2 => 8}) == Map#{1 => 2, 2 => -3}

@Test
def unionWithKey13(): Bool = Map.unionWithKey((k, v1, v2) -> k + v1 - v2, Map#{1 => 2, 2 => 3}, Map#{1 => 8}) == Map#{1 => -5, 2 => 3}

@Test
def unionWithKey14(): Bool = Map.unionWithKey((k, v1, v2) -> k + v1 - v2, Map#{1 => 4}, Map#{1 => 2, 2 => 3}) == Map#{1 => 3, 2 => 3}

@Test
def unionWithKey15(): Bool = Map.unionWithKey((k, v1, v2) -> k + v1 - v2, Map#{2 => 7}, Map#{1 => 2, 2 => 3}) == Map#{1 => 2, 2 => 6}

@Test
def unionWithKey16(): Bool = Map.unionWithKey((k, v1, v2) -> k + v1 - v2, Map#{2 => 7, 11 => 14, 9 => 8, 15 => 22}, Map#{15 => 21, 1 => 2, 2 => 8, 44 => 33})
                                   == Map#{2 => 1, 11 => 14, 9 => 8, 15 => 16, 1 => 2, 44 => 33}

/////////////////////////////////////////////////////////////////////////////
// intersection                                                            //
/////////////////////////////////////////////////////////////////////////////
@Test
def intersection01(): Bool = Map.intersection(Map#{}: Map[Unit, Unit], Map#{}) == Map#{}

@Test
def intersection02(): Bool = Map.intersection(Map#{1 => 2}, Map#{}) == Map#{}

@Test
def intersection03(): Bool = Map.intersection(Map#{}, Map#{1 => 2}) == Map#{}

@Test
def intersection04(): Bool = Map.intersection(Map#{}, Map#{1 => 2, 3 => 4}) == Map#{}

@Test
def intersection05(): Bool = Map.intersection(Map#{1 => 2, 3 => 4}, Map#{}) == Map#{}

@Test
def intersection06(): Bool = Map.intersection(Map#{1 => 2}, Map#{3 => 4}) == Map#{}

@Test
def intersection07(): Bool = Map.intersection(Map#{1 => 2}, Map#{1 => 5}) == Map#{1 => 2}

@Test
def intersection08(): Bool = Map.intersection(Map#{}, Map#{1 => 2, 2 => 3, 3 => 4}) == Map#{}

@Test
def intersection09(): Bool = Map.intersection(Map#{1 => 2, 2 => 3, 3 => 4}, Map#{}) == Map#{}

@Test
def intersection10(): Bool = Map.intersection(Map#{1 => 2, 2 => 3}, Map#{3 => 4}) == Map#{}

@Test
def intersection11(): Bool = Map.intersection(Map#{3 => 4}, Map#{1 => 2, 2 => 3}) == Map#{}

@Test
def intersection12(): Bool = Map.intersection(Map#{1 => 2, 2 => 3}, Map#{2 => 8}) == Map#{2 => 3}

@Test
def intersection13(): Bool = Map.intersection(Map#{1 => 2, 2 => 3}, Map#{1 => 8}) == Map#{1 => 2}

@Test
def intersection14(): Bool = Map.intersection(Map#{1 => 4}, Map#{1 => 2, 2 => 3}) == Map#{1 => 4}

@Test
def intersection15(): Bool = Map.intersection(Map#{2 => 7}, Map#{1 => 2, 2 => 3}) == Map#{2 => 7}

@Test
def intersection16(): Bool = Map.intersection(Map#{2 => 7, 11 => 14, 9 => 8, 15 => 22}, Map#{15 => 21, 1 => 2, 2 => 8, 44 => 33, 11 => 1})
                                   == Map#{2 => 7, 15 => 22, 11 => 14}

/////////////////////////////////////////////////////////////////////////////
// intersectionWith                                                        //
/////////////////////////////////////////////////////////////////////////////
@Test
def intersectionWith01(): Bool = Map.intersectionWith((v1, v2) -> v1 - v2, Map#{}, Map#{}) == Map#{}: Map[Int32, Int32]

@Test
def intersectionWith02(): Bool = Map.intersectionWith((v1, v2) -> v1 - v2, Map#{1 => 2}, Map#{}) == Map#{}

@Test
def intersectionWith03(): Bool = Map.intersectionWith((v1, v2) -> v1 - v2, Map#{}, Map#{1 => 2}) == Map#{}

@Test
def intersectionWith04(): Bool = Map.intersectionWith((v1, v2) -> v1 - v2, Map#{}, Map#{1 => 2, 3 => 4}) == Map#{}

@Test
def intersectionWith05(): Bool = Map.intersectionWith((v1, v2) -> v1 - v2, Map#{1 => 2, 3 => 4}, Map#{}) == Map#{}

@Test
def intersectionWith06(): Bool = Map.intersectionWith((v1, v2) -> v1 - v2, Map#{1 => 2}, Map#{3 => 4}) == Map#{}

@Test
def intersectionWith07(): Bool = Map.intersectionWith((v1, v2) -> v1 - v2, Map#{1 => 2}, Map#{1 => 5}) == Map#{1 => -3}

@Test
def intersectionWith08(): Bool = Map.intersectionWith((v1, v2) -> v1 - v2, Map#{}, Map#{1 => 2, 2 => 3, 3 => 4}) == Map#{}

@Test
def intersectionWith09(): Bool = Map.intersectionWith((v1, v2) -> v1 - v2, Map#{1 => 2, 2 => 3, 3 => 4}, Map#{}) == Map#{}

@Test
def intersectionWith10(): Bool = Map.intersectionWith((v1, v2) -> v1 - v2, Map#{1 => 2, 2 => 3}, Map#{3 => 4}) == Map#{}

@Test
def intersectionWith11(): Bool = Map.intersectionWith((v1, v2) -> v1 - v2, Map#{3 => 4}, Map#{1 => 2, 2 => 3}) == Map#{}

@Test
def intersectionWith12(): Bool = Map.intersectionWith((v1, v2) -> v1 - v2, Map#{1 => 2, 2 => 3}, Map#{2 => 8}) == Map#{2 => -5}

@Test
def intersectionWith13(): Bool = Map.intersectionWith((v1, v2) -> v1 - v2, Map#{1 => 2, 2 => 3}, Map#{1 => 8}) == Map#{1 => -6}

@Test
def intersectionWith14(): Bool = Map.intersectionWith((v1, v2) -> v1 - v2, Map#{1 => 4}, Map#{1 => 2, 2 => 3}) == Map#{1 => 2}

@Test
def intersectionWith15(): Bool = Map.intersectionWith((v1, v2) -> v1 - v2, Map#{2 => 7}, Map#{1 => 2, 2 => 3}) == Map#{2 => 4}

@Test
def intersectionWith16(): Bool = Map.intersectionWith((v1, v2) -> v1 - v2, Map#{2 => 7, 11 => 14, 9 => 8, 15 => 22}, Map#{15 => 21, 1 => 2, 2 => 8, 44 => 33, 11 => 11})
                                   == Map#{2 => -1, 15 => 1, 11 => 3}
@Test
def intersectionWith17(): Bool = Map.intersectionWith((v1, v2) -> "${v1}|${v2}", Map#{3 => 'a', 5 => 'b'}, Map#{5 => "B", 7 => "C"})
                                   == Map#{5 => "b|B"}
@Test
def intersectionWith18(): Bool = Map.intersectionWith((v1, v2) -> "${v1}|${v2}", Map#{3 => 'a', 5 => 'b', 7 => 'c', 10 => 'd'}, Map#{5 => "B", 7 => "C"})
                                   == Map#{5 => "b|B", 7 => "c|C"}

@Test
def intersectionWith19(): Bool = Map.intersectionWith((v1, v2) -> "${v1}|${v2}", Map#{3 => '3'}, Map#{3 => 3})
                                   == Map#{3 => "3|3"}

/////////////////////////////////////////////////////////////////////////////
// intersectionWithKey                                                     //
/////////////////////////////////////////////////////////////////////////////
@Test
def intersectionWithKey01(): Bool = Map.intersectionWithKey((k, v1, v2) -> k + v1 - v2, Map#{}, Map#{}) == Map#{}: Map[Int32, Int32]

@Test
def intersectionWithKey02(): Bool = Map.intersectionWithKey((k, v1, v2) -> k + v1 - v2, Map#{1 => 2}, Map#{}) == Map#{}

@Test
def intersectionWithKey03(): Bool = Map.intersectionWithKey((k, v1, v2) -> k + v1 - v2, Map#{}, Map#{1 => 2}) == Map#{}

@Test
def intersectionWithKey04(): Bool = Map.intersectionWithKey((k, v1, v2) -> k + v1 - v2, Map#{}, Map#{1 => 2, 3 => 4}) == Map#{}

@Test
def intersectionWithKey05(): Bool = Map.intersectionWithKey((k, v1, v2) -> k + v1 - v2, Map#{1 => 2, 3 => 4}, Map#{}) == Map#{}

@Test
def intersectionWithKey06(): Bool = Map.intersectionWithKey((k, v1, v2) -> k + v1 - v2, Map#{1 => 2}, Map#{3 => 4}) == Map#{}

@Test
def intersectionWithKey07(): Bool = Map.intersectionWithKey((k, v1, v2) -> k + v1 - v2, Map#{1 => 2}, Map#{1 => 5}) == Map#{1 => -2}

@Test
def intersectionWithKey08(): Bool = Map.intersectionWithKey((k, v1, v2) -> k + v1 - v2, Map#{}, Map#{1 => 2, 2 => 3, 3 => 4}) == Map#{}

@Test
def intersectionWithKey09(): Bool = Map.intersectionWithKey((k, v1, v2) -> k + v1 - v2, Map#{1 => 2, 2 => 3, 3 => 4}, Map#{}) == Map#{}

@Test
def intersectionWithKey10(): Bool = Map.intersectionWithKey((k, v1, v2) -> k + v1 - v2, Map#{1 => 2, 2 => 3}, Map#{3 => 4}) == Map#{}

@Test
def intersectionWithKey11(): Bool = Map.intersectionWithKey((k, v1, v2) -> k + v1 - v2, Map#{3 => 4}, Map#{1 => 2, 2 => 3}) == Map#{}

@Test
def intersectionWithKey12(): Bool = Map.intersectionWithKey((k, v1, v2) -> k + v1 - v2, Map#{1 => 2, 2 => 3}, Map#{2 => 8}) == Map#{2 => -3}

@Test
def intersectionWithKey13(): Bool = Map.intersectionWithKey((k, v1, v2) -> k + v1 - v2, Map#{1 => 2, 2 => 3}, Map#{1 => 8}) == Map#{1 => -5}

@Test
def intersectionWithKey14(): Bool = Map.intersectionWithKey((k, v1, v2) -> k + v1 - v2, Map#{1 => 4}, Map#{1 => 2, 2 => 3}) == Map#{1 => 3}

@Test
def intersectionWithKey15(): Bool = Map.intersectionWithKey((k, v1, v2) -> k + v1 - v2, Map#{2 => 7}, Map#{1 => 2, 2 => 3}) == Map#{2 => 6}

@Test
def intersectionWithKey16(): Bool = Map.intersectionWithKey((k, v1, v2) -> k + v1 - v2, Map#{2 => 7, 11 => 14, 9 => 8, 15 => 22}, Map#{15 => 21, 1 => 2, 2 => 8, 44 => 33, 11 => 17})
                                  == Map#{2 => 1, 15 => 16, 11 => 8}
@Test
def intersectionWithKey17(): Bool = Map.intersectionWithKey((k, v1, v2) -> "${k}:${v1}|${v2}", Map#{3 => 'a', 5 => 'b'}, Map#{5 => "B", 7 => "C"})
                                   == Map#{5 => "5:b|B"}
@Test
def intersectionWithKey18(): Bool = Map.intersectionWithKey((k, v1, v2) -> "${k}:${v1}|${v2}", Map#{3 => 'a', 5 => 'b', 7 => 'c', 10 => 'd'}, Map#{5 => "B", 7 => "C"})
                                   == Map#{5 => "5:b|B", 7 => "7:c|C"}

@Test
def intersectionWithKey19(): Bool = Map.intersectionWithKey((k, v1, v2) -> "${k}:${v1}|${v2}", Map#{3 => '3'}, Map#{3 => 3})
                                   == Map#{3 => "3:3|3"}

/////////////////////////////////////////////////////////////////////////////
// difference                                                              //
/////////////////////////////////////////////////////////////////////////////
@Test
def difference01(): Bool = Map.difference(Map#{}: Map[Unit, Unit], Map#{}: Map[Unit, Unit]) == Map#{}

@Test
def difference02(): Bool = Map.difference(Map#{1 => 2}, Map#{}) == Map#{1 => 2}

@Test
def difference03(): Bool = Map.difference(Map#{}, Map#{1 => 2}) == Map#{}

@Test
def difference04(): Bool = Map.difference(Map#{}, Map#{1 => 2, 3 => 4}) == Map#{}

@Test
def difference05(): Bool = Map.difference(Map#{1 => 2, 3 => 4}, Map#{}) == Map#{1 => 2, 3 => 4}

@Test
def difference06(): Bool = Map.difference(Map#{1 => 2}, Map#{3 => 4}) == Map#{1 => 2}

@Test
def difference07(): Bool = Map.difference(Map#{1 => 2}, Map#{1 => 5}) == Map#{}

@Test
def difference08(): Bool = Map.difference(Map#{}, Map#{1 => 2, 2 => 3, 3 => 4}) == Map#{}

@Test
def difference09(): Bool = Map.difference(Map#{1 => 2, 2 => 3, 3 => 4}, Map#{}) == Map#{1 => 2, 2 => 3, 3 => 4}

@Test
def difference10(): Bool = Map.difference(Map#{1 => 2, 2 => 3}, Map#{3 => 4}) == Map#{1 => 2, 2 => 3}

@Test
def difference11(): Bool = Map.difference(Map#{3 => 4}, Map#{1 => 2, 2 => 3}) == Map#{3 => 4}

@Test
def difference12(): Bool = Map.difference(Map#{1 => 2, 2 => 3}, Map#{2 => 8}) == Map#{1 => 2}

@Test
def difference13(): Bool = Map.difference(Map#{1 => 2, 2 => 3}, Map#{1 => 8}) == Map#{2 => 3}

@Test
def difference14(): Bool = Map.difference(Map#{1 => 4}, Map#{1 => 2, 2 => 3}) == Map#{}

@Test
def difference15(): Bool = Map.difference(Map#{2 => 7}, Map#{1 => 2, 2 => 3}) == Map#{}

@Test
def difference16(): Bool = Map.difference(Map#{2 => 7, 11 => 14, 9 => 8, 15 => 22}, Map#{15 => 21, 1 => 2, 2 => 8, 44 => 33})
                                  == Map#{11 => 14, 9 => 8}

/////////////////////////////////////////////////////////////////////////////
// differenceWith                                                          //
/////////////////////////////////////////////////////////////////////////////
@Test
def differenceWith01(): Bool = Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, Map#{}, Map#{}) == Map#{}: Map[Int32, Int32]

@Test
def differenceWith02(): Bool = Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, Map#{1 => 2}, Map#{}) == Map#{1 => 2}

@Test
def differenceWith03(): Bool = Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, Map#{}, Map#{1 => 2}) == Map#{}

@Test
def differenceWith04(): Bool = Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, Map#{}, Map#{1 => 2, 3 => 4}) == Map#{}

@Test
def differenceWith05(): Bool = Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, Map#{1 => 2, 3 => 4}, Map#{}) == Map#{1 => 2, 3 => 4}

@Test
def differenceWith06(): Bool = Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, Map#{1 => 2}, Map#{3 => 4}) == Map#{1 => 2}

@Test
def differenceWith07(): Bool = Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, Map#{1 => 2}, Map#{1 => 5}) == Map#{}

@Test
def differenceWith08(): Bool = Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, Map#{1 => 5}, Map#{1 => 4}) == Map#{1 => 5}

@Test
def differenceWith09(): Bool = Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, Map#{}, Map#{1 => 2, 2 => 3, 3 => 4}) == Map#{}

@Test
def differenceWith10(): Bool = Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, Map#{1 => 2, 2 => 3, 3 => 4}, Map#{}) == Map#{1 => 2, 2 => 3, 3 => 4}

@Test
def differenceWith11(): Bool = Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, Map#{1 => 2, 2 => 3}, Map#{3 => 4}) == Map#{1 => 2, 2 => 3}

@Test
def differenceWith12(): Bool = Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, Map#{3 => 4}, Map#{1 => 2, 2 => 3}) == Map#{3 => 4}

@Test
def differenceWith13(): Bool = Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, Map#{1 => 2, 2 => 3}, Map#{2 => 8}) == Map#{1 => 2}

@Test
def differenceWith14(): Bool = Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, Map#{1 => 2, 2 => 9}, Map#{2 => 8}) == Map#{1 => 2, 2 => 9}

@Test
def differenceWith15(): Bool = Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, Map#{1 => 2, 2 => 3}, Map#{1 => 8}) == Map#{2 => 3}

@Test
def differenceWith16(): Bool = Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, Map#{1 => 9, 2 => 3}, Map#{1 => 8}) == Map#{1 => 9, 2 => 3}

@Test
def differenceWith17(): Bool = Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, Map#{1 => 1}, Map#{1 => 2, 2 => 3}) == Map#{}

@Test
def differenceWith18(): Bool = Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, Map#{1 => 4}, Map#{1 => 2, 2 => 3}) == Map#{1 => 4}

@Test
def differenceWith19(): Bool = Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, Map#{2 => -4}, Map#{1 => 2, 2 => 3}) == Map#{}

@Test
def differenceWith20(): Bool = Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, Map#{2 => 7}, Map#{1 => 2, 2 => 3}) == Map#{2 => 7}

@Test
def differenceWith21(): Bool = Map.differenceWith((v1, v2) -> if (v1 > v2) Some(v1) else None, Map#{2 => 7, 11 => 14, 9 => 8, 15 => 22}, Map#{15 => 21, 1 => 2, 2 => 8, 44 => 33})
                                  == Map#{11 => 14, 9 => 8, 15 => 22}

/////////////////////////////////////////////////////////////////////////////
// differenceWithKey                                                       //
/////////////////////////////////////////////////////////////////////////////
@Test
def differenceWithKey01(): Bool = Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, Map#{}, Map#{}) == Map#{}: Map[Int32, Int32]

@Test
def differenceWithKey02(): Bool = Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, Map#{1 => 2}, Map#{}) == Map#{1 => 2}

@Test
def differenceWithKey03(): Bool = Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, Map#{}, Map#{1 => 2}) == Map#{}

@Test
def differenceWithKey04(): Bool = Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, Map#{}, Map#{1 => 2, 3 => 4}) == Map#{}

@Test
def differenceWithKey05(): Bool = Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, Map#{1 => 2, 3 => 4}, Map#{}) == Map#{1 => 2, 3 => 4}

@Test
def differenceWithKey06(): Bool = Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, Map#{1 => 2}, Map#{3 => 4}) == Map#{1 => 2}

@Test
def differenceWithKey07(): Bool = Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, Map#{1 => 2}, Map#{1 => 5}) == Map#{}

@Test
def differenceWithKey08(): Bool = Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, Map#{1 => 5}, Map#{1 => 4}) == Map#{1 => 6}

@Test
def differenceWithKey09(): Bool = Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, Map#{}, Map#{1 => 2, 2 => 3, 3 => 4}) == Map#{}

@Test
def differenceWithKey10(): Bool = Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, Map#{1 => 2, 2 => 3, 3 => 4}, Map#{}) == Map#{1 => 2, 2 => 3, 3 => 4}

@Test
def differenceWithKey11(): Bool = Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, Map#{1 => 2, 2 => 3}, Map#{3 => 4}) == Map#{1 => 2, 2 => 3}

@Test
def differenceWithKey12(): Bool = Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, Map#{3 => 4}, Map#{1 => 2, 2 => 3}) == Map#{3 => 4}

@Test
def differenceWithKey13(): Bool = Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, Map#{1 => 2, 2 => 3}, Map#{2 => 8}) == Map#{1 => 2}

@Test
def differenceWithKey14(): Bool = Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, Map#{1 => 2, 2 => 9}, Map#{2 => 8}) == Map#{1 => 2, 2 => 11}

@Test
def differenceWithKey15(): Bool = Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, Map#{1 => 2, 2 => 3}, Map#{1 => 8}) == Map#{2 => 3}

@Test
def differenceWithKey16(): Bool = Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, Map#{1 => 9, 2 => 3}, Map#{1 => 8}) == Map#{1 => 10, 2 => 3}

@Test
def differenceWithKey17(): Bool = Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, Map#{1 => 1}, Map#{1 => 2, 2 => 3}) == Map#{}

@Test
def differenceWithKey18(): Bool = Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, Map#{1 => 4}, Map#{1 => 2, 2 => 3}) == Map#{1 => 5}

@Test
def differenceWithKey19(): Bool = Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, Map#{2 => -4}, Map#{1 => 2, 2 => 3}) == Map#{}

@Test
def differenceWithKey20(): Bool = Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, Map#{2 => 7}, Map#{1 => 2, 2 => 3}) == Map#{2 => 9}

@Test
def differenceWithKey21(): Bool = Map.differenceWithKey((k, v1, v2) -> if (v1 > v2) Some(v1 + k) else None, Map#{2 => 7, 11 => 14, 9 => 8, 15 => 22}, Map#{15 => 21, 1 => 2, 2 => 8, 44 => 33})
                                  == Map#{11 => 14, 9 => 8, 15 => 37}

/////////////////////////////////////////////////////////////////////////////
// toSet                                                                   //
/////////////////////////////////////////////////////////////////////////////
@Test
def toSet01(): Bool = Map.toSet(Map#{}: Map[Unit, Unit]) == Set#{}

@Test
def toSet02(): Bool = Map.toSet(Map#{1 => 2}) == Set#{(1, 2)}

@Test
def toSet03(): Bool = Map.toSet(Map#{1 => 2, 3 => 4}) == Set#{(1, 2), (3, 4)}

@Test
def toSet04(): Bool = Map.toSet(Map#{1 => 2, 2 => 2, 3 => 4}) == Set#{(1, 2), (2, 2), (3, 4)}

/////////////////////////////////////////////////////////////////////////////
// toList                                                                  //
/////////////////////////////////////////////////////////////////////////////
@Test
def toList01(): Bool = Map.toList(Map#{}: Map[Unit, Unit]) == Nil

@Test
def toList02(): Bool = Map.toList(Map#{1 => 2}) == (1, 2) :: Nil

@Test
def toList03(): Bool = Map.toList(Map#{1 => 2, 3 => 4}) == (1, 2) :: (3, 4) :: Nil

@Test
def toList04(): Bool = Map.toList(Map#{1 => 2, 2 => 2, 3 => 4}) == (1, 2) :: (2, 2) :: (3, 4) :: Nil

/////////////////////////////////////////////////////////////////////////////
// toMutDeque                                                              //
/////////////////////////////////////////////////////////////////////////////

@Test
def toMutDeque01(): Bool = region rc {
    let m: Map[Int32, Int32] = Map#{};
    let d1 = Map.toMutDeque(rc, m);

    let d2 = MutDeque.new(rc);

    d1 `MutDeque.sameElements` d2
}

@Test
def toMutDeque02(): Bool = region rc {
    let m = Map#{1 => 2};
    let d1 = Map.toMutDeque(rc, m);

    let d2 = MutDeque.new(rc);
    MutDeque.pushBack((1, 2), d2);

    d1 `MutDeque.sameElements` d2
}

@Test
def toMutDeque03(): Bool = region rc {
    let m = Map#{1 => 2, 3 => 4, 5 => 6};
    let d1 = Map.toMutDeque(rc, m);

    let d2 = MutDeque.new(rc);
    MutDeque.pushBack((3, 4), d2);
    MutDeque.pushBack((5, 6), d2);
    MutDeque.pushFront((1, 2), d2);

    d1 `MutDeque.sameElements` d2
}

@Test
def toMutDeque04(): Bool = region rc {
    let m = Map#{1 => 'a', 2 => 'b', 3 => 'c'};
    let d1 = Map.toMutDeque(rc, m);

    let d2 = MutDeque.new(rc);
    MutDeque.pushFront((3, 'c'), d2);
    MutDeque.pushFront((2, 'b'), d2);
    MutDeque.pushFront((1, 'a'), d2);

    d1 `MutDeque.sameElements` d2
}

/////////////////////////////////////////////////////////////////////////////
// eq                                                                      //
/////////////////////////////////////////////////////////////////////////////
@Test
def eq01(): Bool = Map#{}: Map[Unit, Unit] == Map#{}

@Test
def eq02(): Bool = Map#{1 => 2} != Map#{}

@Test
def eq03(): Bool = Map#{} != Map#{1 => 2}

@Test
def eq04(): Bool = Map#{1 => 2, 3 => 4} != Map#{}

@Test
def eq05(): Bool = Map#{} != Map#{1 => 2, 3 => 4}

@Test
def eq06(): Bool = Map#{1 => 2} == Map#{1 => 2}

@Test
def eq07(): Bool = Map#{1 => 3} != Map#{1 => 2}

@Test
def eq08(): Bool = Map#{1 => 2} != Map#{1 => 3}

@Test
def eq09(): Bool = Map#{2 => 2} != Map#{1 => 2}

@Test
def eq10(): Bool = Map#{1 => 2} != Map#{4 => 2}

@Test
def eq11(): Bool = Map#{1 => 2, 3 => 4} != Map#{1 => 2}

@Test
def eq12(): Bool = Map#{1 => 2} != Map#{1 => 2, 3 => 4}

@Test
def eq13(): Bool = Map#{1 => 2, 3 => 4} == Map#{1 => 2, 3 => 4}

@Test
def eq14(): Bool = Map#{1 => 2, 3 => 4} == Map#{3 => 4, 1 => 2}

@Test
def eq15(): Bool = Map#{1 => 1, 3 => 4} != Map#{1 => 2, 3 => 4}

@Test
def eq16(): Bool = Map#{4 => 2, 3 => 4} != Map#{1 => 2, 3 => 4}

@Test
def eq17(): Bool = Map#{1 => 2, 3 => 4} != Map#{1 => 2, 2 => 4}

@Test
def eq18(): Bool = Map#{1 => 2, 3 => 4} != Map#{1 => 2, 3 => 8}

@Test
def eq19(): Bool = Map#{1 => 2, 3 => 4, 5 => 6} != Map#{1 => 2, 3 => 4}

@Test
def eq20(): Bool = Map#{1 => 2, 3 => 4} != Map#{1 => 2, 3 => 4, 5 => 6}

@Test
def eq21(): Bool = Map#{1 => 2, 3 => 4, 5 => 6} == Map#{1 => 2, 3 => 4, 5 => 6}

@Test
def eq22(): Bool = Map#{1 => 2, 3 => 4, 5 => 6} == Map#{1 => 2, 5 => 6, 3 => 4}

@Test
def eq23(): Bool = Map#{1 => 2, 3 => 4, 5 => 6} == Map#{3 => 4, 5 => 6, 1 => 2}

@Test
def eq24(): Bool = Map#{1 => 2, 3 => 4, 5 => 6} == Map#{5 => 6, 3 => 4, 1 => 2}

@Test
def eq25(): Bool = Map#{1 => 2, 3 => 4, 5 => 6, 7 => 8, 9 => 10} == Map#{7 => 8, 1 => 2, 3 => 4, 9 => 10, 5 => 6}

/////////////////////////////////////////////////////////////////////////////
// forEach                                                                 //
/////////////////////////////////////////////////////////////////////////////

@Test
def forEach01(): Bool = region rc {
    let ri = ref 21 @ rc;
    Map.forEach((k, _) -> ri := k, Map#{});
    21 == deref ri
}

@Test
def forEach02(): Bool = region rc {
    let ri = ref 21 @ rc;
    Map.forEach((k, _) -> ri := k, Map#{1 => "Hello World!"});
    1 == deref ri
}

/////////////////////////////////////////////////////////////////////////////
// forEachWithIndex                                                        //
/////////////////////////////////////////////////////////////////////////////

@Test
def forEachWithIndex01(): Bool = region rc {
    let ri = ref 21 @ rc;
    Map.forEachWithIndex((i, _, _) -> ri := i, Map#{});
    21 == deref ri
}

@Test
def forEachWithIndex02(): Bool = region rc {
    let ri = ref 21 @ rc;
    Map.forEachWithIndex((i, _, _) -> ri := i, Map#{1 => "Hello World!"});
    0 == deref ri
}

/////////////////////////////////////////////////////////////////////////////
// unfold                                                                  //
/////////////////////////////////////////////////////////////////////////////

@Test
def unfold01(): Bool =
    Map.unfold(s -> if (true) None else Some(s, s + 48, s + 1), 0) == Map#{}

@Test
def unfold02(): Bool =
    Map.unfold(s -> if (s > 0) None else Some(s, s + 48, s + 1), 0) == Map#{0 => 48}

@Test
def unfold03(): Bool =
    Map.unfold(s -> if (s > 1) None else Some(s, s + 48, s + 1), 0) == Map#{0 => 48, 1 => 49}

@Test
def unfold04(): Bool =
    Map.unfold(s -> if (s >= 10) None else Some(s, s + 48, s + 1), 0) == Map#{0 => 48, 1 => 49, 2 => 50, 3 => 51, 4 => 52, 5 => 53, 6 => 54, 7 => 55, 8 => 56, 9 => 57}

@Test
def unfold05(): Bool =
    Map.unfold(s -> if (s >= 10) None else Some(s, s + 48, s + 1), 5) == Map#{5 => 53, 6 => 54, 7 => 55, 8 => 56, 9 => 57}

@Test
def unfold06(): Bool =
    Map.unfold(s -> if (s >= 10) None else Some(s, s + 48, s + 2), 0) == Map#{0 => 48, 2 => 50, 4 => 52, 6 => 54, 8 => 56}

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
            let k = deref x;
            let v = k + 48;
            x := deref x + 1;
            Some(k, v)
        };
    Map.unfoldWithIter(step) == Map#{}
}

@Test
def unfoldWithIter02(): Bool = region rc {
    let x = ref 0 @ rc;
    let step = () ->
        if (deref x > 0)
            None
        else {
            let k = deref x;
            let v = k + 48;
            x := deref x + 1;
            Some(k, v)
        };
    Map.unfoldWithIter(step) == Map#{0 => 48}
}

@Test
def unfoldWithIter03(): Bool = region rc {
    let x = ref 0 @ rc;
    let step = () ->
        if (deref x > 1)
            None
        else {
            let k = deref x;
            let v = k + 48;
            x := deref x + 1;
            Some(k, v)
        };
    Map.unfoldWithIter(step) == Map#{0 => 48, 1 => 49}
}

@Test
def unfoldWithIter04(): Bool = region rc {
    let x = ref 0 @ rc;
    let step = () ->
        if (deref x >= 10)
            None
        else {
            let k = deref x;
            let v = k + 48;
            x := deref x + 1;
            Some(k, v)
        };
    Map.unfoldWithIter(step) == Map#{0 => 48, 1 => 49, 2 => 50, 3 => 51, 4 => 52, 5 => 53, 6 => 54, 7 => 55, 8 => 56, 9 => 57}
}

@Test
def unfoldWithIter05(): Bool = region rc {
    let x = ref 5 @ rc;
    let step = () ->
        if (deref x >= 10)
            None
        else {
            let k = deref x;
            let v = k + 48;
            x := deref x + 1;
            Some(k, v)
        };
    Map.unfoldWithIter(step) == Map#{5 => 53, 6 => 54, 7 => 55, 8 => 56, 9 => 57}
}

@Test
def unfoldWithIter06(): Bool = region rc {
    let x = ref 0 @ rc;
    let step = () ->
        if (deref x >= 10)
            None
        else {
            let k = deref x;
            let v = k + 48;
            x := deref x + 2;
            Some(k, v)
        };
    Map.unfoldWithIter(step) == Map#{0 => 48, 2 => 50, 4 => 52, 6 => 54, 8 => 56}
}

/////////////////////////////////////////////////////////////////////////////
// inverse                                                                 //
/////////////////////////////////////////////////////////////////////////////

@Test
def testInvert01(): Bool = Map.invert(Map#{1 => "a"}) == Map#{"a" => Set#{1}}

@Test
def testInvert02(): Bool = Map.invert(Map#{1 => "a", 2 => "b"}) == Map#{"a" => Set#{1}, "b" => Set#{2}}

@Test
def testInvert03(): Bool = Map.invert(Map#{1 => "a", 2 => "a"}) == Map#{"a" => Set#{1, 2}}

@Test
def testInvert04(): Bool = Map.invert(Map#{1 => "a", 2 => "b", 3 => "a"}) == Map#{"a" => Set#{1, 3}, "b" => Set#{2}}

@Test
def testInvert05(): Bool = Map.invert(Map#{1 => "a", 2 => "a", 3 => "a"}) == Map#{"a" => Set#{1, 2, 3}}

@Test
def testInvert06(): Bool = Map.invert(Map#{1 => "a", 2 => "b", 3 => "a", 4 => "b"}) == Map#{"a" => Set#{1, 3}, "b" => Set#{2, 4}}

/////////////////////////////////////////////////////////////////////////////
// query                                                                   //
/////////////////////////////////////////////////////////////////////////////

@Test
def query01(): Bool =
    Map.query(_ -> Comparison.EqualTo, Map#{}: Map[Unit, Unit]) == Nil

@Test
def query02(): Bool =
    Map.query(_ -> Comparison.EqualTo, Map#{1 => 101, 2 => 42}) == (1, 101) :: (2, 42) :: Nil

@Test
def query03(): Bool =
    Map.query(x -> if (x < 2) Comparison.LessThan else if (x > 2) Comparison.GreaterThan else Comparison.EqualTo, Map#{1 => 1, 2 => 2, 3 => 3}) == (2, 2) :: Nil

@Test
def query04(): Bool =
    Map.query(x -> if (x < 'b') Comparison.LessThan else if (x > 'c') Comparison.GreaterThan else Comparison.EqualTo, Map#{'a' => 0, 'b' => 1, 'c' => 2, 'd' => 3, 'e' => 4}) == ('b', 1) :: ('c', 2) :: Nil

@Test
def query05(): Bool =
    let m = Map.unfold(s -> if (s > 10) None else Some(s, s, s + 1), 5);
    Map.query(x -> if (x < 5) Comparison.LessThan else if (x >= 10) Comparison.GreaterThan else Comparison.EqualTo, m) == List.map(x -> (x, x), List.range(5, 10))

@Test
def query06(): Bool =
    let m = Map.unfold(s -> if (s > 75) None else Some(s, Some(s + 48), s + 1), 25);
    Map.query(x -> if (x < 25) Comparison.LessThan else if (x >= 75) Comparison.GreaterThan else Comparison.EqualTo, m) == List.map(x -> (x, Some(x + 48)), List.range(25, 75))

@Test
def query07(): Bool =
    Map.query(x -> if (x < 42) Comparison.LessThan else if (x > 42) Comparison.GreaterThan else Comparison.EqualTo, Map#{42 => true}) == (42, true) :: Nil

@Test
def query08(): Bool =
    Map.query(x -> if (x < 1) Comparison.LessThan else if (x > 1) Comparison.GreaterThan else Comparison.EqualTo, Map#{1 => 2, 2 => 3, 3 => 4, 4 => 5}) == (1, 2) :: Nil

@Test
def query09(): Bool =
    Map.query(x -> if (x < 2) Comparison.LessThan else if (x > 3) Comparison.GreaterThan else Comparison.EqualTo, Map#{1 => 2, 2 => 3, 3 => 4, 4 => 5}) == (2, 3) :: (3, 4) :: Nil

@Test
def query10(): Bool =
    Map.query(x -> if (x < 4) Comparison.LessThan else if (x > 4) Comparison.GreaterThan else Comparison.EqualTo, Map#{1 => 2, 2 => 3, 3 => 4, 4 => 5}) == (4, 5) :: Nil

@Test
def query11(): Bool =
    let m = Map#{(1, 2) => 0, (1, 3) => 1, (2, 1) => 2, (2, 2) => 3, (2, 3) => 4, (3, 1) => 5};
    Map.query(x -> if (fst(x) < 1) Comparison.LessThan else if (fst(x) > 1) Comparison.GreaterThan else Comparison.EqualTo, m) == ((1, 2), 0) :: ((1, 3), 1) :: Nil

@Test
def query12(): Bool =
    let m = Map#{(1, 2) => 0, (1, 3) => 1, (2, 1) => 2, (2, 2) => 3, (2, 3) => 4, (3, 1) => 5};
    Map.query(x -> if (fst(x) < 3) Comparison.LessThan else if (fst(x) > 3) Comparison.GreaterThan else Comparison.EqualTo, m) == ((3, 1), 5) :: Nil

@Test
def query13(): Bool =
    let m = Map#{(1, 2) => 0, (1, 3) => 1, (2, 1) => 2, (2, 2) => 3, (2, 3) => 4, (3, 1) => 5};
    Map.query(x -> if (fst(x) > 2) Comparison.GreaterThan else Comparison.EqualTo, m) == ((1, 2), 0) :: ((1, 3), 1) :: ((2, 1), 2) :: ((2, 2), 3) :: ((2, 3), 4) :: Nil

@Test
def query14(): Bool =
    let m = Map#{(1, 2) => 0, (1, 3) => 1, (2, 1) => 2, (2, 2) => 3, (2, 3) => 4, (3, 1) => 5};
    Map.query(x -> if (fst(x) < 2) Comparison.LessThan else Comparison.EqualTo, m) == ((2, 1), 2) :: ((2, 2), 3) :: ((2, 3), 4) :: ((3, 1), 5) :: Nil

@Test
def query15(): Bool =
    let m = Map#{(1, 2) => 0, (1, 3) => 1, (2, 1) => 2, (2, 2) => 3, (2, 3) => 4, (3, 1) => 5};
    let f = x -> {
        let cmp = x <=> (2, 2);
        if (cmp == Comparison.GreaterThan)
            cmp
        else
            Comparison.EqualTo
        };
    Map.query(f, m) == ((1, 2), 0) :: ((1, 3), 1) :: ((2, 1), 2) :: ((2, 2), 3) :: Nil

@Test
def query16(): Bool =
    let m = Map#{(1, 2) => 0, (1, 3) => 1, (2, 1) => 2, (2, 2) => 3, (2, 3) => 4, (3, 1) => 5};
    let f = x -> {
        let cmp = x <=> (2, 2);
        if (cmp == Comparison.LessThan)
            cmp
        else
            Comparison.EqualTo
        };
    Map.query(f, m) == ((2, 2), 3) :: ((2, 3), 4) :: ((3, 1), 5) :: Nil

@Test
def query17(): Bool =
    let m = Map#{(1, 2, 2) => 0, (1, 2, 3) => 1, (1, 2, 4) => 2, (2, 2, 4) => 3};
    let f = x -> {
        let cmp = x <=> (1, 2, 3);
        if (cmp == Comparison.LessThan)
            cmp
        else
            Comparison.EqualTo
        };
    Map.query(f, m) == ((1, 2, 3), 1) :: ((1, 2, 4), 2) :: ((2, 2, 4), 3) :: Nil

@Test
def query18(): Bool =
    let m = Map#{(1, 2, 2) => 0, (1, 2, 3) => 1, (1, 2, 4) => 2, (2, 2, 4) => 3};
    let f = x -> {
        let cmp = x <=> (1, 2, 3);
        if (cmp == Comparison.GreaterThan)
            cmp
        else
            Comparison.EqualTo
        };
    Map.query(f, m) == ((1, 2, 2), 0) :: ((1, 2, 3), 1) :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // toString                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toString01(): Bool =
        toString(Map#{1 => 2}) == "Map#{1 => 2}"

    @Test
    def toString02(): Bool =
        toString(Map#{1 => 0, 2 => 1, 3 => 2}) == "Map#{1 => 0, 2 => 1, 3 => 2}"

    @Test
    def toString03(): Bool =
        toString(Map#{1 => "b", 2 => "a"}) == "Map#{1 => b, 2 => a}"

    @Test
    def toString04(): Bool =
        toString(Map#{97 => false, 2 => true, 3 => false, 4 => true, 0 => true}) == "Map#{0 => true, 2 => true, 3 => false, 4 => true, 97 => false}"

    @Test
    def toString05(): Bool =
        toString(Map#{2 => Map#{1 => 0, 2 => 1}, 3 => Map#{3 => 2, 4 => 92}}) == "Map#{2 => Map#{1 => 0, 2 => 1}, 3 => Map#{3 => 2, 4 => 92}}"

    /////////////////////////////////////////////////////////////////////////////
    // hash                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def hash01(): Bool = hash(Map#{}: Map[Unit, Unit]) == hash(Map#{}: Map[Unit, Unit])

    @Test
    def hash02(): Bool = hash(Map#{1 => true, 2 => false, 3 => false}) == hash(Map#{1 => true, 2 => false, 3 => false})

    @Test
    def hash03(): Bool = hash(Map#{1 => 2, 1 => 2}) != hash(Map#{1 => 1, 2 => 2})

    @Test
    def hash04(): Bool = hash(Map#{1 => 'a', 2 => 'b'}) != hash(Map#{'a' => 1, 'b' => 2})

    @Test
    def hash05(): Bool = hash(Map#{1 => 'a', 2 => 'b', 3 => 'z', 4 => 'x'}) == hash(Map#{1 => 'a', 2 => 'b', 3 => 'z', 4 => 'x'})

    /////////////////////////////////////////////////////////////////////////////
    // minimumKey                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def minimumKey01(): Bool = Map.minimumKey(Map#{}: Map[Int32, Int32]) == None

    @Test
    def minimumKey02(): Bool = Map.minimumKey(Map#{1 => 2, 2 => 3, 3 => 4, 5 => 5}) == Some((1, 2))

    @Test
    def minimumKey03(): Bool = Map.minimumKey(Map#{1 => 2, 2 => 3, 3 => 4, 5 => 5, 0 => 2}) == Some((0, 2))

    @Test
    def minimumKey04(): Bool = Map.minimumKey(Map#{1 => 2, 2 => 3, -44 => 4, 5 => 5, 0 => 2}) == Some((-44, 4))

    /////////////////////////////////////////////////////////////////////////////
    // minimumBy                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def minimumKeyBy01(): Bool =
        Map.minimumKeyBy((x, y) -> x <=> y, Map#{}: Map[Int32, Int32]) == None

    @Test
    def minimumKeyBy02(): Bool =
        Map.minimumKeyBy((x, y) -> x <=> y, Map#{1 => 2, 2 => 3, 3 => 4, 5 => 5}) == Some((1, 2))

    @Test
    def minimumKeyBy03(): Bool =
        Map.minimumKeyBy((x, y) -> x <=> y, Map#{1 => 2, 2 => 3, 3 => 4, 5 => 5, 0 => 2}) == Some((0, 2))

    @Test
    def minimumKeyBy04(): Bool =
        Map.minimumKeyBy((x, y) -> x <=> y, Map#{1 => 2, 2 => 3, -44 => 4, 5 => 5, 0 => 2}) == Some((-44, 4))

    @Test
    def minimumKeyBy05(): Bool = region rc {
        let range = List.range(0, 2000);
        let m = List.zip(range, range) |> List.toMap;
        let a = MutList.new(rc);
        discard Map.minimumKeyBy((x, y) -> { let b = MutList.new(rc); MutList.push!(y, b); MutList.append!(MutList.toList(b), a); x <=> y }, m);
        MutList.toList(a) == List.range(1, 2000) // The first / left most value is ignored
    }

    /////////////////////////////////////////////////////////////////////////////
    // minimumValue                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def minimumValue01(): Bool = Map.minimumValue(Map#{}: Map[Int32, Int32]) == None

    @Test
    def minimumValue02(): Bool = Map.minimumValue(Map#{1 => 2, 2 => 3, 3 => 4, 5 => 5}) == Some((1, 2))

    @Test
    def minimumValue03(): Bool = Map.minimumValue(Map#{1 => 2, 2 => 3, 3 => 4, 5 => 5, 0 => 2}) == Some((1, 2))

    @Test
    def minimumValue04(): Bool = Map.minimumValue(Map#{1 => 2, 2 => 3, -44 => 4, 5 => 5, 0 => 2}) == Some((1, 2))

    @Test
    def minimumValue05(): Bool = Map.minimumValue(Map#{1 => 2, 2 => 3, 4 => -44, 5 => 5, 0 => 2}) == Some((4, -44))

    /////////////////////////////////////////////////////////////////////////////
    // minimumBy                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def minimumValueBy01(): Bool =
        Map.minimumValueBy((x, y) -> x <=> y, Map#{}: Map[Int32, Int32]) == None

    @Test
    def minimumValueBy02(): Bool =
        Map.minimumValueBy((x, y) -> x <=> y, Map#{1 => 2, 2 => 3, 3 => 4, 5 => 5}) == Some((1, 2))

    @Test
    def minimumValueBy03(): Bool =
        Map.minimumValueBy((x, y) -> x <=> y, Map#{1 => 2, 2 => 3, 3 => 4, 5 => 5, 0 => 2}) == Some((1, 2))

    @Test
    def minimumValueBy04(): Bool =
        Map.minimumValueBy((x, y) -> x <=> y, Map#{1 => 2, 2 => 3, -44 => 4, 5 => 5, 0 => 2}) == Some((1, 2))

    @Test
    def minimumValueBy05(): Bool =
        Map.minimumValueBy((x, y) -> x <=> y, Map#{1 => 2, 2 => 3, 4 => -44, 5 => 5, 0 => 2}) == Some((4, -44))

    @Test
    def minimumValueBy06(): Bool = region rc {
        let range = List.range(0, 2000);
        let m = List.zip(range, List.reverse(range)) |> List.toMap;
        let a = MutList.new(rc);
        discard Map.minimumValueBy((x, y) -> { let b = MutList.new(rc); MutList.push!(x, b); MutList.append!(MutList.toList(b), a); x <=> y }, m);
        MutList.toList(a) == List.reverse(List.range(1, 2000)) // The first / left most value is ignored
    }


    /////////////////////////////////////////////////////////////////////////////
    // maximumKey                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def maximumKey01(): Bool = Map.maximumKey(Map#{}: Map[Int32, Int32]) == None

    @Test
    def maximumKey02(): Bool = Map.maximumKey(Map#{1 => 2, 2 => 3, 3 => 4, 5 => 5}) == Some((5, 5))

    @Test
    def maximumKey03(): Bool = Map.maximumKey(Map#{1 => 2, 2 => 3, 3 => 4, 5 => 5, 0 => 2}) == Some((5, 5))

    @Test
    def maximumKey04(): Bool = Map.maximumKey(Map#{1 => 2, 2 => 3, 107 => -107, 5 => 5, 0 => 2}) == Some((107, -107))


    /////////////////////////////////////////////////////////////////////////////
    // maximumKeyBy                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def maximumKeyBy01(): Bool = Map.maximumKeyBy((x, y) -> x <=> y, Map#{}: Map[Int32, Int32]) == None

    @Test
    def maximumKeyBy02(): Bool = Map.maximumKeyBy((x, y) -> x <=> y, Map#{1 => 2, 2 => 3, 3 => 4, 5 => 5}) == Some((5, 5))

    @Test
    def maximumKeyBy03(): Bool = Map.maximumKeyBy((x, y) -> x <=> y, Map#{1 => 2, 2 => 3, 3 => 4, 5 => 5, 0 => 2}) == Some((5, 5))

    @Test
    def maximumKeyBy04(): Bool = Map.maximumKeyBy((x, y) -> x <=> y, Map#{1 => 2, 2 => 3, 107 => -107, 5 => 5, 0 => 2}) == Some((107, -107))

    @Test
    def maximumKeyBy05(): Bool = region rc {
        let range = List.range(0, 2000);
        let m = List.zip(range, range) |> List.toMap;
        let a = MutList.new(rc);
        discard Map.maximumKeyBy((x, y) -> { let b = MutList.new(rc); MutList.push!(y, b); MutList.append!(MutList.toList(b), a); x <=> y }, m);
        MutList.toList(a) == List.range(1, 2000) // The first / left most value is ignored
    }


    /////////////////////////////////////////////////////////////////////////////
    // maximumValue                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def maximumValue01(): Bool = Map.maximumValue(Map#{}: Map[Int32, Int32]) == None

    @Test
    def maximumValue02(): Bool = Map.maximumValue(Map#{1 => 2, 2 => 3, 3 => 4, 5 => 5}) == Some((5, 5))

    @Test
    def maximumValue03(): Bool = Map.maximumValue(Map#{1 => 2, 2 => 3, 3 => 4, 5 => 5, 0 => 2}) == Some((5, 5))

    @Test
    def maximumValue04(): Bool = Map.maximumValue(Map#{1 => 2, 2 => 3, -107 => 107, 0 => 2}) == Some((-107, 107))


    /////////////////////////////////////////////////////////////////////////////
    // maximumValueBy                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def maximumValueBy01(): Bool = Map.maximumValueBy((x, y) -> x <=> y, Map#{}: Map[Int32, Int32]) == None

    @Test
    def maximumValueBy02(): Bool = Map.maximumValueBy((x, y) -> x <=> y, Map#{1 => 2, 2 => 3, 3 => 4, 5 => 5}) == Some((5, 5))

    @Test
    def maximumValueBy03(): Bool = Map.maximumValueBy((x, y) -> x <=> y, Map#{1 => 2, 2 => 3, 3 => 4, 5 => 5, 0 => 2}) == Some((5, 5))

    @Test
    def maximumValueBy04(): Bool = Map.maximumValueBy((x, y) -> x <=> y, Map#{1 => 2, 2 => 3, -107 => 107,5 => 5, 0 => 2}) == Some((-107, 107))

    @Test
    def maximumValueBy05(): Bool = region rc {
        let range = List.range(0, 2000);
        let m = List.zip(range, range) |> List.toMap;
        let a = MutList.new(rc);
        discard Map.maximumValueBy((x, y) -> { let b = MutList.new(rc); MutList.push!(y, b); MutList.append!(MutList.toList(b), a); x <=> y }, m);
        MutList.toList(a) == List.range(1, 2000) // The first / left most value is ignored
    }


    /////////////////////////////////////////////////////////////////////////////
    // toDelayList                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toDelayList01(): Bool =
        Map.empty(): Map[Unit, Unit] |> Map.toDelayList == DelayList.empty()

    @Test
    def toDelayList02(): Bool =
        Map.unfold(s -> if (s < 1000) Some((s, "a", s + 1)) else None, 0) |> Map.toDelayList == DelayList.repeat("a") |> DelayList.zip(DelayList.range(0, 1000))

    @Test
    def toDelayList03(): Bool =
        Map.unfold(s -> if (s < 1000) Some((s, "a", s + 1)) else None, 0) |> Map.toDelayList |> DelayList.toMap == Map.unfold(s -> if (s < 1000) Some((s, "a", s + 1)) else None, 0)

    /////////////////////////////////////////////////////////////////////////////
    // toChain                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toChain01(): Bool =
        Map.toChain(Map#{}: Map[Int32, Int32]) == Chain.empty(): Chain[(Int32, Int32)]

    @Test
    def toChain02(): Bool =
        Map.toChain(Map#{1 => 1}) == Chain.singleton((1, 1))

    @Test
    def toChain03(): Bool =
        Map.toChain(Map#{1 => 1, 2 => 2}) == List.toChain((1, 1) :: (2, 2) :: Nil)

    @Test
    def toChain04(): Bool =
        Map.toChain(Map#{1 => 1, 2 => 2, 3 => 3}) == List.toChain((1, 1) :: (2, 2) :: (3, 3) :: Nil)


    /////////////////////////////////////////////////////////////////////////////
    // toMultiMap                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toMultiMap01(): Bool =
        Map.toMultiMap(Map#{}: Map[Int32, Char]) |> MultiMap.toAscList == Nil

    @Test
    def toMultiMap02(): Bool =
        Map.toMultiMap(Map#{1 => 'a'}) |> MultiMap.toAscList == (1, 'a') :: Nil

    @Test
    def toMultiMap03(): Bool =
        Map.toMultiMap(Map#{1 => 'a', 2 => 'b'}) |> MultiMap.toAscList == (1, 'a') :: (2, 'b') :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // iterator                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def iterator01(): Bool = region rc {
        Map.empty(): Map[Int32, Int32] |> Map.iterator(rc) |> Iterator.toMap == Map.empty()
    }

    @Test
    def iterator02(): Bool = region rc {
        Map#{"a" => 1, "b" => 2, "c" => 3, "d" => 4} |> Map.iterator(rc) |> Iterator.toMap == Map#{"a" => 1, "b" => 2, "c" => 3, "d" => 4}
    }

    @Test
    def iterator03(): Bool = region rc {
        let l = List.range(0, 100);
        List.zip(l, List.reverse(l)) |> List.toMap |> Map.iterator(rc) |> Iterator.toMap |> Map.toList == List.zip(List.range(0, 100), List.reverse(List.range(0, 100)))
    }


    /////////////////////////////////////////////////////////////////////////////
    // iteratorKeys                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def iteratorKeys01(): Bool = region rc {
        Map.empty(): Map[Int32, Int32] |> Map.iteratorKeys(rc) |> Iterator.toList == Nil
    }

    @Test
    def iteratorKeys02(): Bool = region rc {
        Map#{"a" => 1, "b" => 2, "c" => 3, "d" => 4} |> Map.iteratorKeys(rc) |> Iterator.toList == "a" :: "b" :: "c" :: "d" :: Nil
    }

    @Test
    def iteratorKeys03(): Bool = region rc {
        let l = List.range(0, 100);
        List.zip(l, List.reverse(l)) |> List.toMap |> Map.iteratorKeys(rc) |> Iterator.toList == List.range(0, 100)
    }


    /////////////////////////////////////////////////////////////////////////////
    // iteratorValues                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def iteratorValues01(): Bool = region rc {
        Map.empty(): Map[Int32, Int32] |> Map.iteratorValues(rc) |> Iterator.toList == Nil
    }

    @Test
    def iteratorValues02(): Bool = region rc {
        Map#{"a" => 1, "b" => 2, "c" => 3, "d" => 4} |> Map.iteratorValues(rc) |> Iterator.toList == 1 :: 2 :: 3 :: 4 :: Nil
    }

    @Test
    def iteratorValues03(): Bool = region rc {
        let l = List.range(0, 100);
        List.zip(l, List.reverse(l)) |> List.toMap |> Map.iteratorValues(rc) |> Iterator.toList == List.range(0, 100) |> List.reverse
    }

    /////////////////////////////////////////////////////////////////////////////
    // sequence                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sequence01(): Bool =
        let m: Map[Int32, Identity[Char]] = Map#{};
        Map.sequence(m) == Identity.Identity(Map#{})

    @Test
    def sequence02(): Bool =
        let m = Map#{1 => Identity.Identity('a')};
        Map.sequence(m) == Identity.Identity(Map#{1 => 'a'})

    @Test
    def sequence03(): Bool =
        let m = Map#{1 => Identity.Identity('a'), 2 => Identity.Identity('b')};
        Map.sequence(m) == Identity.Identity(Map#{1 => 'a', 2 => 'b'})

    @Test
    def sequence04(): Bool =
        let m = Map#{1 => Identity.Identity('a'), 2 => Identity.Identity('b'), 3 => Identity.Identity('c')};
        Map.sequence(m) == Identity.Identity(Map#{1 => 'a', 2 => 'b', 3 => 'c'})

    /////////////////////////////////////////////////////////////////////////////
    // traverse                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def traverse01(): Bool = region rc {
        let st = ref '#' @ rc;
        let m: Map[Int32, Char] = Map#{};
        let ans = Map.traverse(x -> {st := x; Identity.Identity(x)}, m);
        ans == Identity.Identity(Map#{}) and deref st == '#'
    }

    @Test
    def traverse02(): Bool = region rc {
        let st = ref '#' @ rc;
        let m = Map#{1 => 'a'};
        let ans = Map.traverse(x -> {st := x; Identity.Identity(x)}, m);
        ans == Identity.Identity(Map#{1 => 'a'}) and deref st == 'a'
    }

    @Test
    def traverse03(): Bool = region rc {
        let st = ref '#' @ rc;
        let m = Map#{1 => 'a', 2 => 'b'};
        let ans = Map.traverse(x -> {st := x; Identity.Identity(x)}, m);
        ans == Identity.Identity(Map#{1 => 'a', 2 => 'b'}) and deref st == 'b'
    }

    @Test
    def traverse04(): Bool = region rc {
        let st = ref '#' @ rc;
        let m = Map#{1 => 'a', 2 => 'b', 3 => 'c'};
        let ans = Map.traverse(x -> {st := x; Identity.Identity(x)}, m);
        ans == Identity.Identity(Map#{1 => 'a', 2 => 'b', 3 => 'c'}) and deref st == 'c'
    }

    /////////////////////////////////////////////////////////////////////////////
    // traverseWithKey                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def traverseWithKey01(): Bool = region rc {
        let st = ref (0, '#') @ rc;
        let m: Map[Int32, Char] = Map#{};
        let ans = Map.traverseWithKey((k, v) -> {st := (k, v); Identity.Identity(v)}, m);
        ans == Identity.Identity(Map#{}) and deref st == (0, '#')
    }

    @Test
    def traverseWithKey02(): Bool = region rc {
        let st = ref (0, '#') @ rc;
        let m = Map#{1 => 'a'};
        let ans = Map.traverseWithKey((k, v) -> {st := (k, v); Identity.Identity(v)}, m);
        ans == Identity.Identity(Map#{1 => 'a'}) and deref st == (1, 'a')
    }

    @Test
    def traverseWithKey03(): Bool = region rc {
        let st = ref (0, '#') @ rc;
        let m = Map#{1 => 'a', 2 => 'b'};
        let ans = Map.traverseWithKey((k, v) -> {st := (k, v); Identity.Identity(v)}, m);
        ans == Identity.Identity(Map#{1 => 'a', 2 => 'b'}) and deref st == (2, 'b')
    }

    @Test
    def traverseWithKey04(): Bool = region rc {
        let st = ref (0, '#') @ rc;
        let m = Map#{1 => 'a', 2 => 'b', 3 => 'c'};
        let ans = Map.traverseWithKey((k, v) -> {st := (k, v); Identity.Identity(v)}, m);
        ans == Identity.Identity(Map#{1 => 'a', 2 => 'b', 3 => 'c'}) and deref st == (3, 'c')
    }

    /////////////////////////////////////////////////////////////////////////////
    // joinKeys                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def joinKeys01(): Bool =
        Map.empty(): Map[Int32, Int32] |> Map.joinKeys(",") == ""

    @Test
    def joinKeys02(): Bool =
        Map#{1 => 1} |> Map.joinKeys(",") == "1"

    @Test
    def joinKeys03(): Bool =
        Map#{0 => 1, 1 => 2, 2 => 2} |> Map.joinKeys(",") == "0,1,2"

    @Test
    def joinKeys04(): Bool =
        Map#{"0" => 1, "1" => 2, "2" => 2} |> Map.joinKeys(",") == "0,1,2"


    /////////////////////////////////////////////////////////////////////////////
    // joinValues                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def joinValues01(): Bool =
        Map.empty(): Map[Int32, Int32] |> Map.joinValues(",") == ""

    @Test
    def joinValues02(): Bool =
        Map#{1 => 1} |> Map.joinValues(",") == "1"

    @Test
    def joinValues03(): Bool =
        Map#{0 => 1, 1 => 2, 2 => 2} |> Map.joinValues(",") == "1,2,2"

    @Test
    def joinValues04(): Bool =
        Map#{0 => "1", 1 => "2", 2 => "2"} |> Map.joinValues(",") == "1,2,2"


    /////////////////////////////////////////////////////////////////////////////
    // joinWith                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def joinWith01(): Bool =
        Map.empty(): Map[Int32, Int32] |> Map.joinWith((k, v) -> "${k} => ${v}", ",") == ""

    @Test
    def joinWith02(): Bool =
        Map#{1 => 1} |>
            Map.joinWith((k, v) -> "${k} => ${v}", ", ") == "1 => 1"

    @Test
    def joinWith03(): Bool =
        Map#{0 => 1, 1 => 2, 2 => 2} |>
            Map.joinWith((k, v) -> "${k} => ${v}", ", ") == "0 => 1, 1 => 2, 2 => 2"

    @Test
    def joinWith04(): Bool =
        Map#{0 => "1", 1 => "2", 2 => "2"} |>
            Map.joinWith((k, v) -> "${k} => ${v}", ", ") == "0 => 1, 1 => 2, 2 => 2"


    /////////////////////////////////////////////////////////////////////////////
    // explode                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def explode01(): Bool =
        Map.explode(Map#{1 => Set#{2, 3}}) == Set#{(1, 2), (1, 3)}

    @Test
    def explode02(): Bool =
        Map.explode(Map#{1 => Set#{2, 3}, 2 => Set#{2, 3}}) == Set#{(1, 2), (1, 3), (2, 2), (2, 3)}

    @Test
    def explode03(): Bool =
        Map.explode(Map#{1 => Set#{2, 3}, 2 => Set#{5, 6, 7}}) == Set#{(1, 2), (1, 3), (2, 5), (2, 6), (2, 7)}

    @Test
    def explode04(): Bool =
        Map.explode(Map#{1 => Set#{2, 3}, 2 => Set#{5, 6, 7}, 3 => Set#{0}}) == Set#{(1, 2), (1, 3), (2, 5), (2, 6), (2, 7), (3, 0)}

    /////////////////////////////////////////////////////////////////////////////
    // LowerBound.minValue                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def minValue01(): Bool =
        LowerBound.minValue(): Map[Int32, Int32] == Map.empty()

    /////////////////////////////////////////////////////////////////////////////
    // PartialOrder.lessEqual                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def lessEqual01(): Bool = PartialOrder.lessEqual(Map.empty(): Map[Int32, Int32], Map.empty(): Map[Int32, Int32])

    @Test
    def lessEqual02(): Bool = PartialOrder.lessEqual(Map.empty(): Map[Int32, Int32], Map.singleton(1, 1))

    @Test
    def lessEqual03(): Bool = not PartialOrder.lessEqual(Map.singleton(1, 1), Map.empty(): Map[Int32, Int32])

    @Test
    def lessEqual04(): Bool = PartialOrder.lessEqual(Map.singleton(1, 1), Map.singleton(1, 1))

    @Test
    def lessEqual05(): Bool = not PartialOrder.lessEqual(Map.singleton(1, 1), Map.singleton(1, 10))

    /////////////////////////////////////////////////////////////////////////////
    // JoinLattice.leastUpperBound                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def leastUpperBound01(): Bool =
        JoinLattice.leastUpperBound(Map.empty(): Map[Int32, Int32], Map.empty(): Map[Int32, Int32]) == Map.empty(): Map[Int32, Int32]

    @Test
    def leastUpperBound02(): Bool =
        JoinLattice.leastUpperBound(Map.empty(): Map[Int32, Int32], Map.singleton(1, 1)) == Map.singleton(1, 1)

    @Test
    def leastUpperBound03(): Bool =
        JoinLattice.leastUpperBound(Map.singleton(1, 1), Map.empty(): Map[Int32, Int32]) == Map.singleton(1, 1)

    @Test
    def leastUpperBound04(): Bool =
        JoinLattice.leastUpperBound(Map.singleton(1, 1), Map.singleton(1, 1)) == Map.singleton(1, 1)

    @Test
    def leastUpperBound05(): Bool =
        JoinLattice.leastUpperBound(Map.singleton(1, 1), Map.singleton(1, 10)) == Map.singleton(1, 10)

    @Test
    def leastUpperBound06(): Bool =
        JoinLattice.leastUpperBound(Map.singleton(1, 1), Map.singleton(2, 2)) == Map#{1 => 1, 2 => 2}

    @Test
    def leastUpperBound07(): Bool =
        JoinLattice.leastUpperBound(Map.singleton(1, 1), Map#{1 => 10, 2 => 2}) == Map#{1 => 10, 2 => 2}

    /////////////////////////////////////////////////////////////////////////////
    // MeetLattice.greatestLowerBound                                          //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def greatestLowerBound01(): Bool =
        MeetLattice.greatestLowerBound(Map.empty(): Map[Int32, Int32], Map.empty(): Map[Int32, Int32]) == Map.empty(): Map[Int32, Int32]

    @Test
    def greatestLowerBound02(): Bool =
        MeetLattice.greatestLowerBound(Map.empty(): Map[Int32, Int32], Map.singleton(1, 1)) == Map.empty(): Map[Int32, Int32]

    @Test
    def greatestLowerBound03(): Bool =
        MeetLattice.greatestLowerBound(Map.singleton(1, 1), Map.empty(): Map[Int32, Int32]) == Map.empty(): Map[Int32, Int32]

    @Test
    def greatestLowerBound04(): Bool =
        MeetLattice.greatestLowerBound(Map.singleton(1, 1), Map.singleton(1, 1)) == Map.singleton(1, 1)

    @Test
    def greatestLowerBound05(): Bool =
        MeetLattice.greatestLowerBound(Map.singleton(1, 1), Map.singleton(1, 10)) == Map.singleton(1, 1)

    @Test
    def greatestLowerBound06(): Bool =
        MeetLattice.greatestLowerBound(Map.singleton(1, 1), Map.singleton(2, 2)) == Map.empty(): Map[Int32, Int32]

    @Test
    def greatestLowerBound07(): Bool =
        MeetLattice.greatestLowerBound(Map.singleton(1, 1), Map#{1 => 10, 2 => 2}) == Map.singleton(1, 1)

}

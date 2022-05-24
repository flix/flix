/*
 * Copyright 2022 Stephen Tetley
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

namespace TestBagMultiMap {

    use BagMultiMap.{empty, singleton, insert, toAssocList};

    def isEven(i: Int32): Bool = i rem 2 == 0

    def isOdd(i: Int32): Bool = not isEven(i)

    /////////////////////////////////////////////////////////////////////////////
    // Eq                                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def eq01(): Bool = empty(): BagMultiMap[Int32, Char] == empty(): BagMultiMap[Int32, Char]

    @test
    def eq02(): Bool = singleton(1, 'a') != empty()

    @test
    def eq03(): Bool = empty() != singleton(1, 'a')

    @test
    def eq04(): Bool = singleton(1, 'a') != singleton(1, 'b')

    @test
    def eq05(): Bool = singleton(1, 'a') == singleton(1, 'a')

    @test
    def eq06(): Bool = singleton(1, 'a') |> insert(1, 'b') == singleton(1, 'a') |> insert(1, 'b')

    /// different order, not equal...
    @test
    def eq07(): Bool = singleton(1, 'b') |> insert(1, 'a') != singleton(1, 'a') |> insert(1, 'b')

    /////////////////////////////////////////////////////////////////////////////
    // Order (compare)                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def compare01(): Bool = Order.compare(empty(): BagMultiMap[Int32, Char], empty(): BagMultiMap[Int32, Char]) == EqualTo

    @test
    def compare02(): Bool = Order.compare(singleton(1, 'a'), empty()) == GreaterThan

    @test
    def compare03(): Bool = Order.compare(empty(), singleton(1, 'a')) == LessThan

    @test
    def compare04(): Bool = Order.compare(singleton(1, 'b'), singleton(1, 'a')) == GreaterThan

    @test
    def compare05(): Bool = Order.compare(singleton(1, 'a'), singleton(1, 'b')) == LessThan

    /////////////////////////////////////////////////////////////////////////////
    // empty                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def empty01(): Bool = BagMultiMap.isEmpty(BagMultiMap.empty())

    /////////////////////////////////////////////////////////////////////////////
    // singleton                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def singleton01(): Bool =
        BagMultiMap.singleton(1, 'a') |> toAssocList == (1, 'a' :: Nil) :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // fromMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def fromMap01(): Bool =
        BagMultiMap.fromMap(Map#{}: Map[Int32, Char]) |> toAssocList == Nil

    @test
    def fromMap02(): Bool =
        BagMultiMap.fromMap(Map#{1 => 'a'}) |> toAssocList == (1, 'a' :: Nil) :: Nil

    @test
    def fromMap03(): Bool =
        BagMultiMap.fromMap(Map#{1 => 'a', 2 => 'b'}) |> toAssocList == (1, 'a' :: Nil) :: (2, 'b' :: Nil) :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // isEmpty                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def isEmpty01(): Bool =
        BagMultiMap.isEmpty(empty()) == true

    @test
    def isEmpty02(): Bool =
        BagMultiMap.isEmpty(singleton(1, 'a')) == false

    /////////////////////////////////////////////////////////////////////////////
    // get                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def get01(): Bool =
        BagMultiMap.get(1, empty(): BagMultiMap[Int32, Char]) == Nil

    @test
    def get02(): Bool =
        BagMultiMap.get(1, singleton(1, 'a')) == 'a' :: Nil

    @test
    def get03(): Bool =
        BagMultiMap.get(1, singleton(1, 'a') |> insert(2, 'b')) == 'a' :: Nil

    @test
    def get04(): Bool =
        BagMultiMap.get(1, singleton(1, 'a') |> insert(2, 'b') |> insert(1, 'c')) == 'c' :: 'a' :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // memberOf                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def memberOf01(): Bool =
        not BagMultiMap.memberOf(1, empty(): BagMultiMap[Int32, Char])

    @test
    def memberOf02(): Bool = BagMultiMap.memberOf(1, singleton(1, 'a'))

    @test
    def memberOf03(): Bool = not BagMultiMap.memberOf(1, singleton(2, 'b'))

    @test
    def memberOf04(): Bool = BagMultiMap.memberOf(1, singleton(1, 'a') |> insert(2, 'b') |> insert(1, 'c'))

    /////////////////////////////////////////////////////////////////////////////
    // keysOf                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def keysOf01(): Bool = BagMultiMap.keysOf(empty(): BagMultiMap[Int32, Char]) == Set#{}

    @test
    def keysOf02(): Bool = BagMultiMap.keysOf(singleton(1, 'a')) == Set#{1}

    @test
    def keysOf03(): Bool = BagMultiMap.keysOf(singleton(2, 'b')) == Set#{2}

    @test
    def keysOf04(): Bool =
        BagMultiMap.keysOf(singleton(1, 'a') |> insert(2, 'b') |> insert(1, 'c')) == Set#{1, 2}


    /////////////////////////////////////////////////////////////////////////////
    // valuesOf                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def valuesOf01(): Bool = BagMultiMap.valuesOf(empty(): BagMultiMap[Int32, Char]) == Nil

    @test
    def valuesOf02(): Bool = BagMultiMap.valuesOf(singleton(1, 'a')) |> List.sort == 'a' :: Nil

    @test
    def valuesOf03(): Bool = BagMultiMap.valuesOf(singleton(2, 'b')) |> List.sort == 'b' :: Nil

    @test
    def valuesOf04(): Bool =
        BagMultiMap.valuesOf(singleton(1, 'a') |> insert(2, 'b') |> insert(1, 'c'))
            |> List.sort == 'a' :: 'b' :: 'c' :: Nil

    @test
    def valuesOf05(): Bool =
        BagMultiMap.valuesOf(singleton(1, 'a') |> insert(2, 'b') |> insert(1, 'c') |> insert(3, 'a'))
            |> List.sort == 'a' :: 'a' :: 'b' :: 'c' :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // insert                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def insert01(): Bool =
        BagMultiMap.insert(1, 'a', empty()) |> toAssocList == (1, 'a' :: Nil) :: Nil

    @test
    def insert02(): Bool =
        empty() |> BagMultiMap.insert(1, 'a') |> BagMultiMap.insert(2, 'b')
            |> toAssocList == (1, 'a' :: Nil) :: (2, 'b' :: Nil) :: Nil

    @test
    def insert03(): Bool =
        empty() |> BagMultiMap.insert(1, 'a') |> BagMultiMap.insert(2, 'b') |> BagMultiMap.insert(1, 'c')
            |> toAssocList == (1, 'c' :: 'a' :: Nil) :: (2, 'b' :: Nil) :: Nil

    @test
    def insert04(): Bool =
        empty() |> BagMultiMap.insert(1, 'a') |> BagMultiMap.insert(2, 'b') |> BagMultiMap.insert(1, 'c') |> BagMultiMap.insert(3, 'b')
            |> toAssocList == (1, 'c' :: 'a' :: Nil) :: (2, 'b' :: Nil) :: (3, 'b' :: Nil) :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // insertAll                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def insertAll01(): Bool =
        BagMultiMap.insertAll(1, Nil: List[Char], empty()) == empty()

    @test
    def insertAll02(): Bool =
        empty() |> BagMultiMap.insertAll(1, 'a' :: Nil)|> toAssocList == (1, 'a' :: Nil) :: Nil

    @test
    def insertAll03(): Bool =
        empty() |> BagMultiMap.insertAll(1, 'a' :: 'c' :: Nil) |> BagMultiMap.insertAll(2, 'b' :: Nil)
            |> toAssocList == (1, 'a' :: 'c' :: Nil) :: (2, 'b' :: Nil) :: Nil

    @test
    def insertAll04(): Bool =
        empty() |> BagMultiMap.insertAll(1, 'a' :: 'c' :: Nil) |> BagMultiMap.insertAll(2, 'b' :: Nil) |> BagMultiMap.insertAll(3, 'b' :: Nil)
            |> toAssocList == (1, 'a' :: 'c' :: Nil) :: (2, 'b' :: Nil) :: (3, 'b' :: Nil) :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // adjust                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def adjust01(): Bool = BagMultiMap.adjust(v -> 2*v, 1, empty()) == empty()

    @test
    def adjust02(): Bool =
        BagMultiMap.adjust(v -> 2*v, 1, BagMultiMap.singleton(1, 4)) |> toAssocList == (1, 8 :: Nil) :: Nil

    @test
    def adjust03(): Bool =
        BagMultiMap.adjust(v -> 2*v, 3, singleton(1, 4)) |> toAssocList == (1, 4 :: Nil) :: Nil

    @test
    def adjust04(): Bool =
        BagMultiMap.adjust(v -> 2*v, 1, singleton(1, 4) |> insert(2, 4) |> insert(1, 5))
            |> toAssocList == (1, 10 :: 8 :: Nil) :: (2, 4 :: Nil) :: Nil

    @test
    def adjust05(): Bool =
        BagMultiMap.adjust(v -> 2*v, 2, singleton(1, 4) |> insert(2, 4) |> insert(1, 5))
            |> toAssocList == (1, 5 :: 4 :: Nil) :: (2, 8 :: Nil) :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // adjustWithKey                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def adjustWithKey01(): Bool = BagMultiMap.adjustWithKey((k, v) -> k+v, 1, empty()) == empty()

    @test
    def adjustWithKey02(): Bool =
        BagMultiMap.adjustWithKey((k, v) -> k+v, 1, singleton(1, 4))
            |> toAssocList == (1, 5 :: Nil) :: Nil

    @test
    def adjustWithKey03(): Bool =
        BagMultiMap.adjustWithKey((k, v) -> k+v, 3, singleton(1, 4))
            |> toAssocList == (1, 4 :: Nil) :: Nil

    @test
    def adjustWithKey04(): Bool =
        BagMultiMap.adjustWithKey((k, v) -> k+v, 1, singleton(1, 4) |> insert(2, 4) |> insert(1, 5))
            |> toAssocList == (1, 6 :: 5 :: Nil) :: (2, 4 :: Nil) :: Nil

    @test
    def adjustWithKey05(): Bool =
        BagMultiMap.adjustWithKey((k, v) -> k+v, 2, singleton(1, 4) |> insert(2, 4) |> insert(1, 5))
            |> toAssocList == (1, 5 :: 4 :: Nil) :: (2, 6 :: Nil) :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // update                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def update01(): Bool = BagMultiMap.update(v -> if (isEven(v)) Some(2*v) else None, 1, empty()) == empty()

    @test
    def update02(): Bool =
        BagMultiMap.update(v -> if (isEven(v)) Some(2*v) else None, 1, singleton(1, 4))
            |> toAssocList == (1, 8 :: Nil) :: Nil

    @test
    def update03(): Bool =
        BagMultiMap.update(v -> if (isEven(v)) Some(2*v) else None, 3, singleton(1, 4))
            |> toAssocList == (1, 4 :: Nil) :: Nil

    @test
    def update04(): Bool =
        BagMultiMap.update(v -> if (isEven(v)) Some(2*v) else None, 1, singleton(1, 4) |> insert(2, 4) |> insert(1, 5))
            |> toAssocList == (1, 5 :: 8 :: Nil) :: (2, 4 :: Nil) :: Nil

    @test
    def update05(): Bool =
        BagMultiMap.update(v -> if (isEven(v)) Some(2*v) else None, 2, singleton(1, 4) |> insert(2, 4) |> insert(1, 5))
            |> toAssocList == (1, 5 :: 4 :: Nil) :: (2, 8 :: Nil) :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // updateWithKey                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def updateWithKey01(): Bool = BagMultiMap.updateWithKey((k, v) -> if (isEven(k)) Some(k+v) else None, 1, empty()) == empty()

    @test
    def updateWithKey02(): Bool =
        BagMultiMap.updateWithKey((k, v) -> if (isEven(k)) Some(k+v) else None, 1, singleton(1, 4))
            |> toAssocList == (1, 4 :: Nil) :: Nil

    @test
    def updateWithKey03(): Bool =
        BagMultiMap.updateWithKey((k, v) -> if (isOdd(k)) Some(k+v) else None, 1, singleton(1, 4))
            |> toAssocList == (1, 5 :: Nil) :: Nil

    @test
    def updateWithKey04(): Bool =
        BagMultiMap.updateWithKey((k, v) -> if (isEven(k)) Some(k+v) else None, 3, singleton(1, 4))
            |> toAssocList == (1, 4 :: Nil) :: Nil

    @test
    def updateWithKey05(): Bool =
        BagMultiMap.updateWithKey((k, v) -> if (isOdd(k)) Some(k+v) else None, 3, singleton(1, 4))
            |> toAssocList == (1, 4 :: Nil) :: Nil

    @test
    def updateWithKey06(): Bool =
        BagMultiMap.updateWithKey((k, v) -> if (isEven(k)) Some(k+v) else None, 1, singleton(1, 4) |> insert(2, 4) |> insert(1, 5))
            |> toAssocList == (1, 5 :: 4 :: Nil) :: (2, 4 :: Nil) :: Nil

    @test
    def updateWithKey07(): Bool =
        BagMultiMap.updateWithKey((k, v) -> if (isOdd(k)) Some(k+v) else None, 1, singleton(1, 4) |> insert(2, 4) |> insert(1, 5))
            |> toAssocList == (1, 6 :: 5 :: Nil) :: (2, 4 :: Nil) :: Nil

    @test
    def updateWithKey08(): Bool =
        BagMultiMap.updateWithKey((k, v) -> if (isEven(k)) Some(k+v) else None, 2, singleton(1, 4) |> insert(2, 4) |> insert(1, 5))
            |> toAssocList == (1, 5 :: 4 :: Nil) :: (2, 6 :: Nil) :: Nil

    @test
    def updateWithKey09(): Bool =
        BagMultiMap.updateWithKey((k, v) -> if (isOdd(k)) Some(k+v) else None, 2, singleton(1, 4) |> insert(2, 4) |> insert(1, 5))
            |> toAssocList == (1, 5 :: 4 :: Nil) :: (2, 4 :: Nil) :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // remove                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def remove01(): Bool = BagMultiMap.remove(1, empty(): BagMultiMap[Int32, Int32]) == empty()

    @test
    def remove02(): Bool = BagMultiMap.remove(1, singleton(1, 4)) |> toAssocList == Nil

    @test
    def remove03(): Bool = BagMultiMap.remove(3, singleton(1, 4)) |> toAssocList == (1, 4 :: Nil) :: Nil

    @test
    def remove04(): Bool =
        BagMultiMap.remove(1, singleton(1, 4) |> insert(2, 4) |> insert(1, 5)) |> toAssocList == (2, 4 :: Nil) :: Nil

    @test
    def remove05(): Bool =
        BagMultiMap.remove(2, singleton(1, 4) |> insert(2, 4) |> insert(1, 5))
            |> toAssocList == (1, 5 :: 4 :: Nil) :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // find                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def find01(): Bool = BagMultiMap.find((k, v) -> k == v, empty(): BagMultiMap[Int32, Int32]) == None

    @test
    def find02(): Bool = BagMultiMap.find((k, v) -> k == v, singleton(1, 2)) == None

    @test
    def find03(): Bool = BagMultiMap.find((k, v) -> k == v, singleton(1, 1)) == Some((1, 1))

    @test
    def find04(): Bool =
        BagMultiMap.find((k, v) -> k == v, singleton(1, 4) |> insert(2, 4) |> insert(1, 5)) == None

    @test
    def find05(): Bool =
        BagMultiMap.find((k, v) -> k == v, singleton(1, 4) |> insert(2, 2) |> insert(3, 3)) == Some((2,2))

    /////////////////////////////////////////////////////////////////////////////
    // findLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def findLeft01(): Bool = BagMultiMap.findLeft((k, v) -> k == v, empty(): BagMultiMap[Int32, Int32]) == None

    @test
    def findLeft02(): Bool = BagMultiMap.findLeft((k, v) -> k == v, singleton(1, 2)) == None

    @test
    def findLeft03(): Bool = BagMultiMap.findLeft((k, v) -> k == v, singleton(1, 1)) == Some((1, 1))

    @test
    def findLeft04(): Bool =
        BagMultiMap.findLeft((k, v) -> k == v, singleton(1, 4) |> insert(2, 4) |> insert(1, 5)) == None

    @test
    def findLeft05(): Bool =
        BagMultiMap.findLeft((k, v) -> k == v, singleton(1, 4) |> insert(2, 2) |> insert(3, 3)) == Some((2, 2))

    /////////////////////////////////////////////////////////////////////////////
    // findRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def findRight01(): Bool = BagMultiMap.findRight((k, v) -> k == v, empty(): BagMultiMap[Int32, Int32]) == None

    @test
    def findRight02(): Bool = BagMultiMap.findRight((k, v) -> k == v, singleton(1, 2)) == None

    @test
    def findRight03(): Bool = BagMultiMap.findRight((k, v) -> k == v, singleton(1, 1)) == Some((1, 1))

    @test
    def findRight04(): Bool =
        BagMultiMap.findRight((k, v) -> k == v, singleton(1, 4) |> insert(2, 4) |> insert(1, 5)) == None

    @test
    def findRight05(): Bool =
        BagMultiMap.findRight((k, v) -> k == v, singleton(1, 4) |> insert(2, 2) |> insert(3, 3)) == Some((3, 3))

    /////////////////////////////////////////////////////////////////////////////
    // filter                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filter01(): Bool =
        BagMultiMap.filter(isEven, empty(): BagMultiMap[Int32, Int32]) == empty()

    @test
    def filter02(): Bool = BagMultiMap.filter(isEven, singleton(1, 2)) |> toAssocList == (1, 2 :: Nil) :: Nil

    @test
    def filter03(): Bool = BagMultiMap.filter(isEven, singleton(1, 1)) == empty()

    @test
    def filter04(): Bool =
        BagMultiMap.filter(isEven, singleton(1, 4) |> insert(2, 4))
            |> toAssocList == (1, 4 :: Nil) :: (2, 4 :: Nil) :: Nil

    @test
    def filter05(): Bool =
        BagMultiMap.filter(isEven, singleton(1, 4) |> insert(2, 2) |> insert(1, 3))
            |> toAssocList == (1, 4 :: Nil) :: (2, 2 :: Nil) :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // filterWithKey                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filterWithKey01(): Bool =
        BagMultiMap.filterWithKey((k, v) -> k == v, empty(): BagMultiMap[Int32, Int32]) == empty()

    @test
    def filterWithKey02(): Bool = BagMultiMap.filterWithKey((k, v) -> k == v, singleton(1, 2)) == empty()

    @test
    def filterWithKey03(): Bool =
        BagMultiMap.filterWithKey((k, v) -> k == v, singleton(1, 1)) |> toAssocList == (1, 1 :: Nil) :: Nil

    @test
    def filterWithKey04(): Bool =
        BagMultiMap.filterWithKey((k, v) -> k == v, singleton(1, 4) |> insert(2, 4)) == empty()

    @test
    def filterWithKey05(): Bool =
        BagMultiMap.filterWithKey((k, v) -> k == v, singleton(1, 4) |> insert(2, 2) |> insert(1, 1))
            |> toAssocList == (1, 1 :: Nil) :: (2, 2 :: Nil) :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // map                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def map01(): Bool = BagMultiMap.map(v -> v*3, empty(): BagMultiMap[Int32, Int32]) == empty()

    @test
    def map02(): Bool =
        BagMultiMap.map(v -> v*3, singleton(1, 2)) |> toAssocList == (1, 6 :: Nil) :: Nil

    @test
    def map03(): Bool =
        BagMultiMap.map(v -> v*3, singleton(1, 1)) |> toAssocList == (1, 3 :: Nil) :: Nil

    @test
    def map04(): Bool =
        BagMultiMap.map(v -> v*3, singleton(1, 4) |> insert(2, 4))
            |> toAssocList == (1, 12 :: Nil) :: (2, 12 :: Nil) :: Nil

    @test
    def map05(): Bool =
        BagMultiMap.map(v -> v*3, singleton(1, 4) |> insert(2, 2) |> insert(1, 1))
            |> toAssocList == (1, 3 :: 12 :: Nil) :: (2, 6 :: Nil) :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // mapWithKey                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapWithKey01(): Bool =
        BagMultiMap.mapWithKey((k, v) -> v*3+k, empty(): BagMultiMap[Int32, Int32]) == empty()

    @test
    def mapWithKey02(): Bool =
        BagMultiMap.mapWithKey((k, v) -> v*3+k, singleton(1, 2)) |> toAssocList == (1, 7 :: Nil) :: Nil

    @test
    def mapWithKey03(): Bool =
        BagMultiMap.mapWithKey((k, v) -> v*3+k, singleton(1, 1)) |> toAssocList == (1, 4 :: Nil) :: Nil

    @test
    def mapWithKey04(): Bool =
        BagMultiMap.mapWithKey((k, v) -> v*3+k, singleton(1, 4) |> insert(2, 4))
            |> toAssocList == (1, 13 :: Nil) :: (2, 14 :: Nil) :: Nil

    @test
    def mapWithKey05(): Bool =
        BagMultiMap.mapWithKey((k, v) -> v*3+k, singleton(1, 4) |> insert(2, 2) |> insert(1, 1))
            |> toAssocList == (1, 4 :: 13 :: Nil) :: (2, 8 :: Nil) :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // foldWithKey                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldWithKey01(): Bool =
        BagMultiMap.foldWithKey((acc, k, v) -> acc + k + v, 0, empty(): BagMultiMap[Int32, Int32]) == 0

    @test
    def foldWithKey02(): Bool =
        BagMultiMap.foldWithKey((acc, k, v) -> acc + k + v, 0, singleton(1, 2)) == 3

    @test
    def foldWithKey03(): Bool =
        BagMultiMap.foldWithKey((acc, k, v) -> acc + k + v, 0, singleton(1, 4) |> insert(2, 4)) == 11

    @test
    def foldWithKey04(): Bool =
        BagMultiMap.foldWithKey((acc, k, v) -> acc + k + v, 0,
            singleton(1, 4) |> insert(2, 2) |> insert(1, 1)) == 11

    @test
    def foldWithKey05(): Bool =
        BagMultiMap.foldWithKey((acc, k, v) -> acc + k + v, 0,
            singleton(1, 4) |> insert(2, 2) |> insert(1, 1) |> insert(3, 3)) == 17

    /////////////////////////////////////////////////////////////////////////////
    // foldLeft                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldLeft01(): Bool =
        BagMultiMap.foldLeft((acc, v) -> v :: acc, Nil, empty(): BagMultiMap[Int32, Int32]) == Nil

    @test
    def foldLeft02(): Bool =
        BagMultiMap.foldLeft((acc, v) -> v :: acc, Nil, singleton(1, 2)) == 2 :: Nil

    @test
    def foldLeft03(): Bool =
        BagMultiMap.foldLeft((acc, v) -> v :: acc, Nil, singleton(1, 4) |> insert(2, 5)) == 5 :: 4 :: Nil

    @test
    def foldLeft04(): Bool =
        BagMultiMap.foldLeft((acc, v) -> v :: acc, Nil,
            singleton(1, 4) |> insert(2, 2) |> insert(1, 1)) == 2 :: 4 :: 1 :: Nil

    @test
    def foldLeft05(): Bool =
        BagMultiMap.foldLeft((acc, v) -> v :: acc, Nil,
            singleton(1, 4) |> insert(2, 2) |> insert(1, 1) |> insert(3, 3)) == 3 :: 2 :: 4 :: 1 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // foldLeftWithKey                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldLeftWithKey01(): Bool =
        BagMultiMap.foldLeftWithKey((acc, k, v) -> (k, v) :: acc, Nil, empty(): BagMultiMap[Int32, Int32]) == Nil

    @test
    def foldLeftWithKey02(): Bool =
        BagMultiMap.foldLeftWithKey((acc, k, v) -> (k, v) :: acc, Nil, singleton(1, 2)) == (1, 2) :: Nil

    @test
    def foldLeftWithKey03(): Bool =
        BagMultiMap.foldLeftWithKey((acc, k, v) -> (k, v) :: acc, Nil,
            singleton(1, 4) |> insert(2, 5)) == (2, 5) :: (1, 4) :: Nil

    @test
    def foldLeftWithKey04(): Bool =
        BagMultiMap.foldLeftWithKey((acc, k, v) -> (k, v) :: acc, Nil,
            singleton(1, 4) |> insert(2, 2) |> insert(1, 1)) == (2, 2) :: (1, 4) :: (1, 1) :: Nil

    @test
    def foldLeftWithKey05(): Bool =
        BagMultiMap.foldLeftWithKey((acc, k, v) -> (k, v) :: acc, Nil,
            singleton(1, 4) |> insert(2, 2) |> insert(1, 1) |> insert(3, 3)) == (3, 3) :: (2, 2) :: (1, 4) :: (1, 1) :: Nil

    @test
    def foldLeftWithKey06(): Bool =
        BagMultiMap.foldLeftWithKey((acc, k, v) -> (k, v) :: acc, Nil,
            singleton(1, 4) |> insert(2, 2) |> insert(1, 1) |> insert(3, 3) |> insert(1, 2)) == (3, 3) :: (2, 2) :: (1, 4) :: (1, 1) :: (1, 2) :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // foldRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldRight01(): Bool =
        BagMultiMap.foldRight((v, acc) -> v :: acc, Nil, empty(): BagMultiMap[Int32, Int32]) == Nil

    @test
    def foldRight02(): Bool =
        BagMultiMap.foldRight((v, acc) -> v :: acc, Nil, singleton(1, 2)) == 2 :: Nil

    @test
    def foldRight03(): Bool =
        BagMultiMap.foldRight((v, acc) -> v :: acc, Nil, singleton(1, 4) |> insert(2, 5)) == 4 :: 5 :: Nil

    @test
    def foldRight04(): Bool =
        BagMultiMap.foldRight((v, acc) -> v :: acc, Nil,
            singleton(1, 4) |> insert(2, 2) |> insert(1, 1)) == 1 :: 4 :: 2 :: Nil

    @test
    def foldRight05(): Bool =
        BagMultiMap.foldRight((v, acc) -> v :: acc, Nil,
            singleton(1, 4) |> insert(2, 2) |> insert(1, 1) |> insert(3, 3)) == 1 :: 4 :: 2 :: 3 :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // foldRightWithKey                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldRightWithKey01(): Bool =
        BagMultiMap.foldRightWithKey((k, v, acc) -> (k, v) :: acc, Nil, empty(): BagMultiMap[Int32, Int32]) == Nil

    @test
    def foldRightWithKey02(): Bool =
        BagMultiMap.foldRightWithKey((k, v, acc) -> (k, v) :: acc, Nil, singleton(1, 2)) == (1, 2) :: Nil

    @test
    def foldRightWithKey03(): Bool =
        BagMultiMap.foldRightWithKey((k, v, acc) -> (k, v) :: acc, Nil,
            singleton(1, 4) |> insert(2, 5)) == (1, 4) :: (2, 5) :: Nil

    @test
    def foldRightWithKey04(): Bool =
        BagMultiMap.foldRightWithKey((k, v, acc) -> (k, v) :: acc, Nil,
            singleton(1, 4) |> insert(2, 2) |> insert(1, 1)) == (1, 1) :: (1, 4) :: (2, 2) :: Nil

    @test
    def foldRightWithKey05(): Bool =
        BagMultiMap.foldRightWithKey((k, v, acc) -> (k, v) :: acc, Nil,
            singleton(1, 4) |> insert(2, 2) |> insert(1, 1) |> insert(3, 3)) == (1, 1) :: (1, 4) :: (2, 2) :: (3, 3) :: Nil

    @test
    def foldRightWithKey06(): Bool =
        BagMultiMap.foldRightWithKey((k, v, acc) -> (k, v) :: acc, Nil,
            singleton(1, 4) |> insert(2, 2) |> insert(1, 1) |> insert(3, 3) |> insert(1, 2)) == (1, 2) :: (1, 1) :: (1, 4) :: (2, 2) :: (3, 3) :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // reduceLeft                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reduceLeft01(): Bool =
        BagMultiMap.reduceLeft((acc, v) -> acc - v, empty(): BagMultiMap[Int32, Int32]) == None

    @test
    def reduceLeft02(): Bool =
        BagMultiMap.reduceLeft((acc, v) -> acc - v, singleton(1, 2)) == Some(2)

    @test
    def reduceLeft03(): Bool =
        BagMultiMap.reduceLeft((acc, v) -> acc - v, singleton(1, 4) |> insert(2, 5)) == Some(-1)

    /// 1 - 4 - 2
    @test
    def reduceLeft04(): Bool =
        BagMultiMap.reduceLeft((acc, v) -> acc - v,
            singleton(1, 4) |> insert(2, 2) |> insert(1, 1)) == Some(-5)

    /// 1 - 4 - 2 - 3
    @test
    def reduceLeft05(): Bool =
        BagMultiMap.reduceLeft((acc, v) -> acc - v,
            singleton(1, 4) |> insert(2, 2) |> insert(1, 1) |> insert(3, 3)) == Some(-8)

    /// First value
    @test
    def reduceLeft06(): Bool =
        BagMultiMap.reduceLeft((v1, _) -> v1,
            singleton(1, 4) |> insert(2, 2) |> insert(1, 1) |> insert(3, 3)) == Some(1)

    /// Last value
    @test
    def reduceLeft07(): Bool =
        BagMultiMap.reduceLeft((_, v2) -> v2,
            singleton(1, 4) |> insert(2, 2) |> insert(1, 1) |> insert(3, 3)) == Some(3)


    /////////////////////////////////////////////////////////////////////////////
    // reduceLeftWithKey                                                       //
    /////////////////////////////////////////////////////////////////////////////

    /// Leftmost key, rightmost value

    @test
    def reduceLeftWithKey01(): Bool =
        BagMultiMap.reduceLeftWithKey((k1, _, _, v2) -> (k1, v2), empty(): BagMultiMap[Int32, Int32]) == None

    @test
    def reduceLeftWithKey02(): Bool =
        BagMultiMap.reduceLeftWithKey((k1, _, _, v2) -> (k1, v2), singleton(1, 2)) == Some((1, 2))

    @test
    def reduceLeftWithKey03(): Bool =
        BagMultiMap.reduceLeftWithKey((k1, _, _, v2) -> (k1, v2), singleton(1, 1)) == Some((1, 1))

    @test
    def reduceLeftWithKey04(): Bool =
        BagMultiMap.reduceLeftWithKey((k1, _, _, v2) -> (k1, v2),
            singleton(1, 4) |> insert(2, 5)) == Some((1, 5))

    @test
    def reduceLeftWithKey05(): Bool =
        BagMultiMap.reduceLeftWithKey((k1, _, _, v2) -> (k1, v2),
            singleton(1, 4) |> insert(2, 2) |> insert(1, 1)) == Some((1, 2))

    /////////////////////////////////////////////////////////////////////////////
    // reduceRight                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reduceRight01(): Bool =
        BagMultiMap.reduceRight((v, acc) -> acc - v, empty(): BagMultiMap[Int32, Int32]) == None

    @test
    def reduceRight02(): Bool =
        BagMultiMap.reduceRight((v, acc) -> acc - v, singleton(1, 2)) == Some(2)

    @test
    def reduceRight03(): Bool =
        BagMultiMap.reduceRight((v, acc) -> acc - v, singleton(1, 4) |> insert(2, 5)) == Some(1)

    @test
    def reduceRight04(): Bool =
        BagMultiMap.reduceRight((v, acc) -> acc - v,
            singleton(1, 4) |> insert(2, 2) |> insert(1, 1)) == Some(-3)

    /// 3 - 2 - 4 - 1
    @test
    def reduceRight05(): Bool =
        BagMultiMap.reduceRight((v, acc) -> acc - v,
            singleton(1, 4) |> insert(2, 2) |> insert(1, 1) |> insert(3, 3)) == Some(-4)

    /// First value
    @test
    def reduceRight06(): Bool =
        BagMultiMap.reduceRight((_, acc) -> acc,
            singleton(1, 4) |> insert(2, 2) |> insert(1, 1) |> insert(3, 3)) == Some(3)

    /// Last value
    @test
    def reduceRight07(): Bool =
        BagMultiMap.reduceRight((v, _) -> v,
            singleton(1, 4) |> insert(2, 2) |> insert(1, 1) |> insert(3, 3)) == Some(1)


    /////////////////////////////////////////////////////////////////////////////
    // reduceRightWithKey                                                      //
    /////////////////////////////////////////////////////////////////////////////

    /// Rightmost key, leftmost value

    @test
    def reduceRightWithKey01(): Bool =
        BagMultiMap.reduceRightWithKey((_, v1, k2, _) -> (k2, v1), empty(): BagMultiMap[Int32, Int32]) == None

    @test
    def reduceRightWithKey02(): Bool =
        BagMultiMap.reduceRightWithKey((_, v1, k2, _) -> (k2, v1), singleton(1, 2)) == Some((1, 2))

    @test
    def reduceRightWithKey03(): Bool =
        BagMultiMap.reduceRightWithKey((_, v1, k2, _) -> (k2, v1),
            singleton(1, 4) |> insert(2, 5)) == Some((2, 4))

    @test
    def reduceRightWithKey04(): Bool =
        BagMultiMap.reduceRightWithKey((_, v1, k2, _) -> (k2, v1),
            singleton(1, 4) |> insert(2, 2) |> insert(1, 1)) == Some((2, 1))

    @test
    def reduceRightWithKey05(): Bool =
        BagMultiMap.reduceRightWithKey((_, v1, k2, _) -> (k2, v1),
            singleton(1, 4) |> insert(2, 2) |> insert(1, 1) |> insert(3, 3)) == Some((3, 1))

    /////////////////////////////////////////////////////////////////////////////
    // count                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def count01(): Bool =
        BagMultiMap.count((k, v) -> v > k, empty(): BagMultiMap[Int32, Int32]) == 0

    @test
    def count02(): Bool =
        BagMultiMap.count((k, v) -> v > k, singleton(1, 2)) == 1

    @test
    def count03(): Bool =
        BagMultiMap.count((k, v) -> v > k, singleton(1, 4) |> insert(2, 4)) == 2

    @test
    def count04(): Bool =
        BagMultiMap.count((k, v) -> v > k,
            singleton(1, 4) |> insert(2, 2) |> insert(1, 1)) == 1

    @test
    def count05(): Bool =
        BagMultiMap.count((k, v) -> v > k,
            singleton(1, 4) |> insert(2, 2) |> insert(1, 1) |> insert(3, 3)) == 1


    /////////////////////////////////////////////////////////////////////////////
    // sumKeys                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sumKeys01(): Bool = BagMultiMap.sumKeys(empty(): BagMultiMap[Int32, Int32]) == 0

    @test
    def sumKeys02(): Bool = BagMultiMap.sumKeys(singleton(1, 2)) == 1

    @test
    def sumKeys03(): Bool = BagMultiMap.sumKeys(singleton(1, 4) |> insert(2, 4)) == 3

    @test
    def sumKeys04(): Bool =
        BagMultiMap.sumKeys(singleton(1, 4) |> insert(2, 2) |> insert(1, 1)) == 4

    @test
    def sumKeys05(): Bool =
        BagMultiMap.sumKeys(singleton(1, 4) |> insert(2, 2) |> insert(1, 1) |> insert(3, 3)) == 7

    /////////////////////////////////////////////////////////////////////////////
    // sumValues                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sumValues01(): Bool = BagMultiMap.sumValues(empty(): BagMultiMap[Int32, Int32]) == 0

    @test
    def sumValues02(): Bool = BagMultiMap.sumValues(singleton(1, 2)) == 2

    @test
    def sumValues03(): Bool = BagMultiMap.sumValues(singleton(1, 4) |> insert(2, 4)) == 8

    @test
    def sumValues04(): Bool =
        BagMultiMap.sumValues(singleton(1, 4) |> insert(2, 2) |> insert(1, 1)) == 7

    @test
    def sumValues05(): Bool =
        BagMultiMap.sumValues(singleton(1, 4) |> insert(2, 2) |> insert(1, 1) |> insert(3, 3)) == 10

    /////////////////////////////////////////////////////////////////////////////
    // sumWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sumWith01(): Bool = BagMultiMap.sumWith((k, v) -> if (v > k) 10 else 0, empty(): BagMultiMap[Int32, Int32]) == 0

    @test
    def sumWith02(): Bool = BagMultiMap.sumWith((k, v) -> if (v > k) 10 else 0, singleton(1, 2)) == 10

    @test
    def sumWith03(): Bool =
        BagMultiMap.sumWith((k, v) -> if (v > k) 10 else 0, singleton(1, 4) |> insert(2, 4)) == 20

    @test
    def sumWith04(): Bool =
        BagMultiMap.sumWith((k, v) -> if (v > k) 10 else 0,
            singleton(1, 4) |> insert(2, 2) |> insert(1, 1)) == 10

    @test
    def sumWith05(): Bool =
        BagMultiMap.sumWith((k, v) -> if (v > k) 10 else 0,
            singleton(1, 4) |> insert(2, 2) |> insert(1, 1) |> insert(3, 3)) == 10

    /////////////////////////////////////////////////////////////////////////////
    // productKeys                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def productKeys01(): Bool = BagMultiMap.productKeys(empty(): BagMultiMap[Int32, Int32]) == 0

    @test
    def productKeys02(): Bool = BagMultiMap.productKeys(singleton(1, 2)) == 1

    @test
    def productKeys03(): Bool = BagMultiMap.productKeys(singleton(1, 4) |> insert(2, 4)) == 2

    @test
    def productKeys04(): Bool =
        BagMultiMap.productKeys(singleton(1, 4) |> insert(2, 2) |> insert(1, 1)) == 2

    @test
    def productKeys05(): Bool =
        BagMultiMap.productKeys(singleton(1, 4) |> insert(2, 2) |> insert(1, 1) |> insert(3, 3)) == 6

    /////////////////////////////////////////////////////////////////////////////
    // productValues                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def productValues01(): Bool = BagMultiMap.productValues(empty(): BagMultiMap[Int32, Int32]) == 0

    @test
    def productValues02(): Bool = BagMultiMap.productValues(singleton(1, 2)) == 2

    @test
    def productValues03(): Bool = BagMultiMap.productValues(singleton(1, 4) |> insert(2, 4)) == 16

    @test
    def productValues04(): Bool =
        BagMultiMap.productValues(singleton(1, 4) |> insert(2, 2) |> insert(1, 1)) == 8

    @test
    def productValues05(): Bool =
        BagMultiMap.productValues(singleton(1, 4) |> insert(2, 2) |> insert(1, 1) |> insert(3, 3)) == 24

    /////////////////////////////////////////////////////////////////////////////
    // productWith                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def productWith01(): Bool = BagMultiMap.productWith((k, v) -> if (v > k) 10 else 1, empty(): BagMultiMap[Int32, Int32]) == 0

    @test
    def productWith02(): Bool = BagMultiMap.productWith((k, v) -> if (v > k) 10 else 1, singleton(1, 2)) == 10

    @test
    def productWith03(): Bool =
        BagMultiMap.productWith((k, v) -> if (v > k) 10 else 1, singleton(1, 4) |> insert(2, 4)) == 100

    @test
    def productWith04(): Bool =
        BagMultiMap.productWith((k, v) -> if (v > k) 10 else 1,
            singleton(1, 4) |> insert(2, 2) |> insert(1, 1)) == 10

    @test
    def productWith05(): Bool =
        BagMultiMap.productWith((k, v) -> if (v > k) 10 else 1,
            singleton(1, 4) |> insert(2, 2) |> insert(1, 1) |> insert(3, 3)) == 10

    /////////////////////////////////////////////////////////////////////////////
    // exists                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def exists01(): Bool = not BagMultiMap.exists((k, v) -> v > k, empty(): BagMultiMap[Int32, Int32])

    @test
    def exists02(): Bool = BagMultiMap.exists((k, v) -> v > k, singleton(1, 2))

    @test
    def exists03(): Bool = BagMultiMap.exists((k, v) -> v > k, singleton(1, 4) |> insert(2, 4))

    @test
    def exists04(): Bool =
        BagMultiMap.exists((k, v) -> v > k, singleton(1, 4) |> insert(2, 2) |> insert(1, 1))

    @test
    def exists05(): Bool =
        BagMultiMap.exists((k, v) -> v > k, singleton(1, 4) |> insert(2, 2) |> insert(1, 1) |> insert(3, 3))

    /////////////////////////////////////////////////////////////////////////////
    // forall                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def forall01(): Bool = BagMultiMap.forall((k, v) -> v > k, empty(): BagMultiMap[Int32, Int32])

    @test
    def forall02(): Bool = BagMultiMap.forall((k, v) -> v > k, singleton(1, 2))

    @test
    def forall03(): Bool = BagMultiMap.forall((k, v) -> v > k, singleton(1, 4) |> insert(2, 4))

    @test
    def forall04(): Bool =
        not BagMultiMap.forall((k, v) -> v > k, singleton(1, 4) |> insert(2, 2) |> insert(1, 1))

    @test
    def forall05(): Bool =
        not BagMultiMap.forall((k, v) -> v > k, singleton(1, 4) |> insert(2, 2) |> insert(1, 1)  |> insert(3, 3))

    @test
    def forall06(): Bool =
        BagMultiMap.forall((k, v) -> v > k, singleton(1, 4) |> insert(2, 3) |> insert(1, 2)  |> insert(3, 4))

    /////////////////////////////////////////////////////////////////////////////
    // union                                                                   //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def union01(): Bool = BagMultiMap.union(empty(): BagMultiMap[Int32, Int32], empty()) == empty()

    @test
    def union02(): Bool = BagMultiMap.union(singleton(1, 2), empty()) == singleton(1, 2)

    @test
    def union03(): Bool = BagMultiMap.union(empty(), singleton(1, 2)) == singleton(1, 2)

    @test
    def union04(): Bool =
        BagMultiMap.union(empty(), singleton(1, 2) |> insert(2, 4)) |> toAssocList == (1, 2 :: Nil) :: (2, 4 :: Nil) :: Nil

    @test
    def union05(): Bool =
        BagMultiMap.union(singleton(1, 2) |> insert(2, 4), empty()) |> toAssocList == (1, 2 :: Nil) :: (2, 4 :: Nil) :: Nil

    @test
    def union06(): Bool =
        BagMultiMap.union(singleton(1, 2), singleton(2, 4)) |> toAssocList == (1, 2 :: Nil) :: (2, 4 :: Nil) :: Nil

    @test
    def union07(): Bool =
        BagMultiMap.union(singleton(1, 2), singleton(1, 3)) |> toAssocList == (1, 2 :: 3 :: Nil) :: Nil

    @test
    def union08(): Bool =
        BagMultiMap.union(singleton(1, 4), singleton(1, 2) |> insert(2, 3))
            |> toAssocList == (1, 4 :: 2 :: Nil) :: (2, 3 :: Nil) :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // intersection                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def intersection01(): Bool = BagMultiMap.intersection(empty(): BagMultiMap[Int32, Int32], empty()) == empty()

    @test
    def intersection02(): Bool = BagMultiMap.intersection(singleton(1, 2), empty()) == empty()

    @test
    def intersection03(): Bool = BagMultiMap.intersection(empty(), singleton(1, 2)) == empty()

    @test
    def intersection04(): Bool = BagMultiMap.intersection(singleton(1, 2), singleton(1, 3)) == empty()

    @test
    def intersection05(): Bool = BagMultiMap.intersection(singleton(1, 2), singleton(1, 2)) == singleton(1, 2)

    @test
    def intersection06(): Bool =
        BagMultiMap.intersection(singleton(1, 2) |> insert(1, 3), singleton(1, 2) |> insert(1, 4)) == singleton(1, 2)

    @test
    def intersection07(): Bool =
        BagMultiMap.intersection(singleton(1, 2) |> insert(1, 4), singleton(1, 2) |> insert(1, 4))
            |> toAssocList == (1, 4 :: 2 :: Nil) :: Nil

    @test
    def intersection08(): Bool =
        BagMultiMap.intersection(singleton(1, 2) |> insert(1, 4), singleton(1, 4) |> insert(1, 2) |> insert(1, 3))
            |> toAssocList == (1, 4 :: 2 :: Nil) :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // difference                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def difference01(): Bool = BagMultiMap.difference(empty(): BagMultiMap[Int32, Int32], empty()) == empty()

    @test
    def difference02(): Bool = BagMultiMap.difference(singleton(1, 2), empty()) == singleton(1, 2)

    @test
    def difference03(): Bool =
        BagMultiMap.difference(empty(), singleton(1, 2)) == empty()

    @test
    def difference04(): Bool =
        BagMultiMap.difference(singleton(1, 2), singleton(1, 3)) |> toAssocList == (1 , 2 :: Nil) :: Nil

    @test
    def difference05(): Bool = BagMultiMap.difference(singleton(1, 2), singleton(1, 2)) == empty()

    @test
    def difference06(): Bool =
        BagMultiMap.difference(singleton(1, 2) |> insert(1, 3), singleton(1, 2) |> insert(1, 4)) == singleton(1, 3)

    @test
    def difference07(): Bool =
        BagMultiMap.difference(singleton(1, 2) |> insert(1, 4), singleton(1, 2) |> insert(1, 4)) == empty()

    @test
    def difference08(): Bool =
        BagMultiMap.difference(singleton(1, 2) |> insert(1, 4), singleton(1, 4) |> insert(1, 2) |> insert(1, 3)) == empty()

    @test
    def difference09(): Bool =
        BagMultiMap.difference(singleton(1, 2) |> insert(1, 4) |> insert(1, 3), singleton(1, 4) |> insert(1, 2))
            |> toAssocList == (1, 3 :: Nil) :: Nil
    @test
    def difference10(): Bool =
        BagMultiMap.difference(singleton(1, 2) |> insert(1, 4) |> insert(1, 3) |> insert(2, 2), singleton(1, 4) |> insert(1, 2))
            |> toAssocList == (1, 3 :: Nil) :: (2, 2 :: Nil) :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // toList                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toList01(): Bool = BagMultiMap.toList(empty(): BagMultiMap[Int32, Int32]) == Nil

    @test
    def toList02(): Bool = BagMultiMap.toList(singleton(1, 4)) == (1, 4) ::  Nil

    @test
    def toList03(): Bool = BagMultiMap.toList(singleton(1, 4) |> insert(2, 4)) == (1, 4) :: (2, 4) :: Nil

    @test
    def toList04(): Bool =
        BagMultiMap.toList(singleton(1, 4) |> insert(2, 2) |> insert(1, 1)) == (1, 1) :: (1, 4) :: (2, 2) :: Nil

    @test
    def toList05(): Bool =
        BagMultiMap.toList(singleton(1, 4) |> insert(2, 2) |> insert(1, 1)  |> insert(3, 3)) == (1, 1) :: (1, 4) :: (2, 2) :: (3, 3) :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // toAscList                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toAscList01(): Bool = BagMultiMap.toAscList(empty(): BagMultiMap[Int32, Int32]) == Nil

    @test
    def toAscList02(): Bool = BagMultiMap.toAscList(singleton(1, 4)) == (1, 4) ::  Nil

    @test
    def toAscList03(): Bool = BagMultiMap.toAscList(singleton(1, 4) |> insert(2, 4)) == (1, 4) :: (2, 4) :: Nil

    @test
    def toAscList04(): Bool =
        BagMultiMap.toAscList(singleton(1, 4) |> insert(2, 4) |> insert(1, 1)) == (1, 1) :: (1, 4) :: (2, 4) :: Nil

    @test
    def toAscList05(): Bool =
        BagMultiMap.toAscList(singleton(1, 4) |> insert(2, 4) |> insert(1, 1)  |> insert(3, 3)) == (1, 1) :: (1, 4) :: (2, 4) :: (3, 3) :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // toDescList                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toDescList01(): Bool = BagMultiMap.toDescList(empty(): BagMultiMap[Int32, Int32]) == Nil

    @test
    def toDescList02(): Bool = BagMultiMap.toDescList(singleton(1, 4)) == (1, 4) ::  Nil

    @test
    def toDescList03(): Bool = BagMultiMap.toDescList(singleton(1, 4) |> insert(2, 4)) == (2, 4) :: (1, 4) :: Nil

    @test
    def toDescList04(): Bool =
        BagMultiMap.toDescList(singleton(1, 4) |> insert(2, 4) |> insert(1, 1)) == (2, 4) :: (1, 4) :: (1, 1) :: Nil

    @test
    def toDescList05(): Bool =
        BagMultiMap.toDescList(singleton(1, 4) |> insert(2, 4) |> insert(1, 1)  |> insert(3, 3)) == (3, 3) :: (2, 4) :: (1, 4) :: (1, 1) :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // sequence                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sequence01(): Bool = BagMultiMap.sequence(empty(): BagMultiMap[Int32, Option[Int32]]) == Some(empty())

    @test
    def sequence02(): Bool = BagMultiMap.sequence(singleton(1, None): BagMultiMap[Int32, Option[Int32]]) == None

    @test
    def sequence03(): Bool = BagMultiMap.sequence(singleton(1, Some(2))) == Some(singleton(1, 2))

    @test
    def sequence04(): Bool = BagMultiMap.sequence(singleton(1, Some(2)) |> insert(2, None)) == None

    @test
    def sequence05(): Bool =
        BagMultiMap.sequence(singleton(1, Some(2)) |> insert(2, Some(4)))
            |> Option.map(toAssocList) == Some((1, 2 :: Nil) :: (2, 4 :: Nil) :: Nil)


    /////////////////////////////////////////////////////////////////////////////
    // traverse                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def traverse01(): Bool = BagMultiMap.traverse(v -> Some(v+1), empty(): BagMultiMap[Int32, Int32]) == Some(empty())

    @test
    def traverse02(): Bool = BagMultiMap.traverse(v -> Some(v+1), singleton(1, 2)) == Some(singleton(1,3))

    @test
    def traverse03(): Bool =
        BagMultiMap.traverse(v -> Some(v+1), singleton(1, 2) |> insert(2, 4))
            |> Option.map(toAssocList) == Some((1, 3 :: Nil) :: (2, 5 :: Nil) :: Nil)

    @test
    def traverse04(): Bool =
        BagMultiMap.traverse(v -> Some(v+1), singleton(1, 2) |> insert(1, 4))
            |> Option.map(toAssocList) == Some((1, 5 :: 3 :: Nil) :: Nil)

    @test
    def traverse05(): Bool = BagMultiMap.traverse(_ -> None : Option[Int32], singleton(1, 2) |> insert(1, 4)) == None

    /////////////////////////////////////////////////////////////////////////////
    // traverseWithKey                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def traverseWithKey01(): Bool =
        BagMultiMap.traverseWithKey((k, v) -> Some(k+v), empty(): BagMultiMap[Int32, Int32]) == Some(empty())

    @test
    def traverseWithKey02(): Bool =
        BagMultiMap.traverseWithKey((k, v) -> Some(k+v), singleton(1, 2)) == Some(singleton(1,3))

    @test
    def traverseWithKey03(): Bool =
        BagMultiMap.traverseWithKey((k, v) -> Some(k+v), singleton(1, 2) |> insert(2, 4))
            |> Option.map(toAssocList) == Some((1, 3 :: Nil) :: (2, 6 :: Nil) :: Nil)

    @test
    def traverseWithKey04(): Bool =
        BagMultiMap.traverseWithKey((k, v) -> Some(k+v), singleton(1, 2) |> insert(1, 4))
            |> Option.map(toAssocList) == Some((1, 5 :: 3 :: Nil) :: Nil)

    @test
    def traverseWithKey05(): Bool =
        BagMultiMap.traverseWithKey((_, _) -> None : Option[Int32], singleton(1, 2) |> insert(1, 4)) == None

}

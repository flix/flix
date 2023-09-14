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

mod TestMultiMap {

    use MultiMap.{empty, singleton, insert, toAssocList, MultiMap}

    def isEven(i: Int32): Bool = i `Int32.rem` 2 == 0

    def isOdd(i: Int32): Bool = not isEven(i)

    /////////////////////////////////////////////////////////////////////////////
    // ToString                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toString01(): Bool = ToString.toString(empty(): MultiMap[Int32, Int32]) == "MultiMap#{}"

    @Test
    def toString02(): Bool = ToString.toString(singleton(1, 1)) == "MultiMap#{1 => Set#{1}}"

    @Test
    def toString03(): Bool = ToString.toString(singleton(1, 1) |> insert(1, 2)) == "MultiMap#{1 => Set#{1, 2}}"

    @Test
    def toString04(): Bool = ToString.toString(singleton(1, 1) |> insert(2, 2)) == "MultiMap#{1 => Set#{1}, 2 => Set#{2}}"

    /////////////////////////////////////////////////////////////////////////////
    // Eq                                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def eq01(): Bool = empty(): MultiMap[Int32, Char] == empty(): MultiMap[Int32, Char]

    @Test
    def eq02(): Bool = singleton(1, 'a') != empty()

    @Test
    def eq03(): Bool = empty() != singleton(1, 'a')

    @Test
    def eq04(): Bool = singleton(1, 'a') != singleton(1, 'b')

    @Test
    def eq05(): Bool = singleton(1, 'a') == singleton(1, 'a')

    @Test
    def eq06(): Bool = singleton(1, 'a') |> insert(1, 'b') == singleton(1, 'a') |> insert(1, 'b')

    /// Insert order doesn't matter for MultiMap
    @Test
    def eq07(): Bool = singleton(1, 'b') |> insert(1, 'a') == singleton(1, 'a') |> insert(1, 'b')

    /////////////////////////////////////////////////////////////////////////////
    // Order (compare)                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def compare01(): Bool = Order.compare(empty(): MultiMap[Int32, Char], empty(): MultiMap[Int32, Char]) == Comparison.EqualTo

    @Test
    def compare02(): Bool = Order.compare(singleton(1, 'a'), empty()) == Comparison.GreaterThan

    @Test
    def compare03(): Bool = Order.compare(empty(), singleton(1, 'a')) == Comparison.LessThan

    @Test
    def compare04(): Bool = Order.compare(singleton(1, 'b'), singleton(1, 'a')) == Comparison.GreaterThan

    @Test
    def compare05(): Bool = Order.compare(singleton(1, 'a'), singleton(1, 'b')) == Comparison.LessThan

    /////////////////////////////////////////////////////////////////////////////
    // empty                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def empty01(): Bool = MultiMap.isEmpty(empty())

    /////////////////////////////////////////////////////////////////////////////
    // singleton                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def singleton01(): Bool =
        MultiMap.singleton(1, 'a') |> MultiMap.toList |> List.sort == (1, 'a') :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // isEmpty                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isEmpty01(): Bool =
        MultiMap.isEmpty(empty()) == true

    @Test
    def isEmpty02(): Bool =
        MultiMap.isEmpty(singleton(1, 'a')) == false

    /////////////////////////////////////////////////////////////////////////////
    // get                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def get01(): Bool =
        MultiMap.get(1, empty(): MultiMap[Int32, Char]) == Set#{}

    @Test
    def get02(): Bool =
        MultiMap.get(1, singleton(1, 'a')) == Set#{'a'}

    @Test
    def get03(): Bool =
        MultiMap.get(1, singleton(1, 'a') |> insert(2, 'b')) == Set#{'a'}

    @Test
    def get04(): Bool =
        MultiMap.get(1, singleton(1, 'a') |> insert(2, 'b') |> insert(1, 'c')) == Set#{'a', 'c'}

    /////////////////////////////////////////////////////////////////////////////
    // memberOf                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def memberOf01(): Bool =
        not MultiMap.memberOf(1, empty(): MultiMap[Int32, Char])

    @Test
    def memberOf02(): Bool = MultiMap.memberOf(1, singleton(1, 'a'))

    @Test
    def memberOf03(): Bool = not MultiMap.memberOf(1, singleton(2, 'b'))

    @Test
    def memberOf04(): Bool = MultiMap.memberOf(1, singleton(1, 'a') |> insert(2, 'b') |> insert(1, 'c'))

    /////////////////////////////////////////////////////////////////////////////
    // keysOf                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def keysOf01(): Bool = MultiMap.keysOf(empty(): MultiMap[Int32, Char]) == Set#{}

    @Test
    def keysOf02(): Bool = MultiMap.keysOf(singleton(1, 'a')) == Set#{1}

    @Test
    def keysOf03(): Bool = MultiMap.keysOf(singleton(2, 'b')) == Set#{2}

    @Test
    def keysOf04(): Bool =
        MultiMap.keysOf(singleton(1, 'a') |> insert(2, 'b') |> insert(1, 'c')) == Set#{1, 2}


    /////////////////////////////////////////////////////////////////////////////
    // valuesOf                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def valuesOf01(): Bool = MultiMap.valuesOf(empty(): MultiMap[Int32, Char]) == Nil

    @Test
    def valuesOf02(): Bool = MultiMap.valuesOf(singleton(1, 'a')) |> List.sort == 'a' :: Nil

    @Test
    def valuesOf03(): Bool = MultiMap.valuesOf(singleton(2, 'b')) |> List.sort == 'b' :: Nil

    @Test
    def valuesOf04(): Bool =
        MultiMap.valuesOf(singleton(1, 'a') |> insert(2, 'b') |> insert(1, 'c'))
            |> List.sort == 'a' :: 'b' :: 'c' :: Nil

    @Test
    def valuesOf05(): Bool =
        MultiMap.valuesOf(singleton(1, 'a') |> insert(2, 'b') |> insert(1, 'c') |> insert(3, 'a'))
            |> List.sort == 'a' :: 'a' :: 'b' :: 'c' :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // insert                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def insert01(): Bool =
        MultiMap.insert(1, 'a', empty()) |> toAssocList == (1, Set#{'a'}) :: Nil

    @Test
    def insert02(): Bool =
        empty() |> MultiMap.insert(1, 'a') |> MultiMap.insert(2, 'b')
            |> toAssocList == (1, Set#{'a'}) :: (2, Set#{'b'}) :: Nil

    @Test
    def insert03(): Bool =
        empty() |> MultiMap.insert(1, 'a') |> MultiMap.insert(2, 'b') |> MultiMap.insert(1, 'c')
            |> toAssocList == (1, Set#{'a', 'c'}) :: (2, Set#{'b'}) :: Nil

    @Test
    def insert04(): Bool =
        empty() |> MultiMap.insert(1, 'a') |> MultiMap.insert(2, 'b') |> MultiMap.insert(1, 'c') |> MultiMap.insert(3, 'b')
            |> toAssocList == (1, Set#{'a', 'c'}) :: (2, Set#{'b'}) :: (3, Set#{'b'}) :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // insertAll                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def insertAll01(): Bool =
        MultiMap.insertAll(1, Nil: List[Char], empty()) == empty()

    @Test
    def insertAll02(): Bool =
        empty() |> MultiMap.insertAll(1, 'a' :: Nil)|> toAssocList == (1, Set#{'a'}) :: Nil

    @Test
    def insertAll03(): Bool =
        empty() |> MultiMap.insertAll(1, 'a' :: 'c' :: Nil) |> MultiMap.insertAll(2, 'b' :: Nil)
            |> toAssocList == (1, Set#{'a', 'c'}) :: (2, Set#{'b'}) :: Nil

    @Test
    def insertAll04(): Bool =
        empty() |> MultiMap.insertAll(1, 'a' :: 'c' :: Nil) |> MultiMap.insertAll(2, 'b' :: Nil) |> MultiMap.insertAll(3, 'b' :: Nil)
            |> toAssocList == (1, Set#{'a', 'c'}) :: (2, Set#{'b'}) :: (3, Set#{'b'}) :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // adjust                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def adjust01(): Bool = MultiMap.adjust(v -> 2*v, 1, empty()) == empty()

    @Test
    def adjust02(): Bool =
        MultiMap.adjust(v -> 2*v, 1, MultiMap.singleton(1, 4)) |> toAssocList == (1, Set#{8}) :: Nil

    @Test
    def adjust03(): Bool =
        MultiMap.adjust(v -> 2*v, 3, singleton(1, 4)) |> toAssocList == (1, Set#{4}) :: Nil

    @Test
    def adjust04(): Bool =
        MultiMap.adjust(v -> 2*v, 1, singleton(1, 4) |> insert(2, 4) |> insert(1, 5))
            |> toAssocList == (1, Set#{8, 10}) :: (2, Set#{4}) :: Nil

    @Test
    def adjust05(): Bool =
        MultiMap.adjust(v -> 2*v, 2, singleton(1, 4) |> insert(2, 4) |> insert(1, 5))
            |> toAssocList == (1, Set#{4, 5}) :: (2, Set#{8}) :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // adjustWithKey                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def adjustWithKey01(): Bool = MultiMap.adjustWithKey((k, v) -> k+v, 1, empty()) == empty()

    @Test
    def adjustWithKey02(): Bool =
        MultiMap.adjustWithKey((k, v) -> k+v, 1, singleton(1, 4))
            |> toAssocList == (1, Set#{5}) :: Nil

    @Test
    def adjustWithKey03(): Bool =
        MultiMap.adjustWithKey((k, v) -> k+v, 3, singleton(1, 4))
            |> toAssocList == (1, Set#{4}) :: Nil

    @Test
    def adjustWithKey04(): Bool =
        MultiMap.adjustWithKey((k, v) -> k+v, 1, singleton(1, 4) |> insert(2, 4) |> insert(1, 5))
            |> toAssocList == (1, Set#{5, 6}) :: (2, Set#{4}) :: Nil

    @Test
    def adjustWithKey05(): Bool =
        MultiMap.adjustWithKey((k, v) -> k+v, 2, singleton(1, 4) |> insert(2, 4) |> insert(1, 5))
            |> toAssocList == (1, Set#{4, 5}) :: (2, Set#{6}) :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // update                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def update01(): Bool = MultiMap.update(v -> if (isEven(v)) Some(2*v) else None, 1, empty()) == empty()

    @Test
    def update02(): Bool =
        MultiMap.update(v -> if (isEven(v)) Some(2*v) else None, 1, singleton(1, 4))
            |> toAssocList == (1, Set#{8}) :: Nil

    @Test
    def update03(): Bool =
        MultiMap.update(v -> if (isEven(v)) Some(2*v) else None, 3, singleton(1, 4))
            |> toAssocList == (1, Set#{4}) :: Nil

    @Test
    def update04(): Bool =
        MultiMap.update(v -> if (isEven(v)) Some(2*v) else None, 1, singleton(1, 4) |> insert(2, 4) |> insert(1, 5))
            |> toAssocList == (1, Set#{5, 8}) :: (2, Set#{4}) :: Nil

    @Test
    def update05(): Bool =
        MultiMap.update(v -> if (isEven(v)) Some(2*v) else None, 2, singleton(1, 4) |> insert(2, 4) |> insert(1, 5))
            |> toAssocList == (1, Set#{4, 5}) :: (2, Set#{8}) :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // updateWithKey                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def updateWithKey01(): Bool = MultiMap.updateWithKey((k, v) -> if (isEven(k)) Some(k+v) else None, 1, empty()) == empty()

    @Test
    def updateWithKey02(): Bool =
        MultiMap.updateWithKey((k, v) -> if (isEven(k)) Some(k+v) else None, 1, singleton(1, 4))
            |> toAssocList == (1, Set#{4}) :: Nil

    @Test
    def updateWithKey03(): Bool =
        MultiMap.updateWithKey((k, v) -> if (isOdd(k)) Some(k+v) else None, 1, singleton(1, 4))
            |> toAssocList == (1, Set#{5}) :: Nil

    @Test
    def updateWithKey04(): Bool =
        MultiMap.updateWithKey((k, v) -> if (isEven(k)) Some(k+v) else None, 3, singleton(1, 4))
            |> toAssocList == (1, Set#{4}) :: Nil

    @Test
    def updateWithKey05(): Bool =
        MultiMap.updateWithKey((k, v) -> if (isOdd(k)) Some(k+v) else None, 3, singleton(1, 4))
            |> toAssocList == (1, Set#{4}) :: Nil

    @Test
    def updateWithKey06(): Bool =
        MultiMap.updateWithKey((k, v) -> if (isEven(k)) Some(k+v) else None, 1, singleton(1, 4) |> insert(2, 4) |> insert(1, 5))
            |> toAssocList == (1, Set#{4, 5}) :: (2, Set#{4}) :: Nil

    @Test
    def updateWithKey07(): Bool =
        MultiMap.updateWithKey((k, v) -> if (isOdd(k)) Some(k+v) else None, 1, singleton(1, 4) |> insert(2, 4) |> insert(1, 5))
            |> toAssocList == (1, Set#{5, 6}) :: (2, Set#{4}) :: Nil

    @Test
    def updateWithKey08(): Bool =
        MultiMap.updateWithKey((k, v) -> if (isEven(k)) Some(k+v) else None, 2, singleton(1, 4) |> insert(2, 4) |> insert(1, 5))
            |> toAssocList == (1, Set#{4, 5}) :: (2, Set#{6}) :: Nil

    @Test
    def updateWithKey09(): Bool =
        MultiMap.updateWithKey((k, v) -> if (isOdd(k)) Some(k+v) else None, 2, singleton(1, 4) |> insert(2, 4) |> insert(1, 5))
            |> toAssocList == (1, Set#{4, 5}) :: (2, Set#{4}) :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // remove                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def remove01(): Bool = MultiMap.remove(1, empty(): MultiMap[Int32, Int32]) == empty()

    @Test
    def remove02(): Bool = MultiMap.remove(1, singleton(1, 4)) |> toAssocList == Nil

    @Test
    def remove03(): Bool = MultiMap.remove(3, singleton(1, 4)) |> toAssocList == (1, Set#{4}) :: Nil

    @Test
    def remove04(): Bool =
        MultiMap.remove(1, singleton(1, 4) |> insert(2, 4) |> insert(1, 5)) |> toAssocList == (2, Set#{4}) :: Nil

    @Test
    def remove05(): Bool =
        MultiMap.remove(2, singleton(1, 4) |> insert(2, 4) |> insert(1, 5)) |> toAssocList == (1, Set#{4, 5}) :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // removeWithValue                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def removeWithValue01(): Bool = MultiMap.removeWithValue(1, 4, empty(): MultiMap[Int32, Int32]) == empty()

    @Test
    def removeWithValue02(): Bool =
        MultiMap.removeWithValue(1, 4, singleton(1, 4)) |> toAssocList == Nil

    @Test
    def removeWithValue03(): Bool = MultiMap.removeWithValue(1, 1, singleton(1, 4)) |> toAssocList == (1, Set#{4}) :: Nil

    @Test
    def removeWithValue04(): Bool =
        MultiMap.removeWithValue(1, 4, singleton(1, 4) |> insert(2, 4) |> insert(1, 5))
            |> toAssocList == (1, Set#{5}) :: (2, Set#{4}) :: Nil

    @Test
    def removeWithValue05(): Bool =
        MultiMap.removeWithValue(2, 4, singleton(1, 4) |> insert(2, 4) |> insert(1, 5))
            |> toAssocList == (1, Set#{4, 5}) :: Nil

    @Test
    def removeWithValue06(): Bool =
        MultiMap.removeWithValue(2, 0, singleton(1, 4) |> insert(2, 4) |> insert(1, 5))
            |> toAssocList == (1, Set#{4, 5}) :: (2, Set#{4}) :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // find                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def find01(): Bool = MultiMap.find((k, v) -> k == v, empty(): MultiMap[Int32, Int32]) == None

    @Test
    def find02(): Bool = MultiMap.find((k, v) -> k == v, singleton(1, 2)) == None

    @Test
    def find03(): Bool = MultiMap.find((k, v) -> k == v, singleton(1, 1)) == Some((1, 1))

    @Test
    def find04(): Bool =
        MultiMap.find((k, v) -> k == v, singleton(1, 4) |> insert(2, 4) |> insert(1, 5)) == None

    @Test
    def find05(): Bool =
        MultiMap.find((k, v) -> k == v, singleton(1, 4) |> insert(2, 2) |> insert(3, 3)) == Some((2,2))

    /////////////////////////////////////////////////////////////////////////////
    // findLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def findLeft01(): Bool = MultiMap.findLeft((k, v) -> k == v, empty(): MultiMap[Int32, Int32]) == None

    @Test
    def findLeft02(): Bool = MultiMap.findLeft((k, v) -> k == v, singleton(1, 2)) == None

    @Test
    def findLeft03(): Bool = MultiMap.findLeft((k, v) -> k == v, singleton(1, 1)) == Some((1, 1))

    @Test
    def findLeft04(): Bool =
        MultiMap.findLeft((k, v) -> k == v, singleton(1, 4) |> insert(2, 4) |> insert(1, 5)) == None

    @Test
    def findLeft05(): Bool =
        MultiMap.findLeft((k, v) -> k == v, singleton(1, 4) |> insert(2, 2) |> insert(3, 3)) == Some((2, 2))

    /////////////////////////////////////////////////////////////////////////////
    // findRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def findRight01(): Bool = MultiMap.findRight((k, v) -> k == v, empty(): MultiMap[Int32, Int32]) == None

    @Test
    def findRight02(): Bool = MultiMap.findRight((k, v) -> k == v, singleton(1, 2)) == None

    @Test
    def findRight03(): Bool = MultiMap.findRight((k, v) -> k == v, singleton(1, 1)) == Some((1, 1))

    @Test
    def findRight04(): Bool =
        MultiMap.findRight((k, v) -> k == v, singleton(1, 4) |> insert(2, 4) |> insert(1, 5)) == None

    @Test
    def findRight05(): Bool =
        MultiMap.findRight((k, v) -> k == v, singleton(1, 4) |> insert(2, 2) |> insert(3, 3)) == Some((3, 3))

    /////////////////////////////////////////////////////////////////////////////
    // filter                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def filter01(): Bool =
        MultiMap.filter(isEven, empty(): MultiMap[Int32, Int32]) == empty()

    @Test
    def filter02(): Bool = MultiMap.filter(isEven, singleton(1, 2)) |> toAssocList == (1, Set#{2}) :: Nil

    @Test
    def filter03(): Bool = MultiMap.filter(isEven, singleton(1, 1)) == empty()

    @Test
    def filter04(): Bool =
        MultiMap.filter(isEven, singleton(1, 4) |> insert(2, 4))
            |> toAssocList == (1, Set#{4}) :: (2, Set#{4}) :: Nil

    @Test
    def filter05(): Bool =
        MultiMap.filter(isEven, singleton(1, 4) |> insert(2, 2) |> insert(1, 3))
            |> toAssocList == (1, Set#{4}) :: (2, Set#{2}) :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // filterWithKey                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def filterWithKey01(): Bool =
        MultiMap.filterWithKey((k, v) -> k == v, empty(): MultiMap[Int32, Int32]) == empty()

    @Test
    def filterWithKey02(): Bool = MultiMap.filterWithKey((k, v) -> k == v, singleton(1, 2)) == empty()

    @Test
    def filterWithKey03(): Bool =
        MultiMap.filterWithKey((k, v) -> k == v, singleton(1, 1)) |> toAssocList == (1, Set#{1}) :: Nil

    @Test
    def filterWithKey04(): Bool =
        MultiMap.filterWithKey((k, v) -> k == v, singleton(1, 4) |> insert(2, 4)) == empty()

    @Test
    def filterWithKey05(): Bool =
        MultiMap.filterWithKey((k, v) -> k == v, singleton(1, 4) |> insert(2, 2) |> insert(1, 1))
            |> toAssocList == (1, Set#{1}) :: (2, Set#{2}) :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // map                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def map01(): Bool = MultiMap.map(v -> v*3, empty(): MultiMap[Int32, Int32]) == empty()

    @Test
    def map02(): Bool =
        MultiMap.map(v -> v*3, singleton(1, 2)) |> toAssocList == (1, Set#{6}) :: Nil

    @Test
    def map03(): Bool =
        MultiMap.map(v -> v*3, singleton(1, 1)) |> toAssocList == (1, Set#{3}) :: Nil

    @Test
    def map04(): Bool =
        MultiMap.map(v -> v*3, singleton(1, 4) |> insert(2, 4))
            |> toAssocList == (1, Set#{12}) :: (2, Set#{12}) :: Nil

    @Test
    def map05(): Bool =
        MultiMap.map(v -> v*3, singleton(1, 4) |> insert(2, 2) |> insert(1, 1))
            |> toAssocList == (1, Set#{3, 12}) :: (2, Set#{6}) :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // mapWithKey                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def mapWithKey01(): Bool =
        MultiMap.mapWithKey((k, v) -> v*3+k, empty(): MultiMap[Int32, Int32]) == empty()

    @Test
    def mapWithKey02(): Bool =
        MultiMap.mapWithKey((k, v) -> v*3+k, singleton(1, 2)) |> toAssocList == (1, Set#{7}) :: Nil

    @Test
    def mapWithKey03(): Bool =
        MultiMap.mapWithKey((k, v) -> v*3+k, singleton(1, 1)) |> toAssocList == (1, Set#{4}) :: Nil

    @Test
    def mapWithKey04(): Bool =
        MultiMap.mapWithKey((k, v) -> v*3+k, singleton(1, 4) |> insert(2, 4))
            |> toAssocList == (1, Set#{13}) :: (2, Set#{14}) :: Nil

    @Test
    def mapWithKey05(): Bool =
        MultiMap.mapWithKey((k, v) -> v*3+k, singleton(1, 4) |> insert(2, 2) |> insert(1, 1))
            |> toAssocList == (1, Set#{4, 13}) :: (2, Set#{8}) :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // foldWithKey                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldWithKey01(): Bool =
        MultiMap.foldWithKey((acc, k, v) -> acc + k + v, 0, empty(): MultiMap[Int32, Int32]) == 0

    @Test
    def foldWithKey02(): Bool =
        MultiMap.foldWithKey((acc, k, v) -> acc + k + v, 0, singleton(1, 2)) == 3

    @Test
    def foldWithKey03(): Bool =
        MultiMap.foldWithKey((acc, k, v) -> acc + k + v, 0, singleton(1, 4) |> insert(2, 4)) == 11

    @Test
    def foldWithKey04(): Bool =
        MultiMap.foldWithKey((acc, k, v) -> acc + k + v, 0,
            singleton(1, 4) |> insert(2, 2) |> insert(1, 1)) == 11

    @Test
    def foldWithKey05(): Bool =
        MultiMap.foldWithKey((acc, k, v) -> acc + k + v, 0,
            singleton(1, 4) |> insert(2, 2) |> insert(1, 1) |> insert(3, 3)) == 17

    /////////////////////////////////////////////////////////////////////////////
    // foldLeft                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldLeft01(): Bool =
        MultiMap.foldLeft((acc, v) -> v :: acc, Nil, empty(): MultiMap[Int32, Int32]) == Nil

    @Test
    def foldLeft02(): Bool =
        MultiMap.foldLeft((acc, v) -> v :: acc, Nil, singleton(1, 2)) == 2 :: Nil

    @Test
    def foldLeft03(): Bool =
        MultiMap.foldLeft((acc, v) -> v :: acc, Nil, singleton(1, 4) |> insert(2, 5)) == 5 :: 4 :: Nil

    @Test
    def foldLeft04(): Bool =
        MultiMap.foldLeft((acc, v) -> v :: acc, Nil,
            singleton(1, 4) |> insert(2, 2) |> insert(1, 1)) == 2 :: 4 :: 1 :: Nil

    @Test
    def foldLeft05(): Bool =
        MultiMap.foldLeft((acc, v) -> v :: acc, Nil,
            singleton(1, 4) |> insert(2, 2) |> insert(1, 1) |> insert(3, 3)) == 3 :: 2 :: 4 :: 1 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // foldLeftWithKey                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldLeftWithKey01(): Bool =
        MultiMap.foldLeftWithKey((acc, k, v) -> (k, v) :: acc, Nil, empty(): MultiMap[Int32, Int32]) == Nil

    @Test
    def foldLeftWithKey02(): Bool =
        MultiMap.foldLeftWithKey((acc, k, v) -> (k, v) :: acc, Nil, singleton(1, 2)) == (1, 2) :: Nil

    @Test
    def foldLeftWithKey03(): Bool =
        MultiMap.foldLeftWithKey((acc, k, v) -> (k, v) :: acc, Nil,
            singleton(1, 4) |> insert(2, 5)) == (2, 5) :: (1, 4) :: Nil

    @Test
    def foldLeftWithKey04(): Bool =
        MultiMap.foldLeftWithKey((acc, k, v) -> (k, v) :: acc, Nil,
            singleton(1, 4) |> insert(2, 2) |> insert(1, 1)) == (2, 2) :: (1, 4) :: (1, 1) :: Nil

    @Test
    def foldLeftWithKey05(): Bool =
        MultiMap.foldLeftWithKey((acc, k, v) -> (k, v) :: acc, Nil,
            singleton(1, 4) |> insert(2, 2) |> insert(1, 1) |> insert(3, 3)) == (3, 3) :: (2, 2) :: (1, 4) :: (1, 1) :: Nil

    @Test
    def foldLeftWithKey06(): Bool =
        MultiMap.foldLeftWithKey((acc, k, v) -> (k, v) :: acc, Nil,
            singleton(1, 4) |> insert(2, 2) |> insert(1, 1) |> insert(3, 3) |> insert(1, 2)) == (3, 3) :: (2, 2) :: (1, 4) :: (1, 2) :: (1, 1) :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // foldRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldRight01(): Bool =
        MultiMap.foldRight((v, acc) -> v :: acc, Nil, empty(): MultiMap[Int32, Int32]) == Nil

    @Test
    def foldRight02(): Bool =
        MultiMap.foldRight((v, acc) -> v :: acc, Nil, singleton(1, 2)) == 2 :: Nil

    @Test
    def foldRight03(): Bool =
        MultiMap.foldRight((v, acc) -> v :: acc, Nil, singleton(1, 4) |> insert(2, 5)) == 4 :: 5 :: Nil

    @Test
    def foldRight04(): Bool =
        MultiMap.foldRight((v, acc) -> v :: acc, Nil,
            singleton(1, 4) |> insert(2, 2) |> insert(1, 1)) == 1 :: 4 :: 2 :: Nil

    @Test
    def foldRight05(): Bool =
        MultiMap.foldRight((v, acc) -> v :: acc, Nil,
            singleton(1, 4) |> insert(2, 2) |> insert(1, 1) |> insert(3, 3)) == 1 :: 4 :: 2 :: 3 :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // foldRightWithKey                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldRightWithKey01(): Bool =
        MultiMap.foldRightWithKey((k, v, acc) -> (k, v) :: acc, Nil, empty(): MultiMap[Int32, Int32]) == Nil

    @Test
    def foldRightWithKey02(): Bool =
        MultiMap.foldRightWithKey((k, v, acc) -> (k, v) :: acc, Nil, singleton(1, 2)) == (1, 2) :: Nil

    @Test
    def foldRightWithKey03(): Bool =
        MultiMap.foldRightWithKey((k, v, acc) -> (k, v) :: acc, Nil,
            singleton(1, 4) |> insert(2, 5)) == (1, 4) :: (2, 5) :: Nil

    @Test
    def foldRightWithKey04(): Bool =
        MultiMap.foldRightWithKey((k, v, acc) -> (k, v) :: acc, Nil,
            singleton(1, 4) |> insert(2, 2) |> insert(1, 1)) == (1, 1) :: (1, 4) :: (2, 2) :: Nil

    @Test
    def foldRightWithKey05(): Bool =
        MultiMap.foldRightWithKey((k, v, acc) -> (k, v) :: acc, Nil,
            singleton(1, 4) |> insert(2, 2) |> insert(1, 1) |> insert(3, 3)) == (1, 1) :: (1, 4) :: (2, 2) :: (3, 3) :: Nil

    @Test
    def foldRightWithKey06(): Bool =
        MultiMap.foldRightWithKey((k, v, acc) -> (k, v) :: acc, Nil,
            singleton(1, 4) |> insert(2, 2) |> insert(1, 1) |> insert(3, 3) |> insert(1, 2)) == (1, 1) :: (1, 2) :: (1, 4) :: (2, 2) :: (3, 3) :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // foldMapWithKey                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldMapWithKey01(): Bool =
        MultiMap.foldMapWithKey((k, v) -> k + v, MultiMap(Map#{})) == 0

    @Test
    def foldMapWithKey02(): Bool =
        MultiMap.foldMapWithKey((k, v) -> k + v, MultiMap(Map#{1 => Set#{2}, 3 => Set#{5}})) == (1 + 2 + 3 + 5)

    @Test
    def foldMapWithKey03(): Bool =
        MultiMap.foldMapWithKey((k, v) -> k + v, MultiMap(Map#{1 => Set#{2, 3}, 4 => Set#{5}})) == (1 + 2 + 1 + 3 + 4 + 5)

    @Test
    def foldMapWithKey04(): Bool =
        MultiMap.foldMapWithKey((k, v) -> 2 * (k + v), MultiMap(Map#{1 => Set#{2}, 3 => Set#{5}})) == (2 * (1 + 2) + 2 * (3 + 5))

    @Test
    def foldMapWithKey05(): Bool =
        MultiMap.foldMapWithKey((k, v) -> 2 * (k + v), MultiMap(Map#{1 => Set#{2, 3}, 4 => Set#{5}})) == ((2 * (1 + 2)) + (2 * (1 + 3)) + 2 * (4 + 5))


    /////////////////////////////////////////////////////////////////////////////
    // foldMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldMap01(): Bool =
        MultiMap.foldMap(v -> v + 1, MultiMap(Map#{})) == 0

    @Test
    def foldMap02(): Bool =
        MultiMap.foldMap(v -> v + 1, MultiMap(Map#{1 => Set#{2}, 3 => Set#{5}})) == (2 + 1) + (5 + 1)

    @Test
    def foldMap03(): Bool =
        MultiMap.foldMap(v -> v + 1, MultiMap(Map#{1 => Set#{2, 3}, 4 => Set#{5}})) == (2 + 1) + (3 + 1) + (5 + 1)

    @Test
    def foldMap04(): Bool =
        MultiMap.foldMap(v -> 2 * v, MultiMap(Map#{1 => Set#{2}, 3 => Set#{5}})) == (2 * 2) + (2 * 5)

    @Test
    def foldMap05(): Bool =
        MultiMap.foldMap(v -> 2 * v, MultiMap(Map#{1 => Set#{2, 3}, 4 => Set#{5}})) == (2 * 2) + (2 * 3) + (2 * 5)


    /////////////////////////////////////////////////////////////////////////////
    // reduceLeft                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def reduceLeft01(): Bool =
        MultiMap.reduceLeft((acc, v) -> acc - v, empty(): MultiMap[Int32, Int32]) == None

    @Test
    def reduceLeft02(): Bool =
        MultiMap.reduceLeft((acc, v) -> acc - v, singleton(1, 2)) == Some(2)

    @Test
    def reduceLeft03(): Bool =
        MultiMap.reduceLeft((acc, v) -> acc - v, singleton(1, 4) |> insert(2, 5)) == Some(-1)

    /// 1 - 4 - 2
    @Test
    def reduceLeft04(): Bool =
        MultiMap.reduceLeft((acc, v) -> acc - v,
            singleton(1, 4) |> insert(2, 2) |> insert(1, 1)) == Some(-5)

    /// 1 - 4 - 2 - 3
    @Test
    def reduceLeft05(): Bool =
        MultiMap.reduceLeft((acc, v) -> acc - v,
            singleton(1, 4) |> insert(2, 2) |> insert(1, 1) |> insert(3, 3)) == Some(-8)

    /// First value
    @Test
    def reduceLeft06(): Bool =
        MultiMap.reduceLeft((v1, _) -> v1,
            singleton(1, 4) |> insert(2, 2) |> insert(1, 1) |> insert(3, 3)) == Some(1)

    /// Last value
    @Test
    def reduceLeft07(): Bool =
        MultiMap.reduceLeft((_, v2) -> v2,
            singleton(1, 4) |> insert(2, 2) |> insert(1, 1) |> insert(3, 3)) == Some(3)


    /////////////////////////////////////////////////////////////////////////////
    // reduceLeftWithKey                                                       //
    /////////////////////////////////////////////////////////////////////////////

    /// Leftmost key, rightmost value

    @Test
    def reduceLeftWithKey01(): Bool =
        MultiMap.reduceLeftWithKey((k1, _, _, v2) -> (k1, v2), empty(): MultiMap[Int32, Int32]) == None

    @Test
    def reduceLeftWithKey02(): Bool =
        MultiMap.reduceLeftWithKey((k1, _, _, v2) -> (k1, v2), singleton(1, 2)) == Some((1, 2))

    @Test
    def reduceLeftWithKey03(): Bool =
        MultiMap.reduceLeftWithKey((k1, _, _, v2) -> (k1, v2), singleton(1, 1)) == Some((1, 1))

    @Test
    def reduceLeftWithKey04(): Bool =
        MultiMap.reduceLeftWithKey((k1, _, _, v2) -> (k1, v2),
            singleton(1, 4) |> insert(2, 5)) == Some((1, 5))

    @Test
    def reduceLeftWithKey05(): Bool =
        MultiMap.reduceLeftWithKey((k1, _, _, v2) -> (k1, v2),
            singleton(1, 4) |> insert(2, 2) |> insert(1, 1)) == Some((1, 2))

    /////////////////////////////////////////////////////////////////////////////
    // reduceRight                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def reduceRight01(): Bool =
        MultiMap.reduceRight((v, acc) -> acc - v, empty(): MultiMap[Int32, Int32]) == None

    @Test
    def reduceRight02(): Bool =
        MultiMap.reduceRight((v, acc) -> acc - v, singleton(1, 2)) == Some(2)

    @Test
    def reduceRight03(): Bool =
        MultiMap.reduceRight((v, acc) -> acc - v, singleton(1, 4) |> insert(2, 5)) == Some(1)

    @Test
    def reduceRight04(): Bool =
        MultiMap.reduceRight((v, acc) -> acc - v,
            singleton(1, 4) |> insert(2, 2) |> insert(1, 1)) == Some(-3)

    /// 3 - 2 - 4 - 1
    @Test
    def reduceRight05(): Bool =
        MultiMap.reduceRight((v, acc) -> acc - v,
            singleton(1, 4) |> insert(2, 2) |> insert(1, 1) |> insert(3, 3)) == Some(-4)

    /// First value
    @Test
    def reduceRight06(): Bool =
        MultiMap.reduceRight((_, acc) -> acc,
            singleton(1, 4) |> insert(2, 2) |> insert(1, 1) |> insert(3, 3)) == Some(3)

    /// Last value
    @Test
    def reduceRight07(): Bool =
        MultiMap.reduceRight((v, _) -> v,
            singleton(1, 4) |> insert(2, 2) |> insert(1, 1) |> insert(3, 3)) == Some(1)


    /////////////////////////////////////////////////////////////////////////////
    // reduceRightWithKey                                                      //
    /////////////////////////////////////////////////////////////////////////////

    /// Rightmost key, leftmost value

    @Test
    def reduceRightWithKey01(): Bool =
        MultiMap.reduceRightWithKey((_, v1, k2, _) -> (k2, v1), empty(): MultiMap[Int32, Int32]) == None

    @Test
    def reduceRightWithKey02(): Bool =
        MultiMap.reduceRightWithKey((_, v1, k2, _) -> (k2, v1), singleton(1, 2)) == Some((1, 2))

    @Test
    def reduceRightWithKey03(): Bool =
        MultiMap.reduceRightWithKey((_, v1, k2, _) -> (k2, v1),
            singleton(1, 4) |> insert(2, 5)) == Some((2, 4))

    @Test
    def reduceRightWithKey04(): Bool =
        MultiMap.reduceRightWithKey((_, v1, k2, _) -> (k2, v1),
            singleton(1, 4) |> insert(2, 2) |> insert(1, 1)) == Some((2, 1))

    @Test
    def reduceRightWithKey05(): Bool =
        MultiMap.reduceRightWithKey((_, v1, k2, _) -> (k2, v1),
            singleton(1, 4) |> insert(2, 2) |> insert(1, 1) |> insert(3, 3)) == Some((3, 1))

    /////////////////////////////////////////////////////////////////////////////
    // count                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def count01(): Bool =
        MultiMap.count((k, v) -> v > k, empty(): MultiMap[Int32, Int32]) == 0

    @Test
    def count02(): Bool =
        MultiMap.count((k, v) -> v > k, singleton(1, 2)) == 1

    @Test
    def count03(): Bool =
        MultiMap.count((k, v) -> v > k, singleton(1, 4) |> insert(2, 4)) == 2

    @Test
    def count04(): Bool =
        MultiMap.count((k, v) -> v > k,
            singleton(1, 4) |> insert(2, 2) |> insert(1, 1)) == 1

    @Test
    def count05(): Bool =
        MultiMap.count((k, v) -> v > k,
            singleton(1, 4) |> insert(2, 2) |> insert(1, 1) |> insert(3, 3)) == 1


    /////////////////////////////////////////////////////////////////////////////
    // sumKeys                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sumKeys01(): Bool = MultiMap.sumKeys(empty(): MultiMap[Int32, Int32]) == 0

    @Test
    def sumKeys02(): Bool = MultiMap.sumKeys(singleton(1, 2)) == 1

    @Test
    def sumKeys03(): Bool = MultiMap.sumKeys(singleton(1, 4) |> insert(2, 4)) == 3

    @Test
    def sumKeys04(): Bool =
        MultiMap.sumKeys(singleton(1, 4) |> insert(2, 2) |> insert(1, 1)) == 4

    @Test
    def sumKeys05(): Bool =
        MultiMap.sumKeys(singleton(1, 4) |> insert(2, 2) |> insert(1, 1) |> insert(3, 3)) == 7

    /////////////////////////////////////////////////////////////////////////////
    // sumValues                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sumValues01(): Bool = MultiMap.sumValues(empty(): MultiMap[Int32, Int32]) == 0

    @Test
    def sumValues02(): Bool = MultiMap.sumValues(singleton(1, 2)) == 2

    @Test
    def sumValues03(): Bool = MultiMap.sumValues(singleton(1, 4) |> insert(2, 4)) == 8

    @Test
    def sumValues04(): Bool =
        MultiMap.sumValues(singleton(1, 4) |> insert(2, 2) |> insert(1, 1)) == 7

    @Test
    def sumValues05(): Bool =
        MultiMap.sumValues(singleton(1, 4) |> insert(2, 2) |> insert(1, 1) |> insert(3, 3)) == 10

    /////////////////////////////////////////////////////////////////////////////
    // sumWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sumWith01(): Bool = MultiMap.sumWith((k, v) -> if (v > k) 10 else 0, empty(): MultiMap[Int32, Int32]) == 0

    @Test
    def sumWith02(): Bool = MultiMap.sumWith((k, v) -> if (v > k) 10 else 0, singleton(1, 2)) == 10

    @Test
    def sumWith03(): Bool =
        MultiMap.sumWith((k, v) -> if (v > k) 10 else 0, singleton(1, 4) |> insert(2, 4)) == 20

    @Test
    def sumWith04(): Bool =
        MultiMap.sumWith((k, v) -> if (v > k) 10 else 0,
            singleton(1, 4) |> insert(2, 2) |> insert(1, 1)) == 10

    @Test
    def sumWith05(): Bool =
        MultiMap.sumWith((k, v) -> if (v > k) 10 else 0,
            singleton(1, 4) |> insert(2, 2) |> insert(1, 1) |> insert(3, 3)) == 10


    /////////////////////////////////////////////////////////////////////////////
    // exists                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def exists01(): Bool = not MultiMap.exists((k, v) -> v > k, empty(): MultiMap[Int32, Int32])

    @Test
    def exists02(): Bool = MultiMap.exists((k, v) -> v > k, singleton(1, 2))

    @Test
    def exists03(): Bool = MultiMap.exists((k, v) -> v > k, singleton(1, 4) |> insert(2, 4))

    @Test
    def exists04(): Bool =
        MultiMap.exists((k, v) -> v > k, singleton(1, 4) |> insert(2, 2) |> insert(1, 1))

    @Test
    def exists05(): Bool =
        MultiMap.exists((k, v) -> v > k, singleton(1, 4) |> insert(2, 2) |> insert(1, 1) |> insert(3, 3))

    /////////////////////////////////////////////////////////////////////////////
    // forAll                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forAll01(): Bool = MultiMap.forAll((k, v) -> v > k, empty(): MultiMap[Int32, Int32])

    @Test
    def forAll02(): Bool = MultiMap.forAll((k, v) -> v > k, singleton(1, 2))

    @Test
    def forAll03(): Bool = MultiMap.forAll((k, v) -> v > k, singleton(1, 4) |> insert(2, 4))

    @Test
    def forAll04(): Bool =
        not MultiMap.forAll((k, v) -> v > k, singleton(1, 4) |> insert(2, 2) |> insert(1, 1))

    @Test
    def forAll05(): Bool =
        not MultiMap.forAll((k, v) -> v > k, singleton(1, 4) |> insert(2, 2) |> insert(1, 1)  |> insert(3, 3))

    @Test
    def forAll06(): Bool =
        MultiMap.forAll((k, v) -> v > k, singleton(1, 4) |> insert(2, 3) |> insert(1, 2)  |> insert(3, 4))

    /////////////////////////////////////////////////////////////////////////////
    // union                                                                   //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def union01(): Bool = MultiMap.union(empty(): MultiMap[Int32, Int32], empty()) == empty()

    @Test
    def union02(): Bool = MultiMap.union(singleton(1, 2), empty()) == singleton(1, 2)

    @Test
    def union03(): Bool = MultiMap.union(empty(), singleton(1, 2)) == singleton(1, 2)

    @Test
    def union04(): Bool =
        MultiMap.union(empty(), singleton(1, 2) |> insert(2, 4)) |> toAssocList == (1, Set#{2}) :: (2, Set#{4}) :: Nil

    @Test
    def union05(): Bool =
        MultiMap.union(singleton(1, 2) |> insert(2, 4), empty()) |> toAssocList == (1, Set#{2}) :: (2, Set#{4}) :: Nil

    @Test
    def union06(): Bool =
        MultiMap.union(singleton(1, 2), singleton(2, 4)) |> toAssocList == (1, Set#{2}) :: (2, Set#{4}) :: Nil

    @Test
    def union07(): Bool =
        MultiMap.union(singleton(1, 2), singleton(1, 3)) |> toAssocList == (1, Set#{2, 3}) :: Nil

    @Test
    def union08(): Bool =
        MultiMap.union(singleton(1, 4), singleton(1, 2) |> insert(2, 3))
            |> toAssocList == (1, Set#{2, 4}) :: (2, Set#{3}) :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // intersection                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def intersection01(): Bool = MultiMap.intersection(empty(): MultiMap[Int32, Int32], empty()) == empty()

    @Test
    def intersection02(): Bool = MultiMap.intersection(singleton(1, 2), empty()) == empty()

    @Test
    def intersection03(): Bool = MultiMap.intersection(empty(), singleton(1, 2)) == empty()

    @Test
    def intersection04(): Bool = MultiMap.intersection(singleton(1, 2), singleton(1, 3)) == empty()

    @Test
    def intersection05(): Bool = MultiMap.intersection(singleton(1, 2), singleton(1, 2)) == singleton(1, 2)

    @Test
    def intersection06(): Bool =
        MultiMap.intersection(singleton(1, 2) |> insert(1, 3), singleton(1, 2) |> insert(1, 4)) == singleton(1, 2)

    @Test
    def intersection07(): Bool =
        MultiMap.intersection(singleton(1, 2) |> insert(1, 4), singleton(1, 2) |> insert(1, 4))
            |> toAssocList == (1, Set#{2, 4}) :: Nil

    @Test
    def intersection08(): Bool =
        MultiMap.intersection(singleton(1, 2) |> insert(1, 4), singleton(1, 4) |> insert(1, 2) |> insert(1, 3))
            |> toAssocList == (1, Set#{2, 4}) :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // difference                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def difference01(): Bool = MultiMap.difference(empty(): MultiMap[Int32, Int32], empty()) == empty()

    @Test
    def difference02(): Bool = MultiMap.difference(singleton(1, 2), empty()) == singleton(1, 2)

    @Test
    def difference03(): Bool =
        MultiMap.difference(empty(), singleton(1, 2)) == empty()

    @Test
    def difference04(): Bool =
        MultiMap.difference(singleton(1, 2), singleton(1, 3)) |> toAssocList == (1 , Set#{2}) :: Nil

    @Test
    def difference05(): Bool = MultiMap.difference(singleton(1, 2), singleton(1, 2)) == empty()

    @Test
    def difference06(): Bool =
        MultiMap.difference(singleton(1, 2) |> insert(1, 3), singleton(1, 2) |> insert(1, 4)) == singleton(1, 3)

    @Test
    def difference07(): Bool =
        MultiMap.difference(singleton(1, 2) |> insert(1, 4), singleton(1, 2) |> insert(1, 4)) == empty()

    @Test
    def difference08(): Bool =
        MultiMap.difference(singleton(1, 2) |> insert(1, 4), singleton(1, 4) |> insert(1, 2) |> insert(1, 3)) == empty()

    @Test
    def difference09(): Bool =
        MultiMap.difference(singleton(1, 2) |> insert(1, 4) |> insert(1, 3), singleton(1, 4) |> insert(1, 2))
            |> toAssocList == (1, Set#{3}) :: Nil

    @Test
    def difference10(): Bool =
        MultiMap.difference(singleton(1, 2) |> insert(1, 4) |> insert(1, 3) |> insert(2, 2), singleton(1, 4) |> insert(1, 2))
            |> toAssocList == (1, Set#{3}) :: (2, Set#{2}) :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // toList                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toList01(): Bool = MultiMap.toList(empty(): MultiMap[Int32, Int32]) == Nil

    @Test
    def toList02(): Bool = MultiMap.toList(singleton(1, 4)) == (1, 4) ::  Nil

    @Test
    def toList03(): Bool = MultiMap.toList(singleton(1, 4) |> insert(2, 4)) == (1, 4) :: (2, 4) :: Nil

    @Test
    def toList04(): Bool =
        MultiMap.toList(singleton(1, 4) |> insert(2, 2) |> insert(1, 1)) == (1, 1) :: (1, 4) :: (2, 2) :: Nil

    @Test
    def toList05(): Bool =
        MultiMap.toList(singleton(1, 4) |> insert(2, 2) |> insert(1, 1)  |> insert(3, 3)) == (1, 1) :: (1, 4) :: (2, 2) :: (3, 3) :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // toAscList                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toAscList01(): Bool = MultiMap.toAscList(empty(): MultiMap[Int32, Int32]) == Nil

    @Test
    def toAscList02(): Bool = MultiMap.toAscList(singleton(1, 4)) == (1, 4) ::  Nil

    @Test
    def toAscList03(): Bool = MultiMap.toAscList(singleton(1, 4) |> insert(2, 4)) == (1, 4) :: (2, 4) :: Nil

    @Test
    def toAscList04(): Bool =
        MultiMap.toAscList(singleton(1, 4) |> insert(2, 4) |> insert(1, 1)) == (1, 1) :: (1, 4) :: (2, 4) :: Nil

    @Test
    def toAscList05(): Bool =
        MultiMap.toAscList(singleton(1, 4) |> insert(2, 4) |> insert(1, 1)  |> insert(3, 3)) == (1, 1) :: (1, 4) :: (2, 4) :: (3, 3) :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // toDescList                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toDescList01(): Bool = MultiMap.toDescList(empty(): MultiMap[Int32, Int32]) == Nil

    @Test
    def toDescList02(): Bool = MultiMap.toDescList(singleton(1, 4)) == (1, 4) ::  Nil

    @Test
    def toDescList03(): Bool = MultiMap.toDescList(singleton(1, 4) |> insert(2, 4)) == (2, 4) :: (1, 4) :: Nil

    @Test
    def toDescList04(): Bool =
        MultiMap.toDescList(singleton(1, 4) |> insert(2, 4) |> insert(1, 1)) == (2, 4) :: (1, 4) :: (1, 1) :: Nil

    @Test
    def toDescList05(): Bool =
        MultiMap.toDescList(singleton(1, 4) |> insert(2, 4) |> insert(1, 1) |> insert(3, 3))
            == (3, 3) :: (2, 4) :: (1, 4) :: (1, 1) :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // toMap                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toMap01(): Bool = MultiMap.toMap(empty(): MultiMap[Int32, Int32]) == Map#{}

    @Test
    def toMap02(): Bool = MultiMap.toMap(singleton(1, 4)) == Map#{1 => Set#{4}}

    @Test
    def toMap03(): Bool = MultiMap.toMap(singleton(1, 4) |> insert(2, 4)) == Map#{1 => Set#{4}, 2 => Set#{4}}

    @Test
    def toMap04(): Bool =
        MultiMap.toMap(singleton(1, 4) |> insert(2, 4) |> insert(1, 1)) == Map#{1 => Set#{1, 4}, 2 => Set#{4}}

    @Test
    def toMap05(): Bool =
        MultiMap.toMap(singleton(1, 4) |> insert(2, 4) |> insert(1, 1) |> insert(3, 3))
            == Map#{1 => Set#{1, 4}, 2 => Set#{4}, 3 => Set#{3}}

    /////////////////////////////////////////////////////////////////////////////
    // toMutDeque                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toMutDeque01(): Bool = region rc {
        let d1 = MultiMap.toMutDeque(rc, empty(): MultiMap[Int32, Int32]);
        let d2 = MutDeque.new(rc);

        d1 `MutDeque.sameElements` d2
    }

    @Test
    def toMutDeque02(): Bool = region rc {
        let d1 = MultiMap.toMutDeque(rc, singleton(1, 4));

        let d2 = MutDeque.new(rc);
        MutDeque.pushBack((1, Set#{4}), d2);

        d1 `MutDeque.sameElements` d2
    }

    @Test
    def toMutDeque03(): Bool = region rc {
        let d1 = MultiMap.toMutDeque(rc, singleton(1, 4) |> insert(2, 4));

        let d2 = MutDeque.new(rc);
        MutDeque.pushBack((1, Set#{4}), d2);
        MutDeque.pushBack((2, Set#{4}), d2);

        d1 `MutDeque.sameElements` d2
    }

    @Test
    def toMutDeque04(): Bool = region rc {
        let d1 = MultiMap.toMutDeque(rc, singleton(1, 4) |> insert(2, 4) |> insert(1, 1));

        let d2 = MutDeque.new(rc);
        MutDeque.pushBack((1, Set#{1, 4}), d2);
        MutDeque.pushBack((2, Set#{4}), d2);

        d1 `MutDeque.sameElements` d2
    }

    @Test
    def toMutDeque05(): Bool = region rc {
        let d1 = MultiMap.toMutDeque(rc, singleton(1, 4) |> insert(2, 4) |> insert(1, 1) |> insert(3, 3));

        let d2 = MutDeque.new(rc);
        MutDeque.pushBack((1, Set#{1, 4}), d2);
        MutDeque.pushBack((2, Set#{4}), d2);
        MutDeque.pushBack((3, Set#{3}), d2);

        d1 `MutDeque.sameElements` d2
    }

    /////////////////////////////////////////////////////////////////////////////
    // forEach                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forEach01(): Bool =
        region rc {
            let rk = ref 0 @ rc;
            let rv = ref 0 @ rc;
            let m = empty(): MultiMap[Int32, Int32];
            MultiMap.forEach((k, v) -> {rk := k; rv := v}, m);
            deref rk == 0 and deref rv == 0
        }

    @Test
    def forEach02(): Bool =
        region rc {
            let rk = ref 0 @ rc;
            let rv = ref 0 @ rc;
            let m = singleton(1, 4);
            MultiMap.forEach((k, v) -> {rk := k; rv := v}, m);
            deref rk == 1 and deref rv == 4
        }

    @Test
    def forEach03(): Bool =
        region rc {
            let rk = ref 0 @ rc;
            let rv = ref 0 @ rc;
            let m = singleton(1, 4) |> insert(2, 4);
            MultiMap.forEach((k, v) -> {rk := k; rv := v}, m);
            deref rk == 2 and deref rv == 4
        }

    @Test
    def forEach04(): Bool =
        region rc {
            let rk = ref 0 @ rc;
            let rv = ref 0 @ rc;
            let m = singleton(1, 4) |> insert(2, 4) |> insert(1, 1);
            MultiMap.forEach((k, v) -> {rk := k; rv := v}, m);
            deref rk == 2 and deref rv == 4
        }

    @Test
    def forEach05(): Bool =
        region rc {
            let rk = ref 0 @ rc;
            let rv = ref 0 @ rc;
            let m = singleton(1, 4) |> insert(2, 4) |> insert(1, 1) |> insert(3, 3);
            MultiMap.forEach((k, v) -> {rk := k; rv := v}, m);
            deref rk == 3 and deref rv == 3
        }

    /////////////////////////////////////////////////////////////////////////////
    // forEachWithIndex                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forEachWithIndex01(): Bool =
        region rc {
            let ri = ref 0 @ rc;
            let rk = ref 0 @ rc;
            let rv = ref 0 @ rc;
            let m = empty(): MultiMap[Int32, Int32];
            MultiMap.forEachWithIndex((i, k, v) -> {ri := i; rk := k; rv := v}, m);
            deref ri == 0 and deref rk == 0 and deref rv == 0
        }

    @Test
    def forEachWithIndex02(): Bool =
        region rc {
            let ri = ref 0 @ rc;
            let rk = ref 0 @ rc;
            let rv = ref 0 @ rc;
            let m = singleton(1, 4);
            MultiMap.forEachWithIndex((i, k, v) -> {ri := i; rk := k; rv := v}, m);
            deref ri == 0 and deref rk == 1 and deref rv == 4
        }

    @Test
    def forEachWithIndex03(): Bool =
        region rc {
            let ri = ref 0 @ rc;
            let rk = ref 0 @ rc;
            let rv = ref 0 @ rc;
            let m = singleton(1, 4) |> insert(2, 4);
            MultiMap.forEachWithIndex((i, k, v) -> {ri := i; rk := k; rv := v}, m);
            deref ri == 1 and deref rk == 2 and deref rv == 4
        }

    @Test
    def forEachWithIndex04(): Bool =
        region rc {
            let ri = ref 0 @ rc;
            let rk = ref 0 @ rc;
            let rv = ref 0 @ rc;
            let m = singleton(1, 4) |> insert(2, 4) |> insert(1, 1);
            MultiMap.forEachWithIndex((i, k, v) -> {ri := i; rk := k; rv := v}, m);
            deref ri == 2 and deref rk == 2 and deref rv == 4
        }

    @Test
    def forEachWithIndex05(): Bool =
        region rc {
            let ri = ref 0 @ rc;
            let rk = ref 0 @ rc;
            let rv = ref 0 @ rc;
            let m = singleton(1, 4) |> insert(2, 4) |> insert(1, 1) |> insert(3, 3);
            MultiMap.forEachWithIndex((i, k, v) -> {ri := i; rk := k; rv := v}, m);
            deref ri == 3 and deref rk == 3 and deref rv == 3
        }

}

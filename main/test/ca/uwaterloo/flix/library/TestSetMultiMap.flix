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

namespace TestSetMultiMap {

    use SetMultiMap.{empty, singleton, insert, toAssocList};

    def isEven(i: Int32): Bool = i rem 2 == 0
    
    def isOdd(i: Int32): Bool = not isEven(i)

    /////////////////////////////////////////////////////////////////////////////
    // Eq                                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def eq01(): Bool = empty(): SetMultiMap[Int32, Char] == empty(): SetMultiMap[Int32, Char]

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

    /// Insert order doesn't matter for SetMultiMap
    @test
    def eq07(): Bool = singleton(1, 'b') |> insert(1, 'a') == singleton(1, 'a') |> insert(1, 'b')

    /////////////////////////////////////////////////////////////////////////////
    // Order (compare)                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def compare01(): Bool = Order.compare(empty(): SetMultiMap[Int32, Char], empty(): SetMultiMap[Int32, Char]) == EqualTo

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
    def empty01(): Bool = SetMultiMap.isEmpty(empty())

    /////////////////////////////////////////////////////////////////////////////
    // singleton                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def singleton01(): Bool = 
        SetMultiMap.singleton(1, 'a') |> SetMultiMap.toList |> List.sort == (1, 'a') :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // fromMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    /////////////////////////////////////////////////////////////////////////////
    // fromMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def fromMap01(): Bool = 
        SetMultiMap.fromMap(Map#{}: Map[Int32, Char]) |> toAssocList == Nil

    @test
    def fromMap02(): Bool = 
        SetMultiMap.fromMap(Map#{1 => 'a'}) |> toAssocList == (1, Set#{'a'}) :: Nil

    @test
    def fromMap03(): Bool = 
        SetMultiMap.fromMap(Map#{1 => 'a', 2 => 'b'}) |> toAssocList == (1, Set#{'a'}) :: (2, Set#{'b'}) :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // isEmpty                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def isEmpty01(): Bool = 
        SetMultiMap.isEmpty(empty()) == true
    
    @test
    def isEmpty02(): Bool = 
        SetMultiMap.isEmpty(singleton(1, 'a')) == false

    /////////////////////////////////////////////////////////////////////////////
    // get                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def get01(): Bool = 
        SetMultiMap.get(1, empty(): SetMultiMap[Int32, Char]) == Set#{}
    
    @test
    def get02(): Bool = 
        SetMultiMap.get(1, singleton(1, 'a')) == Set#{'a'}

    @test
    def get03(): Bool = 
        SetMultiMap.get(1, singleton(1, 'a') |> insert(2, 'b')) == Set#{'a'}

    @test
    def get04(): Bool = 
        SetMultiMap.get(1, singleton(1, 'a') |> insert(2, 'b') |> insert(1, 'c')) == Set#{'a', 'c'}

    /////////////////////////////////////////////////////////////////////////////
    // memberOf                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def memberOf01(): Bool = 
        not SetMultiMap.memberOf(1, empty(): SetMultiMap[Int32, Char])

    @test
    def memberOf02(): Bool = SetMultiMap.memberOf(1, singleton(1, 'a'))

    @test
    def memberOf03(): Bool = not SetMultiMap.memberOf(1, singleton(2, 'b'))

    @test
    def memberOf04(): Bool = SetMultiMap.memberOf(1, singleton(1, 'a') |> insert(2, 'b') |> insert(1, 'c'))

    /////////////////////////////////////////////////////////////////////////////
    // keysOf                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def keysOf01(): Bool = SetMultiMap.keysOf(empty(): SetMultiMap[Int32, Char]) == Set#{}

    @test
    def keysOf02(): Bool = SetMultiMap.keysOf(singleton(1, 'a')) == Set#{1}

    @test
    def keysOf03(): Bool = SetMultiMap.keysOf(singleton(2, 'b')) == Set#{2}

    @test
    def keysOf04(): Bool = 
        SetMultiMap.keysOf(singleton(1, 'a') |> insert(2, 'b') |> insert(1, 'c')) == Set#{1, 2}


    /////////////////////////////////////////////////////////////////////////////
    // valuesOf                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def valuesOf01(): Bool = SetMultiMap.valuesOf(empty(): SetMultiMap[Int32, Char]) == Nil

    @test
    def valuesOf02(): Bool = SetMultiMap.valuesOf(singleton(1, 'a')) |> List.sort == 'a' :: Nil

    @test
    def valuesOf03(): Bool = SetMultiMap.valuesOf(singleton(2, 'b')) |> List.sort == 'b' :: Nil

    @test
    def valuesOf04(): Bool = 
        SetMultiMap.valuesOf(singleton(1, 'a') |> insert(2, 'b') |> insert(1, 'c'))
            |> List.sort == 'a' :: 'b' :: 'c' :: Nil

    @test
    def valuesOf05(): Bool = 
        SetMultiMap.valuesOf(singleton(1, 'a') |> insert(2, 'b') |> insert(1, 'c') |> insert(3, 'a'))
            |> List.sort == 'a' :: 'a' :: 'b' :: 'c' :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // insert                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def insert01(): Bool = 
        SetMultiMap.insert(1, 'a', empty()) |> toAssocList == (1, Set#{'a'}) :: Nil

    @test
    def insert02(): Bool = 
        empty() |> SetMultiMap.insert(1, 'a') |> SetMultiMap.insert(2, 'b') 
            |> toAssocList == (1, Set#{'a'}) :: (2, Set#{'b'}) :: Nil

    @test
    def insert03(): Bool = 
        empty() |> SetMultiMap.insert(1, 'a') |> SetMultiMap.insert(2, 'b') |> SetMultiMap.insert(1, 'c') 
            |> toAssocList == (1, Set#{'a', 'c'}) :: (2, Set#{'b'}) :: Nil

    @test
    def insert04(): Bool = 
        empty() |> SetMultiMap.insert(1, 'a') |> SetMultiMap.insert(2, 'b') |> SetMultiMap.insert(1, 'c') |> SetMultiMap.insert(3, 'b') 
            |> toAssocList == (1, Set#{'a', 'c'}) :: (2, Set#{'b'}) :: (3, Set#{'b'}) :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // inserts                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def inserts01(): Bool = 
        SetMultiMap.inserts(1, Nil: List[Char], empty()) == empty()

    @test
    def inserts02(): Bool = 
        empty() |> SetMultiMap.inserts(1, 'a' :: Nil)|> toAssocList == (1, Set#{'a'}) :: Nil

    @test
    def inserts03(): Bool = 
        empty() |> SetMultiMap.inserts(1, 'a' :: 'c' :: Nil) |> SetMultiMap.inserts(2, 'b' :: Nil)
            |> toAssocList == (1, Set#{'a', 'c'}) :: (2, Set#{'b'}) :: Nil

    @test
    def inserts04(): Bool = 
        empty() |> SetMultiMap.inserts(1, 'a' :: 'c' :: Nil) |> SetMultiMap.inserts(2, 'b' :: Nil) |> SetMultiMap.inserts(3, 'b' :: Nil) 
            |> toAssocList == (1, Set#{'a', 'c'}) :: (2, Set#{'b'}) :: (3, Set#{'b'}) :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // adjust                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def adjust01(): Bool = SetMultiMap.adjust(v -> 2*v, 1, empty()) == empty()

    @test
    def adjust02(): Bool = 
        SetMultiMap.adjust(v -> 2*v, 1, SetMultiMap.singleton(1, 4)) |> toAssocList == (1, Set#{8}) :: Nil

    @test
    def adjust03(): Bool = 
        SetMultiMap.adjust(v -> 2*v, 3, singleton(1, 4)) |> toAssocList == (1, Set#{4}) :: Nil

    @test
    def adjust04(): Bool = 
        SetMultiMap.adjust(v -> 2*v, 1, singleton(1, 4) |> insert(2, 4) |> insert(1, 5))
            |> toAssocList == (1, Set#{8, 10}) :: (2, Set#{4}) :: Nil

    @test
    def adjust05(): Bool = 
        SetMultiMap.adjust(v -> 2*v, 2, singleton(1, 4) |> insert(2, 4) |> insert(1, 5))
            |> toAssocList == (1, Set#{4, 5}) :: (2, Set#{8}) :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // adjustWithKey                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def adjustWithKey01(): Bool = SetMultiMap.adjustWithKey((k, v) -> k+v, 1, empty()) == empty()

    @test
    def adjustWithKey02(): Bool = 
        SetMultiMap.adjustWithKey((k, v) -> k+v, 1, singleton(1, 4))
            |> toAssocList == (1, Set#{5}) :: Nil

    @test
    def adjustWithKey03(): Bool = 
        SetMultiMap.adjustWithKey((k, v) -> k+v, 3, singleton(1, 4))
            |> toAssocList == (1, Set#{4}) :: Nil

    @test
    def adjustWithKey04(): Bool = 
        SetMultiMap.adjustWithKey((k, v) -> k+v, 1, singleton(1, 4) |> insert(2, 4) |> insert(1, 5))
            |> toAssocList == (1, Set#{5, 6}) :: (2, Set#{4}) :: Nil

    @test
    def adjustWithKey05(): Bool = 
        SetMultiMap.adjustWithKey((k, v) -> k+v, 2, singleton(1, 4) |> insert(2, 4) |> insert(1, 5))
            |> toAssocList == (1, Set#{4, 5}) :: (2, Set#{6}) :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // update                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def update01(): Bool = SetMultiMap.update(v -> if (isEven(v)) Some(2*v) else None, 1, empty()) == empty()

    @test
    def update02(): Bool = 
        SetMultiMap.update(v -> if (isEven(v)) Some(2*v) else None, 1, singleton(1, 4))
            |> toAssocList == (1, Set#{8}) :: Nil

    @test
    def update03(): Bool = 
        SetMultiMap.update(v -> if (isEven(v)) Some(2*v) else None, 3, singleton(1, 4))
            |> toAssocList == (1, Set#{4}) :: Nil

    @test
    def update04(): Bool = 
        SetMultiMap.update(v -> if (isEven(v)) Some(2*v) else None, 1, singleton(1, 4) |> insert(2, 4) |> insert(1, 5))
            |> toAssocList == (1, Set#{5, 8}) :: (2, Set#{4}) :: Nil

    @test
    def update05(): Bool = 
        SetMultiMap.update(v -> if (isEven(v)) Some(2*v) else None, 2, singleton(1, 4) |> insert(2, 4) |> insert(1, 5))
            |> toAssocList == (1, Set#{4, 5}) :: (2, Set#{8}) :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // updateWithKey                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def updateWithKey01(): Bool = SetMultiMap.updateWithKey((k, v) -> if (isEven(k)) Some(k+v) else None, 1, empty()) == empty()

    @test
    def updateWithKey02(): Bool = 
        SetMultiMap.updateWithKey((k, v) -> if (isEven(k)) Some(k+v) else None, 1, singleton(1, 4))
            |> toAssocList == (1, Set#{4}) :: Nil

    @test
    def updateWithKey03(): Bool = 
        SetMultiMap.updateWithKey((k, v) -> if (isOdd(k)) Some(k+v) else None, 1, singleton(1, 4))
            |> toAssocList == (1, Set#{5}) :: Nil

    @test
    def updateWithKey04(): Bool = 
        SetMultiMap.updateWithKey((k, v) -> if (isEven(k)) Some(k+v) else None, 3, singleton(1, 4))
            |> toAssocList == (1, Set#{4}) :: Nil

    @test
    def updateWithKey05(): Bool = 
        SetMultiMap.updateWithKey((k, v) -> if (isOdd(k)) Some(k+v) else None, 3, singleton(1, 4))
            |> toAssocList == (1, Set#{4}) :: Nil

    @test
    def updateWithKey06(): Bool = 
        SetMultiMap.updateWithKey((k, v) -> if (isEven(k)) Some(k+v) else None, 1, singleton(1, 4) |> insert(2, 4) |> insert(1, 5))
            |> toAssocList == (1, Set#{4, 5}) :: (2, Set#{4}) :: Nil

    @test
    def updateWithKey07(): Bool = 
        SetMultiMap.updateWithKey((k, v) -> if (isOdd(k)) Some(k+v) else None, 1, singleton(1, 4) |> insert(2, 4) |> insert(1, 5))
            |> toAssocList == (1, Set#{5, 6}) :: (2, Set#{4}) :: Nil

    @test
    def updateWithKey08(): Bool = 
        SetMultiMap.updateWithKey((k, v) -> if (isEven(k)) Some(k+v) else None, 2, singleton(1, 4) |> insert(2, 4) |> insert(1, 5))
            |> toAssocList == (1, Set#{4, 5}) :: (2, Set#{6}) :: Nil

    @test
    def updateWithKey09(): Bool = 
        SetMultiMap.updateWithKey((k, v) -> if (isOdd(k)) Some(k+v) else None, 2, singleton(1, 4) |> insert(2, 4) |> insert(1, 5))
            |> toAssocList == (1, Set#{4, 5}) :: (2, Set#{4}) :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // remove                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def remove01(): Bool = SetMultiMap.remove(1, empty(): SetMultiMap[Int32, Int32]) == empty()

    @test
    def remove02(): Bool = SetMultiMap.remove(1, singleton(1, 4)) |> toAssocList == Nil

    @test
    def remove03(): Bool = SetMultiMap.remove(3, singleton(1, 4)) |> toAssocList == (1, Set#{4}) :: Nil

    @test
    def remove04(): Bool = 
        SetMultiMap.remove(1, singleton(1, 4) |> insert(2, 4) |> insert(1, 5)) |> toAssocList == (2, Set#{4}) :: Nil

    @test
    def remove05(): Bool = 
        SetMultiMap.remove(2, singleton(1, 4) |> insert(2, 4) |> insert(1, 5)) |> toAssocList == (1, Set#{4, 5}) :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // find                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def find01(): Bool = SetMultiMap.find((k, v) -> k == v, empty(): SetMultiMap[Int32, Int32]) == None

    @test
    def find02(): Bool = SetMultiMap.find((k, v) -> k == v, singleton(1, 2)) == None

    @test
    def find03(): Bool = SetMultiMap.find((k, v) -> k == v, singleton(1, 1)) == Some((1, 1))

    @test
    def find04(): Bool = 
        SetMultiMap.find((k, v) -> k == v, singleton(1, 4) |> insert(2, 4) |> insert(1, 5)) == None

    @test
    def find05(): Bool = 
        SetMultiMap.find((k, v) -> k == v, singleton(1, 4) |> insert(2, 2) |> insert(3, 3)) == Some((2,2))

    /////////////////////////////////////////////////////////////////////////////
    // findLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def findLeft01(): Bool = SetMultiMap.findLeft((k, v) -> k == v, empty(): SetMultiMap[Int32, Int32]) == None

    @test
    def findLeft02(): Bool = SetMultiMap.findLeft((k, v) -> k == v, singleton(1, 2)) == None

    @test
    def findLeft03(): Bool = SetMultiMap.findLeft((k, v) -> k == v, singleton(1, 1)) == Some((1, 1))

    @test
    def findLeft04(): Bool = 
        SetMultiMap.findLeft((k, v) -> k == v, singleton(1, 4) |> insert(2, 4) |> insert(1, 5)) == None

    @test
    def findLeft05(): Bool = 
        SetMultiMap.findLeft((k, v) -> k == v, singleton(1, 4) |> insert(2, 2) |> insert(3, 3)) == Some((2, 2))

    /////////////////////////////////////////////////////////////////////////////
    // findRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def findRight01(): Bool = SetMultiMap.findRight((k, v) -> k == v, empty(): SetMultiMap[Int32, Int32]) == None

    @test
    def findRight02(): Bool = SetMultiMap.findRight((k, v) -> k == v, singleton(1, 2)) == None

    @test
    def findRight03(): Bool = SetMultiMap.findRight((k, v) -> k == v, singleton(1, 1)) == Some((1, 1))

    @test
    def findRight04(): Bool = 
        SetMultiMap.findRight((k, v) -> k == v, singleton(1, 4) |> insert(2, 4) |> insert(1, 5)) == None

    @test
    def findRight05(): Bool = 
        SetMultiMap.findRight((k, v) -> k == v, singleton(1, 4) |> insert(2, 2) |> insert(3, 3)) == Some((3, 3))

    /////////////////////////////////////////////////////////////////////////////
    // filter                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filter01(): Bool = 
        SetMultiMap.filter(isEven, empty(): SetMultiMap[Int32, Int32]) == empty()

    @test
    def filter02(): Bool = SetMultiMap.filter(isEven, singleton(1, 2)) |> toAssocList == (1, Set#{2}) :: Nil

    @test
    def filter03(): Bool = SetMultiMap.filter(isEven, singleton(1, 1)) == empty()

    @test
    def filter04(): Bool = 
        SetMultiMap.filter(isEven, singleton(1, 4) |> insert(2, 4))
            |> toAssocList == (1, Set#{4}) :: (2, Set#{4}) :: Nil

    @test
    def filter05(): Bool = 
        SetMultiMap.filter(isEven, singleton(1, 4) |> insert(2, 2) |> insert(1, 3)) 
            |> toAssocList == (1, Set#{4}) :: (2, Set#{2}) :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // filterWithKey                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filterWithKey01(): Bool = 
        SetMultiMap.filterWithKey((k, v) -> k == v, empty(): SetMultiMap[Int32, Int32]) == empty()

    @test
    def filterWithKey02(): Bool = SetMultiMap.filterWithKey((k, v) -> k == v, singleton(1, 2)) == empty()

    @test
    def filterWithKey03(): Bool = 
        SetMultiMap.filterWithKey((k, v) -> k == v, singleton(1, 1)) |> toAssocList == (1, Set#{1}) :: Nil

    @test
    def filterWithKey04(): Bool = 
        SetMultiMap.filterWithKey((k, v) -> k == v, singleton(1, 4) |> insert(2, 4)) == empty()

    @test
    def filterWithKey05(): Bool = 
        SetMultiMap.filterWithKey((k, v) -> k == v, singleton(1, 4) |> insert(2, 2) |> insert(1, 1)) 
            |> toAssocList == (1, Set#{1}) :: (2, Set#{2}) :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // map                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def map01(): Bool = SetMultiMap.map(v -> v*3, empty(): SetMultiMap[Int32, Int32]) == empty()

    @test
    def map02(): Bool = 
        SetMultiMap.map(v -> v*3, singleton(1, 2)) |> toAssocList == (1, Set#{6}) :: Nil

    @test
    def map03(): Bool = 
        SetMultiMap.map(v -> v*3, singleton(1, 1)) |> toAssocList == (1, Set#{3}) :: Nil

    @test
    def map04(): Bool = 
        SetMultiMap.map(v -> v*3, singleton(1, 4) |> insert(2, 4))
            |> toAssocList == (1, Set#{12}) :: (2, Set#{12}) :: Nil

    @test
    def map05(): Bool = 
        SetMultiMap.map(v -> v*3, singleton(1, 4) |> insert(2, 2) |> insert(1, 1)) 
            |> toAssocList == (1, Set#{3, 12}) :: (2, Set#{6}) :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // mapWithKey                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapWithKey01(): Bool = 
        SetMultiMap.mapWithKey((k, v) -> v*3+k, empty(): SetMultiMap[Int32, Int32]) == empty()

    @test
    def mapWithKey02(): Bool = 
        SetMultiMap.mapWithKey((k, v) -> v*3+k, singleton(1, 2)) |> toAssocList == (1, Set#{7}) :: Nil

    @test
    def mapWithKey03(): Bool = 
        SetMultiMap.mapWithKey((k, v) -> v*3+k, singleton(1, 1)) |> toAssocList == (1, Set#{4}) :: Nil

    @test
    def mapWithKey04(): Bool = 
        SetMultiMap.mapWithKey((k, v) -> v*3+k, singleton(1, 4) |> insert(2, 4))
            |> toAssocList == (1, Set#{13}) :: (2, Set#{14}) :: Nil

    @test
    def mapWithKey05(): Bool = 
        SetMultiMap.mapWithKey((k, v) -> v*3+k, singleton(1, 4) |> insert(2, 2) |> insert(1, 1)) 
            |> toAssocList == (1, Set#{4, 13}) :: (2, Set#{8}) :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // foldWithKey                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldWithKey01(): Bool = 
        SetMultiMap.foldWithKey((acc, k, v) -> acc + k + v, 0, empty(): SetMultiMap[Int32, Int32]) == 0

    @test
    def foldWithKey02(): Bool = 
        SetMultiMap.foldWithKey((acc, k, v) -> acc + k + v, 0, singleton(1, 2)) == 3

    @test
    def foldWithKey03(): Bool = 
        SetMultiMap.foldWithKey((acc, k, v) -> acc + k + v, 0, singleton(1, 4) |> insert(2, 4)) == 11

    @test
    def foldWithKey04(): Bool = 
        SetMultiMap.foldWithKey((acc, k, v) -> acc + k + v, 0, 
            singleton(1, 4) |> insert(2, 2) |> insert(1, 1)) == 11

    @test
    def foldWithKey05(): Bool = 
        SetMultiMap.foldWithKey((acc, k, v) -> acc + k + v, 0, 
            singleton(1, 4) |> insert(2, 2) |> insert(1, 1) |> insert(3, 3)) == 17

    /////////////////////////////////////////////////////////////////////////////
    // foldLeft                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldLeft01(): Bool = 
        SetMultiMap.foldLeft((acc, v) -> v :: acc, Nil, empty(): SetMultiMap[Int32, Int32]) == Nil

    @test
    def foldLeft02(): Bool = 
        SetMultiMap.foldLeft((acc, v) -> v :: acc, Nil, singleton(1, 2)) == 2 :: Nil

    @test
    def foldLeft03(): Bool = 
        SetMultiMap.foldLeft((acc, v) -> v :: acc, Nil, singleton(1, 4) |> insert(2, 5)) == 5 :: 4 :: Nil

    @test
    def foldLeft04(): Bool = 
        SetMultiMap.foldLeft((acc, v) -> v :: acc, Nil, 
            singleton(1, 4) |> insert(2, 2) |> insert(1, 1)) == 2 :: 4 :: 1 :: Nil

    @test
    def foldLeft05(): Bool = 
        SetMultiMap.foldLeft((acc, v) -> v :: acc, Nil, 
            singleton(1, 4) |> insert(2, 2) |> insert(1, 1) |> insert(3, 3)) == 3 :: 2 :: 4 :: 1 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // foldLeftWithKey                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldLeftWithKey01(): Bool = 
        SetMultiMap.foldLeftWithKey((acc, k, v) -> (k, v) :: acc, Nil, empty(): SetMultiMap[Int32, Int32]) == Nil

    @test
    def foldLeftWithKey02(): Bool = 
        SetMultiMap.foldLeftWithKey((acc, k, v) -> (k, v) :: acc, Nil, singleton(1, 2)) == (1, 2) :: Nil

    @test
    def foldLeftWithKey03(): Bool = 
        SetMultiMap.foldLeftWithKey((acc, k, v) -> (k, v) :: acc, Nil, 
            singleton(1, 4) |> insert(2, 5)) == (2, 5) :: (1, 4) :: Nil

    @test
    def foldLeftWithKey04(): Bool = 
        SetMultiMap.foldLeftWithKey((acc, k, v) -> (k, v) :: acc, Nil, 
            singleton(1, 4) |> insert(2, 2) |> insert(1, 1)) == (2, 2) :: (1, 4) :: (1, 1) :: Nil

    @test
    def foldLeftWithKey05(): Bool = 
        SetMultiMap.foldLeftWithKey((acc, k, v) -> (k, v) :: acc, Nil, 
            singleton(1, 4) |> insert(2, 2) |> insert(1, 1) |> insert(3, 3)) == (3, 3) :: (2, 2) :: (1, 4) :: (1, 1) :: Nil

    @test
    def foldLeftWithKey06(): Bool = 
        SetMultiMap.foldLeftWithKey((acc, k, v) -> (k, v) :: acc, Nil, 
            singleton(1, 4) |> insert(2, 2) |> insert(1, 1) |> insert(3, 3) |> insert(1, 2)) == (3, 3) :: (2, 2) :: (1, 4) :: (1, 2) :: (1, 1) :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // foldRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldRight01(): Bool = 
        SetMultiMap.foldRight((v, acc) -> v :: acc, Nil, empty(): SetMultiMap[Int32, Int32]) == Nil

    @test
    def foldRight02(): Bool = 
        SetMultiMap.foldRight((v, acc) -> v :: acc, Nil, singleton(1, 2)) == 2 :: Nil

    @test
    def foldRight03(): Bool = 
        SetMultiMap.foldRight((v, acc) -> v :: acc, Nil, singleton(1, 4) |> insert(2, 5)) == 4 :: 5 :: Nil

    @test
    def foldRight04(): Bool = 
        SetMultiMap.foldRight((v, acc) -> v :: acc, Nil, 
            singleton(1, 4) |> insert(2, 2) |> insert(1, 1)) == 1 :: 4 :: 2 :: Nil

    @test
    def foldRight05(): Bool = 
        SetMultiMap.foldRight((v, acc) -> v :: acc, Nil, 
            singleton(1, 4) |> insert(2, 2) |> insert(1, 1) |> insert(3, 3)) == 1 :: 4 :: 2 :: 3 :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // foldRightWithKey                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldRightWithKey01(): Bool = 
        SetMultiMap.foldRightWithKey((k, v, acc) -> (k, v) :: acc, Nil, empty(): SetMultiMap[Int32, Int32]) == Nil

    @test
    def foldRightWithKey02(): Bool = 
        SetMultiMap.foldRightWithKey((k, v, acc) -> (k, v) :: acc, Nil, singleton(1, 2)) == (1, 2) :: Nil

    @test
    def foldRightWithKey03(): Bool = 
        SetMultiMap.foldRightWithKey((k, v, acc) -> (k, v) :: acc, Nil, 
            singleton(1, 4) |> insert(2, 5)) == (1, 4) :: (2, 5) :: Nil

    @test
    def foldRightWithKey04(): Bool = 
        SetMultiMap.foldRightWithKey((k, v, acc) -> (k, v) :: acc, Nil, 
            singleton(1, 4) |> insert(2, 2) |> insert(1, 1)) == (1, 1) :: (1, 4) :: (2, 2) :: Nil

    @test
    def foldRightWithKey05(): Bool = 
        SetMultiMap.foldRightWithKey((k, v, acc) -> (k, v) :: acc, Nil, 
            singleton(1, 4) |> insert(2, 2) |> insert(1, 1) |> insert(3, 3)) == (1, 1) :: (1, 4) :: (2, 2) :: (3, 3) :: Nil

    @test
    def foldRightWithKey06(): Bool = 
        SetMultiMap.foldRightWithKey((k, v, acc) -> (k, v) :: acc, Nil, 
            singleton(1, 4) |> insert(2, 2) |> insert(1, 1) |> insert(3, 3) |> insert(1, 2)) == (1, 1) :: (1, 2) :: (1, 4) :: (2, 2) :: (3, 3) :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // reduceLeft                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reduceLeft01(): Bool = 
        SetMultiMap.reduceLeft((acc, v) -> acc - v, empty(): SetMultiMap[Int32, Int32]) == None

    @test
    def reduceLeft02(): Bool = 
        SetMultiMap.reduceLeft((acc, v) -> acc - v, singleton(1, 2)) == Some(2)

    @test
    def reduceLeft03(): Bool = 
        SetMultiMap.reduceLeft((acc, v) -> acc - v, singleton(1, 4) |> insert(2, 5)) == Some(-1)

    /// 1 - 4 - 2
    @test
    def reduceLeft04(): Bool =           
        SetMultiMap.reduceLeft((acc, v) -> acc - v, 
            singleton(1, 4) |> insert(2, 2) |> insert(1, 1)) == Some(-5)

    /// 1 - 4 - 2 - 3
    @test
    def reduceLeft05(): Bool =
        SetMultiMap.reduceLeft((acc, v) -> acc - v,  
            singleton(1, 4) |> insert(2, 2) |> insert(1, 1) |> insert(3, 3)) == Some(-8)

    /// First value
    @test
    def reduceLeft06(): Bool =
        SetMultiMap.reduceLeft((v1, _) -> v1, 
            singleton(1, 4) |> insert(2, 2) |> insert(1, 1) |> insert(3, 3)) == Some(1)

    /// Last value
    @test
    def reduceLeft07(): Bool =
        SetMultiMap.reduceLeft((_, v2) -> v2, 
            singleton(1, 4) |> insert(2, 2) |> insert(1, 1) |> insert(3, 3)) == Some(3)


    /////////////////////////////////////////////////////////////////////////////
    // reduceLeftWithKey                                                       //
    /////////////////////////////////////////////////////////////////////////////

    /// Leftmost key, rightmost value

    @test
    def reduceLeftWithKey01(): Bool = 
        SetMultiMap.reduceLeftWithKey((k1, _, _, v2) -> (k1, v2), empty(): SetMultiMap[Int32, Int32]) == None

    @test
    def reduceLeftWithKey02(): Bool = 
        SetMultiMap.reduceLeftWithKey((k1, _, _, v2) -> (k1, v2), singleton(1, 2)) == Some((1, 2))

    @test
    def reduceLeftWithKey03(): Bool = 
        SetMultiMap.reduceLeftWithKey((k1, _, _, v2) -> (k1, v2), singleton(1, 1)) == Some((1, 1))

    @test
    def reduceLeftWithKey04(): Bool = 
        SetMultiMap.reduceLeftWithKey((k1, _, _, v2) -> (k1, v2), 
            singleton(1, 4) |> insert(2, 5)) == Some((1, 5))

    @test
    def reduceLeftWithKey05(): Bool =           
        SetMultiMap.reduceLeftWithKey((k1, _, _, v2) -> (k1, v2), 
            singleton(1, 4) |> insert(2, 2) |> insert(1, 1)) == Some((1, 2))

    /////////////////////////////////////////////////////////////////////////////
    // reduceRight                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reduceRight01(): Bool = 
        SetMultiMap.reduceRight((v, acc) -> acc - v, empty(): SetMultiMap[Int32, Int32]) == None

    @test
    def reduceRight02(): Bool = 
        SetMultiMap.reduceRight((v, acc) -> acc - v, singleton(1, 2)) == Some(2)

    @test
    def reduceRight03(): Bool = 
        SetMultiMap.reduceRight((v, acc) -> acc - v, singleton(1, 4) |> insert(2, 5)) == Some(1)

    @test
    def reduceRight04(): Bool =           
        SetMultiMap.reduceRight((v, acc) -> acc - v, 
            singleton(1, 4) |> insert(2, 2) |> insert(1, 1)) == Some(-3)

    /// 3 - 2 - 4 - 1
    @test
    def reduceRight05(): Bool =           
        SetMultiMap.reduceRight((v, acc) -> acc - v, 
            singleton(1, 4) |> insert(2, 2) |> insert(1, 1) |> insert(3, 3)) == Some(-4)

    /// First value
    @test
    def reduceRight06(): Bool =
        SetMultiMap.reduceRight((_, acc) -> acc, 
            singleton(1, 4) |> insert(2, 2) |> insert(1, 1) |> insert(3, 3)) == Some(3)

    /// Last value
    @test
    def reduceRight07(): Bool =           
        SetMultiMap.reduceRight((v, _) -> v, 
            singleton(1, 4) |> insert(2, 2) |> insert(1, 1) |> insert(3, 3)) == Some(1)


    /////////////////////////////////////////////////////////////////////////////
    // reduceRightWithKey                                                      //
    /////////////////////////////////////////////////////////////////////////////

    /// Rightmost key, leftmost value

    @test
    def reduceRightWithKey01(): Bool = 
        SetMultiMap.reduceRightWithKey((_, v1, k2, _) -> (k2, v1), empty(): SetMultiMap[Int32, Int32]) == None

    @test
    def reduceRightWithKey02(): Bool = 
        SetMultiMap.reduceRightWithKey((_, v1, k2, _) -> (k2, v1), singleton(1, 2)) == Some((1, 2))

    @test
    def reduceRightWithKey03(): Bool = 
        SetMultiMap.reduceRightWithKey((_, v1, k2, _) -> (k2, v1), 
            singleton(1, 4) |> insert(2, 5)) == Some((2, 4))

    @test
    def reduceRightWithKey04(): Bool =
        SetMultiMap.reduceRightWithKey((_, v1, k2, _) -> (k2, v1), 
            singleton(1, 4) |> insert(2, 2) |> insert(1, 1)) == Some((2, 1))

    @test
    def reduceRightWithKey05(): Bool =
        SetMultiMap.reduceRightWithKey((_, v1, k2, _) -> (k2, v1), 
            singleton(1, 4) |> insert(2, 2) |> insert(1, 1) |> insert(3, 3)) == Some((3, 1))

    /////////////////////////////////////////////////////////////////////////////
    // count                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def count01(): Bool = 
        SetMultiMap.count((k, v) -> v > k, empty(): SetMultiMap[Int32, Int32]) == 0

    @test
    def count02(): Bool = 
        SetMultiMap.count((k, v) -> v > k, singleton(1, 2)) == 1

    @test
    def count03(): Bool = 
        SetMultiMap.count((k, v) -> v > k, singleton(1, 4) |> insert(2, 4)) == 2

    @test
    def count04(): Bool = 
        SetMultiMap.count((k, v) -> v > k, 
            singleton(1, 4) |> insert(2, 2) |> insert(1, 1)) == 1

    @test
    def count05(): Bool = 
        SetMultiMap.count((k, v) -> v > k, 
            singleton(1, 4) |> insert(2, 2) |> insert(1, 1) |> insert(3, 3)) == 1


    /////////////////////////////////////////////////////////////////////////////
    // sumKeys                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sumKeys01(): Bool = SetMultiMap.sumKeys(empty(): SetMultiMap[Int32, Int32]) == 0

    @test
    def sumKeys02(): Bool = SetMultiMap.sumKeys(singleton(1, 2)) == 1

    @test
    def sumKeys03(): Bool = SetMultiMap.sumKeys(singleton(1, 4) |> insert(2, 4)) == 3

    @test
    def sumKeys04(): Bool = 
        SetMultiMap.sumKeys(singleton(1, 4) |> insert(2, 2) |> insert(1, 1)) == 4

    @test
    def sumKeys05(): Bool = 
        SetMultiMap.sumKeys(singleton(1, 4) |> insert(2, 2) |> insert(1, 1) |> insert(3, 3)) == 7

    /////////////////////////////////////////////////////////////////////////////
    // sumValues                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sumValues01(): Bool = SetMultiMap.sumValues(empty(): SetMultiMap[Int32, Int32]) == 0

    @test
    def sumValues02(): Bool = SetMultiMap.sumValues(singleton(1, 2)) == 2

    @test
    def sumValues03(): Bool = SetMultiMap.sumValues(singleton(1, 4) |> insert(2, 4)) == 8

    @test
    def sumValues04(): Bool = 
        SetMultiMap.sumValues(singleton(1, 4) |> insert(2, 2) |> insert(1, 1)) == 7

    @test
    def sumValues05(): Bool = 
        SetMultiMap.sumValues(singleton(1, 4) |> insert(2, 2) |> insert(1, 1) |> insert(3, 3)) == 10

    /////////////////////////////////////////////////////////////////////////////
    // sumWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sumWith01(): Bool = SetMultiMap.sumWith((k, v) -> if (v > k) 10 else 0, empty(): SetMultiMap[Int32, Int32]) == 0

    @test
    def sumWith02(): Bool = SetMultiMap.sumWith((k, v) -> if (v > k) 10 else 0, singleton(1, 2)) == 10

    @test
    def sumWith03(): Bool = 
        SetMultiMap.sumWith((k, v) -> if (v > k) 10 else 0, singleton(1, 4) |> insert(2, 4)) == 20

    @test
    def sumWith04(): Bool = 
        SetMultiMap.sumWith((k, v) -> if (v > k) 10 else 0, 
            singleton(1, 4) |> insert(2, 2) |> insert(1, 1)) == 10

    @test
    def sumWith05(): Bool = 
        SetMultiMap.sumWith((k, v) -> if (v > k) 10 else 0, 
            singleton(1, 4) |> insert(2, 2) |> insert(1, 1) |> insert(3, 3)) == 10

    /////////////////////////////////////////////////////////////////////////////
    // productKeys                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def productKeys01(): Bool = SetMultiMap.productKeys(empty(): SetMultiMap[Int32, Int32]) == 0

    @test
    def productKeys02(): Bool = SetMultiMap.productKeys(singleton(1, 2)) == 1

    @test
    def productKeys03(): Bool = SetMultiMap.productKeys(singleton(1, 4) |> insert(2, 4)) == 2

    @test
    def productKeys04(): Bool = 
        SetMultiMap.productKeys(singleton(1, 4) |> insert(2, 2) |> insert(1, 1)) == 2

    @test
    def productKeys05(): Bool = 
        SetMultiMap.productKeys(singleton(1, 4) |> insert(2, 2) |> insert(1, 1) |> insert(3, 3)) == 6

    /////////////////////////////////////////////////////////////////////////////
    // productValues                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def productValues01(): Bool = SetMultiMap.productValues(empty(): SetMultiMap[Int32, Int32]) == 0

    @test
    def productValues02(): Bool = SetMultiMap.productValues(singleton(1, 2)) == 2

    @test
    def productValues03(): Bool = SetMultiMap.productValues(singleton(1, 4) |> insert(2, 4)) == 16

    @test
    def productValues04(): Bool = 
        SetMultiMap.productValues(singleton(1, 4) |> insert(2, 2) |> insert(1, 1)) == 8

    @test
    def productValues05(): Bool = 
        SetMultiMap.productValues(singleton(1, 4) |> insert(2, 2) |> insert(1, 1) |> insert(3, 3)) == 24

    /////////////////////////////////////////////////////////////////////////////
    // productWith                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def productWith01(): Bool = SetMultiMap.productWith((k, v) -> if (v > k) 10 else 1, empty(): SetMultiMap[Int32, Int32]) == 0

    @test
    def productWith02(): Bool = SetMultiMap.productWith((k, v) -> if (v > k) 10 else 1, singleton(1, 2)) == 10

    @test
    def productWith03(): Bool = 
        SetMultiMap.productWith((k, v) -> if (v > k) 10 else 1, singleton(1, 4) |> insert(2, 4)) == 100

    @test
    def productWith04(): Bool = 
        SetMultiMap.productWith((k, v) -> if (v > k) 10 else 1, 
            singleton(1, 4) |> insert(2, 2) |> insert(1, 1)) == 10

    @test
    def productWith05(): Bool = 
        SetMultiMap.productWith((k, v) -> if (v > k) 10 else 1, 
            singleton(1, 4) |> insert(2, 2) |> insert(1, 1) |> insert(3, 3)) == 10

    /////////////////////////////////////////////////////////////////////////////
    // exists                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def exists01(): Bool = not SetMultiMap.exists((k, v) -> v > k, empty(): SetMultiMap[Int32, Int32])

    @test
    def exists02(): Bool = SetMultiMap.exists((k, v) -> v > k, singleton(1, 2))

    @test
    def exists03(): Bool = SetMultiMap.exists((k, v) -> v > k, singleton(1, 4) |> insert(2, 4))

    @test
    def exists04(): Bool = 
        SetMultiMap.exists((k, v) -> v > k, singleton(1, 4) |> insert(2, 2) |> insert(1, 1))

    @test
    def exists05(): Bool = 
        SetMultiMap.exists((k, v) -> v > k, singleton(1, 4) |> insert(2, 2) |> insert(1, 1) |> insert(3, 3))
    
    /////////////////////////////////////////////////////////////////////////////
    // forall                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def forall01(): Bool = SetMultiMap.forall((k, v) -> v > k, empty(): SetMultiMap[Int32, Int32])

    @test
    def forall02(): Bool = SetMultiMap.forall((k, v) -> v > k, singleton(1, 2))

    @test
    def forall03(): Bool = SetMultiMap.forall((k, v) -> v > k, singleton(1, 4) |> insert(2, 4))

    @test
    def forall04(): Bool = 
        not SetMultiMap.forall((k, v) -> v > k, singleton(1, 4) |> insert(2, 2) |> insert(1, 1))

    @test
    def forall05(): Bool = 
        not SetMultiMap.forall((k, v) -> v > k, singleton(1, 4) |> insert(2, 2) |> insert(1, 1)  |> insert(3, 3))

    @test
    def forall06(): Bool = 
        SetMultiMap.forall((k, v) -> v > k, singleton(1, 4) |> insert(2, 3) |> insert(1, 2)  |> insert(3, 4))

    /////////////////////////////////////////////////////////////////////////////
    // union                                                                   //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def union01(): Bool = SetMultiMap.union(empty(): SetMultiMap[Int32, Int32], empty()) == empty()

    @test
    def union02(): Bool = SetMultiMap.union(singleton(1, 2), empty()) == singleton(1, 2)

    @test
    def union03(): Bool = SetMultiMap.union(empty(), singleton(1, 2)) == singleton(1, 2)

    @test
    def union04(): Bool = 
        SetMultiMap.union(empty(), singleton(1, 2) |> insert(2, 4)) |> toAssocList == (1, Set#{2}) :: (2, Set#{4}) :: Nil

    @test
    def union05(): Bool = 
        SetMultiMap.union(singleton(1, 2) |> insert(2, 4), empty()) |> toAssocList == (1, Set#{2}) :: (2, Set#{4}) :: Nil

    @test
    def union06(): Bool = 
        SetMultiMap.union(singleton(1, 2), singleton(2, 4)) |> toAssocList == (1, Set#{2}) :: (2, Set#{4}) :: Nil

    @test
    def union07(): Bool = 
        SetMultiMap.union(singleton(1, 2), singleton(1, 3)) |> toAssocList == (1, Set#{2, 3}) :: Nil

    @test
    def union08(): Bool = 
        SetMultiMap.union(singleton(1, 4), singleton(1, 2) |> insert(2, 3)) 
            |> toAssocList == (1, Set#{2, 4}) :: (2, Set#{3}) :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // intersection                                                            //
    /////////////////////////////////////////////////////////////////////////////
    
    @test
    def intersection01(): Bool = SetMultiMap.intersection(empty(): SetMultiMap[Int32, Int32], empty()) == empty()

    @test
    def intersection02(): Bool = SetMultiMap.intersection(singleton(1, 2), empty()) == empty()

    @test
    def intersection03(): Bool = SetMultiMap.intersection(empty(), singleton(1, 2)) == empty()

    @test
    def intersection04(): Bool = SetMultiMap.intersection(singleton(1, 2), singleton(1, 3)) == empty()

    @test
    def intersection05(): Bool = SetMultiMap.intersection(singleton(1, 2), singleton(1, 2)) == singleton(1, 2)

    @test
    def intersection06(): Bool = 
        SetMultiMap.intersection(singleton(1, 2) |> insert(1, 3), singleton(1, 2) |> insert(1, 4)) == singleton(1, 2)

    @test
    def intersection07(): Bool = 
        SetMultiMap.intersection(singleton(1, 2) |> insert(1, 4), singleton(1, 2) |> insert(1, 4))
            |> toAssocList == (1, Set#{2, 4}) :: Nil

    @test
    def intersection08(): Bool = 
        SetMultiMap.intersection(singleton(1, 2) |> insert(1, 4), singleton(1, 4) |> insert(1, 2) |> insert(1, 3))
            |> toAssocList == (1, Set#{2, 4}) :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // difference                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def difference01(): Bool = SetMultiMap.difference(empty(): SetMultiMap[Int32, Int32], empty()) == empty()

    @test
    def difference02(): Bool = SetMultiMap.difference(singleton(1, 2), empty()) == singleton(1, 2)

    @test
    def difference03(): Bool = 
        SetMultiMap.difference(empty(), singleton(1, 2)) == empty()

    @test
    def difference04(): Bool =
        SetMultiMap.difference(singleton(1, 2), singleton(1, 3)) |> toAssocList == (1 , Set#{2}) :: Nil

    @test
    def difference05(): Bool = SetMultiMap.difference(singleton(1, 2), singleton(1, 2)) == empty()

    @test
    def difference06(): Bool = 
        SetMultiMap.difference(singleton(1, 2) |> insert(1, 3), singleton(1, 2) |> insert(1, 4)) == singleton(1, 3)

    @test
    def difference07(): Bool = 
        SetMultiMap.difference(singleton(1, 2) |> insert(1, 4), singleton(1, 2) |> insert(1, 4)) == empty()

    @test
    def difference08(): Bool = 
        SetMultiMap.difference(singleton(1, 2) |> insert(1, 4), singleton(1, 4) |> insert(1, 2) |> insert(1, 3)) == empty()

    @test
    def difference09(): Bool = 
        SetMultiMap.difference(singleton(1, 2) |> insert(1, 4) |> insert(1, 3), singleton(1, 4) |> insert(1, 2))
            |> toAssocList == (1, Set#{3}) :: Nil

    @test
    def difference10(): Bool = 
        SetMultiMap.difference(singleton(1, 2) |> insert(1, 4) |> insert(1, 3) |> insert(2, 2), singleton(1, 4) |> insert(1, 2))
            |> toAssocList == (1, Set#{3}) :: (2, Set#{2}) :: Nil
    /////////////////////////////////////////////////////////////////////////////
    // toList                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toList01(): Bool = SetMultiMap.toList(empty(): SetMultiMap[Int32, Int32]) == Nil

    @test
    def toList02(): Bool = SetMultiMap.toList(singleton(1, 4)) == (1, 4) ::  Nil

    @test
    def toList03(): Bool = SetMultiMap.toList(singleton(1, 4) |> insert(2, 4)) == (1, 4) :: (2, 4) :: Nil

    @test
    def toList04(): Bool = 
        SetMultiMap.toList(singleton(1, 4) |> insert(2, 2) |> insert(1, 1)) == (1, 1) :: (1, 4) :: (2, 2) :: Nil

    @test
    def toList05(): Bool = 
        SetMultiMap.toList(singleton(1, 4) |> insert(2, 2) |> insert(1, 1)  |> insert(3, 3)) == (1, 1) :: (1, 4) :: (2, 2) :: (3, 3) :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // toAscList                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toAscList01(): Bool = SetMultiMap.toAscList(empty(): SetMultiMap[Int32, Int32]) == Nil

    @test
    def toAscList02(): Bool = SetMultiMap.toAscList(singleton(1, 4)) == (1, 4) ::  Nil

    @test
    def toAscList03(): Bool = SetMultiMap.toAscList(singleton(1, 4) |> insert(2, 4)) == (1, 4) :: (2, 4) :: Nil

    @test
    def toAscList04(): Bool = 
        SetMultiMap.toAscList(singleton(1, 4) |> insert(2, 4) |> insert(1, 1)) == (1, 1) :: (1, 4) :: (2, 4) :: Nil

    @test
    def toAscList05(): Bool = 
        SetMultiMap.toAscList(singleton(1, 4) |> insert(2, 4) |> insert(1, 1)  |> insert(3, 3)) == (1, 1) :: (1, 4) :: (2, 4) :: (3, 3) :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // toDescList                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toDescList01(): Bool = SetMultiMap.toDescList(empty(): SetMultiMap[Int32, Int32]) == Nil

    @test
    def toDescList02(): Bool = SetMultiMap.toDescList(singleton(1, 4)) == (1, 4) ::  Nil

    @test
    def toDescList03(): Bool = SetMultiMap.toDescList(singleton(1, 4) |> insert(2, 4)) == (2, 4) :: (1, 4) :: Nil

    @test
    def toDescList04(): Bool = 
        SetMultiMap.toDescList(singleton(1, 4) |> insert(2, 4) |> insert(1, 1)) == (2, 4) :: (1, 4) :: (1, 1) :: Nil

    @test
    def toDescList05(): Bool = 
        SetMultiMap.toDescList(singleton(1, 4) |> insert(2, 4) |> insert(1, 1)  |> insert(3, 3)) == (3, 3) :: (2, 4) :: (1, 4) :: (1, 1) :: Nil

}

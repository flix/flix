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

mod TestFilterable {

    use Filterable.{filter, filterMap}

    def isOdd(i: Int32): Bool = not (i `Int32.rem` 2 == 0)

    def oddPlus10(i: Int32): Option[Int32] = if (isOdd(i)) Some(i + 10) else None

    /////////////////////////////////////////////////////////////////////////////
    // filterChain                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def filterChain01(): Bool =
        filter(isOdd, Chain.empty(): Chain[Int32]) |> Chain.toList == Nil

    @Test
    def filterChain02(): Bool =
        filter(isOdd, Chain.singleton(1)) |> Chain.toList == 1 :: Nil

    @Test
    def filterChain03(): Bool =
        filter(isOdd, List.toChain(1 :: 2 :: 3 :: 4 :: Nil)) |> Chain.toList == 1 :: 3 :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // filterMapChain                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def filterMapChain01(): Bool =
        filterMap(oddPlus10, Chain.empty(): Chain[Int32]) |> Chain.toList == Nil

    @Test
    def filterMapChain02(): Bool =
        filterMap(oddPlus10, Chain.singleton(1)) |> Chain.toList == 11 :: Nil

    @Test
    def filterMapChain03(): Bool =
        filterMap(oddPlus10, List.toChain(1 :: 2 :: 3 :: 4 :: Nil)) |> Chain.toList == 11 :: 13 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // filterDelayList                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def filterDelayList01(): Bool =
        filter(isOdd, DelayList.empty(): DelayList[Int32]) |> DelayList.toList == Nil

    @Test
    def filterDelayList02(): Bool =
        filter(isOdd, DelayList.range(1, 2)) |> DelayList.toList == 1 :: Nil

    @Test
    def filterDelayList03(): Bool =
        filter(isOdd, DelayList.range(1, 5))  |> DelayList.toList == 1 :: 3 :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // filterMapDelayList                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def filterMapDelayList01(): Bool =
        filterMap(oddPlus10, DelayList.empty(): DelayList[Int32]) |> DelayList.toList == Nil

    @Test
    def filterMapDelayList02(): Bool =
        filterMap(oddPlus10, DelayList.range(1, 2)) |> DelayList.toList == 11 :: Nil

    @Test
    def filterMapDelayList03(): Bool =
        filterMap(oddPlus10, DelayList.range(1, 5)) |> DelayList.toList == 11 :: 13 :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // filterList                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def filterList01(): Bool = filter(isOdd, Nil: List[Int32]) == Nil

    @Test
    def filterList02(): Bool = filter(isOdd, 1 :: Nil) == 1 :: Nil

    @Test
    def filterList03(): Bool = filter(isOdd, 1 :: 2 :: 3 :: 4 :: Nil) == 1 :: 3 :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // filterMapList                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def filterMapList01(): Bool = filterMap(oddPlus10, Nil: List[Int32]) == Nil

    @Test
    def filterMapList02(): Bool = filterMap(oddPlus10, 1 :: Nil) == 11 :: Nil

    @Test
    def filterMapList03(): Bool = filterMap(oddPlus10, 1 :: 2 :: 3 :: 4:: Nil) == 11 :: 13 :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // filterMap                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def filterMap01(): Bool = filter(isOdd, Map.empty(): Map[Int32, Int32]) == Map#{}

    @Test
    def filterMap02(): Bool = filter(isOdd, Map#{1 => 1}) == Map#{1 => 1}

    @Test
    def filterMap03(): Bool = filter(isOdd, Map#{1 => 1, 2 => 2, 3 => 3, 4 => 4}) == Map#{1 => 1, 3 => 3}


    /////////////////////////////////////////////////////////////////////////////
    // filterMapMap                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def filterMapMap01(): Bool = filterMap(oddPlus10, Map.empty(): Map[Int32, Int32]) == Map#{}

    @Test
    def filterMapMap02(): Bool = filterMap(oddPlus10, Map#{1 => 1}) == Map#{1 => 11}

    @Test
    def filterMapMap03(): Bool = filterMap(oddPlus10, Map#{1 => 1, 2 => 2, 3 => 3, 4 => 4}) == Map#{1 => 11, 3 => 13}

    /////////////////////////////////////////////////////////////////////////////
    // filterOption                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def filterOption01(): Bool = filter(isOdd, None: Option[Int32]) == None

    @Test
    def filterOption02(): Bool = filter(isOdd, Some(1)) == Some(1)

    @Test
    def filterOption03(): Bool = filter(isOdd, Some(2)) == None

    /////////////////////////////////////////////////////////////////////////////
    // filterMapOption                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def filterMapOption01(): Bool = filterMap(oddPlus10, None: Option[Int32]) == None

    @Test
    def filterMapOption02(): Bool = filterMap(oddPlus10, Some(1)) == Some(11)

    @Test
    def filterMapOption03(): Bool = filterMap(oddPlus10, Some(2)) == None

    /////////////////////////////////////////////////////////////////////////////
    // filterRedBlackTree                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def filterRedBlackTree01(): Bool =
        filter(isOdd, RedBlackTree.empty(): RedBlackTree[Int32, Int32]) == RedBlackTree.empty()

    @Test
    def filterRedBlackTree02(): Bool =
        filter(isOdd, RedBlackTree.empty() |> RedBlackTree.insert(1, 1))
            |> RedBlackTree.toList == (1, 1) :: Nil

    @Test
    def filterRedBlackTree03(): Bool =
        filter(isOdd, RedBlackTree.empty() |> RedBlackTree.insert(1, 1) |> RedBlackTree.insert(2, 2) |> RedBlackTree.insert(3, 3) |> RedBlackTree.insert(4, 4))
            |> RedBlackTree.toList |> List.sort == (1, 1) :: (3, 3) :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // filterMapRedBlackTree                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def filterMapRedBlackTree01(): Bool =
        filterMap(oddPlus10, RedBlackTree.empty(): RedBlackTree[Int32, Int32]) == RedBlackTree.empty()

    @Test
    def filterMapMapRedBlackTree02(): Bool =
        filterMap(oddPlus10, RedBlackTree.empty() |> RedBlackTree.insert(1, 1))
            |> RedBlackTree.toList |> List.sort == (1, 11) :: Nil

    @Test
    def filterMapRedBlackTree03(): Bool =
        filterMap(oddPlus10, RedBlackTree.empty() |> RedBlackTree.insert(1, 1) |> RedBlackTree.insert(2, 2) |> RedBlackTree.insert(3, 3) |> RedBlackTree.insert(4, 4))
            |> RedBlackTree.toList |> List.sort == (1, 11) :: (3, 13) :: Nil

}


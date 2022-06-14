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

namespace TestFunctorFilter {

    use FunctorFilter.{filter, filterMap};

    def isOdd(i: Int32): Bool = not (i mod 2 == 0)

    def oddPlus10(i: Int32): Option[Int32] = if (isOdd(i)) Some(i + 10) else None

    /////////////////////////////////////////////////////////////////////////////
    // filterChain                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filterChain01(): Bool =
        filter(isOdd, Chain.empty(): Chain[Int32]) |> Chain.toList == Nil

    @test
    def filterChain02(): Bool =
        filter(isOdd, Chain.singleton(1)) |> Chain.toList == 1 :: Nil

    @test
    def filterChain03(): Bool =
        filter(isOdd, List.toChain(1 :: 2 :: 3 :: 4 :: Nil)) |> Chain.toList == 1 :: 3 :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // filterMapChain                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filterMapChain01(): Bool =
        filterMap(oddPlus10, Chain.empty(): Chain[Int32]) |> Chain.toList == Nil

    @test
    def filterMapChain02(): Bool =
        filterMap(oddPlus10, Chain.singleton(1)) |> Chain.toList == 11 :: Nil

    @test
    def filterMapChain03(): Bool =
        filterMap(oddPlus10, List.toChain(1 :: 2 :: 3 :: 4 :: Nil)) |> Chain.toList == 11 :: 13 :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // filterOption                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filterOption01(): Bool = filter(isOdd, None: Option[Int32]) == None

    @test
    def filterOption02(): Bool = filter(isOdd, Some(1)) == None

    @test
    def filterOption03(): Bool = filter(isOdd, Some(2)) == Some(2)

    /////////////////////////////////////////////////////////////////////////////
    // filterMapOption                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filterMapOption01(): Bool = filterMap(oddPlus10, None: Option[Int32]) == None

    @test
    def filterMapOption02(): Bool = filterMap(oddPlus10, Some(1)) == Some(11)

    @test
    def filterMapOption03(): Bool = filterMap(oddPlus10, Some(2)) == None

}


/*
 *  Copyright 2021 Nina Andrup Pedersen
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

namespace TestSemiGroup {

    use SemiGroup.{combine, combineN};

    /////////////////////////////////////////////////////////////////////////////
    // Unit                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def combineUnit01(): Bool = combine((), ()) == ()

    /////////////////////////////////////////////////////////////////////////////
    // String                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def combineString01(): Bool = combine("", "") == ""

    @test
    def combineString02(): Bool = combine("a", "") == "a"

    @test
    def combineString03(): Bool = combine("", "b") == "b"

    @test
    def combineString04(): Bool = combine("a", "b") == "ab"

    /////////////////////////////////////////////////////////////////////////////
    // Option                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def combineOption01(): Bool = combine(None, None) == None

    @test
    def combineOption02(): Bool = combine(Some("a"), None) == Some("a")

    @test
    def combineOption03(): Bool = combine(None, Some("b")) == Some("b")

    @test
    def combineOption04(): Bool = combine(Some("a"), Some("b")) == Some("ab")

    /////////////////////////////////////////////////////////////////////////////
    // List                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def combineList01(): Bool = combine(Nil, Nil) == Nil

    @test
    def combineList02(): Bool = combine(1 :: Nil, Nil) == 1 :: Nil

    @test
    def combineList03(): Bool = combine(Nil, 2 :: Nil) == 2 :: Nil

    @test
    def combineList04(): Bool = combine(1 :: Nil, 2 :: Nil) == 1 :: 2 :: Nil

    @test
    def combineList05(): Bool = combine(1 :: 2 :: Nil, 1 :: 2 :: Nil) == 1 :: 2 :: 1 :: 2 :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // Map                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def combineMap01(): Bool = combine(Map#{}, Map#{}) == Map#{}

    @test
    def combineMap02(): Bool = combine(Map#{1 -> "A"}, Map#{}) == Map#{1 -> "A"}

    @test
    def combineMap03(): Bool = combine(Map#{}, Map#{1 -> "a"}) == Map#{1 -> "a"}

    @test
    def combineMap04(): Bool = combine(Map#{1 -> "A"}, Map#{1 -> "a"}) == Map#{1 -> "Aa"}

    @test
    def combineMap05(): Bool = combine(Map#{1 -> "A", 2 -> "B", 3 -> "C"}, Map#{2 -> "b", 3 -> "c", 4 -> "d"}) == Map#{1 -> "A", 2 -> "Bb", 3 -> "Cc", 4 -> "d"}

}
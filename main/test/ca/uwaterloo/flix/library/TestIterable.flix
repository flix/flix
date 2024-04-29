/*
 * Copyright 2024 Stephen Tetley
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

mod TestIterable {

    /////////////////////////////////////////////////////////////////////////////
    // List instance                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def iterableList01(): Bool = region rc {
        let l : List[Int32] = Nil;
        Iterable.iterator(rc, l) |> Iterator.toList == Nil
    }

    @test
    def iterableList02(): Bool = region rc {
        let l = List#{2, 5, 11, 8};
        Iterable.iterator(rc, l) |> Iterator.toList == List#{2, 5, 11, 8}
    }

    @test
    def iterableList03(): Bool = region rc {
        let l = List#{'A', 'B', 'D', 'C'};
        Iterable.iterator(rc, l) |> Iterator.toList == List#{'A', 'B', 'D', 'C'}
    }

    @test
    def iterableList04(): Bool = region rc {
        let l = List#{(2, 'A'), (5, 'B'), (11, 'D'), (8, 'C')};
        Iterable.iterator(rc, l) |> Iterator.toList == List#{(2, 'A'), (5, 'B'), (11, 'D'), (8, 'C')}
    }

    /////////////////////////////////////////////////////////////////////////////
    // RedBlackTree instance                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def iterableRedBlackTree01(): Bool = region rc {
        let t : RedBlackTree[Int32, Char] = RedBlackTree.empty();
        Iterable.iterator(rc, t) |> Iterator.toList == Nil
    }

    @test
    def iterableRedBlackTree02(): Bool = region rc {
        let Map.Map(t) = Map#{2 => 'A', 5 => 'B', 11 => 'D', 8 => 'C'};
        Iterable.iterator(rc, t) |> Iterator.toList == List#{(2, 'A'), (5, 'B'), (8, 'C'), (11, 'D')}
    }

    /////////////////////////////////////////////////////////////////////////////
    // Map instance                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def iterableMap01(): Bool = region rc {
        let m : Map[Int32, Char] = Map.empty();
        Iterable.iterator(rc, m) |> Iterator.toList == Nil
    }

    @test
    def iterableMap02(): Bool = region rc {
        let m = Map#{2 => 'A', 5 => 'B', 11 => 'D', 8 => 'C'};
        Iterable.iterator(rc, m) |> Iterator.toList == List#{(2, 'A'), (5, 'B'), (8, 'C'), (11, 'D')}
    }

    /////////////////////////////////////////////////////////////////////////////
    // DelayMap instance                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def iterableDelayMap01(): Bool = region rc {
        let m : DelayMap[Int32, Char] = DelayMap.empty();
        Iterable.iterator(rc, m) |> Iterator.toList == Nil
    }

    @test
    def iterableDelayMap02(): Bool = region rc {
        let m = Map#{2 => 'A', 5 => 'B', 11 => 'D', 8 => 'C'} |> Map.toDelayMap;
        Iterable.iterator(rc, m) |> Iterator.toList == List#{(2, 'A'), (5, 'B'), (8, 'C'), (11, 'D')}
    }

}

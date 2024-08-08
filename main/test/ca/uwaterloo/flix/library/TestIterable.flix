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
    // Array instance                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def iterableArray01(): Bool = region rc {
        let a : Array[Int32, rc] = Array#{} @ rc;
        Iterable.iterator(rc, a) |> Iterator.toList == Nil
    }

    @test
    def iterableArray02(): Bool = region rc {
        let a = Array#{2, 5, 11, 8} @ rc;
        Iterable.iterator(rc, a) |> Iterator.toList == List#{2, 5, 11, 8}
    }

    @test
    def iterableArray03(): Bool = region rc {
        let a = Array#{'A', 'B', 'D', 'C'} @ rc;
        Iterable.iterator(rc, a) |> Iterator.toList == List#{'A', 'B', 'D', 'C'}
    }

    @test
    def iterableArray04(): Bool = region rc {
        let a = Array#{(2, 'A'), (5, 'B'), (11, 'D'), (8, 'C')} @ rc;
        Iterable.iterator(rc, a) |> Iterator.toList == List#{(2, 'A'), (5, 'B'), (11, 'D'), (8, 'C')}
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
    // MutDeque instance                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def iterableMutDeque01(): Bool = region rc {
        let d : MutDeque[Int32, rc] = MutDeque.empty(rc);
        Iterable.iterator(rc, d) |> Iterator.toList == Nil
    }

    @test
    def iterableMutDeque02(): Bool = region rc {
        let d = List#{2, 5, 11, 8} |> List.toMutDeque(rc);
        Iterable.iterator(rc, d) |> Iterator.toList == List#{2, 5, 11, 8}
    }

    @test
    def iterableMutDeque03(): Bool = region rc {
        let d = List#{'A', 'B', 'D', 'C'} |> List.toMutDeque(rc);
        Iterable.iterator(rc, d) |> Iterator.toList == List#{'A', 'B', 'D', 'C'}
    }

    @test
    def iterableMutDeque04(): Bool = region rc {
        let d = List#{(2, 'A'), (5, 'B'), (11, 'D'), (8, 'C')} |> List.toMutDeque(rc);
        Iterable.iterator(rc, d) |> Iterator.toList == List#{(2, 'A'), (5, 'B'), (11, 'D'), (8, 'C')}
    }

    /////////////////////////////////////////////////////////////////////////////
    // MutList instance                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def iterableMutList01(): Bool = region rc {
        let l : MutList[Int32, rc] = MutList.empty(rc);
        Iterable.iterator(rc, l) |> Iterator.toList == Nil
    }

    @test
    def iterableMutList02(): Bool = region rc {
        let l = List#{2, 5, 11, 8} |> List.toMutList(rc);
        Iterable.iterator(rc, l) |> Iterator.toList == List#{2, 5, 11, 8}
    }

    @test
    def iterableMutList03(): Bool = region rc {
        let l = List#{'A', 'B', 'D', 'C'} |> List.toMutList(rc);
        Iterable.iterator(rc, l) |> Iterator.toList == List#{'A', 'B', 'D', 'C'}
    }

    @test
    def iterableMutList04(): Bool = region rc {
        let l = List#{(2, 'A'), (5, 'B'), (11, 'D'), (8, 'C')} |> List.toMutList(rc);
        Iterable.iterator(rc, l) |> Iterator.toList == List#{(2, 'A'), (5, 'B'), (11, 'D'), (8, 'C')}
    }

    /////////////////////////////////////////////////////////////////////////////
    // MutMap instance                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def iterableMutMap01(): Bool = region rc {
        let m : MutMap[Int32, Char, rc] = MutMap.empty(rc);
        Iterable.iterator(rc, m) |> Iterator.toList == Nil
    }

    @test
    def iterableMutMap02(): Bool = region rc {
        let m = Map#{2 => 'A', 5 => 'B', 11 => 'D', 8 => 'C'} |> Map.toMutMap(rc);
        Iterable.iterator(rc, m) |> Iterator.toList == List#{(2, 'A'), (5, 'B'), (8, 'C'), (11, 'D')}
    }


    /////////////////////////////////////////////////////////////////////////////
    // MutQueue instance                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def iterableMutQueue01(): Bool = region rc {
        let mq : MutQueue[Int32, rc] = MutQueue.empty(rc);
        Iterable.iterator(rc, mq) |> Iterator.toList == Nil
    }

    @test
    def iterableMutQueue02(): Bool = region rc {
        let mq = MutQueue.empty(rc);
        MutQueue.enqueueAll(mq, List#{2, 5, 11, 8});
        Iterable.iterator(rc, mq) |> Iterator.toList == List#{11, 8, 5, 2}
    }

    @test
    def iterableMutQueue03(): Bool = region rc {
        let mq = MutQueue.empty(rc);
        MutQueue.enqueueAll(mq, List#{'A', 'B', 'D', 'C'});
        Iterable.iterator(rc, mq) |> Iterator.toList == List#{'D', 'C', 'B', 'A'}
    }

    @test
    def iterableMutQueue04(): Bool = region rc {
        let mq = MutQueue.empty(rc);
        MutQueue.enqueueAll(mq, List#{(2, 'A'), (5, 'B'), (11, 'D'), (8, 'C')});
        Iterable.iterator(rc, mq) |> Iterator.toList == List#{(11, 'D'), (8, 'C'), (5, 'B'), (2, 'A')}
    }

    /////////////////////////////////////////////////////////////////////////////
    // MutSet instance                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def iterableMutSet01(): Bool = region rc {
        let s : MutSet[Int32, rc] = MutSet.empty(rc);
        Iterable.iterator(rc, s) |> Iterator.toSet == Set.empty()
    }

    @test
    def iterableMutSet02(): Bool = region rc {
        let s = Set#{2, 5, 11, 8} |> Set.toMutSet(rc);
        Iterable.iterator(rc, s) |> Iterator.toSet == Set#{2, 5, 8, 11}
    }

    @test
    def iterableMutSet03(): Bool = region rc {
        let s = Set#{'A', 'B', 'D', 'C'} |> Set.toMutSet(rc);
        Iterable.iterator(rc, s) |> Iterator.toSet == Set#{'A', 'B', 'C', 'D'}
    }

    @test
    def iterableMutSet04(): Bool = region rc {
        let s = Set#{(2, 'A'), (5, 'B'), (11, 'D'), (8, 'C')} |> Set.toMutSet(rc);
        Iterable.iterator(rc, s) |> Iterator.toSet == Set#{(2, 'A'), (5, 'B'), (8, 'C'), (11, 'D')}
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
    // Set instance                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def iterableSet01(): Bool = region rc {
        let s : Set[Int32] = Set.empty();
        Iterable.iterator(rc, s) |> Iterator.toSet == Set.empty()
    }

    @test
    def iterableSet02(): Bool = region rc {
        let s = Set#{2, 5, 11, 8};
        Iterable.iterator(rc, s) |> Iterator.toSet == Set#{2, 5, 8, 11}
    }

    @test
    def iterableSet03(): Bool = region rc {
        let s = Set#{'A', 'B', 'D', 'C'};
        Iterable.iterator(rc, s) |> Iterator.toSet == Set#{'A', 'B', 'C', 'D'}
    }

    @test
    def iterableSet04(): Bool = region rc {
        let s = Set#{(2, 'A'), (5, 'B'), (11, 'D'), (8, 'C')};
        Iterable.iterator(rc, s) |> Iterator.toSet == Set#{(2, 'A'), (5, 'B'), (8, 'C'), (11, 'D')}
    }

}

mod TestIterable {

    use Assert.{assertEq, assertTrue};

    /////////////////////////////////////////////////////////////////////////////
    // Array instance                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def iterableArray01(): Unit \ Assert = region rc {
        let a : Array[Int32, rc] = Array#{} @ rc;
        assertEq(expected = Nil, Iterable.iterator(rc, a) |> Iterator.toList)
    }

    @Test
    def iterableArray02(): Unit \ Assert = region rc {
        let a = Array#{2, 5, 11, 8} @ rc;
        assertEq(expected = List#{2, 5, 11, 8}, Iterable.iterator(rc, a) |> Iterator.toList)
    }

    @Test
    def iterableArray03(): Unit \ Assert = region rc {
        let a = Array#{'A', 'B', 'D', 'C'} @ rc;
        assertEq(expected = List#{'A', 'B', 'D', 'C'}, Iterable.iterator(rc, a) |> Iterator.toList)
    }

    @Test
    def iterableArray04(): Unit \ Assert = region rc {
        let a = Array#{(2, 'A'), (5, 'B'), (11, 'D'), (8, 'C')} @ rc;
        assertEq(expected = List#{(2, 'A'), (5, 'B'), (11, 'D'), (8, 'C')}, Iterable.iterator(rc, a) |> Iterator.toList)
    }


    /////////////////////////////////////////////////////////////////////////////
    // DelayMap instance                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def iterableDelayMap01(): Unit \ Assert = region rc {
        let m : DelayMap[Int32, Char] = DelayMap.empty();
        assertEq(expected = Nil, Iterable.iterator(rc, m) |> Iterator.toList)
    }

    @Test
    def iterableDelayMap02(): Unit \ Assert = region rc {
        let m = Map#{2 => 'A', 5 => 'B', 11 => 'D', 8 => 'C'} |> Map.toDelayMap;
        assertEq(expected = List#{(2, 'A'), (5, 'B'), (8, 'C'), (11, 'D')}, Iterable.iterator(rc, m) |> Iterator.toList)
    }


    /////////////////////////////////////////////////////////////////////////////
    // List instance                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def iterableList01(): Unit \ Assert = region rc {
        let l : List[Int32] = Nil;
        assertEq(expected = Nil, Iterable.iterator(rc, l) |> Iterator.toList)
    }

    @Test
    def iterableList02(): Unit \ Assert = region rc {
        let l = List#{2, 5, 11, 8};
        assertEq(expected = List#{2, 5, 11, 8}, Iterable.iterator(rc, l) |> Iterator.toList)
    }

    @Test
    def iterableList03(): Unit \ Assert = region rc {
        let l = List#{'A', 'B', 'D', 'C'};
        assertEq(expected = List#{'A', 'B', 'D', 'C'}, Iterable.iterator(rc, l) |> Iterator.toList)
    }

    @Test
    def iterableList04(): Unit \ Assert = region rc {
        let l = List#{(2, 'A'), (5, 'B'), (11, 'D'), (8, 'C')};
        assertEq(expected = List#{(2, 'A'), (5, 'B'), (11, 'D'), (8, 'C')}, Iterable.iterator(rc, l) |> Iterator.toList)
    }


    /////////////////////////////////////////////////////////////////////////////
    // Map instance                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def iterableMap01(): Unit \ Assert = region rc {
        let m : Map[Int32, Char] = Map.empty();
        assertEq(expected = Nil, Iterable.iterator(rc, m) |> Iterator.toList)
    }

    @Test
    def iterableMap02(): Unit \ Assert = region rc {
        let m = Map#{2 => 'A', 5 => 'B', 11 => 'D', 8 => 'C'};
        assertEq(expected = List#{(2, 'A'), (5, 'B'), (8, 'C'), (11, 'D')}, Iterable.iterator(rc, m) |> Iterator.toList)
    }


    /////////////////////////////////////////////////////////////////////////////
    // MutDeque instance                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def iterableMutDeque01(): Unit \ Assert = region rc {
        let d : MutDeque[Int32, rc] = MutDeque.empty(rc);
        assertEq(expected = Nil, Iterable.iterator(rc, d) |> Iterator.toList)
    }

    @Test
    def iterableMutDeque02(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        List.forEach(x -> MutDeque.pushBack(x, d), List#{2, 5, 11, 8});
        assertEq(expected = List#{2, 5, 11, 8}, Iterable.iterator(rc, d) |> Iterator.toList)
    }

    @Test
    def iterableMutDeque03(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        List.forEach(x -> MutDeque.pushBack(x, d), List#{'A', 'B', 'D', 'C'});
        assertEq(expected = List#{'A', 'B', 'D', 'C'}, Iterable.iterator(rc, d) |> Iterator.toList)
    }

    @Test
    def iterableMutDeque04(): Unit \ Assert = region rc {
        let d = MutDeque.empty(rc);
        List.forEach(x -> MutDeque.pushBack(x, d), List#{(2, 'A'), (5, 'B'), (11, 'D'), (8, 'C')});
        assertEq(expected = List#{(2, 'A'), (5, 'B'), (11, 'D'), (8, 'C')}, Iterable.iterator(rc, d) |> Iterator.toList)
    }


    /////////////////////////////////////////////////////////////////////////////
    // MutList instance                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def iterableMutList01(): Unit \ Assert = region rc {
        let l : MutList[Int32, rc] = MutList.empty(rc);
        assertEq(expected = Nil, Iterable.iterator(rc, l) |> Iterator.toList)
    }

    @Test
    def iterableMutList02(): Unit \ Assert = region rc {
        let l = MutList.empty(rc);
        List.forEach(x -> MutList.push(x, l), List#{2, 5, 11, 8});
        assertEq(expected = List#{2, 5, 11, 8}, Iterable.iterator(rc, l) |> Iterator.toList)
    }

    @Test
    def iterableMutList03(): Unit \ Assert = region rc {
        let l = MutList.empty(rc);
        List.forEach(x -> MutList.push(x, l), List#{'A', 'B', 'D', 'C'});
        assertEq(expected = List#{'A', 'B', 'D', 'C'}, Iterable.iterator(rc, l) |> Iterator.toList)
    }

    @Test
    def iterableMutList04(): Unit \ Assert = region rc {
        let l = MutList.empty(rc);
        List.forEach(x -> MutList.push(x, l), List#{(2, 'A'), (5, 'B'), (11, 'D'), (8, 'C')});
        assertEq(expected = List#{(2, 'A'), (5, 'B'), (11, 'D'), (8, 'C')}, Iterable.iterator(rc, l) |> Iterator.toList)
    }

    @Test
    def iterableMutList05(): Unit \ Assert = region rc {
        let l = MutList.empty(rc);
        List.forEach(x -> MutList.push(x, l), List#{(2, 'A'), (5, 'B'), (11, 'D'), (8, 'C')});
        assertEq(expected = List#{(2, 'A'), (5, 'B'), (11, 'D'), (8, 'C')}, Iterable.iterator(rc, l) |> Iterator.toList)
    }


    /////////////////////////////////////////////////////////////////////////////
    // MutMap instance                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def iterableMutMap01(): Unit \ Assert = region rc {
        let m : MutMap[Int32, Char, rc] = MutMap.empty(rc);
        assertEq(expected = Nil, Iterable.iterator(rc, m) |> Iterator.toList)
    }

    @Test
    def iterableMutMap02(): Unit \ Assert = region rc {
        let m = Map#{2 => 'A', 5 => 'B', 11 => 'D', 8 => 'C'} |> Map.toMutMap(rc);
        assertEq(expected = List#{(2, 'A'), (5, 'B'), (8, 'C'), (11, 'D')}, Iterable.iterator(rc, m) |> Iterator.toList)
    }


    /////////////////////////////////////////////////////////////////////////////
    // MutPriorityQueue instance                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def iterableMutPriorityQueue01(): Unit \ Assert = region rc {
        let mq : MutPriorityQueue[Int32, rc] = MutPriorityQueue.empty(rc);
        assertEq(expected = Nil, Iterable.iterator(rc, mq) |> Iterator.toList)
    }

    @Test
    def iterableMutPriorityQueue02(): Unit \ Assert = region rc {
        let mq = MutPriorityQueue.empty(rc);
        MutPriorityQueue.enqueueAll(List#{2, 5, 11, 8}, mq);
        assertEq(expected = List#{11, 8, 5, 2}, Iterable.iterator(rc, mq) |> Iterator.toList)
    }

    @Test
    def iterableMutPriorityQueue03(): Unit \ Assert = region rc {
        let mq = MutPriorityQueue.empty(rc);
        MutPriorityQueue.enqueueAll(List#{'A', 'B', 'D', 'C'}, mq);
        assertEq(expected = List#{'D', 'C', 'B', 'A'}, Iterable.iterator(rc, mq) |> Iterator.toList)
    }


    /////////////////////////////////////////////////////////////////////////////
    // MutSet instance                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def iterableMutSet01(): Unit \ Assert = region rc {
        let s : MutSet[Int32, rc] = MutSet.empty(rc);
        assertEq(expected = Set.empty(), Iterable.iterator(rc, s) |> Iterator.toSet)
    }

    @Test
    def iterableMutSet02(): Unit \ Assert = region rc {
        let s = Set#{2, 5, 11, 8} |> Set.toMutSet(rc);
        assertEq(expected = Set#{2, 5, 8, 11}, Iterable.iterator(rc, s) |> Iterator.toSet)
    }

    @Test
    def iterableMutSet03(): Unit \ Assert = region rc {
        let s = Set#{'A', 'B', 'D', 'C'} |> Set.toMutSet(rc);
        assertEq(expected = Set#{'A', 'B', 'C', 'D'}, Iterable.iterator(rc, s) |> Iterator.toSet)
    }

    @Test
    def iterableMutSet04(): Unit \ Assert = region rc {
        let s = Set#{(2, 'A'), (5, 'B'), (11, 'D'), (8, 'C')} |> Set.toMutSet(rc);
        assertEq(expected = Set#{(2, 'A'), (5, 'B'), (8, 'C'), (11, 'D')}, Iterable.iterator(rc, s) |> Iterator.toSet)
    }


    /////////////////////////////////////////////////////////////////////////////
    // RedBlackTree instance                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def iterableRedBlackTree01(): Unit \ Assert = region rc {
        let t : RedBlackTree[Int32, Char] = RedBlackTree.empty();
        assertEq(expected = Nil, Iterable.iterator(rc, t) |> Iterator.toList)
    }

    @Test
    def iterableRedBlackTree02(): Unit \ Assert = region rc {
        let Map.Map(t) = Map#{2 => 'A', 5 => 'B', 11 => 'D', 8 => 'C'};
        assertEq(expected = List#{(2, 'A'), (5, 'B'), (8, 'C'), (11, 'D')}, Iterable.iterator(rc, t) |> Iterator.toList)
    }


    /////////////////////////////////////////////////////////////////////////////
    // Set instance                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def iterableSet01(): Unit \ Assert = region rc {
        let s : Set[Int32] = Set.empty();
        assertEq(expected = Set.empty(), Iterable.iterator(rc, s) |> Iterator.toSet)
    }

    @Test
    def iterableSet02(): Unit \ Assert = region rc {
        let s = Set#{2, 5, 11, 8};
        assertEq(expected = Set#{2, 5, 8, 11}, Iterable.iterator(rc, s) |> Iterator.toSet)
    }

    @Test
    def iterableSet03(): Unit \ Assert = region rc {
        let s = Set#{'A', 'B', 'D', 'C'};
        assertEq(expected = Set#{'A', 'B', 'C', 'D'}, Iterable.iterator(rc, s) |> Iterator.toSet)
    }

    @Test
    def iterableSet04(): Unit \ Assert = region rc {
        let s = Set#{(2, 'A'), (5, 'B'), (11, 'D'), (8, 'C')};
        assertEq(expected = Set#{(2, 'A'), (5, 'B'), (8, 'C'), (11, 'D')}, Iterable.iterator(rc, s) |> Iterator.toSet)
    }

}

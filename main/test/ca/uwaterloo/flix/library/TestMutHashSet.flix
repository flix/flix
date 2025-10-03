mod TestMutHashSet {

    use Assert.{assertEq, assertNeq, assertTrue, assertFalse, fail}

    /////////////////////////////////////////////////////////////////////////////
    // empty                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def empty01(): Unit \ Assert = region rc {
        let s: MutHashSet[Int32, rc] = MutHashSet.empty(rc);
        assertTrue(MutHashSet.isEmpty(s))
    }

    @Test
    def empty02(): Unit \ Assert = region rc {
        let s: MutHashSet[Int32, rc] = MutHashSet.empty(rc);
        assertEq(expected = 0, MutHashSet.size(s))
    }

    @Test
    def empty03(): Unit \ Assert = region rc {
        let s: MutHashSet[Int32, _] = MutHashSet.empty(rc);
        assertFalse(MutHashSet.memberOf(1, s))
    }

    /////////////////////////////////////////////////////////////////////////////
    // emptyWithCapacity                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def emptyWithCapacity01(): Unit \ Assert = region rc {
        let s: MutHashSet[Int32, rc] = MutHashSet.emptyWithCapacity(rc, 32);
        assertTrue(MutHashSet.isEmpty(s))
    }

    @Test
    def emptyWithCapacity02(): Unit \ Assert = region rc {
        let s: MutHashSet[Int32, rc] = MutHashSet.emptyWithCapacity(rc, -1);
        assertTrue(MutHashSet.isEmpty(s))
    }

    @Test
    def emptyWithCapacity03(): Unit \ Assert = region rc {
        let s: MutHashSet[Int32, rc] = MutHashSet.emptyWithCapacity(rc, 0);
        MutHashSet.add(1, s);
        MutHashSet.add(2, s);
        assertEq(expected = 2, MutHashSet.size(s))
    }

    /////////////////////////////////////////////////////////////////////////////
    // singleton                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def singleton01(): Unit \ Assert = region rc {
        let s = MutHashSet.singleton(rc, 1);
        assertEq(expected = 1, MutHashSet.size(s))
    }

    @Test
    def singleton02(): Unit \ Assert = region rc {
        let s = MutHashSet.singleton(rc, 1);
        assertTrue(MutHashSet.memberOf(1, s))
    }

    @Test
    def singleton03(): Unit \ Assert = region rc {
        let s = MutHashSet.singleton(rc, 42);
        assertFalse(MutHashSet.isEmpty(s));
        assertTrue(MutHashSet.nonEmpty(s));
        assertTrue(MutHashSet.memberOf(42, s));
        assertFalse(MutHashSet.memberOf(41, s))
    }

    /////////////////////////////////////////////////////////////////////////////
    // size                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def size01(): Unit \ Assert = region rc {
        let s: MutHashSet[Int32, rc] = MutHashSet.empty(rc);
        assertEq(expected = 0, MutHashSet.size(s))
    }

    @Test
    def size02(): Unit \ Assert = region rc {
        let s = MutHashSet.empty(rc);
        MutHashSet.add(1, s);
        assertEq(expected = 1, MutHashSet.size(s))
    }

    @Test
    def size03(): Unit \ Assert = region rc {
        let s = MutHashSet.empty(rc);
        MutHashSet.add(1, s);
        MutHashSet.add(2, s);
        MutHashSet.add(3, s);
        assertEq(expected = 3, MutHashSet.size(s))
    }

    @Test
    def size04(): Unit \ Assert = region rc {
        let s = MutHashSet.empty(rc);
        MutHashSet.add(1, s);
        MutHashSet.add(1, s);
        assertEq(expected = 1, MutHashSet.size(s))
    }

    @Test
    def size05(): Unit \ Assert = region rc {
        let s = MutHashSet.empty(rc);
        MutHashSet.add(1, s);
        MutHashSet.add(2, s);
        MutHashSet.remove(1, s);
        assertEq(expected = 1, MutHashSet.size(s))
    }

    /////////////////////////////////////////////////////////////////////////////
    // isEmpty                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isEmpty01(): Unit \ Assert = region rc {
        let s: MutHashSet[Int32, rc] = MutHashSet.empty(rc);
        assertTrue(MutHashSet.isEmpty(s))
    }

    @Test
    def isEmpty02(): Unit \ Assert = region rc {
        let s = MutHashSet.empty(rc);
        MutHashSet.add(1, s);
        assertFalse(MutHashSet.isEmpty(s))
    }

    @Test
    def isEmpty03(): Unit \ Assert = region rc {
        let s = MutHashSet.empty(rc);
        MutHashSet.add(1, s);
        MutHashSet.remove(1, s);
        assertTrue(MutHashSet.isEmpty(s))
    }

    /////////////////////////////////////////////////////////////////////////////
    // nonEmpty                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def nonEmpty01(): Unit \ Assert = region rc {
        let s: MutHashSet[Int32, rc] = MutHashSet.empty(rc);
        assertFalse(MutHashSet.nonEmpty(s))
    }

    @Test
    def nonEmpty02(): Unit \ Assert = region rc {
        let s = MutHashSet.empty(rc);
        MutHashSet.add(1, s);
        assertTrue(MutHashSet.nonEmpty(s))
    }

    @Test
    def nonEmpty03(): Unit \ Assert = region rc {
        let s = MutHashSet.empty(rc);
        MutHashSet.add(1, s);
        MutHashSet.remove(1, s);
        assertFalse(MutHashSet.nonEmpty(s))
    }

    /////////////////////////////////////////////////////////////////////////////
    // add                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def add01(): Unit \ Assert = region rc {
        let s = MutHashSet.empty(rc);
        MutHashSet.add(1, s);
        assertTrue(MutHashSet.memberOf(1, s))
    }

    @Test
    def add02(): Unit \ Assert = region rc {
        let s = MutHashSet.empty(rc);
        MutHashSet.add(1, s);
        MutHashSet.add(2, s);
        assertTrue(MutHashSet.memberOf(1, s));
        assertTrue(MutHashSet.memberOf(2, s))
    }

    @Test
    def add03(): Unit \ Assert = region rc {
        let s = MutHashSet.empty(rc);
        MutHashSet.add(1, s);
        MutHashSet.add(1, s);
        assertEq(expected = 1, MutHashSet.size(s))
    }

    @Test
    def add04(): Unit \ Assert = region rc {
        let s = MutHashSet.empty(rc);
        MutHashSet.add(1, s);
        MutHashSet.add(2, s);
        MutHashSet.add(3, s);
        MutHashSet.add(4, s);
        MutHashSet.add(5, s);
        assertEq(expected = 5, MutHashSet.size(s))
    }

    /////////////////////////////////////////////////////////////////////////////
    // addAll                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def addAll01(): Unit \ Assert = region rc {
        let s: MutHashSet[Int32, _] = MutHashSet.empty(rc);
        MutHashSet.addAll((Nil: List[Int32]), s);
        assertEq(expected = 0, MutHashSet.size(s))
    }

    @Test
    def addAll02(): Unit \ Assert = region rc {
        let s = MutHashSet.empty(rc);
        MutHashSet.addAll(1 :: 2 :: 3 :: Nil, s);
        assertEq(expected = 3, MutHashSet.size(s));
        assertTrue(MutHashSet.memberOf(1, s));
        assertTrue(MutHashSet.memberOf(2, s));
        assertTrue(MutHashSet.memberOf(3, s))
    }

    @Test
    def addAll03(): Unit \ Assert = region rc {
        let s = MutHashSet.empty(rc);
        MutHashSet.add(1, s);
        MutHashSet.addAll(2 :: 3 :: Nil, s);
        assertEq(expected = 3, MutHashSet.size(s))
    }

    /////////////////////////////////////////////////////////////////////////////
    // memberOf                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def memberOf01(): Unit \ Assert = region rc {
        let s: MutHashSet[Int32, rc] = MutHashSet.empty(rc);
        assertFalse(MutHashSet.memberOf(1, s))
    }

    @Test
    def memberOf02(): Unit \ Assert = region rc {
        let s = MutHashSet.empty(rc);
        MutHashSet.add(1, s);
        assertTrue(MutHashSet.memberOf(1, s))
    }

    @Test
    def memberOf03(): Unit \ Assert = region rc {
        let s = MutHashSet.empty(rc);
        MutHashSet.add(1, s);
        assertFalse(MutHashSet.memberOf(2, s))
    }

    @Test
    def memberOf04(): Unit \ Assert = region rc {
        let s = MutHashSet.empty(rc);
        MutHashSet.add(1, s);
        MutHashSet.add(2, s);
        MutHashSet.add(3, s);
        assertTrue(MutHashSet.memberOf(2, s));
        assertFalse(MutHashSet.memberOf(4, s))
    }

    /////////////////////////////////////////////////////////////////////////////
    // remove                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def remove01(): Unit \ Assert = region rc {
        let s: MutHashSet[Int32, rc] = MutHashSet.empty(rc);
        MutHashSet.remove(1, s);
        assertEq(expected = 0, MutHashSet.size(s))
    }

    @Test
    def remove02(): Unit \ Assert = region rc {
        let s = MutHashSet.empty(rc);
        MutHashSet.add(1, s);
        MutHashSet.remove(1, s);
        assertEq(expected = 0, MutHashSet.size(s))
    }

    @Test
    def remove03(): Unit \ Assert = region rc {
        let s = MutHashSet.empty(rc);
        MutHashSet.add(1, s);
        MutHashSet.add(2, s);
        MutHashSet.remove(1, s);
        assertEq(expected = 1, MutHashSet.size(s));
        assertFalse(MutHashSet.memberOf(1, s));
        assertTrue(MutHashSet.memberOf(2, s))
    }

    @Test
    def remove04(): Unit \ Assert = region rc {
        let s = MutHashSet.empty(rc);
        MutHashSet.add(1, s);
        MutHashSet.add(2, s);
        MutHashSet.add(3, s);
        MutHashSet.remove(2, s);
        assertEq(expected = 2, MutHashSet.size(s));
        assertTrue(MutHashSet.memberOf(1, s));
        assertFalse(MutHashSet.memberOf(2, s));
        assertTrue(MutHashSet.memberOf(3, s))
    }

    /////////////////////////////////////////////////////////////////////////////
    // clear                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def clear01(): Unit \ Assert = region rc {
        let s: MutHashSet[Int32, rc] = MutHashSet.empty(rc);
        MutHashSet.clear(s);
        assertTrue(MutHashSet.isEmpty(s))
    }

    @Test
    def clear02(): Unit \ Assert = region rc {
        let s = MutHashSet.empty(rc);
        MutHashSet.add(1, s);
        MutHashSet.clear(s);
        assertTrue(MutHashSet.isEmpty(s))
    }

    @Test
    def clear03(): Unit \ Assert = region rc {
        let s = MutHashSet.empty(rc);
        MutHashSet.add(1, s);
        MutHashSet.add(2, s);
        MutHashSet.add(3, s);
        MutHashSet.clear(s);
        assertEq(expected = 0, MutHashSet.size(s));
        assertFalse(MutHashSet.memberOf(1, s));
        assertFalse(MutHashSet.memberOf(2, s));
        assertFalse(MutHashSet.memberOf(3, s))
    }

    /////////////////////////////////////////////////////////////////////////////
    // Insertion order preservation                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def insertionOrder01(): Unit \ Assert = region rc {
        let s = MutHashSet.empty(rc);
        MutHashSet.add(5, s);
        MutHashSet.add(3, s);
        MutHashSet.add(7, s);
        MutHashSet.add(1, s);
        let list = MutHashSet.toList(s);
        assertEq(expected = 5 :: 3 :: 7 :: 1 :: Nil, list)
    }

    @Test
    def insertionOrder02(): Unit \ Assert = region rc {
        let s = MutHashSet.empty(rc);
        MutHashSet.add(1, s);
        MutHashSet.add(2, s);
        MutHashSet.add(3, s);
        MutHashSet.add(2, s);  // Re-add existing element
        let list = MutHashSet.toList(s);
        // Order should remain 1, 2, 3
        assertEq(expected = 1 :: 2 :: 3 :: Nil, list)
    }

    @Test
    def insertionOrder03(): Unit \ Assert = region rc {
        let s = MutHashSet.empty(rc);
        MutHashSet.add(1, s);
        MutHashSet.add(2, s);
        MutHashSet.add(3, s);
        MutHashSet.remove(2, s);  // Remove middle element
        let list = MutHashSet.toList(s);
        assertEq(expected = 1 :: 3 :: Nil, list)
    }

    /////////////////////////////////////////////////////////////////////////////
    // toString                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toString01(): Unit \ Assert = region rc {
        let s: MutHashSet[Int32, _] = MutHashSet.empty(rc);
        assertEq(expected = "MutHashSet#{}", MutHashSet.toString(s))
    }

    @Test
    def toString02(): Unit \ Assert = region rc {
        let s = MutHashSet.empty(rc);
        MutHashSet.add(1, s);
        assertEq(expected = "MutHashSet#{1}", MutHashSet.toString(s))
    }

    @Test
    def toString03(): Unit \ Assert = region rc {
        let s = MutHashSet.empty(rc);
        MutHashSet.add(1, s);
        MutHashSet.add(2, s);
        MutHashSet.add(3, s);
        assertEq(expected = "MutHashSet#{1, 2, 3}", MutHashSet.toString(s))
    }

    /////////////////////////////////////////////////////////////////////////////
    // removeAll                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def removeAll01(): Unit \ Assert = region rc {
        let s = MutHashSet.empty(rc);
        MutHashSet.addAll(1 :: 2 :: 3 :: Nil, s);
        MutHashSet.removeAll((Nil: List[Int32]), s);
        assertEq(expected = 3, MutHashSet.size(s))
    }

    @Test
    def removeAll02(): Unit \ Assert = region rc {
        let s = MutHashSet.empty(rc);
        MutHashSet.addAll(1 :: 2 :: 3 :: Nil, s);
        MutHashSet.removeAll(2 :: 3 :: Nil, s);
        assertEq(expected = 1, MutHashSet.size(s));
        assertTrue(MutHashSet.memberOf(1, s));
        assertFalse(MutHashSet.memberOf(2, s));
        assertFalse(MutHashSet.memberOf(3, s))
    }

    @Test
    def removeAll03(): Unit \ Assert = region rc {
        let s = MutHashSet.empty(rc);
        MutHashSet.addAll(1 :: 2 :: 3 :: Nil, s);
        MutHashSet.removeAll(1 :: 2 :: 3 :: Nil, s);
        assertTrue(MutHashSet.isEmpty(s))
    }

    /////////////////////////////////////////////////////////////////////////////
    // retainAll                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def retainAll01(): Unit \ Assert = region rc {
        let s = MutHashSet.empty(rc);
        MutHashSet.addAll(1 :: 2 :: 3 :: Nil, s);
        MutHashSet.retainAll(1 :: 2 :: 3 :: Nil, s);
        assertEq(expected = 3, MutHashSet.size(s))
    }

    @Test
    def retainAll02(): Unit \ Assert = region rc {
        let s = MutHashSet.empty(rc);
        MutHashSet.addAll(1 :: 2 :: 3 :: 4 :: Nil, s);
        MutHashSet.retainAll(2 :: 3 :: Nil, s);
        assertEq(expected = 2, MutHashSet.size(s));
        assertFalse(MutHashSet.memberOf(1, s));
        assertTrue(MutHashSet.memberOf(2, s));
        assertTrue(MutHashSet.memberOf(3, s));
        assertFalse(MutHashSet.memberOf(4, s))
    }

    @Test
    def retainAll03(): Unit \ Assert = region rc {
        let s = MutHashSet.empty(rc);
        MutHashSet.addAll(1 :: 2 :: 3 :: Nil, s);
        MutHashSet.retainAll((Nil: List[Int32]), s);
        assertTrue(MutHashSet.isEmpty(s))
    }

    /////////////////////////////////////////////////////////////////////////////
    // refine                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def refine01(): Unit \ Assert = region rc {
        let s: MutHashSet[Int32, rc] = MutHashSet.empty(rc);
        MutHashSet.refine(x -> x > 0, s);
        assertTrue(MutHashSet.isEmpty(s))
    }

    @Test
    def refine02(): Unit \ Assert = region rc {
        let s = MutHashSet.empty(rc);
        MutHashSet.addAll(1 :: 2 :: 3 :: 4 :: 5 :: Nil, s);
        MutHashSet.refine(x -> Int32.remainder(x, 2) == 0, s);
        assertEq(expected = 2, MutHashSet.size(s));
        assertTrue(MutHashSet.memberOf(2, s));
        assertTrue(MutHashSet.memberOf(4, s))
    }

    @Test
    def refine03(): Unit \ Assert = region rc {
        let s = MutHashSet.empty(rc);
        MutHashSet.addAll(1 :: 2 :: 3 :: Nil, s);
        MutHashSet.refine(_ -> true, s);
        assertEq(expected = 3, MutHashSet.size(s))
    }

    /////////////////////////////////////////////////////////////////////////////
    // replace                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def replace01(): Unit \ Assert = region rc {
        let s = MutHashSet.empty(rc);
        MutHashSet.addAll(1 :: 2 :: 3 :: Nil, s);
        MutHashSet.replace(src = 2, dst = 42, s);
        assertEq(expected = 3, MutHashSet.size(s));
        assertTrue(MutHashSet.memberOf(1, s));
        assertFalse(MutHashSet.memberOf(2, s));
        assertTrue(MutHashSet.memberOf(42, s));
        assertTrue(MutHashSet.memberOf(3, s))
    }

    @Test
    def replace02(): Unit \ Assert = region rc {
        let s = MutHashSet.empty(rc);
        MutHashSet.addAll(1 :: 2 :: 3 :: Nil, s);
        MutHashSet.replace(src = 99, dst = 42, s);
        assertEq(expected = 3, MutHashSet.size(s));
        assertFalse(MutHashSet.memberOf(42, s))
    }

    @Test
    def replace03(): Unit \ Assert = region rc {
        let s: MutHashSet[Int32, _] = MutHashSet.empty(rc);
        MutHashSet.replace(src = 1, dst = 2, s);
        assertTrue(MutHashSet.isEmpty(s))
    }

    /////////////////////////////////////////////////////////////////////////////
    // find / findLeft / findRight                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def find01(): Unit \ Assert = region rc {
        let s: MutHashSet[Int32, rc] = MutHashSet.empty(rc);
        assertEq(expected = None, MutHashSet.find(x -> x > 0, s))
    }

    @Test
    def find02(): Unit \ Assert = region rc {
        let s = MutHashSet.empty(rc);
        MutHashSet.addAll(1 :: 2 :: 3 :: Nil, s);
        assertEq(expected = Some(2), MutHashSet.find(x -> x == 2, s))
    }

    @Test
    def find03(): Unit \ Assert = region rc {
        let s = MutHashSet.empty(rc);
        MutHashSet.addAll(1 :: 2 :: 3 :: Nil, s);
        assertEq(expected = None, MutHashSet.find(x -> x > 10, s))
    }

    @Test
    def findLeft01(): Unit \ Assert = region rc {
        let s = MutHashSet.empty(rc);
        MutHashSet.addAll(1 :: 2 :: 3 :: 4 :: Nil, s);
        assertEq(expected = Some(2), MutHashSet.findLeft(x -> Int32.remainder(x, 2) == 0, s))
    }

    @Test
    def findLeft02(): Unit \ Assert = region rc {
        let s: MutHashSet[Int32, _] = MutHashSet.empty(rc);
        assertEq(expected = None, MutHashSet.findLeft(x -> x > 0, s))
    }

    @Test
    def findLeft03(): Unit \ Assert = region rc {
        let s = MutHashSet.empty(rc);
        MutHashSet.addAll(1 :: 3 :: 5 :: Nil, s);
        assertEq(expected = None, MutHashSet.findLeft(x -> Int32.remainder(x, 2) == 0, s))
    }

    @Test
    def findRight01(): Unit \ Assert = region rc {
        let s = MutHashSet.empty(rc);
        MutHashSet.addAll(1 :: 2 :: 3 :: 4 :: Nil, s);
        assertEq(expected = Some(4), MutHashSet.findRight(x -> Int32.remainder(x, 2) == 0, s))
    }

    @Test
    def findRight02(): Unit \ Assert = region rc {
        let s: MutHashSet[Int32, _] = MutHashSet.empty(rc);
        assertEq(expected = None, MutHashSet.findRight(x -> x > 0, s))
    }

    @Test
    def findRight03(): Unit \ Assert = region rc {
        let s = MutHashSet.empty(rc);
        MutHashSet.addAll(1 :: 3 :: 5 :: Nil, s);
        assertEq(expected = None, MutHashSet.findRight(x -> Int32.remainder(x, 2) == 0, s))
    }

    /////////////////////////////////////////////////////////////////////////////
    // exists                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def exists01(): Unit \ Assert = region rc {
        let s: MutHashSet[Int32, rc] = MutHashSet.empty(rc);
        assertFalse(MutHashSet.exists(x -> x > 0, s))
    }

    @Test
    def exists02(): Unit \ Assert = region rc {
        let s = MutHashSet.empty(rc);
        MutHashSet.addAll(1 :: 2 :: 3 :: Nil, s);
        assertTrue(MutHashSet.exists(x -> x == 2, s))
    }

    @Test
    def exists03(): Unit \ Assert = region rc {
        let s = MutHashSet.empty(rc);
        MutHashSet.addAll(1 :: 2 :: 3 :: Nil, s);
        assertFalse(MutHashSet.exists(x -> x == 5, s))
    }

    /////////////////////////////////////////////////////////////////////////////
    // forAll                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forAll01(): Unit \ Assert = region rc {
        let s: MutHashSet[Int32, rc] = MutHashSet.empty(rc);
        assertTrue(MutHashSet.forAll(x -> x > 0, s))
    }

    @Test
    def forAll02(): Unit \ Assert = region rc {
        let s = MutHashSet.empty(rc);
        MutHashSet.addAll(1 :: 2 :: 3 :: Nil, s);
        assertTrue(MutHashSet.forAll(x -> x > 0, s))
    }

    @Test
    def forAll03(): Unit \ Assert = region rc {
        let s = MutHashSet.empty(rc);
        MutHashSet.addAll(1 :: 2 :: 3 :: Nil, s);
        assertFalse(MutHashSet.forAll(x -> x > 2, s))
    }

    /////////////////////////////////////////////////////////////////////////////
    // count                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def count01(): Unit \ Assert = region rc {
        let s: MutHashSet[Int32, rc] = MutHashSet.empty(rc);
        assertEq(expected = 0, MutHashSet.count(x -> x > 0, s))
    }

    @Test
    def count02(): Unit \ Assert = region rc {
        let s = MutHashSet.empty(rc);
        MutHashSet.addAll(1 :: 2 :: 3 :: 4 :: 5 :: Nil, s);
        assertEq(expected = 2, MutHashSet.count(x -> Int32.remainder(x, 2) == 0, s))
    }

    @Test
    def count03(): Unit \ Assert = region rc {
        let s = MutHashSet.empty(rc);
        MutHashSet.addAll(1 :: 2 :: 3 :: Nil, s);
        assertEq(expected = 3, MutHashSet.count(_ -> true, s))
    }

    /////////////////////////////////////////////////////////////////////////////
    // foldLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldLeft01(): Unit \ Assert = region rc {
        let s: MutHashSet[Int32, rc] = MutHashSet.empty(rc);
        let result = MutHashSet.foldLeft((acc, x) -> acc + x, 0, s);
        assertEq(expected = 0, result)
    }

    @Test
    def foldLeft02(): Unit \ Assert = region rc {
        let s = MutHashSet.empty(rc);
        MutHashSet.addAll(1 :: 2 :: 3 :: Nil, s);
        let result = MutHashSet.foldLeft((acc, x) -> acc + x, 0, s);
        assertEq(expected = 6, result)
    }

    @Test
    def foldLeft03(): Unit \ Assert = region rc {
        let s = MutHashSet.empty(rc);
        MutHashSet.addAll(1 :: 2 :: 3 :: Nil, s);
        let result = MutHashSet.foldLeft((acc, x) -> acc * x, 1, s);
        assertEq(expected = 6, result)
    }

    /////////////////////////////////////////////////////////////////////////////
    // foldRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldRight01(): Unit \ Assert = region rc {
        let s: MutHashSet[Int32, rc] = MutHashSet.empty(rc);
        let result = MutHashSet.foldRight((x, acc) -> x + acc, 0, s);
        assertEq(expected = 0, result)
    }

    @Test
    def foldRight02(): Unit \ Assert = region rc {
        let s = MutHashSet.empty(rc);
        MutHashSet.addAll(1 :: 2 :: 3 :: Nil, s);
        let result = MutHashSet.foldRight((x, acc) -> x + acc, 0, s);
        assertEq(expected = 6, result)
    }

    @Test
    def foldRight03(): Unit \ Assert = region rc {
        let s = MutHashSet.empty(rc);
        MutHashSet.addAll(1 :: 2 :: 3 :: Nil, s);
        let result = MutHashSet.foldRight((x, acc) -> x * acc, 1, s);
        assertEq(expected = 6, result)
    }

    /////////////////////////////////////////////////////////////////////////////
    // foldMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldMap01(): Unit \ Assert = region rc {
        let s: MutHashSet[Int32, _] = MutHashSet.empty(rc);
        let result = MutHashSet.foldMap(x -> x, s);
        assertEq(expected = 0, result)
    }

    @Test
    def foldMap02(): Unit \ Assert = region rc {
        let s = MutHashSet.empty(rc);
        MutHashSet.addAll(1 :: 2 :: 3 :: Nil, s);
        let result = MutHashSet.foldMap(x -> x, s);
        assertEq(expected = 6, result)
    }

    @Test
    def foldMap03(): Unit \ Assert = region rc {
        let s = MutHashSet.empty(rc);
        MutHashSet.addAll(1 :: 2 :: 3 :: Nil, s);
        let result = MutHashSet.foldMap(x -> x * 2, s);
        assertEq(expected = 12, result)
    }

    /////////////////////////////////////////////////////////////////////////////
    // forEach / forEachWithIndex                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forEach01(): Unit \ Assert = region rc {
        let s = MutHashSet.empty(rc);
        let count = Ref.fresh(rc, 0);
        MutHashSet.forEach(_ -> Ref.put(Ref.get(count) + 1, count), s);
        assertEq(expected = 0, Ref.get(count))
    }

    @Test
    def forEach02(): Unit \ Assert = region rc {
        let s = MutHashSet.empty(rc);
        MutHashSet.addAll(1 :: 2 :: 3 :: Nil, s);
        let sum = Ref.fresh(rc, 0);
        MutHashSet.forEach(x -> Ref.put(Ref.get(sum) + x, sum), s);
        assertEq(expected = 6, Ref.get(sum))
    }

    @Test
    def forEach03(): Unit \ Assert = region rc {
        let s = MutHashSet.empty(rc);
        MutHashSet.addAll(1 :: 2 :: 3 :: 4 :: 5 :: Nil, s);
        let count = Ref.fresh(rc, 0);
        MutHashSet.forEach(_ -> Ref.put(Ref.get(count) + 1, count), s);
        assertEq(expected = 5, Ref.get(count))
    }

    @Test
    def forEachWithIndex01(): Unit \ Assert = region rc {
        let s = MutHashSet.empty(rc);
        MutHashSet.addAll(10 :: 20 :: 30 :: Nil, s);
        let sum = Ref.fresh(rc, 0);
        MutHashSet.forEachWithIndex((i, x) -> Ref.put(Ref.get(sum) + i + x, sum), s);
        assertEq(expected = 63, Ref.get(sum))
    }

    @Test
    def forEachWithIndex02(): Unit \ Assert = region rc {
        let s: MutHashSet[Int32, _] = MutHashSet.empty(rc);
        let count = Ref.fresh(rc, 0);
        MutHashSet.forEachWithIndex((_, _) -> Ref.put(Ref.get(count) + 1, count), s);
        assertEq(expected = 0, Ref.get(count))
    }

    @Test
    def forEachWithIndex03(): Unit \ Assert = region rc {
        let s = MutHashSet.empty(rc);
        MutHashSet.addAll(1 :: 2 :: 3 :: Nil, s);
        let lastIndex = Ref.fresh(rc, -1);
        MutHashSet.forEachWithIndex((i, _) -> Ref.put(i, lastIndex), s);
        assertEq(expected = 2, Ref.get(lastIndex))
    }

    /////////////////////////////////////////////////////////////////////////////
    // iterator                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def iterator01(): Unit \ Assert = region rc {
        let s = MutHashSet.empty(rc);
        MutHashSet.addAll(1 :: 2 :: 3 :: Nil, s);
        region rc2 {
            let iter = MutHashSet.iterator(rc2, s);
            let list = Iterator.toList(iter);
            assertEq(expected = 1 :: 2 :: 3 :: Nil, list)
        }
    }

    @Test
    def iterator02(): Unit \ Assert = region rc {
        let s: MutHashSet[Int32, _] = MutHashSet.empty(rc);
        region rc2 {
            let iter = MutHashSet.iterator(rc2, s);
            assertEq(expected = Nil, Iterator.toList(iter))
        }
    }

    @Test
    def iterator03(): Unit \ Assert = region rc {
        let s = MutHashSet.empty(rc);
        MutHashSet.add(42, s);
        region rc2 {
            let iter = MutHashSet.iterator(rc2, s);
            assertEq(expected = 42 :: Nil, Iterator.toList(iter))
        }
    }

    /////////////////////////////////////////////////////////////////////////////
    // copy                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def copy01(): Unit \ Assert = region rc {
        let s = MutHashSet.empty(rc);
        MutHashSet.addAll(1 :: 2 :: 3 :: Nil, s);
        region rc2 {
            let s2 = MutHashSet.copy(rc2, s);
            assertEq(expected = 3, MutHashSet.size(s2));
            assertTrue(MutHashSet.memberOf(1, s2));
            assertTrue(MutHashSet.memberOf(2, s2));
            assertTrue(MutHashSet.memberOf(3, s2))
        }
    }

    @Test
    def copy02(): Unit \ Assert = region rc {
        let s: MutHashSet[Int32, _] = MutHashSet.empty(rc);
        region rc2 {
            let s2 = MutHashSet.copy(rc2, s);
            assertTrue(MutHashSet.isEmpty(s2))
        }
    }

    @Test
    def copy03(): Unit \ Assert = region rc {
        let s = MutHashSet.empty(rc);
        MutHashSet.addAll(1 :: 2 :: 3 :: Nil, s);
        region rc2 {
            let s2 = MutHashSet.copy(rc2, s);
            MutHashSet.add(4, s);
            assertEq(expected = 3, MutHashSet.size(s2));
            assertEq(expected = 4, MutHashSet.size(s))
        }
    }

    /////////////////////////////////////////////////////////////////////////////
    // toSet                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toSet01(): Unit \ Assert = region rc {
        let s: MutHashSet[Int32, rc] = MutHashSet.empty(rc);
        let set = MutHashSet.toSet(s);
        assertEq(expected = Set#{}, set)
    }

    @Test
    def toSet02(): Unit \ Assert = region rc {
        let s = MutHashSet.empty(rc);
        MutHashSet.addAll(1 :: 2 :: 3 :: Nil, s);
        let set = MutHashSet.toSet(s);
        assertEq(expected = Set#{1, 2, 3}, set)
    }

    @Test
    def toSet03(): Unit \ Assert = region rc {
        let s = MutHashSet.empty(rc);
        MutHashSet.add(42, s);
        let set = MutHashSet.toSet(s);
        assertEq(expected = Set#{42}, set)
    }

    /////////////////////////////////////////////////////////////////////////////
    // toList                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toList01(): Unit \ Assert = region rc {
        let s: MutHashSet[Int32, rc] = MutHashSet.empty(rc);
        assertEq(expected = Nil, MutHashSet.toList(s))
    }

    @Test
    def toList02(): Unit \ Assert = region rc {
        let s = MutHashSet.empty(rc);
        MutHashSet.addAll(1 :: 2 :: 3 :: Nil, s);
        assertEq(expected = 1 :: 2 :: 3 :: Nil, MutHashSet.toList(s))
    }

    @Test
    def toList03(): Unit \ Assert = region rc {
        let s = MutHashSet.empty(rc);
        MutHashSet.add(42, s);
        assertEq(expected = 42 :: Nil, MutHashSet.toList(s))
    }

    /////////////////////////////////////////////////////////////////////////////
    // toMap                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toMap01(): Unit \ Assert = region rc {
        let s: MutHashSet[(Int32, String), rc] = MutHashSet.empty(rc);
        let m = MutHashSet.toMap(s);
        assertEq(expected = Map#{}, m)
    }

    @Test
    def toMap02(): Unit \ Assert = region rc {
        let s = MutHashSet.empty(rc);
        MutHashSet.addAll((1, "a") :: (2, "b") :: (3, "c") :: Nil, s);
        let m = MutHashSet.toMap(s);
        assertEq(expected = Map#{1 => "a", 2 => "b", 3 => "c"}, m)
    }

    @Test
    def toMap03(): Unit \ Assert = region rc {
        let s = MutHashSet.empty(rc);
        MutHashSet.add((1, "a"), s);
        let m = MutHashSet.toMap(s);
        assertEq(expected = Map#{1 => "a"}, m)
    }

    /////////////////////////////////////////////////////////////////////////////
    // toArray                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toArray01(): Unit \ Assert = region rc {
        let s = MutHashSet.empty(rc);
        MutHashSet.addAll(1 :: 2 :: 3 :: Nil, s);
        region rc2 {
            let arr = MutHashSet.toArray(rc2, s);
            assertEq(expected = 1 :: 2 :: 3 :: Nil, Array.toList(arr))
        }
    }

    @Test
    def toArray02(): Unit \ Assert = region rc {
        let s: MutHashSet[Int32, _] = MutHashSet.empty(rc);
        region rc2 {
            let arr = MutHashSet.toArray(rc2, s);
            assertEq(expected = Nil, Array.toList(arr))
        }
    }

    @Test
    def toArray03(): Unit \ Assert = region rc {
        let s = MutHashSet.empty(rc);
        MutHashSet.add(42, s);
        region rc2 {
            let arr = MutHashSet.toArray(rc2, s);
            assertEq(expected = 42 :: Nil, Array.toList(arr))
        }
    }

    /////////////////////////////////////////////////////////////////////////////
    // toVector                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toVector01(): Unit \ Assert = region rc {
        let s: MutHashSet[Int32, rc] = MutHashSet.empty(rc);
        assertEq(expected = Vector#{}, MutHashSet.toVector(s))
    }

    @Test
    def toVector02(): Unit \ Assert = region rc {
        let s = MutHashSet.empty(rc);
        MutHashSet.addAll(1 :: 2 :: 3 :: Nil, s);
        assertEq(expected = Vector#{1, 2, 3}, MutHashSet.toVector(s))
    }

    @Test
    def toVector03(): Unit \ Assert = region rc {
        let s = MutHashSet.empty(rc);
        MutHashSet.add(42, s);
        assertEq(expected = Vector#{42}, MutHashSet.toVector(s))
    }

    /////////////////////////////////////////////////////////////////////////////
    // join                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def join01(): Unit \ Assert = region rc {
        let s: MutHashSet[Int32, rc] = MutHashSet.empty(rc);
        assertEq(expected = "", MutHashSet.join(", ", s))
    }

    @Test
    def join02(): Unit \ Assert = region rc {
        let s = MutHashSet.empty(rc);
        MutHashSet.addAll(1 :: 2 :: 3 :: Nil, s);
        assertEq(expected = "1, 2, 3", MutHashSet.join(", ", s))
    }

    @Test
    def join03(): Unit \ Assert = region rc {
        let s = MutHashSet.empty(rc);
        MutHashSet.add(42, s);
        assertEq(expected = "42", MutHashSet.join(", ", s))
    }

    /////////////////////////////////////////////////////////////////////////////
    // joinWith                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def joinWith01(): Unit \ Assert = region rc {
        let s: MutHashSet[Int32, rc] = MutHashSet.empty(rc);
        assertEq(expected = "", MutHashSet.joinWith(x -> "${x}", ", ", s))
    }

    @Test
    def joinWith02(): Unit \ Assert = region rc {
        let s = MutHashSet.empty(rc);
        MutHashSet.addAll(1 :: 2 :: 3 :: Nil, s);
        assertEq(expected = "[1], [2], [3]", MutHashSet.joinWith(x -> "[${x}]", ", ", s))
    }

    @Test
    def joinWith03(): Unit \ Assert = region rc {
        let s = MutHashSet.empty(rc);
        MutHashSet.add(42, s);
        assertEq(expected = "[42]", MutHashSet.joinWith(x -> "[${x}]", ", ", s))
    }

    /////////////////////////////////////////////////////////////////////////////
    // Large dataset stress test                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def largeDataset01(): Unit \ Assert = region rc {
        let s = MutHashSet.empty(rc);
        foreach(i <- List.range(0, 100)) {
            MutHashSet.add(i, s)
        };
        assertEq(expected = 100, MutHashSet.size(s))
    }

    @Test
    def largeDataset02(): Unit \ Assert = region rc {
        let s = MutHashSet.empty(rc);
        foreach(i <- List.range(0, 100)) {
            MutHashSet.add(i, s)
        };
        foreach(i <- List.range(0, 50)) {
            MutHashSet.remove(i, s)
        };
        assertEq(expected = 50, MutHashSet.size(s))
    }

}

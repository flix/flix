mod TestSet {

    use Assert.{assertEq, assertTrue, assertFalse};
    use ToString.toString;
    use Hash.hash;

    /////////////////////////////////////////////////////////////////////////////
    // Collectable.collect                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def collect01(): Unit \ Assert = region rc {
        assertEq(expected = Set.empty(), Collectable.collect((Iterator.empty(rc): Iterator[Int32, rc, rc])))
    }

    @Test
    def collect02(): Unit \ Assert = region rc {
        assertEq(expected = Set#{1}, Collectable.collect(Iterator.singleton(rc, 1)))
    }

    @Test
    def collect03(): Unit \ Assert = region rc {
        assertEq(expected = Set#{1, 2}, Collectable.collect(Iterator.range(rc, 1, 3)))
    }

    /////////////////////////////////////////////////////////////////////////////
    // size                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def size01(): Unit \ Assert = 
        assertEq(expected = 0, Set.size(Set#{}))

    @Test
    def size02(): Unit \ Assert = 
        assertEq(expected = 1, Set.size(Set#{1}))

    @Test
    def size03(): Unit \ Assert = 
        assertEq(expected = 2, Set.size(Set#{1, 2}))

    @Test
    def size04(): Unit \ Assert = 
        assertEq(expected = 3, Set.size(Set#{1, 2, 3}))

    @Test
    def size05(): Unit \ Assert = 
        assertEq(expected = 13, Set.size(Set#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 14}))

    /////////////////////////////////////////////////////////////////////////////
    // empty                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def empty01(): Unit \ Assert = 
        assertEq(expected = Set#{}, (Set.empty(): Set[Unit]))

    /////////////////////////////////////////////////////////////////////////////
    // singleton                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def singleton01(): Unit \ Assert = 
        assertEq(expected = Set#{1}, Set.singleton(1))

    @Test
    def singleton02(): Unit \ Assert = 
        assertEq(expected = Set#{2}, Set.singleton(2))

    @Test
    def singleton03(): Unit \ Assert = 
        assertEq(expected = Set#{true}, Set.singleton(true))

    @Test
    def singleton04(): Unit \ Assert = 
        assertEq(expected = Set#{false}, Set.singleton(false))

    /////////////////////////////////////////////////////////////////////////////
    // insert                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def insert01(): Unit \ Assert = 
        assertEq(expected = Set#{1}, Set.insert(1, Set#{}))

    @Test
    def insert02(): Unit \ Assert = 
        assertEq(expected = Set#{1}, Set.insert(1, Set#{1}))

    @Test
    def insert03(): Unit \ Assert = 
        assertEq(expected = Set#{1, 2}, Set.insert(1, Set#{2}))

    @Test
    def insert04(): Unit \ Assert = 
        assertEq(expected = Set#{1, 2, 3}, Set.insert(1, Set#{2, 3}))

    @Test
    def insert05(): Unit \ Assert = 
        assertEq(expected = Set#{1, 2, 3}, Set.insert(1, Set#{1, 2, 3}))

    @Test
    def insert06(): Unit \ Assert = 
        assertEq(expected = Set#{2, 1, 3}, Set.insert(1, Set#{2, 1, 3}))

    @Test
    def insert07(): Unit \ Assert = 
        assertEq(expected = Set#{2, 1, 3, 7, 8, 9, 10}, Set.insert(1, Set#{2, 1, 3, 7, 8, 9, 10}))

    @Test
    def insert08(): Unit \ Assert = 
        assertEq(expected = Set#{11, 2, 1, 3, 7, 8, 9, 10, 35}, Set.insert(11, Set#{2, 1, 3, 7, 8, 9, 10, 35}))

    /////////////////////////////////////////////////////////////////////////////
    // remove                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def remove01(): Unit \ Assert = 
        assertEq(expected = Set#{}, Set.remove(1, Set#{}))

    @Test
    def remove02(): Unit \ Assert = 
        assertEq(expected = Set#{}, Set.remove(1, Set#{1}))

    @Test
    def remove03(): Unit \ Assert = 
        assertEq(expected = Set#{2}, Set.remove(1, Set#{2}))

    @Test
    def remove04(): Unit \ Assert = 
        assertEq(expected = Set#{3}, Set.remove(2, Set#{2, 3}))

    @Test
    def remove05(): Unit \ Assert = 
        assertEq(expected = Set#{2}, Set.remove(3, Set#{2, 3}))

    @Test
    def remove06(): Unit \ Assert = 
        assertEq(expected = Set#{2, 3}, Set.remove(1, Set#{2, 3}))

    @Test
    def remove07(): Unit \ Assert = 
        assertEq(expected = Set#{1, 2, 3, 4, 5, 6, 7}, Set.remove(0, Set#{1, 2, 3, 4, 5, 6, 7}))

    @Test
    def remove08(): Unit \ Assert = 
        assertEq(expected = Set#{2, 3, 4, 5, 6, 7}, Set.remove(1, Set#{1, 2, 3, 4, 5, 6, 7}))

    @Test
    def remove09(): Unit \ Assert = 
        assertEq(expected = Set#{1, 3, 4, 5, 6, 7}, Set.remove(2, Set#{1, 2, 3, 4, 5, 6, 7}))

    @Test
    def remove10(): Unit \ Assert = 
        assertEq(expected = Set#{1, 2, 3, 4, 5, 7}, Set.remove(6, Set#{1, 2, 3, 4, 5, 6, 7}))

    @Test
    def remove11(): Unit \ Assert = 
        assertEq(expected = Set#{1, 2, 3, 4, 5, 6}, Set.remove(7, Set#{1, 2, 3, 4, 5, 6, 7}))

    @Test
    def remove12(): Unit \ Assert = 
        assertEq(expected = Set#{1, 2, 3, 4, 5, 6, 7}, Set.remove(8, Set#{1, 2, 3, 4, 5, 6, 7}))

    /////////////////////////////////////////////////////////////////////////////
    // isEmpty                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isEmpty01(): Unit \ Assert = 
        assertTrue(Set.isEmpty((Set#{}: Set[Unit])))

    @Test
    def isEmpty02(): Unit \ Assert = 
        assertFalse(Set.isEmpty(Set#{1}))

    @Test
    def isEmpty03(): Unit \ Assert = 
        assertFalse(Set.isEmpty(Set#{1, 2}))

    @Test
    def isEmpty04(): Unit \ Assert = 
        assertFalse(Set.isEmpty(Set#{1, 2, 3}))

    @Test
    def isEmpty05(): Unit \ Assert = 
        assertFalse(Set.isEmpty(Set#{1, 2, 3, 4, 5, 6, 7, 8}))

    /////////////////////////////////////////////////////////////////////////////
    // nonEmpty                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def nonEmpty01(): Unit \ Assert = 
        assertFalse(Set.nonEmpty((Set#{}: Set[Unit])))

    @Test
    def nonEmpty02(): Unit \ Assert = 
        assertTrue(Set.nonEmpty(Set#{1}))

    @Test
    def nonEmpty03(): Unit \ Assert = 
        assertTrue(Set.nonEmpty(Set#{1, 2}))

    @Test
    def nonEmpty04(): Unit \ Assert = 
        assertTrue(Set.nonEmpty(Set#{1, 2, 3}))

    @Test
    def nonEmpty05(): Unit \ Assert = 
        assertTrue(Set.nonEmpty(Set#{1, 2, 3, 4, 5, 6, 7, 8}))

    /////////////////////////////////////////////////////////////////////////////
    // memberOf                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def memberOf01(): Unit \ Assert = 
        assertFalse(Set.memberOf(1, Set#{}))

    @Test
    def memberOf02(): Unit \ Assert = 
        assertFalse(Set.memberOf(1, Set#{2}))

    @Test
    def memberOf03(): Unit \ Assert = 
        assertTrue(Set.memberOf(1, Set#{1}))

    @Test
    def memberOf04(): Unit \ Assert = 
        assertTrue(Set.memberOf(1, Set#{1, 2}))

    @Test
    def memberOf05(): Unit \ Assert = 
        assertTrue(Set.memberOf(2, Set#{1, 2}))

    @Test
    def memberOf06(): Unit \ Assert = 
        assertFalse(Set.memberOf(0, Set#{1, 2}))

    @Test
    def memberOf07(): Unit \ Assert = 
        assertFalse(Set.memberOf(3, Set#{1, 2}))

    @Test
    def memberOf08(): Unit \ Assert = 
        assertFalse(Set.memberOf(0, Set#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}))

    @Test
    def memberOf09(): Unit \ Assert = 
        assertTrue(Set.memberOf(1, Set#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}))

    @Test
    def memberOf10(): Unit \ Assert = 
        assertTrue(Set.memberOf(2, Set#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}))

    @Test
    def memberOf11(): Unit \ Assert = 
        assertTrue(Set.memberOf(10, Set#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}))

    @Test
    def memberOf12(): Unit \ Assert = 
        assertFalse(Set.memberOf(12, Set#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}))

    /////////////////////////////////////////////////////////////////////////////
    // find                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def find01(): Unit \ Assert = 
        assertEq(expected = None, Set.find(i -> i > 2, Set#{}))

    @Test
    def find02(): Unit \ Assert = 
        assertEq(expected = None, Set.find(i -> i > 2, Set#{1}))

    @Test
    def find03(): Unit \ Assert = 
        assertEq(expected = Some(3), Set.find(i -> i > 2, Set#{3}))

    @Test
    def find04(): Unit \ Assert = 
        assertEq(expected = None, Set.find(i -> i > 2, Set#{2, 1}))

    @Test
    def find05(): Unit \ Assert = 
        assertEq(expected = Some(6), Set.find(i -> i > 2, Set#{-6, 6}))

    @Test
    def find06(): Unit \ Assert = 
        assertEq(expected = Some(6), Set.find(i -> i > 2, Set#{6, -6}))

    @Test
    def find07(): Unit \ Assert = 
        assertEq(expected = Some(6), Set.find(i -> i > 2, Set#{7, 6}))

    /////////////////////////////////////////////////////////////////////////////
    // findLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def findLeft01(): Unit \ Assert = 
        assertEq(expected = None, Set.findLeft(i -> i > 2, Set#{}))

    @Test
    def findLeft02(): Unit \ Assert = 
        assertEq(expected = None, Set.findLeft(i -> i > 2, Set#{1}))

    @Test
    def findLeft03(): Unit \ Assert = 
        assertEq(expected = Some(3), Set.findLeft(i -> i > 2, Set#{3}))

    @Test
    def findLeft04(): Unit \ Assert = 
        assertEq(expected = None, Set.findLeft(i -> i > 2, Set#{2, 1}))

    @Test
    def findLeft05(): Unit \ Assert = 
        assertEq(expected = Some(6), Set.findLeft(i -> i > 2, Set#{-6, 6}))

    @Test
    def findLeft06(): Unit \ Assert = 
        assertEq(expected = Some(6), Set.findLeft(i -> i > 2, Set#{6, -6}))

    @Test
    def findLeft07(): Unit \ Assert = 
        assertEq(expected = Some(6), Set.findLeft(i -> i > 2, Set#{7, 6}))

    /////////////////////////////////////////////////////////////////////////////
    // findRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def findRight01(): Unit \ Assert = 
        assertEq(expected = None, Set.findRight(i -> i > 2, Set#{}))

    @Test
    def findRight02(): Unit \ Assert = 
        assertEq(expected = None, Set.findRight(i -> i > 2, Set#{1}))

    @Test
    def findRight03(): Unit \ Assert = 
        assertEq(expected = Some(3), Set.findRight(i -> i > 2, Set#{3}))

    @Test
    def findRight04(): Unit \ Assert = 
        assertEq(expected = None, Set.findRight(i -> i > 2, Set#{2, 1}))

    @Test
    def findRight05(): Unit \ Assert = 
        assertEq(expected = Some(6), Set.findRight(i -> i > 2, Set#{-6, 6}))

    @Test
    def findRight06(): Unit \ Assert = 
        assertEq(expected = Some(6), Set.findRight(i -> i > 2, Set#{6, -6}))

    @Test
    def findRight07(): Unit \ Assert = 
        assertEq(expected = Some(7), Set.findRight(i -> i > 2, Set#{7, 6}))

    /////////////////////////////////////////////////////////////////////////////
    // isSubsetOf                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isSubsetOf01(): Unit \ Assert = 
        assertTrue(Set.isSubsetOf((Set#{}: Set[Unit]), Set#{}))

    @Test
    def isSubsetOf02(): Unit \ Assert = 
        assertTrue(Set.isSubsetOf(Set#{}, Set#{1}))

    @Test
    def isSubsetOf03(): Unit \ Assert = 
        assertTrue(Set.isSubsetOf(Set#{1}, Set#{1}))

    @Test
    def isSubsetOf04(): Unit \ Assert = 
        assertFalse(Set.isSubsetOf(Set#{1}, Set#{2}))

    @Test
    def isSubsetOf05(): Unit \ Assert = 
        assertTrue(Set.isSubsetOf(Set#{}, Set#{1, 2}))

    @Test
    def isSubsetOf06(): Unit \ Assert = 
        assertTrue(Set.isSubsetOf(Set#{1}, Set#{1, 2}))

    @Test
    def isSubsetOf07(): Unit \ Assert = 
        assertTrue(Set.isSubsetOf(Set#{2}, Set#{1, 2}))

    @Test
    def isSubsetOf08(): Unit \ Assert = 
        assertFalse(Set.isSubsetOf(Set#{3}, Set#{1, 2}))

    @Test
    def isSubsetOf09(): Unit \ Assert = 
        assertTrue(Set.isSubsetOf(Set#{1, 2}, Set#{1, 2}))

    @Test
    def isSubsetOf10(): Unit \ Assert = 
        assertTrue(Set.isSubsetOf(Set#{2, 1}, Set#{1, 2}))

    @Test
    def isSubsetOf11(): Unit \ Assert = 
        assertFalse(Set.isSubsetOf(Set#{3, 1}, Set#{1, 2}))

    @Test
    def isSubsetOf12(): Unit \ Assert = 
        assertFalse(Set.isSubsetOf(Set#{1, 2, 3}, Set#{1, 2}))

    @Test
    def isSubsetOf13(): Unit \ Assert = 
        assertTrue(Set.isSubsetOf(Set#{10}, Set#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}))

    @Test
    def isSubsetOf14(): Unit \ Assert = 
        assertTrue(Set.isSubsetOf(Set#{9, 1}, Set#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}))

    @Test
    def isSubsetOf15(): Unit \ Assert = 
        assertTrue(Set.isSubsetOf(Set#{6, 5, 8}, Set#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}))

    @Test
    def isSubsetOf16(): Unit \ Assert = 
        assertFalse(Set.isSubsetOf(Set#{10, 2, 3, 6, -1, 4}, Set#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}))

    /////////////////////////////////////////////////////////////////////////////
    // isProperSubsetOf                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isProperSubsetOf01(): Unit \ Assert = 
        assertFalse(Set.isProperSubsetOf((Set#{}: Set[Unit]), Set#{}))

    @Test
    def isProperSubsetOf02(): Unit \ Assert = 
        assertTrue(Set.isProperSubsetOf(Set#{}, Set#{1}))

    @Test
    def isProperSubsetOf03(): Unit \ Assert = 
        assertFalse(Set.isProperSubsetOf(Set#{1}, Set#{1}))

    @Test
    def isProperSubsetOf04(): Unit \ Assert = 
        assertFalse(Set.isProperSubsetOf(Set#{1}, Set#{2}))

    @Test
    def isProperSubsetOf05(): Unit \ Assert = 
        assertTrue(Set.isProperSubsetOf(Set#{}, Set#{1, 2}))

    @Test
    def isProperSubsetOf06(): Unit \ Assert = 
        assertTrue(Set.isProperSubsetOf(Set#{1}, Set#{1, 2}))

    @Test
    def isProperSubsetOf07(): Unit \ Assert = 
        assertTrue(Set.isProperSubsetOf(Set#{2}, Set#{1, 2}))

    @Test
    def isProperSubsetOf08(): Unit \ Assert = 
        assertFalse(Set.isProperSubsetOf(Set#{3}, Set#{1, 2}))

    @Test
    def isProperSubsetOf09(): Unit \ Assert = 
        assertFalse(Set.isProperSubsetOf(Set#{1, 2}, Set#{1, 2}))

    @Test
    def isProperSubsetOf10(): Unit \ Assert = 
        assertFalse(Set.isProperSubsetOf(Set#{2, 1}, Set#{1, 2}))

    @Test
    def isProperSubsetOf11(): Unit \ Assert = 
        assertFalse(Set.isProperSubsetOf(Set#{3, 1}, Set#{1, 2}))

    @Test
    def isProperSubsetOf12(): Unit \ Assert = 
        assertFalse(Set.isProperSubsetOf(Set#{1, 2, 3}, Set#{1, 2}))

    @Test
    def isProperSubsetOf13(): Unit \ Assert = 
        assertTrue(Set.isProperSubsetOf(Set#{10}, Set#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}))

    @Test
    def isProperSubsetOf14(): Unit \ Assert = 
        assertTrue(Set.isProperSubsetOf(Set#{9, 1}, Set#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}))

    @Test
    def isProperSubsetOf15(): Unit \ Assert = 
        assertTrue(Set.isProperSubsetOf(Set#{6, 5, 8}, Set#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}))

    @Test
    def isProperSubsetOf16(): Unit \ Assert = 
        assertFalse(Set.isProperSubsetOf(Set#{10, 2, 3, 6, -1, 4}, Set#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}))

    @Test
    def isProperSubsetOf17(): Unit \ Assert = 
        assertFalse(Set.isProperSubsetOf(Set#{10, 2, 3, 6, 1, 4, 5, 9, 8, 7}, Set#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}))

    /////////////////////////////////////////////////////////////////////////////
    // fold                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def fold01(): Unit \ Assert = 
        assertEq(expected = "", Set.fold((Set#{}: Set[String])))

    @Test
    def fold02(): Unit \ Assert = 
        assertEq(expected = (), Set.fold((Set#{}: Set[Unit])))

    @Test
    def fold03(): Unit \ Assert = 
        assertEq(expected = "abc", Set.fold(Set#{"a", "b", "c"}))

    @Test
    def fold04(): Unit \ Assert = 
        assertEq(expected = ("ac", "bd"), Set.fold(Set#{("a", "b"), ("c", "d")}))

    /////////////////////////////////////////////////////////////////////////////
    // foldLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldLeft01(): Unit \ Assert = 
        assertEq(expected = 100, Set.foldLeft((i, e) -> (i - e)*(e `Int32.remainder` 2 + 1), 100, Set#{}))

    @Test
    def foldLeft02(): Unit \ Assert = 
        assertEq(expected = 198, Set.foldLeft((i, e) -> (i - e)*(e `Int32.remainder` 2 + 1), 100, Set#{1}))

    @Test
    def foldLeft03(): Unit \ Assert = 
        assertEq(expected = 196, Set.foldLeft((i, e) -> (i - e)*(e `Int32.remainder` 2 + 1), 100, Set#{2, 1}))

    @Test
    def foldLeft04(): Unit \ Assert = 
        assertEq(expected = 386, Set.foldLeft((i, e) -> (i - e)*(e `Int32.remainder` 2 + 1), 100, Set#{3, 2, 1}))

    /////////////////////////////////////////////////////////////////////////////
    // foldRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldRight01(): Unit \ Assert = 
        assertEq(expected = 100, Set.foldRight((e, acc) -> (acc - e) * (e `Int32.remainder` 2 + 1), 100, Set#{}))

    @Test
    def foldRight02(): Unit \ Assert = 
        assertEq(expected = 198, Set.foldRight((e, acc) -> (acc - e) * (e `Int32.remainder` 2 + 1), 100, Set#{1}))

    @Test
    def foldRight03(): Unit \ Assert = 
        assertEq(expected = 194, Set.foldRight((e, acc) -> (acc - e) * (e `Int32.remainder` 2 + 1), 100, Set#{2, 1}))

    @Test
    def foldRight04(): Unit \ Assert = 
        assertEq(expected = 382, Set.foldRight((e, acc) -> (acc - e) * (e `Int32.remainder` 2 + 1), 100, Set#{3, 2, 1}))

    /////////////////////////////////////////////////////////////////////////////
    // foldRightWithCont                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldRightWithCont01(): Unit \ Assert = 
        assertEq(expected = 100, Set.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.remainder` 2 + 1), 100, Set#{}))

    @Test
    def foldRightWithCont02(): Unit \ Assert = 
        assertEq(expected = 198, Set.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.remainder` 2 + 1), 100, Set#{1}))

    @Test
    def foldRightWithCont03(): Unit \ Assert = 
        assertEq(expected = 194, Set.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.remainder` 2 + 1), 100, Set#{2, 1}))

    @Test
    def foldRightWithCont04(): Unit \ Assert = 
        assertEq(expected = 382, Set.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.remainder` 2 + 1), 100, Set#{3, 2, 1}))

    /////////////////////////////////////////////////////////////////////////////
    // foldMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldMap01(): Unit \ Assert = 
        assertEq(expected = 0, Set.foldMap(x -> 2 * x, Set#{}))

    @Test
    def foldMap02(): Unit \ Assert = 
        assertEq(expected = 6, Set.foldMap(x -> 2 * x, Set#{1, 2}))

    @Test
    def foldMap03(): Unit \ Assert = 
        assertEq(expected = "b", Set.foldMap(x -> if (x == "a") "b" else x, Set#{"a"}))

    @Test
    def foldMap04(): Unit \ Assert = 
        assertEq(expected = "abb", Set.foldMap(x -> if (x == "c") "b" else x, Set#{"a", "b", "c"}))

    @Test
    def foldMap05(): Unit \ Assert = 
        assertEq(expected = "123", Set.foldMap(Int32.toString, Set#{1, 2, 3}))

    /////////////////////////////////////////////////////////////////////////////
    // reduceLeft                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def reduceLeft01(): Unit \ Assert = 
        assertEq(expected = None, Set.reduceLeft((a, b) -> a-b, (Set#{}: Set[Int32])))

    @Test
    def reduceLeft02(): Unit \ Assert = 
        assertEq(expected = Some(1), Set.reduceLeft((a, b) -> a-b, Set#{1}))

    @Test
    def reduceLeft03(): Unit \ Assert = 
        assertEq(expected = Some(-1), Set.reduceLeft((a, b) -> a-b, Set#{2, 1}))

    @Test
    def reduceLeft04(): Unit \ Assert = 
        assertEq(expected = Some(-4), Set.reduceLeft((a, b) -> a-b, Set#{3, 2, 1}))

    @Test
    def reduceLeft05(): Unit \ Assert = 
        assertEq(expected = Some(-8), Set.reduceLeft((a, b) -> a-b, Set#{4, 3, 2, 1}))

    /////////////////////////////////////////////////////////////////////////////
    // reduceRight                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def reduceRight01(): Unit \ Assert = 
        assertEq(expected = None, Set.reduceRight((a, b) -> a-b, (Set#{}: Set[Int32])))

    @Test
    def reduceRight02(): Unit \ Assert = 
        assertEq(expected = Some(1), Set.reduceRight((a, b) -> a-b, Set#{1}))

    @Test
    def reduceRight03(): Unit \ Assert = 
        assertEq(expected = Some(-1), Set.reduceRight((a, b) -> a-b, Set#{2, 1}))

    @Test
    def reduceRight04(): Unit \ Assert = 
        assertEq(expected = Some(2), Set.reduceRight((a, b) -> a-b, Set#{3, 2, 1}))

    @Test
    def reduceRight05(): Unit \ Assert = 
        assertEq(expected = Some(-2), Set.reduceRight((a, b) -> a-b, Set#{4, 3, 2, 1}))

    /////////////////////////////////////////////////////////////////////////////
    // count                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def count01(): Unit \ Assert = 
        assertEq(expected = 0, Set.count(i -> i > 3, Set#{}))

    @Test
    def count02(): Unit \ Assert = 
        assertEq(expected = 0, Set.count(i -> i > 3, Set#{1}))

    @Test
    def count03(): Unit \ Assert = 
        assertEq(expected = 1, Set.count(i -> i > 3, Set#{4}))

    @Test
    def count04(): Unit \ Assert = 
        assertEq(expected = 0, Set.count(i -> i > 3, Set#{2, 1}))

    @Test
    def count05(): Unit \ Assert = 
        assertEq(expected = 1, Set.count(i -> i > 3, Set#{8, 1}))

    @Test
    def count06(): Unit \ Assert = 
        assertEq(expected = 1, Set.count(i -> i > 3, Set#{1, 8}))

    @Test
    def count07(): Unit \ Assert = 
        assertEq(expected = 2, Set.count(i -> i > 3, Set#{7, 6}))

    @Test
    def count08(): Unit \ Assert = region rc {
        let range = List.range(0, 2000);
        let s = range |> List.toSet;
        let a = MutList.empty(rc);
        discard Set.count(x -> { let b = MutList.empty(rc); MutList.push(x, b); MutList.append(MutList.toList(b), a); true }, s);
        assertEq(expected = range, MutList.toList(a))
    }


    /////////////////////////////////////////////////////////////////////////////
    // sum                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sum01(): Unit \ Assert = 
        assertEq(expected = 0, Set.empty() |> Set.sum)

    @Test
    def sum02(): Unit \ Assert = 
        assertEq(expected = 1, Set#{1} |> Set.sum)

    @Test
    def sum03(): Unit \ Assert = 
        assertEq(expected = 6, Set#{1, 2, 3} |> Set.sum)

    @Test
    def sum04(): Unit \ Assert = 
        assertEq(expected = 3, Set#{1, 2, 3, -3} |> Set.sum)

    @Test
    def sum05(): Unit \ Assert = 
        assertEq(expected = -4, Set#{1, 2, -3, -4} |> Set.sum)

    @Test
    def sum06(): Unit \ Assert = 
        assertEq(expected = 0, Set#{10, -10} |> Set.sum)

    @Test
    def sum07(): Unit \ Assert = 
        assertEq(expected = 5050, Set.range(1, 101) |> Set.sum)


    /////////////////////////////////////////////////////////////////////////////
    // sumWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sumWith01(): Unit \ Assert = 
        assertEq(expected = 0, Set.empty() |> Set.sumWith(x -> x + 1))

    @Test
    def sumWith02(): Unit \ Assert = 
        assertEq(expected = 2, Set#{1} |> Set.sumWith(x -> x + 1))

    @Test
    def sumWith03(): Unit \ Assert = 
        assertEq(expected = 9, Set#{1, 2, 3} |> Set.sumWith(x -> x + 1))

    @Test
    def sumWith04(): Unit \ Assert = 
        assertEq(expected = 7, Set#{1, 2, 3, -3} |> Set.sumWith(x -> x + 1))

    @Test
    def sumWith05(): Unit \ Assert = 
        assertEq(expected = -6, Set#{-1, -2, -3, -4} |> Set.sumWith(x -> x + 1))

    @Test
    def sumWith06(): Unit \ Assert = 
        assertEq(expected = 2, Set#{10, -10} |> Set.sumWith(x -> x + 1))

    @Test
    def sumWith08(): Unit \ Assert = region rc {
        let range = List.range(0, 2000);
        let m = range |> List.toSet;
        let a = MutList.empty(rc);
        discard Set.sumWith(k -> { let b = MutList.empty(rc); MutList.push(k, b); MutList.append(MutList.toList(b), a); 1 }, m);
        assertEq(expected = range, MutList.toList(a))
    }


    /////////////////////////////////////////////////////////////////////////////
    // flatten                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def flatten01(): Unit \ Assert = 
        assertEq(expected = Set#{}, (Set.flatten(Set#{}): Set[Unit]))

    @Test
    def flatten02(): Unit \ Assert = 
        assertEq(expected = Set#{}, Set.flatten((Set#{Set#{}}: Set[Set[Unit]])))

    @Test
    def flatten03(): Unit \ Assert = 
        assertEq(expected = Set#{1}, Set.flatten(Set#{Set#{1}}))

    @Test
    def flatten04(): Unit \ Assert = 
        assertEq(expected = Set#{1, 2}, Set.flatten(Set#{Set#{1, 2}}))

    @Test
    def flatten05(): Unit \ Assert = 
        assertEq(expected = Set#{}, (Set.flatten(Set#{Set#{}, Set#{}}): Set[Unit]))

    @Test
    def flatten06(): Unit \ Assert = 
        assertEq(expected = Set#{1}, Set.flatten(Set#{Set#{1}, Set#{}}))

    @Test
    def flatten07(): Unit \ Assert = 
        assertEq(expected = Set#{1}, Set.flatten(Set#{Set#{}, Set#{1}}))

    @Test
    def flatten08(): Unit \ Assert = 
        assertEq(expected = Set#{1, 2}, Set.flatten(Set#{Set#{1}, Set#{2}}))

    @Test
    def flatten09(): Unit \ Assert = 
        assertEq(expected = Set#{1}, Set.flatten(Set#{Set#{1}, Set#{1}}))

    @Test
    def flatten10(): Unit \ Assert = 
        assertEq(expected = Set#{1, 2, 3, 4, 5}, Set.flatten(Set#{Set#{1, 2}, Set#{3, 4, 5}}))

    @Test
    def flatten11(): Unit \ Assert = 
        assertEq(expected = Set#{1, 2, 3, 4}, Set.flatten(Set#{Set#{1, 2}, Set#{3, 4, 3}}))

    @Test
    def flatten12(): Unit \ Assert = 
        assertEq(expected = Set#{1, 2, 3, 4}, Set.flatten(Set#{Set#{1, 2}, Set#{3, 4, 1}}))

    @Test
    def flatten13(): Unit \ Assert = 
        assertEq(expected = Set#{1, 2, 3, 4}, Set.flatten(Set#{Set#{1}, Set#{2, 3}, Set#{4}}))

    @Test
    def flatten14(): Unit \ Assert = 
        assertEq(expected = Set#{1, 2, 3}, Set.flatten(Set#{Set#{1}, Set#{2, 3}, Set#{1}}))

    /////////////////////////////////////////////////////////////////////////////
    // exists                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def exists01(): Unit \ Assert = 
        assertFalse(Set.exists(x -> x `Int32.remainder` 8 == 7, Set#{}))

    @Test
    def exists02(): Unit \ Assert = 
        assertFalse(Set.exists(x -> x `Int32.remainder` 8 == 7, Set#{5}))

    @Test
    def exists03(): Unit \ Assert = 
        assertTrue(Set.exists(x -> x `Int32.remainder` 8 == 7, Set#{7}))

    @Test
    def exists04(): Unit \ Assert = 
        assertTrue(Set.exists(x -> x `Int32.remainder` 8 == 7, Set#{15}))

    @Test
    def exists05(): Unit \ Assert = 
        assertFalse(Set.exists(x -> x `Int32.remainder` 8 == 7, Set#{1, 44}))

    @Test
    def exists06(): Unit \ Assert = 
        assertTrue(Set.exists(x -> x `Int32.remainder` 8 == 7, Set#{11, 71}))

    @Test
    def exists07(): Unit \ Assert = 
        assertTrue(Set.exists(x -> x `Int32.remainder` 8 == 7, Set#{71, 12}))

    @Test
    def exists08(): Unit \ Assert = 
        assertTrue(Set.exists(x -> x `Int32.remainder` 8 == 7, Set#{71, 79}))

    @Test
    def exists09(): Unit \ Assert = 
        assertFalse(Set.exists(x -> x `Int32.remainder` 8 == 7, Set#{11, -1, -14, -2, 84, 113}))

    @Test
    def exists10(): Unit \ Assert = 
        assertTrue(Set.exists(x -> x `Int32.remainder` 8 == 7, Set#{11, -1, 31, -14, -2, 84, 111}))

    @Test
    def exists11(): Unit \ Assert = 
        assertTrue(Set.exists(x -> x `Int32.remainder` 8 == 7, Set#{11, -1, -14, -2, 84, 111, 38}))

    /////////////////////////////////////////////////////////////////////////////
    // forAll                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forAll01(): Unit \ Assert = 
        assertTrue(Set.forAll(x -> x `Int32.remainder` 8 == 7, Set#{}))

    @Test
    def forAll02(): Unit \ Assert = 
        assertFalse(Set.forAll(x -> x `Int32.remainder` 8 == 7, Set#{5}))

    @Test
    def forAll03(): Unit \ Assert = 
        assertTrue(Set.forAll(x -> x `Int32.remainder` 8 == 7, Set#{7}))

    @Test
    def forAll04(): Unit \ Assert = 
        assertTrue(Set.forAll(x -> x `Int32.remainder` 8 == 7, Set#{15}))

    @Test
    def forAll05(): Unit \ Assert = 
        assertFalse(Set.forAll(x -> x `Int32.remainder` 8 == 7, Set#{1, 44}))

    @Test
    def forAll06(): Unit \ Assert = 
        assertFalse(Set.forAll(x -> x `Int32.remainder` 8 == 7, Set#{11, 71}))

    @Test
    def forAll07(): Unit \ Assert = 
        assertFalse(Set.forAll(x -> x `Int32.remainder` 8 == 7, Set#{71, 12}))

    @Test
    def forAll08(): Unit \ Assert = 
        assertTrue(Set.forAll(x -> x `Int32.remainder` 8 == 7, Set#{71, 79}))

    @Test
    def forAll09(): Unit \ Assert = 
        assertFalse(Set.forAll(x -> x `Int32.remainder` 8 == 7, Set#{7, 15, 23, 84, 111}))

    @Test
    def forAll10(): Unit \ Assert = 
        assertFalse(Set.forAll(x -> x `Int32.remainder` 8 == 7, Set#{7, 15, 23, 111, 3}))

    @Test
    def forAll11(): Unit \ Assert = 
        assertTrue(Set.forAll(x -> x `Int32.remainder` 8 == 7, Set#{7, 15, 23, 119, 111}))

    /////////////////////////////////////////////////////////////////////////////
    // union                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def union01(): Unit \ Assert = 
        assertEq(expected = Set#{}, Set.union((Set#{}: Set[Unit]), Set#{}))

    @Test
    def union02(): Unit \ Assert = 
        assertEq(expected = Set#{1}, Set.union(Set#{1}, Set#{}))

    @Test
    def union03(): Unit \ Assert = 
        assertEq(expected = Set#{2}, Set.union(Set#{}, Set#{2}))

    @Test
    def union04(): Unit \ Assert = 
        assertEq(expected = Set#{1}, Set.union(Set#{1}, Set#{1}))

    @Test
    def union05(): Unit \ Assert = 
        assertEq(expected = Set#{1, -1}, Set.union(Set#{1}, Set#{-1}))

    @Test
    def union06(): Unit \ Assert = 
        assertEq(expected = Set#{-1, 9}, Set.union(Set#{}, Set#{-1, 9}))

    @Test
    def union07(): Unit \ Assert = 
        assertEq(expected = Set#{-1, 9}, Set.union(Set#{9}, Set#{-1, 9}))

    @Test
    def union08(): Unit \ Assert = 
        assertEq(expected = Set#{4, -1, 9}, Set.union(Set#{4}, Set#{-1, 9}))

    @Test
    def union09(): Unit \ Assert = 
        assertEq(expected = Set#{-1, 9}, Set.union(Set#{9, -1}, Set#{-1, 9}))

    @Test
    def union10(): Unit \ Assert = 
        assertEq(expected = Set#{5, -1, 9}, Set.union(Set#{9, 5}, Set#{-1, 9}))

    @Test
    def union11(): Unit \ Assert = 
        assertEq(expected = Set#{6, 5, -1, 9}, Set.union(Set#{6, 5}, Set#{-1, 9}))

    @Test
    def union12(): Unit \ Assert = 
        assertEq(expected = Set#{6, 5, -1, 9, 43, 7, 8, -99}, Set.union(Set#{6, -99}, Set#{6, 5, -1, 9, 43, 7, 8, -99}))

    @Test
    def union13(): Unit \ Assert = 
        assertEq(expected = Set#{22, 6, 5, -1, 9, 43, 7, 8, -99}, Set.union(Set#{6, -99, -1, 5, 22}, Set#{6, 5, -1, 9, 43, 7, 8, -99}))

    @Test
    def union14(): Unit \ Assert = 
        assertEq(expected = Set#{-2, -3, -4, 6, 5, -1, 9, 43, 7, 8, -99}, Set.union(Set#{-2, -3, -4}, Set#{6, 5, -1, 9, 43, 7, 8, -99}))

    /////////////////////////////////////////////////////////////////////////////
    // intersection                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def intersection01(): Unit \ Assert = 
        assertEq(expected = Set#{}, Set.intersection((Set#{}: Set[Unit]), Set#{}))

    @Test
    def intersection02(): Unit \ Assert = 
        assertEq(expected = Set#{}, Set.intersection(Set#{1}, Set#{}))

    @Test
    def intersection03(): Unit \ Assert = 
        assertEq(expected = Set#{}, Set.intersection(Set#{}, Set#{2}))

    @Test
    def intersection04(): Unit \ Assert = 
        assertEq(expected = Set#{}, Set.intersection(Set#{1}, Set#{2}))

    @Test
    def intersection05(): Unit \ Assert = 
        assertEq(expected = Set#{1}, Set.intersection(Set#{1}, Set#{1}))

    @Test
    def intersection06(): Unit \ Assert = 
        assertEq(expected = Set#{}, Set.intersection(Set#{}, Set#{1, 2}))

    @Test
    def intersection07(): Unit \ Assert = 
        assertEq(expected = Set#{}, Set.intersection(Set#{1, 2}, Set#{}))

    @Test
    def intersection08(): Unit \ Assert = 
        assertEq(expected = Set#{2}, Set.intersection(Set#{2}, Set#{1, 2}))

    @Test
    def intersection09(): Unit \ Assert = 
        assertEq(expected = Set#{1}, Set.intersection(Set#{1}, Set#{1, 2}))

    @Test
    def intersection10(): Unit \ Assert = 
        assertEq(expected = Set#{2, 1}, Set.intersection(Set#{2, 1}, Set#{1, 2}))

    @Test
    def intersection11(): Unit \ Assert = 
        assertEq(expected = Set#{1, 2}, Set.intersection(Set#{1, 2}, Set#{1, 2}))

    @Test
    def intersection12(): Unit \ Assert = 
        assertEq(expected = Set#{2}, Set.intersection(Set#{3, 2}, Set#{1, 2}))

    @Test
    def intersection13(): Unit \ Assert = 
        assertEq(expected = Set#{}, Set.intersection(Set#{3, 55}, Set#{1, 2}))

    @Test
    def intersection14(): Unit \ Assert = 
        assertEq(expected = Set#{3, 87, -87}, Set.intersection(Set#{3, 55, 11, 87, 22, 34, -87, 23}, Set#{1, 2, 84, -87, 87, 3, 44}))

    @Test
    def intersection15(): Unit \ Assert = 
        assertEq(expected = Set#{3, 87, 23}, Set.intersection(Set#{3, 55, 11, 87, 22, 34, -87, 23}, Set#{23, 1, 2, 84, 87, 3}))

    /////////////////////////////////////////////////////////////////////////////
    // difference                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def difference01(): Unit \ Assert = 
        assertEq(expected = Set#{}, Set.difference((Set#{}: Set[Unit]), Set#{}))

    @Test
    def difference02(): Unit \ Assert = 
        assertEq(expected = Set#{}, Set.difference(Set#{}, Set#{2}))

    @Test
    def difference03(): Unit \ Assert = 
        assertEq(expected = Set#{1}, Set.difference(Set#{1}, Set#{}))

    @Test
    def difference04(): Unit \ Assert = 
        assertEq(expected = Set#{1}, Set.difference(Set#{1}, Set#{2}))

    @Test
    def difference05(): Unit \ Assert = 
        assertEq(expected = Set#{}, Set.difference(Set#{1}, Set#{1}))

    @Test
    def difference06(): Unit \ Assert = 
        assertEq(expected = Set#{1, 2}, Set.difference(Set#{1, 2}, Set#{}))

    @Test
    def difference07(): Unit \ Assert = 
        assertEq(expected = Set#{2}, Set.difference(Set#{1, 2}, Set#{1}))

    @Test
    def difference08(): Unit \ Assert = 
        assertEq(expected = Set#{1}, Set.difference(Set#{1, 2}, Set#{2}))

    @Test
    def difference09(): Unit \ Assert = 
        assertEq(expected = Set#{1}, Set.difference(Set#{1, 2}, Set#{8, 2, 4}))

    @Test
    def difference10(): Unit \ Assert = 
        assertEq(expected = Set#{}, Set.difference(Set#{1, 2}, Set#{3, 1, 2, 4}))

    @Test
    def difference11(): Unit \ Assert = 
        assertEq(expected = Set#{1, 2}, Set.difference(Set#{1, 2}, Set#{3, 11, 21, 4}))

    @Test
    def difference12(): Unit \ Assert = 
        assertEq(expected = Set#{87, 6, 111}, Set.difference(Set#{1, 2, 87, 4, 5, 6, 86, 92, 111, -1}, Set#{-1, 92, 4, 5, 1, 2, 86}))

    @Test
    def difference13(): Unit \ Assert = 
        assertEq(expected = Set#{87, 111}, Set.difference(Set#{1, 2, 87, 4, 5, 6, 86, 92, 111, -1}, Set#{-1, 92, 4, 5, 1, 2, 86, 99, 6}))

    /////////////////////////////////////////////////////////////////////////////
    // subsets                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def subsets01(): Unit \ Assert = 
        assertEq(expected = Set#{Set#{}}, Set.subsets((Set#{}: Set[Unit])))

    @Test
    def subsets02(): Unit \ Assert = 
        assertEq(expected = Set#{Set#{1}, Set#{}}, Set.subsets(Set#{1}))

    @Test
    def subsets03(): Unit \ Assert = 
        assertEq(expected = Set#{Set#{1, 2}, Set#{1}, Set#{2}, Set#{}}, Set.subsets(Set#{1, 2}))

    @Test
    def subsets04(): Unit \ Assert = 
        assertEq(expected = Set#{Set#{1, 2, 3}, Set#{1, 2}, Set#{1, 3}, Set#{1}, Set#{2, 3}, Set#{2}, Set#{3}, Set#{}}, Set.subsets(Set#{1, 2, 3}))

    /////////////////////////////////////////////////////////////////////////////
    // filter                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def filter01(): Unit \ Assert = 
        assertEq(expected = Set#{}, Set.filter(x -> x `Int32.remainder` 2 == 0, Set#{}))

    @Test
    def filter02(): Unit \ Assert = 
        assertEq(expected = Set#{}, Set.filter(x -> x `Int32.remainder` 2 == 0, Set#{1}))

    @Test
    def filter03(): Unit \ Assert = 
        assertEq(expected = Set#{2}, Set.filter(x -> x `Int32.remainder` 2 == 0, Set#{2}))

    @Test
    def filter04(): Unit \ Assert = 
        assertEq(expected = Set#{}, Set.filter(x -> x `Int32.remainder` 2 == 0, Set#{1, 3}))

    @Test
    def filter05(): Unit \ Assert = 
        assertEq(expected = Set#{8}, Set.filter(x -> x `Int32.remainder` 2 == 0, Set#{8, 3}))

    @Test
    def filter06(): Unit \ Assert = 
        assertEq(expected = Set#{32}, Set.filter(x -> x `Int32.remainder` 2 == 0, Set#{-1, 32}))

    @Test
    def filter07(): Unit \ Assert = 
        assertEq(expected = Set#{12, 34}, Set.filter(x -> x `Int32.remainder` 2 == 0, Set#{12, 34}))

    @Test
    def filter08(): Unit \ Assert = 
        assertEq(expected = Set#{12, 34, 88}, Set.filter(x -> x `Int32.remainder` 2 == 0, Set#{-33, -1, 12, 1, 34, 88, 7, 77, 31}))

    @Test
    def filter09(): Unit \ Assert = 
        assertEq(expected = Set#{12, 34, 88, -92}, Set.filter(x -> x `Int32.remainder` 2 == 0, Set#{-33, -1, 12, 1, 34, 88, 7, 77, 31, 7, -92, 841}))

    /////////////////////////////////////////////////////////////////////////////
    // map                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def map01(): Unit \ Assert = 
        assertEq(expected = Set#{}, Set.map(x -> x `Int32.remainder` 2 == 0, Set#{}))

    @Test
    def map02(): Unit \ Assert = 
        assertEq(expected = Set#{false}, Set.map(x -> x `Int32.remainder` 2 == 0, Set#{1}))

    @Test
    def map03(): Unit \ Assert = 
        assertEq(expected = Set#{true}, Set.map(x -> x `Int32.remainder` 2 == 0, Set#{2}))

    @Test
    def map04(): Unit \ Assert = 
        assertEq(expected = Set#{false}, Set.map(x -> x `Int32.remainder` 2 == 0, Set#{1, -1}))

    @Test
    def map05(): Unit \ Assert = 
        assertEq(expected = Set#{false, true}, Set.map(x -> x `Int32.remainder` 2 == 0, Set#{1, -12}))

    @Test
    def map06(): Unit \ Assert = 
        assertEq(expected = Set#{true, false}, Set.map(x -> x `Int32.remainder` 2 == 0, Set#{16, -1}))

    @Test
    def map07(): Unit \ Assert = 
        assertEq(expected = Set#{true}, Set.map(x -> x `Int32.remainder` 2 == 0, Set#{12, -12}))

    @Test
    def map08(): Unit \ Assert = 
        assertEq(expected = Set#{true}, Set.map(x -> x `Int32.remainder` 2 == 0, Set#{12, -12}))

    @Test
    def map09(): Unit \ Assert = 
        assertEq(expected = Set#{false, true}, Set.map(x -> x `Int32.remainder` 2 == 0, Set#{12, -12, 1, 14}))

    @Test
    def map10(): Unit \ Assert = 
        assertEq(expected = Set#{true, false}, Set.map(x -> x `Int32.remainder` 2 == 0, Set#{12, -12, 1, 14, 7, 88, -91}))

    @Test
    def map11(): Unit \ Assert = 
        assertEq(expected = Set#{true}, Set.map(x -> x `Int32.remainder` 2 == 0, Set#{12, -12, 122, 14}))

    @Test
    def map12(): Unit \ Assert = 
        assertEq(expected = Set#{false}, Set.map(x -> x `Int32.remainder` 2 == 0, Set#{123, -123, 1223, 141}))

    @Test
    def map13(): Unit \ Assert = 
        assertEq(expected = Set#{2, 5, 7, 4}, Set.map(x -> x `Int32.remainder` 9, Set#{11, 5, 16, 4}))

    @Test
    def map14(): Unit \ Assert = 
        assertEq(expected = Set#{5, 1, 0, -8}, Set.map(x -> x `Int32.remainder` 9, Set#{0, 5, 1, -9, -8}))

    @Test
    def map15(): Unit \ Assert = 
        assertEq(expected = Set#{0, 5, 7, 1, 2}, Set.map(x -> x `Int32.remainder` 9, Set#{0, 5, 1, 10, 7, 19, 28, 2}))

    @Test
    def map16(): Unit \ Assert = region rc {
        let range = List.range(0, 2000);
        let s = List.toSet(range);
        let a = MutList.empty(rc);
        discard Set.map(x -> { let b = MutList.empty(rc); MutList.push(x, b); MutList.append(MutList.toList(b), a); x }, s);
        assertEq(expected = range, MutList.toList(a))
    }


    /////////////////////////////////////////////////////////////////////////////
    // flatMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def flatMap01(): Unit \ Assert = 
        assertEq(expected = Set#{}, Set.flatMap(x -> if (x `Int32.remainder` 2 == 0) Set#{} else Set#{x}, Set#{}))

    @Test
    def flatMap02(): Unit \ Assert = 
        assertEq(expected = Set#{}, Set.flatMap(x -> if (x `Int32.remainder` 2 == 0) Set#{} else Set#{x}, Set#{2}))

    @Test
    def flatMap03(): Unit \ Assert = 
        assertEq(expected = Set#{1}, Set.flatMap(x -> if (x `Int32.remainder` 2 == 0) Set#{} else Set#{x}, Set#{1}))

    @Test
    def flatMap04(): Unit \ Assert = 
        assertEq(expected = Set#{}, Set.flatMap(x -> if (x `Int32.remainder` 2 == 0) Set#{} else Set#{x}, Set#{2, 4}))

    @Test
    def flatMap05(): Unit \ Assert = 
        assertEq(expected = Set#{13}, Set.flatMap(x -> if (x `Int32.remainder` 2 == 0) Set#{} else Set#{x}, Set#{2, 13}))

    @Test
    def flatMap06(): Unit \ Assert = 
        assertEq(expected = Set#{15}, Set.flatMap(x -> if (x `Int32.remainder` 2 == 0) Set#{} else Set#{x}, Set#{15, -8}))

    @Test
    def flatMap07(): Unit \ Assert = 
        assertEq(expected = Set#{1, 11}, Set.flatMap(x -> if (x `Int32.remainder` 2 == 0) Set#{} else Set#{x}, Set#{1, 11}))

    @Test
    def flatMap08(): Unit \ Assert = 
        assertEq(expected = Set#{1, 8, 16, 2, 4}, Set.flatMap(x -> Set#{x, 2*x}, Set#{1, 4, 8, 2}))

    @Test
    def flatMap09(): Unit \ Assert = 
        assertEq(expected = Set#{1, 8, 24, 3, 2, 6, 9, 27, -5, -15, -1, -3}, Set.flatMap(x -> Set#{x, 3*x}, Set#{1, 8, 3, 2, 9, -5, -1}))

    @Test
    def flatMap10(): Unit \ Assert = 
        assertEq(expected = Set#{1, 8, 12, 3, 9, 2, 4, 6}, Set.flatMap(x -> Set#{x, 2*x, 3*x}, Set#{1, 4, 3, 2}))

    /////////////////////////////////////////////////////////////////////////////
    // filterMap                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def filterMap01(): Unit \ Assert = 
        assertEq(expected = Set.empty(), Set.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, Set.empty()))

    @Test
    def filterMap02(): Unit \ Assert = 
        assertEq(expected = Set#{}, Set.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, Set#{1}))

    @Test
    def filterMap03(): Unit \ Assert = 
        assertEq(expected = Set#{1}, Set.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, Set#{2}))

    @Test
    def filterMap04(): Unit \ Assert = 
        assertEq(expected = Set#{}, Set.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, Set#{1, 3}))

    @Test
    def filterMap05(): Unit \ Assert = 
        assertEq(expected = Set#{2}, Set.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, Set#{1, 4}))

    @Test
    def filterMap06(): Unit \ Assert = 
        assertEq(expected = Set#{3}, Set.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, Set#{-1, 6}))

    @Test
    def filterMap07(): Unit \ Assert = 
        assertEq(expected = Set#{3, 4}, Set.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, Set#{6, 8}))

    @Test
    def filterMap08(): Unit \ Assert = {
        let s1 = Set#{0, 1, 2, 3, 4, 5, 10};
        let s2 = Set#{0, 1, 2, 5};
        assertEq(expected = s2, Set.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, s1))
    }

    /////////////////////////////////////////////////////////////////////////////
    // replace                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def replace01(): Unit \ Assert = 
        assertEq(expected = Set#{}, Set.replace(src = 3, dst = 4, Set#{}))

    @Test
    def replace02(): Unit \ Assert = 
        assertEq(expected = Set#{1}, Set.replace(src = 3, dst = 4, Set#{1}))

    @Test
    def replace03(): Unit \ Assert = 
        assertEq(expected = Set#{4}, Set.replace(src = 3, dst = 4, Set#{3}))

    @Test
    def replace04(): Unit \ Assert = 
        assertEq(expected = Set#{4}, Set.replace(src = 3, dst = 4, Set#{4}))

    @Test
    def replace05(): Unit \ Assert = 
        assertEq(expected = Set#{1, 2}, Set.replace(src = 3, dst = 4, Set#{1, 2}))

    @Test
    def replace06(): Unit \ Assert = 
        assertEq(expected = Set#{1, 4}, Set.replace(src = 3, dst = 4, Set#{1, 3}))

    @Test
    def replace07(): Unit \ Assert = 
        assertEq(expected = Set#{4, 2}, Set.replace(src = 3, dst = 4, Set#{3, 2}))

    @Test
    def replace08(): Unit \ Assert = 
        assertEq(expected = Set#{4}, Set.replace(src = 3, dst = 4, Set#{3, 4}))

    /////////////////////////////////////////////////////////////////////////////
    // partition                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def partition01(): Unit \ Assert = 
        assertEq(expected = (Set#{}, Set#{}), Set.partition(x -> x `Int32.remainder` 2 == 0, Set#{}))

    @Test
    def partition02(): Unit \ Assert = 
        assertEq(expected = (Set#{}, Set#{1}), Set.partition(x -> x `Int32.remainder` 2 == 0, Set#{1}))

    @Test
    def partition03(): Unit \ Assert = 
        assertEq(expected = (Set#{2}, Set#{}), Set.partition(x -> x `Int32.remainder` 2 == 0, Set#{2}))

    @Test
    def partition04(): Unit \ Assert = 
        assertEq(expected = (Set#{}, Set#{1, 3}), Set.partition(x -> x `Int32.remainder` 2 == 0, Set#{1, 3}))

    @Test
    def partition05(): Unit \ Assert = 
        assertEq(expected = (Set#{2}, Set#{1}), Set.partition(x -> x `Int32.remainder` 2 == 0, Set#{1, 2}))

    @Test
    def partition06(): Unit \ Assert = 
        assertEq(expected = (Set#{2}, Set#{1}), Set.partition(x -> x `Int32.remainder` 2 == 0, Set#{2, 1}))

    @Test
    def partition07(): Unit \ Assert = 
        assertEq(expected = (Set#{2, -4}, Set#{}), Set.partition(x -> x `Int32.remainder` 2 == 0, Set#{2, -4}))

    @Test
    def partition08(): Unit \ Assert = 
        assertEq(expected = (Set#{2, -4, -6, 84}, Set#{-11, 89, 11}), Set.partition(x -> x `Int32.remainder` 2 == 0, Set#{2, -11, 89, -4, 11, -6, 84}))

    @Test
    def partition09(): Unit \ Assert = 
        assertEq(expected = (Set#{84, -6, -4, 2}, Set#{11, 89, -11}), Set.partition(x -> x `Int32.remainder` 2 == 0, Set#{84, -6, 11, -4, 89, -11, 2}))

    @Test
    def partition10(): Unit \ Assert = 
        assertEq(expected = (Set#{2, 4, 6, 8}, Set#{1, 3, 5, 7}), Set.partition(x -> x `Int32.remainder` 2 == 0, Set#{1, 2, 3, 4, 5, 6, 7, 8}))

    /////////////////////////////////////////////////////////////////////////////
    // toList                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toList01(): Unit \ Assert = 
        assertEq(expected = Nil, Set.toList((Set#{}: Set[Unit])))

    @Test
    def toList02(): Unit \ Assert = 
        assertEq(expected = 1 :: Nil, Set.toList(Set#{1}))

    @Test
    def toList03(): Unit \ Assert = 
        assertEq(expected = 1 :: 2 :: Nil, Set.toList(Set#{1, 2}))

    @Test
    def toList04(): Unit \ Assert = 
        assertEq(expected = 1 :: 2 :: 3 :: Nil, Set.toList(Set#{1, 2, 3}))

    /////////////////////////////////////////////////////////////////////////////
    // toArray                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toArray01(): Unit \ Assert = region rc {
        assertTrue(Array.sameElements(Set.toArray(rc, (Set#{}: Set[Unit])), (Array#{} @ rc)))
    }

    @Test
    def toArray02(): Unit \ Assert = region rc {
        assertTrue(Array.sameElements(Set.toArray(rc, Set#{1}), Array#{1} @ rc))
    }

    @Test
    def toArray03(): Unit \ Assert = region rc {
        assertTrue(Array.sameElements(Set.toArray(rc, Set#{1, 2}), Array#{1, 2} @ rc))
    }

    @Test
    def toArray04(): Unit \ Assert = region rc {
        assertTrue(Array.sameElements(Set.toArray(rc, Set#{1, 2, 3}), Array#{1, 2, 3} @ rc))
    }

    @Test
    def toArray05(): Unit \ Assert = region rc {
        assertTrue(Array.sameElements(Set.toArray(rc, Set#{3, 2, 1}), Array#{1, 2, 3} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // toVector                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toVector01(): Unit \ Assert = 
        assertEq(expected = Vector.empty(), Set.toVector((Set#{}: Set[Unit])))

    @Test
    def toVector02(): Unit \ Assert = 
        assertEq(expected = Vector#{1}, Set.toVector(Set#{1}))

    @Test
    def toVector03(): Unit \ Assert = 
        assertEq(expected = Vector#{1, 2}, Set.toVector(Set#{1, 2}))

    @Test
    def toVector04(): Unit \ Assert = 
        assertEq(expected = Vector#{1, 2, 3}, Set.toVector(Set#{1, 2, 3}))

    @Test
    def toVector05(): Unit \ Assert = 
        assertEq(expected = Vector#{1, 2, 3}, Set.toVector(Set#{3, 2, 1}))

    /////////////////////////////////////////////////////////////////////////////
    // toChain                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toChain01(): Unit \ Assert = 
        assertEq(expected = (Chain.empty(): Chain[Int32]), Set.toChain((Set#{}: Set[Int32])))

    @Test
    def toChain02(): Unit \ Assert = 
        assertEq(expected = Chain.singleton(1), Set.toChain(Set#{1}))

    @Test
    def toChain03(): Unit \ Assert = 
        assertEq(expected = List.toChain(1 :: 2 :: Nil), Set.toChain(Set#{1, 2}))

    @Test
    def toChain04(): Unit \ Assert = 
        assertEq(expected = List.toChain(1 :: 2 :: 3 :: Nil), Set.toChain(Set#{1, 2, 3}))

    /////////////////////////////////////////////////////////////////////////////
    // toMap                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toMap01(): Unit \ Assert = 
        assertEq(expected = Map#{}, Set.toMap((Set#{}: Set[(Unit, Unit)])))

    @Test
    def toMap02(): Unit \ Assert = 
        assertEq(expected = Map#{1 => true}, Set.toMap(Set#{(1, true)}))

    @Test
    def toMap03(): Unit \ Assert = 
        assertEq(expected = Map#{1 => true, 2 => false}, Set.toMap(Set#{(1, true), (2, false)}))

    @Test
    def toMap04(): Unit \ Assert = 
        assertEq(expected = Map#{1 => false}, Set.toMap(Set#{(1, true), (1, false)}))

    @Test
    def toMap05(): Unit \ Assert = 
        assertEq(expected = Map#{1 => true, 2 => false, 3 => true}, Set.toMap(Set#{(1, true), (2, false), (3, true)}))


    /////////////////////////////////////////////////////////////////////////////
    // toMapWith                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toMapWith02(): Unit \ Assert = 
        assertEq(expected = Map#{}, Set.toMapWith(x -> 0 * x, Set#{}))

    @Test
    def toMapWith03(): Unit \ Assert = 
        assertEq(expected = Map#{1 => 2, 3 => 6, 6 => 12}, Set.toMapWith(x -> 2 * x, Set#{1, 3, 6}))

    @Test
    def toMapWith04(): Unit \ Assert = 
        assertEq(expected = Map#{1 => 11, 3 => 13, 6 => 16}, Set.toMapWith(x -> x + 10, Set#{1, 3, 6}))

    @Test
    def toMapWith05(): Unit \ Assert = 
        assertEq(expected = Map#{1 => 11, 3 => 13, 6 => 16, 7 => 17}, Set.toMapWith(x -> x + 10, Set#{7, 1, 3, 6}))

    /////////////////////////////////////////////////////////////////////////////
    // toMutDeque                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toMutDeque01(): Unit \ Assert = region rc {
        let s: Set[Int32] = Set#{};
        let d1 = Set.toMutDeque(rc, s);

        let d2 = MutDeque.empty(rc);

        assertTrue(d1 `MutDeque.sameElements` d2)
    }

    @Test
    def toMutDeque02(): Unit \ Assert = region rc {
        let s = Set#{1};
        let d1 = Set.toMutDeque(rc, s);

        let d2 = MutDeque.empty(rc);
        MutDeque.pushBack(1, d2);

        assertTrue(d1 `MutDeque.sameElements` d2)
    }

    @Test
    def toMutDeque03(): Unit \ Assert = region rc {
        let s = Set#{1, 3, 6};
        let d1 = Set.toMutDeque(rc, s);

        let d2 = MutDeque.empty(rc);
        MutDeque.pushBack(3, d2);
        MutDeque.pushBack(6, d2);
        MutDeque.pushFront(1, d2);

        assertTrue(d1 `MutDeque.sameElements` d2)
    }

    @Test
    def toMutDeque04(): Unit \ Assert = region rc {
        let s = Set#{7, 1, 3, 6};
        let d1 = Set.toMutDeque(rc, s);

        let d2 = MutDeque.empty(rc);
        MutDeque.pushFront(7, d2);
        MutDeque.pushFront(6, d2);
        MutDeque.pushFront(3, d2);
        MutDeque.pushFront(1, d2);

        assertTrue(d1 `MutDeque.sameElements` d2)
    }

    /////////////////////////////////////////////////////////////////////////////
    // eq                                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def eq01(): Unit \ Assert = 
        assertTrue((Set#{}: Set[Unit]) == Set#{})

    @Test
    def eq02(): Unit \ Assert = 
        assertTrue(Set#{1} != Set#{})

    @Test
    def eq03(): Unit \ Assert = 
        assertTrue(Set#{} != Set#{1})

    @Test
    def eq04(): Unit \ Assert = 
        assertTrue(Set#{1, 2} != Set#{})

    @Test
    def eq05(): Unit \ Assert = 
        assertTrue(Set#{} != Set#{1, 2})

    @Test
    def eq06(): Unit \ Assert = 
        assertTrue(Set#{1} != Set#{2})

    @Test
    def eq07(): Unit \ Assert = 
        assertTrue(Set#{1} == Set#{1})

    @Test
    def eq08(): Unit \ Assert = 
        assertTrue(Set#{1, 2, 3} != Set#{})

    @Test
    def eq09(): Unit \ Assert = 
        assertTrue(Set#{1, 2} != Set#{1})

    @Test
    def eq10(): Unit \ Assert = 
        assertTrue(Set#{1} != Set#{1, 2})

    @Test
    def eq11(): Unit \ Assert = 
        assertTrue(Set#{} != Set#{1, 2, 3})

    @Test
    def eq12(): Unit \ Assert = 
        assertTrue(Set#{1, 2} != Set#{1, 3})

    @Test
    def eq13(): Unit \ Assert = 
        assertTrue(Set#{1, 2} == Set#{2, 1})

    @Test
    def eq14(): Unit \ Assert = 
        assertTrue(Set#{1, 2} == Set#{1, 2})

    @Test
    def eq15(): Unit \ Assert = 
        assertTrue(Set#{1, 2, 3} != Set#{1, 2, 4})

    @Test
    def eq16(): Unit \ Assert = 
        assertTrue(Set#{1, 2, 3} == Set#{1, 2, 3})

    @Test
    def eq17(): Unit \ Assert = 
        assertTrue(Set#{1, 2, 3} == Set#{2, 3, 1})

    @Test
    def eq18(): Unit \ Assert = 
        assertTrue(Set#{1, 2, 3} == Set#{3, 1, 2})

    @Test
    def eq19(): Unit \ Assert = 
        assertTrue(Set#{1, 2, 3} == Set#{2, 1, 3})

    /////////////////////////////////////////////////////////////////////////////
    // forEach                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forEach01(): Unit \ Assert = region rc {
        let ri = Ref.fresh(rc, 21);
        Set.forEach(x -> Ref.put(x, ri), Set#{});
        assertEq(expected = 21, Ref.get(ri))
    }

    @Test
    def forEach02(): Unit \ Assert = region rc {
        let ri = Ref.fresh(rc, 21);
        Set.forEach(x -> Ref.put(x, ri), Set#{42});
        assertEq(expected = 42, Ref.get(ri))
    }

    /////////////////////////////////////////////////////////////////////////////
    // forEachWithIndex                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forEachWithIndex01(): Unit \ Assert = region rc {
        let ri = Ref.fresh(rc, 21);
        Set.forEachWithIndex((i, _) -> Ref.put(i, ri), Set#{});
        assertEq(expected = 21, Ref.get(ri))
    }

    @Test
    def forEachWithIndex02(): Unit \ Assert = region rc {
        let ri = Ref.fresh(rc, 21);
        Set.forEachWithIndex((i, _) -> Ref.put(i, ri), Set#{42});
        assertEq(expected = 0, Ref.get(ri))
    }

    /////////////////////////////////////////////////////////////////////////////
    // unfold                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def unfold01(): Unit \ Assert = 
        assertEq(expected = Set#{}, Set.unfold(s -> if (true) None else Some((s + 48, s + 1)), 0))

    @Test
    def unfold02(): Unit \ Assert = 
        assertEq(expected = Set#{48}, Set.unfold(s -> if (s > 0) None else Some((s + 48, s + 1)), 0))

    @Test
    def unfold03(): Unit \ Assert = 
        assertEq(expected = Set#{48, 49}, Set.unfold(s -> if (s > 1) None else Some((s + 48, s + 1)), 0))

    @Test
    def unfold04(): Unit \ Assert = 
        assertEq(expected = Set#{48, 49, 50, 51, 52, 53, 54, 55, 56, 57}, Set.unfold(s -> if (s >= 10) None else Some((s + 48, s + 1)), 0))

    @Test
    def unfold05(): Unit \ Assert = 
        assertEq(expected = Set#{53, 54, 55, 56, 57}, Set.unfold(s -> if (s >= 10) None else Some((s + 48, s + 1)), 5))

    @Test
    def unfold06(): Unit \ Assert = 
        assertEq(expected = Set#{48, 50, 52, 54, 56}, Set.unfold(s -> if (s >= 10) None else Some((s + 48, s + 2)), 0))

    /////////////////////////////////////////////////////////////////////////////
    // unfoldWithIter                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def unfoldWithIter01(): Unit \ Assert = region rc {
        let x = Ref.fresh(rc, 0);
        let step = () ->
            if (true)
                None
            else {
                let c = Some(Ref.get(x) + 48);
                Ref.put(Ref.get(x) + 1, x);
                c
            };
        assertEq(expected = Set#{}, Set.unfoldWithIter(step))
    }

    @Test
    def unfoldWithIter02(): Unit \ Assert = region rc {
        let x = Ref.fresh(rc, 0);
        let step = () ->
            if (Ref.get(x) > 0)
                None
            else {
                let c = Some(Ref.get(x) + 48);
                Ref.put(Ref.get(x) + 1, x);
                c
            };
        assertEq(expected = Set#{48}, Set.unfoldWithIter(step))
    }

    @Test
    def unfoldWithIter03(): Unit \ Assert = region rc {
        let x = Ref.fresh(rc, 0);
        let step = () ->
            if (Ref.get(x) > 1)
                None
            else {
                let c = Some(Ref.get(x) + 48);
                Ref.put(Ref.get(x) + 1, x);
                c
            };
        assertEq(expected = Set#{48, 49}, Set.unfoldWithIter(step))
    }

    @Test
    def unfoldWithIter04(): Unit \ Assert = region rc {
        let x = Ref.fresh(rc, 0);
        let step = () ->
            if (Ref.get(x) >= 10)
                None
            else {
                let c = Some(Ref.get(x) + 48);
                Ref.put(Ref.get(x) + 1, x);
                c
            };
        assertEq(expected = Set#{48, 49, 50, 51, 52, 53, 54, 55, 56, 57}, Set.unfoldWithIter(step))
    }

    @Test
    def unfoldWithIter05(): Unit \ Assert = region rc {
        let x = Ref.fresh(rc, 5);
        let step = () ->
            if (Ref.get(x) >= 10)
                None
            else {
                let c = Some(Ref.get(x) + 48);
                Ref.put(Ref.get(x) + 1, x);
                c
            };
        assertEq(expected = Set#{53, 54, 55, 56, 57}, Set.unfoldWithIter(step))
    }

    @Test
    def unfoldWithIter06(): Unit \ Assert = region rc {
        let x = Ref.fresh(rc, 0);
        let step = () ->
            if (Ref.get(x) >= 10)
                None
            else {
                let c = Some(Ref.get(x) + 48);
                Ref.put(Ref.get(x) + 2, x);
                c
            };
        assertEq(expected = Set#{48, 50, 52, 54, 56}, Set.unfoldWithIter(step))
    }

    /////////////////////////////////////////////////////////////////////////////
    // toString                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toString01(): Unit \ Assert = 
        assertEq(expected = "Set#{1}", toString(Set#{1}))

    @Test
    def toString02(): Unit \ Assert = 
        assertEq(expected = "Set#{1, 2, 3}", toString(Set#{1, 2, 3}))

    @Test
    def toString03(): Unit \ Assert = 
        assertEq(expected = "Set#{1, 2}", toString(Set#{1, 2}))

    @Test
    def toString04(): Unit \ Assert = 
        assertEq(expected = "Set#{0, 2, 3, 4, 97}", toString(Set#{97, 2, 3, 4, 0}))

    @Test
    def toString05(): Unit \ Assert = 
        assertEq(expected = "Set#{Set#{1, 2}, Set#{4, 6}}", toString(Set#{Set#{1, 2}, Set#{4, 6}}))

    /////////////////////////////////////////////////////////////////////////////
    // hash                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def hash01(): Unit \ Assert = 
        assertEq(expected = hash((Set#{}: Set[Unit])), hash((Set#{}: Set[Unit])))

    @Test
    def hash02(): Unit \ Assert = 
        assertEq(expected = hash(Set#{1, 2}), hash(Set#{1, 2}))

    @Test
    def hash03(): Unit \ Assert = 
        assertEq(expected = hash(Set#{'a', 'b', 'c', 'd'}), hash(Set#{'a', 'b', 'c', 'd'}))

    @Test
    def hash04(): Unit \ Assert = 
        assertTrue(hash(Set#{'a', 'b', 'c', 'e'}) != hash(Set#{'a', 'b', 'c', 'd'}))

    @Test
    def hash05(): Unit \ Assert = 
        assertTrue(hash(Set#{false}) != hash(Set#{true}))

    /////////////////////////////////////////////////////////////////////////////
    // minimum                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def minimum01(): Unit \ Assert = 
        assertEq(expected = None, Set.minimum((Set#{}: Set[Int32])))

    @Test
    def minimum02(): Unit \ Assert = 
        assertEq(expected = Some(1), Set.minimum(Set#{1}))

    @Test
    def minimum03(): Unit \ Assert = 
        assertEq(expected = Some(0), Set.minimum(Set#{1, 2, 3, 0}))

    @Test
    def minimum04(): Unit \ Assert = 
        assertEq(expected = Some(5), Set.minimum(Set.range(5, 55)))

    /////////////////////////////////////////////////////////////////////////////
    // minimumBy                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def minimumBy01(): Unit \ Assert = 
        assertEq(expected = None, Set.minimumBy((x, y) -> x <=> y, (Set#{}: Set[Int32])))

    @Test
    def minimumBy02(): Unit \ Assert = 
        assertEq(expected = Some(1), Set.minimumBy((x, y) -> x <=> y, Set#{1}))

    @Test
    def minimumBy03(): Unit \ Assert = 
        assertEq(expected = Some(0), Set.minimumBy((x, y) -> x <=> y, Set#{1, 2, 3, 0}))

    @Test
    def minimumBy04(): Unit \ Assert = 
        assertEq(expected = Some(5), Set.minimumBy((x, y) -> x <=> y, Set.range(5, 55)))

    @Test
    def minimumBy05(): Unit \ Assert = region rc {
        let s = List.range(0, 2000) |> List.toSet;
        let a = MutList.empty(rc);
        discard Set.minimumBy((x, y) -> { let b = MutList.empty(rc); MutList.push(y, b); MutList.append(MutList.toList(b), a); x <=> y }, s);
        assertEq(expected = List.range(1, 2000), MutList.toList(a)) // The first / left most value is ignored
    }


    /////////////////////////////////////////////////////////////////////////////
    // maximum                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def maximum01(): Unit \ Assert = 
        assertEq(expected = None, Set.maximum((Set#{}: Set[Int32])))

    @Test
    def maximum02(): Unit \ Assert = 
        assertEq(expected = Some(1), Set.maximum(Set#{1}))

    @Test
    def maximum03(): Unit \ Assert = 
        assertEq(expected = Some(3), Set.maximum(Set#{1, 2, 3, 0}))

    @Test
    def maximum04(): Unit \ Assert = 
        assertEq(expected = Some(54), Set.maximum(Set.range(5, 55)))

    /////////////////////////////////////////////////////////////////////////////
    // maximumBy                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def maximumBy01(): Unit \ Assert = 
        assertEq(expected = None, Set.maximumBy((x, y) -> x <=> y, (Set#{}: Set[Int32])))

    @Test
    def maximumBy02(): Unit \ Assert = 
        assertEq(expected = Some(1), Set.maximumBy((x, y) -> x <=> y, Set#{1}))

    @Test
    def maximumBy03(): Unit \ Assert = 
        assertEq(expected = Some(3), Set.maximumBy((x, y) -> x <=> y, Set#{1, 2, 3, 0}))

    @Test
    def maximumBy04(): Unit \ Assert = 
        assertEq(expected = Some(54), Set.maximumBy((x, y) -> x <=> y, Set.range(5, 55)))


    /////////////////////////////////////////////////////////////////////////////
    // toDelayList                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toDelayList01(): Unit \ Assert = 
        assertEq(expected = DelayList.empty(), (Set.empty(): Set[Unit]) |> Set.toDelayList)

    @Test
    def toDelayList02(): Unit \ Assert = 
        assertEq(expected = DelayList.range(-1000, 1000), Set.range(-1000, 1000) |> Set.toDelayList)

    @Test
    def toDelayList03(): Unit \ Assert = 
        assertEq(expected = Set.range(-1000, 1000), Set.range(-1000, 1000) |> Set.toDelayList |> DelayList.toSet)


    /////////////////////////////////////////////////////////////////////////////
    // iterator                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def iterator01(): Unit \ Assert = region rc {
        assertEq(expected = Set.empty(), (Set.empty(): Set[Int32]) |> Set.iterator(rc) |> Iterator.toSet)
    }

    @Test
    def iterator02(): Unit \ Assert = region rc {
        assertEq(expected = Set#{1, 2, 3, 4}, Set#{1, 2, 3, 4} |> Set.iterator(rc) |> Iterator.toSet)
    }

    @Test
    def iterator03(): Unit \ Assert = region rc {
        assertEq(expected = Set.range(0, 100), Set.range(0, 100) |> Set.iterator(rc) |> Iterator.toSet)
    }


    /////////////////////////////////////////////////////////////////////////////
    // join                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def join01(): Unit \ Assert = 
        assertEq(expected = "", Set.join(",", (Set.empty(): Set[Int32])))

    @Test
    def join02(): Unit \ Assert = 
        assertEq(expected = "1", Set.join(",", Set#{1}))

    @Test
    def join03(): Unit \ Assert = 
        assertEq(expected = "1,2,3", Set.join(",", Set#{1, 2, 3}))

    @Test
    def join04(): Unit \ Assert = 
        assertEq(expected = "1,2,3", Set.join(",", Set#{"1", "2", "3"}))


    /////////////////////////////////////////////////////////////////////////////
    // joinWith                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def joinWith01(): Unit \ Assert = 
        assertEq(expected = "", (Nil: List[Int32]) |> List.toSet |> Set.joinWith(x -> "${x + 1}", ","))

    @Test
    def joinWith02(): Unit \ Assert = 
        assertEq(expected = "2", (1 :: Nil) |> List.toSet |> Set.joinWith(x -> "${x + 1}", ","))

    @Test
    def joinWith03(): Unit \ Assert = 
        assertEq(expected = "2,3,4", (1 :: 2 :: 3 :: Nil) |> List.toSet |> Set.joinWith(x -> "${x + 1}", ","))

    @Test
    def joinWith04(): Unit \ Assert = 
        assertEq(expected = "11,22,33", ("1" :: "2" :: "3" :: Nil) |> List.toSet |> Set.joinWith(x -> x + x, ","))

}


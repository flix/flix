mod TestUnorderedFoldable {

    use Assert.{assertEq, assertTrue, assertFalse}

    // Most tests are copied over from the `Set` test suite
    // and optionally adapted to types with a `CommutativeMonoid` instance.

    /////////////////////////////////////////////////////////////////////////////
    // fold                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def fold01(): Unit \ Assert =
        assertEq(expected = (), UnorderedFoldable.fold((Set#{}: Set[Unit])))

    @Test
    def fold02(): Unit \ Assert =
        assertEq(expected = 0, UnorderedFoldable.fold((Set#{}: Set[Int32])))

    @Test
    def fold03(): Unit \ Assert =
        assertEq(expected = 6, UnorderedFoldable.fold(Set#{1, 2, 3}))

    @Test
    def fold04(): Unit \ Assert =
        assertEq(expected = (4, 6), UnorderedFoldable.fold(Set#{(1, 2), (3, 4)}))


    /////////////////////////////////////////////////////////////////////////////
    // isEmpty                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isEmpty01(): Unit \ Assert =
        assertTrue(UnorderedFoldable.isEmpty((Set#{}: Set[Unit])))

    @Test
    def isEmpty02(): Unit \ Assert =
        assertFalse(UnorderedFoldable.isEmpty(Set#{1}))

    @Test
    def isEmpty03(): Unit \ Assert =
        assertFalse(UnorderedFoldable.isEmpty(Set#{1, 2}))

    @Test
    def isEmpty04(): Unit \ Assert =
        assertFalse(UnorderedFoldable.isEmpty(Set#{1, 2, 3}))

    @Test
    def isEmpty05(): Unit \ Assert =
        assertFalse(UnorderedFoldable.isEmpty(Set#{1, 2, 3, 4, 5, 6, 7, 8}))


    /////////////////////////////////////////////////////////////////////////////
    // nonEmpty                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def nonEmpty01(): Unit \ Assert =
        assertFalse(UnorderedFoldable.nonEmpty((Set#{}: Set[Unit])))

    @Test
    def nonEmpty02(): Unit \ Assert =
        assertTrue(UnorderedFoldable.nonEmpty(Set#{1}))

    @Test
    def nonEmpty03(): Unit \ Assert =
        assertTrue(UnorderedFoldable.nonEmpty(Set#{1, 2}))

    @Test
    def nonEmpty04(): Unit \ Assert =
        assertTrue(UnorderedFoldable.nonEmpty(Set#{1, 2, 3}))

    @Test
    def nonEmpty05(): Unit \ Assert =
        assertTrue(UnorderedFoldable.nonEmpty(Set#{1, 2, 3, 4, 5, 6, 7, 8}))


    /////////////////////////////////////////////////////////////////////////////
    // exists                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def exists01(): Unit \ Assert =
        assertFalse(UnorderedFoldable.exists(x -> x `Int32.remainder` 8 == 7, Set#{}))

    @Test
    def exists02(): Unit \ Assert =
        assertFalse(UnorderedFoldable.exists(x -> x `Int32.remainder` 8 == 7, Set#{5}))

    @Test
    def exists03(): Unit \ Assert =
        assertTrue(UnorderedFoldable.exists(x -> x `Int32.remainder` 8 == 7, Set#{7}))

    @Test
    def exists04(): Unit \ Assert =
        assertTrue(UnorderedFoldable.exists(x -> x `Int32.remainder` 8 == 7, Set#{15}))

    @Test
    def exists05(): Unit \ Assert =
        assertFalse(UnorderedFoldable.exists(x -> x `Int32.remainder` 8 == 7, Set#{1, 44}))

    @Test
    def exists06(): Unit \ Assert =
        assertTrue(UnorderedFoldable.exists(x -> x `Int32.remainder` 8 == 7, Set#{11, 71}))

    @Test
    def exists07(): Unit \ Assert =
        assertTrue(UnorderedFoldable.exists(x -> x `Int32.remainder` 8 == 7, Set#{71, 12}))

    @Test
    def exists08(): Unit \ Assert =
        assertTrue(UnorderedFoldable.exists(x -> x `Int32.remainder` 8 == 7, Set#{71, 79}))

    @Test
    def exists09(): Unit \ Assert =
        assertFalse(UnorderedFoldable.exists(x -> x `Int32.remainder` 8 == 7, Set#{11, -1, -14, -2, 84, 113}))

    @Test
    def exists10(): Unit \ Assert =
        assertTrue(UnorderedFoldable.exists(x -> x `Int32.remainder` 8 == 7, Set#{11, -1, 31, -14, -2, 84, 111}))

    @Test
    def exists11(): Unit \ Assert =
        assertTrue(UnorderedFoldable.exists(x -> x `Int32.remainder` 8 == 7, Set#{11, -1, -14, -2, 84, 111, 38}))


    /////////////////////////////////////////////////////////////////////////////
    // forAll                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forAll01(): Unit \ Assert =
        assertTrue(UnorderedFoldable.forAll(x -> x `Int32.remainder` 8 == 7, Set#{}))

    @Test
    def forAll02(): Unit \ Assert =
        assertFalse(UnorderedFoldable.forAll(x -> x `Int32.remainder` 8 == 7, Set#{5}))

    @Test
    def forAll03(): Unit \ Assert =
        assertTrue(UnorderedFoldable.forAll(x -> x `Int32.remainder` 8 == 7, Set#{7}))

    @Test
    def forAll04(): Unit \ Assert =
        assertTrue(UnorderedFoldable.forAll(x -> x `Int32.remainder` 8 == 7, Set#{15}))

    @Test
    def forAll05(): Unit \ Assert =
        assertFalse(UnorderedFoldable.forAll(x -> x `Int32.remainder` 8 == 7, Set#{1, 44}))

    @Test
    def forAll06(): Unit \ Assert =
        assertFalse(UnorderedFoldable.forAll(x -> x `Int32.remainder` 8 == 7, Set#{11, 71}))

    @Test
    def forAll07(): Unit \ Assert =
        assertFalse(UnorderedFoldable.forAll(x -> x `Int32.remainder` 8 == 7, Set#{71, 12}))

    @Test
    def forAll08(): Unit \ Assert =
        assertTrue(UnorderedFoldable.forAll(x -> x `Int32.remainder` 8 == 7, Set#{71, 79}))

    @Test
    def forAll09(): Unit \ Assert =
        assertFalse(UnorderedFoldable.forAll(x -> x `Int32.remainder` 8 == 7, Set#{7, 15, 23, 84, 111}))

    @Test
    def forAll10(): Unit \ Assert =
        assertFalse(UnorderedFoldable.forAll(x -> x `Int32.remainder` 8 == 7, Set#{7, 15, 23, 111, 3}))

    @Test
    def forAll11(): Unit \ Assert =
        assertTrue(UnorderedFoldable.forAll(x -> x `Int32.remainder` 8 == 7, Set#{7, 15, 23, 119, 111}))


    /////////////////////////////////////////////////////////////////////////////
    // memberOf                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def memberOf01(): Unit \ Assert =
        assertFalse(UnorderedFoldable.memberOf(1, Set#{}))

    @Test
    def memberOf02(): Unit \ Assert =
        assertFalse(UnorderedFoldable.memberOf(1, Set#{2}))

    @Test
    def memberOf03(): Unit \ Assert =
        assertTrue(UnorderedFoldable.memberOf(1, Set#{1}))

    @Test
    def memberOf04(): Unit \ Assert =
        assertTrue(UnorderedFoldable.memberOf(1, Set#{1, 2}))

    @Test
    def memberOf05(): Unit \ Assert =
        assertTrue(UnorderedFoldable.memberOf(2, Set#{1, 2}))

    @Test
    def memberOf06(): Unit \ Assert =
        assertFalse(UnorderedFoldable.memberOf(0, Set#{1, 2}))

    @Test
    def memberOf07(): Unit \ Assert =
        assertFalse(UnorderedFoldable.memberOf(3, Set#{1, 2}))

    @Test
    def memberOf08(): Unit \ Assert =
        assertFalse(UnorderedFoldable.memberOf(0, Set#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}))

    @Test
    def memberOf09(): Unit \ Assert =
        assertTrue(UnorderedFoldable.memberOf(1, Set#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}))

    @Test
    def memberOf10(): Unit \ Assert =
        assertTrue(UnorderedFoldable.memberOf(2, Set#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}))

    @Test
    def memberOf11(): Unit \ Assert =
        assertTrue(UnorderedFoldable.memberOf(10, Set#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}))

    @Test
    def memberOf12(): Unit \ Assert =
        assertFalse(UnorderedFoldable.memberOf(12, Set#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}))


    /////////////////////////////////////////////////////////////////////////////
    // size                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def size01(): Unit \ Assert =
        assertEq(expected = 0, UnorderedFoldable.size(Set#{}))

    @Test
    def size02(): Unit \ Assert =
        assertEq(expected = 1, UnorderedFoldable.size(Set#{1}))

    @Test
    def size03(): Unit \ Assert =
        assertEq(expected = 2, UnorderedFoldable.size(Set#{1, 2}))

    @Test
    def size04(): Unit \ Assert =
        assertEq(expected = 3, UnorderedFoldable.size(Set#{1, 2, 3}))

    @Test
    def size05(): Unit \ Assert =
        assertEq(expected = 13, UnorderedFoldable.size(Set#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 14}))


    /////////////////////////////////////////////////////////////////////////////
    // count                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def count01(): Unit \ Assert =
        assertEq(expected = 0, UnorderedFoldable.count(i -> i > 3, Set#{}))

    @Test
    def count02(): Unit \ Assert =
        assertEq(expected = 0, UnorderedFoldable.count(i -> i > 3, Set#{1}))

    @Test
    def count03(): Unit \ Assert =
        assertEq(expected = 1, UnorderedFoldable.count(i -> i > 3, Set#{4}))

    @Test
    def count04(): Unit \ Assert =
        assertEq(expected = 0, UnorderedFoldable.count(i -> i > 3, Set#{2, 1}))

    @Test
    def count05(): Unit \ Assert =
        assertEq(expected = 1, UnorderedFoldable.count(i -> i > 3, Set#{8, 1}))

    @Test
    def count06(): Unit \ Assert =
        assertEq(expected = 1, UnorderedFoldable.count(i -> i > 3, Set#{1, 8}))

    @Test
    def count07(): Unit \ Assert =
        assertEq(expected = 2, UnorderedFoldable.count(i -> i > 3, Set#{7, 6}))

    @Test
    def count08(): Unit \ Assert =
        assertEq(expected = 96, UnorderedFoldable.count(i -> i > 3, Set.range(0, 100)))

}

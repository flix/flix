mod TestTraversable {

    use Traversable.{sequence, traverse, mapAccumLeft}
    use Assert.{assertEq, assertEqWithMsg}


    /////////////////////////////////////////////////////////////////////////////
    // for                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    // Just test `for` on List - `for` will work for every Traversable with a
    // properly defined `traverse` method.

    @Test
    def for01(): Unit \ Assert =
        let xs: List[Int32] = Nil;
        assertEq(expected = Some(Nil), Traversable.for(xs, x -> Some(x+1)))

    @Test
    def for02(): Unit \ Assert =
        let xs: List[Int32] = 1 :: Nil;
        assertEq(expected = Some(2 :: Nil), Traversable.for(xs, x -> Some(x+1)))

    @Test
    def for03(): Unit \ Assert =
        let xs: List[Int32] = 1 :: 2 :: Nil;
        assertEq(expected = Some(2 :: 3 :: Nil), Traversable.for(xs, x -> Some(x+1)))

    @Test
    def for04(): Unit \ Assert =
        let xs: List[Int32] = Nil;
        assertEq(expected = Some(Nil), Traversable.for(xs, _ -> (None: Option[Int32])))

    @Test
    def for05(): Unit \ Assert =
        let xs: List[Int32] = 1 :: Nil;
        assertEq(expected = None, Traversable.for(xs, _ -> (None: Option[Int32])))

    @Test
    def for06(): Unit \ Assert =
        let xs: List[Int32] = 1 :: 2 :: Nil;
        assertEq(expected = None, Traversable.for(xs, _ -> (None: Option[Int32])))

    /////////////////////////////////////////////////////////////////////////////
    // sequenceOption                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sequenceOption01(): Unit \ Assert =
        let o: Option[Option[Int32]] = None;
        assertEq(expected = Some(None), sequence(o))

    @Test
    def sequenceOption02(): Unit \ Assert =
        let o: Option[Option[Int32]] = Some(None);
        assertEq(expected = None, sequence(o))

    @Test
    def sequenceOption03(): Unit \ Assert =
        let o: Option[Option[Int32]] = Some(Some(1));
        assertEq(expected = Some(Some(1)), sequence(o))

    /////////////////////////////////////////////////////////////////////////////
    // traverseOption                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def traverseOption01(): Unit \ Assert =
        let o: Option[Int32] = None;
        assertEq(expected = Some(None), traverse(x -> Some(x+1), o))

    @Test
    def traverseOption02(): Unit \ Assert =
        let o: Option[Int32] = Some(1);
        assertEq(expected = Some(Some(2)), traverse(x -> Some(x+1), o))

    /////////////////////////////////////////////////////////////////////////////
    // mapAccumLeftOption                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def mapAccumLeftOption01(): Unit \ Assert =
        let o: Option[Int32] = None;
        assertEq(expected = (10, None), mapAccumLeft((acc, x) -> (acc+1, x+acc), 10, o))

    @Test
    def mapAccumLeftOption02(): Unit \ Assert =
        let o: Option[Int32] = Some(1);
        assertEq(expected = (11, Some(11)), mapAccumLeft((acc, x) -> (acc+1, x+acc), 10, o))

    /////////////////////////////////////////////////////////////////////////////
    // sequenceList                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sequenceList01(): Unit \ Assert =
        let xs: List[Option[Int32]] = Nil;
        assertEq(expected = Some(Nil), sequence(xs))

    @Test
    def sequenceList02(): Unit \ Assert =
        let xs: List[Option[Int32]] = None :: Nil;
        assertEq(expected = None, sequence(xs))

    @Test
    def sequenceList03(): Unit \ Assert =
        let xs = Some(1) :: Nil;
        assertEq(expected = Some(1 :: Nil), sequence(xs))

    @Test
    def sequenceList04(): Unit \ Assert =
        let xs = Some(1) :: None :: Nil;
        assertEq(expected = None, sequence(xs))

    @Test
    def sequenceList05(): Unit \ Assert =
        let xs = Some(1) :: Some(2) :: Nil;
        assertEq(expected = Some(1 :: 2 :: Nil), sequence(xs))

    /////////////////////////////////////////////////////////////////////////////
    // traverseList                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def traverseList01(): Unit \ Assert =
        let xs: List[Int32] = Nil;
        assertEq(expected = Some(Nil), traverse(x -> Some(x+1), xs))

    @Test
    def traverseList02(): Unit \ Assert =
        let xs: List[Int32] = 1 :: Nil;
        assertEq(expected = Some(2 :: Nil), traverse(x -> Some(x+1), xs))

    @Test
    def traverseList03(): Unit \ Assert =
        let xs: List[Int32] = 1 :: 2 :: Nil;
        assertEq(expected = Some(2 :: 3 :: Nil), traverse(x -> Some(x+1), xs))

    @Test
    def traverseList04(): Unit \ Assert =
        let xs: List[Int32] = Nil;
        assertEq(expected = Some(Nil), traverse(_ -> (None: Option[Int32]), xs))

    @Test
    def traverseList05(): Unit \ Assert =
        let xs: List[Int32] = 1 :: Nil;
        assertEq(expected = None, traverse(_ -> (None: Option[Int32]), xs))

    @Test
    def traverseList06(): Unit \ Assert =
        let xs: List[Int32] = 1 :: 2 :: Nil;
        assertEq(expected = None, traverse(_ -> (None: Option[Int32]), xs))

    /////////////////////////////////////////////////////////////////////////////
    // mapAccumLeftList                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def mapAccumLeftList01(): Unit \ Assert =
        let xs: List[Int32] = Nil;
        assertEq(expected = (10, Nil), mapAccumLeft((acc, x) -> (acc+1, x+acc), 10, xs))

    @Test
    def mapAccumLeftList02(): Unit \ Assert =
        let xs: List[Int32] = 1 :: Nil;
        assertEq(expected = (11, 11 :: Nil), mapAccumLeft((acc, x) -> (acc+1, x+acc), 10, xs))

    @Test
    def mapAccumLeftList03(): Unit \ Assert =
        let xs: List[Int32] = 1 :: 2 :: Nil;
        assertEq(expected = (12, 11 :: 13 :: Nil), mapAccumLeft((acc, x) -> (acc+1, x+acc), 10, xs))

    @Test
    def mapAccumLeftList04(): Unit \ Assert =
        let xs: List[Int32] = 1 :: 2 :: 3 :: Nil;
        assertEq(expected = (13, 11 :: 13 :: 15 :: Nil), mapAccumLeft((acc, x) -> (acc+1, x+acc), 10, xs))

    /////////////////////////////////////////////////////////////////////////////
    // sequenceNel.Nel                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sequenceNel01(): Unit \ Assert =
        let xs: Nel[Option[Int32]] = Nel.singleton(None);
        assertEq(expected = None, sequence(xs))

    @Test
    def sequenceNel02(): Unit \ Assert =
        let xs: Nel[Option[Int32]] = Nel.singleton(Some(1));
        assertEq(expected = Some(Nel.singleton(1)), sequence(xs))

    @Test
    def sequenceNel03(): Unit \ Assert =
        let xs = Nel.Nel(Some(1), Some(2) :: Nil);
        assertEq(expected = Some(Nel.Nel(1, 2 :: Nil)), sequence(xs))

    @Test
    def sequenceNel04(): Unit \ Assert =
        let xs = Nel.Nel(Some(1), Some(2) :: Some(3) :: Nil);
        assertEq(expected = Some(Nel.Nel(1, 2 :: 3 :: Nil)), sequence(xs))

    /////////////////////////////////////////////////////////////////////////////
    // traverseNel.Nel                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def traverseNel01(): Unit \ Assert =
        let xs: Nel[Int32] = Nel.Nel(1, Nil);
        assertEq(expected = Some(Nel.Nel(2, Nil)), traverse(x -> Some(x+1), xs))

    @Test
    def traverseNel02(): Unit \ Assert =
        let xs: Nel[Int32] = Nel.Nel(1, 2 :: Nil);
        assertEq(expected = Some(Nel.Nel(2, 3 :: Nil)), traverse(x -> Some(x+1), xs))

    @Test
    def traverseNel03(): Unit \ Assert =
        let xs: Nel[Int32] = Nel.Nel(1, Nil);
        assertEq(expected = None, traverse(_ -> (None: Option[Int32]), xs))

    @Test
    def traverseNel04(): Unit \ Assert =
        let xs: Nel[Int32] = Nel.Nel(1, 2 :: Nil);
        assertEq(expected = None, traverse(_ -> (None: Option[Int32]), xs))

    /////////////////////////////////////////////////////////////////////////////
    // mapAccumLeftNel.Nel                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def mapAccumLeftNel01(): Unit \ Assert =
        let xs: Nel[Int32] = Nel.Nel(1, Nil);
        assertEq(expected = (11, Nel.Nel(11, Nil)), mapAccumLeft((acc, x) -> (acc+1, x+acc), 10, xs))

    @Test
    def mapAccumLeftNel02(): Unit \ Assert =
        let xs: Nel[Int32] = Nel.Nel(1, 2 :: Nil);
        assertEq(expected = (12, Nel.Nel(11, 13 :: Nil)), mapAccumLeft((acc, x) -> (acc+1, x+acc), 10, xs))

    @Test
    def mapAccumLeftNel03(): Unit \ Assert =
        let xs: Nel[Int32] = Nel.Nel(1, 2 :: 3 :: Nil);
        assertEq(expected = (13, Nel.Nel(11, 13 :: 15 :: Nil)), mapAccumLeft((acc, x) -> (acc+1, x+acc), 10, xs))

    /////////////////////////////////////////////////////////////////////////////
    // sequenceChain                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sequenceChain01(): Unit \ Assert =
        let xs: Chain[Option[Int32]] = Chain.empty();
        assertEq(expected = Some(Chain.empty()), sequence(xs))

    @Test
    def sequenceChain02(): Unit \ Assert =
        let xs: Chain[Option[Int32]] = Chain.singleton(None);
        assertEq(expected = None, sequence(xs))

    @Test
    def sequenceChain03(): Unit \ Assert =
        let xs = Chain.singleton(Some(1));
        assertEq(expected = Some(Chain.singleton(1)), sequence(xs))

    @Test
    def sequenceChain04(): Unit \ Assert =
        let xs = List.toChain(Some(1) :: None :: Nil);
        assertEq(expected = None, sequence(xs))

    @Test
    def sequenceChain05(): Unit \ Assert =
        let xs = List.toChain(Some(1) :: Some(2) :: Nil);
        assertEq(expected = Some(List.toChain(1 :: 2 :: Nil)), sequence(xs))

    /////////////////////////////////////////////////////////////////////////////
    // traverseChain                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def traverseChain01(): Unit \ Assert =
        let xs: Chain[Int32] = Chain.empty();
        assertEq(expected = Some(Chain.empty()), traverse(x -> Some(x+1), xs))

    @Test
    def traverseChain02(): Unit \ Assert =
        let xs: Chain[Int32] = Chain.singleton(1);
        assertEq(expected = Some(Chain.singleton(2)), traverse(x -> Some(x+1), xs))

    @Test
    def traverseChain03(): Unit \ Assert =
        let xs: Chain[Int32] = List.toChain(1 :: 2 :: Nil);
        assertEq(expected = Some(List.toChain(2 :: 3 :: Nil)), traverse(x -> Some(x+1), xs))

    @Test
    def traverseChain04(): Unit \ Assert =
        let xs: Chain[Int32] = Chain.empty();
        assertEq(expected = Some(Chain.empty()), traverse(_ -> (None: Option[Int32]), xs))

    @Test
    def traverseChain05(): Unit \ Assert =
        let xs: Chain[Int32] = Chain.singleton(1);
        assertEq(expected = None, traverse(_ -> (None: Option[Int32]), xs))

    @Test
    def traverseChain06(): Unit \ Assert =
        let xs: Chain[Int32] = List.toChain(1 :: 2 :: Nil);
        assertEq(expected = None, traverse(_ -> (None: Option[Int32]), xs))

    /////////////////////////////////////////////////////////////////////////////
    // mapAccumLeftChain                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def mapAccumLeftChain01(): Unit \ Assert =
        let xs: Chain[Int32] = Chain.empty();
        assertEq(expected = (10, Chain.empty()), mapAccumLeft((acc, x) -> (acc+1, x+acc), 10, xs))

    @Test
    def mapAccumLeftChain02(): Unit \ Assert =
        let xs: Chain[Int32] = Chain.singleton(1);
        assertEq(expected = (11, Chain.singleton(11)), mapAccumLeft((acc, x) -> (acc+1, x+acc), 10, xs))

    @Test
    def mapAccumLeftChain03(): Unit \ Assert =
        let xs: Chain[Int32] = List.toChain(1 :: 2 :: Nil);
        assertEq(expected = (12, List.toChain(11 :: 13 :: Nil)), mapAccumLeft((acc, x) -> (acc+1, x+acc), 10, xs))

    @Test
    def mapAccumLeftChain04(): Unit \ Assert =
        let xs: Chain[Int32] = List.toChain(1 :: 2 :: 3 :: Nil);
        assertEq(expected = (13, List.toChain(11 :: 13 :: 15 :: Nil)), mapAccumLeft((acc, x) -> (acc+1, x+acc), 10, xs))

    /////////////////////////////////////////////////////////////////////////////
    // sequenceMap                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sequenceMap01(): Unit \ Assert =
        let m: Map[Int32, Option[Int32]] = Map#{};
        assertEq(expected = Some(Map#{}), sequence(m))

    @Test
    def sequenceMap02(): Unit \ Assert =
        let m: Map[Int32, Option[Int32]] = Map#{1 => None};
        assertEq(expected = None, sequence(m))

    @Test
    def sequenceMap03(): Unit \ Assert =
        let m = Map#{1 => Some(1)};
        assertEq(expected = Some(Map#{1 => 1}), sequence(m))

    @Test
    def sequenceMap04(): Unit \ Assert =
        let m = Map#{1 => Some(1), 2 => None};
        assertEq(expected = None, sequence(m))

    @Test
    def sequenceMap05(): Unit \ Assert =
        let m = Map#{1 => Some(1), 2 => Some(2)};
        assertEq(expected = Some(Map#{1 => 1, 2 => 2}), sequence(m))

    /////////////////////////////////////////////////////////////////////////////
    // traverseMap                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def traverseMap01(): Unit \ Assert =
        let m: Map[Int32, Int32] = Map#{};
        assertEq(expected = Some(Map#{}), traverse(x -> Some(x+1), m))

    @Test
    def traverseMap02(): Unit \ Assert =
        let m: Map[Int32, Int32] = Map#{1 => 1};
        assertEq(expected = Some(Map#{1 => 2}), traverse(x -> Some(x+1), m))

    @Test
    def traverseMap03(): Unit \ Assert =
        let m: Map[Int32, Int32] = Map#{1 => 1, 2 => 2};
        assertEq(expected = Some(Map#{1 => 2, 2 => 3}), traverse(x -> Some(x+1), m))

    @Test
    def traverseMap04(): Unit \ Assert =
        let m: Map[Int32, Int32] = Map#{};
        assertEq(expected = Some(Map#{}), traverse(_ -> (None: Option[Int32]), m))

    @Test
    def traverseMap05(): Unit \ Assert =
        let m: Map[Int32, Int32] = Map#{1 => 1};
        assertEq(expected = None, traverse(_ -> (None: Option[Int32]), m))

    @Test
    def traverseMap06(): Unit \ Assert =
        let m: Map[Int32, Int32] = Map#{1 => 1, 2 => 2};
        assertEq(expected = None, traverse(_ -> (None: Option[Int32]), m))

    /////////////////////////////////////////////////////////////////////////////
    // mapAccumLeftMap                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def mapAccumLeftMap01(): Unit \ Assert =
        let m: Map[Int32, Int32] = Map#{};
        assertEq(expected = (10, Map#{}), mapAccumLeft((acc, x) -> (acc+1, x+acc), 10, m))

    @Test
    def mapAccumLeftMap02(): Unit \ Assert =
        let m: Map[Int32, Int32] = Map#{1 => 1};
        assertEq(expected = (11, Map#{1 => 11}), mapAccumLeft((acc, x) -> (acc+1, x+acc), 10, m))

    @Test
    def mapAccumLeftMap03(): Unit \ Assert =
        let m: Map[Int32, Int32]  = Map#{1 => 1, 2 => 2};
        assertEq(expected = (12, Map#{1 => 11, 2 => 13}), mapAccumLeft((acc, x) -> (acc+1, x+acc), 10, m))

    @Test
    def mapAccumLeftMap04(): Unit \ Assert =
        let m: Map[Int32, Int32]  = Map#{1 => 1, 2 => 2, 3 => 3};
        assertEq(expected = (13, Map#{1 => 11, 2 => 13, 3 => 15}), mapAccumLeft((acc, x) -> (acc+1, x+acc), 10, m))

    /////////////////////////////////////////////////////////////////////////////
    /// Helper for RedBlackTree                                                //
    /////////////////////////////////////////////////////////////////////////////

    def toRedBlackTree(xs: List[(k, v)]): RedBlackTree[k, v] with Order[k] =
        List.foldLeft((acc, kv) -> {let (k,v) = kv; RedBlackTree.insert(k, v, acc)}, RedBlackTree.empty(), xs)

    /////////////////////////////////////////////////////////////////////////////
    // sequenceRedBlackTree                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sequenceRedBlackTree01(): Unit \ Assert =
        let t: RedBlackTree[Int32, Option[Int32]] = RedBlackTree.empty();
        assertEqWithMsg(expected = Some(RedBlackTree.empty()), sequence(t), "sequenceRedBlackTree01")

    @Test
    def sequenceRedBlackTree02(): Unit \ Assert =
        let t: RedBlackTree[Int32, Option[Int32]] = toRedBlackTree((1, None) :: Nil);
        assertEqWithMsg(expected = None, sequence(t), "sequenceRedBlackTree02")

    @Test
    def sequenceRedBlackTree03(): Unit \ Assert =
        let t = toRedBlackTree((1, Some(1)) :: Nil);
        assertEqWithMsg(expected = Some(toRedBlackTree((1, 1) :: Nil)), sequence(t), "sequenceRedBlackTree03")

    @Test
    def sequenceRedBlackTree04(): Unit \ Assert =
        let t = toRedBlackTree((1, Some(1)) :: (2, None) :: Nil);
        assertEqWithMsg(expected = None, sequence(t), "sequenceRedBlackTree04")

    @Test
    def sequenceRedBlackTree05(): Unit \ Assert =
        let t = toRedBlackTree((1, Some(1)) :: (2, Some(2)) :: Nil);
        assertEqWithMsg(expected = Some(toRedBlackTree((1, 1) :: (2, 2) :: Nil)), sequence(t), "sequenceRedBlackTree05")

    /////////////////////////////////////////////////////////////////////////////
    // traverseRedBlackTree                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def traverseRedBlackTree01(): Unit \ Assert =
        let t: RedBlackTree[Int32, Int32] = RedBlackTree.empty();
        assertEqWithMsg(expected = Some(RedBlackTree.empty()), traverse(x -> Some(x+1), t), "traverseRedBlackTree01")

    @Test
    def traverseRedBlackTree02(): Unit \ Assert =
        let t: RedBlackTree[Int32, Int32] = toRedBlackTree((1, 1) :: Nil);
        assertEqWithMsg(expected = Some(toRedBlackTree((1, 2) :: Nil)), traverse(x -> Some(x+1), t), "traverseRedBlackTree02")

    @Test
    def traverseRedBlackTree03(): Unit \ Assert =
        let t: RedBlackTree[Int32, Int32] = toRedBlackTree((1, 1) :: (2, 2) :: Nil);
        assertEqWithMsg(expected = Some(toRedBlackTree((1, 2) :: (2, 3) :: Nil)), traverse(x -> Some(x+1), t), "traverseRedBlackTree03")

    @Test
    def traverseRedBlackTree04(): Unit \ Assert =
        let t: RedBlackTree[Int32, Int32] = RedBlackTree.empty();
        assertEqWithMsg(expected = Some(RedBlackTree.empty()), traverse(_ -> (None: Option[Int32]), t), "traverseRedBlackTree04")

    @Test
    def traverseRedBlackTree05(): Unit \ Assert =
        let t: RedBlackTree[Int32, Int32] = toRedBlackTree((1, 1):: Nil);
        assertEqWithMsg(expected = None, traverse(_ -> (None: Option[Int32]), t), "traverseRedBlackTree05")

    @Test
    def traverseRedBlackTree06(): Unit \ Assert =
        let t: RedBlackTree[Int32, Int32] = toRedBlackTree((1, 1) :: (2, 2) :: Nil);
        assertEqWithMsg(expected = None, traverse(_ -> (None: Option[Int32]), t), "traverseRedBlackTree06")

    /////////////////////////////////////////////////////////////////////////////
    // mapAccumLeftRedBlackTree                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def mapAccumLeftRedBlackTree01(): Unit \ Assert =
        let m: RedBlackTree[Int32, Int32] = RedBlackTree.empty();
        assertEqWithMsg(expected = (10, RedBlackTree.empty()), mapAccumLeft((acc, x) -> (acc+1, x+acc), 10, m), "mapAccumLeftRedBlackTree01")

    @Test
    def mapAccumLeftRedBlackTree02(): Unit \ Assert =
        let m: RedBlackTree[Int32, Int32] = toRedBlackTree((1, 1) :: Nil);
        assertEqWithMsg(expected = (11, toRedBlackTree((1, 11) :: Nil)), mapAccumLeft((acc, x) -> (acc+1, x+acc), 10, m), "mapAccumLeftRedBlackTree02")

    @Test
    def mapAccumLeftRedBlackTree03(): Unit \ Assert =
        let m: RedBlackTree[Int32, Int32] = toRedBlackTree((1, 1) :: (2, 2) :: Nil);
        assertEqWithMsg(expected = (12, toRedBlackTree((1, 11) :: (2, 13) :: Nil)), mapAccumLeft((acc, x) -> (acc+1, x+acc), 10, m), "mapAccumLeftRedBlackTree03")

    @Test
    def mapAccumLeftRedBlackTree04(): Unit \ Assert =
        let m: RedBlackTree[Int32, Int32] = toRedBlackTree((1, 1) :: (2, 2) :: (3, 3) :: Nil);
        assertEqWithMsg(expected = (13, toRedBlackTree((1, 11) :: (2, 13) :: (3, 15) :: Nil)), mapAccumLeft((acc, x) -> (acc+1, x+acc), 10, m), "mapAccumLeftRedBlackTree04")

}

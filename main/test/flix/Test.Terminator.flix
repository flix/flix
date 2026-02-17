mod Test.Terminator {

    use Assert.assertTrue;

    // --- Basic data types ---
    enum MyList[a] with Eq { case Nil, case Cons(a, MyList[a]) }
    enum MyTree[a] with Eq { case Leaf(a), case Node(MyTree[a], MyTree[a]) }
    enum Nat with Eq { case Zero, case Succ(Nat) }
    enum MyOption[a] with Eq { case None, case Some(a) }

    // --- Structurally recursive functions on List ---

    @Terminates
    def length(l: MyList[Int32]): Int32 = match l {
        case MyList.Nil         => 0
        case MyList.Cons(_, xs) => 1 + length(xs)
    }

    @Test
    def testLength01(): Unit \ Assert =
        assertTrue(length(MyList.Cons(1, MyList.Cons(2, MyList.Nil))) == 2)

    @Terminates
    def sum(l: MyList[Int32]): Int32 = match l {
        case MyList.Nil         => 0
        case MyList.Cons(x, xs) => x + sum(xs)
    }

    @Test
    def testSum01(): Unit \ Assert =
        assertTrue(sum(MyList.Cons(1, MyList.Cons(2, MyList.Cons(3, MyList.Nil)))) == 6)

    @Terminates
    def append(l1: MyList[Int32], l2: MyList[Int32]): MyList[Int32] = match l1 {
        case MyList.Nil         => l2
        case MyList.Cons(x, xs) => MyList.Cons(x, append(xs, l2))
    }

    @Test
    def testAppend01(): Unit \ Assert =
        assertTrue(append(MyList.Cons(1, MyList.Nil), MyList.Cons(2, MyList.Nil)) ==
            MyList.Cons(1, MyList.Cons(2, MyList.Nil)))

    @Terminates
    def rev(l: MyList[Int32], acc: MyList[Int32]): MyList[Int32] = match l {
        case MyList.Nil         => acc
        case MyList.Cons(x, xs) => rev(xs, MyList.Cons(x, acc))
    }

    @Test
    def testReverse01(): Unit \ Assert =
        assertTrue(rev(MyList.Cons(1, MyList.Cons(2, MyList.Nil)), MyList.Nil) ==
            MyList.Cons(2, MyList.Cons(1, MyList.Nil)))

    @Terminates
    def doubleAll(l: MyList[Int32]): MyList[Int32] = match l {
        case MyList.Nil         => MyList.Nil
        case MyList.Cons(x, xs) => MyList.Cons(x * 2, doubleAll(xs))
    }

    @Test
    def testDoubleAll01(): Unit \ Assert =
        assertTrue(doubleAll(MyList.Cons(1, MyList.Cons(2, MyList.Nil))) ==
            MyList.Cons(2, MyList.Cons(4, MyList.Nil)))

    // --- Structurally recursive functions on Tree ---

    @Terminates
    def treeSize(t: MyTree[Int32]): Int32 = match t {
        case MyTree.Leaf(_)    => 1
        case MyTree.Node(l, r) => treeSize(l) + treeSize(r)
    }

    @Test
    def testTreeSize01(): Unit \ Assert =
        assertTrue(treeSize(MyTree.Node(MyTree.Leaf(1), MyTree.Leaf(2))) == 2)

    @Terminates
    def treeHeight(t: MyTree[Int32]): Int32 = match t {
        case MyTree.Leaf(_)    => 0
        case MyTree.Node(l, r) =>
            let lh = treeHeight(l);
            let rh = treeHeight(r);
            1 + (if (lh > rh) lh else rh)
    }

    @Test
    def testTreeHeight01(): Unit \ Assert =
        assertTrue(treeHeight(MyTree.Node(MyTree.Node(MyTree.Leaf(1), MyTree.Leaf(2)), MyTree.Leaf(3))) == 2)

    @Terminates
    def treeMirror(t: MyTree[Int32]): MyTree[Int32] = match t {
        case MyTree.Leaf(v)    => MyTree.Leaf(v)
        case MyTree.Node(l, r) => MyTree.Node(treeMirror(r), treeMirror(l))
    }

    @Test
    def testTreeMirror01(): Unit \ Assert =
        assertTrue(treeMirror(MyTree.Node(MyTree.Leaf(1), MyTree.Leaf(2))) ==
            MyTree.Node(MyTree.Leaf(2), MyTree.Leaf(1)))

    // --- Structurally recursive functions on Nat ---

    @Terminates
    def natToInt(n: Nat): Int32 = match n {
        case Nat.Zero    => 0
        case Nat.Succ(m) => 1 + natToInt(m)
    }

    @Test
    def testNatToInt01(): Unit \ Assert =
        assertTrue(natToInt(Nat.Succ(Nat.Succ(Nat.Succ(Nat.Zero)))) == 3)

    @Terminates
    def natAdd(n: Nat, m: Nat): Nat = match n {
        case Nat.Zero    => m
        case Nat.Succ(p) => Nat.Succ(natAdd(p, m))
    }

    @Test
    def testNatAdd01(): Unit \ Assert =
        assertTrue(natToInt(natAdd(Nat.Succ(Nat.Succ(Nat.Zero)), Nat.Succ(Nat.Zero))) == 3)

    // --- Non-recursive (trivially terminating) ---

    @Terminates
    def trivial(x: Int32): Int32 = x

    @Test
    def testTrivial01(): Unit \ Assert =
        assertTrue(trivial(42) == 42)

    @Terminates
    def constant(): Int32 = 7

    @Test
    def testTrivial02(): Unit \ Assert =
        assertTrue(constant() == 7)

    // --- Nested pattern matches ---

    @Terminates
    def dropTwo(l: MyList[Int32]): MyList[Int32] = match l {
        case MyList.Nil                            => MyList.Nil
        case MyList.Cons(_, MyList.Nil)            => MyList.Nil
        case MyList.Cons(_, MyList.Cons(_, rest))  => rest
    }

    @Test
    def testNestedMatch01(): Unit \ Assert =
        assertTrue(dropTwo(MyList.Cons(1, MyList.Cons(2, MyList.Cons(3, MyList.Nil)))) ==
            MyList.Cons(3, MyList.Nil))

    // --- Let-bound alias ---

    @Terminates
    def lengthAlias(l: MyList[Int32]): Int32 = match l {
        case MyList.Nil         => 0
        case MyList.Cons(_, xs) =>
            let tail = xs;
            1 + lengthAlias(tail)
    }

    @Test
    def testLetAlias01(): Unit \ Assert =
        assertTrue(lengthAlias(MyList.Cons(1, MyList.Cons(2, MyList.Nil))) == 2)

    // --- If-then-else inside match ---

    @Terminates
    def filterPositive(l: MyList[Int32]): MyList[Int32] = match l {
        case MyList.Nil         => MyList.Nil
        case MyList.Cons(x, xs) =>
            if (x > 0) MyList.Cons(x, filterPositive(xs))
            else filterPositive(xs)
    }

    @Test
    def testFilter01(): Unit \ Assert =
        assertTrue(filterPositive(MyList.Cons(-1, MyList.Cons(2, MyList.Cons(3, MyList.Nil)))) ==
            MyList.Cons(2, MyList.Cons(3, MyList.Nil)))

    // --- Multiple parameters, only one decreasing ---

    @Terminates
    def take(n: Nat, l: MyList[Int32]): MyList[Int32] = match n {
        case Nat.Zero    => MyList.Nil
        case Nat.Succ(m) => match l {
            case MyList.Nil         => MyList.Nil
            case MyList.Cons(x, xs) => MyList.Cons(x, take(m, xs))
        }
    }

    @Test
    def testTake01(): Unit \ Assert =
        assertTrue(take(Nat.Succ(Nat.Succ(Nat.Zero)), MyList.Cons(1, MyList.Cons(2, MyList.Cons(3, MyList.Nil)))) ==
            MyList.Cons(1, MyList.Cons(2, MyList.Nil)))

    // --- Option type ---

    @Terminates
    def optGetOrElse(o: MyOption[Int32], default: Int32): Int32 = match o {
        case MyOption.None    => default
        case MyOption.Some(v) => v
    }

    @Test
    def testOptionGetOrElse01(): Unit \ Assert =
        assertTrue(optGetOrElse(MyOption.Some(42), 0) == 42)

}

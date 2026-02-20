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

    // --- Tuple-scrutinee structural recursion ---

    @Terminates
    def isPrefixOf(l1: MyList[Int32], l2: MyList[Int32]): Bool = match (l1, l2) {
        case (MyList.Nil, _)                          => true
        case (_, MyList.Nil)                           => false
        case (MyList.Cons(x, xs), MyList.Cons(y, ys)) =>
            if (x == y) isPrefixOf(xs, ys) else false
    }

    @Test
    def testIsPrefixOf01(): Unit \ Assert =
        assertTrue(isPrefixOf(MyList.Cons(1, MyList.Nil), MyList.Cons(1, MyList.Cons(2, MyList.Nil))))

    @Test
    def testIsPrefixOf02(): Unit \ Assert =
        assertTrue(not isPrefixOf(MyList.Cons(3, MyList.Nil), MyList.Cons(1, MyList.Nil)))

    @Terminates
    def myZip(l1: MyList[Int32], l2: MyList[Int32]): MyList[(Int32, Int32)] = match (l1, l2) {
        case (MyList.Nil, _)                          => MyList.Nil
        case (_, MyList.Nil)                           => MyList.Nil
        case (MyList.Cons(x, xs), MyList.Cons(y, ys)) =>
            MyList.Cons((x, y), myZip(xs, ys))
    }

    @Test
    def testZip01(): Unit \ Assert =
        assertTrue(myZip(MyList.Cons(1, MyList.Cons(2, MyList.Nil)),
                         MyList.Cons(3, MyList.Cons(4, MyList.Nil))) ==
            MyList.Cons((1, 3), MyList.Cons((2, 4), MyList.Nil)))

    @Terminates
    def natEq(n: Nat, m: Nat): Bool = match (n, m) {
        case (Nat.Zero, Nat.Zero)       => true
        case (Nat.Succ(a), Nat.Succ(b)) => natEq(a, b)
        case _                           => false
    }

    @Test
    def testNatEq01(): Unit \ Assert =
        assertTrue(natEq(Nat.Succ(Nat.Succ(Nat.Zero)), Nat.Succ(Nat.Succ(Nat.Zero))))

    @Test
    def testNatEq02(): Unit \ Assert =
        assertTrue(not natEq(Nat.Succ(Nat.Zero), Nat.Zero))

    // --- Local defs ---

    @Terminates
    def localLength(l: MyList[Int32]): Int32 =
        def loop(ll: MyList[Int32], acc: Int32): Int32 = match ll {
            case MyList.Nil         => acc
            case MyList.Cons(_, xs) => loop(xs, acc + 1)
        };
        loop(l, 0)

    @Test
    def testLocalLength01(): Unit \ Assert =
        assertTrue(localLength(MyList.Cons(1, MyList.Cons(2, MyList.Cons(3, MyList.Nil)))) == 3)

    @Terminates
    def nestedLocalDef(l: MyList[Int32]): Int32 =
        def outer(ll: MyList[Int32]): Int32 = match ll {
            case MyList.Nil         => 0
            case MyList.Cons(_, xs) =>
                def inner(mm: MyList[Int32], acc: Int32): Int32 = match mm {
                    case MyList.Nil         => acc
                    case MyList.Cons(_, ys) => inner(ys, acc + 1)
                };
                1 + inner(xs, 0) + outer(xs)
        };
        outer(l)

    @Test
    def testNestedLocalDef01(): Unit \ Assert =
        assertTrue(nestedLocalDef(MyList.Cons(1, MyList.Cons(2, MyList.Nil))) > 0)

    @Terminates
    def nonRecursiveLocalDef(l: MyList[Int32]): Int32 =
        def helper(x: Int32): Int32 = x + 1;
        match l {
            case MyList.Nil         => 0
            case MyList.Cons(x, xs) => helper(x) + nonRecursiveLocalDef(xs)
        }

    @Test
    def testNonRecursiveLocalDef01(): Unit \ Assert =
        assertTrue(nonRecursiveLocalDef(MyList.Cons(1, MyList.Cons(2, MyList.Nil))) == 5)

    // --- Local def calling outer function with strict sub ---

    @Terminates
    def localDefCallsOuter(l: MyList[Int32]): Int32 = match l {
        case MyList.Nil         => 0
        case MyList.Cons(_, xs) =>
            def helper(ll: MyList[Int32]): Int32 = match ll {
                case MyList.Nil         => localDefCallsOuter(xs)
                case MyList.Cons(_, ys) => helper(ys)
            };
            helper(xs)
    }

    @Test
    def testLocalDefCallsOuter01(): Unit \ Assert =
        assertTrue(localDefCallsOuter(MyList.Cons(1, MyList.Cons(2, MyList.Nil))) == 0)

    // --- Trait default implementations ---

    trait ListLen[a] {
        @Terminates
        pub def listLen(l: MyList[a]): Int32 = match l {
            case MyList.Nil         => 0
            case MyList.Cons(_, xs) => 1 + ListLen.listLen(xs)
        }
    }

    instance ListLen[Int32]

    @Test
    def testTraitDefault01(): Unit \ Assert =
        assertTrue(ListLen.listLen(MyList.Cons(1, MyList.Cons(2, MyList.Nil))) == 2)

    // --- Instance implementations ---

    trait MySize[a] {
        pub def mySize(x: a): Int32
    }

    instance MySize[MyList[a]] {
        @Terminates
        pub def mySize(x: MyList[a]): Int32 = match x {
            case MyList.Nil         => 0
            case MyList.Cons(_, xs) => 1 + MySize.mySize(xs)
        }
    }

    @Test
    def testInstanceDef01(): Unit \ Assert =
        assertTrue(MySize.mySize(MyList.Cons(1, MyList.Cons(2, MyList.Cons(3, MyList.Nil)))) == 3)

    instance MySize[MyTree[a]] {
        @Terminates
        pub def mySize(x: MyTree[a]): Int32 = match x {
            case MyTree.Leaf(_)    => 1
            case MyTree.Node(l, r) => MySize.mySize(l) + MySize.mySize(r)
        }
    }

    @Test
    def testInstanceDef02(): Unit \ Assert =
        assertTrue(MySize.mySize(MyTree.Node(MyTree.Leaf(1), MyTree.Leaf(2))) == 2)

    // --- @Terminates calling another @Terminates def ---

    @Terminates
    def helperLen(l: MyList[Int32]): Int32 = match l {
        case MyList.Nil         => 0
        case MyList.Cons(_, xs) => 1 + helperLen(xs)
    }

    @Terminates
    def callsTerminating(l: MyList[Int32]): Int32 = match l {
        case MyList.Nil         => 0
        case MyList.Cons(_, xs) => helperLen(xs) + callsTerminating(xs)
    }

    @Test
    def testCallsTerminating01(): Unit \ Assert =
        assertTrue(callsTerminating(MyList.Cons(1, MyList.Cons(2, MyList.Cons(3, MyList.Nil)))) == 3)

    // --- Nested constructor pattern with recursive call ---

    @Terminates
    def everyOther(l: MyList[Int32]): MyList[Int32] = match l {
        case MyList.Nil                           => MyList.Nil
        case MyList.Cons(x, MyList.Nil)           => MyList.Cons(x, MyList.Nil)
        case MyList.Cons(x, MyList.Cons(_, rest)) => MyList.Cons(x, everyOther(rest))
    }

    @Test
    def testEveryOther01(): Unit \ Assert =
        assertTrue(everyOther(MyList.Cons(1, MyList.Cons(2, MyList.Cons(3, MyList.Cons(4, MyList.Nil))))) ==
            MyList.Cons(1, MyList.Cons(3, MyList.Nil)))

    // --- Match guard with structural recursion ---

    @Terminates
    def takeWhilePositive(l: MyList[Int32]): MyList[Int32] = match l {
        case MyList.Cons(x, xs) if x > 0 => MyList.Cons(x, takeWhilePositive(xs))
        case _                            => MyList.Nil
    }

    @Test
    def testTakeWhilePositive01(): Unit \ Assert =
        assertTrue(takeWhilePositive(MyList.Cons(1, MyList.Cons(2, MyList.Cons(-1, MyList.Cons(3, MyList.Nil))))) ==
            MyList.Cons(1, MyList.Cons(2, MyList.Nil)))

    // --- Closure application on formal parameters ---

    @Terminates
    def applyParam(g: Int32 -> Int32, x: Int32): Int32 = g(x)

    @Test
    def testApplyParam01(): Unit \ Assert =
        assertTrue(applyParam(x -> x + 1, 5) == 6)

    @Terminates
    def applyCurriedParam(g: Int32 -> Int32 -> Int32, x: Int32, y: Int32): Int32 = g(x)(y)

    @Test
    def testApplyCurriedParam01(): Unit \ Assert =
        assertTrue(applyCurriedParam((x, y) -> x + y, 3, 4) == 7)

    @Terminates
    def applyLetAlias(g: Int32 -> Int32, x: Int32): Int32 =
        let h = g;
        h(x)

    @Test
    def testApplyLetAlias01(): Unit \ Assert =
        assertTrue(applyLetAlias(x -> x * 2, 5) == 10)

    @Terminates
    def applyDoubleLetAlias(g: Int32 -> Int32, x: Int32): Int32 =
        let h = g;
        let k = h;
        k(x)

    @Test
    def testApplyDoubleLetAlias01(): Unit \ Assert =
        assertTrue(applyDoubleLetAlias(x -> x * 3, 4) == 12)

    @Terminates
    def applyParamInMatch(g: Int32 -> Int32, x: MyList[Int32]): Int32 = match x {
        case MyList.Nil         => 0
        case MyList.Cons(h, _)  => g(h)
    }

    @Test
    def testApplyParamInMatch01(): Unit \ Assert =
        assertTrue(applyParamInMatch(x -> x + 10, MyList.Cons(5, MyList.Nil)) == 15)

    @Terminates
    def applyParamInIfThenElse(g: Int32 -> Int32, x: Int32): Int32 =
        if (true) g(x) else g(x)

    @Test
    def testApplyParamInIfThenElse01(): Unit \ Assert =
        assertTrue(applyParamInIfThenElse(x -> x - 1, 10) == 9)

    @Terminates
    def mapList(f: Int32 -> Int32, l: MyList[Int32]): MyList[Int32] = match l {
        case MyList.Nil         => MyList.Nil
        case MyList.Cons(x, xs) => MyList.Cons(f(x), mapList(f, xs))
    }

    @Test
    def testMapList01(): Unit \ Assert =
        assertTrue(mapList(x -> x * 2, MyList.Cons(1, MyList.Cons(2, MyList.Cons(3, MyList.Nil)))) ==
            MyList.Cons(2, MyList.Cons(4, MyList.Cons(6, MyList.Nil))))

    @Terminates
    def foldRight(f: Int32 -> Int32 -> Int32, z: Int32, l: MyList[Int32]): Int32 = match l {
        case MyList.Nil         => z
        case MyList.Cons(x, xs) => f(x)(foldRight(f, z, xs))
    }

    @Test
    def testFoldRight01(): Unit \ Assert =
        assertTrue(foldRight((x, y) -> x + y, 0, MyList.Cons(1, MyList.Cons(2, MyList.Cons(3, MyList.Nil)))) == 6)

    // --- Double negation in strict positivity (should pass) ---

    enum Good { case MkGood((Good -> Int32) -> Int32) }

    @Terminates
    def useGood(x: Good): Int32 = match x {
        case Good.MkGood(_) => 0
    }

    @Test
    def testDoubleNegation01(): Unit \ Assert =
        assertTrue(useGood(Good.MkGood(_ -> 0)) == 0)

}

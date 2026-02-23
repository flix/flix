mod Test.Terminator {

    // --- Basic data types ---
    enum MyList[a] with Eq { case Nil, case Cons(a, MyList[a]) }
    enum MyTree[a] with Eq { case Leaf(a), case Node(MyTree[a], MyTree[a]) }
    enum Nat with Eq { case Zero, case Succ(Nat) }
    enum MyOption[a] with Eq { case None, case Some(a) }

    // --- Structurally recursive functions on List ---

    @CompileTest @Terminates
    def length(l: MyList[Int32]): Int32 = match l {
        case MyList.Nil         => 0
        case MyList.Cons(_, xs) => 1 + length(xs)
    }

    @CompileTest @Terminates
    def sum(l: MyList[Int32]): Int32 = match l {
        case MyList.Nil         => 0
        case MyList.Cons(x, xs) => x + sum(xs)
    }

    @CompileTest @Terminates
    def append(l1: MyList[Int32], l2: MyList[Int32]): MyList[Int32] = match l1 {
        case MyList.Nil         => l2
        case MyList.Cons(x, xs) => MyList.Cons(x, append(xs, l2))
    }

    @CompileTest @Terminates
    def rev(l: MyList[Int32], acc: MyList[Int32]): MyList[Int32] = match l {
        case MyList.Nil         => acc
        case MyList.Cons(x, xs) => rev(xs, MyList.Cons(x, acc))
    }

    @CompileTest @Terminates
    def doubleAll(l: MyList[Int32]): MyList[Int32] = match l {
        case MyList.Nil         => MyList.Nil
        case MyList.Cons(x, xs) => MyList.Cons(x * 2, doubleAll(xs))
    }

    // --- Structurally recursive functions on Tree ---

    @CompileTest @Terminates
    def treeSize(t: MyTree[Int32]): Int32 = match t {
        case MyTree.Leaf(_)    => 1
        case MyTree.Node(l, r) => treeSize(l) + treeSize(r)
    }

    @CompileTest @Terminates
    def treeHeight(t: MyTree[Int32]): Int32 = match t {
        case MyTree.Leaf(_)    => 0
        case MyTree.Node(l, r) =>
            let lh = treeHeight(l);
            let rh = treeHeight(r);
            1 + (if (lh > rh) lh else rh)
    }

    @CompileTest @Terminates
    def treeMirror(t: MyTree[Int32]): MyTree[Int32] = match t {
        case MyTree.Leaf(v)    => MyTree.Leaf(v)
        case MyTree.Node(l, r) => MyTree.Node(treeMirror(r), treeMirror(l))
    }

    // --- Structurally recursive functions on Nat ---

    @CompileTest @Terminates
    def natToInt(n: Nat): Int32 = match n {
        case Nat.Zero    => 0
        case Nat.Succ(m) => 1 + natToInt(m)
    }

    @CompileTest @Terminates
    def natAdd(n: Nat, m: Nat): Nat = match n {
        case Nat.Zero    => m
        case Nat.Succ(p) => Nat.Succ(natAdd(p, m))
    }

    // --- Non-recursive (trivially terminating) ---

    @CompileTest @Terminates
    def trivial(x: Int32): Int32 = x

    @CompileTest @Terminates
    def constant(): Int32 = 7

    // --- Nested pattern matches ---

    @CompileTest @Terminates
    def dropTwo(l: MyList[Int32]): MyList[Int32] = match l {
        case MyList.Nil                            => MyList.Nil
        case MyList.Cons(_, MyList.Nil)            => MyList.Nil
        case MyList.Cons(_, MyList.Cons(_, rest))  => rest
    }

    // --- Let-bound alias ---

    @CompileTest @Terminates
    def lengthAlias(l: MyList[Int32]): Int32 = match l {
        case MyList.Nil         => 0
        case MyList.Cons(_, xs) =>
            let tail = xs;
            1 + lengthAlias(tail)
    }

    // --- If-then-else inside match ---

    @CompileTest @Terminates
    def filterPositive(l: MyList[Int32]): MyList[Int32] = match l {
        case MyList.Nil         => MyList.Nil
        case MyList.Cons(x, xs) =>
            if (x > 0) MyList.Cons(x, filterPositive(xs))
            else filterPositive(xs)
    }

    // --- Multiple parameters, only one decreasing ---

    @CompileTest @Terminates
    def take(n: Nat, l: MyList[Int32]): MyList[Int32] = match n {
        case Nat.Zero    => MyList.Nil
        case Nat.Succ(m) => match l {
            case MyList.Nil         => MyList.Nil
            case MyList.Cons(x, xs) => MyList.Cons(x, take(m, xs))
        }
    }

    // --- Option type ---

    @CompileTest @Terminates
    def optGetOrElse(o: MyOption[Int32], default: Int32): Int32 = match o {
        case MyOption.None    => default
        case MyOption.Some(v) => v
    }

    // --- Tuple-scrutinee structural recursion ---

    @CompileTest @Terminates
    def isPrefixOf(l1: MyList[Int32], l2: MyList[Int32]): Bool = match (l1, l2) {
        case (MyList.Nil, _)                          => true
        case (_, MyList.Nil)                           => false
        case (MyList.Cons(x, xs), MyList.Cons(y, ys)) =>
            if (x == y) isPrefixOf(xs, ys) else false
    }

    @CompileTest @Terminates
    def myZip(l1: MyList[Int32], l2: MyList[Int32]): MyList[(Int32, Int32)] = match (l1, l2) {
        case (MyList.Nil, _)                          => MyList.Nil
        case (_, MyList.Nil)                           => MyList.Nil
        case (MyList.Cons(x, xs), MyList.Cons(y, ys)) =>
            MyList.Cons((x, y), myZip(xs, ys))
    }

    @CompileTest @Terminates
    def natEq(n: Nat, m: Nat): Bool = match (n, m) {
        case (Nat.Zero, Nat.Zero)       => true
        case (Nat.Succ(a), Nat.Succ(b)) => natEq(a, b)
        case _                           => false
    }

    // --- Local defs ---

    @CompileTest @Terminates
    def localLength(l: MyList[Int32]): Int32 =
        def loop(ll: MyList[Int32], acc: Int32): Int32 = match ll {
            case MyList.Nil         => acc
            case MyList.Cons(_, xs) => loop(xs, acc + 1)
        };
        loop(l, 0)

    @CompileTest @Terminates
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

    @CompileTest @Terminates
    def nonRecursiveLocalDef(l: MyList[Int32]): Int32 =
        def helper(x: Int32): Int32 = x + 1;
        match l {
            case MyList.Nil         => 0
            case MyList.Cons(x, xs) => helper(x) + nonRecursiveLocalDef(xs)
        }

    // --- Local def calling outer function with strict sub ---

    @CompileTest @Terminates
    def localDefCallsOuter(l: MyList[Int32]): Int32 = match l {
        case MyList.Nil         => 0
        case MyList.Cons(_, xs) =>
            def helper(ll: MyList[Int32]): Int32 = match ll {
                case MyList.Nil         => localDefCallsOuter(xs)
                case MyList.Cons(_, ys) => helper(ys)
            };
            helper(xs)
    }

    // --- Trait default implementations ---

    trait ListLen[a] {
        @CompileTest @Terminates
        pub def listLen(l: MyList[a]): Int32 = match l {
            case MyList.Nil         => 0
            case MyList.Cons(_, xs) => 1 + ListLen.listLen(xs)
        }
    }

    instance ListLen[Int32]

    // --- Instance implementations ---

    trait MySize[a] {
        pub def mySize(x: a): Int32
    }

    instance MySize[MyList[a]] {
        @CompileTest @Terminates
        pub def mySize(x: MyList[a]): Int32 = match x {
            case MyList.Nil         => 0
            case MyList.Cons(_, xs) => 1 + MySize.mySize(xs)
        }
    }

    instance MySize[MyTree[a]] {
        @CompileTest @Terminates
        pub def mySize(x: MyTree[a]): Int32 = match x {
            case MyTree.Leaf(_)    => 1
            case MyTree.Node(l, r) => MySize.mySize(l) + MySize.mySize(r)
        }
    }

    // --- @Tailrec functions ---

    @CompileTest @Tailrec
    def tailRecLength(l: MyList[Int32], acc: Int32): Int32 = match l {
        case MyList.Nil         => acc
        case MyList.Cons(_, xs) => tailRecLength(xs, acc + 1)
    }

    @CompileTest @Tailrec
    def tailRecSum(l: MyList[Int32], acc: Int32): Int32 = match l {
        case MyList.Nil         => acc
        case MyList.Cons(x, xs) => tailRecSum(xs, acc + x)
    }

    @CompileTest @Tailrec
    def tailRecReverse(l: MyList[Int32], acc: MyList[Int32]): MyList[Int32] = match l {
        case MyList.Nil         => acc
        case MyList.Cons(x, xs) => tailRecReverse(xs, MyList.Cons(x, acc))
    }

    @CompileTest @Tailrec
    def tailRecIfBranch(n: Nat, acc: Int32): Int32 = match n {
        case Nat.Zero    => acc
        case Nat.Succ(m) =>
            if (acc > 100) acc
            else tailRecIfBranch(m, acc + 1)
    }

    // --- @Terminates calling another @Terminates def ---

    @CompileTest @Terminates
    def helperLen(l: MyList[Int32]): Int32 = match l {
        case MyList.Nil         => 0
        case MyList.Cons(_, xs) => 1 + helperLen(xs)
    }

    @CompileTest @Terminates
    def callsTerminating(l: MyList[Int32]): Int32 = match l {
        case MyList.Nil         => 0
        case MyList.Cons(_, xs) => helperLen(xs) + callsTerminating(xs)
    }

    // --- Nested constructor pattern with recursive call ---

    @CompileTest @Terminates
    def everyOther(l: MyList[Int32]): MyList[Int32] = match l {
        case MyList.Nil                           => MyList.Nil
        case MyList.Cons(x, MyList.Nil)           => MyList.Cons(x, MyList.Nil)
        case MyList.Cons(x, MyList.Cons(_, rest)) => MyList.Cons(x, everyOther(rest))
    }

    // --- Match guard with structural recursion ---

    @CompileTest @Terminates
    def takeWhilePositive(l: MyList[Int32]): MyList[Int32] = match l {
        case MyList.Cons(x, xs) if x > 0 => MyList.Cons(x, takeWhilePositive(xs))
        case _                            => MyList.Nil
    }

    // --- Closure application on formal parameters ---

    @CompileTest @Terminates
    def applyParam(g: Int32 -> Int32, x: Int32): Int32 = g(x)

    @CompileTest @Terminates
    def applyCurriedParam(g: Int32 -> Int32 -> Int32, x: Int32, y: Int32): Int32 = g(x)(y)

    @CompileTest @Terminates
    def applyLetAlias(g: Int32 -> Int32, x: Int32): Int32 =
        let h = g;
        h(x)

    @CompileTest @Terminates
    def applyDoubleLetAlias(g: Int32 -> Int32, x: Int32): Int32 =
        let h = g;
        let k = h;
        k(x)

    @CompileTest @Terminates
    def applyParamInMatch(g: Int32 -> Int32, x: MyList[Int32]): Int32 = match x {
        case MyList.Nil         => 0
        case MyList.Cons(h, _)  => g(h)
    }

    @CompileTest @Terminates
    def applyParamInIfThenElse(g: Int32 -> Int32, x: Int32): Int32 =
        if (true) g(x) else g(x)

    @CompileTest @Terminates
    def mapList(f: Int32 -> Int32, l: MyList[Int32]): MyList[Int32] = match l {
        case MyList.Nil         => MyList.Nil
        case MyList.Cons(x, xs) => MyList.Cons(f(x), mapList(f, xs))
    }

    @CompileTest @Terminates
    def foldRight(f: Int32 -> Int32 -> Int32, z: Int32, l: MyList[Int32]): Int32 = match l {
        case MyList.Nil         => z
        case MyList.Cons(x, xs) => f(x)(foldRight(f, z, xs))
    }

    // --- Double negation in strict positivity (should pass) ---

    enum Good { case MkGood((Good -> Int32) -> Int32) }

    @CompileTest @Terminates
    def useGood(x: Good): Int32 = match x {
        case Good.MkGood(_) => 0
    }

    // --- @Tailrec on local defs ---

    @CompileTest
    def tailRecLocalLoop01(l: MyList[Int32]): Int32 =
        @Tailrec
        def loop(ll: MyList[Int32], acc: Int32): Int32 = match ll {
            case MyList.Nil         => acc
            case MyList.Cons(_, xs) => loop(xs, acc + 1)
        };
        loop(l, 0)

    // --- @Terminates on standalone local defs ---

    @CompileTest
    def standaloneTerminatesLocal01(l: MyList[Int32]): Int32 =
        @Terminates
        def loop(ll: MyList[Int32]): Int32 = match ll {
            case MyList.Nil         => 0
            case MyList.Cons(_, xs) => 1 + loop(xs)
        };
        loop(l)

    // --- @Terminates @Tailrec combined on local defs ---

    @CompileTest @Terminates
    def combinedAnnotationsLocal01(l: MyList[Int32]): Int32 =
        @Terminates @Tailrec
        def loop(ll: MyList[Int32], acc: Int32): Int32 = match ll {
            case MyList.Nil         => acc
            case MyList.Cons(_, xs) => loop(xs, acc + 1)
        };
        loop(l, 0)

    // --- @Tailrec local def inside @Terminates function ---

    @CompileTest @Terminates
    def tailRecInsideTerminates01(l: MyList[Int32]): Int32 =
        @Tailrec
        def loop(ll: MyList[Int32], acc: Int32): Int32 = match ll {
            case MyList.Nil         => acc
            case MyList.Cons(_, xs) => loop(xs, acc + 1)
        };
        loop(l, 0)

    // --- @Tailrec local def with external call in let-binding ---

    @CompileTest @Terminates
    def tailRecLocalLetBound01(l: MyList[(Int32, Int32)]): (MyList[Int32], MyList[Int32]) =
        @Terminates @Tailrec
        def loop(ll: MyList[(Int32, Int32)], acc1: MyList[Int32], acc2: MyList[Int32]): (MyList[Int32], MyList[Int32]) = match ll {
            case MyList.Nil              => (acc1, acc2)
            case MyList.Cons((x1, x2), xs) => loop(xs, MyList.Cons(x1, acc1), MyList.Cons(x2, acc2))
        };
        let (l1, l2) = loop(l, MyList.Nil, MyList.Nil);
        (l1, l2)

    @CompileTest
    def tailRecLocalLetBound02(l: MyList[Int32]): Int32 =
        @Tailrec
        def loop(ll: MyList[Int32], acc: Int32): Int32 = match ll {
            case MyList.Nil         => acc
            case MyList.Cons(x, xs) => loop(xs, acc + x)
        };
        let result = loop(l, 0);
        result + 1

    // --- @Terminates @Tailrec combined on top-level def ---

    @CompileTest @Terminates @Tailrec
    def tailRecTerminatingLength(l: MyList[Int32], acc: Int32): Int32 = match l {
        case MyList.Nil         => acc
        case MyList.Cons(_, xs) => tailRecTerminatingLength(xs, acc + 1)
    }

    // --- Heterogeneous tuple scrutinee ---

    @CompileTest @Terminates
    def heteroTupleDecrease(n: Nat, l: MyList[Int32]): Int32 = match (n, l) {
        case (Nat.Zero, _)                          => 0
        case (_, MyList.Nil)                        => 0
        case (Nat.Succ(m), MyList.Cons(x, xs))      => x + heteroTupleDecrease(m, xs)
    }

    // --- Multiple decreasing parameters ---

    @CompileTest @Terminates
    def bothDecrease(l1: MyList[Int32], l2: MyList[Int32]): Int32 = match (l1, l2) {
        case (MyList.Nil, _)                          => 0
        case (_, MyList.Nil)                          => 0
        case (MyList.Cons(_, xs), MyList.Cons(_, ys)) => 1 + bothDecrease(xs, ys)
    }

    // --- Recursive call in let-binding position ---

    @CompileTest @Terminates
    def recursionInLet(l: MyList[Int32]): Int32 = match l {
        case MyList.Nil         => 0
        case MyList.Cons(x, xs) =>
            let rest = recursionInLet(xs);
            x + rest
    }

    // --- Let-chain alias propagation ---

    @CompileTest @Terminates
    def letChainAlias(l: MyList[Int32]): Int32 = match l {
        case MyList.Nil         => 0
        case MyList.Cons(_, xs) =>
            let ys = xs;
            let zs = ys;
            1 + letChainAlias(zs)
    }

    // --- Lambda inside @Terminates body ---

    @CompileTest @Terminates
    def lambdaInTerminates(x: Int32): Int32 -> Int32 = (y: Int32) -> x + y

    // --- Nested tuple scrutinee ---

    @CompileTest @Terminates
    def nestedTupleDecrease(l1: MyList[Int32], l2: MyList[Int32], q: MyList[Int32]): Int32 =
        match ((l1, l2), q) {
            case ((MyList.Nil, _), _)                  => 0
            case ((MyList.Cons(_, xs), ys), _)         => 1 + nestedTupleDecrease(xs, ys, q)
            case _                                     => 0
        }

    // --- Type ascription inside @Terminates ---

    @CompileTest @Terminates
    def ascribeInTerminates(l: MyList[Int32]): Int32 = match l {
        case MyList.Nil         => (0 : Int32)
        case MyList.Cons(_, xs) => (1 + ascribeInTerminates(xs) : Int32)
    }

    // --- Non-recursive @Terminates calling @Terminates ---

    @CompileTest @Terminates
    def nonRecCallsTerminating(l: MyList[Int32]): Int32 = helperLen(l)

}

mod Test.Eff.SubEffecting {

    use Assert.assertEq

    ///
    /// Sub-effecting lets a lambda or local def have a larger effect than its body.
    ///
    /// Every test except those in the last two groups is rejected without sub-effecting. The tests are grouped
    /// by where the widening happens:
    ///
    ///   - A pure lambda or local def passed where an `IO` function is required.
    ///   - A def or signature passed as a function value; it is eta-expanded to a lambda.
    ///   - A body that is not pure, widened to a larger effect.
    ///   - A lambda widened to a rigid effect variable of the enclosing def. This is the shape of the
    ///     `checked_ecast`s that sub-effecting makes redundant in the standard library.
    ///   - Lambdas with different effects unified with each other, with no declared target.
    ///   - A declared function type in a position other than a parameter: return type, ascription,
    ///     enum case, struct field.
    ///   - Curried lambdas: the inner arrow widens; the outer arrow does not sub-effect.
    ///   - User-defined effects, under a handler.
    ///   - Lambdas inside an instance def and a trait default def.
    ///   - Regions: a region absorbs the declared effect of its def, and a region effect is widened.
    ///   - Recursive local defs: the effect variable of the def appears in its own bound. These compile
    ///     without sub-effecting too.
    ///   - Programs that sub-effecting must keep accepting: the shapes that failed before #13583 and
    ///     #13585. These compile without sub-effecting too.
    ///

    def requiresPure(f: Int32 -> Int32): Int32 = f(1)

    def requiresPure2(f: (Int32, Int32) -> Int32): Int32 = f(1, 2)

    def requiresIO(f: Int32 -> Int32 \ IO): Int32 \ IO = f(1)

    def requiresIO2(f: (Int32, Int32) -> Int32 \ IO): Int32 \ IO = f(1, 2)

    def requiresIOPair(f: ((Int32, Int32)) -> Int32 \ IO): Int32 \ IO = f((1, 2))

    eff Log {
        def log(s: String): Unit
    }

    eff Ask {
        def ask(): Int32
    }

    /////////////////////////////////////////////////////////////////////////////
    // Lambdas and local defs                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testLambda01(): Unit \ Assert + IO =
        assertEq(expected = 2, requiresIO(x -> x + 1))

    @Test
    def testLambda02(): Unit \ Assert + IO =
        assertEq(expected = 3, requiresIO2((x, y) -> x + y))

    @Test
    def testLambda03(): Unit \ Assert + IO =
        let f = x -> x + 1;
        assertEq(expected = 2, requiresIO(f))

    @Test
    def testLambda04(): Unit \ Assert + IO =
        let f = (x, y) -> x + y;
        assertEq(expected = 3, requiresIO2(f))

    @Test
    def testLambda05(): Unit \ Assert + IO =
        let f = x -> x;
        assertEq(expected = 1, requiresIO(f))

    @Test
    def testLambda06(): Unit \ Assert + IO =
        assertEq(expected = 3, requiresIOPair(match (x, y) -> x + y))

    @Test
    def testLambda07(): Unit \ Assert + IO =
        // One lambda used at two sites that require IO.
        let f = x -> x + 1;
        assertEq(expected = 4, requiresIO(f) + requiresIO(f))

    @Test
    def testLocalDef01(): Unit \ Assert + IO =
        def inc(x) = x + 1;
        assertEq(expected = 2, requiresIO(inc))

    @Test
    def testLocalDef02(): Unit \ Assert + IO =
        def add(x, y) = x + y;
        assertEq(expected = 3, requiresIO2(add))

    @Test
    def testLocalDef03(): Unit \ Assert + IO =
        def inc(x) = x + 1;
        assertEq(expected = 2, requiresIO(x -> inc(x)))

    @Test
    def testLocalDef04(): Unit \ Assert + IO =
        // One local def used at two sites that require IO.
        def inc(x) = x + 1;
        assertEq(expected = 4, requiresIO(inc) + requiresIO(inc))

    @Test
    def testLocalDef05(): Unit \ Assert + IO =
        // A local def that calls another local def. Only the outer one is used where IO is required.
        def inc(x) = x + 1;
        def twice(x) = inc(inc(x));
        assertEq(expected = 3, requiresIO(twice))

    /////////////////////////////////////////////////////////////////////////////
    // Defs and signatures passed as function values                           //
    /////////////////////////////////////////////////////////////////////////////

    def inc(x: Int32): Int32 = x + 1

    @Test
    def testDefReference01(): Unit \ Assert + IO =
        assertEq(expected = 2, requiresIO(inc))

    @Test
    def testDefReference02(): Unit \ Assert + IO =
        assertEq(expected = 1, requiresIO(Int32.abs))

    @Test
    def testDefReference03(): Unit \ Assert + IO =
        assertEq(expected = 3, requiresIO2(Add.add))

    /////////////////////////////////////////////////////////////////////////////
    // A body that is not pure                                                 //
    /////////////////////////////////////////////////////////////////////////////

    def requiresIOLog(f: Int32 -> Int32 \ IO + Log): Int32 \ IO + Log = f(1)

    def requiresLogAsk(f: Int32 -> Int32 \ Log + Ask): Int32 \ Log + Ask = f(Ask.ask())

    @Test
    def testNonPureBody01(): Unit \ Assert + IO =
        // The body has effect Log; the lambda is widened to IO + Log.
        let r = run {
            requiresIOLog(x -> { Log.log("x"); x + 1 })
        } with handler Log {
            def log(_, k) = k()
        };
        assertEq(expected = 2, r)

    @Test
    def testNonPureBody02(): Unit \ Assert + IO =
        // The body has effect IO; the lambda is widened to IO + Log.
        let cell = Ref.fresh(Static, 0);
        let r = run {
            requiresIOLog(x -> { Ref.put(x, cell); x + 1 })
        } with handler Log {
            def log(_, k) = k()
        };
        assertEq(expected = (2, 1), (r, Ref.get(cell)))

    @Test
    def testNonPureBody03(): Unit \ Assert =
        // The body has effect Log; the lambda is widened to Log + Ask.
        let r = run {
            requiresLogAsk(x -> { Log.log("x"); x + 1 })
        } with handler Log {
            def log(_, k) = k()
        } with handler Ask {
            def ask(k) = k(41)
        };
        assertEq(expected = 42, r)

    @Test
    def testNonPureBody04(): Unit \ Assert + IO =
        // The body of a local def has effect Log; the local def is widened to IO + Log.
        def logged(x) = { Log.log("x"); x + 1 };
        let r = run {
            requiresIOLog(logged)
        } with handler Log {
            def log(_, k) = k()
        };
        assertEq(expected = 2, r)

    /////////////////////////////////////////////////////////////////////////////
    // A rigid effect variable                                                 //
    /////////////////////////////////////////////////////////////////////////////

    def both(f: Int32 -> Int32 \ ef, g: Int32 -> Int32 \ ef): Int32 \ ef = f(g(1))

    def useBoth(f: Int32 -> Int32 \ ef): Int32 \ ef = both(f, x -> x + 1)

    def useBothLocalDef(f: Int32 -> Int32 \ ef): Int32 \ ef =
        def inc(x) = x + 1;
        both(f, inc)

    def constant2(x: a): b -> a \ ef = _ -> x

    def loopK(l: List[Int32], f: Int32 -> Int32 \ ef, k: List[Int32] -> List[Int32] \ ef): List[Int32] \ ef = match l {
        case Nil     => k(Nil)
        case x :: xs => loopK(xs, f, acc -> k(f(x) :: acc))
    }

    def mapK(f: Int32 -> Int32 \ ef, l: List[Int32]): List[Int32] \ ef = loopK(l, f, x -> x)

    def pair(f1: Unit -> Int32 \ ef1, f2: Unit -> Int32 \ ef2): (Unit -> Int32 \ ef1 + ef2, Unit -> Int32 \ ef1 + ef2) =
        let g1 = () -> f1();
        let g2 = () -> f2();
        (g1, g2)

    @Test
    def testRigidEffectVar01(): Unit \ Assert =
        // The lambda in `useBoth` is widened to `ef`, here instantiated to Pure.
        assertEq(expected = 3, useBoth(x -> x + 1))

    @Test
    def testRigidEffectVar02(): Unit \ Assert = region rc {
        // The lambda in `useBoth` is widened to `ef`, here instantiated to the region.
        let r = Ref.fresh(rc, 0);
        let v = useBoth(x -> { Ref.put(x, r); x });
        assertEq(expected = (2, 2), (v, Ref.get(r)))
    }

    @Test
    def testRigidEffectVar03(): Unit \ Assert =
        // `Prelude.constant` without its cast: the lambda is returned at `ef`.
        let f: Int32 -> Int32 = constant2(42);
        assertEq(expected = 42, f(0))

    @Test
    def testRigidEffectVar04(): Unit \ Assert + IO =
        let f: Int32 -> Int32 \ IO = constant2(42);
        assertEq(expected = 42, f(0))

    @Test
    def testRigidEffectVar05(): Unit \ Assert =
        // The identity continuation of a CPS loop, as in List, Nec and Chain.
        assertEq(expected = 2 :: 3 :: 4 :: Nil, mapK(x -> x + 1, 1 :: 2 :: 3 :: Nil))

    @Test
    def testRigidEffectVar06(): Unit \ Assert = region rc {
        let r = Ref.fresh(rc, 0);
        let l = mapK(x -> { Ref.put(Ref.get(r) + x, r); x }, 1 :: 2 :: 3 :: Nil);
        assertEq(expected = (1 :: 2 :: 3 :: Nil, 6), (l, Ref.get(r)))
    }

    @Test
    def testRigidEffectVar07(): Unit \ Assert =
        // A body with rigid effect `ef1` widened to `ef1 + ef2`, as in Iterator.
        let (a, b) = pair(() -> 1, () -> 2);
        assertEq(expected = 3, a() + b())

    @Test
    def testRigidEffectVar08(): Unit \ Assert =
        // The local def in `useBothLocalDef` is widened to `ef`, here instantiated to Pure.
        assertEq(expected = 3, useBothLocalDef(x -> x + 1))

    @Test
    def testRigidEffectVar09(): Unit \ Assert = region rc {
        // The local def in `useBothLocalDef` is widened to `ef`, here instantiated to the region.
        let r = Ref.fresh(rc, 0);
        let v = useBothLocalDef(x -> { Ref.put(x, r); x });
        assertEq(expected = (2, 2), (v, Ref.get(r)))
    }

    /////////////////////////////////////////////////////////////////////////////
    // Lambdas with different effects unified with each other                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testMixedEffects01(): Unit \ Assert + IO =
        // The branches of an if-then-else: a pure lambda and an IO lambda.
        let cell = Ref.fresh(Static, 0);
        let f = x -> x + 1;
        let g = x -> { Ref.put(x, cell); x };
        let h = if (Ref.get(cell) == 0) f else g;
        assertEq(expected = 2, h(1))

    @Test
    def testMixedEffects02(): Unit \ Assert + IO =
        // The elements of a list.
        let cell = Ref.fresh(Static, 0);
        let fs = (x -> x + 1) :: (x -> { Ref.put(x, cell); x }) :: Nil;
        assertEq(expected = 3, List.foldLeft((acc, k) -> acc + k(1), 0, fs))

    @Test
    def testMixedEffects03(): Unit \ Assert + IO =
        // The arms of a match.
        let cell = Ref.fresh(Static, 0);
        let h = match Ref.get(cell) {
            case 0 => x -> x + 1
            case _ => x -> { Ref.put(x, cell); x }
        };
        assertEq(expected = 2, h(1))

    @Test
    def testMixedEffects04(): Unit \ Assert = region rc {
        // Inside a region: a pure lambda and a lambda that writes to the region.
        let r = Ref.fresh(rc, 0);
        let f = x -> x + 1;
        let g = x -> { Ref.put(x, r); x };
        let h = if (Ref.get(r) == 0) f else g;
        assertEq(expected = 2, h(1))
    }

    /////////////////////////////////////////////////////////////////////////////
    // A declared function type in another position                            //
    /////////////////////////////////////////////////////////////////////////////

    enum Callback {
        case Callback(Int32 -> Int32 \ IO)
    }

    struct Callbacks[r] {
        cb: Int32 -> Int32 \ IO
    }

    mod Callbacks {
        pub def make(rc: Region[r]): Callbacks[r] \ r = new Callbacks @ rc { cb = x -> x + 1 }

        pub def call(c: Callbacks[r]): Int32 \ IO = (c->cb)(1)
    }

    def mkInc(): Int32 -> Int32 \ IO = x -> x + 1

    def mkInc2(): Int32 -> Int32 \ IO =
        def step(x) = x + 1;
        step

    @Test
    def testDeclaredType01(): Unit \ Assert + IO =
        // The return type of a def.
        assertEq(expected = 2, (mkInc())(1))

    @Test
    def testDeclaredType02(): Unit \ Assert + IO =
        // The return type of a def, returning a local def.
        assertEq(expected = 2, (mkInc2())(1))

    @Test
    def testDeclaredType03(): Unit \ Assert + IO =
        // A type ascription on a let.
        let f: Int32 -> Int32 \ IO = x -> x + 1;
        assertEq(expected = 2, f(1))

    @Test
    def testDeclaredType04(): Unit \ Assert + IO =
        // A type ascription on an expression.
        let f = ((x -> x + 1) : (Int32 -> Int32 \ IO));
        assertEq(expected = 2, f(1))

    @Test
    def testDeclaredType05(): Unit \ Assert + IO =
        // An enum case.
        match Callback.Callback(x -> x + 1) {
            case Callback.Callback(k) => assertEq(expected = 2, k(1))
        }

    @Test
    def testDeclaredType06(): Unit \ Assert + IO = region rc {
        // A struct field.
        assertEq(expected = 2, Callbacks.call(Callbacks.make(rc)))
    }

    @Test
    def testDeclaredType07(): Unit \ Assert + IO =
        // A type ascription on a let, binding a local def.
        def inc(x) = x + 1;
        let f: Int32 -> Int32 \ IO = inc;
        assertEq(expected = 2, f(1))

    /////////////////////////////////////////////////////////////////////////////
    // Curried lambdas                                                         //
    /////////////////////////////////////////////////////////////////////////////

    def requiresCurried(f: Int32 -> (Int32 -> Int32 \ IO)): Int32 \ IO = f(1)(2)

    @Test
    def testCurried01(): Unit \ Assert + IO =
        // The outer lambda has a lambda as its body and keeps its effect; the inner lambda is widened.
        assertEq(expected = 3, requiresCurried(x -> y -> x + y))

    @Test
    def testCurried02(): Unit \ Assert + IO =
        assertEq(expected = 3, requiresCurried((x, y) -> x + y))

    @Test
    def testCurried03(): Unit \ Assert + IO =
        // A local def whose body is a lambda. The lambda is widened to IO.
        def adder(x) = y -> x + y;
        assertEq(expected = 3, requiresCurried(adder))

    /////////////////////////////////////////////////////////////////////////////
    // User-defined effects                                                    //
    /////////////////////////////////////////////////////////////////////////////

    def requiresAsk(f: Int32 -> Int32 \ Ask): Int32 \ Ask = f(Ask.ask())

    def hof1(f: Unit -> Unit \ IO): Unit \ IO = f()

    def hof2(f: Unit -> Unit \ ef): Unit \ ef = f()

    def hofLog(f: Unit -> Unit \ {ef, Log}): Unit \ {ef, Log} = f()

    def ioAndLog(): Int32 \ IO + Log =
        Log.log("a");
        let f = () -> ();
        hof1(f);
        hof2(f);
        1

    def logOnly(): Int32 =
        run {
            hofLog(x -> x);
            2
        } with handler Log {
            def log(_, k) = k()
        }

    @Test
    def testUserEffect01(): Unit \ Assert =
        // A pure lambda where Ask is required, handled in a pure def.
        let r = run {
            requiresAsk(x -> x + 1)
        } with handler Ask {
            def ask(k) = k(41)
        };
        assertEq(expected = 42, r)

    @Test
    def testUserEffect02(): Unit \ Assert + IO =
        // One pure lambda passed to an IO function and to an effect-polymorphic function.
        // Rejected before sub-effecting as Test.ArgumentGivenWrongEffect.05.
        let r = run {
            ioAndLog()
        } with handler Log {
            def log(_, k) = k()
        };
        assertEq(expected = 1, r)

    @Test
    def testUserEffect03(): Unit \ Assert =
        // A pure lambda widened to {ef, Log}. Rejected before sub-effecting as Test.UnexpectedArgument.03.
        assertEq(expected = 2, logOnly())

    @Test
    def testUserEffect04(): Unit \ Assert =
        // A pure local def where Ask is required, handled in a pure def.
        def inc(x) = x + 1;
        let r = run {
            requiresAsk(inc)
        } with handler Ask {
            def ask(k) = k(41)
        };
        assertEq(expected = 42, r)

    /////////////////////////////////////////////////////////////////////////////
    // Instance defs and trait default defs                                    //
    /////////////////////////////////////////////////////////////////////////////

    trait Area[a] {
        pub def area(x: a): Int32 \ IO

        pub def two(_x: a): Int32 \ IO = requiresIO(y -> y + y)
    }

    instance Area[Int32] {
        pub def area(x: Int32): Int32 \ IO = requiresIO(y -> y * x)
    }

    @Test
    def testInstanceDef01(): Unit \ Assert + IO =
        assertEq(expected = 21, Area.area(21))

    @Test
    def testTraitDefaultDef01(): Unit \ Assert + IO =
        assertEq(expected = 2, Area.two(21))

    /////////////////////////////////////////////////////////////////////////////
    // Regions                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    def regionAbsorbsIO01(): Int32 \ IO = region rc {
        let r = Ref.fresh(rc, 0);
        List.forEach(x -> Ref.put(x, r), 1 :: 2 :: Nil);
        Ref.get(r)
    }

    def regionAbsorbsIO02(): Int32 \ IO = region rc {
        let r = Ref.fresh(rc, 0);
        foreach (x <- 1 :: 2 :: Nil) Ref.put(x, r);
        Ref.get(r)
    }

    def requiresRegionIO(_rc: Region[r], f: Int32 -> Unit \ {IO, r}): Unit \ {IO, r} = f(1)

    @Test
    def testRegionAbsorbsIO01(): Unit \ Assert + IO =
        assertEq(expected = 2, regionAbsorbsIO01())

    @Test
    def testRegionAbsorbsIO02(): Unit \ Assert + IO =
        assertEq(expected = 2, regionAbsorbsIO02())

    @Test
    def testRegionEffectWidened01(): Unit \ Assert + IO = region rc {
        // The body has the effect of the region; the lambda is widened to IO + rc.
        let r = Ref.fresh(rc, 0);
        requiresRegionIO(rc, x -> Ref.put(x, r));
        assertEq(expected = 1, Ref.get(r))
    }

    @Test
    def testRegionEffectWidened02(): Unit \ Assert + IO = region rc {
        // The body of a local def has the effect of the region; the local def is widened to IO + rc.
        let r = Ref.fresh(rc, 0);
        def put(x) = Ref.put(x, r);
        requiresRegionIO(rc, put);
        assertEq(expected = 1, Ref.get(r))
    }

    /////////////////////////////////////////////////////////////////////////////
    // Recursive local defs                                                    //
    //                                                                         //
    // These compile without sub-effecting too: a recursive call has the       //
    // effect of the local def itself, so the effect of the def is already     //
    // open and is fixed by its use. With sub-effecting the effect variable of //
    // the def appears on both sides of its own bound.                         //
    /////////////////////////////////////////////////////////////////////////////

    def useBothRecursive(f: Int32 -> Int32 \ ef): Int32 \ ef =
        def loop(n) = if (n == 0) 0 else loop(n - 1);
        both(f, loop)

    @Test
    def testRecursiveLocalDef01(): Unit \ Assert + IO =
        def loop(n) = if (n == 0) 0 else loop(n - 1);
        assertEq(expected = 0, requiresIO(loop))

    @Test
    def testRecursiveLocalDef02(): Unit \ Assert + IO =
        // Two parameters, with an accumulator.
        def loop(n, acc) = if (n == 0) acc else loop(n - 1, acc + n);
        assertEq(expected = 3, requiresIO2(loop))

    @Test
    def testRecursiveLocalDef03(): Unit \ Assert + IO =
        // Not tail recursive.
        def sum(n) = if (n == 0) 0 else n + sum(n - 1);
        assertEq(expected = 1, requiresIO(sum))

    @Test
    def testRecursiveLocalDef04(): Unit \ Assert + IO =
        // A recursive local def that calls another local def.
        def inc(x) = x + 1;
        def loop(n) = if (n == 0) 0 else inc(loop(n - 1));
        assertEq(expected = 1, requiresIO(loop))

    @Test
    def testRecursiveLocalDef05(): Unit \ Assert + IO =
        // The body has effect Log; the local def is widened to IO + Log.
        def loop(n) = if (n == 0) { Log.log("done"); 0 } else 1 + loop(n - 1);
        let r = run {
            requiresIOLog(loop)
        } with handler Log {
            def log(_, k) = k()
        };
        assertEq(expected = 1, r)

    @Test
    def testRecursiveLocalDef06(): Unit \ Assert =
        // The recursive local def in `useBothRecursive` is widened to `ef`, here instantiated to Pure.
        assertEq(expected = 1, useBothRecursive(x -> x + 1))

    @Test
    def testRecursiveLocalDef07(): Unit \ Assert + IO = region rc {
        // The body has the effect of the region; the local def is widened to IO + rc.
        let r = Ref.fresh(rc, 0);
        def put(n) = if (n == 0) () else { Ref.put(Ref.get(r) + n, r); put(n - 1) };
        requiresRegionIO(rc, put);
        assertEq(expected = 1, Ref.get(r))
    }

    /////////////////////////////////////////////////////////////////////////////
    // Programs that sub-effecting must keep accepting                         //
    //                                                                         //
    // These compile without sub-effecting too. With it, every lambda inside a //
    // region leaves an effect variable in the effect equation of the region.  //
    // Twelve or more such lambdas failed until #13585; a pure lambda bound    //
    // outside the region and used inside it failed until #13583.              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testManyLambdasInRegion12(): Unit \ Assert = region rc {
        let l = 1 :: 2 :: 3 :: Nil;
        let r = Ref.fresh(rc, 0);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        assertEq(expected = 72, Ref.get(r))
    }

    @Test
    def testManyLambdasInRegion16(): Unit \ Assert = region rc {
        let l = 1 :: 2 :: 3 :: Nil;
        let r = Ref.fresh(rc, 0);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        assertEq(expected = 96, Ref.get(r))
    }

    @Test
    def testManyLambdasInRegion20(): Unit \ Assert = region rc {
        let l = 1 :: 2 :: 3 :: Nil;
        let r = Ref.fresh(rc, 0);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        List.forEach(x -> Ref.put(Ref.get(r) + x, r), l);
        assertEq(expected = 120, Ref.get(r))
    }

    @Test
    def testForeachInRegion01(): Unit \ Assert = region rc {
        let l = 1 :: 2 :: 3 :: Nil;
        let k = 4 :: 5 :: 6 :: Nil;
        let r = Ref.fresh(rc, 0);
        foreach (x <- l) Ref.put(Ref.get(r) + x, r);
        foreach (x <- l) Ref.put(Ref.get(r) + x, r);
        List.iterator(rc, l) |> Iterator.forEach(match x -> Ref.put(Ref.get(r) + x, r));
        foreach (a <- l) {
            Ref.put(Ref.get(r) + a, r);
            foreach (b <- k) Ref.put(Ref.get(r) + b, r)
        };
        List.iterator(rc, l) |> Iterator.forEach(match a -> {
            Ref.put(Ref.get(r) + a, r);
            List.iterator(rc, k) |> Iterator.forEach(match b -> Ref.put(Ref.get(r) + b, r))
        });
        foreach (a <- l; b <- k) Ref.put(Ref.get(r) + a + b, r);
        List.iterator(rc, l) |> Iterator.forEach(match a ->
            List.iterator(rc, k) |> Iterator.forEach(match b -> Ref.put(Ref.get(r) + a + b, r)));
        foreach (a <- l; if a > 1; b <- k; if b < 6) Ref.put(Ref.get(r) + a + b, r);
        foreach (a <- l; b <- k; if a > 1 and b < 6) Ref.put(Ref.get(r) + a + b, r);
        List.iterator(rc, l) |> Iterator.forEach(match a ->
            List.iterator(rc, k) |> Iterator.forEach(match b ->
                if (a > 1 and b < 6) Ref.put(Ref.get(r) + a + b, r) else ()));
        assertEq(expected = 6 + 6 + 6 + (6 + 45) + (6 + 45) + 63 + 63 + 28 + 28 + 28, Ref.get(r))
    }

    @Test
    def testOuterLambdaInRegion01(): Unit \ Assert =
        // The body is syntactically pure and nothing outside the region constrains the effect of the lambda.
        let f = (x: Int32) -> x;
        region rc {
            let r = Ref.fresh(rc, 0);
            Ref.put(requiresPure(f), r);
            assertEq(expected = 1, Ref.get(r))
        }

    @Test
    def testOuterLambdaInRegion02(): Unit \ Assert =
        // The outer layer of a curried lambda has a lambda as its body, which is syntactically pure.
        let f = (x: Int32, y: Int32) -> x + y;
        region rc {
            let r = Ref.fresh(rc, 0);
            Ref.put(requiresPure2(f), r);
            assertEq(expected = 3, Ref.get(r))
        }

    @Test
    def testOuterLambdaInRegion03(): Unit \ Assert =
        // The use of the outer lambda is itself inside a lambda inside the region.
        let f = (x: Int32, y: Int32) -> x + y;
        region rc {
            let r = Ref.fresh(rc, 0);
            List.forEach(_ -> Ref.put(Ref.get(r) + requiresPure2(f), r), 1 :: 2 :: Nil);
            assertEq(expected = 6, Ref.get(r))
        }

    @Test
    def testOuterLambdaInRegion04(): Unit \ Assert =
        // The outer lambda captures a variable and is used inside a lambda inside a region inside another lambda.
        let height = 2;
        let src = (x: Int32, y: Int32) -> if (y < 0 or y >= height or x < 0) 0 else x + y;
        let rows = List.range(0, height) |> List.map(y ->
            region rc {
                let r = Ref.fresh(rc, 0);
                List.forEach(x -> Ref.put(Ref.get(r) + requiresPure2(src) + x + y, r), 0 :: 1 :: Nil);
                Ref.get(r)
            });
        assertEq(expected = 1 :: 3 :: Nil, rows)

}

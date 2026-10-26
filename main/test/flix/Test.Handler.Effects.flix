mod Test.Handler.Effects {

    use Assert.assertEq;

    eff Ask {
        def ask(): Int32
        def fail(m: String): Int32
    }

    eff Big {
        def op01(x: Int32): Int32
        def op02(x: Int32): Int32
        def op03(x: Int32): Int32
        def op04(x: Int32): Int32
        def op05(x: Int32): Int32
        def op06(x: Int32): Int32
        def op07(x: Int32): Int32
        def op08(x: Int32): Int32
        def op09(x: Int32): Int32
        def op10(x: Int32): Int32
        def op11(x: Int32): Int32
        def op12(x: Int32): Int32
    }

    ///
    /// A handler with twelve rules whose body effects include the rigid effect of `g`.
    ///
    /// The continuation effect equation of the handler mentions every rule.
    ///
    def runBig(g: Unit -> Unit \ ef2, f: Unit -> a \ ef): a \ (ef - Big) + ef2 =
        run { f() } with handler Big {
            def op01(x, k) = { g(); k(x + 1) }
            def op02(x, k) = { g(); k(x + 2) }
            def op03(x, k) = { g(); k(x + 3) }
            def op04(x, k) = { g(); k(x + 4) }
            def op05(x, k) = k(x + 5)
            def op06(x, k) = k(x + 6)
            def op07(x, k) = { g(); g(); k(x + 7) }
            def op08(x, k) = { g(); k(x + 8) }
            def op09(x, k) = { g(); k(x + 9) }
            def op10(x, k) = { g(); k(x + 10) }
            def op11(x, k) = { g(); k(x + 11) }
            def op12(x, k) = { g(); k(x + 12) }
        }

    ///
    /// An aborting rule, whose continuation is a wildcard, next to a resuming rule.
    ///
    def runAbort(f: Unit -> Int32 \ ef): Int32 \ (ef - Ask) =
        run { f() } with handler Ask {
            def ask(k) = k(42)
            def fail(_m, _k) = -1
        }

    ///
    /// A rule that mentions its continuation without calling it: its body is pure while the
    /// continuation effect includes the rigid effect of `g`.
    ///
    def runStore(g: Unit -> Unit \ ef2, f: Unit -> Int32 \ ef): Int32 \ (ef - Ask) + ef2 =
        run { f() } with handler Ask {
            def ask(k) = { let _f = k; 7 }
            def fail(m, k) = { g(); k(String.length(m)) }
        }

    ///
    /// Rules with different effects: one pure apart from the resumption, one with the effect of `g`.
    ///
    def runMixed(g: Unit -> Unit \ ef2, f: Unit -> Int32 \ ef): Int32 \ (ef - Ask) + ef2 =
        run { f() } with handler Ask {
            def ask(k) = k(1)
            def fail(m, k) = { g(); k(String.length(m)) }
        }

    ///
    /// A handler inside a region whose rules read and write a reference of the region.
    ///
    def inRegion(): Int32 = region rc {
        let r = Ref.fresh(rc, 0);
        run { Ask.ask() + Ask.ask() } with handler Ask {
            def ask(k) = { Ref.put(Ref.get(r) + 1, r); k(Ref.get(r)) }
            def fail(_m, _k) = -1
        }
    }

    @Test
    def testBig01(): Unit \ Assert =
        assertEq(expected = 15, runBig(() -> (), () -> Big.op01(1) + Big.op12(1)))

    @Test
    def testBig02(): Unit \ Assert =
        assertEq(expected = 78, runBig(() -> (), () -> Big.op01(Big.op02(Big.op03(Big.op04(Big.op05(Big.op06(Big.op07(Big.op08(Big.op09(Big.op10(Big.op11(Big.op12(0))))))))))))))

    @Test
    def testBig03(): Unit \ Assert = region rc {
        let r = Ref.fresh(rc, 0);
        let g = () -> Ref.put(Ref.get(r) + 1, r);
        let v = runBig(g, () -> Big.op07(Big.op05(0)));
        assertEq(expected = (12, 2), (v, Ref.get(r)))
    }

    @Test
    def testAbort01(): Unit \ Assert =
        assertEq(expected = 42, runAbort(() -> Ask.ask()))

    @Test
    def testAbort02(): Unit \ Assert =
        assertEq(expected = -1, runAbort(() -> Ask.fail("boom")))

    @Test
    def testStore01(): Unit \ Assert =
        assertEq(expected = 7, runStore(() -> (), () -> Ask.ask()))

    @Test
    def testStore02(): Unit \ Assert =
        assertEq(expected = 3, runStore(() -> (), () -> Ask.fail("abc")))

    @Test
    def testMixed01(): Unit \ Assert =
        assertEq(expected = 4, runMixed(() -> (), () -> Ask.ask() + Ask.fail("abc")))

    @Test
    def testRegion01(): Unit \ Assert =
        assertEq(expected = 3, inRegion())

}

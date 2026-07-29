mod Test.Handler.MultiEffect {

    use Assert.assertEq

    eff Collect {
        def col(x: Int32): Int32
    }

    eff Pair {
        def pair(x: Int32, y: Int32): Int32
    }

    eff Tag {
        def tag(x: String): String
    }

    eff Ask {
        def ask(): Int32
    }

    // Regression tests for a bug where a handler operation's argument was
    // corrupted when read *after* invoking the resumption, in the presence of a
    // second effect whose suspension propagates through the outer handler
    // activations.
    //
    // The handler closure was shared across every invocation of an operation,
    // so its argument fields were overwritten in place by inner invocations
    // before the outer continuation frames were captured. As a result, every
    // outer activation observed the innermost argument value after its
    // resumption returned. The fix copies the closure per invocation, so each
    // case below must observe its own argument value(s) post-resume.

    @Test
    def testArgAfterResume01(): Unit \ Assert =
        // Single-shot resume; the result is order-sensitive, so a permuted or
        // clobbered argument would change the list.
        let result = run {
            let a = Collect.col(1);
            let b = Collect.col(2);
            let c = Collect.col(3);
            let d = Ask.ask();
            -a :: -b :: -c :: d :: Nil
        } with handler Collect {
            def col(i, k) = {
                let cs = k(i);
                i :: cs
            }
        } with handler Ask {
            def ask(k) = k(999)
        };
        assertEq(expected = 1 :: 2 :: 3 :: -1 :: -2 :: -3 :: 999 :: Nil, result)

    @Test
    def testArgAfterResumeWithCapture01(): Unit \ Assert =
        // The handler body captures `base`; the fresh per-invocation closure
        // must preserve captures while keeping distinct argument slots.
        let base = 10;
        let result = run {
            let a = Collect.col(1);
            let b = Collect.col(2);
            let d = Ask.ask();
            a :: b :: d :: Nil
        } with handler Collect {
            def col(i, k) = {
                let cs = k(i);
                (base + i) :: cs
            }
        } with handler Ask {
            def ask(k) = k(7)
        };
        assertEq(expected = 11 :: 12 :: 1 :: 2 :: 7 :: Nil, result)

    @Test
    def testArgAfterResumeMultiShot01(): Unit \ Assert =
        // Multi-shot resumption combined with a second effect.
        //
        // `with handler Collect with handler Ask` installs Collect as the inner
        // handler and Ask as the outer one. The two `+ i` return-additions of
        // the Collect handler therefore lie *inside* the `ask` delimiter, so
        // each of `ask`'s two resumptions re-runs them:
        //   body (a = 1, b = 2): 3 + d, then + 2 (col 2) + 1 (col 1) = 6 + d
        //   ask: (6 + 10) + (6 + 100) = 16 + 106 = 122
        // With the shared-closure bug the inner `+ i` reads were corrupted.
        let result = run {
            let a = Collect.col(1);
            let b = Collect.col(2);
            let d = Ask.ask();
            a + b + d
        } with handler Collect {
            def col(i, k) = k(i) + i
        } with handler Ask {
            def ask(k) = k(10) + k(100)
        };
        assertEq(expected = 122, result)

    @Test
    def testMultiArgAfterResume01(): Unit \ Assert =
        // An operation with *two* arguments: both `x` and `y` must survive the
        // resumption. The `x * 100 + y * 10` encoding makes any swap or
        // clobber observable.
        //   pair(1, 2): tail 1*100 + 2*10 = 120
        //   pair(3, 4): tail 3*100 + 4*10 = 340
        //   body: a + b + d = (1+2) + (3+4) + 5 = 15
        //   total = 15 + 340 + 120 = 475
        let result = run {
            let a = Pair.pair(1, 2);
            let b = Pair.pair(3, 4);
            let d = Ask.ask();
            a + b + d
        } with handler Pair {
            def pair(x, y, k) = {
                let rest = k(x + y);
                x * 100 + y * 10 + rest
            }
        } with handler Ask {
            def ask(k) = k(5)
        };
        assertEq(expected = 475, result)

    @Test
    def testObjectArgAfterResume01(): Unit \ Assert =
        // The operation argument is object-typed (String) rather than a
        // primitive, exercising the boxed argument slot. Each `tag` activation
        // must observe its own string after resuming.
        let result = run {
            let _ = Tag.tag("x");
            let _ = Tag.tag("y");
            let _ = Ask.ask();
            ""
        } with handler Tag {
            def tag(s, k) = {
                let rest = k(s);
                "${s}${rest}"
            }
        } with handler Ask {
            def ask(k) = k(0)
        };
        assertEq(expected = "xy", result)

}

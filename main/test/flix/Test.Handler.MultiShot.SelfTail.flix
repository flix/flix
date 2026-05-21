mod Test.Handler.MultiShot.SelfTail {

    use Assert.assertEq;

    /// Regression tests for https://github.com/flix/flix/issues/12765.
    ///
    /// Before the fix, [[TailPos]] rewrote self-recursive tail calls into
    /// [[ReducedAst.Expr.ApplySelfTail]] even for control-impure functions.
    /// The JVM lowering of ApplySelfTail in an EffectContext mutates the
    /// enclosing function-object's `argI`, `pc`, and lparam fields in place
    /// and jumps to the entry label. When the function may suspend, the
    /// function object is captured (via `copy`) as a continuation Frame, and
    /// multi-shot resumption re-invokes that *same* Frame instance --- so the
    /// mutations made by the first resume corrupt the snapshot relied on by
    /// later resumes. In particular `pc` is reset to 0, causing subsequent
    /// resumes to miss their suspension point and restart from the function
    /// entry, silently dropping the resumed value.
    ///
    /// The tests below exercise multi-shot resumption inside self-recursive
    /// tail calls whose arguments invoke an effect operation.

    eff RandomBool {
        def randomBool(): Bool
    }

    eff AskInt32 {
        def ask(): Int32
    }

    /// Tail-recursive function with effect operation in the *accumulator*
    /// argument of the self-tail-call.
    def loopAcc(n: Int32, acc: Bool): Bool \ RandomBool =
        if (n == 0) acc else loopAcc(n - 1, RandomBool.randomBool())

    /// Tail-recursive function with effect operation in the *index* argument.
    def loopIdx(n: Int32, acc: Bool): Bool \ RandomBool =
        if (n <= 0) acc else loopIdx(n - 1, if (RandomBool.randomBool()) acc else not acc)

    /// Tail-recursive accumulator into a list.
    def collect(n: Int32, acc: List[Bool]): List[Bool] \ RandomBool =
        if (n == 0) acc else collect(n - 1, RandomBool.randomBool() :: acc)

    /// Tail-recursive sum accumulator that asks for each addend.
    def sumAsk(n: Int32, acc: Int32): Int32 \ AskInt32 =
        if (n == 0) acc else sumAsk(n - 1, acc + AskInt32.ask())

    @Test
    def testSelfTailEffectInAcc01(): Unit \ Assert =
        let result = run {
            loopAcc(1, false) :: Nil
        } with handler RandomBool {
            def randomBool(k) = k(true) ::: k(false)
        };
        assertEq(expected = true :: false :: Nil, result)

    @Test
    def testSelfTailEffectInAcc02(): Unit \ Assert =
        // loopAcc(2, _) makes the self-tail-call fire twice. Only the second
        // randomBool() determines the result (acc is overwritten each iter).
        let result = run {
            loopAcc(2, false) :: Nil
        } with handler RandomBool {
            def randomBool(k) = k(true) ::: k(false)
        };
        assertEq(expected = true :: false :: true :: false :: Nil, result)

    @Test
    def testSelfTailEffectInIdx01(): Unit \ Assert =
        // Exercises the effect operation appearing inside an argument
        // expression that is not the literal first thing evaluated.
        let result = run {
            loopIdx(1, false) :: Nil
        } with handler RandomBool {
            def randomBool(k) = k(true) ::: k(false)
        };
        assertEq(expected = false :: true :: Nil, result)

    @Test
    def testSelfTailAccumulateList01(): Unit \ Assert =
        // collect(1, Nil) -> collect(0, b1 :: Nil) -> [b1]
        let result = run {
            collect(1, Nil) :: Nil
        } with handler RandomBool {
            def randomBool(k) = k(true) ::: k(false)
        };
        assertEq(expected = (true :: Nil) :: (false :: Nil) :: Nil, result)

    @Test
    def testSelfTailAccumulateList02(): Unit \ Assert =
        // collect(2, Nil) -> collect(1, b1 :: Nil) -> collect(0, b2 :: b1 :: Nil) -> [b2, b1]
        // With the handler exploring (true, false) at each suspension in order,
        // the result enumerates [[t,t], [f,t], [t,f], [f,f]] (innermost first).
        let result = run {
            collect(2, Nil) :: Nil
        } with handler RandomBool {
            def randomBool(k) = k(true) ::: k(false)
        };
        assertEq(
            expected = (true :: true :: Nil) :: (false :: true :: Nil) :: (true :: false :: Nil) :: (false :: false :: Nil) :: Nil,
            result
        )

    @Test
    def testSelfTailViaListMap(): Unit \ Assert =
        // The original report's pattern: `List.map(_ -> randomBool(), xs)`.
        // `List.map`'s internal accumulator is a self-tail-call whose argument
        // applies the effect-bearing function `f` to each list element.
        let result = run {
            List.map(_ -> RandomBool.randomBool(), 1 :: 2 :: Nil) :: Nil
        } with handler RandomBool {
            def randomBool(k) = k(true) ::: k(false)
        };
        assertEq(
            expected = (true :: true :: Nil) :: (true :: false :: Nil) :: (false :: true :: Nil) :: (false :: false :: Nil) :: Nil,
            result
        )

    @Test
    def testSelfTailThreeShot(): Unit \ Assert =
        // A handler that resumes three times exercises the corruption past
        // the first resume: any single mutation of the captured frame would
        // make resumes 2 and 3 read the same (wrong) state.
        let result = run {
            loopAcc(1, false) :: Nil
        } with handler RandomBool {
            def randomBool(k) = k(true) ::: k(false) ::: k(true)
        };
        assertEq(expected = true :: false :: true :: Nil, result)

    @Test
    def testSelfTailIntAccumulator(): Unit \ Assert =
        // The accumulator is an Int32, so the field-mutation path uses a
        // primitive-typed argument field rather than an object-typed one.
        let result = run {
            sumAsk(1, 0) :: Nil
        } with handler AskInt32 {
            def ask(k) = k(10) ::: k(20)
        };
        assertEq(expected = 10 :: 20 :: Nil, result)

    @Test
    def testSelfTailIntAccumulator02(): Unit \ Assert =
        // Two iterations: each ask() contributes additively. The handler
        // explores (10, 20) at each suspension, so the four paths sum to
        // 20, 30, 30, 40 (in the resume order: a1=10/a2=10, a1=10/a2=20,
        // a1=20/a2=10, a1=20/a2=20).
        let result = run {
            sumAsk(2, 0) :: Nil
        } with handler AskInt32 {
            def ask(k) = k(10) ::: k(20)
        };
        assertEq(expected = 20 :: 30 :: 30 :: 40 :: Nil, result)

}

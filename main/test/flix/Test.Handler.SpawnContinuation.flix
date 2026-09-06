mod Test.Handler.SpawnContinuation {

    use Assert.assertEq;

    /// Tests that a captured continuation can be safely *moved* to another
    /// thread and *re-executed* there.
    ///
    /// Each test defines an effect, builds a computation (`fib`) that calls the
    /// effect, and captures the continuation `k` in the handler. The handler
    /// then resumes `k` either on the same thread or on one (or more) spawned
    /// threads --- the latter forcing the continuation's captured frames to be
    /// transferred across a thread boundary and resumed there.
    ///
    /// See https://github.com/flix/flix/issues/7084.
    eff Gen {
        def gen(): Int32
    }

    /// A small recursive computation that calls `Gen.gen` at every leaf.
    /// With a single-shot handler that resumes with `v`, `fib(n)` evaluates to
    /// `Fib(n+1) * v` (the number of leaves times `v`).
    def fib(n: Int32): Int32 \ Gen =
        if (n <= 1) Gen.gen() else fib(n - 1) + fib(n - 2)

    /// Runs `thunk` on a freshly spawned thread and returns its result,
    /// transferred back over a channel. This is the primitive used below to
    /// resume a captured continuation on another thread.
    def onSpawnedThread(thunk: Unit -> Int32 \ Chan + NonDet): Int32 \ Chan + NonDet =
        region rc {
            let (tx, rx) = Channel.unbuffered();
            spawn { Channel.send(thunk(), tx) } @ rc;
            Channel.recv(rx)
        }

    /////////////////////////////////////////////////////////////////////////
    // (i) Resume the captured continuation on the SAME thread (baseline).   //
    /////////////////////////////////////////////////////////////////////////

    @Test
    def testSameThread01(): Unit \ Assert =
        let result = run {
            fib(2)
        } with handler Gen {
            def gen(k) = k(1)
        };
        assertEq(expected = 2, result)

    @Test
    def testSameThread02(): Unit \ Assert =
        let result = run {
            fib(5)
        } with handler Gen {
            def gen(k) = k(1)
        };
        assertEq(expected = 8, result)

    /////////////////////////////////////////////////////////////////////////
    // (ii) Resume the captured continuation on a SPAWNED thread.            //
    //                                                                       //
    // `fib` calls `gen` at every leaf, so the continuation re-enters the    //
    // handler at each suspension; every resumption therefore runs on a      //
    // fresh thread, exercising repeated cross-thread transfer.              //
    /////////////////////////////////////////////////////////////////////////

    def singleShotOnSpawn(n: Int32): Int32 \ Chan + NonDet =
        run {
            fib(n)
        } with handler Gen {
            def gen(k) = onSpawnedThread(_ -> k(1))
        }

    @Test
    def testSpawnedThread01(): Unit \ Assert + Chan + NonDet =
        assertEq(expected = 2, singleShotOnSpawn(2))

    @Test
    def testSpawnedThread02(): Unit \ Assert + Chan + NonDet =
        assertEq(expected = 8, singleShotOnSpawn(5))

    @Test
    def testSpawnedThread03(): Unit \ Assert + Chan + NonDet =
        assertEq(expected = 13, singleShotOnSpawn(6))

    /// The result of resuming on a spawned thread must agree with resuming on
    /// the same thread.
    def singleShotSame(n: Int32): Int32 \ {} =
        run { fib(n) } with handler Gen { def gen(k) = k(1) }

    @Test
    def testSpawnAgreesWithSame01(): Unit \ Assert + Chan + NonDet =
        assertEq(expected = singleShotSame(7), singleShotOnSpawn(7))

    /////////////////////////////////////////////////////////////////////////
    // Multi-shot: resume the SAME captured continuation more than once.     //
    //                                                                       //
    // A captured continuation may be moved to another thread and re-run, and //
    // it may be re-executed multiple times --- but each resumption must run  //
    // to completion before the next one begins. It must NOT be resumed from  //
    // two threads *simultaneously*: the continuation's captured frames are   //
    // mutated in place during execution, so concurrent resumptions of the    //
    // same continuation race and yield corrupted, nondeterministic results.  //
    // The tests below therefore resume sequentially (each on its own thread, //
    // one after another), never concurrently.                               //
    /////////////////////////////////////////////////////////////////////////

    /// Baseline: both resumptions on the same thread.
    def multiShotSame(n: Int32): Int32 \ {} =
        run {
            fib(n)
        } with handler Gen {
            def gen(k) = k(1) + k(10)
        }

    @Test
    def testMultiShotSame01(): Unit \ Assert =
        assertEq(expected = 44, multiShotSame(2))

    /// Resume the same continuation twice, each resumption on its own spawned
    /// thread, one after another. Tests that a continuation survives being
    /// moved to a thread, run there, and then moved again and re-run.
    def multiShotOnSpawnSequential(n: Int32): Int32 \ Chan + NonDet =
        run {
            fib(n)
        } with handler Gen {
            def gen(k) =
                let a = onSpawnedThread(_ -> k(1));
                let b = onSpawnedThread(_ -> k(10));
                a + b
        }

    @Test
    def testMultiShotOnSpawnSequential01(): Unit \ Assert + Chan + NonDet =
        assertEq(expected = 44, multiShotOnSpawnSequential(2))

    @Test
    def testMultiShotSpawnAgreesWithSame01(): Unit \ Assert + Chan + NonDet =
        assertEq(expected = multiShotSame(3), multiShotOnSpawnSequential(3))

    @Test
    def testMultiShotSpawnAgreesWithSame02(): Unit \ Assert + Chan + NonDet =
        assertEq(expected = multiShotSame(4), multiShotOnSpawnSequential(4))

}

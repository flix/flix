mod Test.Portable.RegionSpawn {

    def assertTrue(cond: Bool, msg: String): Unit =
        if (cond) () else bug!(msg)

    @Test
    def childExceptionPrecedence01(): Unit \ IO = {
        let v = try {
            region rc {
                spawn { throw Exn.mk(1); () } @ rc;
                ()
            };
            0
        } catch {
            case exn: Int32 => Exn.payloadAs(exn)
            case _: Exn => -1
        };
        assertTrue(v == 1, "childExceptionPrecedence01 failed.")
    }

    @Test
    def childExceptionPrecedence02(): Unit \ IO = {
        let v = try {
            region rc {
                spawn { throw Exn.mk(1); () } @ rc;
                throw Exn.mk(2);
                ()
            };
            0
        } catch {
            case exn: Int32 => Exn.payloadAs(exn)
            case _: Exn => -1
        };
        assertTrue(v == 1, "childExceptionPrecedence02 failed.")
    }

    @Test
    def childCancellation01(): Unit \ IO = {
        let start = Clock.runWithIO(() -> Clock.now());
        let payload = try {
            region rc {
                spawn {
                    Timer.runWithIO(() -> Timer.sleepMillis(5000i64));
                    ()
                } @ rc;
                spawn {
                    throw Exn.mk(1);
                    ()
                } @ rc;
                ()
            };
            -1
        } catch {
            case exn: Int32 => Exn.payloadAs(exn)
            case _: Exn => -2
        };
        let elapsed = Clock.runWithIO(() -> Clock.now()) - start;
        let _ = assertTrue(payload == 1, "childCancellation01 failed: wrong exception payload.");
        let _ = assertTrue(elapsed < 2000i64, "childCancellation01 failed: region did not cancel sleeping child promptly.");
        ()
    }

    pub def runAll(): Unit \ IO = {
        let _ = childExceptionPrecedence01();
        let _ = childExceptionPrecedence02();
        let _ = childCancellation01();
        ()
    }

}

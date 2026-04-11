mod Test.Portable.CyclicBarrier {

    use Assert.{assertEq, assertTrue}
    use Sync.CyclicBarrier
    use Sync.CyclicBarrier.Barrier
    use Sync.CyclicBarrier.BarrierError

    def mkBarrier(parties: Int32): Barrier \ IO =
        match CyclicBarrier.newBarrier(parties) {
            case Ok(barrier) => barrier
            case Err(_) => bug!("unexpected invalid barrier")
        }

    def assertBarrierPair(a: Int32, bOpt: Option[Int32]): Unit \ Assert =
        match bOpt {
            case Some(b) => assertTrue((a == 0i32 and b == 1i32) or (a == 1i32 and b == 0i32))
            case None => bug!("barrier did not wake second participant")
        }

    @Test
    def newBarrierInvalid01(): Unit \ IO = {
        match CyclicBarrier.newBarrier(0i32) {
            case Err(BarrierError.InvalidParties) => ()
            case Ok(_) => bug!("expected invalid-parties error")
        };
        match CyclicBarrier.newBarrier(-1i32) {
            case Err(BarrierError.InvalidParties) => ()
            case Ok(_) => bug!("expected invalid-parties error")
        }
    }

    @Test
    def awaitBarrierTripsTwoParty01(): Unit \ {Assert, Chan, IO, NonDet} = {
        let barrier = mkBarrier(2i32);
        region rc {
            let (tx, rx): (Sender[Int32], Receiver[Int32]) = Channel.buffered(1);
            spawn {
                Channel.send(CyclicBarrier.awaitBarrier(barrier), tx);
                ()
            } @ rc;
            let a = CyclicBarrier.awaitBarrier(barrier);
            let b = Channel.recvWithTimeout(rx, 5, TimeUnit.Seconds);
            assertBarrierPair(a, b)
        }
    }

    @Test
    def awaitBarrierResetsGeneration01(): Unit \ {Assert, Chan, IO, NonDet} = {
        let barrier = mkBarrier(2i32);
        region rc {
            let (tx, rx): (Sender[Int32], Receiver[Int32]) = Channel.buffered(1);

            spawn {
                Channel.send(CyclicBarrier.awaitBarrier(barrier), tx);
                ()
            } @ rc;
            let a0 = CyclicBarrier.awaitBarrier(barrier);
            let b0 = Channel.recvWithTimeout(rx, 5, TimeUnit.Seconds);
            assertBarrierPair(a0, b0);

            spawn {
                Channel.send(CyclicBarrier.awaitBarrier(barrier), tx);
                ()
            } @ rc;
            let a1 = CyclicBarrier.awaitBarrier(barrier);
            let b1 = Channel.recvWithTimeout(rx, 5, TimeUnit.Seconds);
            assertBarrierPair(a1, b1)
        }
    }

    @Test
    def blockedAwaitCancellation01(): Unit \ {Assert, Chan, IO, NonDet} = {
        let start = Clock.runWithIO(() -> Clock.now());
        let payload = try {
            region rc {
                let barrier = mkBarrier(2i32);
                let (readyTx, readyRx): (Sender[Unit], Receiver[Unit]) = Channel.buffered(1);
                spawn {
                    Channel.send((), readyTx);
                    discard CyclicBarrier.awaitBarrier(barrier);
                    ()
                } @ rc;
                Channel.recv(readyRx);
                Timer.runWithIO(() -> Timer.sleepMillis(50i64));
                spawn {
                    throw Exn.mk(15);
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
        assertEq(expected = 15, payload);
        let _ = if (elapsed < 2000i64) () else bug!("blockedAwaitCancellation01 failed: region did not cancel blocked barrier await promptly.");
        ()
    }

    pub def runAll(): Unit \ {Assert, Chan, IO, NonDet} = {
        let _ = newBarrierInvalid01();
        let _ = awaitBarrierTripsTwoParty01();
        let _ = awaitBarrierResetsGeneration01();
        let _ = blockedAwaitCancellation01();
        ()
    }

}

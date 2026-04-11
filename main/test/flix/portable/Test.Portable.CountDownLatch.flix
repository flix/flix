mod Test.Portable.CountDownLatch {

    use Assert.{assertEq}
    use Sync.CountDownLatch
    use Sync.CountDownLatch.Latch
    use Sync.CountDownLatch.LatchError

    def mkLatch(count: Int32): Latch \ IO =
        match CountDownLatch.newLatch(count) {
            case Ok(latch) => latch
            case Err(_) => bug!("unexpected invalid latch")
        }

    @Test
    def newLatchInvalid01(): Unit \ IO = {
        match CountDownLatch.newLatch(-1i32) {
            case Err(LatchError.InvalidCount) => ()
            case Ok(_) => bug!("expected invalid-count error")
        }
    }

    @Test
    def awaitImmediateZero01(): Unit \ IO = {
        let latch = mkLatch(0i32);
        CountDownLatch.await(latch)
    }

    @Test
    def awaitUnblocksAfterCountdown01(): Unit \ {Assert, Chan, IO, NonDet} = {
        let latch = mkLatch(2i32);
        region rc {
            let (readyTx, readyRx): (Sender[Unit], Receiver[Unit]) = Channel.buffered(1);
            let (doneTx, doneRx): (Sender[Unit], Receiver[Unit]) = Channel.buffered(1);
            spawn {
                Channel.send((), readyTx);
                CountDownLatch.await(latch);
                Channel.send((), doneTx);
                ()
            } @ rc;
            Channel.recv(readyRx);
            Timer.runWithIO(() -> Timer.sleepMillis(50i64));
            CountDownLatch.countDown(latch);
            assertEq(expected = None, Channel.recvWithTimeout(doneRx, 100i32, TimeUnit.Milliseconds));
            CountDownLatch.countDown(latch);
            assertEq(expected = Some(()), Channel.recvWithTimeout(doneRx, 5i32, TimeUnit.Seconds))
        }
    }

    @Test
    def countDownPastZero01(): Unit \ IO = {
        let latch = mkLatch(1i32);
        CountDownLatch.countDown(latch);
        CountDownLatch.countDown(latch);
        CountDownLatch.await(latch)
    }

    @Test
    def blockedAwaitCancellation01(): Unit \ {Assert, Chan, IO, NonDet} = {
        let start = Clock.runWithIO(() -> Clock.now());
        let payload = try {
            region rc {
                let latch = mkLatch(1i32);
                let (readyTx, readyRx): (Sender[Unit], Receiver[Unit]) = Channel.buffered(1);
                spawn {
                    Channel.send((), readyTx);
                    CountDownLatch.await(latch);
                    ()
                } @ rc;
                Channel.recv(readyRx);
                Timer.runWithIO(() -> Timer.sleepMillis(50i64));
                spawn {
                    throw Exn.mk(17);
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
        assertEq(expected = 17, payload);
        let _ = if (elapsed < 2000i64) () else bug!("blockedAwaitCancellation01 failed: region did not cancel blocked latch await promptly.");
        ()
    }

    pub def runAll(): Unit \ {Assert, Chan, IO, NonDet} = {
        let _ = newLatchInvalid01();
        let _ = awaitImmediateZero01();
        let _ = awaitUnblocksAfterCountdown01();
        let _ = countDownPastZero01();
        let _ = blockedAwaitCancellation01();
        ()
    }

}

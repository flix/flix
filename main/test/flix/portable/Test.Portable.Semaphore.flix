mod Test.Portable.Semaphore {

    use Assert.{assertEq, assertFalse, assertTrue}
    use Sync.Semaphore
    use Sync.Semaphore.Sem
    use Sync.Semaphore.SemaphoreError

    def mkSemaphore(permits: Int32): Sem \ IO =
        match Semaphore.newSemaphore(permits) {
            case Ok(sem) => sem
            case Err(_) => bug!("unexpected invalid semaphore")
        }

    @Test
    def newSemaphoreInvalid01(): Unit \ IO = {
        match Semaphore.newSemaphore(-1i32) {
            case Err(SemaphoreError.InvalidPermits) => ()
            case Ok(_) => bug!("expected invalid-permits error")
        }
    }

    @Test
    def tryAcquireBasic01(): Unit \ {Assert, IO} = {
        let sem = mkSemaphore(2i32);
        assertTrue(Semaphore.tryAcquire(sem));
        assertTrue(Semaphore.tryAcquire(sem));
        assertFalse(Semaphore.tryAcquire(sem));
        Semaphore.release(sem);
        assertTrue(Semaphore.tryAcquire(sem))
    }

    @Test
    def releaseWakesOneBlockedAcquire01(): Unit \ {Assert, Chan, IO, NonDet} = {
        let sem = mkSemaphore(0i32);
        region rc {
            let (readyTx, readyRx): (Sender[Int32], Receiver[Int32]) = Channel.buffered(2);
            let (doneTx, doneRx): (Sender[Int32], Receiver[Int32]) = Channel.buffered(2);

            spawn {
                Channel.send(1i32, readyTx);
                Semaphore.acquire(sem);
                Channel.send(1i32, doneTx);
                ()
            } @ rc;

            spawn {
                Channel.send(2i32, readyTx);
                Semaphore.acquire(sem);
                Channel.send(2i32, doneTx);
                ()
            } @ rc;

            discard Channel.recv(readyRx);
            discard Channel.recv(readyRx);
            Timer.runWithIO(() -> Timer.sleepMillis(50i64));

            Semaphore.release(sem);
            let first = Channel.recvWithTimeout(doneRx, 5i32, TimeUnit.Seconds);
            let second = Channel.recvWithTimeout(doneRx, 100i32, TimeUnit.Milliseconds);
            assertTrue(match first { case Some(1i32) => true case Some(2i32) => true case _ => false });
            assertEq(expected = None, second);

            Semaphore.release(sem);
            let final = Channel.recvWithTimeout(doneRx, 5i32, TimeUnit.Seconds);
            let _ = match (first, final) {
                case (Some(1i32), Some(2i32)) => ()
                case (Some(2i32), Some(1i32)) => ()
                case _ => bug!("semaphore did not release exactly one waiter per permit")
            };
            ()
        }
    }

    @Test
    def blockedAcquireCancellation01(): Unit \ {Assert, Chan, IO, NonDet} = {
        let start = Clock.runWithIO(() -> Clock.now());
        let payload = try {
            region rc {
                let sem = mkSemaphore(0i32);
                let (readyTx, readyRx): (Sender[Unit], Receiver[Unit]) = Channel.buffered(1);
                spawn {
                    Channel.send((), readyTx);
                    Semaphore.acquire(sem);
                    ()
                } @ rc;
                Channel.recv(readyRx);
                Timer.runWithIO(() -> Timer.sleepMillis(50i64));
                spawn {
                    throw Exn.mk(19);
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
        assertEq(expected = 19, payload);
        let _ = if (elapsed < 2000i64) () else bug!("blockedAcquireCancellation01 failed: region did not cancel blocked semaphore acquire promptly.");
        ()
    }

    pub def runAll(): Unit \ {Assert, Chan, IO, NonDet} = {
        let _ = newSemaphoreInvalid01();
        let _ = tryAcquireBasic01();
        let _ = releaseWakesOneBlockedAcquire01();
        let _ = blockedAcquireCancellation01();
        ()
    }

}

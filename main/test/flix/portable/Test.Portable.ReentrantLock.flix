mod Test.Portable.ReentrantLock {

    use Assert.{assertEq, assertFalse, assertTrue}
    use Sync.ReentrantLock
    use Sync.ReentrantLock.UnlockError

    @Test
    def basic01(): Unit \ {Assert, IO} = {
        let lock = ReentrantLock.newLock();
        ReentrantLock.lock(lock);
        assertEq(expected = Ok(()), ReentrantLock.unlock(lock))
    }

    @Test
    def reentrant01(): Unit \ {Assert, IO} = {
        let lock = ReentrantLock.newLock();
        ReentrantLock.lock(lock);
        ReentrantLock.lock(lock);
        assertEq(expected = Ok(()), ReentrantLock.unlock(lock));
        assertEq(expected = Ok(()), ReentrantLock.unlock(lock))
    }

    @Test
    def tryLockOwnedBySelf01(): Unit \ {Assert, IO} = {
        let lock = ReentrantLock.newLock();
        ReentrantLock.lock(lock);
        assertTrue(ReentrantLock.tryLock(lock));
        assertEq(expected = Ok(()), ReentrantLock.unlock(lock));
        assertEq(expected = Ok(()), ReentrantLock.unlock(lock))
    }

    @Test
    def tryLockOwnedByOther01(): Unit \ {Assert, Chan, IO, NonDet} = {
        let lock = ReentrantLock.newLock();
        region rc {
            let (readyTx, readyRx) = Channel.unbuffered();
            let (releaseTx, releaseRx) = Channel.unbuffered();
            spawn {
                ReentrantLock.lock(lock);
                Channel.send((), readyTx);
                Channel.recv(releaseRx);
                discard ReentrantLock.unlock(lock);
                ()
            } @ rc;
            Channel.recv(readyRx);
            assertFalse(ReentrantLock.tryLock(lock));
            Channel.send((), releaseTx)
        }
    }

    @Test
    def wrongTaskUnlock01(): Unit \ {Assert, Chan, IO, NonDet} = {
        let lock = ReentrantLock.newLock();
        region rc {
            let (tx, rx) = Channel.buffered(1);
            ReentrantLock.lock(lock);
            spawn {
                Channel.send(ReentrantLock.unlock(lock), tx);
                ()
            } @ rc;
            assertEq(expected = Err(UnlockError.NotOwner), Channel.recv(rx));
            assertEq(expected = Ok(()), ReentrantLock.unlock(lock))
        }
    }

    @Test
    def blockedLockWake01(): Unit \ {Assert, Chan, IO, NonDet} = {
        let lock = ReentrantLock.newLock();
        region rc {
            let (readyTx, readyRx): (Sender[Unit], Receiver[Unit]) = Channel.unbuffered();
            let (doneTx, doneRx): (Sender[Result[UnlockError, Unit]], Receiver[Result[UnlockError, Unit]]) = Channel.buffered(1);
            ReentrantLock.lock(lock);
            spawn {
                Channel.send((), readyTx);
                ReentrantLock.lock(lock);
                Channel.send(ReentrantLock.unlock(lock), doneTx);
                ()
            } @ rc;
            Channel.recv(readyRx);
            assertEq(expected = Ok(()), ReentrantLock.unlock(lock));
            assertEq(expected = Some(Ok(())), Channel.recvWithTimeout(doneRx, 5, TimeUnit.Seconds))
        }
    }

    @Test
    def blockedLockCancellation01(): Unit \ {Assert, IO} = {
        let start = Clock.runWithIO(() -> Clock.now());
        let payload = try {
            region rc {
                let lock = ReentrantLock.newLock();
                ReentrantLock.lock(lock);
                spawn {
                    ReentrantLock.lock(lock);
                    ()
                } @ rc;
                spawn {
                    throw Exn.mk(11);
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
        assertEq(expected = 11, payload);
        let _ = if (elapsed < 2000i64) () else bug!("blockedLockCancellation01 failed: region did not cancel blocked lock promptly.");
        ()
    }

    pub def runAll(): Unit \ {Assert, Chan, IO, NonDet} = {
        let _ = basic01();
        let _ = reentrant01();
        let _ = tryLockOwnedBySelf01();
        let _ = tryLockOwnedByOther01();
        let _ = wrongTaskUnlock01();
        let _ = blockedLockWake01();
        let _ = blockedLockCancellation01();
        ()
    }

}

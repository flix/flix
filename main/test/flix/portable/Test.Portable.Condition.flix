mod Test.Portable.Condition {

    use Assert.{assertEq, assertTrue}
    use Sync.Condition
    use Sync.Condition.ConditionError
    use Sync.ReentrantLock
    use Sync.ReentrantLock.UnlockError

    @Test
    def awaitNotOwner01(): Unit \ {Assert, IO} = {
        let lock = ReentrantLock.newLock();
        let cond = Condition.newCondition(lock);
        assertEq(expected = Err(ConditionError.NotOwner), Condition.await(cond))
    }

    @Test
    def signalNotOwner01(): Unit \ {Assert, IO} = {
        let lock = ReentrantLock.newLock();
        let cond = Condition.newCondition(lock);
        assertEq(expected = Err(ConditionError.NotOwner), Condition.signal(cond))
    }

    @Test
    def signalAllNotOwner01(): Unit \ {Assert, IO} = {
        let lock = ReentrantLock.newLock();
        let cond = Condition.newCondition(lock);
        assertEq(expected = Err(ConditionError.NotOwner), Condition.signalAll(cond))
    }

    @Test
    def signalWake01(): Unit \ {Assert, Chan, IO, NonDet} = {
        let lock = ReentrantLock.newLock();
        let cond = Condition.newCondition(lock);
        region rc {
            let (readyTx, readyRx): (Sender[Unit], Receiver[Unit]) = Channel.unbuffered();
            let (doneTx, doneRx): (Sender[(Result[ConditionError, Unit], Result[UnlockError, Unit])], Receiver[(Result[ConditionError, Unit], Result[UnlockError, Unit])]) = Channel.buffered(1);
            spawn {
                ReentrantLock.lock(lock);
                Channel.send((), readyTx);
                let waitRes = Condition.await(cond);
                let unlockRes = ReentrantLock.unlock(lock);
                Channel.send((waitRes, unlockRes), doneTx);
                ()
            } @ rc;
            Channel.recv(readyRx);
            ReentrantLock.lock(lock);
            assertEq(expected = Ok(()), Condition.signal(cond));
            assertEq(expected = Ok(()), ReentrantLock.unlock(lock));
            assertEq(expected = Some((Ok(()), Ok(()))), Channel.recvWithTimeout(doneRx, 5, TimeUnit.Seconds))
        }
    }

    @Test
    def signalAllWake01(): Unit \ {Assert, Chan, IO, NonDet} = {
        let lock = ReentrantLock.newLock();
        let cond = Condition.newCondition(lock);
        region rc {
            let (readyTx, readyRx): (Sender[Unit], Receiver[Unit]) = Channel.buffered(2);
            let (doneTx, doneRx): (Sender[(Result[ConditionError, Unit], Result[UnlockError, Unit])], Receiver[(Result[ConditionError, Unit], Result[UnlockError, Unit])]) = Channel.buffered(2);
            spawn {
                ReentrantLock.lock(lock);
                Channel.send((), readyTx);
                let waitRes = Condition.await(cond);
                let unlockRes = ReentrantLock.unlock(lock);
                Channel.send((waitRes, unlockRes), doneTx);
                ()
            } @ rc;
            spawn {
                ReentrantLock.lock(lock);
                Channel.send((), readyTx);
                let waitRes = Condition.await(cond);
                let unlockRes = ReentrantLock.unlock(lock);
                Channel.send((waitRes, unlockRes), doneTx);
                ()
            } @ rc;
            Channel.recv(readyRx);
            ReentrantLock.lock(lock);
            assertEq(expected = Ok(()), ReentrantLock.unlock(lock));
            Channel.recv(readyRx);
            ReentrantLock.lock(lock);
            assertEq(expected = Ok(()), Condition.signalAll(cond));
            assertEq(expected = Ok(()), ReentrantLock.unlock(lock));
            assertEq(expected = Some((Ok(()), Ok(()))), Channel.recvWithTimeout(doneRx, 5, TimeUnit.Seconds));
            assertEq(expected = Some((Ok(()), Ok(()))), Channel.recvWithTimeout(doneRx, 5, TimeUnit.Seconds))
        }
    }

    @Test
    def blockedAwaitCancellation01(): Unit \ {Assert, Chan, IO, NonDet} = {
        let start = Clock.runWithIO(() -> Clock.now());
        let (readyTx, readyRx): (Sender[Unit], Receiver[Unit]) = Channel.buffered(1);
        let payload = try {
            region rc {
                let lock = ReentrantLock.newLock();
                let cond = Condition.newCondition(lock);
                spawn {
                    ReentrantLock.lock(lock);
                    Channel.send((), readyTx);
                    discard Condition.await(cond);
                    discard ReentrantLock.unlock(lock);
                    ()
                } @ rc;
                Channel.recv(readyRx);
                ReentrantLock.lock(lock);
                assertEq(expected = Ok(()), ReentrantLock.unlock(lock));
                spawn {
                    throw Exn.mk(12);
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
        assertEq(expected = 12, payload);
        let _ = if (elapsed < 2000i64) () else bug!("blockedAwaitCancellation01 failed: region did not cancel blocked await promptly.");
        ()
    }

    pub def runAll(): Unit \ {Assert, Chan, IO, NonDet} = {
        let _ = awaitNotOwner01();
        let _ = signalNotOwner01();
        let _ = signalAllNotOwner01();
        let _ = signalWake01();
        let _ = signalAllWake01();
        let _ = blockedAwaitCancellation01();
        ()
    }

}

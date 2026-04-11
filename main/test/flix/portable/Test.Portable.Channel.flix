mod Test.Portable.Channel {

    use Assert.{assertEq, assertNone, assertTrue}
    use Sync.ReentrantLock

    @Test
    def basic01(): Unit \ {Chan, NonDet} =
        let (tx, rx) = Channel.buffered(1);
        Channel.send(42, tx);
        discard Channel.recv(rx)

    @Test
    def timeout01(): Unit \ {Chan, NonDet, IO} = {
        let rx = Channel.timeout(1, TimeUnit.Milliseconds);
        Channel.recv(rx)
    }

    @Test
    def tryRecvEmpty01(): Unit \ {Assert, Chan, NonDet} = {
        let (_, rx) = Channel.buffered(1);
        assertNone(Channel.tryRecv(rx))
    }

    @Test
    def tryRecvReady01(): Unit \ {Assert, Chan, NonDet} = {
        let (tx, rx) = Channel.buffered(1);
        Channel.send(42, tx);
        let r = Channel.tryRecv(rx);
        assertTrue(match r { case Some(42) => true case _ => false })
    }

    @Test
    def recvWithTimeoutTimeout01(): Unit \ {Assert, Chan, NonDet, IO} = {
        let (_, rx) = Channel.buffered(1);
        assertNone(Channel.recvWithTimeout(rx, 1, TimeUnit.Milliseconds))
    }

    @Test
    def recvWithTimeoutReadyWins01(): Unit \ {Assert, Chan, NonDet, IO} = {
        let (tx, rx) = Channel.buffered(1);
        Channel.send(21, tx);
        let r = Channel.recvWithTimeout(rx, 5, TimeUnit.Seconds);
        assertTrue(match r { case Some(21) => true case _ => false })
    }

    @Test
    def blockedRecvCancellation01(): Unit \ {Chan, NonDet, IO} = {
        let start = Clock.runWithIO(() -> Clock.now());
        let payload = try {
            region rc {
                let (_, rx) = Channel.unbuffered();
                spawn {
                    Channel.recv(rx);
                    ()
                } @ rc;
                spawn {
                    throw Exn.mk(7);
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
        let _ = if (payload == 7) () else bug!("blockedRecvCancellation01 failed: wrong exception payload.");
        let _ = if (elapsed < 2000i64) () else bug!("blockedRecvCancellation01 failed: region did not cancel blocked recv promptly.");
        ()
    }

    @Test
    def blockedBufferedSendCancellation01(): Unit \ {Chan, NonDet, IO} = {
        let start = Clock.runWithIO(() -> Clock.now());
        let payload = try {
            region rc {
                let (tx, _) = Channel.buffered(1);
                let (_, probeRx) = Channel.buffered(1);
                Channel.send(1, tx);
                spawn {
                    let _ = Channel.tryRecv(probeRx);
                    Channel.send(2, tx);
                    ()
                } @ rc;
                spawn {
                    throw Exn.mk(8);
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
        let _ = if (payload == 8) () else bug!("blockedBufferedSendCancellation01 failed: wrong exception payload.");
        let _ = if (elapsed < 2000i64) () else bug!("blockedBufferedSendCancellation01 failed: region did not cancel blocked buffered send promptly.");
        ()
    }

    @Test
    def blockedUnbufferedSendParentCancellation01(): Unit \ {Chan, NonDet, IO} = {
        let start = Clock.runWithIO(() -> Clock.now());
        let payload = try {
            region rc {
                let (tx, _) = Channel.unbuffered();
                let (_, probeRx) = Channel.buffered(1);
                spawn {
                    throw Exn.mk(9);
                    ()
                } @ rc;
                let _ = Channel.tryRecv(probeRx);
                Channel.send(42, tx)
            };
            -1
        } catch {
            case exn: Int32 => Exn.payloadAs(exn)
            case _: Exn => -2
        };
        let elapsed = Clock.runWithIO(() -> Clock.now()) - start;
        let _ = if (payload == 9) () else bug!("blockedUnbufferedSendParentCancellation01 failed: wrong exception payload.");
        let _ = if (elapsed < 2000i64) () else bug!("blockedUnbufferedSendParentCancellation01 failed: region did not cancel blocked unbuffered send promptly.");
        ()
    }

    @Test
    def detachedTimeoutLateWakeAfterCancellation01(): Unit \ {Assert, Chan, NonDet, IO} = {
        let (tx, rx) = Channel.buffered(1);
        Channel.send(21, tx);
        assertEq(expected = Some(21), Channel.recvWithTimeout(rx, 50, TimeUnit.Milliseconds));

        let payload = try {
            region rc {
                let lock = ReentrantLock.newLock();
                ReentrantLock.lock(lock);
                spawn {
                    ReentrantLock.lock(lock);
                    ()
                } @ rc;
                spawn {
                    throw Exn.mk(13);
                    ()
                } @ rc;
                ()
            };
            -1
        } catch {
            case exn: Int32 => Exn.payloadAs(exn)
            case _: Exn => -2
        };

        assertEq(expected = 13, payload);
        Timer.runWithIO(() -> Timer.sleepFor(100i64, TimeUnit.Milliseconds))
    }

    pub def runAll(): Unit \ {Assert, Chan, NonDet, IO} = {
        let _ = basic01();
        let _ = timeout01();
        let _ = tryRecvEmpty01();
        let _ = tryRecvReady01();
        let _ = recvWithTimeoutTimeout01();
        let _ = recvWithTimeoutReadyWins01();
        let _ = blockedRecvCancellation01();
        let _ = blockedBufferedSendCancellation01();
        let _ = blockedUnbufferedSendParentCancellation01();
        let _ = detachedTimeoutLateWakeAfterCancellation01();
        ()
    }

}

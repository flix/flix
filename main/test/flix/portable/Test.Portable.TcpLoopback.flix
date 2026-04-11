mod Test.Portable.TcpLoopback {

    def assertTrue(cond: Bool, msg: String): Unit =
        if (cond) () else bug!(msg)

    def expectOk(res: Result[IoError, a], msg: String): a = match res {
        case Ok(v) => v
        case Err(e) => bug!("${msg}: ${e}")
    }

    def expectOkUnit(res: Result[IoError, Unit], msg: String): Unit = match res {
        case Ok(()) => ()
        case Err(e) => bug!("${msg}: ${e}")
    }

    def loopbackAddr(): IpAddr = match IpAddr.fromString("127.0.0.1") {
        case Some(ip) => ip
        case None => unreachable!()
    }

    def mkMsg(rc: Region[r]): Array[Int8, r] \ r =
        Array.init(rc, i -> match i {
            case 0 => 70i8 // 'F'
            case 1 => 76i8 // 'L'
            case 2 => 73i8 // 'I'
            case 3 => 88i8 // 'X'
            case 4 => 10i8 // '\n'
            case _ => 0i8
        }, 5)

    def readAll(rc: Region[r], sock: TcpSocket): Array[Int8, r] \ r + IO = {
        let buf = Array.repeat(rc, 64, 0i8);
        let acc = MutList.empty(rc);

        def copyN(n: Int32): Unit \ r = {
            def loop(i: Int32): Unit \ r =
                if (i >= n) () else {
                    MutList.push(Array.get(i, buf), acc);
                    loop(i + 1)
                };
            loop(0)
        };

        def loopRead(): Unit \ r + IO = match TcpSocket.read(buf, sock) {
            case Ok(n) =>
                if (n <= 0) () else {
                    copyN(n);
                    loopRead()
                }
            case Err(e) => bug!("TcpSocket.read failed: ${e}")
        };

        loopRead();
        MutList.toArray(rc, acc)
    }

    @Test
    def loopback01(): Unit \ IO = region rc {
        let ip = loopbackAddr();

        // Bind on an ephemeral port.
        let server = expectOk(
            TcpBindWithResult.runWithIO(() -> TcpBindWithResult.bind(ip, 0)),
            "TcpBindWithResult.bind failed"
        );
        let port = expectOk(TcpServer.localPort(server), "TcpServer.localPort failed");

        // Client: connect, write, close.
        spawn {
            let sock = expectOk(
                TcpConnectWithResult.runWithIO(() -> TcpConnectWithResult.connect(ip, port)),
                "TcpConnectWithResult.connect failed"
            );

            let msg = mkMsg(Static);
            let n = expectOk(TcpSocket.write(msg, sock), "TcpSocket.write failed");
            let _ = assertTrue(n == Array.length(msg), "TcpSocket.write returned unexpected byte count.");
            expectOkUnit(TcpSocket.close(sock), "TcpSocket.close failed");
            ()
        } @ rc;

        // Server: accept, read until EOF, validate, close.
        let sock = expectOk(
            TcpAcceptWithResult.runWithIO(() -> TcpAcceptWithResult.accept(server)),
            "TcpAcceptWithResult.accept failed"
        );

        let got = readAll(rc, sock);
        let expected = mkMsg(rc);
        let _ = assertTrue(Array.sameElements(got, expected), "Unexpected TCP payload.");

        expectOkUnit(TcpSocket.close(sock), "TcpSocket.close failed (server)");
        expectOkUnit(TcpServer.close(server), "TcpServer.close failed");
        ()
    }

    pub def runAll(): Unit \ IO = {
        let _ = loopback01();
        ()
    }

}

mod Test.Portable.TcpProcessSignatures {

    // Compile-only signatures (not executed) to ensure these types/effects are present
    // in the portable stdlib profile.

    def tcpConnectSig(ip: IpAddr, port: Int32): TcpSocket \ TcpConnect =
        TcpConnect.connect(ip, port)

    def tcpConnectWithResultSig(ip: IpAddr, port: Int32): Result[IoError, TcpSocket] \ TcpConnectWithResult =
        TcpConnectWithResult.connect(ip, port)

    def tcpBindSig(ip: IpAddr, port: Int32): TcpServer \ TcpBind =
        TcpBind.bind(ip, port)

    def tcpBindWithResultSig(ip: IpAddr, port: Int32): Result[IoError, TcpServer] \ TcpBindWithResult =
        TcpBindWithResult.bind(ip, port)

    def tcpAcceptSig(server: TcpServer): TcpSocket \ TcpAccept =
        TcpAccept.accept(server)

    def tcpAcceptWithResultSig(server: TcpServer): Result[IoError, TcpSocket] \ TcpAcceptWithResult =
        TcpAcceptWithResult.accept(server)

    def tcpSocketReadSig(buffer: Array[Int8, r], socket: TcpSocket): Result[IoError, Int32] \ r + IO =
        TcpSocket.read(buffer, socket)

    def tcpSocketWriteSig(buffer: Array[Int8, r], socket: TcpSocket): Result[IoError, Int32] \ r + IO =
        TcpSocket.write(buffer, socket)

    def tcpSocketCloseSig(socket: TcpSocket): Result[IoError, Unit] \ IO =
        TcpSocket.close(socket)

    def tcpServerCloseSig(server: TcpServer): Result[IoError, Unit] \ IO =
        TcpServer.close(server)

    def tcpServerLocalPortSig(server: TcpServer): Result[IoError, Int32] \ IO =
        TcpServer.localPort(server)

    def processExecSig(cmd: String, args: List[String]): ProcessHandle \ Process =
        Process.exec(cmd, args)

    def processIsAliveSig(ph: ProcessHandle): Bool \ Process =
        Process.isAlive(ph)

    def processStdinSig(ph: ProcessHandle): Process.StdIn \ Process =
        Process.stdin(ph)

    def processStdoutSig(ph: ProcessHandle): Process.StdOut \ Process =
        Process.stdout(ph)

    def processStderrSig(ph: ProcessHandle): Process.StdErr \ Process =
        Process.stderr(ph)

    def processStdInWriteSig(b: Array[Int8, r], r: Process.StdIn): Result[IoError, Int32] \ r + IO =
        Writable.write(b, r)

    def processStdOutReadSig(b: Array[Int8, r], r: Process.StdOut): Result[IoError, Int32] \ r + IO =
        Readable.read(b, r)

    def processStdErrReadSig(b: Array[Int8, r], r: Process.StdErr): Result[IoError, Int32] \ r + IO =
        Readable.read(b, r)

    def processHandleReleaseSig(ph: ProcessHandle): Result[IoError, Unit] \ IO =
        ProcessHandle.release(ph)

    def processWithResultExecSig(cmd: String, args: List[String]): Result[IoError, ProcessHandle] \ ProcessWithResult =
        ProcessWithResult.exec(cmd, args)

    def processWithResultWaitForSig(ph: ProcessHandle): Result[IoError, Int32] \ ProcessWithResult =
        ProcessWithResult.waitFor(ph)

    def httpGetSig(url: String, headers: Map[String, List[String]]): Http.Response \ Http =
        Http.get(url, headers)

    def httpWithResultGetSig(url: String, headers: Map[String, List[String]]): Result[IoError, Http.Response] \ HttpWithResult =
        HttpWithResult.get(url, headers)

    def httpRunWithIOSig(): Result[IoError, Http.Response] \ IO =
        Http.runWithIO(() -> Http.get("http://example.com", Map.empty()))

    def httpWithResultRunWithIOSig(): Result[IoError, Http.Response] \ IO =
        HttpWithResult.runWithIO(() -> HttpWithResult.get("http://example.com", Map.empty()))

    @Test
    def smoke01(): Unit = {
        let _ = tcpConnectSig;
        let _ = tcpConnectWithResultSig;
        let _ = tcpBindSig;
        let _ = tcpBindWithResultSig;
        let _ = tcpAcceptSig;
        let _ = tcpAcceptWithResultSig;
        let _ = tcpSocketReadSig;
        let _ = tcpSocketWriteSig;
        let _ = tcpSocketCloseSig;
        let _ = tcpServerCloseSig;
        let _ = tcpServerLocalPortSig;
        let _ = processExecSig;
        let _ = processIsAliveSig;
        let _ = processStdinSig;
        let _ = processStdoutSig;
        let _ = processStderrSig;
        let _ = processStdInWriteSig;
        let _ = processStdOutReadSig;
        let _ = processStdErrReadSig;
        let _ = processHandleReleaseSig;
        let _ = processWithResultExecSig;
        let _ = processWithResultWaitForSig;
        let _ = httpGetSig;
        let _ = httpWithResultGetSig;
        let _ = httpRunWithIOSig;
        let _ = httpWithResultRunWithIOSig;
        ()
    }

    pub def runAll(): Unit = {
        let _ = smoke01();
        ()
    }

}

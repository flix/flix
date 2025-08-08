mod Server {
    import java.nio.charset.StandardCharsets

    use Peekable.readLine
    use Writable.writeLine

    pub def launch(ipAddr: IpAddr, port: Int16): Result[IoError, Unit] \ TcpBindWithResult + TcpAcceptWithResult + Logger + Sys + IO =
        forM(
            server <- {
                Logger.info("Binding server to ${ipAddr}:${port}");
                TcpBindWithResult.bind(ipAddr, port)
            };
            socket <- {
                Logger.info("Server bound! Listening for a client connection...");
                TcpAcceptWithResult.accept(server)
            };
            res <- region rc {
                let reader = initReader(rc, socket);
                let writer = initWriter(socket);
                Logger.info("Client accepted!");
                def serverLoop() = {
                    forM(
                        line <- readLine(reader);
                        _ <- writeLine(line, writer);
                        _ <- serverLoop()
                    ) yield ()
                };
                serverLoop()
            }
        ) yield res

    def initReader(rc: Region[r], socket: TcpSocket): BufReader[Char, DecodingReader[TcpSocket, r], r] \ IO + r =
        let utf8 = CharacterSet.CharacterSet(StandardCharsets.UTF_8);
        let decodingReader = DecodingReader.wrap(rc, utf8, socket);
        BufReader.withDefaultCapacity(rc, decodingReader)

    def initWriter(socket: TcpSocket): EncodingWriter[TcpSocket] \ IO =
        let utf8 = CharacterSet.CharacterSet(StandardCharsets.UTF_8);
        EncodingWriter.wrap(utf8, socket)

}

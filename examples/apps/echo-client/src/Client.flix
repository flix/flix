mod Client {
    import java.nio.charset.StandardCharsets

    use Peekable.readLine
    use Writable.writeLine

    pub def launch(ipAddr: IpAddr, port: Int16): Result[IoError, Unit] \ TcpConnectWithResult + Console + Logger + IO + Sys =
        forM(
            socket <- {
                Logger.info("Connecting to server at ${ipAddr}:${port}");
                TcpConnectWithResult.connect(ipAddr, port)
            };
            res <- region rc {
                let reader = initReader(rc, socket);
                let writer = initWriter(socket);
                Logger.info("Connected to server! Type a message below...");
                def clientLoop() = {
                    let line = Console.readln();
                    forM(
                        _ <- writeLine(line, writer);
                        read <- readLine(reader);
                        res <- {
                            Console.println(read);
                            clientLoop()
                        }
                    ) yield res
                };
                clientLoop()
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

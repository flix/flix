/*
 *  Copyright 2025 Ry Wiese
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

pub mod Net.TcpBind {
    use IoError.ErrorKind
    use IoError.IoError
    use Net.IpAddr
    use Net.TcpServer
    use Net.TcpServer.TcpServer

    import java.net.InetAddress
    import java.net.ServerSocket
    import java.io.IOException
    import java.lang.IllegalArgumentException

    ///
    /// The effect used to bind a TCP server.
    ///
    pub eff TcpBind {
        def bind(ip: IpAddr, port: Int16): Result[IoError, TcpServer]
    }

    ///
    /// Handles the `TcpBind` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `TcpBind` effect using the `IO` effect.
    ///
    pub def handle(f: a -> b \ ef): a -> b \ (ef - TcpBind) + IO = x ->
        run {
            f(x)
        } with handler TcpBind {
            def bind(ip, port, k) = {
                try {
                    let inet: InetAddress = InetAddress.getByAddress(IpAddr.toBytes(ip));
                    let maxQueueLength = 50;
                    let socket = new ServerSocket(Int16.toInt32(port), maxQueueLength, inet);
                    let server = TcpServer(socket);
                    k(Ok(server))
                } catch {
                    case ex: IllegalArgumentException => k(Err(IoError(ErrorKind.InvalidInput, ex.getMessage())))
                    case ex: IOException              => k(Err(IoError(ErrorKind.Other, ex.getMessage())))
                }
            }
        }

    ///
    /// Runs the `TcpBind` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `TcpBind` effect using the `IO` effect.
    ///
    @DefaultHandler
    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - TcpBind) + IO = handle(f)()

}

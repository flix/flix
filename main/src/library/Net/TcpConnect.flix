/*
 *  Copyright 2025 Ry Wiese
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

pub mod Net.TcpConnect {
    use IoError.ErrorKind
    use IoError.IoError
    use Net.IpAddr
    use Net.TcpSocket
    use Net.TcpSocket.TcpSocket

    import java.net.InetAddress
    import java.net.Socket
    import java.io.IOException

    ///
    /// The effect used to connect to a TCP server.
    ///
    pub eff TcpConnect {
        def connect(ip: IpAddr, port: Int16): Result[IoError, TcpSocket]
    }

    ///
    /// Handles the `TcpConnect` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `TcpConnect` effect using the `IO` effect.
    ///
    pub def handle(f: a -> b \ ef): a -> b \ (ef - TcpConnect) + IO = x ->
        run {
            f(x)
        } with handler TcpConnect {
            def connect(ip, port, k) = {
                try {
                    let inet: InetAddress = InetAddress.getByAddress(IpAddr.toBytes(ip));
                    let socket = new Socket(inet, Int16.toInt32(port));
                    let inputStream = socket.getInputStream();
                    let outputStream = socket.getOutputStream();
                    k(Ok(TcpSocket(socket, inputStream, outputStream)))
                } catch {
                    case ex: IOException => k(Err(IoError(ErrorKind.Other, ex.getMessage())))
                }
            }
        }

    ///
    /// Runs the `TcpConnect` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `TcpConnect` effect using the `IO` effect.
    ///
    @DefaultHandler
    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - TcpConnect) + IO = handle(f)()

}

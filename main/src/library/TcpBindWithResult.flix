/*
 *  Copyright 2025 Ry Wiese
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

///
/// The effect used to bind a TCP server.
///
eff TcpBindWithResult {
    def bind(ip: IpAddr, port: Int16): Result[IoError, TcpServer]
}

pub mod TcpBindWithResult {
    import java.net.InetAddress
    import java.net.ServerSocket
    use IoError.ErrorKind
    use IoError.IoError

    import java.io.IOException
    import java.lang.IllegalArgumentException

    ///
    /// Handles the `Tcp` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `TcpBindWithResult` effect using the `IO` effect.
    ///
    pub def handle(f: a -> b \ ef): a -> b \ ef - TcpBindWithResult + IO = x ->
        run {
            f(x)
        } with handler TcpBindWithResult {
            def bind(ip, port, resume) = {
                try {
                    let inet: InetAddress = InetAddress.getByAddress(IpAddr.toBytes(ip));
                    let maxQueueLength = 50;
                    let socket = new ServerSocket(Int16.toInt32(port), maxQueueLength, inet);
                    let server = TcpServer.TcpServer(socket);
                    resume(Ok(server))
                } catch {
                    case ex: IllegalArgumentException => resume(Err(IoError(ErrorKind.InvalidInput, ex.getMessage())))
                    case ex: IOException              => resume(Err(IoError(ErrorKind.Other, ex.getMessage())))
                }
            }
        }

    ///
    /// Runs the `TcpBindWithResult` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `TcpBindWithResult` effect using the `IO` effect.
    ///
    @DefaultHandler
    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - TcpBindWithResult) + IO = handle(f)()

}

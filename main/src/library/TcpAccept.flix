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
/// The effect used to accept a TCP server connection.
///
eff TcpAccept {
    def accept(server: TcpServer): TcpSocket
}

pub mod TcpAccept {
    use IoError.ErrorKind
    use IoError.IoError

    import java.io.IOException
    import java.net.SocketTimeoutException
    import java.nio.channels.IllegalBlockingModeException

    ///
    /// Handles the `TcpAccept` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `TcpAccept` effect using the `IO` effect.
    ///
    pub def handle(f: a -> b \ ef): a -> Result[IoError, b] \ ef - TcpAccept + IO = x ->
        run {
            Ok(f(x))
        } with handler TcpAccept {
            def accept(server, resume) = {
                try {
                    let TcpServer.TcpServer(serverSocket) = server;
                    let socket = serverSocket.accept();
                    let inputStream = socket.getInputStream();
                    let outputStream = socket.getOutputStream();
                    resume(TcpSocket.TcpSocket(socket, inputStream, outputStream))
                } catch {
                    case ex: SocketTimeoutException => Err(IoError(ErrorKind.Timeout, ex.getMessage()))
                    case ex: IOException            => Err(IoError(ErrorKind.Other, ex.getMessage()))
                }
            }
        }

    ///
    /// Runs the `TcpAccept` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `TcpAccept` effect using the `IO` effect.
    ///
    pub def runWithIO(f: Unit -> a \ ef): Result[IoError, a] \ ef - TcpAccept + IO = handle(f)()

}

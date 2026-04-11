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

    use IoError.IoError

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
                let TcpServer.TcpServer(serverId) = server;
                let (ok, socketId, kind, msg) = %%TCP_SERVER_ACCEPT%%(serverId);
                if (ok) {
                    resume(TcpSocket.TcpSocket(socketId))
                } else {
                    Err(IoError(IoError.errorKindFromCode(kind), msg))
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

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
/// The effect used to connect to a TCP server.
///
eff TcpConnect {
    def connect(ip: IpAddr, port: Int32): TcpSocket
}

pub mod TcpConnect {

    use IoError.IoError

    ///
    /// Handles the `TcpConnect` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `TcpConnect` effect using the `IO` effect.
    ///
    pub def handle(f: a -> b \ ef): a -> Result[IoError, b] \ ef - TcpConnect + IO = x ->
        run {
            Ok(f(x))
        } with handler TcpConnect {
            def connect(ip, port, k) = {
                let (ok, id, kind, msg) = %%TCP_SOCKET_CONNECT%%((IpAddr.toBytes(ip), port));
                if (ok) {
                    k(TcpSocket.TcpSocket(id))
                } else {
                    Err(IoError(IoError.errorKindFromCode(kind), msg))
                }
            }
        }

    ///
    /// Runs the `TcpConnect` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `TcpConnect` effect using the `IO` effect.
    ///
    pub def runWithIO(f: Unit -> a \ ef): Result[IoError, a] \ ef - TcpConnect + IO = handle(f)()

}

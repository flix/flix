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
/// Represents a TCP server
///
///
/// The underlying value is an opaque runtime handle.
///
enum TcpServer(Int64)

pub mod TcpServer {

    use IoError.ErrorKind
    use IoError.IoError

    ///
    /// Closes the server socket.
    ///
    pub def close(server: TcpServer): Result[IoError, Unit] \ IO =
        let TcpServer(id) = server;
        let (ok, msg) = %%TCP_SERVER_CLOSE%%(id);
        if (ok) Ok(()) else Err(IoError(ErrorKind.Other, msg))

    ///
    /// Returns the local port that the server is bound to.
    ///
    pub def localPort(server: TcpServer): Result[IoError, Int32] \ IO =
        let TcpServer(id) = server;
        let (ok, port, msg) = %%TCP_SERVER_LOCAL_PORT%%(id);
        if (ok) Ok(port) else Err(IoError(ErrorKind.Other, msg))

}

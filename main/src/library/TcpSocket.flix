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
/// Represents a TCP socket
///
///
/// The underlying value is an opaque runtime handle.
///
enum TcpSocket(Int64)

instance Readable[TcpSocket] {

    type Elm = Int8

    type Aef = IO

    pub def read(buffer: Array[Int8, r], socket: TcpSocket): Result[IoError, Int32] \ r + IO =
        TcpSocket.read(buffer, socket)
}

instance Writable[TcpSocket] {

    type Elm = Int8

    type Aef = IO

    pub def write(buffer: Array[Int8, r], socket: TcpSocket): Result[IoError, Int32] \ r + IO =
        TcpSocket.write(buffer, socket)

}

pub mod TcpSocket {

    use IoError.ErrorKind
    use IoError.IoError

    pub def read(buffer: Array[Int8, r], socket: TcpSocket): Result[IoError, Int32] \ r + IO =
        let TcpSocket(id) = socket;
        let (ok, n, msg) = %%TCP_SOCKET_READ%%((id, buffer));
        if (ok) Ok(n) else Err(IoError(ErrorKind.Other, msg))

    pub def write(buffer: Array[Int8, r], socket: TcpSocket): Result[IoError, Int32] \ r + IO =
        let TcpSocket(id) = socket;
        let (ok, n, msg) = %%TCP_SOCKET_WRITE%%((id, buffer));
        if (ok) Ok(n) else Err(IoError(ErrorKind.Other, msg))

    ///
    /// Closes the socket.
    ///
    pub def close(socket: TcpSocket): Result[IoError, Unit] \ IO =
        let TcpSocket(id) = socket;
        let (ok, msg) = %%TCP_SOCKET_CLOSE%%(id);
        if (ok) Ok(()) else Err(IoError(ErrorKind.Other, msg))

}

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

pub mod Net.TcpSocket {

    use Net.TcpSocket
    use Net.TcpSocket.TcpSocket

    pub def read(buffer: Array[Int8, r], socket: TcpSocket): Result[IoError, Int32] \ r + IO =
        let TcpSocket(_, inputStream, _) = socket;
        Readable.read(buffer, inputStream)

    pub def write(buffer: Array[Int8, r], socket: TcpSocket): Result[IoError, Int32] \ r + IO =
        let TcpSocket(_, _, outputStream) = socket;
        Writable.write(buffer, outputStream)

}

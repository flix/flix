/*
 *  Copyright 2025 Ry Wiese
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

pub mod Net.TcpSocket {

    import java.io.InputStream
    import java.io.OutputStream
    import java.net.Socket

    use Net.TcpSocket
    use Net.TcpSocket.TcpSocket

    ///
    /// Represents a TCP socket.
    ///
    pub enum TcpSocket(Socket, InputStream, OutputStream)

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

    pub def read(buffer: Array[Int8, r], socket: TcpSocket): Result[IoError, Int32] \ r + IO =
        let TcpSocket(_, inputStream, _) = socket;
        Readable.read(buffer, inputStream)

    pub def write(buffer: Array[Int8, r], socket: TcpSocket): Result[IoError, Int32] \ r + IO =
        let TcpSocket(_, _, outputStream) = socket;
        Writable.write(buffer, outputStream)

}

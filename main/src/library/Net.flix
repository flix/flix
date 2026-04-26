/*
 * Copyright 2026 Flix Authors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

pub mod Net {

    use Time.Duration
    use Time.TimeUnit

    import java.io.InputStream
    import java.io.OutputStream
    import java.net.ServerSocket
    import java.net.Socket

    ///
    /// Represents an Ipv4 Socket Address.
    ///
    pub enum SocketAddrV4 with Eq {
        case SocketAddrV4(Ipv4Addr, Int32)
    }

    instance ToString[SocketAddrV4] {
        pub def toString(x: SocketAddrV4): String =
            let SocketAddrV4.SocketAddrV4(ip, port) = x;
            "${ToString.toString(ip)}:${port}"
    }

    instance Formattable[SocketAddrV4] {
        pub def format(x: SocketAddrV4): RichString = RichString.fromString(ToString.toString(x))
    }

    ///
    /// Represents an Ipv6 Socket Address.
    ///
    pub enum SocketAddrV6 with Eq {
        case SocketAddrV6(Ipv6Addr, Int32)
    }

    instance ToString[SocketAddrV6] {
        pub def toString(x: SocketAddrV6): String =
            let SocketAddrV6.SocketAddrV6(ip, port) = x;
            "[${ToString.toString(ip)}]:${port}"
    }

    instance Formattable[SocketAddrV6] {
        pub def format(x: SocketAddrV6): RichString = RichString.fromString(ToString.toString(x))
    }

    ///
    /// Represents a Socket Address.
    ///
    pub enum SocketAddr with Eq {
        case V4(SocketAddrV4)
        case V6(SocketAddrV6)
    }

    instance FromString[SocketAddr] {
        pub def fromString(x: String): Option[SocketAddr] = SocketAddr.fromString(x)
    }

    instance ToString[SocketAddr] {
        pub def toString(x: SocketAddr): String = match x {
            case SocketAddr.V4(SocketAddrV4.SocketAddrV4(ip, port)) => "${ToString.toString(ip)}:${port}"
            case SocketAddr.V6(SocketAddrV6.SocketAddrV6(ip, port)) => "[${ToString.toString(ip)}]:${port}"
        }
    }

    instance Formattable[SocketAddr] {
        pub def format(x: SocketAddr): RichString = RichString.fromString(ToString.toString(x))
    }

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

    ///
    /// The effect used to resolve domain names to IP addresses.
    ///
    pub eff Dns {
        def lookup(host: String): Result[IoError, IpAddr]
        def lookupAll(host: String): Result[IoError, List[IpAddr]]
    }

    ///
    /// The effect used to send HTTP requests (both http:// and https://).
    ///
    pub eff Http {
        def request(req: HttpRequest): Result[IoError, HttpResponse]
    }

    ///
    /// The effect used to send HTTPS-only requests.
    ///
    pub eff Https {
        def request(req: HttpRequest): Result[IoError, HttpResponse]
    }

    ///
    /// The effect used to determine if an address is reachable.
    ///
    pub eff Ping {
        def isReachable(host: IpAddr, timeout: Int32, u: TimeUnit): Result[IoError, Bool]
    }

    ///
    /// Represents a TCP server.
    ///
    pub enum TcpServer(ServerSocket)

    ///
    /// The effect used to connect to a TCP server.
    ///
    pub eff TcpConnect {
        def connect(ip: IpAddr, port: Int16): Result[IoError, TcpSocket]
    }

    ///
    /// The effect used to bind a TCP server.
    ///
    pub eff TcpBind {
        def bind(ip: IpAddr, port: Int16): Result[IoError, TcpServer]
    }

    ///
    /// The effect used to accept a TCP server connection.
    ///
    pub eff TcpAccept {
        def accept(server: TcpServer): Result[IoError, TcpSocket]
    }

}

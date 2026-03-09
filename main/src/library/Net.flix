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

    import java.io.InputStream
    import java.io.OutputStream
    import java.net.ServerSocket
    import java.net.Socket

    ///
    /// An enum representing an HTTP method.
    ///
    pub enum Method with Eq, ToString {
        case Get
        case Head
        case Options
        case Post
        case Put
        case Patch
        case Delete
    }

    ///
    /// An enum representing the body of an HTTP request or response.
    ///
    pub enum Body with Eq, ToString {
        case Text(String)
        case Bytes(Vector[Int8])
    }

    ///
    /// Describes why a retry is being considered.
    ///
    pub enum RetryOutcome {
        /// A transport-level failure (connection refused, timeout, etc.).
        case TransportError(IoError)
        /// A response was received but its status code warrants a retry (e.g. 429, 503).
        case HttpError(HttpResponse)
    }

    ///
    /// The decision returned by a retry handler.
    ///
    pub enum RetryDecision with Eq, ToString {
        /// Retry after the given duration.
        case Retry(Duration)
        /// Do not retry; propagate the last result.
        case Stop
    }

    ///
    /// An HTTP request with method, URL, multi-valued headers, and optional body.
    ///
    pub enum HttpRequest({
        method          = Method,
        url             = String,
        headers         = Map[String, List[String]],
        body            = Option[Body],
        requestTimeout  = Option[Duration],
        connectTimeout  = Option[Duration],
        followRedirects = Bool
    })

    ///
    /// An HTTP response with final URL, status code, multi-valued headers, and body.
    ///
    pub enum HttpResponse({
        url     = String,
        status  = Int32,
        headers = Map[String, List[String]],
        body    = Body
    })

    ///
    /// Represents a V4 Ip Address.
    ///
    pub enum Ipv4Addr with Eq { // 32 bits
        case Ipv4Addr(Int8, Int8, Int8, Int8)
    }

    instance FromString[Ipv4Addr] {
        pub def fromString(x: String): Option[Ipv4Addr] = Ipv4Addr.fromString(x)
    }

    instance ToString[Ipv4Addr] {
        pub def toString(x: Ipv4Addr): String = Ipv4Addr.toString(x)
    }

    instance Formattable[Ipv4Addr] {
        pub def format(x: Ipv4Addr): RichString = RichString.fromString(ToString.toString(x))
    }

    ///
    /// Represents a V6 Ip Address.
    ///
    pub enum Ipv6Addr with Eq {
        case Ipv6Addr( // 128 bits
          Int8, Int8, Int8, Int8, // 32 bits
          Int8, Int8, Int8, Int8, // 32 bits
          Int8, Int8, Int8, Int8, // 32 bits
          Int8, Int8, Int8, Int8  // 32 bits
        )
    }

    instance FromString[Ipv6Addr] {
        pub def fromString(x: String): Option[Ipv6Addr] = Ipv6Addr.fromString(x)
    }

    instance ToString[Ipv6Addr] {
        pub def toString(x: Ipv6Addr): String = Ipv6Addr.toString(x)
    }

    instance Formattable[Ipv6Addr] {
        pub def format(x: Ipv6Addr): RichString = RichString.fromString(ToString.toString(x))
    }

    ///
    /// Represents an IP address.
    ///
    pub enum IpAddr with Eq, ToString {
        case V4(Ipv4Addr),
        case V6(Ipv6Addr)
    }

    instance FromString[IpAddr] {
        pub def fromString(x: String): Option[IpAddr] = IpAddr.fromString(x)
    }

    instance Formattable[IpAddr] {
        pub def format(x: IpAddr): RichString = RichString.fromString(ToString.toString(x))
    }

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
        def isReachable(host: IpAddr, timeout: Int32, u: Time.TimeUnit): Result[IoError, Bool]
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

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

    use Time.TimeUnit

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

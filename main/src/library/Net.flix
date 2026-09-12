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

///
/// The `Net` module is a collection of networking utility submodules,
/// each offering a self-contained piece of functionality:
///
/// - `Http` and `Https` perform web requests.
/// - `HttpRequest` and `HttpResponse` represent the messages exchanged.
/// - `Method` represents an HTTP method and `Body` an HTTP message body.
/// - `Retry` configures how failed requests are retried.
/// - `Dns` resolves host names and `Ping` checks host reachability.
/// - `TcpConnect`, `TcpBind`, and `TcpAccept` open and accept TCP connections.
/// - `TcpServer` and `TcpSocket` represent a listening server and a connection.
/// - `IpAddr`, `Ipv4Addr`, and `Ipv6Addr` represent IP addresses.
/// - `SocketAddr` represents a socket address (an IP address and a port).
///
pub mod Net {
    /* empty */
}

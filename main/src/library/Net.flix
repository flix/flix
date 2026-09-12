/*
 * Copyright 2026 Flix Authors
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
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

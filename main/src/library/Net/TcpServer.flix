/*
 * Copyright 2026 Flix Authors
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

///
/// Type of TCP servers, and its operations.
///
pub mod Net.TcpServer {

    import java.net.ServerSocket

    ///
    /// Type of TCP servers.
    ///
    pub enum TcpServer(ServerSocket)

}

/*
 * Copyright 2026 Flix Authors
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

///
/// Type of IPv4 socket addresses, and its operations.
///
pub mod Net.SocketAddrV4 {

    use Net.Ipv4Addr

    ///
    /// Type of IPv4 socket addresses.
    ///
    pub enum SocketAddrV4 with Eq {
        case SocketAddrV4(Ipv4Addr, Int32)
    }

    instance ToString[SocketAddrV4] {
        pub def toString(x: SocketAddrV4): String =
            let SocketAddrV4.SocketAddrV4(ip, port) = x;
            "${ip}:${port}"
    }

    instance Formattable[SocketAddrV4] {
        pub def format(x: SocketAddrV4): RichString = RichString.fromString(ToString.toString(x))
    }

}

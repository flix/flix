/*
 * Copyright 2026 Flix Authors
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

pub mod Net.SocketAddrV6 {

    use Net.Ipv6Addr

    ///
    /// Represents an Ipv6 Socket Address.
    ///
    pub enum SocketAddrV6 with Eq {
        case SocketAddrV6(Ipv6Addr, Int32)
    }

    instance ToString[SocketAddrV6] {
        pub def toString(x: SocketAddrV6): String =
            let SocketAddrV6.SocketAddrV6(ip, port) = x;
            "[${ip}]:${port}"
    }

    instance Formattable[SocketAddrV6] {
        pub def format(x: SocketAddrV6): RichString = RichString.fromString(ToString.toString(x))
    }

}

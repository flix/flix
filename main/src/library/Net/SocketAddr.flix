/*
 *  Copyright 2025 Cade Lueker
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

pub mod Net.SocketAddr {

    use Net.IpAddr
    use Net.Ipv4Addr
    use Net.Ipv6Addr
    use Net.SocketAddr
    use Net.SocketAddrV4
    use Net.SocketAddrV6

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
            case SocketAddr.V4(SocketAddrV4.SocketAddrV4(ip, port)) => "${ip}:${port}"
            case SocketAddr.V6(SocketAddrV6.SocketAddrV6(ip, port)) => "[${ip}]:${port}"
        }
    }

    instance Formattable[SocketAddr] {
        pub def format(x: SocketAddr): RichString = RichString.fromString(ToString.toString(x))
    }

    ///
    /// Attempts to parse the given String `x` as a `SocketAddr`.
    ///
    /// Returns `Some(socket)` if the string was successfully parsed. Otherwise returns `None`.
    ///
    pub def fromString(x: String): Option[SocketAddr] =
        match String.contains(substr = "]", x) {
            case true  => parseSocketV6(x)
            case false => parseSocketV4(x)
        }

    ///
    /// Return the port of the SocketAddr `s`.
    ///
    pub def port(s: SocketAddr): Int32 = match s {
        case SocketAddr.V4(SocketAddrV4.SocketAddrV4(_, p)) => p
        case SocketAddr.V6(SocketAddrV6.SocketAddrV6(_, p)) => p
    }

    ///
    /// Return the IpAddr of the SocketAddr `s`.
    ///
    pub def ip(s: SocketAddr): IpAddr = match s {
        case SocketAddr.V4(SocketAddrV4.SocketAddrV4(ip, _)) => IpAddr.V4(ip)
        case SocketAddr.V6(SocketAddrV6.SocketAddrV6(ip, _)) => IpAddr.V6(ip)
    }

    ///
    /// Attempts to parse the given string `x` as a `SocketAddr.V4`.
    ///
    def parseSocketV4(x: String): Option[SocketAddr] =
        let ipPort = String.split({regex = ":"}, x);
        forM (
            (ip, port) <- separateIpPort(ipPort);
            v4 <- Ipv4Addr.fromString(ip);
            p <- parsePort(port)
        ) yield SocketAddr.V4(SocketAddrV4.SocketAddrV4(v4, p))

    ///
    /// Attempts to parse the given string `x` as a `SocketAddr.V6`.
    ///
    def parseSocketV6(x: String): Option[SocketAddr] =
        forM (
            ipPort <- x |> String.drop(1) |> String.split({regex = "]:"}) |> Some;
            if String.contains(substr = "[", x);
            (ip, port) <- separateIpPort(ipPort);
            v6 <- Ipv6Addr.fromString(ip);
            p <- parsePort(port)
        ) yield SocketAddr.V6(SocketAddrV6.SocketAddrV6(v6, p))

    ///
    /// Attempts to parse the given String `x` as a Socket port.
    ///
    def parsePort(x: String): Option[Int32] =
        forM (
            p <- Int32.fromString(x);
            if 0 < p and p < 65536
        ) yield p

    ///
    /// Attempts to split the given List[String] `x` into IpAddr and port.
    ///
    def separateIpPort(x: List[String]): Option[(String, String)] = match x {
        case ip :: port :: Nil => Some((ip, port))
        case _                 => None
    }

}

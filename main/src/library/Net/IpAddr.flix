/*
 *  Copyright 2025 Cade Lueker
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

pub mod Net.IpAddr {

    use Net.IpAddr
    use Net.Ipv4Addr
    use Net.Ipv6Addr

    import java.net.UnknownHostException

    ///
    /// Represents an IP address.
    ///
    pub enum IpAddr with Eq, ToString {
        case V4(Ipv4Addr)
        case V6(Ipv6Addr)
    }

    instance FromString[IpAddr] {
        pub def fromString(x: String): Option[IpAddr] = IpAddr.fromString(x)
    }

    instance Formattable[IpAddr] {
        pub def format(x: IpAddr): RichString = RichString.fromString(ToString.toString(x))
    }

    ///
    /// Converts a `List[Int8]` into `IpAddr`
    ///
    /// an Ipv4 is 4 bytes and Ipv6 is 16 bytes
    ///
    pub def fromBytes(l: List[Int8]): IpAddr = match l {
        case b1 :: b2 :: b3 :: b4 :: Nil => {
            IpAddr.V4(Ipv4Addr.Ipv4Addr(b1, b2, b3, b4))
        }
        case b1 :: b2 :: b3 :: b4 :: b5 :: b6 :: b7 :: b8 :: b9 :: b10 :: b11 :: b12 :: b13 :: b14 :: b15 :: b16 :: Nil => {
            IpAddr.V6(Ipv6Addr.Ipv6Addr(b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15, b16))
        }
        case _ => unreachable!()
    }

    ///
    /// Converts an `IpAddr` into an `Vector[Int8]`
    ///
    /// an Ipv4 is 4 bytes and Ipv6 is 16 bytes
    ///
    pub def toBytes(ip: IpAddr): Vector[Int8] =
        match ip {
            case IpAddr.V4(Ipv4Addr.Ipv4Addr(b1, b2, b3, b4)) =>
                Vector#{b1, b2, b3, b4}
            case IpAddr.V6(Ipv6Addr.Ipv6Addr(b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15, b16)) =>
                Vector#{b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15, b16}
        }

    ///
    /// Attempts to parse the given String `s` as an `IpAddr`.
    ///
    /// Returns `Some(addr)` if the string was successfully parsed. Otherwise returns `None`.
    ///
    pub def fromString(s: String): Option[IpAddr] =
        try {
            match Ipv6Addr.fromString(s) {
                case Some(v6) => Some(IpAddr.V6(v6))
                case None => match Ipv4Addr.fromString(s) {
                    case Some(v4) => Some(IpAddr.V4(v4))
                    case None     => None
                }
            }
        } catch {
            case _: UnknownHostException => None
        }

}

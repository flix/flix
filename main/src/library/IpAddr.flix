/*
 *  Copyright 2025 Cade Lueker
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

///
/// Represents an IP address.
///
enum IpAddr with Eq, ToString {
    case V4(Ipv4Addr),
    case V6(Ipv6Addr)
}

instance FromString[IpAddr] {
    pub def fromString(x: String): Option[IpAddr] = IpAddr.fromString(x)
}

instance Formattable[IpAddr] {
    pub def format(x: IpAddr): RichString = RichString.fromString(ToString.toString(x))
}

mod IpAddr {

    use Ipv4Addr.Ipv4Addr
    use Ipv6Addr.Ipv6Addr

    import java.net.UnknownHostException

    ///
    /// Converts a `List[Int8]` into `IpAddr`
    ///
    /// an Ipv4 is 4 bytes and Ipv6 is 16 bytes
    ///
    pub def fromBytes(l: List[Int8]): IpAddr = match l {
        case b1::b2::b3::b4::Nil => {
            IpAddr.V4(Ipv4Addr(b1, b2, b3, b4))
        }
        case b1::b2::b3::b4::b5::b6::b7::b8::b9::b10::b11::b12::b13::b14::b15::b16::Nil => {
            IpAddr.V6(Ipv6Addr(b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15, b16))
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
            case IpAddr.V4(Ipv4Addr(b1, b2, b3, b4)) =>
                Vector#{b1, b2, b3, b4}
            case IpAddr.V6(Ipv6Addr(b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15, b16)) =>
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
                case None => None
              }
            }
        } catch {
            case _ : UnknownHostException => None
        }

}


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
/// Represents a Socket Address.
///
enum SocketAddr with Eq {
    case V4(SocketAddrV4)
    case V6(SocketAddrV6)
}

instance FromString[SocketAddr] {
    pub def fromString(x: String): Option[SocketAddr] = SocketAddr.fromString(x)
}

mod SocketAddr {

    use SocketAddrV4.SocketAddrV4
    use SocketAddrV6.SocketAddrV6

    ///
    /// Attempts to parse the given String `x` as a `SocketAddr`.
    ///
    /// Returns `Some(socket)` if the string was successfully parsed. Otherwise returns `None`.
    ///
    pub def fromString(x: String): Option[SocketAddr] =
        match String.contains({substr = "]"}, x) {
            case true => { // Ipv6
                if (not String.contains({substr = "["}, x))
                    None
                else
                    let ip_port = x |> String.drop(1) |> String.split({regex = "]:"});
                    match ip_port {
                        case ip :: port :: Nil => {
                            match IpAddr.fromString(ip) {
                                case Some(IpAddr.V6(v4)) =>
                                    forM(p <- Int32.fromString(port); if p > 0 and p < 65536) yield SocketAddr.V6(SocketAddrV6(v4, p))
                                case _ => None
                            }
                        }
                        case _ => None
                    }
            }
            case false => { // Ipv4
                let ip_port = String.split({regex = ":"}, x);
                match ip_port {
                    case ip :: port :: Nil => {
                        match IpAddr.fromString(ip) {
                            case Some(IpAddr.V4(v4)) =>
                                forM(p <- Int32.fromString(port); if p > 0 and p < 65536) yield SocketAddr.V4(SocketAddrV4(v4, p))
                            case _ => None
                        }
                    }
                    case _ => None
                }
            }
        }

    ///
    /// Return the port of the SocketAddr `s`.
    ///
    pub def port(s: SocketAddr): Int32 = match s {
        case SocketAddr.V4(SocketAddrV4(_, p)) => p
        case SocketAddr.V6(SocketAddrV6(_, p)) => p
    }

    ///
    /// Return the IpAddr of the SocketAddr `s`.
    ///
    pub def ip(s: SocketAddr): IpAddr = match s {
        case SocketAddr.V4(SocketAddrV4(ip, _)) => IpAddr.V4(ip)
        case SocketAddr.V6(SocketAddrV6(ip, _)) => IpAddr.V6(ip)
    }
}


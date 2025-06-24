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
/// Represents a V4 Ip Address.
///
enum Ipv4Addr with Eq, ToString { // 32 bits
    case Ipv4Addr(Int8, Int8, Int8, Int8)
}

instance FromString[Ipv4Addr] {
    pub def fromString(x: String): Option[Ipv4Addr] = Ipv4Addr.fromString(x)
}

mod Ipv4Addr {
    import java.net.InetAddress
    import java.net.UnknownHostException

    use Ipv4Addr.Ipv4Addr

    ///
    /// Attempts to parse the given String `s` as an `Ipv4Addr`.
    ///
    /// Returns `Some(addr)` if the string was successfully parsed. Otherwise returns `None`.
    ///
    pub def fromString(s: String): Option[Ipv4Addr] =
        unsafe try {
            let addr = InetAddress.getByName(s);
            match Array.toList(addr.getAddress()) {
              case b1::b2::b3::b4::Nil => Some(Ipv4Addr(b1, b2, b3, b4))
              case _ => None
            }
        } catch {
            case _ : UnknownHostException => None
        }
}


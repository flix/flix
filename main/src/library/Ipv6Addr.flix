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
///Represents a V6 Ip Address.
///
enum Ipv6Addr with Eq, ToString {
  case Ipv6Addr( // 128 bits
    Int8, Int8, Int8, Int8, // 32 bits
    Int8, Int8, Int8, Int8, // 32 bits
    Int8, Int8, Int8, Int8, // 32 bits
    Int8, Int8, Int8, Int8  // 32 bits
  )
}

mod Ipv6Addr {
    import java.net.InetAddress
    import java.net.UnknownHostException

    use Ipv6Addr.Ipv6Addr

    ///
    /// Parse the string `s` as an Ipv6Addr.
    /// A successful parse is wrapped with `Some(x)`, a parse failure is indicated by `None`.
    ///
    pub def fromString(s: String): Option[Ipv6Addr] = try {
          let addr = unsafe InetAddress.getByName(s);
          match unsafe Array.toList(addr.getAddress()) {
            case b1::b2::b3::b4::b5::b6::b7::b8::b9::b10::b11::b12::b13::b14::b15::b16::Nil =>
              Some(Ipv6Addr(
                b1,  b2,  b3,  b4,
                b5,  b6,  b7,  b8,
                b9,  b10, b11, b12,
                b13, b14, b15, b16
              ))
            case _ => None
          }
        } catch {
          case _ : UnknownHostException => None
        }
}

instance FromString[Ipv6Addr] {
  pub def fromString(x: String): Option[Ipv6Addr] = Ipv6Addr.fromString(x)
}


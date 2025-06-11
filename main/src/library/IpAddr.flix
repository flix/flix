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
/// Represents an IP address
///
enum IpAddr with Eq, ToString {
    case IPv4(Int8, Int8, Int8, Int8), // 32 bits
    case IPv6(Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8) // 128 bits
}

mod IpAddr {
    ///
    /// Converts a `List[Int8]` into `IpAddr`
    ///
    /// an Ipv4 is 32 bytes and Ipv6 is 128 bytes
    ///
    pub def fromBytes(l: List[Int8]): IpAddr = match l {
        case b1 :: b2 :: b3 :: b4 :: Nil => {
            IpAddr.IPv4(b1, b2, b3, b4)
        }
        case b1 :: b2 :: b3 :: b4 :: b5 :: b6 :: b7 :: b8 :: b9 :: b10 :: b11 :: b12 :: b13 :: b14 :: b15 :: b16 :: Nil => {
            IpAddr.IPv6(b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15, b16)
        }
        case _ => unreachable!()
    }

}



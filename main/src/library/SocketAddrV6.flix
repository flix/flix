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
/// Represents an Ipv6 Socket Address.
///
enum SocketAddrV6 with Eq {
    case SocketAddrV6(Ipv6Addr, Int32)
}

instance ToString[SocketAddrV6] {
    pub def toString(x: SocketAddrV6): String =
        let SocketAddrV6.SocketAddrV6(ip, port) = x;
        "[${ToString.toString(ip)}]:${port}"
}

instance Formattable[SocketAddrV6] {
    pub def format(x: SocketAddrV6): RichString = RichString.fromString(ToString.toString(x))
}


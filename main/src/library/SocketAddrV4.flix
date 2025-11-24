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
/// Represents an Ipv4 Socket Address.
///
enum SocketAddrV4 with Eq {
    case SocketAddrV4(Ipv4Addr, Int32)
}

instance ToString[SocketAddrV4] {
    pub def toString(x: SocketAddrV4): String =
        let SocketAddrV4.SocketAddrV4(ip, port) = x;
        "${ToString.toString(ip)}:${port}"
}

instance Formattable[SocketAddrV4] {
    pub def format(x: SocketAddrV4): RichString = RichString.fromString(ToString.toString(x))
}


/*
 * Copyright 2025 Cade Lueker
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

mod TestSocketAddr {

    /////////////////////////////////////////////////////////////////////////////
    // fromString                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def socketAddrFromString0(): Bool =
        match SocketAddr.fromString("192.168.1.1:8080") {
            case Some(s) => {
                let ip = SocketAddr.ip(s);
                let port = SocketAddr.port(s);
                port == 8080 and "V4(192.168.1.1)" == "${ip}"
            }
            case None => false
        }

    @Test
    def socketAddrFromString1(): Bool =
        match SocketAddr.fromString("[2001:db8::1]:443") {
            case Some(s) => {
                let ip = SocketAddr.ip(s);
                let port = SocketAddr.port(s);
                port == 443 and "V6(2001:0DB8:0000:0000:0000:0000:0000:0001)" == "${ip}"
            }
            case None => false
        }

    @Test
    def socketAddrFromString2(): Bool =
        match SocketAddr.fromString("10.0.0.1:53") {
            case Some(s) => {
                let ip = SocketAddr.ip(s);
                let port = SocketAddr.port(s);
                port == 53 and "V4(10.0.0.1)" == "${ip}"
            }
            case None => false
        }

    @Test
    def socketAddrFromString3(): Bool =
        match SocketAddr.fromString("[::1]:8080") {
            case Some(s) => {
                let ip = SocketAddr.ip(s);
                let port = SocketAddr.port(s);
                port == 8080 and "V6(0000:0000:0000:0000:0000:0000:0000:0001)" == "${ip}"
            }
            case None => false
        }

    @Test
    def socketAddrFromString4(): Bool =
        match SocketAddr.fromString("[2001:0db8:85a3::8a2e:0370:7334]:65535") {
            case Some(s) => {
                let ip = SocketAddr.ip(s);
                let port = SocketAddr.port(s);
                port == 65535 and "V6(2001:0DB8:85A3:0000:0000:8A2E:0370:7334)" == "${ip}"
            }
            case None => false
        }

    @Test
    def socketAddrFromString5(): Bool =
        SocketAddr.fromString("256.100.50.25:8080") |> Option.isEmpty

    @Test
    def socketAddrFromString6(): Bool =
        SocketAddr.fromString("156.100.50.25:80808080") |> Option.isEmpty

    @Test
    def socketAddrFromString7(): Bool =
        SocketAddr.fromString("156.100.50.25:65536") |> Option.isEmpty

    @Test
    def socketAddrFromString8(): Bool =
        SocketAddr.fromString("[:1:8080") |> Option.isEmpty

    @Test
    def socketAddrFromString9(): Bool =
        SocketAddr.fromString("0::1]:8080") |> Option.isEmpty

    @Test
    def socketAddrFromString10(): Bool =
       SocketAddr.fromString("[2345:0425:2CA1:0000:0000:0567:5673:23B5:1234]:8080") |> Option.isEmpty

}


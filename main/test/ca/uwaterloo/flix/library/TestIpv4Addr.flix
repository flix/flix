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

mod TestIpv4Addr {

    use Ipv4Addr.Ipv4Addr

    /////////////////////////////////////////////////////////////////////////////
    // fromString                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def fromString0(): Bool =
        Ipv4Addr.fromString("256.100.50.25") |> Option.isEmpty

    @Test
    def fromString1(): Bool =
        Ipv4Addr.fromString("192.168.1") |> Option.isEmpty

    @Test
    def fromString3(): Bool =
        Ipv4Addr.fromString("123.045.067.089") |> Option.isEmpty

    @Test
    def fromString4(): Bool =
        Ipv4Addr.fromString("192.168.1.300") |> Option.isEmpty

    @Test
    def fromString5(): Bool =
        let parseIp = Ipv4Addr.fromString("192.168.1.1");
        let ip = Some(Ipv4Addr(-64i8, -88i8, 1i8, 1i8));
        parseIp == ip

    @Test
    def fromString6(): Bool =
        let parseIp = Ipv4Addr.fromString("8.8.8.8");
        let ip = Some(Ipv4Addr(8i8, 8i8, 8i8, 8i8));
        parseIp == ip

    @Test
    def fromString7(): Bool =
        let parseIp = Ipv4Addr.fromString("10.0.0.255");
        let ip = Some(Ipv4Addr(10i8, 0i8, 0i8, -1i8));
        parseIp == ip

    @Test
    def fromString8(): Bool =
        let parseIp = Ipv4Addr.fromString("172.16.254.3");
        let ip = Some(Ipv4Addr(-84i8, 16i8, -2i8, 3i8));
        parseIp == ip

    @Test
    def fromString9(): Bool =
        let parseIp = Ipv4Addr.fromString("1.2.3.4");
        let ip = Some(Ipv4Addr(1i8, 2i8, 3i8, 4i8));
        parseIp == ip

}


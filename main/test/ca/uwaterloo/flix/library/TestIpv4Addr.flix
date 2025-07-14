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
    def fromString2(): Bool =
        Ipv4Addr.fromString("123.45.67.89.1") |> Option.isEmpty

    @Test
    def fromString3(): Bool =
        Ipv4Addr.fromString("123.045.067.089") |> Option.isEmpty

    @Test
    def fromString4(): Bool =
        Ipv4Addr.fromString("192.168.1.300") |> Option.isEmpty

    @Test
    def fromString5(): Bool =
        let s = "192.168.1.1";
        "${Ipv4Addr.fromString(s)}" == "Some(${s})"

    @Test
    def fromString6(): Bool =
        let s = "8.8.8.8";
        "${Ipv4Addr.fromString(s)}" == "Some(${s})"

    @Test
    def fromString7(): Bool =
        let s = "10.0.0.255";
        "${Ipv4Addr.fromString(s)}" == "Some(${s})"

    @Test
    def fromString8(): Bool =
        let s = "172.16.254.3";
        "${Ipv4Addr.fromString(s)}" == "Some(${s})"

    @Test
    def fromString9(): Bool =
        let s = "1.2.3.4";
        "${Ipv4Addr.fromString(s)}" == "Some(${s})"

    /////////////////////////////////////////////////////////////////////////////
    // localhost                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def localhost0(): Bool =
        let expected = "127.0.0.1";
        "${Ipv4Addr.localhost()}" == expected

}


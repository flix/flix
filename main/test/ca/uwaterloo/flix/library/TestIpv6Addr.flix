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

mod TestIpv6Addr {

    use Ipv6Addr.Ipv6Addr

    /////////////////////////////////////////////////////////////////////////////
    // fromString                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def fromString0(): Bool =
        Ipv6Addr.fromString("192.168.1.1") |> Option.isEmpty

    @Test
    def fromString1(): Bool =
        Ipv6Addr.fromString("2345:0425:2CA1:0000:0000:0567:5673:23G5") |> Option.isEmpty

    @Test
    def fromString2(): Bool =
        Ipv6Addr.fromString("2345:0425:2CA1:0000:0000:0567:5673:23B5:1234") |> Option.isEmpty

    @Test
    def fromString3(): Bool =
        Ipv6Addr.fromString("192:168:1") |> Option.isEmpty

    @Test
    def fromString4(): Bool =
        Ipv6Addr.fromString("2345:0425:2CA1:0567:5673:") |> Option.isEmpty

    @Test
    def fromString5(): Bool =
        let s = "2345:0425:2CA1:0000:0000:0567:5673:23B5";
        "${Ipv6Addr.fromString(s)}" == "Some(${s})"

    @Test
    def fromString6(): Bool =
        let s = "2345:0425:2CA1:0000:0000:0567:5673:23B5";
        "${Ipv6Addr.fromString(s)}" == "Some(${s})"

    @Test
    def fromString7(): Bool =
        let input = "2345:425:2CA1:0:0:567:5673:23B5";
        let expected = "2345:0425:2CA1:0000:0000:0567:5673:23B5";
        "${Ipv6Addr.fromString(input)}" == "Some(${expected})"

    @Test
    def fromString8(): Bool =
        let input = "::1234";
        let expected = "0000:0000:0000:0000:0000:0000:0000:1234";
        "${Ipv6Addr.fromString(input)}" == "Some(${expected})"


    @Test
    def fromString9(): Bool =
        let input = "FE80::1234";
        let expected = "FE80:0000:0000:0000:0000:0000:0000:1234";
        "${Ipv6Addr.fromString(input)}" == "Some(${expected})"

    /////////////////////////////////////////////////////////////////////////////
    // localhost                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def localhost0(): Bool =
        let expected = "0000:0000:0000:0000:0000:0000:0000:0001";
        "${Ipv6Addr.localhost()}" == expected

}


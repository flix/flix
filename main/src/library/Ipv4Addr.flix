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
enum Ipv4Addr with Eq { // 32 bits
    case Ipv4Addr(Int8, Int8, Int8, Int8)
}

instance FromString[Ipv4Addr] {
    pub def fromString(x: String): Option[Ipv4Addr] = Ipv4Addr.fromString(x)
}

instance ToString[Ipv4Addr] {
    pub def toString(x: Ipv4Addr): String = Ipv4Addr.toString(x)
}

instance Formattable[Ipv4Addr] {
    pub def format(x: Ipv4Addr): RichString = RichString.fromString(ToString.toString(x))
}

mod Ipv4Addr {
    import java.net.InetAddress
    import java.net.UnknownHostException
    import java.lang.NumberFormatException

    use Ipv4Addr.Ipv4Addr

    ///
    /// Returns the Ipv4 localhost address.
    ///
    pub def localhost(): Ipv4Addr =
        Ipv4Addr(127i8, 0i8, 0i8, 1i8)

    ///
    /// Convert `x` from Ipv4 to Ipv6.
    ///
    pub def toIpv6(x: Ipv4Addr): Ipv6Addr =
        let Ipv4Addr.Ipv4Addr(b1, b2, b3, b4) = x;
        Ipv6Addr.Ipv6Addr(0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, -1i8, -1i8, b1, b2, b3, b4)

    ///
    /// Attempts to parse the given String `s` as an `Ipv4Addr`.
    ///
    /// Returns `Some(addr)` if the string was successfully parsed. Otherwise returns `None`.
    ///
    pub def fromString(s: String): Option[Ipv4Addr] =
        let bytes = s
            |> String.split({regex = "[.]" })
            |> List.traverse(parseInt8NoLeading0);
        match bytes {
            case Some(b1::b2::b3::b4::Nil) => {
                Some(Ipv4Addr(b1, b2, b3, b4))
            }
            case _ => None
        }

    ///
    /// Returns a String representation of the given Ipv4Addr `x`.
    ///
    pub def toString(x: Ipv4Addr): String =
        let Ipv4Addr.Ipv4Addr(b1, b2, b3, b4) = x;
        "${toUnsigned8(b1)}.${toUnsigned8(b2)}.${toUnsigned8(b3)}.${toUnsigned8(b4)}"

    ///
    /// Convert `x` from i8 to u8.
    ///
    def toUnsigned8(x: Int8): Int32 = Int32.bitwiseAnd(Int8.toInt32(x), 0xFF)

    ///
    /// Attempt to convert `x` from Int32 to Int8.
    ///
    def toSigned8(x: Int32): Option[Int8] =
        forM(
            bu8 <- Some(x); if 0 <= x and x < 256;
            bi8 <- Int32.tryToInt8(
                    Int32.bitwiseXor(
                        Int32.bitwiseAnd(bu8, 0xFF),
                        0x80
                    ) - 0x80
                )
        ) yield bi8

    ///
    /// Attempt to parse `b` from String to Int8.
    ///
    def parseInt8NoLeading0(b: String): Option[Int8] =
        forM(
            s <- Some(b); if not (String.length(b) > 1 and String.startsWith({prefix = "0"}, b));
            u8 <- Int32.fromString(s);
            i8 <- toSigned8(u8)
        ) yield i8

}


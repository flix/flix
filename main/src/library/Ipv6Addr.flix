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

instance FromString[Ipv6Addr] {
    pub def fromString(x: String): Option[Ipv6Addr] = Ipv6Addr.fromString(x)
}

mod Ipv6Addr {
    import java.lang.Integer
    import java.lang.NumberFormatException
    import java.net.InetAddress
    import java.net.UnknownHostException

    use Ipv6Addr.Ipv6Addr

    ///
    /// Attempts to convert a hex String `s` into Int8.
    ///
    def byteStringToInt8(s: String): Option[Int8] =
        try {
            let b = Integer.parseInt(s, 16);
            if (b > 127)
                let bs = Int32.toString(b-256);
                Int8.fromString(bs)
            else
                let bs = Int32.toString(b);
                Int8.fromString(bs)
        } catch {
            case _: NumberFormatException => None
        }

    ///
    /// Parses an Ipv6 hostname String `s` into a List[String].
    ///
    /// Each element of the List is a group of the Ip Address.
    ///
    def cleanIpv6String(s: String): List[String] =
        s
        |> String.replace({src = "::"}, {dst = ":0000:0000:"})    // expand "::"
        |> String.replace({src = ":0:0:"}, {dst = ":0000:0000:"}) // expand ":0:0:"
        |> String.split(regex = "[:]")                            // split groups
        |> List.map(g -> {                                        // add leading 0's
            match String.length(g) {
                case 4 => g
                case i => String.repeat(4 - i,"0") ++ g
            }
        })

    ///
    /// Attemps to convert groups of an Ipv6 address `l` into a List[Int8].
    ///
    def parseIpv6List(l: List[String]): Option[List[Int8]] =
        l
        |> List.map(g -> String.splitAt(2, g))
        |> List.map(b -> {
            let (b1, b2) = b;
            b1 :: b2 :: Nil
        })
        |> List.flatten
        |> List.traverse(byteStringToInt8)

    ///
    /// Attempts to parse the given String `s` as an `Ipv6Addr`.
    ///
    /// Returns `Some(addr)` if the string was successfully parsed. Otherwise returns `None`.
    ///
    pub def fromString(s: String): Option[Ipv6Addr] =
        match s |> cleanIpv6String |> parseIpv6List {
            case Some(b1::b2::b3::b4::b5::b6::b7::b8::b9::b10::b11::b12::b13::b14::b15::b16::Nil) =>
                Some(Ipv6Addr(
                  b1,  b2,  b3,  b4,
                  b5,  b6,  b7,  b8,
                  b9,  b10, b11, b12,
                  b13, b14, b15, b16
                ))
            case _ => None
        }

}


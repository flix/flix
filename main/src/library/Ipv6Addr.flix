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
/// Represents a V6 Ip Address.
///
enum Ipv6Addr with Eq {
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

instance ToString[Ipv6Addr] {
    pub def toString(x: Ipv6Addr): String = Ipv6Addr.toString(x)
}

instance Formattable[Ipv6Addr] {
    pub def format(x: Ipv6Addr): RichString = RichString.fromString(ToString.toString(x))
}

mod Ipv6Addr {
    import java.lang.Integer
    import java.lang.NumberFormatException
    import java.net.InetAddress
    import java.net.UnknownHostException

    use Ipv6Addr.Ipv6Addr

    ///
    /// Attempt to convert `x` from Ipv6 to Ipv4.
    ///
    pub def tryToIpv4(x: Ipv6Addr): Option[Ipv4Addr] = match x {
        case Ipv6Addr(0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, -1i8, -1i8, b1, b2, b3, b4) => Some(Ipv4Addr.Ipv4Addr(b1, b2, b3, b4))
        case _ => None
    }

    ///
    /// Returns the Ipv6 localhost address.
    ///
    pub def localhost(): Ipv6Addr =
        Ipv6Addr(0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 1i8)


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

    ///
    /// Returns a String representation of the given Ipv6Addr `x`.
    ///
    pub def toString(x: Ipv6Addr): String =
        let Ipv6Addr(b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15, b16) = x;
        "${byteToHex(b1, b2)}:${byteToHex(b3, b4)}:${byteToHex(b5, b6)}:${byteToHex(b7, b8)}:${byteToHex(b9, b10)}:${byteToHex(b11, b12)}:${byteToHex(b13, b14)}:${byteToHex(b15, b16)}"

    ///
    /// convert `x` from i8 to u8.
    ///
    def toUnsigned8(x: Int8): Int32 = Int32.bitwiseAnd(Int8.toInt32(x), 0xFF)

    ///
    /// Convert two Int8's into a hex string.
    ///
    def byteToHex(b1: Int8, b2: Int8): String = {
        let combined = (toUnsigned8(b1) * 256) + toUnsigned8(b2);
        let hexString = String.toUpperCase(Integer.toString(combined, 16));
        match String.length(hexString) {
            case 4 => hexString
            case i => String.repeat(4 - i,"0") ++ hexString
        }
    }

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
    pub def cleanIpv6String(s: String): List[String] =
        let compressed = String.countSubstring({substr = "::"}, s);
        if (compressed > 1)
            Nil
        else
            let groups = s
                |> String.replace({src = "::"}, {dst = ":expa:"})         // mark for expansion
                |> String.replace({src = ":0:0:"}, {dst = ":0000:0000:"}) // expand ":0:0:"
                |> String.split(regex = "[:]")                            // split groups
                |> List.map(g -> {                                        // add leading 0's
                    match String.length(g) {
                        case 4 => g
                        case i => String.repeat(4 - i,"0") ++ g
                    }
                });
            let missing = 8 - List.length(groups);
            if (missing == 0)
                groups
            else
                addGroups(groups, missing)

    ///
    /// Fill in expasion gaps in Ipv6 String representation with "0000".
    ///
    def addGroups(l: List[String], missing: Int32): List[String] =
        match l {
            case h :: t => {
                if (h == "expa")
                    List.repeat(missing+1, "0000") ::: t
                else
                    h :: addGroups(t, missing)
            }
            case Nil => Nil
        }

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

}


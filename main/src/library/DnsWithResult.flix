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
/// an effect for resolving a domain name to an ip address
///
eff DnsWithResult {
    def lookup(host: String): Result[IoError, IpAddr]
    def lookupAll(host: String): Result[IoError, List[IpAddr]]
}

mod DnsWithResult {
    use IoError.ErrorKind
    use IoError.IoError

    import java.io.IOException
    import java.lang.IllegalArgumentException
    import java.net.InetAddress
    import java.net.UnknownHostException

    ///
    /// Converts a `List[Int8]` into `IpAddr`
    ///
    /// an Ipv4 is 32 bytes and Ipv6 is 128 bytes
    ///
    def bytesToIpAddr(l: List[Int8]): IpAddr = match l {
        case b1 :: b2 :: b3 :: b4 :: Nil => {
            IpAddr.IPv4(b1, b2, b3, b4)
        }
        case b1 :: b2 :: b3 :: b4 :: b5 :: b6 :: b7 :: b8 :: b9 :: b10 :: b11 :: b12 :: b13 :: b14 :: b15 :: b16 :: Nil => {
            IpAddr.IPv6(b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15, b16)
        }
        case _ => unreachable!()
    }

    ///
    /// Handles the `DnsWithResult` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `DnsWithResult` effect using the `Net` and `IO` effects.
    ///
    pub def handle(f: a -> b \ ef): a -> b \ (ef - DnsWithResult) + {Net, IO} = x ->
        run {
            f(x)
        } with handler DnsWithResult {
            def lookup(host, k) = {
                try {
                    let address = InetAddress.getByName(host);
                    let addrBytes = Array.toList(address.getAddress());
                    let ipAddr = bytesToIpAddr(addrBytes);
                    k(Ok(ipAddr))
                } catch {
                    case ex: UnknownHostException     => k(Err(IoError(ErrorKind.UnknownHost, ex.getMessage())))
                    case ex: IllegalArgumentException => k(Err(IoError(ErrorKind.InvalidInput, ex.getMessage())))
                    case ex: IOException              => k(Err(IoError(ErrorKind.Other, ex.getMessage())))
                }
            }

            def lookupAll(host, k) = {
                try {
                    let addrsArr = InetAddress.getAllByName(host);
                    let addresses = addrsArr
                        |> Array.toList
                        |> List.map(a -> a.getAddress() |> Array.toList)
                        |> List.map(bytesToIpAddr);
                    k(Ok(addresses))
                } catch {
                    case ex: UnknownHostException     => k(Err(IoError(ErrorKind.UnknownHost, ex.getMessage())))
                    case ex: IllegalArgumentException => k(Err(IoError(ErrorKind.InvalidInput, ex.getMessage())))
                    case ex: IOException              => k(Err(IoError(ErrorKind.Other, ex.getMessage())))
                }
            }

        }

    ///
    /// Handles the `DnsWithResult` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `DnsWithResult` effect using the `Net` and `IO` effects.
    ///
    pub def runWithIO(f: Unit -> a \ ef): a \ ef - DnsWithResult + {Net, IO} = handle(f)()

}


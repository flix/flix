/*
 *  Copyright 2025 Cade Lueker
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

pub mod Net.Dns {
    use IoError.ErrorKind
    use IoError.IoError
    use Net.IpAddr

    import java.io.IOException
    import java.lang.IllegalArgumentException
    import java.net.InetAddress
    import java.net.UnknownHostException

    ///
    /// The effect used to resolve domain names to IP addresses.
    ///
    pub eff Dns {
        def lookup(host: String): Result[IoError, IpAddr]
        def lookupAll(host: String): Result[IoError, List[IpAddr]]
    }

    ///
    /// Handles the `Dns` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `Dns` effect using the `IO` effect.
    ///
    pub def handle(f: a -> b \ ef): a -> b \ (ef - Dns) + IO = x ->
        run {
            f(x)
        } with handler Dns {
            def lookup(host, k) = {
                try {
                    let address = InetAddress.getByName(host);
                    let addrBytes = Array.toList(address.getAddress());
                    let ipAddr = IpAddr.fromBytes(addrBytes);
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
                        |> List.map(IpAddr.fromBytes);
                    k(Ok(addresses))
                } catch {
                    case ex: UnknownHostException     => k(Err(IoError(ErrorKind.UnknownHost, ex.getMessage())))
                    case ex: IllegalArgumentException => k(Err(IoError(ErrorKind.InvalidInput, ex.getMessage())))
                    case ex: IOException              => k(Err(IoError(ErrorKind.Other, ex.getMessage())))
                }
            }
        }

    ///
    /// Handles the `Dns` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `Dns` effect using the `IO` effect.
    ///
    @DefaultHandler
    pub def runWithIO(f: Unit -> a \ ef): a \ ef - Dns + IO = handle(f)()

}

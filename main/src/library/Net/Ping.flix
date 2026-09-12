/*
 *  Copyright 2025 Cade Lueker
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

pub mod Net.Ping {
    use IoError.ErrorKind
    use IoError.IoError
    use Net.IpAddr
    use Time.TimeUnit

    import java.io.IOException
    import java.lang.IllegalArgumentException
    import java.net.InetAddress
    import java.net.UnknownHostException

    ///
    /// The effect used to determine if an address is reachable.
    ///
    pub eff Ping {
        def isReachable(host: IpAddr, timeout: Int32, u: TimeUnit): Result[IoError, Bool]
    }

    ///
    /// Handles the `Ping` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `Ping` effect using the `IO` effect.
    ///
    pub def handle(f: a -> b \ ef): a -> b \ (ef - Ping) + IO = x ->
        run {
            f(x)
        } with handler Ping {
            def isReachable(host, timeout, u, k) = {
                try {
                    let ms = TimeUnit.toMilliseconds(u);
                    let timeoutMs = ms * timeout;
                    let addr = InetAddress.getByAddress(IpAddr.toBytes(host));
                    k(Ok(addr.isReachable(timeoutMs)))
                } catch {
                    case ex: UnknownHostException     => k(Err(IoError(ErrorKind.UnknownHost, ex.getMessage())))
                    case ex: IllegalArgumentException => k(Err(IoError(ErrorKind.InvalidInput, ex.getMessage())))
                    case ex: IOException              => k(Err(IoError(ErrorKind.Other, ex.getMessage())))
                }
            }
        }

    ///
    /// Runs the `Ping` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `Ping` effect using the `IO` effect.
    ///
    @DefaultHandler
    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - Ping) + IO = handle(f)()

}

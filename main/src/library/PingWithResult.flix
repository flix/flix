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
/// The effect used to determine if an address is reachable.
///
eff PingWithResult {
    def isReachable(host: IpAddr, timeout: Int32, u: TimeUnit): Result[IoError, Bool]
}

pub mod PingWithResult {
    use IoError.ErrorKind
    use IoError.IoError

    import java.io.IOException
    import java.lang.IllegalArgumentException
    import java.net.InetAddress
    import java.net.UnknownHostException

    ///
    /// Handles the `PingWithResult` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `PingWithResult` effect using the `IO` effect.
    ///
    pub def handle(f: a -> b \ ef): a -> b \ (ef - PingWithResult) + IO = x ->
        run {
            f(x)
        } with handler PingWithResult {
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
    /// Runs the `PingWithResult` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `PingWithResult` effect using the `IO` effect.
    ///
    @DefaultHandler
    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - PingWithResult) + IO = handle(f)()

}


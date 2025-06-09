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
/// The effect used to connect to an address
///
eff Ping {
    def isReachable(host: String, timeout: Int32): Bool
}

mod Ping {
    use IoError.ErrorKind
    use IoError.IoError

    import java.net.InetAddress
    import java.io.IOException
    import java.net.UnknownHostException

    pub def handle(f: a -> b \ ef): a -> Result[IoError, b] \ (ef - Ping) + {Net, IO} = x ->
        run {
            Ok(f(x))
        } with handler Ping {
            def isReachable(host, timeout, k) = {
                try {
                    let addr = InetAddress.getByName(host);
                    k(addr.isReachable(timeout))
                } catch {
                    case ex: IOException => Err(IoError(ErrorKind.Other, ex.getMessage()))
                    case ex: UnknownHostException => Err(IoError(ErrorKind.UnknownHost, ex.getMessage()))
                }
            }
        }
}

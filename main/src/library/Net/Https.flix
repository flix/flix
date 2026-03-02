/*
 * Copyright 2026 Flix Authors
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

pub mod Net.Https {

    use Net.Https
    use Net.HttpClient
    use Net.Retry
    use Net.Request
    use Net.Request.Request
    use Net.Response
    use Net.Response.Response
    use IoError.ErrorKind
    use IoError.IoError

    ///
    /// Sends the given request `req` via the `Https` effect.
    ///
    pub def send(req: Request): Result[IoError, Response] \ Https =
        Https.request(req)

    ///
    /// Sends a GET request to the given `url`.
    ///
    pub def get(url: String): Result[IoError, Response] \ Https =
        send(Request.get(url))

    ///
    /// Sends a HEAD request to the given `url`.
    ///
    pub def head(url: String): Result[IoError, Response] \ Https =
        send(Request.head(url))

    ///
    /// Sends a DELETE request to the given `url`.
    ///
    pub def delete(url: String): Result[IoError, Response] \ Https =
        send(Request.delete(url))

    ///
    /// Sends an OPTIONS request to the given `url`.
    ///
    pub def options(url: String): Result[IoError, Response] \ Https =
        send(Request.options(url))

    ///
    /// Sends a POST request to the given `url` with the given `body`.
    ///
    pub def post(url: String, body: String): Result[IoError, Response] \ Https =
        send(Request.post(url, body))

    ///
    /// Sends a PUT request to the given `url` with the given `body`.
    ///
    pub def put(url: String, body: String): Result[IoError, Response] \ Https =
        send(Request.put(url, body))

    ///
    /// Sends a PATCH request to the given `url` with the given `body`.
    ///
    pub def patch(url: String, body: String): Result[IoError, Response] \ Https =
        send(Request.patch(url, body))

    ///
    /// Validates that the given request URL uses the `https://` scheme.
    ///
    def validateHttps(req: Request): Result[IoError, Unit] =
        let Request(x) = req;
        if (String.startsWith({prefix = "https://"}, x#url))
            Ok()
        else
            Err(IoError(ErrorKind.InvalidInput, "URL must use the https scheme"))

    ///
    /// Handles the `Https` effect of the given function `f`.
    ///
    /// Re-interprets the `Https` effect using the `IO` effect.
    /// Validates that request URLs use the `https://` scheme.
    ///
    pub def handle(f: a -> b \ ef): a -> b \ (ef - Https) + IO = x ->
        run {
            f(x)
        } with handler Https {
            def request(req, k) = k(HttpClient.execute(validateHttps, req))
        }

    ///
    /// Handles the `Https` effect of the given function `f`.
    ///
    /// Re-interprets the `Https` effect using the `Retry` and `IO` effects.
    /// Validates that request URLs use the `https://` scheme.
    /// On retryable failures, raises `Retry.shouldRetry` to decide whether to retry.
    ///
    pub def handleWithRetry(f: a -> b \ ef): a -> b \ (ef - Https) + Retry + IO = x ->
        run {
            f(x)
        } with handler Https {
            def request(req, k) = k(HttpClient.executeWithRetry(validateHttps, req))
        }

    ///
    /// Runs the `Https` effect of the given function `f`.
    ///
    /// Re-interprets the `Https` effect using the `IO` effect.
    ///
    @DefaultHandler
    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - Https) + IO = handle(f)()

    ///
    /// Runs the `Https` effect of the given function `f`.
    ///
    /// Re-interprets the `Https` effect using the `Retry` and `IO` effects.
    ///
    pub def runWithRetry(f: Unit -> a \ ef): a \ (ef - Https) + Retry + IO = handleWithRetry(f)()

}

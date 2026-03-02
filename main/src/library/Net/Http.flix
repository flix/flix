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

pub mod Net.Http {

    use Net.Http
    use Net.HttpClient
    use Net.Retry
    use Net.Request
    use Net.Request.Request
    use Net.Response
    use Net.Response.Response
    use IoError.IoError

    ///
    /// Sends the given request `req` via the `Http` effect.
    ///
    pub def send(req: Request): Result[IoError, Response] \ Http =
        Http.request(req)

    ///
    /// Sends a GET request to the given `url`.
    ///
    pub def get(url: String): Result[IoError, Response] \ Http =
        send(Request.get(url))

    ///
    /// Sends a HEAD request to the given `url`.
    ///
    pub def head(url: String): Result[IoError, Response] \ Http =
        send(Request.head(url))

    ///
    /// Sends a DELETE request to the given `url`.
    ///
    pub def delete(url: String): Result[IoError, Response] \ Http =
        send(Request.delete(url))

    ///
    /// Sends an OPTIONS request to the given `url`.
    ///
    pub def options(url: String): Result[IoError, Response] \ Http =
        send(Request.options(url))

    ///
    /// Sends a POST request to the given `url` with the given `body`.
    ///
    pub def post(url: String, body: String): Result[IoError, Response] \ Http =
        send(Request.post(url, body))

    ///
    /// Sends a PUT request to the given `url` with the given `body`.
    ///
    pub def put(url: String, body: String): Result[IoError, Response] \ Http =
        send(Request.put(url, body))

    ///
    /// Sends a PATCH request to the given `url` with the given `body`.
    ///
    pub def patch(url: String, body: String): Result[IoError, Response] \ Http =
        send(Request.patch(url, body))

    ///
    /// Handles the `Http` effect of the given function `f`.
    ///
    /// Re-interprets the `Http` effect using the `IO` effect.
    ///
    pub def handle(f: a -> b \ ef): a -> b \ (ef - Http) + IO = x ->
        run {
            f(x)
        } with handler Http {
            def request(req, k) = k(HttpClient.execute((_) -> Ok(), req))
        }

    ///
    /// Handles the `Http` effect of the given function `f`.
    ///
    /// Re-interprets the `Http` effect using the `Retry` and `IO` effects.
    /// On retryable failures, raises `Retry.shouldRetry` to decide whether to retry.
    ///
    pub def handleWithRetry(f: a -> b \ ef): a -> b \ (ef - Http) + Retry + IO = x ->
        run {
            f(x)
        } with handler Http {
            def request(req, k) = k(HttpClient.executeWithRetry((_) -> Ok(), req))
        }

    ///
    /// Runs the `Http` effect of the given function `f`.
    ///
    /// Re-interprets the `Http` effect using the `IO` effect.
    ///
    @DefaultHandler
    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - Http) + IO = handle(f)()

    ///
    /// Runs the `Http` effect of the given function `f`.
    ///
    /// Re-interprets the `Http` effect using the `Retry` and `IO` effects.
    ///
    pub def runWithRetry(f: Unit -> a \ ef): a \ (ef - Http) + Retry + IO = handleWithRetry(f)()

}

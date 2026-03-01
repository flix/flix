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
    use Net.Retry
    use Net.Request
    use Net.Request.Request
    use Net.Response
    use Net.Response.Response
    use Net.Method
    use IoError.ErrorKind
    use IoError.IoError

    import java.io.IOException
    import java.io.UncheckedIOException
    import java.lang.IllegalArgumentException
    import java.lang.InterruptedException
    import java.net.ConnectException
    import java.net.http.HttpClient
    import java.net.http.{HttpResponse$BodyHandlers => BodyHandlers}
    import java.net.http.HttpRequest
    import java.net.http.{HttpRequest$BodyPublishers => BodyPublishers}
    import java.net.URI
    import java.util.{List => JList}

    ///
    /// Sends the given request `req` via the `Https` effect.
    ///
    pub def send(req: Request): Result[IoError, Response] \ Https =
        Https.request(req)

    ///
    /// Sends a GET request to the given `url`.
    ///
    pub def get(url: String): Result[IoError, Response] \ Https =
        send(Request({method = Method.Get, url = url, headers = Map.empty(), body = None}))

    ///
    /// Sends a HEAD request to the given `url`.
    ///
    pub def head(url: String): Result[IoError, Response] \ Https =
        send(Request({method = Method.Head, url = url, headers = Map.empty(), body = None}))

    ///
    /// Sends a DELETE request to the given `url`.
    ///
    pub def delete(url: String): Result[IoError, Response] \ Https =
        send(Request({method = Method.Delete, url = url, headers = Map.empty(), body = None}))

    ///
    /// Sends an OPTIONS request to the given `url`.
    ///
    pub def options(url: String): Result[IoError, Response] \ Https =
        send(Request({method = Method.Options, url = url, headers = Map.empty(), body = None}))

    ///
    /// Sends a POST request to the given `url` with the given `body`.
    ///
    pub def post(url: String, body: String): Result[IoError, Response] \ Https =
        send(Request({method = Method.Post, url = url, headers = Map.empty(), body = Some(body)}))

    ///
    /// Sends a PUT request to the given `url` with the given `body`.
    ///
    pub def put(url: String, body: String): Result[IoError, Response] \ Https =
        send(Request({method = Method.Put, url = url, headers = Map.empty(), body = Some(body)}))

    ///
    /// Sends a PATCH request to the given `url` with the given `body`.
    ///
    pub def patch(url: String, body: String): Result[IoError, Response] \ Https =
        send(Request({method = Method.Patch, url = url, headers = Map.empty(), body = Some(body)}))

    ///
    /// Performs the actual HTTP request using the Java HTTP client.
    ///
    def doRequest(req: Request): Result[IoError, Response] \ IO =
        try {
            let Request(x) = req;
            let methodStr = match x#method {
                case Method.Get     => "GET"
                case Method.Head    => "HEAD"
                case Method.Delete  => "DELETE"
                case Method.Options => "OPTIONS"
                case Method.Post    => "POST"
                case Method.Put     => "PUT"
                case Method.Patch   => "PATCH"
            };
            let bodyPublisher = match x#body {
               case Some(b) => BodyPublishers.ofString(b)
               case None    => BodyPublishers.noBody()
            };
            let builder =
               HttpRequest.newBuilder(URI.create(x#url)).method(methodStr, bodyPublisher)
                   |> b -> Map.foldLeftWithKey(
                       (mb, key, values) -> List.foldLeft(
                           (b1, v) -> b1.header(key, v),
                           mb,
                           values
                       ),
                       b,
                       x#headers
                   );
            let jrequest = builder.build();
            let client = HttpClient.newHttpClient();
            let response = client.send(jrequest, BodyHandlers.ofString());
            let responseHeadersJlist: Map[String, JList] = ToFlix.toFlix(response.headers().map());
            let responseHeaders: Map[String, List[String]] = Map.map(ToFlix.toFlix, responseHeadersJlist);
            Ok(Response({
               status = response.statusCode(),
               headers = responseHeaders,
               body = response.body().toString()
            }))
        } catch {
            case ex: IllegalArgumentException => Err(IoError(ErrorKind.InvalidInput, ex.getMessage()))
            case ex: ConnectException          => Err(IoError(ErrorKind.ConnectionFailed, ex.getMessage()))
            case ex: InterruptedException     => Err(IoError(ErrorKind.Interrupted, ex.getMessage()))
            case ex: UncheckedIOException     => Err(IoError(ErrorKind.Other, ex.getMessage()))
            case ex: IOException              => Err(IoError(ErrorKind.Other, ex.getMessage()))
        }

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
            def request(req, k) =
                match validateHttps(req) {
                    case Err(error) => k(Err(error))
                    case Ok()       => k(doRequest(req))
                }
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
            def request(req, k) =
                match validateHttps(req) {
                    case Err(error) => k(Err(error))
                    case Ok()       =>
                        def loop(attempt) =
                            match doRequest(req) {
                                case Ok(resp) => k(Ok(resp))
                                case Err(error) =>
                                    if (Retry.shouldRetry(attempt, req, error))
                                        loop(attempt + 1)
                                    else
                                        k(Err(error))
                            };
                        loop(0)
                }
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

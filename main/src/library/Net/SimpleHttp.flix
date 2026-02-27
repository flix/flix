/*
 * Copyright 2025 Magnus Madsen
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

pub mod Net.SimpleHttp {

    use Net.Method
    use Net.Method.{Get, Head, Delete, Options, Post, Put, Patch}
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
    /// A response from an HTTP request.
    ///
    pub enum Response({status = Int32, headers = Map[String, String], body = String})

    ///
    /// Send a `GET` request to the given `url` and wait for the response.
    ///
    pub def get(url: String): Result[IoError, Response] \ Net.SimpleHttp =
        Net.SimpleHttp.request(Get, url, Map.empty(), None)

    ///
    /// Send a `HEAD` request to the given `url` and wait for the response.
    ///
    /// A `HEAD` request is identical to a `GET` request except that the server should not return a message-body in the response.
    ///
    pub def head(url: String): Result[IoError, Response] \ Net.SimpleHttp =
        Net.SimpleHttp.request(Head, url, Map.empty(), None)

    ///
    /// Send a `DELETE` request to the given `url` and wait for the response.
    ///
    pub def delete(url: String): Result[IoError, Response] \ Net.SimpleHttp =
        Net.SimpleHttp.request(Delete, url, Map.empty(), None)

    ///
    /// Send an `OPTIONS` request to the given `url` and wait for the response.
    ///
    pub def options(url: String): Result[IoError, Response] \ Net.SimpleHttp =
        Net.SimpleHttp.request(Options, url, Map.empty(), None)

    ///
    /// Send a `POST` request to the given `url` with the given `body` and wait for the response.
    ///
    pub def post(url: String, body: String): Result[IoError, Response] \ Net.SimpleHttp =
        Net.SimpleHttp.request(Post, url, Map.empty(), Some(body))

    ///
    /// Send a `PUT` request to the given `url` with the given `body` and wait for the response.
    ///
    pub def put(url: String, body: String): Result[IoError, Response] \ Net.SimpleHttp =
        Net.SimpleHttp.request(Put, url, Map.empty(), Some(body))

    ///
    /// Send a `PATCH` request to the given `url` with the given `body` and wait for the response.
    ///
    pub def patch(url: String, body: String): Result[IoError, Response] \ Net.SimpleHttp =
        Net.SimpleHttp.request(Patch, url, Map.empty(), Some(body))

    ///
    /// Handles the `SimpleHttp` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `SimpleHttp` effect using the `IO` effect.
    ///
    pub def handle(f: a -> b \ ef): a -> b \ (ef - Net.SimpleHttp) + IO = x ->
        run {
            f(x)
        } with handler Net.SimpleHttp {
            def request(method, url, headers, body, k) =
                try {
                    let methodStr = match method {
                        case Get     => "GET"
                        case Head    => "HEAD"
                        case Delete  => "DELETE"
                        case Options => "OPTIONS"
                        case Post    => "POST"
                        case Put     => "PUT"
                        case Patch   => "PATCH"
                    };
                    let bodyPublisher = match body {
                       case Some(b) => BodyPublishers.ofString(b)
                       case None    => BodyPublishers.noBody()
                    };
                    let builder =
                       HttpRequest.newBuilder(URI.create(url)).method(methodStr, bodyPublisher)
                           |> builder -> Map.foldLeftWithKey(
                               (mb, key, v) -> mb.header(key, v),
                               builder,
                               headers
                           );
                    let request = builder.build();

                    let client = HttpClient.newHttpClient();
                    let response = client.send(request, BodyHandlers.ofString());

                    let responseHeadersJlist: Map[String, JList] = ToFlix.toFlix(response.headers().map());
                    let responseHeadersMulti: Map[String, List[String]] = Map.map(ToFlix.toFlix, responseHeadersJlist);
                    let responseHeaders: Map[String, String] = Map.filterMap(List.head, responseHeadersMulti);

                    k(Ok(Response.Response({
                       status = response.statusCode(),
                       headers = responseHeaders,
                       body = response.body().toString() // Body already is a string but has type Object
                    })))
                } catch {
                    case ex: IllegalArgumentException => k(Err(IoError(ErrorKind.InvalidInput, ex.getMessage())))
                    case _: ConnectException          => k(Err(IoError(ErrorKind.ConnectionFailed, "Connection failed")))
                    case ex: InterruptedException     => k(Err(IoError(ErrorKind.Interrupted, ex.getMessage())))
                    case ex: UncheckedIOException     => k(Err(IoError(ErrorKind.Other, ex.getMessage())))
                    case ex: IOException              => k(Err(IoError(ErrorKind.Other, ex.getMessage())))
                }
        }

    ///
    /// Runs the `SimpleHttp` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `SimpleHttp` effect using the `IO` effect.
    ///
    @DefaultHandler
    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - Net.SimpleHttp) + IO = handle(f)()

}

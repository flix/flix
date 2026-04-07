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

pub mod Net.HttpLayer {

    use IoError.ErrorKind
    use IoError.IoError
    use Net.Body
    use Net.Method
    use Net.HttpRequest
    use Net.HttpRequest.HttpRequest
    use Net.HttpResponse
    use Net.HttpResponse.HttpResponse
    use Time.Duration.toMillis

    import java.io.IOException
    import java.io.UncheckedIOException
    import java.lang.IllegalArgumentException
    import java.lang.InterruptedException
    import java.lang.{Thread => JThread}
    import java.lang.{Throwable => JThrowable}
    import java.lang.{String => JString}
    import java.net.ConnectException
    import java.net.URI
    import java.net.URLEncoder
    import java.nio.charset.StandardCharsets
    import java.net.http.HttpClient
    import java.net.http.{HttpClient$Redirect => Redirect}
    import java.net.http.{HttpRequest => JHttpRequest}
    import java.net.http.{HttpRequest$BodyPublishers => BodyPublishers}
    import java.net.http.HttpTimeoutException
    import java.net.http.{HttpResponse$BodyHandlers => BodyHandlers}
    import java.time.{Duration => JDuration}
    import java.util.{List => JList}

    ///
    /// Builds a default `HttpClient` with `Redirect.NORMAL` and no connect timeout.
    ///
    pub def newDefaultClient(): HttpClient \ IO =
        HttpClient.newBuilder().followRedirects(Redirect.NORMAL).build()

    ///
    /// Sleeps the current thread for `ms` milliseconds.
    ///
    pub def sleep(ms: Int64): Unit \ IO =
        JThread.sleep(ms)

    ///
    /// Returns the URL-encoded form of the string `s`.
    ///
    pub def urlEncode(s: String): String =
        URLEncoder.encode(s, StandardCharsets.UTF_8)

    ///
    /// Decodes the byte vector `v` as a UTF-8 string.
    ///
    pub def bytesToString(v: Vector[Int8]): String =
        unchecked_cast(new JString(v, StandardCharsets.UTF_8) as _ \ {})

    ///
    /// Encodes the string `s` as a UTF-8 byte vector.
    ///
    pub def stringToBytes(s: String): Vector[Int8] =
        unchecked_cast(Array.toVector(s.getBytes(StandardCharsets.UTF_8)) as _ \ {})

    ///
    /// Returns the exception message, or a fallback if `getMessage()` returns null.
    ///
    def messageOf(ex: JThrowable): String \ IO =
        let msg = ex.getMessage();
        if (Object.isNull(msg)) ex.getClass().getName() else msg

    ///
    /// Performs the actual HTTP request using the Java HTTP client.
    ///
    /// Reuses `defaultClient` when the request has default client-level settings
    /// (`connectTimeout == None` and `followRedirects == true`).
    /// Otherwise, builds a one-off client with the request's specific settings.
    ///
    pub def doRequest(defaultClient: HttpClient, req: HttpRequest): Result[IoError, HttpResponse] \ IO =
        try {
            let HttpRequest(x) = req;
            let methodStr = match x#method {
                case Method.Get     => "GET"
                case Method.Head    => "HEAD"
                case Method.Options => "OPTIONS"
                case Method.Post    => "POST"
                case Method.Put     => "PUT"
                case Method.Patch   => "PATCH"
                case Method.Delete  => "DELETE"
            };
            let bodyPublisher = match x#body {
               case Some(Body.Text(s))  => BodyPublishers.ofString(s)
               case Some(Body.Bytes(v)) => BodyPublishers.ofByteArray(v)
               case None                => BodyPublishers.noBody()
            };
            let builder =
               JHttpRequest.newBuilder(URI.create(x#url)).method(methodStr, bodyPublisher)
                   |> b -> Map.foldLeftWithKey(
                       (mb, key, values) -> List.foldLeft(
                           (b1, v) -> b1.header(key, v),
                           mb,
                           values
                       ),
                       b,
                       x#headers
                   );
            let jrequest = match x#requestTimeout {
                case None       => builder.build()
                case Some(d)    => builder.timeout(JDuration.ofMillis(toMillis(d))).build()
            };
            let client = {
                let HttpRequest(x0) = req;
                if (x0#connectTimeout == None and x0#followRedirects == true)
                    defaultClient
                else {
                    let redirectPolicy = if (x0#followRedirects) Redirect.NORMAL else Redirect.NEVER;
                    match x0#connectTimeout {
                        case None    => HttpClient.newBuilder().followRedirects(redirectPolicy).build()
                        case Some(d) => HttpClient.newBuilder().followRedirects(redirectPolicy).connectTimeout(JDuration.ofMillis(toMillis(d))).build()
                    }
                }
            };
            let response = client.send(jrequest, BodyHandlers.ofByteArray());
            let responseHeadersJlist: Map[String, JList[String]] = ToFlix.toFlix(response.headers().map());
            let responseHeaders: Map[String, List[String]] = Map.map(ToFlix.toFlix, responseHeadersJlist);
            let bytes = response.body();
            Ok(HttpResponse({
               url = response.uri().toString(),
               status = response.statusCode(),
               headers = responseHeaders,
               body = Body.Bytes(Array.toVector(bytes))
            }))
        } catch {
            case ex: IllegalArgumentException => Err(IoError(ErrorKind.InvalidInput, messageOf(checked_cast(ex))))
            case ex: HttpTimeoutException     => Err(IoError(ErrorKind.Timeout, messageOf(checked_cast(ex))))
            case ex: ConnectException          => Err(IoError(ErrorKind.ConnectionFailed, messageOf(checked_cast(ex))))
            case ex: InterruptedException     => Err(IoError(ErrorKind.Interrupted, messageOf(checked_cast(ex))))
            case ex: UncheckedIOException     => Err(IoError(ErrorKind.Other, messageOf(checked_cast(ex))))
            case ex: IOException              => Err(IoError(ErrorKind.Other, messageOf(checked_cast(ex))))
        }

    ///
    /// Validates then performs an HTTP request.
    ///
    pub def execute(validate: HttpRequest -> Result[IoError, Unit],
                    defaultClient: HttpClient,
                    req: HttpRequest): Result[IoError, HttpResponse] \ IO =
        match validate(req) {
            case Err(e) => Err(e)
            case Ok()   => doRequest(defaultClient, req)
        }

}

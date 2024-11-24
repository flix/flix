/*
 *  Copyright 2024 Holger Dal Mogensen
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
/// The effect used to interact with the HTTP protocol.
///
eff Http {

    ///
    /// Send a `GET` request to the given `url` with the given `headers` and wait for the response.
    ///
    def get(url: String, headers: Map[String, List[String]]): Http.Response

    ///
    /// Send a `HEAD` request to the given `url` with the given `headers` and wait for the response.
    ///
    /// A `HEAD` request is identical to a `GET` request except that the server should not return a message-body in the response.
    ///
    def head(url: String, headers: Map[String, List[String]]): Http.Response

    ///
    /// Send a `DELETE` request to the given `url` with the given `headers` and wait for the response.
    ///
    def delete(url: String, headers: Map[String, List[String]]): Http.Response

    ///
    /// Send an `OPTIONS` request to the given `url` with the given `headers` and wait for the response.
    ///
    def options(url: String, headers: Map[String, List[String]]): Http.Response

    ///
    /// Send a `TRACE` request to the given `url` with the given `headers` and wait for the response.
    ///
    def trace(url: String, headers: Map[String, List[String]]): Http.Response

    ///
    /// Send a `POST` request to the given `url` with the given `headers` and `body` and wait for the response.
    ///
    def post(url: String, headers: Map[String, List[String]], body: String): Http.Response

    ///
    /// Send a `PUT` request to the given `url` with the given `headers` and `body` and wait for the response.
    ///
    def put(url: String, headers: Map[String, List[String]], body: String): Http.Response

    ///
    /// Send a `PATCH` request to the given `url` with the given `headers` and `body` and wait for the response.
    ///
    def patch(url: String, headers: Map[String, List[String]], body: String): Http.Response

}

mod Http {

    import java.net.http.HttpClient
    import java.net.http.{HttpResponse$BodyHandlers => BodyHandlers}
    import java.net.http.HttpRequest
    import java.net.http.{HttpRequest$BodyPublishers => BodyPublishers}
    import java.net.URI
    import java.util.{List => JList}

    ///
    /// A response from an HTTP request.
    ///
    pub enum Response({status = Int32, headers = Map[String, List[String]], body = String})

    mod Response {
        pub def status(r: Response): Int32 = match r {
            case Response(x) => x#status
        }
        pub def headers(r: Response): Map[String, List[String]] = match r {
            case Response(x) => x#headers
        }
        pub def body(r: Response): String = match r {
            case Response(x) => x#body
        }
    }

    ///
    /// Handles the `Http` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `Http` effect using the `Net` and `IO` effects.
    ///
    pub def handle(f: a -> b \ ef): a -> b \ (ef - Http) + {Net, IO} = x ->
        try {
            f(x)
        } with Http {
            def get(url, headers, k)         = k(sendRequest(url, "GET", headers, None))
            def head(url, headers, k)        = k(sendRequest(url, "HEAD", headers, None))
            def delete(url, headers, k)      = k(sendRequest(url, "DELETE", headers, None))
            def options(url, headers, k)     = k(sendRequest(url, "OPTIONS", headers, None))
            def trace(url, headers, k)       = k(sendRequest(url, "TRACE", headers, None))
            def post(url, headers, body, k)  = k(sendRequest(url, "POST", headers, Some(body)))
            def put(url, headers, body, k)   = k(sendRequest(url, "PUT", headers, Some(body)))
            def patch(url, headers, body, k) = k(sendRequest(url, "PATCH", headers, Some(body)))
        }

    def sendRequest(url: String, method: String, headers: Map[String, List[String]], body: Option[String]): Http.Response \ {Net, IO} = {
        let bodyPublisher = match body {
            case Some(b) => BodyPublishers.ofString(b)
            case None    => BodyPublishers.noBody()
        };
        let builder =
            HttpRequest.newBuilder(URI.create(url)).method(method, bodyPublisher)
                |> builder -> Map.foldLeftWithKey(
                    (mb, k, l) -> List.foldLeft((lb, v) -> lb.header(k, v), mb, l),
                    builder,
                    headers
                );
        let request = builder.build();

        let client = HttpClient.newHttpClient();
        let response = client.send(request, BodyHandlers.ofString());

        let responseHeadersJlist: Map[String, JList] = FromJava.fromJava(response.headers().map());
        let responseHeaders: Map[String, List[String]] = Map.map(FromJava.fromJava, responseHeadersJlist);

        Http.Response.Response({
            status = response.statusCode(),
            headers = responseHeaders,
            body = response.body().toString() // Body already is a string but has type Object
        })
    }

}

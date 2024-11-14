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
    def get(url: String, headers: Map[String, String]): Http.Response
}

mod Http {

    import java.net.http.HttpClient
    import java.net.http.{HttpResponse$BodyHandlers => BodyHandlers}
    import java.net.http.HttpRequest
    import java.net.URI

    ///
    /// A response from an HTTP request.
    ///
    pub enum Response({status = Int32, statusText = String, headers = Map[String, String], body = String})

    mod Response {
        pub def status(r: Response): Int32 = match r {
            case Response(x) => x#status
        }
        pub def statusText(r: Response): String = match r {
            case Response(x) => x#statusText
        }
        pub def headers(r: Response): Map[String, String] = match r {
            case Response(x) => x#headers
        }
        pub def body(r: Response): String = match r {
            case Response(x) => x#body
        }
    }

    ///
    /// Handles the `Http` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `Http` effect using the `IO` effect.
    ///
    pub def handle(f: a -> b \ ef): a -> b \ (ef - Http) + IO = x ->
        try {
            f(x)
        } with Http {
            def get(url, headers, k) = {
                let builder =
                    HttpRequest.newBuilder(URI.create(url))
                        |> builder -> Map.foldLeftWithKey((b, k, v) -> b.header(k, v), builder, headers);
                let request = builder.build();

                let client = HttpClient.newHttpClient();
                let response = client.send(request, BodyHandlers.ofString());

                let responseHeadersJlist: Map[String, JList] = FromJava.fromJava(response.headers().map());
                let responseHeaders = Map.map(l -> l.toString(), responseHeadersJlist);

                k(Http.Response.Response({
                    status = response.statusCode(),
                    statusText = "", // TODO: Get status text
                    headers = responseHeaders,
                    body = response.body().toString() // Body already is a string but has type Object
                }))
            }
        }

}

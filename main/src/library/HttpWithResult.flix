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
eff HttpWithResult {
    def request(method: String, url: String, headers: Map[String, List[String]], body: Option[String]): Result[IoError, Http.Response]
}

pub mod HttpWithResult {

    use IoError.ErrorKind
    use IoError.IoError

    def headersFromPairs(pairs: Vector[String]): Map[String, List[String]] =
        let n = Vector.length(pairs);
        def loop(i, m) = {
            if (i >= n) {
                m
            } else {
                let k = Vector.get(i, pairs);
                let v = Vector.get(i + 1, pairs);
                let m1 = Map.insertWith(
                    (newList, oldList) -> match newList {
                        case x :: _ => x :: oldList
                        case Nil => oldList
                    },
                    k,
                    v :: Nil,
                    m
                );
                loop(i + 2, m1)
            }
        };
        let mRev = loop(0, Map.empty());
        Map.map(List.reverse, mRev)

    ///
    /// Send a `GET` request to the given `url` with the given `headers` and wait for the response.
    ///
    pub def get(url: String, headers: Map[String, List[String]]): Result[IoError, Http.Response] \ HttpWithResult =
        HttpWithResult.request("GET", url, headers, None)

    ///
    /// Send a `HEAD` request to the given `url` with the given `headers` and wait for the response.
    ///
    /// A `HEAD` request is identical to a `GET` request except that the server should not return a message-body in the response.
    ///
    pub def head(url: String, headers: Map[String, List[String]]): Result[IoError, Http.Response] \ HttpWithResult =
        HttpWithResult.request("HEAD", url, headers, None)

    ///
    /// Send a `DELETE` request to the given `url` with the given `headers` and wait for the response.
    ///
    pub def delete(url: String, headers: Map[String, List[String]]): Result[IoError, Http.Response] \ HttpWithResult =
        HttpWithResult.request("DELETE", url, headers, None)

    ///
    /// Send an `OPTIONS` request to the given `url` with the given `headers` and wait for the response.
    ///
    pub def options(url: String, headers: Map[String, List[String]]): Result[IoError, Http.Response] \ HttpWithResult =
        HttpWithResult.request("OPTIONS", url, headers, None)

    ///
    /// Send a `TRACE` request to the given `url` with the given `headers` and wait for the response.
    ///
    pub def trace(url: String, headers: Map[String, List[String]]): Result[IoError, Http.Response] \ HttpWithResult =
        HttpWithResult.request("TRACE", url, headers, None)

    ///
    /// Send a `POST` request to the given `url` with the given `headers` and `body` and wait for the response.
    ///
    pub def post(url: String, headers: Map[String, List[String]], body: String): Result[IoError, Http.Response] \ HttpWithResult =
        HttpWithResult.request("POST", url, headers, Some(body))

    ///
    /// Send a `PUT` request to the given `url` with the given `headers` and `body` and wait for the response.
    ///
    pub def put(url: String, headers: Map[String, List[String]], body: String): Result[IoError, Http.Response] \ HttpWithResult =
        HttpWithResult.request("PUT", url, headers, Some(body))

    ///
    /// Send a `PATCH` request to the given `url` with the given `headers` and `body` and wait for the response.
    ///
    pub def patch(url: String, headers: Map[String, List[String]], body: String): Result[IoError, Http.Response] \ HttpWithResult =
        HttpWithResult.request("PATCH", url, headers, Some(body))

    ///
    /// Handles the `HttpWithResult` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `HttpWithResult` effect using the `IO` effect.
    ///
    pub def handle(f: a -> b \ ef): a -> b \ (ef - HttpWithResult) + IO = x ->
        run {
            f(x)
        } with handler HttpWithResult {
	            def request(method, url, headers, bodyOpt, k) = {
	                let res = region rc {
	                    let reqPairs =
	                        headers
	                            |> Map.toList
	                            |> List.flatMap(e -> {
	                                let key = fst(e);
	                                let vs = snd(e);
	                                vs |> List.flatMap(v -> key :: v :: Nil)
	                            });
	                    let reqHeaders = List.toArray(rc, reqPairs);

	                    let (hasBody, bodyStr) = match bodyOpt {
	                        case Some(b) => (true, b)
	                        case None => (false, "")
	                    };

	                    let (ok, status, respPairs, respBody, kindCode, msg) = %%HTTP_REQUEST%%((method, url, reqHeaders, hasBody, bodyStr));
	                    if (ok) {
	                        let respHeaders = headersFromPairs(respPairs);
	                        Ok(Http.Response.Response({
	                            status = status,
	                            headers = respHeaders,
	                            body = respBody
	                        }))
	                    } else {
	                        let kind = IoError.errorKindFromCode(kindCode);
	                        let msg1 = match kind {
	                            case ErrorKind.ConnectionFailed => "Connection failed"
	                            case _ => msg
	                        };
	                        Err(IoError(kind, msg1))
	                    }
	                };
	                k(res)
	            }
        }

    ///
    /// Runs the `HttpWithResult` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `HttpWithResult` effect using the `IO` effect.
    ///
    @DefaultHandler
    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - HttpWithResult) + IO = handle(f)()

}

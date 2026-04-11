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
    def request(method: String, url: String, headers: Map[String, List[String]], body: Option[String]): Http.Response
}

pub mod Http {

    use IoError.IoError

    ///
    /// A response from an HTTP request.
    ///
    pub enum Response({status = Int32, headers = Map[String, List[String]], body = String})

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
    pub def get(url: String, headers: Map[String, List[String]]): Http.Response \ Http =
        Http.request("GET", url, headers, None)

    ///
    /// Send a `HEAD` request to the given `url` with the given `headers` and wait for the response.
    ///
    /// A `HEAD` request is identical to a `GET` request except that the server should not return a message-body in the response.
    ///
    pub def head(url: String, headers: Map[String, List[String]]): Http.Response \ Http =
        Http.request("HEAD", url, headers, None)

    ///
    /// Send a `DELETE` request to the given `url` with the given `headers` and wait for the response.
    ///
    pub def delete(url: String, headers: Map[String, List[String]]): Http.Response \ Http =
        Http.request("DELETE", url, headers, None)

    ///
    /// Send an `OPTIONS` request to the given `url` with the given `headers` and wait for the response.
    ///
    pub def options(url: String, headers: Map[String, List[String]]): Http.Response \ Http =
        Http.request("OPTIONS", url, headers, None)

    ///
    /// Send a `TRACE` request to the given `url` with the given `headers` and wait for the response.
    ///
    pub def trace(url: String, headers: Map[String, List[String]]): Http.Response \ Http =
        Http.request("TRACE", url, headers, None)

    ///
    /// Send a `POST` request to the given `url` with the given `headers` and `body` and wait for the response.
    ///
    pub def post(url: String, headers: Map[String, List[String]], body: String): Http.Response \ Http =
        Http.request("POST", url, headers, Some(body))

    ///
    /// Send a `PUT` request to the given `url` with the given `headers` and `body` and wait for the response.
    ///
    pub def put(url: String, headers: Map[String, List[String]], body: String): Http.Response \ Http =
        Http.request("PUT", url, headers, Some(body))

    ///
    /// Send a `PATCH` request to the given `url` with the given `headers` and `body` and wait for the response.
    ///
    pub def patch(url: String, headers: Map[String, List[String]], body: String): Http.Response \ Http =
        Http.request("PATCH", url, headers, Some(body))

    ///
    /// Handles the `Http` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `Http` effect using the `IO` effect.
    ///
    pub def handle(f: a -> b \ ef): a -> Result[IoError, b] \ (ef - Http) + IO = x ->
        run {
            Ok(f(x))
        } with handler Http {
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
	                        Err(IoError(IoError.errorKindFromCode(kindCode), msg))
	                    }
	                };
	                match res {
	                    case Ok(resp) => k(resp)
	                    case Err(err) => Err(err)
	                }
	            }
        }

    ///
    /// Runs the `Http` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `Http` effect using the `IO` effect.
    ///
    pub def runWithIO(f: Unit -> a \ ef): Result[IoError, a] \ (ef - Http) + IO = handle(f)()

}

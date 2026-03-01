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

pub mod Net.Request {

    use Net.Request
    use Net.Request.Request
    use Net.Method

    ///
    /// Returns the HTTP method of the request `r`.
    ///
    pub def method(r: Request): Method = match r {
        case Request(x) => x#method
    }

    ///
    /// Returns the URL of the request `r`.
    ///
    pub def url(r: Request): String = match r {
        case Request(x) => x#url
    }

    ///
    /// Returns the headers of the request `r`.
    ///
    pub def headers(r: Request): Map[String, List[String]] = match r {
        case Request(x) => x#headers
    }

    ///
    /// Returns the body of the request `r`.
    ///
    pub def body(r: Request): Option[String] = match r {
        case Request(x) => x#body
    }

    ///
    /// Creates a GET request to the given `url`.
    ///
    pub def get(url: String): Request =
        Request({method = Method.Get, url = url, headers = Map.empty(), body = None})

    ///
    /// Creates a HEAD request to the given `url`.
    ///
    pub def head(url: String): Request =
        Request({method = Method.Head, url = url, headers = Map.empty(), body = None})

    ///
    /// Creates a DELETE request to the given `url`.
    ///
    pub def delete(url: String): Request =
        Request({method = Method.Delete, url = url, headers = Map.empty(), body = None})

    ///
    /// Creates an OPTIONS request to the given `url`.
    ///
    pub def options(url: String): Request =
        Request({method = Method.Options, url = url, headers = Map.empty(), body = None})

    ///
    /// Creates a POST request to the given `url` with the given `body`.
    ///
    pub def post(url: String, body: String): Request =
        Request({method = Method.Post, url = url, headers = Map.empty(), body = Some(body)})

    ///
    /// Creates a PUT request to the given `url` with the given `body`.
    ///
    pub def put(url: String, body: String): Request =
        Request({method = Method.Put, url = url, headers = Map.empty(), body = Some(body)})

    ///
    /// Creates a PATCH request to the given `url` with the given `body`.
    ///
    pub def patch(url: String, body: String): Request =
        Request({method = Method.Patch, url = url, headers = Map.empty(), body = Some(body)})

    ///
    /// Adds the header `name` with value `value` to the request `r`.
    ///
    /// If the header already exists, the value is appended to the list of values.
    ///
    pub def withHeader(name: String, value: String, r: Request): Request =
        let Request(x) = r;
        let current = x#headers;
        let values = match Map.get(name, current) {
            case Some(vs) => List.append(vs, value :: Nil)
            case None     => value :: Nil
        };
        Request({method = x#method, url = x#url, headers = Map.insert(name, values, current), body = x#body})

    ///
    /// Merges the given headers `h` into the request `r`.
    ///
    /// For headers that already exist, the new values are appended to the existing values.
    ///
    pub def withHeaders(h: Map[String, List[String]], r: Request): Request =
        let Request(x) = r;
        let merged = Map.foldLeftWithKey((acc, name, newValues) -> {
            let existing = match Map.get(name, acc) {
                case Some(vs) => vs
                case None     => Nil
            };
            Map.insert(name, List.append(existing, newValues), acc)
        }, x#headers, h);
        Request({method = x#method, url = x#url, headers = merged, body = x#body})

    ///
    /// Sets the body of the request `r` to the given `body`.
    ///
    pub def withBody(body: String, r: Request): Request =
        let Request(x) = r;
        Request({method = x#method, url = x#url, headers = x#headers, body = Some(body)})

}

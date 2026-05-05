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

pub mod Net.HttpRequest {

    use Net.Body
    use Net.HttpLayer
    use Net.Method
    use Net.HttpRequest
    use Net.HttpRequest.HttpRequest
    use Time.Duration

    ///
    /// An HTTP request with method, URL, multi-valued headers, and optional body.
    ///
    pub enum HttpRequest({
        method          = Method,
        url             = String,
        headers         = Map[String, List[String]],
        body            = Option[Body],
        requestTimeout  = Option[Duration],
        connectTimeout  = Option[Duration],
        followRedirects = Bool
    })

    // --- Constructors ---

    ///
    /// Creates a GET request to the given `url`.
    ///
    pub def get(url: String): HttpRequest =
        HttpRequest({method = Method.Get, url = url, headers = Map.empty(), body = None,
                 requestTimeout = None, connectTimeout = None, followRedirects = true})

    ///
    /// Creates a HEAD request to the given `url`.
    ///
    pub def head(url: String): HttpRequest =
        HttpRequest({method = Method.Head, url = url, headers = Map.empty(), body = None,
                 requestTimeout = None, connectTimeout = None, followRedirects = true})

    ///
    /// Creates an OPTIONS request to the given `url`.
    ///
    pub def options(url: String): HttpRequest =
        HttpRequest({method = Method.Options, url = url, headers = Map.empty(), body = None,
                 requestTimeout = None, connectTimeout = None, followRedirects = true})

    ///
    /// Creates a POST request to the given `url` with the given `body`.
    ///
    pub def post(url: String, body: String): HttpRequest =
        HttpRequest({method = Method.Post, url = url, headers = Map.empty(), body = Some(Body.Text(body)),
                 requestTimeout = None, connectTimeout = None, followRedirects = true})

    ///
    /// Creates a PUT request to the given `url` with the given `body`.
    ///
    pub def put(url: String, body: String): HttpRequest =
        HttpRequest({method = Method.Put, url = url, headers = Map.empty(), body = Some(Body.Text(body)),
                 requestTimeout = None, connectTimeout = None, followRedirects = true})

    ///
    /// Creates a PATCH request to the given `url` with the given `body`.
    ///
    pub def patch(url: String, body: String): HttpRequest =
        HttpRequest({method = Method.Patch, url = url, headers = Map.empty(), body = Some(Body.Text(body)),
                 requestTimeout = None, connectTimeout = None, followRedirects = true})

    ///
    /// Creates a DELETE request to the given `url`.
    ///
    pub def delete(url: String): HttpRequest =
        HttpRequest({method = Method.Delete, url = url, headers = Map.empty(), body = None,
                 requestTimeout = None, connectTimeout = None, followRedirects = true})

    // --- Getters ---

    ///
    /// Returns the HTTP method of the request `r`.
    ///
    pub def method(r: HttpRequest): Method = match r {
        case HttpRequest(x) => x#method
    }

    ///
    /// Returns the URL of the request `r`.
    ///
    pub def url(r: HttpRequest): String = match r {
        case HttpRequest(x) => x#url
    }

    ///
    /// Returns the headers of the request `r`.
    ///
    pub def headers(r: HttpRequest): Map[String, List[String]] = match r {
        case HttpRequest(x) => x#headers
    }

    ///
    /// Returns the text body of the request `r`, or `None` if absent or binary.
    ///
    pub def body(r: HttpRequest): Option[String] = match r {
        case HttpRequest(x) => match x#body {
            case Some(Body.Text(s)) => Some(s)
            case _                  => None
        }
    }

    ///
    /// Returns the binary body of the request `r`, or `None` if absent or text.
    ///
    pub def bodyBytes(r: HttpRequest): Option[Vector[Int8]] = match r {
        case HttpRequest(x) => match x#body {
            case Some(Body.Bytes(v)) => Some(v)
            case _                   => None
        }
    }

    ///
    /// Returns the request timeout of the request `r`.
    ///
    pub def requestTimeout(r: HttpRequest): Option[Duration] = match r {
        case HttpRequest(x) => x#requestTimeout
    }

    ///
    /// Returns the connect timeout of the request `r`.
    ///
    pub def connectTimeout(r: HttpRequest): Option[Duration] = match r {
        case HttpRequest(x) => x#connectTimeout
    }

    ///
    /// Returns `true` if the request `r` follows redirects.
    ///
    pub def followRedirects(r: HttpRequest): Bool = match r {
        case HttpRequest(x) => x#followRedirects
    }

    // --- Modifiers: Headers ---

    ///
    /// Adds the header `name` with value `value` to the request `r`.
    ///
    /// If the header already exists, the value is appended to the list of values.
    ///
    pub def withHeader(name: String, value: String, r: HttpRequest): HttpRequest =
        let HttpRequest(x) = r;
        let current = x#headers;
        let values = match Map.get(name, current) {
            case Some(vs) => List.append(vs, value :: Nil)
            case None     => value :: Nil
        };
        HttpRequest({headers = Map.insert(name, values, current) | x})

    ///
    /// Sets the header `name` to `value` on the request `r`, replacing any existing values.
    ///
    pub def setHeader(name: String, value: String, r: HttpRequest): HttpRequest =
        let HttpRequest(x) = r;
        HttpRequest({headers = Map.insert(name, value :: Nil, x#headers) | x})

    ///
    /// Merges the given headers `h` into the request `r`.
    ///
    /// For headers that already exist, the new values are appended to the existing values.
    ///
    pub def withHeaders(h: Map[String, List[String]], r: HttpRequest): HttpRequest =
        let HttpRequest(x) = r;
        let merged = Map.foldLeftWithKey((acc, name, newValues) -> {
            let existing = Map.getWithDefault(name, Nil, acc);
            Map.insert(name, List.append(existing, newValues), acc)
        }, x#headers, h);
        HttpRequest({headers = merged | x})

    ///
    /// Sets the `Authorization` header to `Bearer <token>` on the request `r`.
    ///
    pub def withBearerToken(token: String, r: HttpRequest): HttpRequest =
        setHeader("Authorization", "Bearer ${token}", r)

    ///
    /// Sets the `Content-Type` header to `contentType` on the request `r`.
    ///
    pub def withContentType(contentType: String, r: HttpRequest): HttpRequest =
        setHeader("Content-Type", contentType, r)

    ///
    /// Sets the `Accept` header to `accept` on the request `r`.
    ///
    pub def withAccept(accept: String, r: HttpRequest): HttpRequest =
        setHeader("Accept", accept, r)

    // --- Modifiers: Body ---

    ///
    /// Sets the body of the request `r` to the given text `body`.
    ///
    pub def withBody(body: String, r: HttpRequest): HttpRequest =
        let HttpRequest(x) = r;
        HttpRequest({body = Some(Body.Text(body)) | x})

    ///
    /// Sets the body of the request `r` to the given binary `body`.
    ///
    pub def withBodyBytes(body: Vector[Int8], r: HttpRequest): HttpRequest =
        let HttpRequest(x) = r;
        HttpRequest({body = Some(Body.Bytes(body)) | x})

    // --- Modifiers: Query Parameters ---

    ///
    /// Appends a query parameter `name=value` to the URL of the request `r`.
    ///
    /// The name and value are URL-encoded.
    ///
    pub def withQueryParam(name: String, value: String, r: HttpRequest): HttpRequest =
        let HttpRequest(x) = r;
        let encodedName = HttpLayer.urlEncode(name);
        let encodedValue = HttpLayer.urlEncode(value);
        let separator = if (String.contains(substr = "?", x#url)) "&" else "?";
        let newUrl = "${x#url}${separator}${encodedName}=${encodedValue}";
        HttpRequest({url = newUrl | x})

    ///
    /// Appends multiple query parameters to the URL of the request `r`.
    ///
    /// The names and values are URL-encoded.
    ///
    pub def withQueryParams(params: Map[String, String], r: HttpRequest): HttpRequest =
        Map.foldLeftWithKey((acc, name, value) -> withQueryParam(name, value, acc), r, params)

    // --- Modifiers: Timeouts & Redirects ---

    ///
    /// Sets the request timeout (time until response headers are received).
    ///
    pub def withTimeout(d: Duration, r: HttpRequest): HttpRequest =
        let HttpRequest(x) = r;
        HttpRequest({requestTimeout = Some(d) | x})

    ///
    /// Sets the connect timeout (time to establish the TCP connection).
    ///
    pub def withConnectTimeout(d: Duration, r: HttpRequest): HttpRequest =
        let HttpRequest(x) = r;
        HttpRequest({connectTimeout = Some(d) | x})

    ///
    /// Sets whether the request `r` should follow HTTP redirects.
    ///
    pub def withFollowRedirects(follow: Bool, r: HttpRequest): HttpRequest =
        let HttpRequest(x) = r;
        HttpRequest({followRedirects = follow | x})

}

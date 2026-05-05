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

pub mod Net.HttpResponse {

    use IoError.ErrorKind
    use IoError.IoError
    use Net.Body
    use Net.HttpLayer
    use Net.HttpResponse
    use Net.HttpResponse.HttpResponse

    ///
    /// An HTTP response with final URL, status code, multi-valued headers, and body.
    ///
    pub enum HttpResponse({
        url     = String,
        status  = Int32,
        headers = Map[String, List[String]],
        body    = Body
    })

    ///
    /// Returns the final URL of the response `r` (after any redirects).
    ///
    pub def url(r: HttpResponse): String = match r {
        case HttpResponse(x) => x#url
    }

    ///
    /// Returns the status code of the response `r`.
    ///
    pub def status(r: HttpResponse): Int32 = match r {
        case HttpResponse(x) => x#status
    }

    ///
    /// Returns the headers of the response `r`.
    ///
    pub def headers(r: HttpResponse): Map[String, List[String]] = match r {
        case HttpResponse(x) => x#headers
    }

    ///
    /// Returns the body of the response `r` as a string.
    ///
    /// If the body is binary, it is decoded as UTF-8.
    ///
    pub def body(r: HttpResponse): String = match r {
        case HttpResponse(x) => match x#body {
            case Body.Text(s)  => s
            case Body.Bytes(v) => HttpLayer.bytesToString(v)
        }
    }

    ///
    /// Returns the body of the response `r` as bytes.
    ///
    /// If the body is text, it is encoded as UTF-8.
    ///
    pub def bodyBytes(r: HttpResponse): Vector[Int8] = match r {
        case HttpResponse(x) => match x#body {
            case Body.Bytes(v) => v
            case Body.Text(s)  => HttpLayer.stringToBytes(s)
        }
    }

    ///
    /// Returns the list of values for the header with the given `name`, or `None` if not present.
    ///
    pub def header(name: String, r: HttpResponse): Option[List[String]] =
        Map.get(name, headers(r))

    ///
    /// Returns the first value of the header with the given `name`, or `None` if not present.
    ///
    pub def headerValue(name: String, r: HttpResponse): Option[String] =
        match header(name, r) {
            case Some(v :: _) => Some(v)
            case _            => None
        }

    ///
    /// Returns `true` if the response status is 200 OK.
    ///
    pub def isOk(r: HttpResponse): Bool = status(r) == 200

    ///
    /// Returns `true` if the response status is in the 2xx range.
    ///
    pub def isSuccess(r: HttpResponse): Bool =
        let s = status(r);
        200 <= s and s <= 299

    ///
    /// Returns `true` if the response status is in the 3xx range.
    ///
    pub def isRedirect(r: HttpResponse): Bool =
        let s = status(r);
        300 <= s and s <= 399

    ///
    /// Returns `true` if the response status is in the 4xx range.
    ///
    pub def isClientError(r: HttpResponse): Bool =
        let s = status(r);
        400 <= s and s <= 499

    ///
    /// Returns `true` if the response status is in the 5xx range.
    ///
    pub def isServerError(r: HttpResponse): Bool =
        let s = status(r);
        500 <= s and s <= 599

    ///
    /// Returns `true` if the response status is in the 4xx or 5xx range.
    ///
    pub def isError(r: HttpResponse): Bool =
        isClientError(r) or isServerError(r)

    ///
    /// Returns `Ok(r)` if the response status is in the 2xx range, or `Err` otherwise.
    ///
    pub def expect(r: HttpResponse): Result[IoError, HttpResponse] =
        if (isSuccess(r)) Ok(r)
        else Err(IoError(ErrorKind.Other, "Unexpected HTTP status: ${status(r)}"))

    ///
    /// Returns the value of the `content-type` header, or `None` if not present.
    ///
    pub def contentType(r: HttpResponse): Option[String] =
        headerValue("content-type", r)

    ///
    /// Returns the value of the `content-length` header parsed as an `Int64`, or `None` if not present or not a valid number.
    ///
    pub def contentLength(r: HttpResponse): Option[Int64] =
        match headerValue("content-length", r) {
            case Some(v) => Int64.fromString(v)
            case None    => None
        }

}

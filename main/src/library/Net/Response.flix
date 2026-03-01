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

pub mod Net.Response {

    use Net.Response
    use Net.Response.Response

    ///
    /// Returns the status code of the response `r`.
    ///
    pub def status(r: Response): Int32 = match r {
        case Response(x) => x#status
    }

    ///
    /// Returns the headers of the response `r`.
    ///
    pub def headers(r: Response): Map[String, List[String]] = match r {
        case Response(x) => x#headers
    }

    ///
    /// Returns the body of the response `r`.
    ///
    pub def body(r: Response): String = match r {
        case Response(x) => x#body
    }

    ///
    /// Returns the list of values for the header with the given `name`, or `None` if not present.
    ///
    pub def header(name: String, r: Response): Option[List[String]] =
        Map.get(name, headers(r))

    ///
    /// Returns the first value of the header with the given `name`, or `None` if not present.
    ///
    pub def headerValue(name: String, r: Response): Option[String] =
        match header(name, r) {
            case Some(v :: _) => Some(v)
            case _            => None
        }

    ///
    /// Returns `true` if the response status is 200 OK.
    ///
    pub def isOk(r: Response): Bool = status(r) == 200

    ///
    /// Returns `true` if the response status is in the 2xx range.
    ///
    pub def isSuccess(r: Response): Bool =
        let s = status(r);
        s >= 200 and s <= 299

    ///
    /// Returns `true` if the response status is in the 3xx range.
    ///
    pub def isRedirect(r: Response): Bool =
        let s = status(r);
        s >= 300 and s <= 399

    ///
    /// Returns `true` if the response status is in the 4xx range.
    ///
    pub def isClientError(r: Response): Bool =
        let s = status(r);
        s >= 400 and s <= 499

    ///
    /// Returns `true` if the response status is in the 5xx range.
    ///
    pub def isServerError(r: Response): Bool =
        let s = status(r);
        s >= 500 and s <= 599

    ///
    /// Returns `true` if the response status is in the 4xx or 5xx range.
    ///
    pub def isError(r: Response): Bool =
        isClientError(r) or isServerError(r)

}

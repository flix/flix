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

pub mod Net {

    ///
    /// An enum representing an HTTP method.
    ///
    pub enum Method with Eq, ToString {
        case Get
        case Head
        case Delete
        case Options
        case Post
        case Put
        case Patch
    }

    ///
    /// An HTTP request with method, URL, multi-valued headers, and optional body.
    ///
    pub enum Request({
        method  = Method,
        url     = String,
        headers = Map[String, List[String]],
        body    = Option[String]
    })

    ///
    /// An HTTP response with status code, multi-valued headers, and body.
    ///
    pub enum Response({
        status  = Int32,
        headers = Map[String, List[String]],
        body    = String
    })

    ///
    /// The effect used to send HTTP requests (both http:// and https://).
    ///
    pub eff Http {
        def request(req: Request): Result[IoError, Response]
    }

    ///
    /// The effect used to send HTTPS-only requests.
    ///
    pub eff Https {
        def request(req: Request): Result[IoError, Response]
    }

    ///
    /// The effect used to decide whether a failed request should be retried.
    ///
    pub eff Retry {
        def shouldRetry(attempt: Int32, request: Request, error: IoError): Bool
    }

}

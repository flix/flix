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

    use Time.Duration

    ///
    /// An enum representing an HTTP method.
    ///
    pub enum Method with Eq, ToString {
        case Get
        case Head
        case Options
        case Post
        case Put
        case Patch
        case Delete
    }

    ///
    /// An enum representing the body of an HTTP request or response.
    ///
    pub enum Body with Eq, ToString {
        case Text(String)
        case Bytes(Vector[Int8])
    }

    ///
    /// Represents a timeout for an HTTP operation.
    ///
    pub enum Timeout with Eq, ToString {
        /// No timeout — wait indefinitely.
        case NoTimeout
        /// Timeout after the given duration.
        case After(Duration)
    }

    ///
    /// Describes why a retry is being considered.
    ///
    pub enum RetryOutcome {
        /// A transport-level failure (connection refused, timeout, etc.).
        case TransportError(IoError)
        /// A response was received but its status code warrants a retry (e.g. 429, 503).
        case HttpError(Response)
    }

    ///
    /// The decision returned by a retry handler.
    ///
    pub enum RetryDecision with Eq, ToString {
        /// Retry after the given duration.
        case Retry(Duration)
        /// Do not retry; propagate the last result.
        case Stop
    }

    ///
    /// An HTTP request with method, URL, multi-valued headers, and optional body.
    ///
    pub enum Request({
        method          = Method,
        url             = String,
        headers         = Map[String, List[String]],
        body            = Option[Body],
        requestTimeout  = Timeout,
        connectTimeout  = Timeout,
        followRedirects = Bool
    })

    ///
    /// An HTTP response with final URL, status code, multi-valued headers, and body.
    ///
    pub enum Response({
        url     = String,
        status  = Int32,
        headers = Map[String, List[String]],
        body    = Body
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

}

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

pub mod Net.Http {

    use IoError.ErrorKind
    use IoError.IoError
    use Net.Http
    use Net.HttpClient
    use Net.Request
    use Net.Request.Request
    use Net.Response
    use Net.Response.Response
    use Net.RetryDecision
    use Net.RetryOutcome
    use RichString.{text, bold, cyan, green, yellow, red, gray}

    import java.lang.{Thread => JThread}

    ///
    /// Sends the given request `req` via the `Http` effect.
    ///
    pub def send(req: Request): Result[IoError, Response] \ Http =
        Http.request(req)

    ///
    /// Sends a GET request to the given `url`.
    ///
    pub def get(url: String): Result[IoError, Response] \ Http =
        send(Request.get(url))

    ///
    /// Sends a HEAD request to the given `url`.
    ///
    pub def head(url: String): Result[IoError, Response] \ Http =
        send(Request.head(url))

    ///
    /// Sends an OPTIONS request to the given `url`.
    ///
    pub def options(url: String): Result[IoError, Response] \ Http =
        send(Request.options(url))

    ///
    /// Sends a POST request to the given `url` with the given `body`.
    ///
    pub def post(url: String, body: String): Result[IoError, Response] \ Http =
        send(Request.post(url, body))

    ///
    /// Sends a PUT request to the given `url` with the given `body`.
    ///
    pub def put(url: String, body: String): Result[IoError, Response] \ Http =
        send(Request.put(url, body))

    ///
    /// Sends a PATCH request to the given `url` with the given `body`.
    ///
    pub def patch(url: String, body: String): Result[IoError, Response] \ Http =
        send(Request.patch(url, body))

    ///
    /// Sends a DELETE request to the given `url`.
    ///
    pub def delete(url: String): Result[IoError, Response] \ Http =
        send(Request.delete(url))

    ///
    /// Handles the `Http` effect of the given function `f`.
    ///
    /// Re-interprets the `Http` effect using the `IO` effect.
    ///
    pub def handle(f: a -> b \ ef): a -> b \ (ef - Http) + IO = x ->
        let client = HttpClient.newDefaultClient();
        run {
            f(x)
        } with handler Http {
            def request(req, k) = k(HttpClient.execute((_) -> Ok(), client, req))
        }

    ///
    /// Runs the `Http` effect of the given function `f`.
    ///
    /// Re-interprets the `Http` effect using the `IO` effect.
    ///
    @DefaultHandler
    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - Http) + IO = handle(f)()

    ///
    /// Middleware that intercepts the `Http` effect, enforcing a minimum interval
    /// (in milliseconds) between consecutive HTTP requests.
    ///
    pub def withMinInterval(intervalMs: Int64, f: Unit -> a \ ef): a \ (ef - Http) + {Clock, Http} =
        region rc {
            let last = Ref.fresh(rc, 0i64);
            run { f() } with handler Http {
                def request(req, k) =
                    let now = Clock.now();
                    let wait = intervalMs - (now - Ref.get(last));
                    if (wait > 0i64) unsafe IO { JThread.sleep(wait) } else ();
                    Ref.put(Clock.now(), last);
                    k(Http.request(req))
            }
        }

    ///
    /// Middleware that intercepts the `Http` effect, enforcing a token-bucket
    /// rate limit. Allows an initial burst of `burstSize` requests, then
    /// sustains at 1 request per `intervalMs` milliseconds.
    ///
    pub def withTokenBucket(burstSize: Int32, intervalMs: Int64, f: Unit -> a \ ef): a \ (ef - Http) + {Clock, Http} =
        region rc {
            let tokens   = Ref.fresh(rc, burstSize);
            let lastTime = Ref.fresh(rc, Clock.now());
            run { f() } with handler Http {
                def request(req, k) =
                    def acquire() = {
                        let now     = Clock.now();
                        let elapsed = now - Ref.get(lastTime);
                        let added   = Int64.clampToInt32(min = 0, max = burstSize, elapsed / intervalMs);
                        if (added > 0)
                            Ref.put(Int32.min(burstSize, Ref.get(tokens) + added), tokens)
                        else
                            ();
                        if (added > 0)
                            Ref.put(Ref.get(lastTime) + Int32.toInt64(added) * intervalMs, lastTime)
                        else
                            ();
                        if (Ref.get(tokens) >= 1) {
                            Ref.put(Ref.get(tokens) - 1, tokens)
                        } else {
                            let wait = intervalMs - (Clock.now() - Ref.get(lastTime));
                            if (wait > 0i64) unsafe IO { JThread.sleep(wait) } else ();
                            acquire()
                        }
                    };
                    acquire();
                    k(Http.request(req))
            }
        }

    ///
    /// Middleware that intercepts the `Http` effect, enforcing a sliding-window
    /// rate limit. Allows at most `maxRequests` within any rolling `windowMs`-millisecond
    /// window. When the limit is reached, sleeps until the oldest request expires.
    ///
    pub def withSlidingWindow(maxRequests: Int32, windowMs: Int64, f: Unit -> a \ ef): a \ (ef - Http) + {Clock, Http} =
        region rc {
            let timestamps = MutDeque.empty(rc);
            run { f() } with handler Http {
                def request(req, k) =
                    def acquire() = {
                        let now = Clock.now();
                        def evict() = match MutDeque.peekFront(timestamps) {
                            case Some(ts) if now - ts > windowMs =>
                                discard MutDeque.popFront(timestamps);
                                evict()
                            case _ => ()
                        };
                        evict();
                        if (MutDeque.size(timestamps) < maxRequests) {
                            MutDeque.pushBack(now, timestamps)
                        } else {
                            match MutDeque.peekFront(timestamps) {
                                case Some(oldest) =>
                                    let wait = windowMs - (now - oldest);
                                    if (wait > 0i64) unsafe IO { JThread.sleep(wait) } else ();
                                    acquire()
                                case None =>
                                    // maxRequests <= 0: sleep for the full window and retry.
                                    unsafe IO { JThread.sleep(windowMs) };
                                    acquire()
                            }
                        }
                    };
                    acquire();
                    k(Http.request(req))
            }
        }

    ///
    /// Middleware that intercepts the `Http` effect, implementing a circuit breaker.
    ///
    /// Tracks consecutive failures (transport errors and 5xx server errors).
    /// After `failureThreshold` consecutive failures, the circuit opens and
    /// immediately rejects requests for `cooldownMs` milliseconds.
    /// After the cooldown, one probe request is allowed through (half-open state).
    /// A successful probe closes the circuit; a failed probe restarts the cooldown.
    ///
    /// Non-failure responses (2xx, 3xx, 4xx) reset the failure counter.
    ///
    pub def withCircuitBreaker(failureThreshold: Int32, cooldownMs: Int64,
                               f: Unit -> a \ ef): a \ (ef - Http) + {Clock, Http} =
        region rc {
            let failures  = Ref.fresh(rc, 0);
            let openSince = Ref.fresh(rc, 0i64);
            run { f() } with handler Http {
                def request(req, k) =
                    let now = Clock.now();
                    let since = Ref.get(openSince);
                    if (since > 0i64 and now - since < cooldownMs)
                        // Open: reject immediately
                        k(Err(IoError(ErrorKind.Other, "Circuit breaker is open")))
                    else {
                        // Closed or Half-Open: allow the request
                        let result = Http.request(req);
                        match result {
                            case Err(_) =>
                                let count = Ref.get(failures) + 1;
                                Ref.put(count, failures);
                                if (count >= failureThreshold)
                                    Ref.put(Clock.now(), openSince)
                                else
                                    ();
                                k(result)
                            case Ok(resp) =>
                                if (Response.isServerError(resp)) {
                                    let count = Ref.get(failures) + 1;
                                    Ref.put(count, failures);
                                    if (count >= failureThreshold)
                                        Ref.put(Clock.now(), openSince)
                                    else
                                        ()
                                } else {
                                    Ref.put(0, failures);
                                    Ref.put(0i64, openSince)
                                };
                                k(result)
                        }
                    }
            }
        }

    ///
    /// Middleware that intercepts the `Http` effect, adding retry logic.
    ///
    /// The `strategy` function decides whether to retry and how long to wait.
    /// Each `Http.request` is re-raised to an outer `Http` handler that performs the actual I/O.
    ///
    pub def withRetry(strategy: (Int32, Request, RetryOutcome) -> RetryDecision \ ef1,
                      f: Unit -> a \ ef2): a \ ef1 + (ef2 - Http) + Http =
        run { f() } with handler Http {
            def request(req, k) =
                def loop(attempt) =
                    match Http.request(req) {
                        case Ok(resp) if Response.isSuccess(resp) => Ok(resp)
                        case Ok(resp) =>
                            match strategy(attempt, req, RetryOutcome.HttpError(resp)) {
                                case RetryDecision.Retry(d) => unsafe IO { JThread.sleep(d); loop(attempt + 1) } // sleep is pure here
                                case RetryDecision.Stop     => Ok(resp)
                            }
                        case Err(error) =>
                            match strategy(attempt, req, RetryOutcome.TransportError(error)) {
                                case RetryDecision.Retry(d) => unsafe IO { JThread.sleep(d); loop(attempt + 1) } // sleep is pure here
                                case RetryDecision.Stop     => Err(error)
                            }
                    };
                k(loop(0))
        }

    ///
    /// Middleware that intercepts the `Http` effect, injecting default headers
    /// into every request. Headers already present on the request are not overridden.
    ///
    pub def withDefaultHeaders(defaults: Map[String, List[String]],
                               f: Unit -> a \ ef): a \ (ef - Http) + Http =
        run { f() } with handler Http {
            def request(req, k) =
                let Request(x) = req;
                let merged = Map.unionWith((reqVals, _) -> reqVals, x#headers, defaults);
                k(Http.request(Request({headers = merged | x})))
        }

    ///
    /// Middleware that intercepts the `Http` effect, prepending `base` to
    /// request URLs that do not already start with a scheme. Absolute URLs
    /// (where "://" appears before the first "/") are left unchanged.
    ///
    pub def withBaseUrl(base: String, f: Unit -> a \ ef): a \ (ef - Http) + Http =
        let base1 = if (String.endsWith(suffix = "/", base))
                         String.dropRight(1, base) else base;
        run { f() } with handler Http {
            def request(req, k) =
                let Request(x) = req;
                let url = x#url;
                // Check that "://" appears before the first "/" to detect a scheme.
                let hasScheme = match String.indexOf(substr = "://", url) {
                    case Some(i) => match String.indexOf(substr = "/", url) {
                        case Some(s) => i < s
                        case None    => true
                    }
                    case None => false
                };
                if (hasScheme)
                    k(Http.request(req))
                else
                    let path1 = if (String.startsWith(prefix = "/", url))
                                     url else "/${url}";
                    k(Http.request(Request({url = "${base1}${path1}" | x})))
        }

    ///
    /// Middleware that intercepts the `Http` effect, logging each request
    /// and its corresponding response via the `Logger` effect at `Debug` level.
    ///
    pub def withLogging(f: Unit -> a \ ef): a \ (ef - Http) + {Http, Logger} =
        run { f() } with handler Http {
            def request(req, k) =
                let method = String.toUpperCase("${Request.method(req)}");
                let url    = Request.url(req);
                Logger.log(Severity.Debug,
                    gray("-->") + text(" ") + bold(method) + text(" ") + cyan(url));
                let result = Http.request(req);
                match result {
                    case Ok(resp) =>
                        let status = Response.status(resp);
                        let color  = if (status >= 200 and status <= 299)
                                         green("${status}")
                                     else if (status >= 400)
                                         red("${status}")
                                     else
                                         yellow("${status}");
                        Logger.log(Severity.Debug,
                            gray("<--") + text(" ") + color + text(" ") + cyan(url))
                    case Err(err) =>
                        Logger.log(Severity.Debug,
                            gray("<--") + text(" ") + red("ERR") + text(" ") + cyan(url) +
                            text(" ") + red("${err}"))
                };
                k(result)
        }

}

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

pub mod Net.Https {

    use IoError.ErrorKind
    use IoError.IoError
    use Logger.Severity
    use Net.HttpLayer
    use Net.Https
    use Net.HttpRequest
    use Net.HttpRequest.HttpRequest
    use Net.HttpResponse
    use Net.HttpResponse.HttpResponse
    use Net.RetryDecision
    use Net.RetryOutcome
    use RichString.{text, bold, cyan, green, yellow, red, gray}
    use Time.Clock
    use Time.Duration
    use Time.Duration.toMillis


    ///
    /// The effect used to send HTTPS-only requests.
    ///
    pub eff Https {
        def request(req: HttpRequest): Result[IoError, HttpResponse]
    }

    ///
    /// Sends the given request `req` via the `Https` effect.
    ///
    pub def send(req: HttpRequest): Result[IoError, HttpResponse] \ Https =
        Https.request(req)

    ///
    /// Sends a GET request to the given `url`.
    ///
    pub def get(url: String): Result[IoError, HttpResponse] \ Https =
        send(HttpRequest.get(url))

    ///
    /// Sends a HEAD request to the given `url`.
    ///
    pub def head(url: String): Result[IoError, HttpResponse] \ Https =
        send(HttpRequest.head(url))

    ///
    /// Sends an OPTIONS request to the given `url`.
    ///
    pub def options(url: String): Result[IoError, HttpResponse] \ Https =
        send(HttpRequest.options(url))

    ///
    /// Sends a POST request to the given `url` with the given `body`.
    ///
    pub def post(url: String, body: String): Result[IoError, HttpResponse] \ Https =
        send(HttpRequest.post(url, body))

    ///
    /// Sends a PUT request to the given `url` with the given `body`.
    ///
    pub def put(url: String, body: String): Result[IoError, HttpResponse] \ Https =
        send(HttpRequest.put(url, body))

    ///
    /// Sends a PATCH request to the given `url` with the given `body`.
    ///
    pub def patch(url: String, body: String): Result[IoError, HttpResponse] \ Https =
        send(HttpRequest.patch(url, body))

    ///
    /// Sends a DELETE request to the given `url`.
    ///
    pub def delete(url: String): Result[IoError, HttpResponse] \ Https =
        send(HttpRequest.delete(url))

    ///
    /// Validates that the given request URL uses the `https://` scheme.
    ///
    def validateHttps(req: HttpRequest): Result[IoError, Unit] =
        let HttpRequest(x) = req;
        if (String.startsWith({prefix = "https://"}, x#url))
            Ok()
        else
            Err(IoError(ErrorKind.InvalidInput, "URL must use the https scheme"))

    ///
    /// Handles the `Https` effect of the given function `f`.
    ///
    /// Re-interprets the `Https` effect using the `IO` effect.
    /// Validates that request URLs use the `https://` scheme.
    ///
    pub def handle(f: a -> b \ ef): a -> b \ (ef - Https) + IO = x ->
        let client = HttpLayer.newDefaultClient();
        run {
            f(x)
        } with handler Https {
            def request(req, k) = k(HttpLayer.execute(validateHttps, client, req))
        }

    ///
    /// Runs the `Https` effect of the given function `f`.
    ///
    /// Re-interprets the `Https` effect using the `IO` effect.
    ///
    @DefaultHandler
    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - Https) + IO = handle(f)()

    ///
    /// Middleware that intercepts the `Https` effect, enforcing a minimum interval
    /// between consecutive HTTPS requests.
    ///
    pub def withMinInterval(ims: {interval = Duration}, f: Unit -> a \ ef): a \ (ef - Https) + {Clock, Https} =
        let intervalMs = toMillis(ims#interval);
        region rc {
            let last = Ref.fresh(rc, 0i64);
            run { f() } with handler Https {
                def request(req, k) =
                    let now = Clock.now();
                    let wait = intervalMs - (now - Ref.get(last));
                    if (wait > 0i64) { unsafe IO { HttpLayer.sleep(wait) } };
                    Ref.put(Clock.now(), last);
                    k(Https.request(req))
            }
        }

    ///
    /// Middleware that intercepts the `Https` effect, enforcing a token-bucket
    /// rate limit. Allows an initial burst of `burstSize` requests, then
    /// sustains at 1 request per `interval`.
    ///
    pub def withTokenBucket(bs: {burstSize = Int32}, ims: {interval = Duration}, f: Unit -> a \ ef): a \ (ef - Https) + {Clock, Https} =
        let burstSize = bs#burstSize;
        let intervalMs = toMillis(ims#interval);
        region rc {
            let tokens   = Ref.fresh(rc, burstSize);
            let lastTime = Ref.fresh(rc, Clock.now());
            run { f() } with handler Https {
                def request(req, k) =
                    def acquire() = {
                        let now     = Clock.now();
                        let elapsed = now - Ref.get(lastTime);
                        let added   = Int64.clampToInt32(min = 0, max = burstSize, elapsed / intervalMs);
                        if (added > 0) {
                            Ref.put(Int32.min(burstSize, Ref.get(tokens) + added), tokens);
                            Ref.put(Ref.get(lastTime) + Int32.toInt64(added) * intervalMs, lastTime)
                        };
                        if (Ref.get(tokens) >= 1) {
                            Ref.put(Ref.get(tokens) - 1, tokens)
                        } else {
                            let wait = intervalMs - (Clock.now() - Ref.get(lastTime));
                            if (wait > 0i64) { unsafe IO { HttpLayer.sleep(wait) } };
                            acquire()
                        }
                    };
                    acquire();
                    k(Https.request(req))
            }
        }

    ///
    /// Middleware that intercepts the `Https` effect, enforcing a sliding-window
    /// rate limit. Allows at most `maxRequests` within any rolling `window`.
    /// When the limit is reached, sleeps until the oldest request expires.
    ///
    pub def withSlidingWindow(mr: {maxRequests = Int32}, wms: {window = Duration}, f: Unit -> a \ ef): a \ (ef - Https) + {Clock, Https} =
        let maxRequests = mr#maxRequests;
        let windowMs = toMillis(wms#window);
        region rc {
            let timestamps = MutDeque.empty(rc);
            run { f() } with handler Https {
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
                                    if (wait > 0i64) { unsafe IO { HttpLayer.sleep(wait) } };
                                    acquire()
                                case None =>
                                    // maxRequests <= 0: sleep for the full window and retry.
                                    unsafe IO { HttpLayer.sleep(windowMs) };
                                    acquire()
                            }
                        }
                    };
                    acquire();
                    k(Https.request(req))
            }
        }

    ///
    /// Middleware that intercepts the `Https` effect, implementing a circuit breaker.
    ///
    /// Tracks consecutive failures (transport errors and 5xx server errors).
    /// After `failureThreshold` consecutive failures, the circuit opens and
    /// immediately rejects requests for `cooldown`.
    /// After the cooldown, one probe request is allowed through (half-open state).
    /// A successful probe closes the circuit; a failed probe restarts the cooldown.
    ///
    /// Non-failure responses (2xx, 3xx, 4xx) reset the failure counter.
    ///
    pub def withCircuitBreaker(ft: {failureThreshold = Int32}, cdm: {cooldown = Duration},
                               f: Unit -> a \ ef): a \ (ef - Https) + {Clock, Https} =
        let failureThreshold = ft#failureThreshold;
        let cooldownMs = toMillis(cdm#cooldown);
        region rc {
            let failures  = Ref.fresh(rc, 0);
            let openSince = Ref.fresh(rc, 0i64);
            run { f() } with handler Https {
                def request(req, k) =
                    let now = Clock.now();
                    let since = Ref.get(openSince);
                    if (since > 0i64 and now - since < cooldownMs)
                        k(Err(IoError(ErrorKind.Other, "Circuit breaker is open")))
                    else {
                        let result = Https.request(req);
                        match result {
                            case Err(_) =>
                                let count = Ref.get(failures) + 1;
                                Ref.put(count, failures);
                                if (count >= failureThreshold) {
                                    Ref.put(Clock.now(), openSince)
                                };
                                k(result)
                            case Ok(resp) =>
                                if (HttpResponse.isServerError(resp)) {
                                    let count = Ref.get(failures) + 1;
                                    Ref.put(count, failures);
                                    if (count >= failureThreshold) {
                                        Ref.put(Clock.now(), openSince)
                                    }
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
    /// Middleware that intercepts the `Https` effect, adding retry logic.
    ///
    /// The `strategy` function decides whether to retry and how long to wait.
    /// Validates the `https://` scheme once before entering the retry loop.
    /// Each `Https.request` is re-raised to an outer `Https` handler that performs the actual I/O.
    ///
    pub def withRetry(strategy: (Int32, HttpRequest, RetryOutcome) -> RetryDecision \ ef1,
                      f: Unit -> a \ ef2): a \ ef1 + (ef2 - Https) + Https =
        run { f() } with handler Https {
            def request(req, k) =
                match validateHttps(req) {
                    case Err(e) => k(Err(e))
                    case Ok()   =>
                        def loop(attempt) =
                            match Https.request(req) {
                                case Ok(resp) if HttpResponse.isSuccess(resp) => Ok(resp)
                                case Ok(resp) =>
                                    match strategy(attempt, req, RetryOutcome.HttpError(resp)) {
                                        case RetryDecision.Retry(d) => unsafe IO { HttpLayer.sleep(toMillis(d)); loop(attempt + 1) } // sleep is pure here
                                        case RetryDecision.Stop     => Ok(resp)
                                    }
                                case Err(error) =>
                                    match strategy(attempt, req, RetryOutcome.TransportError(error)) {
                                        case RetryDecision.Retry(d) => unsafe IO { HttpLayer.sleep(toMillis(d)); loop(attempt + 1) } // sleep is pure here
                                        case RetryDecision.Stop     => Err(error)
                                    }
                            };
                        k(loop(0))
                }
        }

    ///
    /// Middleware that intercepts the `Https` effect, injecting default headers
    /// into every request. Headers already present on the request are not overridden.
    ///
    pub def withDefaultHeaders(defaults: Map[String, List[String]],
                               f: Unit -> a \ ef): a \ (ef - Https) + Https =
        run { f() } with handler Https {
            def request(req, k) =
                let HttpRequest(x) = req;
                let merged = Map.unionWith((reqVals, _) -> reqVals, x#headers, defaults);
                k(Https.request(HttpRequest({headers = merged | x})))
        }

    ///
    /// Middleware that intercepts the `Https` effect, prepending `base` to
    /// request URLs that do not already start with a scheme. Absolute URLs
    /// (where "://" appears before the first "/") are left unchanged.
    ///
    pub def withBaseUrl(base: String, f: Unit -> a \ ef): a \ (ef - Https) + Https =
        let base1 = if (String.endsWith(suffix = "/", base))
                         String.dropRight(1, base) else base;
        run { f() } with handler Https {
            def request(req, k) =
                let HttpRequest(x) = req;
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
                    k(Https.request(req))
                else
                    let path1 = if (String.startsWith(prefix = "/", url))
                                     url else "/${url}";
                    k(Https.request(HttpRequest({url = "${base1}${path1}" | x})))
        }

    ///
    /// Middleware that intercepts the `Https` effect, logging each request
    /// and its corresponding response via the `Logger` effect at `Debug` level.
    ///
    pub def withLogging(f: Unit -> a \ ef): a \ (ef - Https) + {Https, Logger} =
        run { f() } with handler Https {
            def request(req, k) =
                let method = String.toUpperCase("${HttpRequest.method(req)}");
                let url    = HttpRequest.url(req);
                Logger.log(Severity.Debug,
                    gray("-->") + text(" ") + bold(method) + text(" ") + cyan(url));
                let result = Https.request(req);
                match result {
                    case Ok(resp) =>
                        let status = HttpResponse.status(resp);
                        let color  = if (status >= 200 and status <= 299)
                                         green("${status}")
                                     else if (status >= 400)
                                         red("${status}")
                                     else
                                         yellow("${status}");
                        let size = Vector.length(HttpResponse.bodyBytes(resp));
                        Logger.log(Severity.Debug,
                            gray("<--") + text(" ") + color + text(" ") + cyan(url) +
                            gray(" (${size} bytes)"))
                    case Err(err) =>
                        Logger.log(Severity.Debug,
                            gray("<--") + text(" ") + red("ERR") + text(" ") + cyan(url) +
                            text(" ") + red("${err}"))
                };
                k(result)
        }

}

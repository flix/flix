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

    use Net.Http
    use Net.HttpClient
    use Net.RetryOutcome
    use Net.RetryDecision
    use Net.Request
    use Net.Request.Request
    use Net.Response
    use Net.Response.Response
    use IoError.IoError

    import java.lang.{System => JSystem}
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
    /// Sends a DELETE request to the given `url`.
    ///
    pub def delete(url: String): Result[IoError, Response] \ Http =
        send(Request.delete(url))

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
    pub def withMinInterval(intervalMs: Int64, f: Unit -> a \ ef): a \ (ef - Http) + Http =
        region rc {
            let last = Ref.fresh(rc, 0i64);
            run { f() } with handler Http {
                def request(req, k) =
                    let now = JSystem.currentTimeMillis();
                    let wait = intervalMs - (now - Ref.get(last));
                    if (wait > 0i64) unsafe IO { JThread.sleep(wait) } else ();
                    Ref.put(JSystem.currentTimeMillis(), last);
                    k(Http.request(req))
            }
        }

    ///
    /// Middleware that intercepts the `Http` effect, enforcing a token-bucket
    /// rate limit. Allows an initial burst of `burstSize` requests, then
    /// sustains at 1 request per `intervalMs` milliseconds.
    ///
    pub def withTokenBucket(burstSize: Int32, intervalMs: Int64, f: Unit -> a \ ef): a \ (ef - Http) + Http =
        region rc {
            let tokens   = Ref.fresh(rc, burstSize);
            let lastTime = Ref.fresh(rc, JSystem.currentTimeMillis());
            run { f() } with handler Http {
                def request(req, k) =
                    def acquire() = {
                        let now     = JSystem.currentTimeMillis();
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
                            let wait = intervalMs - (JSystem.currentTimeMillis() - Ref.get(lastTime));
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
    pub def withSlidingWindow(maxRequests: Int32, windowMs: Int64, f: Unit -> a \ ef): a \ (ef - Http) + Http =
        region rc {
            let timestamps = MutDeque.empty(rc);
            run { f() } with handler Http {
                def request(req, k) =
                    def acquire() = {
                        let now = JSystem.currentTimeMillis();
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
                                case None => ()
                            }
                        }
                    };
                    acquire();
                    k(Http.request(req))
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

}

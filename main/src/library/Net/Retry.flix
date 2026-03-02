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

pub mod Net.Retry {

    use Net.Retry
    use Net.RetryOutcome
    use Net.RetryDecision
    use Net.Request
    use Net.Response

    ///
    /// Handles `Retry` by never retrying.
    ///
    pub def noRetry(f: Unit -> a \ ef): a \ (ef - Retry) =
        run { f() } with handler Retry {
            def retryDecision(_, _, _, k) = k(RetryDecision.Stop)
        }

    ///
    /// Handles `Retry` by retrying up to `maxRetries` times with a fixed `delayMs` delay.
    ///
    pub def linear(maxRetries: Int32, delayMs: Int64, f: Unit -> a \ ef): a \ (ef - Retry) =
        run { f() } with handler Retry {
            def retryDecision(attempt, _, _, k) =
                if (attempt < maxRetries) k(RetryDecision.Retry(delayMs))
                else k(RetryDecision.Stop)
        }

    ///
    /// Handles `Retry` with exponential backoff.
    ///
    /// Retries up to `maxRetries` times with delays: `baseDelayMs`, `2*base`, `4*base`, …
    ///
    pub def exponential(maxRetries: Int32, baseDelayMs: Int64, f: Unit -> a \ ef): a \ (ef - Retry) =
        run { f() } with handler Retry {
            def retryDecision(attempt, _, _, k) =
                if (attempt < maxRetries)
                    k(RetryDecision.Retry(baseDelayMs * Int64.leftShift(1i64, attempt)))
                else k(RetryDecision.Stop)
        }

    ///
    /// Handles `Retry` by retrying up to `maxRetries` times, honouring an integer-seconds
    /// `Retry-After` response header on 429/503 responses.
    ///
    /// Falls back to `baseDelayMs` when the header is absent or unparseable.
    ///
    pub def retryAfter(maxRetries: Int32, baseDelayMs: Int64, f: Unit -> a \ ef): a \ (ef - Retry) =
        run { f() } with handler Retry {
            def retryDecision(attempt, _, outcome, k) =
                if (attempt >= maxRetries) k(RetryDecision.Stop)
                else
                    let delay = match outcome {
                        case RetryOutcome.HttpError(resp) =>
                            match Response.headerValue("retry-after", resp) {
                                case Some(s) => match Int64.fromString(s) {
                                    case Some(secs) => secs * 1000i64
                                    case None       => baseDelayMs
                                }
                                case None => baseDelayMs
                            }
                        case RetryOutcome.TransportError(_) => baseDelayMs
                    };
                    k(RetryDecision.Retry(delay))
        }

    ///
    /// Handles `Retry` using a custom `strategy` function.
    ///
    /// The strategy receives the attempt number, the request, and the outcome,
    /// and returns a `RetryDecision`.
    ///
    pub def withStrategy(strategy: (Int32, Request, RetryOutcome) -> RetryDecision \ ef1,
                         f: Unit -> a \ ef2): a \ ef1 + (ef2 - Retry) =
        run { f() } with handler Retry {
            def retryDecision(attempt, req, outcome, k) = k(strategy(attempt, req, outcome))
        }

    ///
    /// Wraps `inner` by adding uniform random jitter in `[0, maxJitterMs]` to any `Retry` delay.
    ///
    /// The `inner` argument is any handler wrapper (e.g. `Retry.linear(3, 500i64)`).
    ///
    pub def withJitter(maxJitterMs: Int64,
                       inner: (Unit -> RetryDecision \ Retry) -> RetryDecision \ ef1,
                       f: Unit -> a \ ef): a \ ef1 + (ef - Retry) + Random =
        run { f() } with handler Retry {
            def retryDecision(attempt, req, outcome, k) =
                let base = inner(() -> Retry.retryDecision(attempt, req, outcome));
                let jittered = match base {
                    case RetryDecision.Stop     => RetryDecision.Stop
                    case RetryDecision.Retry(d) =>
                        let j = Int64.abs(Random.randomInt64()) `Int64.remainder` (maxJitterMs + 1i64);
                        RetryDecision.Retry(d + j)
                };
                k(jittered)
        }

    ///
    /// Wraps `inner` and clamps any `Retry` delay to the time remaining before
    /// `startMs + deadlineMs`.
    ///
    /// Returns `Stop` if the deadline is already past.
    ///
    pub def withDeadline(deadlineMs: Int64, startMs: Int64,
                         inner: (Unit -> RetryDecision \ Retry) -> RetryDecision \ ef1,
                         f: Unit -> a \ ef): a \ ef1 + (ef - Retry) + Clock =
        run { f() } with handler Retry {
            def retryDecision(attempt, req, outcome, k) =
                let base = inner(() -> Retry.retryDecision(attempt, req, outcome));
                let clamped = match base {
                    case RetryDecision.Stop     => RetryDecision.Stop
                    case RetryDecision.Retry(d) =>
                        let elapsed = Clock.now() - startMs;
                        if (elapsed >= deadlineMs) RetryDecision.Stop
                        else RetryDecision.Retry(Int64.min(d, deadlineMs - elapsed))
                };
                k(clamped)
        }

}

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

    use Net.Request
    use Net.Response
    use Net.RetryDecision
    use Net.RetryOutcome

    ///
    /// Returns a strategy that never retries.
    ///
    pub def noRetry(): (Int32, Request, RetryOutcome) -> RetryDecision =
        (_, _, _) -> RetryDecision.Stop

    ///
    /// Returns a strategy that retries up to `maxRetries` times with a fixed `delayMs` delay.
    ///
    pub def linear(mr: {maxRetries = Int32}, dm: {delayMs = Int32}): (Int32, Request, RetryOutcome) -> RetryDecision =
        let maxRetries = mr#maxRetries;
        let delayMs = Int32.toInt64(dm#delayMs);
        (attempt, _, _) ->
            if (attempt < maxRetries) RetryDecision.Retry(delayMs)
            else RetryDecision.Stop

    ///
    /// Returns a strategy with exponential backoff.
    ///
    /// Retries up to `maxRetries` times with delays: `baseDelayMs`, `2*base`, `4*base`, …
    ///
    pub def exponential(mr: {maxRetries = Int32}, bdm: {baseDelayMs = Int32}): (Int32, Request, RetryOutcome) -> RetryDecision =
        let maxRetries = mr#maxRetries;
        let baseDelayMs = Int32.toInt64(bdm#baseDelayMs);
        (attempt, _, _) ->
            if (attempt < maxRetries)
                // Clamp to 62 to prevent shift overflow/wrap-around.
                RetryDecision.Retry(baseDelayMs * Int64.leftShift(1i64, Int32.min(attempt, 62)))
            else RetryDecision.Stop

    ///
    /// Returns a strategy that retries up to `maxRetries` times, honouring an integer-seconds
    /// `Retry-After` response header on 429/503 responses.
    ///
    /// Falls back to `baseDelayMs` when the header is absent or unparseable.
    ///
    pub def retryAfter(mr: {maxRetries = Int32}, bdm: {baseDelayMs = Int32}): (Int32, Request, RetryOutcome) -> RetryDecision =
        let maxRetries = mr#maxRetries;
        let baseDelayMs = Int32.toInt64(bdm#baseDelayMs);
        (attempt, _, outcome) ->
            if (attempt >= maxRetries) RetryDecision.Stop
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
                RetryDecision.Retry(delay)

    ///
    /// Returns a strategy that retries up to `maxRetries` times with a fixed `delayMs` delay,
    /// but only on transport errors (connection refused, timeout, etc.).
    ///
    /// HTTP-level errors (non-2xx responses) are never retried.
    ///
    pub def retryTransportOnly(mr: {maxRetries = Int32}, dm: {delayMs = Int32}): (Int32, Request, RetryOutcome) -> RetryDecision =
        let maxRetries = mr#maxRetries;
        let delayMs = Int32.toInt64(dm#delayMs);
        (attempt, _, outcome) -> match outcome {
            case RetryOutcome.TransportError(_) if attempt < maxRetries => RetryDecision.Retry(delayMs)
            case _                                                       => RetryDecision.Stop
        }

    ///
    /// Wraps `inner` by adding uniform random jitter in `[0, maxJitterMs]` to any `Retry` delay.
    ///
    pub def withJitter(mjm: {maxJitterMs = Int32},
                       inner: (Int32, Request, RetryOutcome) -> RetryDecision \ ef):
                       (Int32, Request, RetryOutcome) -> RetryDecision \ ef + Random =
        let maxJitterMs = Int32.toInt64(mjm#maxJitterMs);
        (attempt, req, outcome) ->
            let base = inner(attempt, req, outcome);
            match base {
                case RetryDecision.Stop     => RetryDecision.Stop
                case RetryDecision.Retry(d) =>
                    // Mask the sign bit instead of abs to avoid Int64.minValue() staying negative.
                    let j = Int64.bitwiseAnd(Random.randomInt64(), Int64.maxValue()) `Int64.remainder` (maxJitterMs + 1i64);
                    RetryDecision.Retry(d + j)
            }

    ///
    /// Wraps `inner` and clamps any `Retry` delay to the time remaining before
    /// `startMs + deadlineMs`.
    ///
    /// Returns `Stop` if the deadline is already past.
    ///
    pub def withDeadline(dlm: {deadlineMs = Int32}, sm: {startMs = Int32},
                         inner: (Int32, Request, RetryOutcome) -> RetryDecision \ ef):
                         (Int32, Request, RetryOutcome) -> RetryDecision \ ef + Clock =
        let deadlineMs = Int32.toInt64(dlm#deadlineMs);
        let startMs = Int32.toInt64(sm#startMs);
        (attempt, req, outcome) ->
            let base = inner(attempt, req, outcome);
            match base {
                case RetryDecision.Stop     => RetryDecision.Stop
                case RetryDecision.Retry(d) =>
                    let elapsed = Clock.now() - startMs;
                    if (elapsed >= deadlineMs) RetryDecision.Stop
                    else RetryDecision.Retry(Int64.min(d, deadlineMs - elapsed))
            }

}

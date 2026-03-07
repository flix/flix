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

    use Net.HttpRequest
    use Net.HttpResponse
    use Net.RetryDecision
    use Net.RetryOutcome
    use Time.Duration
    use Time.Duration.{nanoseconds, milliseconds, seconds, toMillis, toNanos}

    ///
    /// Returns a strategy that never retries.
    ///
    pub def noRetry(): (Int32, HttpRequest, RetryOutcome) -> RetryDecision =
        (_, _, _) -> RetryDecision.Stop

    ///
    /// Returns a strategy that retries up to `maxRetries` times with a fixed `delay`.
    ///
    pub def linear(mr: {maxRetries = Int32}, dm: {delay = Duration}): (Int32, HttpRequest, RetryOutcome) -> RetryDecision =
        let maxRetries = mr#maxRetries;
        let delay = dm#delay;
        (attempt, _, _) ->
            if (attempt < maxRetries) RetryDecision.Retry(delay)
            else RetryDecision.Stop

    ///
    /// Returns a strategy with exponential backoff.
    ///
    /// Retries up to `maxRetries` times with delays: `baseDelay`, `2*base`, `4*base`, …
    ///
    pub def exponential(mr: {maxRetries = Int32}, bdm: {baseDelay = Duration}): (Int32, HttpRequest, RetryOutcome) -> RetryDecision =
        let maxRetries = mr#maxRetries;
        let baseNanos = toNanos(bdm#baseDelay);
        (attempt, _, _) ->
            if (attempt < maxRetries)
                // Clamp shift to 62 to prevent shift overflow, then cap the product at Int64.maxValue to prevent multiplication wrap-around.
                let delay = baseNanos * Int64.leftShift(1i64, Int32.min(attempt, 62));
                let clamped = if (delay <= 0i64) Int64.maxValue() else delay;
                RetryDecision.Retry(nanoseconds(clamped))
            else RetryDecision.Stop

    ///
    /// Returns a strategy that retries up to `maxRetries` times, honouring an integer-seconds
    /// `Retry-After` response header on 429/503 responses.
    ///
    /// Falls back to `baseDelay` when the header is absent or unparseable.
    ///
    pub def retryAfter(mr: {maxRetries = Int32}, bdm: {baseDelay = Duration}): (Int32, HttpRequest, RetryOutcome) -> RetryDecision =
        let maxRetries = mr#maxRetries;
        let baseDelay = bdm#baseDelay;
        (attempt, _, outcome) ->
            if (attempt >= maxRetries) RetryDecision.Stop
            else
                let delay = match outcome {
                    case RetryOutcome.HttpError(resp) =>
                        match HttpResponse.headerValue("retry-after", resp) {
                            case Some(s) => match Int64.fromString(s) {
                                // Clamp to Int32 range to guard against absurd Retry-After values from untrusted servers.
                            case Some(secs) => seconds(Int64.clampToInt32(min = 0, max = Int32.maxValue(), secs))
                                case None       => baseDelay
                            }
                            case None => baseDelay
                        }
                    case RetryOutcome.TransportError(_) => baseDelay
                };
                RetryDecision.Retry(delay)

    ///
    /// Returns a strategy that retries up to `maxRetries` times with a fixed `delay`,
    /// but only on transport errors (connection refused, timeout, etc.).
    ///
    /// HTTP-level errors (non-2xx responses) are never retried.
    ///
    pub def retryTransportOnly(mr: {maxRetries = Int32}, dm: {delay = Duration}): (Int32, HttpRequest, RetryOutcome) -> RetryDecision =
        let maxRetries = mr#maxRetries;
        let delay = dm#delay;
        (attempt, _, outcome) -> match outcome {
            case RetryOutcome.TransportError(_) if attempt < maxRetries => RetryDecision.Retry(delay)
            case _                                                       => RetryDecision.Stop
        }

    ///
    /// Wraps `inner` by adding uniform random jitter in `[0, maxJitter]` to any `Retry` delay.
    ///
    pub def withJitter(mjm: {maxJitter = Duration},
                       inner: (Int32, HttpRequest, RetryOutcome) -> RetryDecision \ ef):
                       (Int32, HttpRequest, RetryOutcome) -> RetryDecision \ ef + Random =
        let maxJitterNanos = toNanos(mjm#maxJitter);
        (attempt, req, outcome) ->
            let base = inner(attempt, req, outcome);
            match base {
                case RetryDecision.Stop     => RetryDecision.Stop
                case RetryDecision.Retry(d) =>
                    // Mask the sign bit instead of abs to avoid Int64.minValue() staying negative.
                    let j = Int64.bitwiseAnd(Random.randomInt64(), Int64.maxValue()) `Int64.remainder` (maxJitterNanos + 1i64);
                    RetryDecision.Retry(d + nanoseconds(j))
            }

    ///
    /// Wraps `inner` and clamps any `Retry` delay to the time remaining before the `deadline`.
    ///
    /// `startMs` should be the result of `Clock.now()` captured before entering the retry loop.
    /// Returns `Stop` if the deadline is already past.
    ///
    pub def withDeadline(dlm: {deadline = Duration}, sm: {startMs = Int64},
                         inner: (Int32, HttpRequest, RetryOutcome) -> RetryDecision \ ef):
                         (Int32, HttpRequest, RetryOutcome) -> RetryDecision \ ef + Clock =
        let deadlineMs = toMillis(dlm#deadline);
        let startMs = sm#startMs;
        (attempt, req, outcome) ->
            let base = inner(attempt, req, outcome);
            match base {
                case RetryDecision.Stop     => RetryDecision.Stop
                case RetryDecision.Retry(d) =>
                    let elapsed = Clock.now() - startMs;
                    if (elapsed >= deadlineMs) RetryDecision.Stop
                    else
                        let ms = Int64.min(toMillis(d), deadlineMs - elapsed);
                        RetryDecision.Retry(milliseconds(Int64.clampToInt32(min = 0, max = Int32.maxValue(), ms)))
            }

}

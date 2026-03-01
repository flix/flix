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
    use Net.Request

    import java.lang.{Thread => JThread}

    ///
    /// Handles the `Retry` effect by never retrying.
    ///
    pub def noRetry(f: Unit -> a \ ef): a \ (ef - Retry) =
        run {
            f()
        } with handler Retry {
            def shouldRetry(_attempt, _request, _error, k) =
                k(false)
        }

    ///
    /// Handles the `Retry` effect with exponential backoff.
    ///
    /// Retries up to `maxRetries` times with a delay of `baseDelayMs * 2^attempt` milliseconds.
    ///
    pub def exponential(maxRetries: Int32, baseDelayMs: Int64, f: Unit -> a \ ef): a \ (ef - Retry) + IO =
        run {
            f()
        } with handler Retry {
            def shouldRetry(attempt, _request, _error, k) =
                if (attempt < maxRetries) {
                    JThread.sleep(baseDelayMs * Int64.leftShift(1i64, attempt));
                    k(true)
                } else {
                    k(false)
                }
        }

    ///
    /// Handles the `Retry` effect using a custom `strategy` function.
    ///
    /// The strategy receives the attempt number, the request, and the error,
    /// and returns `true` if the request should be retried.
    ///
    pub def withStrategy(
        strategy: (Int32, Request, IoError) -> Bool \ ef1,
        f: Unit -> a \ ef2
    ): a \ ef1 + (ef2 - Retry) =
        run {
            f()
        } with handler Retry {
            def shouldRetry(attempt, request, error, k) =
                k(strategy(attempt, request, error))
        }

}

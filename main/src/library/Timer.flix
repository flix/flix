/*
 * Copyright 2026 Magnus Madsen
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

///
/// An effect used to sleep for at least a given number of milliseconds.
///
/// The public contract is intentionally millisecond-based in v0.
/// Sub-millisecond units are rounded down by the helper conversion functions below.
///
eff Timer {

    ///
    /// Sleeps for at least `ms` milliseconds.
    ///
    def sleep(ms: Int64): Unit

}

pub mod Timer {

    ///
    /// Sleeps for the given `duration` measured in the given `unit`.
    ///
    pub def sleepFor(duration: Int64, unit: TimeUnit): Unit \ Timer =
        Timer.sleep(toMillis(duration, unit))

    ///
    /// Sleeps for at least `ms` milliseconds.
    ///
    pub def sleepMillis(ms: Int64): Unit \ Timer =
        Timer.sleep(ms)

    ///
    /// Handles the `Timer` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `Timer` effect using the `IO` effect.
    ///
    pub def handle(f: a -> b \ ef): a -> b \ (ef - Timer) + IO = x ->
        run {
            f(x)
        } with handler Timer {
            def sleep(ms, k) = {
                %%SLEEP_MILLIS%%(clampMillis(ms));
                k()
            }
        }

    ///
    /// Runs the `Timer` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `Timer` effect using the `IO` effect.
    ///
    @DefaultHandler
    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - Timer) + IO = handle(f)()

    def toMillis(duration: Int64, unit: TimeUnit): Int64 = match unit {
        case TimeUnit.Days         => saturatingMul(duration, 86_400_000i64)
        case TimeUnit.Hours        => saturatingMul(duration, 3_600_000i64)
        case TimeUnit.Microseconds => duration / 1_000i64
        case TimeUnit.Milliseconds => duration
        case TimeUnit.Minutes      => saturatingMul(duration, 60_000i64)
        case TimeUnit.Nanoseconds  => duration / 1_000_000i64
        case TimeUnit.Seconds      => saturatingMul(duration, 1_000i64)
    }

    def clampMillis(ms: Int64): Int64 =
        if (ms <= 0i64) 0i64 else ms

    def saturatingMul(x: Int64, m: Int64): Int64 =
        if (m == 0i64) {
            0i64
        } else if (x > 0i64 and x > Int64.maxValue() / m) {
            Int64.maxValue()
        } else if (x < 0i64 and x < Int64.minValue() / m) {
            Int64.minValue()
        } else {
            x * m
        }

}

/*
 * Copyright 2026 Flix Contributors
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
/// Represents a duration of time stored as nanoseconds (Int64).
///
pub enum Duration(Int64) with Eq, Order, Hash

instance Add[Duration] {
    pub def add(x: Duration, y: Duration): Duration =
        let Duration.Duration(x1) = x;
        let Duration.Duration(y1) = y;
        Duration.Duration(x1 + y1)
}

instance Sub[Duration] {
    pub def sub(x: Duration, y: Duration): Duration =
        let Duration.Duration(x1) = x;
        let Duration.Duration(y1) = y;
        Duration.Duration(x1 - y1)
}

instance ToString[Duration] {
    pub def toString(d: Duration): String =
        let Duration.Duration(nanos) = d;
        if (nanos == 0i64)
            "0ns"
        else
            let abs = Int64.abs(nanos);
            let sign = if (nanos < 0i64) "-" else "";
            if (abs >= 3_600_000_000_000i64)
                let h = abs / 3_600_000_000_000i64;
                let rem1 = abs - (h * 3_600_000_000_000i64);
                let m = rem1 / 60_000_000_000i64;
                let rem2 = rem1 - (m * 60_000_000_000i64);
                let s = rem2 / 1_000_000_000i64;
                "${sign}${h}h ${m}m ${s}s"
            else if (abs >= 60_000_000_000i64)
                let m = abs / 60_000_000_000i64;
                let rem1 = abs - (m * 60_000_000_000i64);
                let s = rem1 / 1_000_000_000i64;
                "${sign}${m}m ${s}s"
            else if (abs >= 1_000_000_000i64)
                let s = abs / 1_000_000_000i64;
                let rem1 = abs - (s * 1_000_000_000i64);
                let ms = rem1 / 1_000_000i64;
                "${sign}${s}s ${ms}ms"
            else if (abs >= 1_000_000i64)
                let ms = abs / 1_000_000i64;
                "${sign}${ms}ms"
            else if (abs >= 1_000i64)
                let us = abs / 1_000i64;
                "${sign}${us}us"
            else
                "${sign}${abs}ns"
}

instance Formattable[Duration] {
    pub def format(d: Duration): RichString = RichString.fromString(ToString.toString(d))
}

pub mod Duration {

    /// Returns the zero duration.
    pub def zero(): Duration = Duration.Duration(0i64)

    /// Returns a duration of `n` in the given time unit `u`.
    pub def of(n: Int64, u: TimeUnit): Duration = match u {
        case TimeUnit.Days         => days(n),
        case TimeUnit.Hours        => hours(n),
        case TimeUnit.Microseconds => microseconds(n),
        case TimeUnit.Milliseconds => milliseconds(n),
        case TimeUnit.Minutes      => minutes(n),
        case TimeUnit.Nanoseconds  => nanoseconds(n),
        case TimeUnit.Seconds      => seconds(n)
    }

    /// Returns a duration of `n` nanoseconds.
    pub def nanoseconds(n: Int64): Duration = Duration.Duration(n)

    /// Returns a duration of `n` microseconds.
    pub def microseconds(n: Int64): Duration = Duration.Duration(n * 1_000i64)

    /// Returns a duration of `n` milliseconds.
    pub def milliseconds(n: Int64): Duration = Duration.Duration(n * 1_000_000i64)

    /// Returns a duration of `n` seconds.
    pub def seconds(n: Int64): Duration = Duration.Duration(n * 1_000_000_000i64)

    /// Returns a duration of `n` minutes.
    pub def minutes(n: Int64): Duration = Duration.Duration(n * 60_000_000_000i64)

    /// Returns a duration of `n` hours.
    pub def hours(n: Int64): Duration = Duration.Duration(n * 3_600_000_000_000i64)

    /// Returns a duration of `n` days.
    pub def days(n: Int64): Duration = Duration.Duration(n * 86_400_000_000_000i64)

    /// Returns the total number of nanoseconds in `d`.
    pub def toNanos(d: Duration): Int64 =
        let Duration.Duration(n) = d;
        n

    /// Returns the total number of microseconds in `d` (truncated).
    pub def toMicros(d: Duration): Int64 =
        let Duration.Duration(n) = d;
        n / 1_000i64

    /// Returns the total number of milliseconds in `d` (truncated).
    pub def toMillis(d: Duration): Int64 =
        let Duration.Duration(n) = d;
        n / 1_000_000i64

    /// Returns the total number of seconds in `d` (truncated).
    pub def toSeconds(d: Duration): Int64 =
        let Duration.Duration(n) = d;
        n / 1_000_000_000i64

    /// Returns the total number of minutes in `d` (truncated).
    pub def toMinutes(d: Duration): Int64 =
        let Duration.Duration(n) = d;
        n / 60_000_000_000i64

    /// Returns the total number of hours in `d` (truncated).
    pub def toHours(d: Duration): Int64 =
        let Duration.Duration(n) = d;
        n / 3_600_000_000_000i64

    /// Returns the total number of days in `d` (truncated).
    pub def toDays(d: Duration): Int64 =
        let Duration.Duration(n) = d;
        n / 86_400_000_000_000i64

    /// Returns `true` if `d` is zero.
    pub def isZero(d: Duration): Bool =
        let Duration.Duration(n) = d;
        n == 0i64

    /// Returns `true` if `d` is positive.
    pub def isPositive(d: Duration): Bool =
        let Duration.Duration(n) = d;
        n > 0i64

    /// Returns `true` if `d` is negative.
    pub def isNegative(d: Duration): Bool =
        let Duration.Duration(n) = d;
        n < 0i64

}

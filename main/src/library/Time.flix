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

pub mod Time {

    ///
    /// Represents a duration of time stored as nanoseconds (Int64).
    ///
    pub enum Duration(Int64) with Eq, Order, Hash

    ///
    /// Represents a time unit.
    ///
    pub enum TimeUnit with Eq, ToString {
        case Days,
        case Hours,
        case Microseconds,
        case Milliseconds,
        case Minutes,
        case Nanoseconds,
        case Seconds
    }

}

instance Add[Time.Duration] {
    pub def add(x: Time.Duration, y: Time.Duration): Time.Duration =
        let Time.Duration.Duration(x1) = x;
        let Time.Duration.Duration(y1) = y;
        Time.Duration.Duration(x1 + y1)
}

instance Sub[Time.Duration] {
    pub def sub(x: Time.Duration, y: Time.Duration): Time.Duration =
        let Time.Duration.Duration(x1) = x;
        let Time.Duration.Duration(y1) = y;
        Time.Duration.Duration(x1 - y1)
}

instance ToString[Time.Duration] {
    pub def toString(d: Time.Duration): String =
        let Time.Duration.Duration(nanos) = d;
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

instance Formattable[Time.Duration] {
    pub def format(d: Time.Duration): RichString = RichString.fromString(ToString.toString(d))
}

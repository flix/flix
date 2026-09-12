/*
 * Copyright 2024 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

pub mod Time.TimeUnit {

    use Time.TimeUnit

    ///
    /// Represents a time unit.
    ///
    pub enum TimeUnit with Eq, ToString {
        case Days
        case Hours
        case Microseconds
        case Milliseconds
        case Minutes
        case Nanoseconds
        case Seconds
    }

    ///
    /// Returns the number of milliseconds for timeunit `u`
    ///
    pub def toMilliseconds(u: Time.TimeUnit): Int32 = match u {
        case TimeUnit.Days         => 24 * 60 * 60 * 1_000,
        case TimeUnit.Hours        => 60 * 60 * 1_000,
        case TimeUnit.Microseconds => 1 / 1_000,
        case TimeUnit.Milliseconds => 1,
        case TimeUnit.Minutes      => 60 * 1_000,
        case TimeUnit.Nanoseconds  => 1 / 1_000_000,
        case TimeUnit.Seconds      => 1_000
    }

}

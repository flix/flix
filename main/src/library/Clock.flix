/*
 *  Copyright 2024 Magnus Madsen
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

///
/// An effect used to access the current (real-world) time.
///
eff Clock {

    ///
    /// Returns a measure of time since the epoch in the given time unit `u`.
    ///
    def currentTime(u: TimeUnit): Int64

}

mod Clock {

    import java.lang.System
    import java.util.concurrent.{TimeUnit => JTimeUnit}

    ///
    /// Returns the number of milliseconds since the epoch.
    ///
    /// Equivalent to `currentTime(TimeUnit.Milliseconds)`.
    ///
    pub def now(): Int64 \ Clock = Clock.currentTime(TimeUnit.Milliseconds)

    ///
    /// Handles the `Clock` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `Clock` effect using the `IO` effect.
    ///
    pub def handle(f: a -> b \ ef): a -> b \ (ef - Clock) + IO = x ->
        run {
            f(x)
        } with handler Clock {
            def currentTime(u, k) = {
                let time = System.currentTimeMillis();
                let ms = JTimeUnit.MILLISECONDS;
                match u {
                    case TimeUnit.Days          => k(ms.toDays(time))
                    case TimeUnit.Hours         => k(ms.toHours(time))
                    case TimeUnit.Microseconds  => k(ms.toMicros(time))
                    case TimeUnit.Milliseconds  => k(ms.toMillis(time))
                    case TimeUnit.Minutes       => k(ms.toMinutes(time))
                    case TimeUnit.Nanoseconds   => k(ms.toNanos(time))
                    case TimeUnit.Seconds       => k(ms.toSeconds(time))
                }
            }
        }

    ///
    /// Runs the `Clock` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `Clock` effect using the `IO` effect.
    ///
    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - Clock) + IO = handle(f)()

}

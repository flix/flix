/*
 * Copyright 2019 Magnus Madsen
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

mod Time {

    ///
    /// A wrapper around a Java instant.
    ///
    enum Instant(##java.time.Instant)

    instance ToString[Instant] {
        pub def toString(x: Time.Instant): String = Instant.toString(x)
    }

    mod Instant {

        ///
        /// Returns the current time from the system clock.
        ///
        @Experimental
        pub def now(): Time.Instant \ IO =
            import static java.time.Instant.now(): ##java.time.Instant \ IO;
            Time.Instant.Instant(now())

        ///
        /// Returns `true` if `x` is before `y`.
        ///
        @Experimental
        pub def before(x: Time.Instant, y: Time.Instant): Bool =
            import java.time.Instant.isBefore(##java.time.Instant): Bool \ {};
            let Time.Instant.Instant(t1) = x;
            let Time.Instant.Instant(t2) = y;
            (t1 `isBefore` t2)

        ///
        /// Returns `true` if `x` is after `y`.
        ///
        @Experimental
        pub def after(x: Time.Instant, y: Time.Instant): Bool =
            import java.time.Instant.isAfter(##java.time.Instant): Bool \ {};
            let Time.Instant.Instant(t1) = x;
            let Time.Instant.Instant(t2) = y;
            (t1 `isAfter` t2)

        ///
        /// Returns the given instant `x` plus `d` number of nano seconds.
        ///
        @Experimental
        pub def plus(x: Time.Instant, d: Int64): Time.Instant =
            import java.time.Instant.plusNanos(Int64): ##java.time.Instant \ {};
            let Time.Instant.Instant(t) = x;
            Time.Instant.Instant(t `plusNanos` d)

        ///
        /// Returns the given instant `x` minus `d` number of nano seconds.
        ///
        @Experimental
        pub def minus(x: Time.Instant, d: Int64): Time.Instant =
            import java.time.Instant.minusNanos(Int64): ##java.time.Instant \ {};
            let Time.Instant.Instant(t) = x;
            Time.Instant.Instant(t `minusNanos` d)

        ///
        /// Returns `x` as a string using the ISO-8601 representation.
        ///
        @Experimental
        pub def toString(x: Time.Instant): String =
            import java.time.Instant.toString(): String \ {} as str;
            let Time.Instant.Instant(t) = x;
            str(t)

    }

}

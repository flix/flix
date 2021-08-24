/*
 * Copyright 2021 Jakob Schneider Villumsen
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

namespace Epoch {

    ///
    /// Returns the number of nano seconds since the Unix epoch.
    ///
    pub def nanoseconds(): Int64 & Impure =
        import java.time.Instant:now();
        import java.time.Instant.getEpochSecond();
        import java.time.Instant.getNano();
        let n = now();
        (n.getEpochSecond() * 1000000000i64) + Int32.toInt64(n.getNano())


    ///
    /// Returns the number of micro seconds since the Unix epoch.
    ///
    pub def microSeconds(): Int64 & Impure = ???

    ///
    /// Returns the number of milli seconds since the Unix epoch.
    ///
    pub def milliseconds(): Int64 & Impure =
        import java.time.Instant:now();
        import java.time.Instant.toEpochMilli();
        now().toEpochMilli()

    ///
    /// Returns the number of seconds since the Unix epoch.
    ///
    pub def seconds(): Int64 & Impure =
        import java.time.Instant:now();
        import java.time.Instant.getEpochSecond();
        now().getEpochSecond()
}

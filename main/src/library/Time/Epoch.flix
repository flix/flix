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

mod Time.Epoch {
    import java.lang.System

    ///
    /// Returns the number of nano seconds since the Unix epoch.
    ///
    @Experimental
    pub def nanoseconds(): Int64 \ IO =
        System.nanoTime()

    ///
    /// Returns the number of micro seconds since the Unix epoch.
    ///
    @Experimental
    pub def microseconds(): Int64 \ IO = nanoseconds() / 1000i64

    ///
    /// Returns the number of milli seconds since the Unix epoch.
    ///
    @Experimental
    pub def milliseconds(): Int64 \ IO = microseconds() / 1000i64

    ///
    /// Returns the number of seconds since the Unix epoch.
    ///
    @Experimental
    pub def seconds(): Int64 \ IO = milliseconds() / 1000i64

}

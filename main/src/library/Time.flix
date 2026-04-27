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
    /// An effect used to access the current (real-world) time.
    ///
    pub eff Clock {

        ///
        /// Returns a measure of time since the epoch in the given time unit `u`.
        ///
        def currentTime(u: TimeUnit): Int64

    }

    ///
    /// An effect used to sleep the current thread.
    ///
    pub eff Sleep {

        ///
        /// Sleeps the current thread for the given duration `d`.
        ///
        def sleep(d: Duration): Unit

    }

}

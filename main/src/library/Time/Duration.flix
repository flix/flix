/*
 * Copyright 2018 Magnus Madsen
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

namespace Time {

    namespace Duration {

        ///
        /// Returns the number of nanoseconds in one 24 hour day.
        ///
        @Experimental
        pub def oneDay(): Int64 = 24i64 * oneHour()

        ///
        /// Returns the number of nanoseconds in one hour.
        ///
        @Experimental
        pub def oneHour(): Int64 = 60i64 * oneMinute()

        ///
        /// Returns the number of nanoseconds in one minute.
        ///
        @Experimental
        pub def oneMinute(): Int64 = 60i64 * oneSecond()

        ///
        /// Returns the number of nanoseconds in one second.
        ///
        @Experimental
        pub def oneSecond(): Int64 = 1000i64 * oneMillisecond()

        ///
        /// Returns the number of nanoseconds in one millisecond.
        ///
        @Experimental
        pub def oneMillisecond(): Int64 = 1000i64 * oneMicrosecond()

        ///
        /// Returns the number of nanoseconds in one microsecond.
        ///
        @Experimental
        pub def oneMicrosecond(): Int64 = 1000i64 * oneNanosecond()

        ///
        /// Returns the number of nanoseconds in one nanosecond, i.e. one.
        ///
        @Experimental
        pub def oneNanosecond(): Int64 = 1i64

    }

}

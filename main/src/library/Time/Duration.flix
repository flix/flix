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

namespace Time/Duration {
    use Time.Duration;
    use Time.Duration.Duration;

    ///
    /// Returns a duration of `n` nano seconds.
    ///
    @Experimental
    pub def fromNanoSeconds(n: Int64): Duration = Duration(n)

    ///
    /// Returns a duration of `n` micro seconds.
    ///
    @Experimental
    pub def fromMicroSeconds(n: Int64): Duration = Duration(n * 1_00i64)

    ///
    /// Returns a duration of `n` milli seconds.
    ///
    @Experimental
    pub def fromMilliSeconds(n: Int64): Duration = Duration(n * 1_000_000i64)

    ///
    /// Returns a duration of `n` seconds.
    ///
    @Experimental
    pub def fromSeconds(n: Int64): Duration = Duration(n * 1_000_000_000i64)

}

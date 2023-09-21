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

mod Time.Duration {
    use Int32.toInt64
    use Time.Duration
    use Time.Duration.Duration

    ///
    /// Returns a duration of `n` nano seconds.
    ///
    @experimental
    pub def fromNanoSeconds(n: Int32): Duration = Duration(toInt64(n))

    ///
    /// Returns a duration of `n` micro seconds.
    ///
    @experimental
    pub def fromMicroSeconds(n: Int32): Duration = Duration(toInt64(n) * 1_000i64)

    ///
    /// Returns a duration of `n` milli seconds.
    ///
    @experimental
    pub def fromMilliSeconds(n: Int32): Duration = Duration(toInt64(n) * 1_000_000i64)

    ///
    /// Returns a duration of `n` seconds.
    ///
    @experimental
    pub def fromSeconds(n: Int32): Duration = Duration(toInt64(n) * 1_000_000_000i64)

}

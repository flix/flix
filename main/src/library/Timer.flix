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

namespace Timer {

    ///
    /// Returns a channel that receives the Unit value after `n` days.
    ///
    pub def days(n: Int64): Channel[Unit] & Impure =
        let c = chan Unit 100;
        spawn sleepAndSend(n * Duration.oneDay(), c);
        c

    ///
    /// Returns a channel that receives the Unit value after `n` hours.
    ///
    pub def hours(n: Int64): Channel[Unit] & Impure =
        let c = chan Unit 100;
        spawn sleepAndSend(n * Duration.oneHour(), c);
        c

    ///
    /// Returns a channel that receives the Unit value after `n` minutes.
    ///
    pub def minutes(n: Int64): Channel[Unit] & Impure =
        let c = chan Unit 100;
        spawn sleepAndSend(n * Duration.oneMinute(), c);
        c

    ///
    /// Returns a channel that receives the Unit value after `n` seconds.
    ///
    pub def seconds(n: Int64): Channel[Unit] & Impure =
        let c = chan Unit 100;
        spawn sleepAndSend(n * Duration.oneSecond(), c);
        c

    ///
    /// Returns a channel that receives the Unit value after `n` milliseconds.
    ///
    pub def milliseconds(n: Int64): Channel[Unit] & Impure =
        let c = chan Unit 100;
        spawn sleepAndSend(n * Duration.oneMillisecond(), c);
        c

    ///
    /// Returns a channel that receives the Unit value after `n` microseconds.
    ///
    pub def microseconds(n: Int64): Channel[Unit] & Impure =
        let c = chan Unit 100;
        spawn sleepAndSend(n * Duration.oneMicrosecond(), c);
        c

    ///
    /// Returns a channel that receives the Unit value after `n` nanoseconds.
    ///
    pub def nanoseconds(n: Int64): Channel[Unit] & Impure =
        let c = chan Unit 100;
        spawn sleepAndSend(n * Duration.oneNanosecond(), c);
        c

    ///
    /// Sleeps for `s` nanoseconds and then sends the Unit value to the given channel `c`.
    ///
    def sleepAndSend(ns: Int64, c: Channel[Unit]): Unit & Impure =
        import java.lang.Thread:sleep(Int64);
        sleep(ns / 1000000i64); c <- (); ()
}

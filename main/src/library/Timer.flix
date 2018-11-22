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

///
/// A collection of timers.
///
/// A timer is a channel into which the Unit value is put once after some duration.
///
namespace Timer {

    ///
    /// Returns a channel that receives the Unit value after `n` days.
    ///
    pub def days(n: Int): Channel[Unit] =
        let c = chan Unit;
        spawn {
            sleep(n * oneDay());
            c <- ()
        };
        c

    ///
    /// Returns a channel that receives the Unit value after `n` hours.
    ///
    pub def hours(n: Int): Channel[Unit] =
        let c = chan Unit;
        spawn {
            sleep(n * oneHour());
            c <- ()
        };
        c

    ///
    /// Returns a channel that receives the Unit value after `n` minutes.
    ///
    pub def minutes(n: Int): Channel[Unit] =
        let c = chan Unit;
        spawn {
            sleep(n * oneMinute());
            c <- ()
        };
        c

    ///
    /// Returns a channel that receives the Unit value after `n` seconds.
    ///
    pub def seconds(n: Int): Channel[Unit] =
        let c = chan Unit;
        spawn {
            sleep(n * oneSecond());
            c <- ()
        };
        c

    ///
    /// Returns a channel that receives the Unit value after `n` milliseconds.
    ///
    pub def milliseconds(n: Int): Channel[Unit] =
        let c = chan Unit;
        spawn {
            sleep(n * oneMillisecond());
            c <- ()
        };
        c

    ///
    /// Returns a channel that receives the Unit value after `n` microseconds.
    ///
    pub def microseconds(n: Int): Channel[Unit] =
        let c = chan Unit;
        spawn {
            sleep(n * oneMicrosecond());
            c <- ()
        };
        c

    ///
    /// Returns a channel that receives the Unit value after `n` nanoseconds.
    ///
    pub def nanoseconds(n: Int): Channel[Unit] =
        let c = chan Unit;
        spawn {
            sleep(n * oneNanosecond());
            c <- ()
        };
        c

    ///
    /// Returns the number of nanoseconds in one 24 hour day.
    ///
    def oneDay(): Int = 24 * oneHour()

    ///
    /// Returns the number of nanoseconds in one hour.
    ///
    def oneHour(): Int = 60 * oneMinute()

    ///
    /// Returns the number of nanoseconds in one minute.
    ///
    def oneMinute(): Int = 60 * oneSecond()

    ///
    /// Returns the number of nanoseconds in one second.
    ///
    def oneSecond(): Int = 1000 * oneMillisecond()

    ///
    /// Returns the number of nanoseconds in one millisecond.
    ///
    def oneMillisecond(): Int = 1000 * oneMicrosecond()

    ///
    /// Returns the number of nanoseconds in one microsecond.
    ///
    def oneMicrosecond(): Int = 1000 * oneNanosecond()

    ///
    /// Returns the number of nanoseconds in one nanosecond, i.e. one.
    ///
    def oneNanosecond(): Int = 1

    def sleep(l: Int): Unit = () // TODO: Replace by build in command.

    // TODO: Move these into a test file.

    @test
    def testDaysTimer01(): Bool = select {
        case x <- Timer.days(0) => true
    }

    @test
    def testHoursTimer01(): Bool = select {
        case x <- Timer.hours(0) => true
    }

    @test
    def testMinutesTimer01(): Bool = select {
        case x <- Timer.minutes(0) => true
    }

    @test
    def testSecondsTimer01(): Bool = select {
        case x <- Timer.seconds(0) => true
    }

    @test
    def testMillisecondsTimer01(): Bool = select {
        case x <- Timer.milliseconds(0) => true
    }

    @test
    def testMicrosecondsTimer01(): Bool = select {
        case x <- Timer.microseconds(0) => true
    }

    @test
    def testNanosecondsTimer01(): Bool = select {
        case x <- Timer.nanoseconds(0) => true
    }

}

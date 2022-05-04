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

namespace Concurrent/Channel/Ticker {

    ///
    /// Returns a channel that receives the Unit value repeatedly every `n` days.
    ///
    pub def days(n: Int64): Ticker & Impure =
        let ticker = newTicker();
        spawn sleepSendAndRepeat(n * Time/Duration.oneDay(), ticker, 0);
        ticker

    ///
    /// Returns a channel that receives the Unit value repeatedly every `n` hours.
    ///
    pub def hours(n: Int64): Ticker & Impure =
        let ticker = newTicker();
        spawn sleepSendAndRepeat(n * Time/Duration.oneHour(), ticker, 0);
        ticker

    ///
    /// Returns a channel that receives the Unit value repeatedly every `n` minutes.
    ///
    pub def minutes(n: Int64): Ticker & Impure =
        let ticker = newTicker();
        spawn sleepSendAndRepeat(n * Time/Duration.oneMinute(), ticker, 0);
        ticker

    ///
    /// Returns a channel that receives the Unit value repeatedly every `n` seconds.
    ///
    pub def seconds(n: Int64): Ticker & Impure =
        let ticker = newTicker();
        spawn sleepSendAndRepeat(n * Time/Duration.oneSecond(), ticker, 0);
        ticker

    ///
    /// Returns a channel that receives the Unit value repeatedly every `n` milliseconds.
    ///
    pub def milliseconds(n: Int64): Ticker & Impure =
        let ticker = newTicker();
        spawn sleepSendAndRepeat(n * Time/Duration.oneMillisecond(), ticker, 0);
        ticker

    ///
    /// Returns a channel that receives the Unit value repeatedly every `n` microseconds.
    ///
    pub def microseconds(n: Int64): Ticker & Impure =
        let ticker = newTicker();
        spawn sleepSendAndRepeat(n * Time/Duration.oneMicrosecond(), ticker, 0);
        ticker

    ///
    /// Returns a channel that receives the Unit value repeatedly every `n` nanoseconds.
    ///
    pub def nanoseconds(n: Int64): Ticker & Impure =
        let ticker = newTicker();
        spawn sleepSendAndRepeat(n * Time/Duration.oneNanosecond(), ticker, 0);
        ticker

    ///
    /// Sleeps for `s` nanoseconds, sends the Unit value to the given channel `c`, and then repeats.
    ///
    def sleepSendAndRepeat(ns: Int64, ticker: Ticker, count: Int32): Unit & Impure =
        import static java.lang.Thread.sleep(Int64): Unit & Impure;

        let (tickChannel, stopChannel) = match ticker {
          case Ticker(t, s) => (t, s)
        };

        if (ns < 0i64) sleep(0i64)
        else           sleep(ns / 1000000i64);

        select {
          case _stop <- stopChannel => ()
          case _ => {
            let _ = tickChannel <- count;
            sleepSendAndRepeat(ns, ticker, count + 1)
          }
        }

    pub enum Ticker {
      case Ticker(Channel[Int32], Channel[Unit])
    }

    def newTicker(): Ticker & Impure = Ticker(chan Int32 100, chan Unit 100)

    pub def stop(ticker: Ticker): Unit & Impure = {
      match ticker {
        case Ticker(_tickChannel, stopChannel) =>
        let _ = stopChannel <- ();
        ()
      }
    }

    pub def tick(ticker: Ticker): Channel[Int32] = {
      match ticker {
        case Ticker(tickChannel, _stopChannel) => tickChannel
      }
    }
}

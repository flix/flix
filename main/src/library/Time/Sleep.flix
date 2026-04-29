/*
 *  Copyright 2026 Flix Authors
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

pub mod Time.Sleep {

    use Math.Random
    use Time.Duration
    use Time.Sleep
    use RichString.{text, gray}

    import java.lang.{Thread => JThread}

    ///
    /// An effect used to sleep the current thread.
    ///
    pub eff Sleep {

        ///
        /// Sleeps the current thread for the given duration `d`.
        ///
        def sleep(d: Duration): Unit

    }

    ///
    /// Handles the `Sleep` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `Sleep` effect using the `IO` effect.
    ///
    pub def handle(f: a -> b \ ef): a -> b \ (ef - Sleep) + IO = x ->
        run {
            f(x)
        } with handler Sleep {
            def sleep(d, k) =
                let millis = Duration.toMillis(d);
                JThread.sleep(millis);
                k()
        }

    ///
    /// Runs the `Sleep` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `Sleep` effect using the `IO` effect.
    ///
    @DefaultHandler
    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - Sleep) + IO = handle(f)()

    ///
    /// Runs the given function `f` ignoring all sleeps (no-op).
    ///
    pub def withNoOp(f: Unit -> a \ ef): a \ (ef - Sleep) =
        run {
            f()
        } with handler Sleep {
            def sleep(_d, k) = k()
        }

    ///
    /// Middleware that intercepts the `Sleep` effect, replacing each sleep duration
    /// with the given constant `duration`, ignoring the original duration.
    ///
    pub def withConstant(duration: Duration, f: Unit -> a \ ef): a \ (ef - Sleep) + Sleep =
        run {
            f()
        } with handler Sleep {
            def sleep(_d, k) = {
                Sleep.sleep(duration);
                k()
            }
        }

    ///
    /// Runs the given function `f` collecting all sleep durations into a list.
    ///
    /// Returns a pair of the result and the list of durations in call order.
    ///
    pub def withCollect(f: Unit -> a \ ef): (a, List[Duration]) \ (ef - Sleep) =
        run {
            (f(), List.empty())
        } with handler Sleep {
            def sleep(d, k) =
                let (r, l) = k();
                (r, d :: l)
        }

    ///
    /// Middleware that intercepts the `Sleep` effect, logging each sleep duration
    /// via the `Logger` effect and re-raising the `Sleep` effect.
    ///
    pub def withLogging(f: Unit -> a \ ef): a \ (ef - Sleep) + {Sleep, Logger} =
        run {
            f()
        } with handler Sleep {
            def sleep(d, k) = {
                Logger.log(Severity.Debug, gray("Sleep: ") + text("${d}"));
                Sleep.sleep(d);
                k()
            }
        }

    ///
    /// Middleware that intercepts the `Sleep` effect, scaling each sleep duration
    /// by the given `factor`.
    ///
    pub def withScale(factor: Float64, f: Unit -> a \ ef): a \ (ef - Sleep) + Sleep =
        run {
            f()
        } with handler Sleep {
            def sleep(d, k) = {
                let nanos = Int64.toFloat64(Duration.toNanos(d)) * factor;
                let scaled = Float64.clampToInt64(min = 0i64, max = Int64.maxValue(), nanValue = 0i64, nanos);
                Sleep.sleep(Duration.nanoseconds(scaled));
                k()
            }
        }

    ///
    /// Middleware that intercepts the `Sleep` effect, capping each sleep duration
    /// to the given `cap`.
    ///
    pub def withMaxSleep(cap: Duration, f: Unit -> a \ ef): a \ (ef - Sleep) + Sleep =
        run {
            f()
        } with handler Sleep {
            def sleep(d, k) = {
                Sleep.sleep(Order.min(d, cap));
                k()
            }
        }

    ///
    /// Middleware that intercepts the `Sleep` effect, capping the total cumulative
    /// sleep duration to the given `budget`. Once the budget is exhausted, all
    /// further sleeps become no-ops.
    ///
    pub def withMaxTotalSleep(budget: Duration, f: Unit -> a \ ef): a \ (ef - Sleep) + Sleep =
        region rc {
            let remaining: Ref[Duration, _] = Ref.fresh(rc, budget);
            run {
                f()
            } with handler Sleep {
                def sleep(d, k) = {
                    let r = Ref.get(remaining);
                    let actual = Order.min(d, r);
                    Ref.put(r - actual, remaining);
                    Sleep.sleep(actual);
                    k()
                }
            }
        }

    ///
    /// Middleware that intercepts the `Sleep` effect, ensuring each sleep duration
    /// is at least the given `floor`.
    ///
    pub def withMinSleep(floor: Duration, f: Unit -> a \ ef): a \ (ef - Sleep) + Sleep =
        run {
            f()
        } with handler Sleep {
            def sleep(d, k) = {
                Sleep.sleep(Order.max(d, floor));
                k()
            }
        }

    ///
    /// Middleware that intercepts the `Sleep` effect, adding random jitter to each
    /// sleep duration. The `factor` controls the maximum jitter fraction, e.g. `0.2`
    /// means each sleep duration is randomly adjusted by up to ±20%.
    ///
    pub def withJitter(factor: Float64, f: Unit -> a \ ef): a \ (ef - Sleep) + {Sleep, Random} =
        run {
            f()
        } with handler Sleep {
            def sleep(d, k) = {
                let nanos = Int64.toFloat64(Duration.toNanos(d));
                let r = Random.randomFloat64() * 2.0f64 * factor - factor;
                let jittered = nanos * (1.0f64 + r);
                let clamped = Float64.clampToInt64(min = 0i64, max = Int64.maxValue(), nanValue = 0i64, jittered);
                Sleep.sleep(Duration.nanoseconds(clamped));
                k()
            }
        }

}

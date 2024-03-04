/*
 * Copyright 2021 Jonathan Lindegaard Starup
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

mod Concurrent {

    ///
    /// A wrapper around a Java Condition.
    ///
    @Internal
    pub enum Condition[r: Region](Region[r], ##java.util.concurrent.locks.Condition)

    mod Condition {
        // TODO NS-REFACTOR these imports can be removed
        use Concurrent.Condition;
        use Concurrent.Condition.Condition;

        ///
        /// Causes the current thread to wait until it is signalled or interrupted.
        ///
        @Internal
        pub def await(condition: Condition[r]): Unit \ r =
            import java.util.concurrent.locks.Condition.await(): Unit \ r;
            let Condition(_, c) = condition;
            await(c)

        ///
        /// Causes the current thread to wait until it is signalled or interrupted, or the specified waiting
        /// time(nanos) elapses. The method returns an estimate of the number of nanoseconds remaining to wait
        /// given the supplied nanosTimeout value upon return, or a value less than or equal to zero if it timed out.
        ///
        @Internal
        pub def awaitTimeout(condition: Condition[r], timeout: Int64): Int64 \ r =
            import java.util.concurrent.locks.Condition.awaitNanos(Int64): Int64 \ r;
            let Condition(_, c) = condition;
            awaitNanos(c, timeout)

        ///
        /// Wakes up one waiting thread.
        ///
        @Internal
        pub def signal(condition: Condition[r]): Result[##java.lang.IllegalMonitorStateException, Unit] \ r =
            import java.util.concurrent.locks.Condition.signal(): Unit \ r;
            let Condition(_, c) = condition;
            try {
                Ok(signal(c))
            } catch {
                case e: ##java.lang.IllegalMonitorStateException => Err(e)
            }

        ///
        /// Wakes up all waiting threads.
        ///
        @Internal
        pub def signalAll(condition: Condition[r]): Result[##java.lang.IllegalMonitorStateException, Unit] \ r =
            import java.util.concurrent.locks.Condition.signalAll(): Unit \ r;
            let Condition(_, c) = condition;
            try {
                Ok(signalAll(c))
            } catch {
                case e: ##java.lang.IllegalMonitorStateException => Err(e)
            }
    }
}

/*
 * Copyright 2021 Jonathan Lindegaard Starup
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

pub mod Concurrent.Condition {

    import java.lang.IllegalMonitorStateException
    import java.util.concurrent.locks.{Condition => JCondition}

    ///
    /// A wrapper around a Java Condition.
    ///
    pub enum Condition(JCondition)

    ///
    /// Causes the current thread to wait until it is signalled or interrupted.
    ///
    pub def await(condition: Condition): Unit \ IO =
        let Condition(c) = condition;
        c.await()

    ///
    /// Causes the current thread to wait until it is signalled or interrupted, or the specified waiting
    /// time(nanos) elapses. The method returns an estimate of the number of nanoseconds remaining to wait
    /// given the supplied nanosTimeout value upon return, or a value less than or equal to zero if it timed out.
    ///
    pub def awaitTimeout(condition: Condition, timeout: Int64): Int64 \ IO =
        let Condition(c) = condition;
        c.awaitNanos(timeout)

    ///
    /// Wakes up one waiting thread.
    ///
    pub def signal(condition: Condition): Result[IllegalMonitorStateException, Unit] \ IO =
        let Condition(c) = condition;
        try {
            Ok(c.signal())
        } catch {
            case e: IllegalMonitorStateException => Err(e)
        }

    ///
    /// Wakes up all waiting threads.
    ///
    pub def signalAll(condition: Condition): Result[IllegalMonitorStateException, Unit] \ IO =
        let Condition(c) = condition;
        try {
            Ok(c.signalAll())
        } catch {
            case e: IllegalMonitorStateException => Err(e)
        }
}

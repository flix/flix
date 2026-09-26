/*
 * Copyright 2021 Jonathan Lindegaard Starup
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

pub mod Concurrent.ReentrantLock {

    import java.util.concurrent.TimeUnit
    import java.lang.InterruptedException
    import java.lang.IllegalMonitorStateException
    import java.util.concurrent.locks.{ReentrantLock => JReentrantLock}

    use Concurrent.Condition
    use Concurrent.Condition.Condition

    ///
    /// A wrapper around a Java ReentrantLock.
    ///
    pub enum ReentrantLock(JReentrantLock)

    ///
    /// Creates an instance of ReentrantLock with the given fairness policy.
    /// `fair = False` is the default for Java.
    ///
    pub def newLock(fair: Bool): ReentrantLock \ IO =
        ReentrantLock(new JReentrantLock(fair))

    ///
    /// Returns true if this lock has fairness set true.
    ///
    pub def isFair(lock: ReentrantLock): Bool \ {} =
        let ReentrantLock(l) = lock;
        l.isFair()

    ///
    /// Acquires the lock.
    ///
    pub def lock(lock: ReentrantLock): Unit \ IO =
        let ReentrantLock(l) = lock;
        l.lock()

    ///
    /// Acquires the lock unless the current thread is interrupted.
    ///
    pub def lockInterruptibly(lock: ReentrantLock): Result[InterruptedException, Unit] \ IO =
        let ReentrantLock(l) = lock;
        try {
            Ok(l.lockInterruptibly())
        } catch {
            case e: InterruptedException => Err(e)
        }

    ///
    /// Returns a Condition instance for use with this Lock instance.
    ///
    pub def newCondition(lock: ReentrantLock): Condition \ IO =
        let ReentrantLock(l) = lock;
        Condition(l.newCondition())

    ///
    /// Acquires the lock only if it is not held by another thread at the time of invocation.
    ///
    pub def tryLock(lock: ReentrantLock): Bool \ IO =
        let ReentrantLock(l) = lock;
        l.tryLock()

    ///
    /// Acquires the lock if it is not held by another thread within the given waiting time (nanos) and the current thread has not been interrupted.
    ///
    pub def tryLockNanos(lock: ReentrantLock, nanosTimeout: Int64): Result[InterruptedException, Bool] \ IO =
        let ReentrantLock(l) = lock;
        try {
            Ok(l.tryLock(nanosTimeout, TimeUnit.NANOSECONDS))
        } catch {
            case e: InterruptedException => Err(e)
        }

    ///
    /// Attempts to release this lock.
    ///
    pub def unlock(lock: ReentrantLock): Result[IllegalMonitorStateException, Unit] \ IO =
        let ReentrantLock(l) = lock;
        try {
            Ok(l.unlock())
        } catch {
            case e: IllegalMonitorStateException => Err(e)
        }

}

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

///
/// A wrapper around a Java ReentrantLock.
///
pub opaque type ReentrantLock = ##java.util.concurrent.locks.ReentrantLock

// TODO: waiting for Condition.flix to be merged into master
opaque type Condition = ##java.util.concurrent.locks.Condition

namespace ReentrantLock {

    ///
    /// Creates an instance of ReentrantLock with the given fairness policy.
    /// `fair = False` is the default for Java.
    ///
    pub def makeLock(fair: Bool): ReentrantLock & Impure =
        import new java.util.concurrent.locks.ReentrantLock(Bool) as reentrantLock;
        ReentrantLock(reentrantLock(fair))

    ///
    /// Queries the number of holds on this lock by the current thread.
    ///
    pub def getHoldCount(lock: ReentrantLock): Int & Impure =
        import java.util.concurrent.locks.ReentrantLock.getHoldCount();
        let ReentrantLock(l) = lock;
        let x = Instant(???);
        l.getHoldCount()

    ///
    /// Returns an estimate of the number of threads waiting to acquire this lock.
    ///
    pub def getQueueLength(lock: ReentrantLock): Int & Impure =
        import java.util.concurrent.locks.ReentrantLock.getQueueLength();
        let ReentrantLock(l) = lock;
        l.getQueueLength()

    ///
    /// Returns an estimate of the number of threads waiting on the given condition associated with this lock.
    ///
    pub def getWaitQueueLength(lock: ReentrantLock, condition: Condition): Result[Int, ##java.lang.Exception] & Impure =
        import java.util.concurrent.locks.ReentrantLock.getWaitQueueLength(##java.util.concurrent.locks.Condition);
        let ReentrantLock(l) = lock;
        let Condition(c) = condition;
        try {
            Ok(l.getWaitQueueLength(c))
        } catch {
            case e: ##java.lang.IllegalMonitorStateException => Err(e as ##java.lang.Exception)
            case e: ##java.lang.IllegalArgumentException => Err(e as ##java.lang.Exception)
            case e: ##java.lang.NullPointerException => Err(e as ##java.lang.Exception)
        }

    // TODO: should this be skipped?
    // public final boolean hasQueuedThread​(Thread thread)

    ///
    /// Queries whether the given thread is waiting to acquire this lock.
    ///
    pub def hasQueuedThreads(lock: ReentrantLock): Bool & Impure =
        import java.util.concurrent.locks.ReentrantLock.hasQueuedThreads();
        let ReentrantLock(l) = lock;
        l.hasQueuedThreads()

    ///
    /// Queries whether any threads are waiting to acquire this lock.
    ///
    pub def hasWaiters(lock: ReentrantLock, condition: Condition): Result[Bool, ##java.lang.Exception] & Impure =
        import java.util.concurrent.locks.ReentrantLock.hasWaiters(##java.util.concurrent.locks.Condition);
        let ReentrantLock(l) = lock;
        let Condition(c) = condition;
        try {
            Ok(l.hasWaiters(c))
        } catch {
            case e: ##java.lang.IllegalMonitorStateException => Err(e as ##java.lang.Exception)
            case e: ##java.lang.IllegalArgumentException => Err(e as ##java.lang.Exception)
            case e: ##java.lang.NullPointerException => Err(e as ##java.lang.Exception)
        }

    ///
    /// Returns true if this lock has fairness set true.
    ///
    pub def isFair(lock: ReentrantLock): Bool & Impure =
        import java.util.concurrent.locks.ReentrantLock.isFair();
        let ReentrantLock(l) = lock;
        l.isFair()

    ///
    /// Queries if this lock is held by the current thread.
    ///
    pub def isHeldByCurrentThread(lock: ReentrantLock): Bool & Impure =
        import java.util.concurrent.locks.ReentrantLock.isHeldByCurrentThread();
        let ReentrantLock(l) = lock;
        l.isHeldByCurrentThread()

    ///
    /// Queries if this lock is held by any thread.
    ///
    pub def isLocked(lock: ReentrantLock): Bool & Impure =
        import java.util.concurrent.locks.ReentrantLock.isLocked();
        let ReentrantLock(l) = lock;
        l.isLocked()

    ///
    /// Acquires the lock.
    ///
    pub def lock(lock: ReentrantLock): Unit & Impure =
        import java.util.concurrent.locks.ReentrantLock.lock() as lockIt;
        let ReentrantLock(l) = lock;
        l.lockIt()

    ///
    /// Acquires the lock unless the current thread is interrupted.
    ///
    pub def lockInterruptibly(lock: ReentrantLock): Result[Unit, ##java.lang.InterruptedException] & Impure =
        import java.util.concurrent.locks.ReentrantLock.lockInterruptibly();
        let ReentrantLock(l) = lock;
        try {
            Ok(l.lockInterruptibly())
        } catch {
            case e: ##java.lang.InterruptedException => Err(e)
        }

    ///
    /// Returns a Condition instance for use with this Lock instance.
    ///
    pub def newCondition(lock: ReentrantLock): Condition & Impure =
        import java.util.concurrent.locks.ReentrantLock.newCondition();
        let ReentrantLock(l) = lock;
        Condition(l.newCondition())

    ///
    /// Acquires the lock only if it is not held by another thread at the time of invocation.
    ///
    pub def tryLock(lock: ReentrantLock): Bool & Impure =
        import java.util.concurrent.locks.ReentrantLock.tryLock();
        let ReentrantLock(l) = lock;
        l.tryLock()

    ///
    /// Acquires the lock if it is not held by another thread within the given waiting time (nanos) and the current thread has not been interrupted.
    ///
    pub def tryLockNanos(lock: ReentrantLock, nanosTimeout: Int64): Result[Bool, ##java.lang.InterruptedException] & Impure =
        import java.util.concurrent.locks.ReentrantLock.tryLock(Int64, ##java.util.concurrent.TimeUnit);
        import get java.util.concurrent.TimeUnit:NANOSECONDS as nano;
        let ReentrantLock(l) = lock;
        try {
            Ok(l.tryLock(nanosTimeout, nano()))
        } catch {
            case e: ##java.lang.InterruptedException => Err(e)
        }

    ///
    /// Attempts to release this lock.
    ///
    pub def unlock(lock: ReentrantLock): Result[Unit, ##java.lang.IllegalMonitorStateException] & Impure =
        import java.util.concurrent.locks.ReentrantLock.unlock();
        let ReentrantLock(l) = lock;
        try {
            Ok(l.unlock())
        } catch {
            case e: ##java.lang.IllegalMonitorStateException => Err(e)
        }
}

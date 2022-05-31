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

namespace Concurrent/ReentrantLock {

    ///
    /// A wrapper around a Java ReentrantLock.
    ///
    pub enum ReentrantLock(##java.util.concurrent.locks.ReentrantLock)

    ///
    /// Creates an instance of ReentrantLock with the given fairness policy.
    /// `fair = False` is the default for Java.
    ///
    pub def new(fair: Bool): Concurrent/ReentrantLock.ReentrantLock & Impure =
        import new java.util.concurrent.locks.ReentrantLock(Bool): ##java.util.concurrent.locks.ReentrantLock & Impure as reentrantLock;
        ReentrantLock(reentrantLock(fair))

    ///
    /// Returns true if this lock has fairness set true.
    ///
    pub def isFair(lock: Concurrent/ReentrantLock.ReentrantLock): Bool & Pure =
        import java.util.concurrent.locks.ReentrantLock.isFair(): Bool & Pure;
        let ReentrantLock(l) = lock;
        isFair(l)

    ///
    /// Acquires the lock.
    ///
    pub def lock(lock: Concurrent/ReentrantLock.ReentrantLock): Unit & Impure =
        import java.util.concurrent.locks.ReentrantLock.lock(): Unit & Impure as lockIt;
        let ReentrantLock(l) = lock;
        lockIt(l)

    ///
    /// Acquires the lock unless the current thread is interrupted.
    ///
    pub def lockInterruptibly(lock: Concurrent/ReentrantLock.ReentrantLock): Result[Unit, ##java.lang.InterruptedException] & Impure =
        import java.util.concurrent.locks.ReentrantLock.lockInterruptibly(): Unit & Impure;
        let ReentrantLock(l) = lock;
        try {
            Ok(lockInterruptibly(l))
        } catch {
            case e: ##java.lang.InterruptedException => Err(e)
        }

    ///
    /// Returns a Condition instance for use with this Lock instance.
    ///
    pub def newCondition(lock: Concurrent/ReentrantLock.ReentrantLock): Concurrent/Condition.Condition & Impure =
        import java.util.concurrent.locks.ReentrantLock.newCondition(): ##java.util.concurrent.locks.Condition & Impure;
        let ReentrantLock(l) = lock;
        use Concurrent/Condition.Condition;
        Condition.Condition(newCondition(l))

    ///
    /// Acquires the lock only if it is not held by another thread at the time of invocation.
    ///
    pub def tryLock(lock: Concurrent/ReentrantLock.ReentrantLock): Bool & Impure =
        import java.util.concurrent.locks.ReentrantLock.tryLock(): Bool & Impure;
        let ReentrantLock(l) = lock;
        tryLock(l)

    ///
    /// Acquires the lock if it is not held by another thread within the given waiting time (nanos) and the current thread has not been interrupted.
    ///
    pub def tryLockNanos(lock: Concurrent/ReentrantLock.ReentrantLock, nanosTimeout: Int64): Result[Bool, ##java.lang.InterruptedException] & Impure =
        import java.util.concurrent.locks.ReentrantLock.tryLock(Int64, ##java.util.concurrent.TimeUnit): Bool & Impure;
        import static get java.util.concurrent.TimeUnit.NANOSECONDS: ##java.util.concurrent.TimeUnit & Impure as nano;
        let ReentrantLock(l) = lock;
        try {
            Ok(tryLock(l, nanosTimeout, nano()))
        } catch {
            case e: ##java.lang.InterruptedException => Err(e)
        }

    ///
    /// Attempts to release this lock.
    ///
    pub def unlock(lock: Concurrent/ReentrantLock.ReentrantLock): Result[Unit, ##java.lang.IllegalMonitorStateException] & Impure =
        import java.util.concurrent.locks.ReentrantLock.unlock(): Unit & Impure;
        let ReentrantLock(l) = lock;
        try {
            Ok(unlock(l))
        } catch {
            case e: ##java.lang.IllegalMonitorStateException => Err(e)
        }
}

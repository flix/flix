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
    import java.util.concurrent.locks.{ReentrantLock => JReentrantLock}

    ///
    /// A wrapper around a Java ReentrantLock.
    ///
    @Internal
    pub enum ReentrantLock(JReentrantLock)

    mod ReentrantLock {
        // TODO NS-REFACTOR these imports can be removed
        use Concurrent.ReentrantLock
        use Concurrent.ReentrantLock.ReentrantLock

        use Concurrent.Condition
        use Concurrent.Condition.Condition

        import java.util.concurrent.locks.{ReentrantLock => JReentrantLock};

        ///
        /// Creates an instance of ReentrantLock with the given fairness policy.
        /// `fair = False` is the default for Java.
        ///
        @Internal
        pub def newLock(fair: Bool): ReentrantLock \ IO =
            ReentrantLock(new JReentrantLock(fair))

        ///
        /// Returns true if this lock has fairness set true.
        ///
        @Internal
        pub def isFair(lock: ReentrantLock): Bool \ {} =
            let ReentrantLock(l) = lock;
            unsafe l.isFair()

        ///
        /// Acquires the lock.
        ///
        @Internal
        pub def lock(lock: ReentrantLock): Unit \ IO =
            let ReentrantLock(l) = lock;
            l.lock()

        ///
        /// Acquires the lock unless the current thread is interrupted.
        ///
        @Internal
        pub def lockInterruptibly(lock: ReentrantLock): Result[##java.lang.InterruptedException, Unit] \ IO =
            let ReentrantLock(l) = lock;
            try {
                Ok(l.lockInterruptibly())
            } catch {
                case e: ##java.lang.InterruptedException => Err(e)
            }

        ///
        /// Returns a Condition instance for use with this Lock instance.
        ///
        @Internal
        pub def newCondition(lock: ReentrantLock): Condition \ IO =
            let ReentrantLock(l) = lock;
            Condition(l.newCondition())

        ///
        /// Acquires the lock only if it is not held by another thread at the time of invocation.
        ///
        @Internal
        pub def tryLock(lock: ReentrantLock): Bool \ IO =
            let ReentrantLock(l) = lock;
            l.tryLock()

        ///
        /// Acquires the lock if it is not held by another thread within the given waiting time (nanos) and the current thread has not been interrupted.
        ///
        @Internal
        pub def tryLockNanos(lock: ReentrantLock, nanosTimeout: Int64): Result[##java.lang.InterruptedException, Bool] \ IO =
            import static java_get_field java.util.concurrent.TimeUnit.NANOSECONDS: ##java.util.concurrent.TimeUnit \ IO as nano;
            let ReentrantLock(l) = lock;
            try {
                Ok(l.tryLock(nanosTimeout, nano()))
            } catch {
                case e: ##java.lang.InterruptedException => Err(e)
            }

        ///
        /// Attempts to release this lock.
        ///
        @Internal
        pub def unlock(lock: ReentrantLock): Result[##java.lang.IllegalMonitorStateException, Unit] \ IO =
            let ReentrantLock(l) = lock;
            try {
                Ok(l.unlock())
            } catch {
                case e: ##java.lang.IllegalMonitorStateException => Err(e)
            }

    }
}

/*
 * Copyright 2025 Casper Dalgaard Nielsen
 *                Adam Yasser Tallouzi
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

pub mod BPlusTree.Lock {
    import java.util.concurrent.locks.{StampedLock => JStampedLock}

    // Unchecked casts are here used to remove the IO effect. It is replaced with an
    // effect to a region to ensure that the code is not optimized away.

    ///
    /// Wait for `lock` to be unlocked by the current holder of the write-lock on it.
    ///
    /// Why:
    ///
    /// Virtual threads can exhibit weird behaviour when using all physical threads
    /// and using locking. Without this the threads using optimistic reading will
    /// never be removed from the physical thread. Meanwhile a thread that holds the
    /// lock will never get to run and we have a deadlock. Progress can happen, but it
    /// appears to be extremely unlikely. To fix this call a blocking operation which
    /// makes the JVM remove the virtual threads from the physical threads allowing
    /// the lock-holder to unlock.
    ///
    /// For a better explanation see Netflix's encounter of a similar problem:
    /// https://netflixtechblog.com/java-21-virtual-threads-dude-wheres-my-lock-3052540e231d?gi=128dba0ad426
    ///
    pub def yieldBasedOn(lock: Lock[r]): Unit \ r =
        let Lock(l) = lock;
        unsafe IO as r { l.unlockRead(l.readLock()) }

    ///
    /// Returns `true` if `lock` is currently held by a thread.
    ///
    /// Only supposed to be used for testing.
    ///
    pub def isLocked(lock: Lock[r]): Bool \ r =
        let Lock(l) = lock;
        unsafe IO as r { l.isWriteLocked() or l.isReadLocked() }

    ///
    /// Returns a fresh lock.
    ///
    pub def mkLock(_: Region[r]): Lock[r] \ r =
        Lock(unsafe IO as r { new JStampedLock() })

    /// Attempt to upgrade `stamp` on `lock` to a write-lock on `lock`.
    /// The returned stamp cannot be invalidated if it was valid when issued.
    ///
    /// This operation is non-blocking.
    ///
    pub def tryConvertToWrite(stamp: Int64, lock: Lock[r]): Int64 \ r =
        let Lock(l) = lock;
        unsafe IO as r { l.tryConvertToWriteLock(stamp) }

    ///
    /// Return an optimistic stamp on `lock` which can be used in `valid` to
    /// assert that no thread has taken a write lock since `stamp` was issued.
    ///
    /// This operation is non-blocking.
    ///
    pub def tryReadLock(lock: Lock[r]): Int64 \ r =
        let Lock(l) = lock;
        unsafe IO as r { l.tryOptimisticRead() }

    ///
    /// Unlock `lock` with the issued write-stamp, `stamp`.
    ///
    /// This operation is non-blocking.
    ///
    pub def unlockWrite(stamp: Int64, lock: Lock[r]): Unit \ r =
        let Lock(l) = lock;
        unsafe IO as r { l.unlockWrite(stamp) }

    ///
    /// Returns true if `stamp` is still valid on `lock`, meaning no thread
    /// has acquired a write-lock on `lock` since `stamp` was issued.
    ///
    /// This operation is non-blocking.
    ///
    pub def valid(stamp: Int64, lock: Lock[r]): Bool \ r =
        let Lock(l) = lock;
        unsafe IO as r { l.validate(stamp) }

    ///
    /// Lock `lock` and returns the stamp.
    ///
    /// This operation is blocking.
    ///
    pub def writeLock(lock: Lock[r]): Int64 \ r =
        let Lock(l) = lock;
        unsafe IO as r { l.writeLock() }
}

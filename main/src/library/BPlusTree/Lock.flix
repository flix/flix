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

    ///
    /// The purpose of Lock is to provide a thin wrapper around Java's StampedLock without
    /// the IO effect. The lock is optimistic in the sense that reads are performed by
    /// reading the values and only afterwards verifying that no other thread has changed
    /// the values. Only annotating a region effect to StampedLock is safe, since it can
    /// only non-deterministically block a thread for some ( potentially infinite) amount
    /// of time. This is allowed under non-effectful behavior.
    ///
    ///
    /// One possible danger, introduced by StampedLock, is handled by a null-check in
    /// binarySearch. Consider the following example where everything is initialized to 0:
    /// Thread 1 executes
    /// {a = 2; b = 3}
    /// Thread 2 executes
    /// {c = a; d = b}
    /// The question is what can `c` and `d` be? It depends on the JVM, but can be any of:
    /// {c = 2, d = 3}, {c = 0, d = 0}, {c = 2, d = 0}, {c = 0, d = 3}
    /// The last follows from the fact that the JVM can reorder statements under some
    /// requirements. The requirements are roughly that the new 'code' should be
    /// equivalent on a single-threaded machine and there are no happens-before relations
    /// which link to other threads preventing the reordering. If either variable were
    /// volatile we might avoid this, but volatile is expensive (and not in Flix).
    ///
    /// For us, this means that {array[size] = 5; size++} can be reordered to
    /// {oldSize = size++; array[oldSize] = 5}.
    /// Another thread reading may then see the increase of size and try to access that
    /// position getting null. This has not happened in our experiments, but we wish to
    /// handle the case. The risk is in binarySearch and we therefore check whether any
    /// value read is null and if so 'aborts'.
    ///
    /// For Java's memory model see: https://docs.oracle.com/javase/specs/jls/se8/html/jls-17.html#jls-17.4
    ///
    pub enum Lock[_: Region](JStampedLock)

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
    /// Executes `onValid` if `stamp` is still valid on `lock`.
    ///
    /// Otherwise continues with `onInvalid` without yielding.
    ///
    /// Caution: callers often use `onInvalid` to restart a larger operation. In such
    /// cases, effectful callbacks passed here may be invoked an unexpected number of
    /// times across retries.
    ///
    pub def ifValidElseNoYield(
        stamp: Int64,
        lock: Lock[r],
        onValid: Unit -> a \ ef1,
        onInvalid: Unit -> a \ ef2
    ): a \ ef1 + ef2 + r =
        if (valid(stamp, lock))
            onValid()
        else
            onInvalid()

    ///
    /// Executes `onValid` if `stamp` is still valid on `lock`.
    ///
    /// Otherwise yields and continues with `onInvalid`.
    ///
    /// Caution: callers often use `onInvalid` to restart a larger operation. In such
    /// uses, effectful callbacks passed here may be invoked an unexpected number of
    /// times across retries.
    ///
    pub def ifValidElse(
        stamp: Int64,
        lock: Lock[r],
        onValid: Unit -> a \ ef1,
        onInvalid: Unit -> a \ ef2
    ): a \ ef1 + ef2 + r =
        ifValidElseNoYield(stamp, lock, onValid, () -> {
            yieldBasedOn(lock);
            onInvalid()
        })

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
    /// Attempts to upgrade `stamp` to a write lock and executes `onWrite`
    /// with the resulting write stamp if successful.
    ///
    /// Otherwise yields and continues with `onInvalid`.
    ///
    /// Caution: callers often use `onInvalid` to restart a larger operation. In such
    /// uses, effectful callbacks passed here may be invoked an unexpected number of
    /// times across retries.
    ///
    pub def withWriteLockOrElse(
        stamp: Int64,
        lock: Lock[r],
        onWrite: Int64 -> a \ ef1,
        onInvalid: Unit -> a \ ef2
    ): a \ ef1 + ef2 + r =
        let writeStamp = tryConvertToWrite(stamp, lock);
        ifValidElse(writeStamp, lock, () -> onWrite(writeStamp), onInvalid)

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

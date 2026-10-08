/*
 * Copyright 2025 Casper Dalgaard Nielsen
 *                Adam Yasser Tallouzi
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

/// Functions on read-write locks.
pub mod Fixpoint3.ReadWriteLock {
    import java.util.concurrent.locks.{StampedLock => JStampedLock}

    /// Returns a fresh read-write lock in the given region.
    pub def mkLock(_: Region[r]): ReadWriteLock[r] \ r =
        ReadWriteLock(unsafe IO as r { new JStampedLock().asReadWriteLock() })

    /// Acquires the read lock of `lock`, blocking until it is available.
    pub def readLock(lock: ReadWriteLock[r]): Unit \ r =
        let ReadWriteLock(l) = lock;
        unsafe IO as r { l.readLock().lock() }

    /// Acquires the write lock of `lock`, blocking until it is available.
    pub def writeLock(lock: ReadWriteLock[r]): Unit \ r =
        let ReadWriteLock(l) = lock;
        unsafe IO as r { l.writeLock().lock() }

    /// Releases the read lock of `lock`.
    pub def unlockRead(lock: ReadWriteLock[r]): Unit \ r =
        let ReadWriteLock(l) = lock;
        unsafe IO as r { l.readLock().unlock() }

    /// Releases the write lock of `lock`.
    pub def unlockWrite(lock: ReadWriteLock[r]): Unit \ r =
        let ReadWriteLock(l) = lock;
        unsafe IO as r { l.writeLock().unlock() }
}

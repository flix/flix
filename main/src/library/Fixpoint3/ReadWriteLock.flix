/*
 * Copyright 2025 Casper Dalgaard Nielsen
 *                Adam Yasser Tallouzi
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

pub mod Fixpoint3.ReadWriteLock {
    import java.util.concurrent.locks.{StampedLock => JStampedLock}

    pub def mkLock(_: Region[r]): ReadWriteLock[r] \ r =
        ReadWriteLock(unsafe IO as r { new JStampedLock().asReadWriteLock() })

    pub def readLock(lock: ReadWriteLock[r]): Unit \ r =
        let ReadWriteLock(l) = lock;
        unsafe IO as r { l.readLock().lock() }

    pub def writeLock(lock: ReadWriteLock[r]): Unit \ r =
        let ReadWriteLock(l) = lock;
        unsafe IO as r { l.writeLock().lock() }

    pub def unlockRead(lock: ReadWriteLock[r]): Unit \ r =
        let ReadWriteLock(l) = lock;
        unsafe IO as r { l.readLock().unlock() }

    pub def unlockWrite(lock: ReadWriteLock[r]): Unit \ r =
        let ReadWriteLock(l) = lock;
        unsafe IO as r { l.writeLock().unlock() }
}

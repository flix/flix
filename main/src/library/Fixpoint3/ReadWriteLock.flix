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

mod Fixpoint3.ReadWriteLock {
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

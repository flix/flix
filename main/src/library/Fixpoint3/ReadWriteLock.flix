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

/// 
/// A wrapper around a Java read-write-lock.
///
mod Fixpoint3 {
    import java.util.concurrent.locks.{ReadWriteLock => JReadWriteLock}

    @Internal
    enum ReadWriteLock[_: Region](JReadWriteLock)

    mod ReadWriteLock {
        import java.util.concurrent.locks.{StampedLock => JStampedLock}
        use Fixpoint3.ReadWriteLock

        pub def mkLock(_: Region[r]): ReadWriteLock[r] \ r =
            ReadWriteLock(unchecked_cast((new JStampedLock().asReadWriteLock(): _ \ IO) as _ \ r))

        pub def readLock(lock: ReadWriteLock[r]): Unit \ r =
            let ReadWriteLock(l) = lock;
            unchecked_cast((l.readLock().lock(): _ \ IO) as _ \ r)

        pub def writeLock(lock: ReadWriteLock[r]): Unit \ r =
            let ReadWriteLock(l) = lock;
            unchecked_cast((l.writeLock().lock(): _ \ IO) as _ \ r)

        pub def unlockRead(lock: ReadWriteLock[r]): Unit \ r =
            let ReadWriteLock(l) = lock;
            unchecked_cast((l.readLock().unlock(): _ \ IO) as _ \ r)

        pub def unlockWrite(lock: ReadWriteLock[r]): Unit \ r =
            let ReadWriteLock(l) = lock;
            unchecked_cast((l.writeLock().unlock(): _ \ IO) as _ \ r)
    }

}

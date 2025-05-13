/// 
/// A wrapper around a Java read-write-lock.
///
mod Fixpoint3 {
    import java.util.concurrent.locks.{ReadWriteLock => JReadWriteLock}
    @Internal
    pub enum ReadWriteLock[_: Region](JReadWriteLock)

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
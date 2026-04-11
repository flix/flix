pub mod Fixpoint3.ReadWriteLock {

    pub def mkLock(_: Region[r]): ReadWriteLock[r] \ r =
        checked_ecast(ReadWriteLock(unchecked_cast(() as AnyType)))

    pub def readLock(_lock: ReadWriteLock[r]): Unit \ r = checked_ecast(())

    pub def writeLock(_lock: ReadWriteLock[r]): Unit \ r = checked_ecast(())

    pub def unlockRead(_lock: ReadWriteLock[r]): Unit \ r = checked_ecast(())

    pub def unlockWrite(_lock: ReadWriteLock[r]): Unit \ r = checked_ecast(())

}

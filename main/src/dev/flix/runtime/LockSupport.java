package dev.flix.runtime;

import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.ReentrantLock;

/**
 * Portable synchronization runtime support for the JVM backend.
 *
 * The contract is task-oriented: ownership is tracked by the current Flix task.
 * On the JVM today each portable Flix task runs on one Java thread, so thread identity
 * is the concrete owner token used here.
 */
public final class LockSupport {

    private static final int CONDITION_AWAIT_OK = 0;
    private static final int CONDITION_AWAIT_NOT_OWNER = 1;

    private LockSupport() {
    }

    public static Object newLock() {
        return new PortableLock();
    }

    public static void lock(Object lock) {
        ((PortableLock) lock).lock();
    }

    public static boolean tryLock(Object lock) {
        return ((PortableLock) lock).tryLock();
    }

    public static boolean unlock(Object lock) {
        return ((PortableLock) lock).unlock();
    }

    public static Object newCondition(Object lock) {
        return ((PortableLock) lock).newCondition();
    }

    public static int awaitCondition(Object condition) {
        return ((PortableCondition) condition).await();
    }

    public static boolean signalCondition(Object condition) {
        return ((PortableCondition) condition).signal();
    }

    public static boolean signalAllCondition(Object condition) {
        return ((PortableCondition) condition).signalAll();
    }

    public static Object newBarrier(int parties) {
        return parties > 0 ? new PortableBarrier(parties) : null;
    }

    public static int awaitBarrier(Object barrier) {
        return ((PortableBarrier) barrier).await();
    }

    public static Object newCountDownLatch(int count) {
        return count >= 0 ? new PortableCountDownLatch(count) : null;
    }

    public static void awaitCountDownLatch(Object latch) {
        ((PortableCountDownLatch) latch).await();
    }

    public static void countDownCountDownLatch(Object latch) {
        ((PortableCountDownLatch) latch).countDown();
    }

    public static Object newSemaphore(int permits) {
        return permits >= 0 ? new PortableSemaphore(permits) : null;
    }

    public static void acquireSemaphore(Object semaphore) {
        ((PortableSemaphore) semaphore).acquire();
    }

    public static boolean tryAcquireSemaphore(Object semaphore) {
        return ((PortableSemaphore) semaphore).tryAcquire();
    }

    public static void releaseSemaphore(Object semaphore) {
        ((PortableSemaphore) semaphore).release();
    }

    private static void awaitOrCancel(Condition condition) {
        try {
            condition.await();
        } catch (InterruptedException e) {
            throw CancellationWakeup.INSTANCE;
        }
    }

    private static final class PortableLock {
        private final ReentrantLock mutex = new ReentrantLock();
        private final Condition available = mutex.newCondition();
        private Thread owner = null;
        private int depth = 0;

        private PortableCondition newCondition() {
            return new PortableCondition(this, mutex.newCondition());
        }

        private void lock() {
            final Thread current = Thread.currentThread();
            mutex.lock();
            try {
                if (owner == current) {
                    depth += 1;
                    return;
                }

                while (owner != null) {
                    awaitOrCancel(available);
                }

                owner = current;
                depth = 1;
            } finally {
                mutex.unlock();
            }
        }

        private boolean tryLock() {
            final Thread current = Thread.currentThread();
            mutex.lock();
            try {
                if (owner == null) {
                    owner = current;
                    depth = 1;
                    return true;
                }
                if (owner == current) {
                    depth += 1;
                    return true;
                }
                return false;
            } finally {
                mutex.unlock();
            }
        }

        private boolean unlock() {
            final Thread current = Thread.currentThread();
            mutex.lock();
            try {
                if (owner != current || depth == 0) {
                    return false;
                }

                depth -= 1;
                if (depth == 0) {
                    owner = null;
                    available.signal();
                }
                return true;
            } finally {
                mutex.unlock();
            }
        }
    }

    private static final class PortableCondition {
        private final PortableLock lock;
        private final Condition condition;

        private PortableCondition(PortableLock lock, Condition condition) {
            this.lock = lock;
            this.condition = condition;
        }

        private int await() {
            final Thread current = Thread.currentThread();
            lock.mutex.lock();
            try {
                if (lock.owner != current || lock.depth == 0) {
                    return CONDITION_AWAIT_NOT_OWNER;
                }

                final int savedDepth = lock.depth;
                lock.owner = null;
                lock.depth = 0;
                lock.available.signal();

                try {
                    condition.await();
                } catch (InterruptedException e) {
                    throw CancellationWakeup.INSTANCE;
                }

                while (lock.owner != null) {
                    awaitOrCancel(lock.available);
                }

                lock.owner = current;
                lock.depth = savedDepth;
                return CONDITION_AWAIT_OK;
            } finally {
                lock.mutex.unlock();
            }
        }

        private boolean signal() {
            final Thread current = Thread.currentThread();
            lock.mutex.lock();
            try {
                if (lock.owner != current || lock.depth == 0) {
                    return false;
                }
                condition.signal();
                return true;
            } finally {
                lock.mutex.unlock();
            }
        }

        private boolean signalAll() {
            final Thread current = Thread.currentThread();
            lock.mutex.lock();
            try {
                if (lock.owner != current || lock.depth == 0) {
                    return false;
                }
                condition.signalAll();
                return true;
            } finally {
                lock.mutex.unlock();
            }
        }
    }

    private static final class PortableBarrier {
        private final ReentrantLock mutex = new ReentrantLock();
        private final Condition tripped = mutex.newCondition();
        private final int parties;
        private int waiting = 0;
        private int generation = 0;

        private PortableBarrier(int parties) {
            this.parties = parties;
        }

        private int await() {
            mutex.lock();
            try {
                final int generation0 = generation;
                final int arrivalIndex = parties - waiting - 1;

                if (waiting + 1 == parties) {
                    waiting = 0;
                    generation += 1;
                    tripped.signalAll();
                    return 0;
                }

                waiting += 1;
                while (generation == generation0) {
                    try {
                        tripped.await();
                    } catch (InterruptedException e) {
                        if (generation == generation0 && waiting > 0) {
                            waiting -= 1;
                            tripped.signalAll();
                        }
                        throw CancellationWakeup.INSTANCE;
                    }
                }
                return arrivalIndex;
            } finally {
                mutex.unlock();
            }
        }
    }

    private static final class PortableCountDownLatch {
        private final ReentrantLock mutex = new ReentrantLock();
        private final Condition opened = mutex.newCondition();
        private int count;

        private PortableCountDownLatch(int count) {
            this.count = count;
        }

        private void await() {
            mutex.lock();
            try {
                while (count > 0) {
                    try {
                        opened.await();
                    } catch (InterruptedException e) {
                        throw CancellationWakeup.INSTANCE;
                    }
                }
            } finally {
                mutex.unlock();
            }
        }

        private void countDown() {
            mutex.lock();
            try {
                if (count == 0) {
                    return;
                }
                count -= 1;
                if (count == 0) {
                    opened.signalAll();
                }
            } finally {
                mutex.unlock();
            }
        }
    }

    private static final class PortableSemaphore {
        private final ReentrantLock mutex = new ReentrantLock();
        private final Condition available = mutex.newCondition();
        private int permits;

        private PortableSemaphore(int permits) {
            this.permits = permits;
        }

        private void acquire() {
            mutex.lock();
            try {
                while (permits == 0) {
                    try {
                        available.await();
                    } catch (InterruptedException e) {
                        throw CancellationWakeup.INSTANCE;
                    }
                }
                permits -= 1;
            } finally {
                mutex.unlock();
            }
        }

        private boolean tryAcquire() {
            mutex.lock();
            try {
                if (permits == 0) {
                    return false;
                }
                permits -= 1;
                return true;
            } finally {
                mutex.unlock();
            }
        }

        private void release() {
            mutex.lock();
            try {
                permits += 1;
                available.signal();
            } finally {
                mutex.unlock();
            }
        }
    }
}

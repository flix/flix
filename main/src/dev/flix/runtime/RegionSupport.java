package dev.flix.runtime;

import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.atomic.AtomicReference;

/**
 * Internal JVM region-cancellation helpers.
 */
public final class RegionSupport {

    private RegionSupport() {
    }

    public static void cancelChildren(ConcurrentLinkedQueue<Thread> threads, Thread regionThread) {
        final Thread current = Thread.currentThread();
        if (regionThread != current) {
            regionThread.interrupt();
        }
        for (Thread thread : threads) {
            if (thread != null && thread != current) {
                thread.interrupt();
            }
        }
    }

    public static void reportChildException(AtomicReference<Throwable> childException,
                                            Throwable throwable,
                                            ConcurrentLinkedQueue<Thread> threads,
                                            Thread regionThread) {
        if (throwable instanceof CancellationWakeup) {
            return;
        }
        if (childException.compareAndSet(null, throwable)) {
            cancelChildren(threads, regionThread);
        }
    }
}

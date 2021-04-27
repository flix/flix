package flix.runtime;
import java.util.concurrent.atomic.AtomicLong;

public class GlobalCounter {
    private static final AtomicLong globalCounter = new AtomicLong();

    public static long newId() {
        return globalCounter.getAndIncrement();
    }
}
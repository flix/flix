package flix.runtime;
import java.util.concurrent.atomic.AtomicInteger;

public class GlobalCounter {
    private static final AtomicInteger globalCounter = new AtomicInteger();

    public static int newId() {
        return globalCounter.getAndIncrement();
    }
}
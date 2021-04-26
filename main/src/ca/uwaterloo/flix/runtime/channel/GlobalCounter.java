package flix.runtime;
import java.util.concurrent.atomic.AtomicInteger;

public class GlobalCounter {
    private static final AtomicInteger gloablCounter = new AtomicInteger();

    public static int newId() {
        return gloablCounter.getAndIncrement();
    }
}
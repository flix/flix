package dev.flix.runtime;

import java.util.concurrent.atomic.AtomicLong;

public final class GlobalCounter {
    private static final AtomicLong counter = new AtomicLong();

    public static final long newId() {
        return counter.getAndIncrement();
    }
}
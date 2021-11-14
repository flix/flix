package dev.flix.runtime;

import java.util.concurrent.atomic.AtomicLong;

/**
 * A copy of the class generated in `main/src/ca/uwaterloo/flix/language/phase/jvm/GenGlobalCounterClass.scala`
 * that allows the compiler to load the reference in `main/src/library/Channel.flix` and check for types
 * (the code here is never run).
 */
public final class GlobalCounter {
    private static final AtomicLong counter = new AtomicLong();

    public static final long newId() {
        return counter.getAndIncrement();
    }
}

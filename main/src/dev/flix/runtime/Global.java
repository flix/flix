package dev.flix.runtime;

import java.util.concurrent.atomic.AtomicLong;

/**
 * OBS: A copy of the class generated in
 * `main/src/ca/uwaterloo/flix/language/phase/jvm/GenGlobalClass.scala` that
 * allows the compiler to load the imports in i.e. channel and check for types
 * (the code here is never run).
 */
public final class Global {
    private static final AtomicLong counter = new AtomicLong();
    private static String[] args = null;

    public static final long newId() {
        return counter.getAndIncrement();
    }

    public static final String[] getArgs() {
        String[] var0 = new String[args.length];
        System.arraycopy(args, 0, var0, 0, args.length);
        return var0;
    }

    public static final void setArgs(String[] var0) {
        args = var0;
    }
}

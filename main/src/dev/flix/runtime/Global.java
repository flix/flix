package dev.flix.runtime;

import java.util.concurrent.atomic.AtomicLong;

/**
 * OBS: A copy of the class generated in
 * `main/src/ca/uwaterloo/flix/language/phase/jvm/GenGlobalClass.scala` that
 * allows the compiler to load the imports in i.e. channel and check for types
 * (the code here is never run).
 */
public final class Global {
    private static final AtomicLong tCounter = null;
    private static String[] tArgs = null;

    public static final long newId() {
        throw new RuntimeException("Global.newId should not be called on the mock class");
    }

    public static final String[] getArgs() {
        throw new RuntimeException("Global.getArgs should not be called on the mock class");
    }

    public static final void setArgs(String[] var0) {
        throw new RuntimeException("Global.setArgs should not be called on the mock class");
    }
}

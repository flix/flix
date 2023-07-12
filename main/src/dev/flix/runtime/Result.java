package dev.flix.runtime;

/**
 * A Cont is either: Done (null) | Thunk | Suspend
 */
public interface Result {
    public int getResult();
}


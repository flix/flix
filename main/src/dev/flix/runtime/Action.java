package dev.flix.runtime;

/**
 * A Cont is either: Done (null) | Cont | Suspend
 */
public interface Action {
    public int getResult();
}

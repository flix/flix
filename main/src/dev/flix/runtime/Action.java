package dev.flix.runtime;

/**
 * A Cont is either: Done (null) | Suspend | TailCall
 */
public interface Action {
    public int getResult();
}
